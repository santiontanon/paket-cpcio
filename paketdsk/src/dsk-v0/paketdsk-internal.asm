;------------------------------------------------------------------------
; Amstrad CPC disk file load/save library for the PAKET engine
; Santiago Ontañón, 2024
;
; Internal functions that should probably not be called by the user of the PAKETDSK library.
; 
; Original disasembly, which was the starting point for this file, was obtained from here:
; - https://cpcrulez.fr/coding_src-list-disassembly_of_amsdos_rom.htm
;
; Additional information obtained from:
; - AMSDOS memory map: https://www.cpcwiki.eu/index.php/AMSDOS_Memory_Map
; - FDC: https://www.cpcwiki.eu/index.php/765_FDC#FDC_Status_Registers
; 
; Terms:
; - DPH: Disk Parameter Header
; - XDPB: eXtended Disk Parameter Block
; - FDC: Floppy Disk Controller
; - FCB: File Control Block
; 
; Notes:
; - a floppy has 2 sides
; - each side has a series of tracks
; - each track has a number of records/sectors/blocks
; - record: 128 bytes
; - block: a group of records (1KB in size)
; - sector: smallest physically addressable unit in the disk (512 bytes in CPC).
; - "directory entry" == "catalog"?: 32 byte structure storing a file user, filename and where 16KB of data are.
;                                    Files larger than 16KB, need multiple catalogs.
; - There are no "directories" in a disk, it's just a flat list of files. The only grouping that can be done
;   is by "user" (with user "#e5" reserved for erased files).
; 
; Datastructures (info from https://www.cpcwiki.eu/index.php/AMSDOS_Memory_Map, with some corrections):
; 
; Extended Disk Parameter Block (XDPB,CSA,ALT) for Drive A/B
;  00h  2 SPT   Records per Track (CPM/Data=36, IBM=32)
;  02h  1 BSH   Block Shift (3) (records per block = 1 SHL N)
;  03h  1 BLM   Block Mask  (7) (records per block minus 1)
;  04h  1 EXM   Extent Mask (0)
;  05h  2 DSM   Max Block Number  (CPM=170, Data=179, IBM=155)
;  07h  2 DRM   Max Dir Entries-1 (63)
;  09h  2 AL    Directory Allocation Table (11000000b,00000000b = Block 0,1)
;  0Bh  2 CKS   Checksum Area Size (Size of the "CSA" field) (0010h)
;  0Dh  2 OFF   Track Offset (Size of Bootstrap) (CPM=2, Data=0, IBM=1)
;  0Fh  1 FSC   First Sector on Track (CPM=41h, Data=C1h, IBM=01h)
;  10h  1 PST   Physical Sectors per Track (CPM/Data=9, IBM=8)
;  11h  1 GPS   GAP3 for Sector Read/Write (2Ah)
;  12h  1 GPT   GAP3 for Track Formatting  (52h)
;  13h  1 FLB   Fillbyte for Track Formatting (E5h)
;  14h  1 BPS   Bytes per Sector (2=512) (80h SHL N, Shift amount or so?)
;  15h  1 RPS   Records per Sector (4)
;  16h  1 Current Track
;  17h  1 Recalibrate Track 0 Flag
;  18h  1 Flag Login on any access ? (re-detect CPM/Data format?)
;  19h 16 CSA   Checksums (directory record chksums, for sensing disk-changes)
;  29h 23 ALT   Block Allocation Table (23x8bit = 184bit) (only 180bit used)
;                   - This table contains 180bits: each bit corresponds to a block.
;                   - If it's 1, block is used, if it's 0, block is not used.
;
; Extended File Control Block (FCB) for OPENIN/OPENOUT
;  00h  1 Drive Number (00h=A, 01h=B, FFh=Not Open)
;  01h  1 User Number
;  02h  8 Filename  (padded with 20h)
;  0Ah  3 Extension (padded with 20h)
;  0Dh  1 Extent Number (00h=First directory entry of file)
;  0Eh  2 Zero
;  10h  1 Number of Records in current Extent
;  11h 16 Block Numbers for current Extent
;  21h  3 Number of previously accessed Records
;
; Disk Parameter Header (DPH) for Drive A/B
;  00h  2 XLT    Skew Factor Translation (physical-to-logical sector) (not used)
;  02h  2 TRACK  Current Directory Number (should be Current Track, misused as DIRNUM by AMSDOS)
;  04h  2 SECTOR Current Sector
;  06h  2 DIRNUM Current Directory Number (unused)
;  08h  2 DIRBUF Pointer to Directory Buffer (A930h)
;  0Ah  2 DPB    Pointer to DPB/XDPB (A890h/A8D0h for Drive A/B) Drive Param Block
;  0Ch  2 CSV    Pointer to CSA (A8A9h/A8E9h for Drive A/B) Checksums
;  0Eh  2 ALV    Pointer to ALT (A8B9h/A8F9h for Drive A/B) Allocation Table
;
; File Header for OPENIN/OPENOUT
;  00h  1 Access Mode (1=CHAR, 2=DIRECT)
;  01h  2 Pointer to 2K work buffer
;  03h  2 Pointer to current CHAR in 2K work buffer
;  05h 16 User Number and Filename (padded with 00h)
;  15h  1 Block Number (whatever, can be zero)
;  16h  1 Last Block   (whatever, can be zero)
;  17h  1 File Type    (#00:  BASIC, #01: encrypted BASIC, #02: binary)
;  18h  2 Data Length  (whatever, can be zero)
;  1Ah  2 Load Address in memory
;  1Ch  1 First Block           FFh
;  1Dh  2 Filesize (excluding the 80h-byte header)
;  1Fh  2 Entrypoint (for executable binary files)
;  21h 36 Unused (free for user?) (parts are used for OPENOUT)
;  45h  3 24bit Filepos in CHARs, or 16bit Filesize if non-ASCII file
;  48h  2 Checksum accross [05h..47h]
;
; Catalog structure (a.k.a. "directory entry") (info from https://www.cpcwiki.eu/index.php/765_FDC#The_15_FDC_Commands)
;  0    User Number (from 0 to 255, USER 229, #E5, is for deleted files).
;  1-8  Filename in CAPS.
;  9-11 Extension in CAPS. Read Only flag on byte9 bit7. Hidden flag on byte10 bit7. Archive flag on byte11 bit7.
;  12   Current Extent  0 is first extent. There can be up to 128K bytes (8 logical extents) directly addressed by a single directory entry.
;  13   Reserved    
;  14   Extent High Byte
;  15   Record Count (Number of 128 Byte Blocks). Goes up to 128.
;  16–31   Block IDs where to find the file data for this extent.


;------------------------------------------------------------------------
; Executes one of the tickers that has been installed in the interrupt
;
; Each event is 7 bytes:
; - 2 bytes: ptr to the next event
; - 1 byte: count
; - 1 byte: class (#80: asynchronous, #00: synchronous)
; - 2 bytes: routine address
; - 1 byte: bank
;
; input:
; - af': will have its carry flag set
; - de: address of next ticker in the chain
; - hl: address of current event
paketdsk_interrupt_execute_ticker:
    inc hl  ; skip next event ptr
    inc hl
    di
    ld a, (hl)  ; get count
    inc (hl)  ; increment count
    jp p, interrupt_execute_ticker__positive_count
    dec (hl)
    ret

interrupt_execute_ticker__positive_count:
    or a
    ret nz  ; count has not yet reached zero

interrupt_execute_ticker__loop:
    push hl
        call interrupt_call_event_fn
    pop hl
    dec (hl)
    ret z

    jp p, interrupt_execute_ticker__loop
    inc (hl)
    ret


;------------------------------------------------------------------------
; Calls the function in an event block
;
; input:
; - hl: event block
interrupt_call_event_fn:
    inc hl  ; skip count
    inc hl  ; skip event type (as we will only handle "near" calls in this code,
            ; which do not require bank changes)
    ld e, (hl)
    inc hl
    ld d, (hl)
    ex de, hl
    jp (hl)


;------------------------------------------------------------------------
jp_hl:
    jp (hl)


;------------------------------------------------------------------------
; setup XDPB, and get address of drive's XDPB (eXtended Disk Parameter Block)
;
; input:
; - iy: base address of AMSDOS work RAM
; - c: drive index
; - e: #ff if the drive matches that of openin/openout control blocks. And 0 if it does not.
; output:
; - hl: XDPB ptr
; - carry clear: error
; - carry set: hl = address of drive's XDPB
; - XDPB is updated to reflect format
fw_c4f0_setup_XDPB:
    ld a, c
    cp 2
    ld hl, 0
    ret nc  ; quit if drive index is >=2
    ; drive is 0 or 1
    ld a, e
    rra  ; transfer bit 0 into carry
    jr c, setup_XDPB__format_detected  ; force

    ld e, c
    ld a, AMSDOS_XDPB_offset_detect_format_flag
    call fw_ca5c_get_xdpb_parameter_value  ; get XDPB parameter by index
    or a  ; test detect format flag
    jr nz, setup_XDPB__format_detected  ; != 0: do not detect format     
    ; detect format using "read id"
    push hl
        call fw_c56c_detect_format_from_and_setup_xdpb  ; detect format on disk and setup XDPB
    pop hl
    ret nc

setup_XDPB__format_detected:
    ld a, c
    ld (memory_be53_drive_hs_us), a

    ; get offset of XDPB in AMSDOS work ram
    ld hl, AMSDOS_work_RAM_offset_DPH_drive_a  ; offset of XDPB for drive 0
    or a
    jr z, setup_XDPB__dph_offset_set          
    ld hl, AMSDOS_work_RAM_offset_DPH_drive_b  ; offset of XDPB for drive 1

setup_XDPB__dph_offset_set:
    jp fw_ca9f_hl_pluseq_iy  ; HL = HL + IY


;------------------------------------------------------------------------
; CP/M function "setdma"
;
; input:
; - bc: address
fw_c51a_set_directory_buffer_ptr:
    ld (memory_be60_directory_buffer_ptr), bc
    ret     


;------------------------------------------------------------------------
; Writes the current sector, and then sets 'memory_be54_track' to 0
fw_c51f_write_current_sector_and_reset_track:
    call fw_c86f_write_current_sector
    ld a, 0
    ld (memory_be54_track), a
    ret     


;------------------------------------------------------------------------
; Increments sector/track and writes a sector to disk.
;
; input:
; - c: if c == 1, it saves the current content of the directory buffer instead.
;      if c == 2, it inits the drive params
fw_c52e_inc_sector_track_and_write_sector:
	push bc
    	ld a, c
    	cp 2
    	call z, fw_c7eb_init_n_records_left_and_drive_params_3
    	call fw_c800_check_there_are_records_left_and_drive_params_3_match_1
    	call c, fw_c81b_inc_record_sector_and_track  ; preserves carry
    	call fw_c832_read_write_sector_if_not_done_before
	pop bc
	ret nc

	call fw_c8b6_copy_directory_to_current_record_buffer
	dec c
	scf     
	call z, fw_c86f_write_current_sector
	ret nc
	ld a, 0
	ret     


;--------------------------------------------------------------------------
fw_c54c_read_sector_and_copy_to_directory_buffer:
    xor a
    ld (memory_be59_n_records_left), a
    call fw_c832_read_write_sector_if_not_done_before
    call fw_c8c7_copy_current_record_to_directory_buffer
    ret nc
    ld a, 0
    ret     


;------------------------------------------------------------------------
fw_c55d_fdc_execute_getid_command:
    ld bc, #fb7e  ; BC = I/O address of FDC main status register
    ld a, #4a  ; read id
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    ld a, e  ; drive 
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    jp fw_c8f9_get_fdc_response_and_restore_stack_if_error


;------------------------------------------------------------------------
; detect format on disk and setup XDPB
;
; input:
; - e: drive
;
; NOTES:
; - uses current track
; - performs a read ID and uses sector ID to select the format
fw_c56c_detect_format_from_and_setup_xdpb:
    call fw_c976_spin_motor_up_if_off  ; spin up drive motor
    ld a, AMSDOS_XDPB_offset_current_track
    call fw_ca5c_get_xdpb_parameter_value
    ld d, a  ; D = current track
    ld c, #10
    ld hl, fw_c55d_fdc_execute_getid_command  ; read id function
    call fw_c6ff_execute_function_with_retry  ; execute function with retry
    ret nc
    ; "read id" succeeded
    ; get sector id
    ld a, (memory_be4c_fdc_response + 5)  ; R from result phase data
    ; jr fw_c581_set_xdpb_format


;------------------------------------------------------------------------
; Initializes the XDPB with the given format
;
; input:
; - a: id of format
;   #41 (SYSTEM/VENDOR)
;   #c1 (DATA)
;   #01 (IBM)
fw_c581_set_xdpb_format:
    push af
        ; copy the XDPB values for the SYSTEM format the the current format pointer:
        xor a  ; XDPB: SPT
        call fw_ca63_get_xdpb_parameter_address ; get address of XDPB parameter
        push hl
            ex de, hl
            ld hl, fw_ca43_system_format_xdpb_values  ; full XPDB (setup for SYSTEM format)
            ld bc, #16
            ldir
        pop hl
    pop af
    and #c0
    cp #40  ; if we wanted system format, then we are done
    scf     
    ret z
    
    ld de, fw_c5ca_data_format_spec  ; definition for DATA format
IF SUPPORT_IBM_FORMAT = 1
    cp #c0  ; DATA FORMAT
    jr z, set_xdpb_format__format_set

    ld de, fw_c5c0_ibm_format_spec ; definition for IBM format
set_xdpb_format__format_set:
ENDIF
    ; Overwrite XDPB config with the DATA/IBM overrides:
    ld a, (de)
    inc de
    ld (hl), a  ; records per track (low)
    inc hl
    ld a, (de)
    inc de
    ld (hl), a  ; records per track (high)
    ld bc, 4
    add hl, bc
    ld a, (de)
    inc de
    ld (hl), a  ; number of blocks (high)
    inc hl
    ld a, (de)
    inc de
    ld (hl), a  ; number of blocks (high)
    ld bc, 7
    add hl, bc
    ex de, hl
    ld bc, 6
    ldir  ; rest of the config
    scf     
    ret     


;------------------------------------------------------------------------
; format (10 bytes):
; offset 0, 1: records per track
; offset 2, 3: number of blocks
; offset 4, 5: track offset
; offset 6: first sector id
; offset 7: sectors per track
; offset 8: gap length for reading/writing
; offset 9: gap length for format
IF SUPPORT_IBM_FORMAT = 1
; IBM format
fw_c5c0_ibm_format_spec:
    db #20, #00, #9b, #00, #01, #00, #01, #08, #2a, #50
ENDIF
; DATA format
fw_c5ca_data_format_spec:
    db #24, #00, #b3, #00, #00, #00, #c1, #09, #2a, #52


;-----------------------------------------------------------------------
; Gets the drive/track/sector id/sector ptr from 'memory_be56_drive_hs_us_2', and
; then calls the BIOS function to write a sector.
fw_c86f_write_current_sector:
    ld hl, memory_be5e_read_write_sector_flag
    ld (hl), 0  ; init to 0
    dec hl  ; hl = memory_be5d  ????
    ld a, (hl)
    or a
    scf  ; set carry
    ret z  
    
    inc (hl)
    call fw_c8a2_get_drive_track_sector_and_buffer  ; generate sector ID, drive, track and address of sector buffer
    ; jp fw_c64e_bios_write_sector


;------------------------------------------------------------------------
; BIOS: WRITE SECTOR
;
; input:
; - e: drive
; - d: track
; - c: sector id
; - hl: table of C,H,R,N for each sector
;
; NOTES:
; - H parameter is forced to 0
; - N parameter comes from XDPB
; - R parameter defined by user
; - only 1 sector written at a time
; - C parameter defined by user (must be valid track number)
; - double density only
; - "write data" only
fw_c64e_bios_write_sector:
    ld a, #45 ; write data
IF SUPPORT_FORMATTING = 1
    jr bios_format_sector__entry_point
ENDIF


;------------------------------------------------------------------------
; BIOS: FORMAT TRACK
; 
; input:
; - hl: table of C,H,R,N for each sector
; - e: drive & side
; - d: track
; - c: sector id
;
; NOTES:
; - N parameter for format from XDPB
; - SC parameter for format from XDPB
; - GPL parameter for format from XDPB
; - D parameter for format from XDPB
; - double density only
; - C, H, R, N for each sector id field can be any values therefore possible to write strange formats.
IF SUPPORT_FORMATTING = 1
fw_c652_bios_format_sector:
    ld a, #4d  ; "format track" command (mfm)
bios_format_sector__entry_point:
ENDIF
    call fw_c976_spin_motor_up_if_off
    ld b, #11
    call bios_read_sector__entry_point
    ld a, (memory_be48_formatting_delay)
bios_format_sector__wait_loop:
    dec a
    inc bc
    inc bc
    inc bc
    jr nz, bios_format_sector__wait_loop
    ret


;------------------------------------------------------------------------
; BIOS: READ SECTOR
;
; input:
; - hl: buffer
; - e: drive
; - d: track
; - c: sector id
;
; NOTES:
; - H parameter is forced to 0
; - N parameter comes from XDPB
; - R parameter defined by user
; - only 1 sector read at a time
; - C parameter defined by user (must be valid track number)
; - double density only
; - "read data" only + skip
fw_c666_bios_read_sector:
    call fw_c976_spin_motor_up_if_off
    ld a, #66  ; read data (mfm, skip)
    ld b, #10
bios_read_sector__entry_point:
    ld (memory_be62_sector_buffer_ptr), hl
    ld h, a  ; fdc command code
    ld l, c  ; R = sector id
    ld (memory_be74_desired_track_sector_number), hl
    ld c, b
    ld hl, fw_c67c_execute_read_write_or_format  ; execute "read data", "write data" or "format track" command
    jp fw_c6ff_execute_function_with_retry  ; execute function with retry


;------------------------------------------------------------------------
; execute "read data", "write data" or "format track" command
;
; input:
; - e: drive and side
; - d: C parameter (a valid track number)
fw_c67c_execute_read_write_or_format:
    ld hl, (memory_be74_desired_track_sector_number)  ; l = R parameter, h = FDC command code
    ld bc, #fb7e  ; BC = I/O address for FDC main status register
    ld a, h  ; fdc command code
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    ld a, e  ; drive and side
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
IF SUPPORT_FORMATTING = 1
    ld a, h
    cp #4d  ; "format track"  command?
    jr nz, fw_c6a5_execute_read_or_write  ; "read data" or "write data"
    ; "format track" command
    ld a, #14  ; N parameter
    call fw_c959_send_floppy_controller_xdpb_parameter  ; write XDPB parameter to FDC
    ld a, #10  ; SC parameter
    call fw_c959_send_floppy_controller_xdpb_parameter  ; write XDPB parameter to FDC
    ld a, #12  ; GPL parameter
    call fw_c959_send_floppy_controller_xdpb_parameter  ; write XDPB parameter to FDC
    ld a, AMSDOS_XDPB_offset_formatting_fillbyte  ; D parameter
    call fw_ca5c_get_xdpb_parameter_value  ; get XDPB parameter by index
    jr execute_read_or_write__continue            
ENDIF


;------------------------------------------------------------------
; Executes either "read data" or "write data" commands.
;
; input:
; - d: C parameter
; - l: R parameter
fw_c6a5_execute_read_or_write:
    ; "read data" or "write data" command
    ld a, d  ; C parameter
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    xor a  ; H parameter
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    ld a, l  ; R parameter
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    ld a, #14  ; N parameter
    call fw_c959_send_floppy_controller_xdpb_parameter  ; write XDPB parameter to FDC
    ld a, l  ; EOT parameter
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    ld a, #11  ; GPL parameter
    call fw_c959_send_floppy_controller_xdpb_parameter  ; write XDPB parameter to FDC
    ld a, #ff  ; DTL parameter
execute_read_or_write__continue:
    call fw_c6d1_send_last_fdc_command_byte_and_transfer_data ; send last byte of command and transfer execution data

    ei      
    call fw_c907_get_fdc_response_and_restore_stack_if_error_from_fcd_response
    ret c
    ret nz

    ld a, (memory_be4c_fdc_response + 1)  ; FDG status reguster 1
    add a, a
    ret c  ; return if "end-of-track" bit is set (in this case, errors from read/write are ok)
    xor a  ; nc: error!
    ret


;------------------------------------------------------------------------
; send last byte of command and transfer execution data
;
; input:
; - a: command byte
; - h: first command byte (contains FDC command code)
fw_c6d1_send_last_fdc_command_byte_and_transfer_data:
    di  ; disable interrupts (prevents overrun condition)
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    ld a, h  ; get FDC command code
    ld hl, (memory_be62_sector_buffer_ptr)  ; address of buffer to transfer data to/from 
    cp #66  ; write data command?
    jr nz, execution_phase_write_data__wait  ; fdc: write data in execution phase
    jr execution_phase_read_data__wait  ; fdc: read data in execution phase


;------------------------------------------------------------------------
; fdc: read data in execution phase
; quits if data is ready and execution phase has ended
;
; input:
; - hl: ptr to write to
; - bc: I/O address of FDC main status register
fw_c6df_execution_phase_read_data:
    inc c  ; BC = I/O address for FDC data register
    in a, (c)  ; read from FDC data register
    ld (hl), a  ; write to RAM
    dec c  ; BC = I/O address for FDC main status register
    inc hl  ; increment RAM pointer
execution_phase_read_data__wait:
    ; start here
    in a, (c)  ; read FDC main status register
    jp p, execution_phase_read_data__wait  ; data ready?
    and #20  ; execution phase active?
    jr nz, fw_c6df_execution_phase_read_data  ; go to transfer byte
    ; execution phase over
    ret     


;------------------------------------------------------------------------
; fdc: write data in execution phase
; quits if data is ready and execution phase has ended
;
; input:
; - hl: ptr to read from
; - bc: I/O address of FDC main status register
fw_c6ef_execution_phase_write_data:
    inc c  ; BC = I/O address for FDC data register
    ld a, (hl)  ; read from RAM
    out (c), a  ; write to FDC data register
    dec c  ; BC = I/O address for FDC main status register
    inc hl  ; increment RAM pointer
execution_phase_write_data__wait:
    ; start here
    in a, (c)  ; read main status register
    jp p, execution_phase_write_data__wait  ; data ready?
    and #20  ; execution phase active?
    jr nz, fw_c6ef_execution_phase_write_data  ; go to transfer byte
    ; execution phase over
    ret     


;------------------------------------------------------------------------
; execute function with retry
;
; input:
; - hl: function to execute
; - e: drive & side
; - d: track
; - c: message code if error
fw_c6ff_execute_function_with_retry:
    ld a, (memory_be66_retry_count)  ; retry count
    ld b, a
execute_function_with_retry__loop:
    call fw_c72b_move_to_track_and_try_command
    ret c
    ret z
    ld a, b
    and #04
    jr z, execute_function_with_retry__reset_aligned_flag

    push de
        ld d, 39
        call fw_c766_bios_move_to_track__motor_already_on  ; move to track 39
    pop de
    jr execute_function_with_retry__loop  ; try command again
execute_function_with_retry__reset_aligned_flag:
    push hl
        ld a, #17  ; XDPB: aligned flag
        call fw_ca63_get_xdpb_parameter_address  ; get address of XDPB parameter
        ld (hl), 0
    pop hl
    jr execute_function_with_retry__loop  ; try command again


;-------------------------------------------------------------
; Moves to the desired track and tries to execute a command.
; if failure will step up to higher or lower track and try the command again.
;
; input:
; - b: retry count
; - d: current track
; - There might other arguments as required by the command to tyy.
fw_c72b_move_to_track_and_try_command:
    call fw_c754_move_to_track_and_execute_function  ; move to track, and execute function
    ret c
    ret z
    
    call fw_c947_clear_fdc_interrupt  ; clear fdc interrupt

    ; try command again
    call fw_c754_move_to_track_and_execute_function  ; move to track and execute function
    ret c
    ret z

    ; attempt step to higher track...
    ld a, d  ; get current track
    cp 39  ; CPC single sided disks have 40 tracks, so 39 is the last track
    dec b  ; preserves carry flag
    jr nc, move_to_track_and_try_command__in_lower_track  ; we are already at track 39, move to a lower track and then try again
    inc b
    ; if not at track 39, do step to higher track 
    inc d
    call fw_c766_bios_move_to_track__motor_already_on  ; move to track
    dec d

    ; try command again
    call fw_c754_move_to_track_and_execute_function  ; move to track and execute function
    ret c
move_to_track_and_try_command__in_lower_track:
    ret z  ; if we were at track 39, or the try to a higher track failed, return
    ld a, d  ; get track number
    or a  ; are we in track 0?
    jr nz, move_to_track_and_try_command__in_lower_track_continue  ; if not at track zero, do step to lower track and try command again

    ; at track zero; can't step to lower track
    dec b ; decrement retry count
    ret     

move_to_track_and_try_command__in_lower_track_continue:
    ; do step to lower track
    dec d
    call fw_c766_bios_move_to_track__motor_already_on  ; move to track
    inc d
    ; jr fw_c754_move_to_track_and_execute_function


;-------------------------------------------------------------
; Moves to a given track ('d'), and executes a function ('hl'). If it fails, it decrements the retry count in 'b'.
;
; input:
; - hl: function to execute
; - d: track
; - e: drive & side
; - b: retry count
; output:
; - carry set - function executed with no errors
; - carry clear & zero clear - try again
; - carry clear & zero set - decrement retry count
fw_c754_move_to_track_and_execute_function:
    call fw_c766_bios_move_to_track__motor_already_on  ; move to track
    push hl
    push bc
        call jp_hl
    pop bc
    pop hl
    ret c
    jr nz, fw_c754_move_to_track_and_execute_function         
    dec b  ; decrement the number of retries we have left
    ret


;------------------------------------------------------------------------
; BIOS: MOVE TRACK
;
; input:
; - e: drive
; - d: track
; output:
; - carry flag set if successful
fw_c763_bios_move_to_track:
    call fw_c976_spin_motor_up_if_off
fw_c766_bios_move_to_track__motor_already_on:
    push hl
    push de
    push bc
        ld a, (memory_be66_retry_count)  ; retry count
        ld b, a
        ld a, #17  ; XDPB: aligned flag
        call fw_ca63_get_xdpb_parameter_address  ; get address of XDPB parameter
        ld a, (hl)
        or a
        jr nz, bios_move_to_track__send_request
bios_move_to_track__recalibrate:
        ; drive is not aligned, send the recalibrate command, before asking it to move to the desired track:
        push bc
            ld bc, #fb7e ; BC = I/O address of FDC main status register
            ld a, #07 ; recalibrate command
            call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
            ld a, e
            call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
            ld a, #28
            call fw_c7c7_wait_and_send_interrupt_status_command
            jr nc, bios_move_to_track__failure
            ld a, #16  ; XDPB: current track
            call fw_ca63_get_xdpb_parameter_address ; get address of XDPB parameter
            ld (hl), 0
            inc hl
            ld (hl), #ff
        pop bc
bios_move_to_track__send_request:
        dec hl
        ld a, (hl)
        sub d  ; are we already in the desired track?
        jr z, bios_move_to_track__success
        ; Ask the drive to move to the desired track:
        push bc
            ld bc, #fb7e  ; BC = I/O address of FDC main status register
            ld a, #0f  ; seek command
            call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
            ld a, e
            call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
            ld a, d
            call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
            ; calculate how many tracks we need to move (to get how much we need to wait)
            sub (hl)
            jr nc, bios_move_to_track__delay_computed
            ld a, (hl)
            sub d
bios_move_to_track__delay_computed:
            ; here 'a' contains how much do we need to wait (how many tracks we need to move)
            ld (hl), d
            call fw_c7c7_wait_and_send_interrupt_status_command
bios_move_to_track__failure:
        pop bc
        jr c, bios_move_to_track__success
        jr nz, bios_move_to_track__recalibrate
        dec b  ; failed, decrease the retey count
        jp z, fw_c9ad_restore_stack_stop_motor_and_get_error_if_any  ; we are out of retries
        call fw_c947_clear_fdc_interrupt  ; clear fdc interrupt
        jr bios_move_to_track__recalibrate
bios_move_to_track__success:
    pop bc
    pop de
    pop hl
    scf
    ret


;------------------------------------------------------------------------
; Waits some amount of time (controlled by "a"), and then sends an "interrupt status command".
;
; input:
; - a: number of times to wait for a track_settle_delay.
fw_c7c7_wait_and_send_interrupt_status_command:
    push af
        ld a, (memory_be4a_track_settle_delay)
        call fw_c7e0_delay_in_milliseconds  ; delay
    pop af
    dec a
    jr nz, fw_c7c7_wait_and_send_interrupt_status_command

    ld a, (memory_be49_delay)
    call fw_c7e0_delay_in_milliseconds
    ld a, #08  ; sense interrupt status command
    call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte
    jp fw_c8f9_get_fdc_response_and_restore_stack_if_error


;------------------------------------------------------------------------
; delay in milliseconds
;
; input:
; - a: amount of delay (in milliseconds)
fw_c7e0_delay_in_milliseconds:
delay_in_milliseconds__loop:
    push af
        ld a, #f6
delay_in_milliseconds__loop2:
        dec a
        jr nz, delay_in_milliseconds__loop2
    pop af
    dec a
    jr nz, delay_in_milliseconds__loop
    ret     


;------------------------------------------------------------------------
; Initializes (memory_be59_n_records_left) to the number of records per block,
; and copies drive/track/record from memory_be53_drive_hs_us to memory_be5a_drive_hs_us_3
fw_c7eb_init_n_records_left_and_drive_params_3:
	ld hl, memory_be53_drive_hs_us
	ld e, (hl)  ; get current drive
	ld a, AMSDOS_XDPB_offset_block_mask  ; (block mask == number of records per block - 1)
	call fw_ca5c_get_xdpb_parameter_value
	inc a  ; a = number of records per block
	ld de, memory_be59_n_records_left
	ld (de), a
	inc de  ; de = memory_be5a_drive_hs_us_3
	ld bc, 3
	ldir  ; copy drive, track, sector to 'memory_be5a_drive_hs_us_3'
	ret


;-------------------------------------------------------------
; output:
; - carry: memory_be59_n_records_left > 0 and
;          memory_be53_drive_hs_us drive/track/record match with memory_be5a_drive_hs_us_3
; - no carry: if memory_be59_n_records_left == 0 or there is a difference.
;             in this case memory_be59_n_records_left is reset to 0.
fw_c800_check_there_are_records_left_and_drive_params_3_match_1:
	ld de, memory_be59_n_records_left
	ld a, (de)
	or a
	ret z
    ; check that drive/track/sector in 'memory_be53_drive_hs_us'
    ; are the same as in 'memory_be5a_drive_hs_us_3':
	inc de  ; de = memory_be5a_drive_hs_us_3
	ld hl, memory_be53_drive_hs_us
	ld b, 3
check_there_are_records_left_and_drive_params_3_match_1__loop:
	ld a, (de)
	xor (hl)
	jr nz, check_there_are_records_left_and_drive_params_3_match_1__difference  ; jump if there is a difference
	inc de
	inc hl
	djnz check_there_are_records_left_and_drive_params_3_match_1__loop
	scf     
	ret
check_there_are_records_left_and_drive_params_3_match_1__difference:
	xor a
	ld (memory_be59_n_records_left), a
	ret     


;-------------------------------------------------------------
; Decrements the number of records left,
; increments the current sector, and updates current track if necessary
fw_c81b_inc_record_sector_and_track:
	push af
    	ld hl, memory_be59_n_records_left
    	dec (hl)
    	inc hl  ; hl = memory_be5a_drive_hs_us_3
    	ld e, (hl)
    	inc hl
    	inc hl  ; hl = memory_be5c_sector_3
    	inc (hl)  ; increment the current sector
    	xor a  ; a = AMSDOS_XDPB_offset_records_per_track
    	call fw_ca5c_get_xdpb_parameter_value  ; get XDPB parameter by index
    	cp (hl)  ; check if we have reached past the last sector
    	jr nz, inc_record_sector_and_track__track_set
    	ld (hl), 0  ; reset sector count
    	dec hl  ; hl = memory_be5b_track_3
    	inc (hl) ; increment current track
inc_record_sector_and_track__track_set:
	pop af
	ret     


;-------------------------------------------------------------
; This method:
; - checks if we have already read/written this sector
; - if we have, we are done
; - otherwise, it reads or writes it (depending on "carry")
;
; input:
; - carry: writing
; - no carry: reading 
fw_c832_read_write_sector_if_not_done_before:
    push af
        call fw_c854_check_if_we_have_already_read_or_written_this_sector
        jr c, read_write_sector_if_not_done_before__sector_written  ; if we have, we are done
        call fw_c86f_write_current_sector
    pop bc
    ret nc

    push bc
        call fw_c880_copy_drive_track_record_to_second_copy
    pop af
    jr c, read_write_sector_if_not_done_before__sector_read

    call fw_c8a2_get_drive_track_sector_and_buffer
    call fw_c666_bios_read_sector
read_write_sector_if_not_done_before__sector_read:
    push af
        sbc a, a
        ld (memory_be5e_read_write_sector_flag), a  ; set to #ff upon a successful read
    pop af
    ret

read_write_sector_if_not_done_before__sector_written:
    pop af
    scf     
    ret


;-----------------------------------------------------------------------
; This method checks that the values in (memory_be53_drive_hs_us) match those in (memory_be56_drive_hs_us_2),
; for drive, track and sector/record.
; output:
; - c: there was a match
; - no carry, z: there was a failure reading/writing before
; - no carry, nz: there was no match of drive/track/sector values
fw_c854_check_if_we_have_already_read_or_written_this_sector:
    ld a, (memory_be5e_read_write_sector_flag)
    or a
    ret z  ; there was a failure reading/writing before

    ld bc, memory_be53_drive_hs_us
    ld hl, memory_be56_drive_hs_us_2
    ld e, (hl)
    ld a, (bc)
    xor (hl)  ; check if (memory_be53_drive_hs_us) == (memory_be56_drive_hs_us_2)
    ret nz  ; no match, error!

    inc bc
    inc hl
    ld a, (bc)  ; check if (memory_be54_track) == (memory_be57_track_2)
    xor (hl)  ; no match, error!
    ret nz

    ; since (memory_be58_sector_2) is in sectors, and (memory_be55_record) is in records, we need to translate first:
    call fw_c892_translate_records_2_to_sectors
    xor (hl)  ; check it mactches with the position in sectors in (memory_be58_sector_2)
    ret nz  ; no match, error!
    scf  ; all matches, set carry flag to indicate success
    ret     


;-----------------------------------------------------------------------
; Copies the drive/track/record from (memory_be53_drive_hs_us) to (memory_be56_drive_hs_us_2),
; translating from records to sectors (since 'memory_be56_drive_hs_us_2' stores position in sectors).
fw_c880_copy_drive_track_record_to_second_copy:
    ld bc, memory_be53_drive_hs_us
    ld hl, memory_be56_drive_hs_us_2
    ld a, (bc)
    ld (hl), a
    ld e, a
    inc hl
    inc bc
    ld a, (bc)
    ld (hl), a
    call fw_c892_translate_records_2_to_sectors
    ld (hl), a
    ret   


;-----------------------------------------------------------------------
; Gets the current value of memory_be55_record, and assuming it is in "records",
; translates it to "sectors". 
;
; input:
; - bc: ptr to position in records
; - hl: ptr to position in sectors
; output:
; - bc/hl incremented
; - a: position in (bc + 1) translated to sectors
fw_c892_translate_records_2_to_sectors:
    inc bc  ; point to record
    inc hl  ; point to sector
    ld a, AMSDOS_XDPB_offset_records_per_sector
    call fw_ca5c_get_xdpb_parameter_value
    ld d, a  ; d = records per sector
    ld a, (bc)  ; a = record for drive 1
translate_records_2_to_sectors__loop:
    srl d
    ret c  ; when we return, a will be the record divided by the number of records per sector
    srl a
    jr translate_records_2_to_sectors__loop


;------------------------------------------------------------------------
; Get sector ID, drive, track and address of sector buffer
;
; output:
; - c: sector ID
; - e: drive
; - d: track
; - hl: ptr to sector buffer
fw_c8a2_get_drive_track_sector_and_buffer:
    ld de, (memory_be56_drive_hs_us_2)  ; e = drive, d = track
    ld a, AMSDOS_XDPB_offset_first_sector_on_track    
    call fw_ca5c_get_xdpb_parameter_value  ; get XDPB parameter by index
    ld hl, memory_be58_sector_2  ; current sector index
    add a, (hl)  ; add first sector ID value
    ld c, a  ; C = final sector ID
    ld hl, AMSDOS_work_RAM_offset_sector_buffer  ; offset of sector buffer in AMSDOS work ram
    jp fw_ca9f_hl_pluseq_iy  ; HL = HL + IY


;------------------------------------------------------------------------
; Also sets (memory_be5d) to #ff
fw_c8b6_copy_directory_to_current_record_buffer:
	push hl
	push de
	push bc
	push af
    	ld a, #ff
    	ld (memory_be5d), a
    	call fw_c8d6_get_record_and_directory_ptrs
    	ldir  ; copy from directory buffer to record buffer
    	jr copy_current_record_to_directory_buffer__done


;-----------------------------------------------------------------------
fw_c8c7_copy_current_record_to_directory_buffer:
    push hl
    push de
    push bc
    push af
        call fw_c8d6_get_record_and_directory_ptrs
        ex de, hl
        ldir  ; copy from record buffer to directory buffer
copy_current_record_to_directory_buffer__done:
    pop af
    pop bc
    pop de
    pop hl
    ret     


;-----------------------------------------------------------------------
; output:
; - de: ptr to record buffer
; - hl: directory_buffer_ptr
; - bc: record size
fw_c8d6_get_record_and_directory_ptrs:
    ld hl, memory_be53_drive_hs_us
    ld e, (hl)  ; drive
    ld a, AMSDOS_XDPB_offset_records_per_sector
    call fw_ca5c_get_xdpb_parameter_value
    dec a  ; make it into a "records per sector && mask"
    inc hl
    inc hl
    and (hl)  ; (memory_be55_record)  ; a = record within the current sector
    ld de, #0080  ; record size
    ld hl, AMSDOS_work_RAM_offset_sector_buffer - #0080
    inc a
get_record_and_directory_ptrs__loop:
    add hl, de
    dec a
    jr nz, get_record_and_directory_ptrs__loop
    ex de, hl  ; de = ptr to the record within the sector buffer
    call fw_ca98_de_pluseq_iy  ; DE = IY + DE
    ld hl, (memory_be60_directory_buffer_ptr)
    ld bc, #0080  ; record size
    ret     


;------------------------------------------------------------------------
fw_c8f9_get_fdc_response_and_restore_stack_if_error:
    call fw_c91c_get_floppy_controller_response  ; fdc get result phase
    ret c

    ld a, (memory_be4c_fdc_response)  ; get result phase data byte: fdc status register 0 
    and #08  ; isolate "not ready" flag 
    ret z
    jp fw_c9ad_restore_stack_stop_motor_and_get_error_if_any  ; "not ready" flag is set: disk missing


;------------------------------------------------------------------------
; - Gets the response from the FDC
; - If there is an error -> restore stack, stop motor, and return
; - Otherwise, it gets the status of the fdg result phase, and if error -> restore stack, stop motor, and return
; - Otherwise, just return.
fw_c907_get_fdc_response_and_restore_stack_if_error_from_fcd_response:
    call fw_c8f9_get_fdc_response_and_restore_stack_if_error
    ret c
    ret nz

    ld a, (memory_be4c_fdc_response + 1)  ; get result phase data byte: fdc status register 1
    and #02  ; isolate "not writeable" flag
    ret z
    jp fw_c9ad_restore_stack_stop_motor_and_get_error_if_any  ; "not writeable" flag is set:  therefore drive is write protected


;------------------------------------------------------------------------
; Floppy Disk Controller (FDC): get result phase
;
; Read result phase of fdc command.
;
; input:
; - bc: I/O address of FDC main status register
; 
; output:
; - D: number of bytes read in result phase
; - A: command result code (bit 7 and 6 of fdc status register 0)
; - zero clear and carry clear if condition was not "command completed successfully"
; - zero set and carry set if condition was "command completed successfully"
; - memory_be4b_result_phase_received_n_bytes: number of bytes received in result phase
; - memory_be4c_fdc_response: buffer for result phase data
fw_c91c_get_floppy_controller_response:
    push hl
    push de
        ld d, 0  ; initialise count of result bytes received
        ld hl, memory_be4c_fdc_response  ; buffer for result phase data
        push hl
get_floppy_controller_response__loop:
            in a, (c)  ; read FDC main status register
            cp #c0  ; "data ready" & "data direction from fdc to cpu"
            jr c, get_floppy_controller_response__loop
            
            inc c  ; BC = I/O address of FDC data register
            in a, (c)  ; read data from FDC data register
            dec c  ; BC = I/O address of FDC main status register
            ld (hl), a  ; store result byte in buffer
            inc hl  ; increment buffer point
            inc d  ; increment count of result bytes received
            
            ld a, 5
get_floppy_controller_response__wait:
            dec a
            jr nz, get_floppy_controller_response__wait

            ; is FDC busy 
            ; - if set, FDC has not completed command and furthur result bytes are
            ; available to be read,
            ; - if clear, FDC has completed command and all result bytes have been
            ; read by CPU

            in a, (c)  ; read FDC main status register
            and #10  ; "FDC busy"?
            jr nz, get_floppy_controller_response__loop
        
        pop hl
        ; HL = start of result phase data buffer
        ld a, (hl)  ; read first status byte (FDC status register 0)
        and #c0  ; isolate execution status
        dec hl
        ld (hl), d  ; store count
    pop de
    pop hl
    ret nz
    
    scf     
    ret     


;------------------------------------------------------------------------
; clear fdc interrupt
fw_c947_clear_fdc_interrupt:
    push bc
        ld bc, #fb7e  ; BC = I/O address of FDC main status register
clear_fdc_interrupt__loop:
        ld a, 8  ; sense interrupt status
        call fw_c95c_send_floppy_controller_command_byte  ; fdc: send command byte 
        call fw_c91c_get_floppy_controller_response
        cp #80  ; "invalid"?
        jr nz, clear_fdc_interrupt__loop
    pop bc
    ret     


;------------------------------------------------------------------------
; write XDPB parameter to FDC. It first gets the parameter from the XDPB, and then sends it to the
; floppy disk controller.
;
; input:
; - bc: I/O address of FDC main status register
; - a: XDPB parameter index
fw_c959_send_floppy_controller_xdpb_parameter:
    call fw_ca5c_get_xdpb_parameter_value  ; get XDPB parameter by index
    ; jr fw_c95c_send_floppy_controller_command_byte


;------------------------------------------------------------------------
; fdc: send command byte
;
; input:
; - bc: I/O address of FDC main status register
; - a: data byte to write to FDC
fw_c95c_send_floppy_controller_command_byte:
    push af
        push af
send_floppy_controller_command_byte__wait:
            ; fdc ready to accept data?
            in a, (c)  ; read FDC main status register
            add a, a  ; transfer bit 7 ("data ready") to carry
            jr nc, send_floppy_controller_command_byte__wait
            ; data direction to fdc?
            add a, a  ; transfer bit 6 ("data direction") to carry
            jr nc, send_floppy_controller_command_byte__continue         
            ; conditions not met: fail
        pop af
    pop af
    ret     
send_floppy_controller_command_byte__continue:
        ; conditions match to write command byte to fdc
        pop af
        inc c  ; BC = I/O address for FDC data register
        out (c), a  ; write data to FDC data register
        dec c  ; BC = I/O address for FDC main status register
        ; delay
        ld a, 5
send_floppy_controller_command_byte__delay:
        dec a
        nop     
        jr nz, send_floppy_controller_command_byte__delay
    pop af  ; success
    ret     


;------------------------------------------------------------------------
; Note: What this method does with the stack is horrible. It might have been "acceptable" back in the day,
;       although I doubt it. This is not the way to code things.
fw_c976_spin_motor_up_if_off:
    ld (memory_be76_sector_buffer_ptr), hl
    ex (sp), hl  ; hl = return address, and (memory_be76_sector_buffer_ptr) goes to the stack
    push de
    push bc
    ld (memory_be64_temporary_stack), sp
    push hl  ; push the return address again
    ld hl, fw_c9ad_restore_stack_stop_motor_and_get_error_if_any
    ex (sp), hl  ; hl = return address (and fw_c9ad_restore_stack_stop_motor_and_get_error_if_any goes to the stack, so that the stack is fixed again after we are done here)
    push hl  ; push the return address again
    push de
    push bc
    push af
        call fw_c9df_delete_motor_ticker  ; delete ticker

        ; is motor already on?
        ld a, (memory_be5f_motor_on_off)  ; get motor state flag
        or a
        jr nz, spin_motor_up_if_off__motor_on
        
        ; motor wasn't already on, switch it on
        ld bc, #fa7e ; motor on
        ld a, 1
        out (c), a

        ; install ticker
        ld de, (memory_be44_motor_spinup_delay)
        call fw_c9cd_install_motor_on_off_ticker

        ; wait until motor flag signals drive is on
spin_motor_up_if_off__wait_loop:
        ld a, (memory_be5f_motor_on_off)
        or a
        jr z, spin_motor_up_if_off__wait_loop

spin_motor_up_if_off__motor_on:
    pop af
    pop bc
    pop de
    ld hl, (memory_be76_sector_buffer_ptr)
    ret


;------------------------------------------------------------------------
fw_c9ad_restore_stack_stop_motor_and_get_error_if_any:
    ld sp, (memory_be64_temporary_stack)
    push af
        ld de, (memory_be46_motor_spin_down_delay)
        call fw_c9cd_install_motor_on_off_ticker  ; "turn motor off" ticker
    pop af
    pop bc
    pop de
    pop hl  ; hl = memory_be76_sector_buffer_ptr
    ld a, 0
    ret c
    ; An error occurred:
    ld hl, memory_be4c_fdc_response
    ld a, (hl)
    and #08  ; "not ready flag" of FDC status register 0
    inc hl
    or (hl)  ; or with FDC status register 1 (which contains error flags)
    or #40
    dec hl
    dec hl  ; hl = memory_be4b_result_phase_received_n_bytes
    ret     


;------------------------------------------------------------------------
; install motor on/off ticker
;
; input:
; - de: initial value for counter
fw_c9cd_install_motor_on_off_ticker:
    ld hl, memory_be67_ticker_block  ; address of event block
    ld bc, 0
    push hl
        inc hl
        inc hl
        di
        ld (hl), e  ; initial counter
        inc hl
        ld (hl), d
        inc hl
        ld (hl), c  ; reset count
        inc hl
        ld (hl), b
    pop hl
    ld de, address_of_the_first_ticker_block_in_chain  ; ticker list
    jp add_event_to_an_event_list  ; add event to list


;------------------------------------------------------------------------
; drive motor ticker function (this is what is executed at the VSYNC interrupt when the counter reaches 0)
fw_c9d6_motor_ticker:
    ld hl, memory_be5f_motor_on_off
    ld a, (hl)
    cpl     
    ld (hl), a  ; change motor flag state
    or a  ; new state is off?
    jr z, fw_c9e5_turn_motor_off  ; turn off motor, set flag, and delete ticker
    ; new state is on. 


;------------------------------------------------------------------------
; Removes the motor ticker function from the set of tickers to be executed by the interrupt.
fw_c9df_delete_motor_ticker:
    ld hl, memory_be67_ticker_block  ; address of event block
    ld de, address_of_the_first_ticker_block_in_chain
    call delete_event_from_list  ; remove event from list
    ret nc
    ex de, hl
    inc hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    ret


;------------------------------------------------------------------------
; turn off motor
fw_c9e5_turn_motor_off:
    ; delete ticker
    call fw_c9df_delete_motor_ticker

    ; turn off drive motor
    ld a, 0
    ld bc, #fa7e
    out (c), a

    ; set disk motor flag
    xor a
    ld (memory_be5f_motor_on_off), a
    ret     


;------------------------------------------------------------------------
; initial value for the XDPB for using the SYSTEM format:
fw_ca43_system_format_xdpb_values:
    db #24, #00, #03, #07, #00, #aa, #00, #3f
    db #00, #c0, #00, #10, #00, #02, #00, #41
    db #09, #2a, #52, #e5, #02, #04, #00, #00
    db #00


;------------------------------------------------------------------------
; get XDPB parameter by index
;
; input:
; - a: XDPB parameter index
; - e: drive
; output:
; - a: parameter data
; - hl, de preserved
; - Flags corrupt
fw_ca5c_get_xdpb_parameter_value:
    push hl
        call fw_ca63_get_xdpb_parameter_address
        ld a, (hl)
    pop hl
    ret     


;------------------------------------------------------------------------
; get address of XDPB parameter
;
; input:
; - a: XDPB parameter index
; - e: drive (e == 1 means drive 1, e != 1, means drive 0)
; output:
; - hl: address of XDPB parameter data
; - de preserved
; - flags corrupt
fw_ca63_get_xdpb_parameter_address:
    push de
        ld hl, (memory_be42_drive_0_xdpb_ptr) ; HL = XDPB for drive 0 
        dec e
        ld de, #0040 ; size of XDPB data
        jr nz, get_xdpb_parameter_address__drive_0
        add hl, de  ; HL = XDPB for drive 1
get_xdpb_parameter_address__drive_0:
        ld e, a
        add hl, de  ; add offset of XDPB parameter index
    pop de
    ret     


;------------------------------------------------------------------------
; bc += iy
fw_ca90_bc_pluseq_iy:
	push iy
    	ex (sp), hl  ; exchange HL with top of stack (HL now on top of stack)
    	add hl, bc
    	ld b, h  ; transfer result to BC
    	ld c, l
	pop hl  ; restore original HL value
	ret


;------------------------------------------------------------------------
; de += iy
fw_ca98_de_pluseq_iy:
    push iy
        ex (sp), hl
        add hl, de
        ex de, hl
    pop hl
    ret     


;------------------------------------------------------------------------
; HL = HL + IY
fw_ca9f_hl_pluseq_iy:
    push de
        push iy
        pop de
        add hl, de
    pop de
    ret     


;------------------------------------------------------------------------
; convert character to upper case 
;
; input:
; - a: character
; output:
; - a: character converted to uper case
fw_caa6_char_to_upper_case:
    cp 'a'
    ret c
    cp 'z'
    ret nc
    add a, #e0
    ret     


;------------------------------------------------------------------------
; clear memory
;
; input:
; - de: address
; - bc: length
fw_caaf_clear_memory:
    xor a
    ld (de), a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, fw_caaf_clear_memory
    ret     


;------------------------------------------------------------------------
; calculate return address (in case of an error)
fw_cd77_store_return_address_in_case_of_an_error:
    push hl
        ld hl, 6
        add hl, sp
        ld (iy + AMSDOS_work_RAM_offset_return_address), l
        ld (iy + AMSDOS_work_RAM_offset_return_address + 1), h
    pop hl
    ret     


;------------------------------------------------------------------------
fw_cd84_set_XDPB_for_reading:
    call fw_cd77_store_return_address_in_case_of_an_error
    push af
        ld a, (iy + AMSDOS_work_RAM_offset_openin_control_block)  ; drive number: #00: A, #01: B, #FF: not open
        jr set_XDPB_for_writing__entry_point


;------------------------------------------------------------------------
fw_cd8d_set_XDPB_for_writing:
	call fw_cd77_store_return_address_in_case_of_an_error
	push af
    	ld a, (iy + AMSDOS_work_RAM_offset_openout_control_block)
set_XDPB_for_writing__entry_point:
        cp #ff  ; is the drive open?
        jr z, fw_cdaa_return_with_error  ; not open!
        call fw_ce16_setup_XDPB_for_drive
    pop af
    ret     


;------------------------------------------------------------------------
; If the drive is already open for reading, returns with an error
fw_cd9d_check_if_drive_is_not_already_open_for_reading:
    ld a, (iy + AMSDOS_work_RAM_offset_openin_control_block)
    jr check_if_drive_is_not_already_open_for_writing__entry_point


;------------------------------------------------------------------------
; If the drive is already open for writing, returns with an error
fw_cda2_check_if_drive_is_not_already_open_for_writing:
    ld a, (iy + AMSDOS_work_RAM_offset_openout_control_block)
check_if_drive_is_not_already_open_for_writing__entry_point:
    call fw_cd77_store_return_address_in_case_of_an_error
    inc a
    ret z  ; return if drive is not open


;--------------------------------------------------------------------------
fw_cdaa_return_with_error:
fw_cdaf_return_with_error:
    cp a  ; nc


;--------------------------------------------------------------------------
; get stored return address
fw_cdb9_restore_return_address:
    ld l, (iy + AMSDOS_work_RAM_offset_return_address)
    ld h, (iy + AMSDOS_work_RAM_offset_return_address + 1)
    ld sp, hl
    ret     


;------------------------------------------------------------------------------
; setup XDPB for drive
;
; input:
; - bc: ptr to the FCB (file control block)
fw_ce14_setup_XDPB_for_drive_in_fcb:
    ld a, (bc)  ; drive number
    inc bc


;------------------------------------------------------------------------------
; setup XDPB for drive
;
; input:
; - a: drive number
fw_ce16_setup_XDPB_for_drive:
    push hl
    push de
    push bc
    push af
        ld c, a
        ld e, #ff
        ld a, (iy + AMSDOS_work_RAM_offset_openin_control_block)  ; drive number
        cp c
        jr z, setup_XDPB_for_drive__drive_number_set
        ld a, (iy + AMSDOS_work_RAM_offset_openout_control_block)  ; drive number
        cp c
        jr z, setup_XDPB_for_drive__drive_number_set
        ld e, 0
setup_XDPB_for_drive__drive_number_set:
        ; here: e = #ff if the drive matches that in openin or openout control block.
        ;       e = #00 otherwise.
        push de
        push bc
            call fw_c4f0_setup_XDPB  ; setup XDPB, and get address of drive's XDPB
        pop bc
        pop de
        
        ld a, h  ; offset is 0?
        or l
        jp z, fw_cdaa_return_with_error  ; display "Bad command" and quit command

        ; store address of drive's XDPB        
        ld (iy + AMSDOS_work_RAM_offset_DPH_ptr), l
        ld (iy + AMSDOS_work_RAM_offset_DPH_ptr + 1), h
        ld (iy + AMSDOS_work_RAM_offset_default_drive_flag), e
        ; store drive
        ld (iy + AMSDOS_work_RAM_offset_active_drive), c
    pop af
    pop bc
    pop de
    pop hl
    ret


;------------------------------------------------------------------------
; input:
; - de: address of 2k buffer
; - bc: ptr to the FCB + 1 (pointing at the user)
; output:
; - hl: points at user / filename in the file header
fw_ce48_create_openin_fileheader:
    ld hl, AMSDOS_work_RAM_offset_openin_file_header
    call create_openout_fileheader__entry_point
    push hl
        ld de, #0042
        add hl, de
        ld (hl), #80
    pop hl
    ret     


;------------------------------------------------------------------------
; input:
; - de: address of 2k buffer
; - bc: ptr to the FCB + 1 (pointing at the user)
; output:
; - hl: points at user / filename in the file header
fw_ce57_create_openout_fileheader:
    ld hl, AMSDOS_work_RAM_offset_openout_file_header
create_openout_fileheader__entry_point:
    push bc
        push de
            call fw_ca9f_hl_pluseq_iy ; hl = openout file header ptr.
            ld (hl), 0  ; access mode
            inc hl
            ld (hl), e  ; address of 2k buffer
            inc hl
            ld (hl), d
            inc hl
            ld (hl), e  ; address of 2k buffer
            inc hl
            ld (hl), d
            inc hl
            push hl
                push bc
                    ld bc, #0045
                    ex de, hl
                    call fw_caaf_clear_memory  ; 69 bytes in the openout file header ptr (the rest of the fileheader wich is 74 bytes)
                pop bc
                ld h, b  ; hl = user / filename ptr
                ld l, c
            pop de
            push de
                ld bc, 12
                ldir  ; copy user (1 byte) + filename (8 + 3 bytes).
            pop hl  ; hl = openout file header ptr + 5 (pointing at user number and filename)
        pop de  ; de = address of 2k buffer
        push hl
            ld bc, 18
            add hl, bc  ; hl = openout file header ptr + 23
            ld (hl), #16  ; file type (strange, since #16 does not seem to be a valid file type,
                          ; maybe it's just an initialization value that is overwriten later ????)
            inc hl
            inc hl
            inc hl
            ld (hl), e  ; load address in memory
            inc hl
            ld (hl), d
            inc hl
            ld (hl), #ff  ; first block
        pop hl
    pop bc
    ret     


;------------------------------------------------------------------------
; generate AMSDOS file header checksum
;
; input:
; - hl: address of header + 5 (pointing at filename)
; output:
; - de: generated checksum
; - hl: address of header + #48 (ptr to checksum)
fw_ce92_generate_header_16bit_checksum:
    push hl
        ld hl, 0  ; initialise checksum
        ld d, h
        ld b, #43  ; number of bytes
generate_header_16bit_checksum__loop:
        ex (sp), hl  ; get address from stack
        ld a, (hl)  ; get byte
        inc hl  ; increment pointer
        ex (sp), hl  ; store address back to stack
        ld e, a
        add hl, de  ; update checksum
        djnz generate_header_16bit_checksum__loop
        ex de, hl  ; de = checksum
    pop hl
    ret     


;------------------------------------------------------------------------
; generate and store checksum
;
; input:
; - hl: address of file header + 5 (pointing at filename)
fw_cea4_update_header_checksum_and_write_first_file_record:
	push hl
    	call fw_ce92_generate_header_16bit_checksum  ; generate checksum
    	ld (hl), e  ; store checksum
    	inc hl
    	ld (hl), d
	pop hl
    push hl
        ld de, AMSDOS_work_RAM_offset_openout_control_block
        call fw_ca98_de_pluseq_iy  ; de = OPENOUT FCB (file control block)
        call fw_d79c_clear_FCB_previously_accessed_records
        call fw_d410_get_file_next_record_index
        ex de, hl  ; de = index of the first record of the file
    pop hl
    ld c, 0
    jp c, fw_d9f3_write_record_from_record_index_to_next_sector_track
    jp fw_cdaf_return_with_error  ; "Bad command"


;------------------------------------------------------------------------
; BIOS: CAS IN OPEN
;
; input:
; - b: filename length in characters
; - hl: address of filename
; - de: address of 2K buffer
; - iy: AMSDOS work RAM
fw_ceaf_cas_in_open:
    call fw_cd9d_check_if_drive_is_not_already_open_for_reading
    push de
        call fw_da6f_normalize_file_name_into_FCB  ; bc = FCB ptr
        call fw_ce14_setup_XDPB_for_drive_in_fcb

        ; Note: here the firmware had some code in case an extension was not specified,
        ;       which I have removed. Extensions should always be specified for any normal use.
        call fw_d651_check_if_filename_exists
        jp nc, fw_cdaa_return_with_error  ; "<filename> not found"
    pop de
    call fw_ce48_create_openin_fileheader
    push hl
	    ld de, AMSDOS_work_RAM_offset_openin_control_block
	    call fw_ca98_de_pluseq_iy  ; de = openin FCB
	    dec bc
	    ld a, (bc)
	    ld (de), a
	    
	    call fw_d79c_clear_FCB_previously_accessed_records
	    
	    ld hl, AMSDOS_work_RAM_offset_tmp_record_buffer
	    call fw_ca9f_hl_pluseq_iy  ; hl = tmp record buffer
	    
	    call fw_d392_read_next_record_from_file_into_the_tmp_record_buffer
	    jr nc, cas_in_open__continue
	    ; We could not load the first record:
	    push hl
	    push de
	        call fw_ce92_generate_header_16bit_checksum  ; generate file header checksum
	        call fw_dbf9_ld_word_from_hl  ; hl = stored checksum
	        call fw_dbf3_cp_hl_de
	    pop de
	    pop hl
	    jr nz, cas_in_open__no_header  ; if the checksum does not match, assume there is no header

		; file has a header
	    ld de, AMSDOS_work_RAM_offset_openin_file_header + 5
	    call fw_ca98_de_pluseq_iy  ; DE = IY + DE
	    ld bc, #0045
	    ldir    
	    jr cas_in_open__continue

cas_in_open__no_header:
        ; file doesn't have a header
    	call fw_d79c_clear_FCB_previously_accessed_records
cas_in_open__continue
    pop hl
    ; HL = address of in-memory file header
    push hl
	    ld de, #0015
	    add hl, de  ; hl = load address ptr inside the header
	    ld e, (hl)
	    inc hl
	    ld d, (hl)  ; de = load address (from header)
	    inc hl
	    inc hl
	    ld c, (hl)
	    inc hl
	    ld b, (hl)  ; bc = filesize (excluding header) (from header)
    pop hl  ; HL = address of buffer containing header
    scf  ; carry true
    sbc a, a  ; zero false
    ld a, (iy + #67)  ; A = file type (from header)
    ret     


;------------------------------------------------------------------------
; BIOS: CAS OUT OPEN
;
; input:
; - hl: address of filename
; - b: length of filename
; - de: address of 2K buffer
; - iy: AMSDOS work RAM
fw_cf37_cas_out_open:
	call fw_cda2_check_if_drive_is_not_already_open_for_writing
	push de
        ; the drive, user number and normalized name are created into AMSDOS_work_RAM_offset_tmp_filename_buffer:
		call fw_da6a_normalize_file_name_into_FCB_and_clear_extent_info  ; bc = FCB ptr
		call fw_ce14_setup_XDPB_for_drive_in_fcb  ; bc += 1 as a side effect
	pop de
    ; here bc = ptr to FCB + 1 (pointing at the user)
	call fw_ce57_create_openout_fileheader  ; hl = user / filename ptr
	push hl
		call fw_d2ab_replace_extension_with_dollars
		call fw_d676_write_sector_and_delete_matching_files
		ld h, b
		ld l, c
		dec hl  ; make hl point at the drive
		ld de, AMSDOS_work_RAM_offset_openout_control_block
		call fw_ca98_de_pluseq_iy  ; de = openout FCB
		ld bc, 13  ; copy drive, user and filename/extension
		ldir 
		ld bc, 23
		call fw_caaf_clear_memory ; clear the rest of the FCB
	pop hl
	scf  
	sbc a, a  ; z flag set, carry is preserved
	ret     


;------------------------------------------------------------------------
; BIOS: CAS IN DIRECT
;
; input:
; - hl: load address
; output:
; - hl: execution address
fw_cff5_cas_in_direct:
    call fw_cd84_set_XDPB_for_reading
    push hl
        ld hl, AMSDOS_work_RAM_offset_openin_file_header
        call fw_ca9f_hl_pluseq_iy  ; hl = openin file header
        ld a, (hl)
        cp 1  ; reading in character mode?
        jp z, fw_cdaa_return_with_error
        ld (hl), 2  ; read in direct mode

        ld de, AMSDOS_FILEHEADER_offset_file_size_2
        add hl, de
        ld e, (hl)
        inc hl
        ld d, (hl)  ; de = file size in bytes
    pop hl
    ; here:
    ; - hl: ptr to load the file
    ; - de: file size
    push de
        push hl
            ex de, hl
            ld a, 7
            call fw_dbeb_shift_hl_right_by_a  ; hl /= 128
            ld b, h
            ld c, l  ; bc: file size in records
        pop hl
        call fw_d05f_read_file_into_memory_buffer
    pop de
    jr nc, cas_in_direct__done_reading
    ld a, e
    and #7f
    jr z, cas_in_direct__done_reading
    ; are there any left over bytes in the last record (< 128 bytes)
    push af
    push hl
        ld hl, AMSDOS_work_RAM_offset_tmp_record_buffer
        call fw_ca9f_hl_pluseq_iy
        push hl
            ld bc, 1  ; just one record
            call fw_d05f_read_file_into_memory_buffer
        pop hl
    pop de
    pop bc
    jr nc, cas_in_direct__done_reading
    ; copy them from the temporary memory buffer to their target memory position:
    ld c, b
    ld b, 0
    ldir
cas_in_direct__done_reading:
    ; load execution address, and return:
    ld hl, AMSDOS_work_RAM_offset_openin_file_header + AMSDOS_FILEHEADER_offset_execution_entry_point
    call fw_ca9f_hl_pluseq_iy  ; hl = ptr to execution address (for executable binaries)
    scf     
    sbc a, a
    jp fw_dbf9_ld_word_from_hl


;-----------------------------------------------------------------------------
; Reads a whole file into memory
;
; input:
; - hl: tmp record buffer ptr
; - bc: filesize in sectors
read_file_into_memory_buffer__loop:
    call fw_d392_read_next_record_from_file_into_the_tmp_record_buffer
    ret nc

    ld de, AMSDOS_work_RAM_offset_openin_file_header + AMSDOS_FILEHEADER_offset_file_type
    call fw_ca98_de_pluseq_iy  ; de = ptr to filetype
    ld a, (de)
    rra  ; is it encrypted BASIC?
    call c, fw_d252_encrypt_decrypt_record_buffer
    ld de, #0080
    add hl, de
    dec bc
fw_d05f_read_file_into_memory_buffer:
    ld a, b
    or c
    jr nz, read_file_into_memory_buffer__loop
    scf
    ret     


;------------------------------------------------------------------------
; CAS OUT DIRECT
;
; input:
; - hl: load address
; - de: length
; - bc: execution address
; - a: type
fw_d0d8_cas_out_direct:
	call fw_cd8d_set_XDPB_for_writing
	push af
		push hl
			push de
				ld hl, AMSDOS_work_RAM_offset_openout_file_header
				call fw_ca9f_hl_pluseq_iy  ; hl = openout file header
				ld a, (hl)  ; a = access mode
				cp 1
				jp z, fw_cdaa_return_with_error  ; if acccess mode is "CHAR", return with error
				ld (hl), 2  ; set access mode to "DIRECT"
				ld de, AMSDOS_FILEHEADER_offset_execution_entry_point + 1
				add hl, de
				ld (hl), b
				dec hl
				ld (hl), c  ; save execution entry point
			pop bc  ; bc = length
			dec hl  
			ld (hl), b
			dec hl  ;  here hl is "header + AMSDOS_FILEHEADER_offset_file_size_1"
			ld (hl), c  ; save length in bytes
			ld de, 41
			add hl, de  ; hl = header + AMSDOS_FILEHEADER_offset_file_size_2 + 1
			ld (hl), b
			dec hl
			ld (hl), c  ; save length in bytes (this seems to be stored twice in the fileheader, for some reason)
			ld de, -45
			add hl, de  ; hl = header + AMSDOS_FILEHEADER_offset_data_length
			ld (hl), c
			inc hl
			ld (hl), b  ; save data length
		pop bc  ; bc = load address
		inc hl  ; hl = header + AMSDOS_FILEHEADER_offset_load_address
		ld (hl), c
		inc hl
		ld (hl), b  ; save load address
		ld de, -AMSDOS_FILEHEADER_offset_load_address
		add hl, de  ; hl = header + 1
		ld (hl), c
		inc hl
		ld (hl), b  ; ptr to 2k work buffer
	pop af
	ld de, AMSDOS_FILEHEADER_offset_file_type - 2
	add hl, de
	ld (hl), a  ; save file type
cas_out_direct__flush_content_in_file_header_to_disk:
	push iy
	pop de
	ld hl, AMSDOS_work_RAM_offset_openout_file_header + AMSDOS_FILEHEADER_offset_first_block  ; = #00b6
	add hl, de
	ld a, (hl)  ; a = first block
	or a
	jr z, cas_out_direct__first_block_set
    ; The first block is not 0
	ld hl, AMSDOS_work_RAM_offset_openout_file_header + AMSDOS_FILEHEADER_offset_file_type  ; = #00b1
	add hl, de
	ld a, (hl)  ; a = file type
	and #0f
	cp #06
	jr z, cas_out_direct__first_block_set
    ; file type is of the form xxxxxyyx, where y != 0, we need to skip one block
	ld hl, AMSDOS_work_RAM_offset_openout_control_block
	add hl, de
	push de
		ex de, hl
		call fw_d7a7_inc_num_previously_accessed_records
		call fw_d77d_inc_num_records_in_current_extent
	pop de
cas_out_direct__first_block_set:
	ld hl, AMSDOS_work_RAM_offset_openout_file_header + AMSDOS_FILEHEADER_offset_data_length  ; = #00b2
	add hl, de
	push hl
		ld e, (hl)
		inc hl
		ld d, (hl)  ; de = data length
		ld bc, -24
		add hl, bc
		call fw_dbf9_ld_word_from_hl  ; LD HL,(HL)  ; hl = ptr to 2k work buffer
		push hl
			call fw_d164_write_file_from_memory_buffer
		pop bc  ; bc = ptr to 2k work buffer
	pop hl
	ld (hl), 0
	inc hl
	ld (hl), 0  ; set data length to 0
	inc hl
	inc hl
	inc hl
	ld (hl), 0  ; set first block to 0
	ld de, -25
	add hl, de  ; hl = header + AMSDOS_FILEHEADER_offset_current_byte_in_2k_buffer_ptr
	ld (hl), c
	inc hl
	ld (hl), b  ; set ptr to current byte in 2k buffer
	scf  
	sbc a, a  ; preserve carry, set z flag
	ret     


;-----------------------------------------------------------------------------
; Writes a file of size 'de' from the memory buffer to disk
;
; input:
; - de: file size in bytes
; - hl: ptr to the memory buffer
fw_d164_write_file_from_memory_buffer:
	push de
    	ld a, 7
    	ex de, hl
        	call fw_dbeb_shift_hl_right_by_a  ; hl /= 128
    	ex de, hl
    	ld b, d
    	ld c, e  ; bc = file size in records
    	call write_file_from_memory_buffer__loop_pre_entry_point
	pop bc
    ; is there a left-over amount of bytes (< 128) for a final record?
	ld a, c
	and #7f
	ret z

	ld c, a
	ld b, 0
	ld de, AMSDOS_work_RAM_offset_tmp_record_buffer
	call fw_ca98_de_pluseq_iy  ; de = tmp record buffer ptr
	push de
    	ldir
    	ld a, #1a  ; add one additional byte at the end of the file
    	ld (de), a
	pop hl
	inc bc  ; bc = 1
write_file_from_memory_buffer__loop_pre_entry_point:
	jr write_file_from_memory_buffer__loop_entry_point
write_file_from_memory_buffer__loop:
	push hl
    	ld de, AMSDOS_work_RAM_offset_openout_file_header + AMSDOS_FILEHEADER_offset_file_type
    	call fw_ca98_de_pluseq_iy  ; de = ptr to file type in the openout file header
    	ld a, (de)  ; a = file type
    	rra     
    	jr nc, write_file_from_memory_buffer__decrypted
        ; encrypted record, decrypt:
    	push bc
        	ld de, AMSDOS_work_RAM_offset_tmp_record_buffer
        	call fw_ca98_de_pluseq_iy  ; de = tmp record buffer ptr
        	push de
            	ld bc, 128
            	ldir
        	pop hl
    	pop bc
    	call fw_d252_encrypt_decrypt_record_buffer
write_file_from_memory_buffer__decrypted:
    	call fw_d3af_write_next_record_to_file_from_the_tmp_record_buffer
	pop hl
	ld de, 128
	add hl, de
	dec bc  ; decrease the number of records to save
write_file_from_memory_buffer__loop_entry_point:
    ; are we done writing records?
	ld a, b
	or c
	jr nz, write_file_from_memory_buffer__loop
	ret     


;------------------------------------------------------------------------
; BIOS: CAS IN CLOSE
fw_d1b6_cas_in_close:
    call fw_cd84_set_XDPB_for_reading
    call fw_c9e5_turn_motor_off
    ld (iy + AMSDOS_work_RAM_offset_openin_control_block), #ff  ; set drive to #ff
    jr cas_out_close__done


;------------------------------------------------------------------------
; CBIOS: AS OUT ABANDON
fw_d1bc_cas_out_abandon:
	call fw_cd8d_set_XDPB_for_writing
	ld de, AMSDOS_work_RAM_offset_openout_control_block + 1
	call fw_ca98_de_pluseq_iy ; DE = IY+DE
	xor a
	call fw_d83c_update_block_allocation_table_for_current_extent
	dec de  ; de = ptr to file control block (FCB)
	ld a, #ff
	ld (de), a  ; drive number in the FCB = #ff
	call fw_c51f_write_current_sector_and_reset_track
	jr cas_out_close__done


;------------------------------------------------------------------------
; BIOS: CAS OUT CLOSE
fw_d1d8_cas_out_close:
	ld hl, AMSDOS_work_RAM_offset_openout_file_header + AMSDOS_FILEHEADER_offset_file_size_2  ; = #00df
	call fw_ca9f_hl_pluseq_iy
    ; check if the file size is 0
	ld a, (hl)
	inc hl
	or (hl)
	inc hl
	or (hl)
	jr z, fw_d1bc_cas_out_abandon  ; if file size is 0, abandon
	call fw_cd8d_set_XDPB_for_writing
	call cas_out_direct__flush_content_in_file_header_to_disk
	ld de, AMSDOS_work_RAM_offset_openout_control_block
	call fw_ca98_de_pluseq_iy
	push de
    	call fw_d78c_init_next_available_directory_from_FCB
    	ld bc, AMSDOS_work_RAM_offset_openout_file_header + AMSDOS_FILEHEADER_offset_filename
    	call fw_ca90_bc_pluseq_iy  ; bc: ptr to the filename in the openout file header
    	ld hl, 18
    	add hl, bc  ; hl ptr to the file type in the openout file header
    	ld e, (hl)  ; e = filetype
    	ld hl, 9 
    	add hl, bc  ; ptr to the middle character of the extension
    	ld a, (hl)  ; a = middle character of the extension
    	inc a
    	jr nz, cas_out_close__file_already_has_extension
        ; file has #ff as extension, figure out the correct extension:
    	ld a, e
    	and #0e
    	jr nz, cas_out_close__file_is_not_BASIC
        ; it's a BASIC file:
    	call fw_d2b3_replace_extension_with_bas
    	jr cas_out_close__file_already_has_extension
cas_out_close__file_is_not_BASIC:
    	cp 2
    	jr nz, cas_out_close__file_is_not_binary
        ; it's a binary file
    	call fw_d2b7_replace_extension_with_bin
    	jr cas_out_close__file_already_has_extension
cas_out_close__file_is_not_binary:
        ; not BASIC, not binary, set an empty extension
    	call fw_d2a8_replace_extension_with_spaces
cas_out_close__file_already_has_extension:
    	ld h, b
    	ld l, c  ; hl: ptr to the filename in the openout file header
    	ld a, e  ; a = filetype
    	and #0f
    	cp #06
    	call nz, fw_cea4_update_header_checksum_and_write_first_file_record  ; generate and store checksum
	pop bc  ; bc = openout control block ptr
	ld a, #ff
	ld (bc), a  ; set drive to #ff, which means "not open"
	inc bc
	call fw_d2da_write_file_renaming_previous_as_BAK_if_needed
cas_out_close__done:
    scf
    sbc a, a
    ret     


;----------------------------------------------------------------
; Closes any file open in the current drive
fw_d233_close_file_in_current_drive:
    ld h, (iy + AMSDOS_work_RAM_offset_active_drive)
    ld (iy + AMSDOS_work_RAM_offset_default_drive_flag), 0
    ld de, AMSDOS_work_RAM_offset_openin_control_block
    call fw_d243_close_file_in_drive
    ld de, AMSDOS_work_RAM_offset_openout_control_block
    ; jr fw_d243_close_file_in_drive


;----------------------------------------------------------------
; input:
; - h: active drive
; - de: openin or openout work RAM offset
fw_d243_close_file_in_drive:
    call fw_ca98_de_pluseq_iy  ; de = openin/out control block
    ld a, (de)  ; drive number
    cp h
    ret nz  ; not the current active drive

    ld a, #ff
    ld (de), a  ; mark drive number as "not open"
    inc de
    ret  ; "disk changed, closing <filename>"


;----------------------------------------------------------------
; This encrypts a 128 byte block. To do so, it uses two keys:
; - one of 11 bytes, and one of 13 bytes
; - 11 and 13 are coprimes, and hence, their combination results in a key of length 11 * 13 = 143, which is longer than 128 bytes,
;   which is enough.
; - encrypting twice, results in decrypting. So, the same encrypt method can also decrypt.
;
; input:
; - hl: ptr to the tmp record buffer to encrypt or decrypt
fw_d252_encrypt_decrypt_record_buffer:
    push hl
    push bc
    push hl
        ld de, #0101  ; d = 1, e = 1 (so that ix and hl will be set below)
        ld b, 128 + 1
        jr encrypt_decrypt_record_buffer__loop_start
encrypt_decrypt_record_buffer__loop:
        ex (sp), hl  ; hl = ptr to tmp record buffer
        ld a, (hl)  ; was "rst #20" in the AMSDOS original
        ex (sp), hl  ; restore 'encryptedbasic_key2' in hl
        ; a = [record buffer ptr] XOR [encryptedbasic_key1] XOR [encryptedbasic_key2]
        xor (hl)
        xor (ix)
        ; update the record buffer ptr data, and move on to the next byte
        ex (sp), hl
        ld (hl), a
        inc hl
        ex (sp), hl
        inc ix
        inc hl
encrypt_decrypt_record_buffer__loop_start:
        dec d
        jr nz, encrypt_decrypt_record_buffer__key1_reset_skip
        ld d, 11  ; length of 'encryptedbasic_key1'
        ld ix, fw_d281_encryptedbasic_key1
encrypt_decrypt_record_buffer__key1_reset_skip:
        dec e
        jr nz, encrypt_decrypt_record_buffer__key2_reset_skip
        ld e, 13  ; length of 'encryptedbasic_key2'
        ld hl, fw_d28c_encryptedbasic_key2
encrypt_decrypt_record_buffer__key2_reset_skip:
        djnz encrypt_decrypt_record_buffer__loop
    pop hl
    pop de
    pop hl
    ret     


;----------------------------------------------------------------
; Keys to encrypt BASIC programs:
fw_d281_encryptedbasic_key1:
    db #49, #b1, #36, #f0, #2e, #1e, #06, #2a, #28, #19, #ea
fw_d28c_encryptedbasic_key2:
    db #e2, #9d, #db, #1a, #42, #29, #39, #c6, #b3, #c6, #90, #45, #8a


;----------------------------------------------------------------
; extensions list
fw_d299_extension_list:
    db "   "
    db "$$$"
    db "BAK"
    db "BAS"
    db "BIN"


;----------------------------------------------------------------
; "   "
;
; input:
; - bc: address of filename
fw_d2a8_replace_extension_with_spaces:
	xor a
	jr fw_d2b9_replace_extension_to_a


;----------------------------------------------------------------
; "$$$"
;
; input:
; - bc: address of filename
fw_d2ab_replace_extension_with_dollars:
	ld a, #03
	jr fw_d2b9_replace_extension_to_a


;----------------------------------------------------------------
; "BAK"
;
; input:
; - bc: address of filename
fw_d2af_replace_extension_with_bak:
	ld a, #06
	jr fw_d2b9_replace_extension_to_a


;----------------------------------------------------------------
; "BAS"
;
; input:
; - bc: address of filename
fw_d2b3_replace_extension_with_bas:
	ld a, #09
	jr fw_d2b9_replace_extension_to_a


;----------------------------------------------------------------
; "BIN"
;
; input:
; - bc: address of filename
fw_d2b7_replace_extension_with_bin:
	ld a, #0c


;----------------------------------------------------------------
; input:
; - a: offset into table
; - bc: address of filename
fw_d2b9_replace_extension_to_a:
	push de
		; add base of table (fw_d299_extension_list)
		add a, fw_d299_extension_list % 256
		ld e, a
		adc a, fw_d299_extension_list / 256
		sub e
		ld d, a
		jr fw_d2ca_replace_name_extension


;----------------------------------------------------------------
; Copies the extension in the openout header to the filename pointed to by hl.
;
; input:
; - bc: address of the filename (user number + 8.3 format)
fw_d2c3_replace_name_extension_with_the_one_in_openout_header:
	push de
		ld de, AMSDOS_work_RAM_offset_openout_file_header + 14
		call fw_ca98_de_pluseq_iy  ;   ; de = extension part of the filename in the openout header
        ; jr fw_d2ca_replace_name_extension


;----------------------------------------------------------------
; replace extension
;
; input:
; - bc: address of filename (user number + 8.3 format)
; - de: pointer to replacement extension
fw_d2ca_replace_name_extension:
		push hl
		push bc
			ld hl, 9  ; add offset to get pointer to extension (8 + 1, to skip user number)
			add hl, bc

			ld bc, 3  ; length of extension
			ex de, hl
			ldir  ; copy bytes
		pop bc
		pop hl
	pop de
	ret


;----------------------------------------------------------------
; This method first looks to see if there is already a file with the name of the file we are trying
; to save, and also if there is a .BAK file (same name as target, just with .BAK extension), and then
; tries to save the new file, renaming the previous as .BAK (deleting the old .BAK if necessary), and
; having in mind if files are read-only, or read-write.
;
; input:
; - bc: openout control block ptr + 1
fw_d2da_write_file_renaming_previous_as_BAK_if_needed:
	ld hl, 12
	add hl, bc  ; hl = ptr to the last character of the filename name (not the extension).
	ld (hl), #ff  ; set the last character of the filename to #ff
	inc hl
	inc hl
	ld (hl), #ff  ; set the middle character of the filename to #ff
	call fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table
	push hl  ; hl should be -1 here
    	ld hl, 0
write_file_renaming_previous_as_BAK_if_needed__loop:
    	ex (sp), hl  ; hl = last directory entry number
    	call fw_d6a2_get_next_directory_entry_ptr  ; de = directory entry/catalog ptr, hl++
    	ex (sp), hl  ; directory number back to the stack
    	jr nc, write_file_renaming_previous_as_BAK_if_needed__loop_over
    	call fw_d2af_replace_extension_with_bak
    	call fw_d7d8_user_filename_extent_in_control_block_match_catalog
    	jr nc, write_file_renaming_previous_as_BAK_if_needed__no_BAK
        ; catalog matches with openout control block ptr name, but with .BAK extension
    	ld h, 1
    	call fw_d9d9_is_file_read_only  ; get read/write state of file
    	jr c, write_file_renaming_previous_as_BAK_if_needed__no_BAK  ; jump if read-only
    	inc h
write_file_renaming_previous_as_BAK_if_needed__no_BAK:
        ; here: h = 1 if file is read only, and 2 if it's read/write
    	call fw_d2c3_replace_name_extension_with_the_one_in_openout_header
    	call fw_d7d8_user_filename_extent_in_control_block_match_catalog
    	jr nc, write_file_renaming_previous_as_BAK_if_needed__no_target
        ; catalog matches with openout ptr name, with the openout header extension
    	ld l, 1
    	call fw_d9d9_is_file_read_only  ; get read/write state of file
    	jr c, write_file_renaming_previous_as_BAK_if_needed__no_target  ; jump if read-only
    	inc l
write_file_renaming_previous_as_BAK_if_needed__no_target:
        ; here l = 1 if file is read-only, and 2 if it's read/write
    	ld a, h
    	or a
    	jr z, write_file_renaming_previous_as_BAK_if_needed__loop
    	ld a, l
    	or a
    	jr z, write_file_renaming_previous_as_BAK_if_needed__loop
        ; if we are here, h != 0 and l != 0, which means that we have found both a file with the .BAK extension,
        ; and one with the target extension
write_file_renaming_previous_as_BAK_if_needed__loop_over:
	pop af
	ld a, l
	or a
	jr z, fw_d362_write_current_sector_and_rename_any_matching_dollar_to_target_and_target_to_bak
    ; if we are here, there was a file with the target name we are writing:
	dec a
	jr z, write_file_renaming_previous_as_BAK_if_needed__existing_target_is_read_only
    ; and it was read/write:
	ld a, h
	or a
	jr z, fw_d362_write_current_sector_and_rename_any_matching_dollar_to_target_and_target_to_bak
    ; there was already a .BAK file:
	dec a
	jr z, fw_d36e_write_current_sector_and_rename_any_matching_dollar_to_target
    ; the .BAK file was read/write. Here, we will delete the old .BAK file, rename the file that matches the
    ; target name as .BAK, and then write the target file.
	call fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table
write_file_renaming_previous_as_BAK_if_needed__loop2:
	call fw_d6a2_get_next_directory_entry_ptr
	ret nc

	call fw_d335_rename_previous_file_as_bak
	jr write_file_renaming_previous_as_BAK_if_needed__loop2

write_file_renaming_previous_as_BAK_if_needed__existing_target_is_read_only:
    call fw_d2c3_replace_name_extension_with_the_one_in_openout_header
    ld d, b
    ld e, c
    jp fw_cdaa_return_with_error  ; "<filename> is read only"


;----------------------------------------------------------------
; - If the catalog contains a file that has the same name as the one we are trying to save, but .BAK, delete the .BAK
; - If the catalog matches the file we are trying to write, nor .$$$, rename the existing file as .BAK
; - If the catalog matches with the same name but .$$$, rename to the name we want.
;
; input:
; - bc: openout control block ptr + 1
; - de: directory entry/catalog ptr
fw_d335_rename_previous_file_as_bak:
    ; renames file in openout control block to .BAK, and if there was a match, remove the file and return
	call fw_d2af_replace_extension_with_bak
	call fw_d7d8_user_filename_extent_in_control_block_match_catalog
	jp c, fw_d4aa_erase_file_and_return_if_error
rename_previous_file_as_bak__rename_dollar_to_target_and_target_to_bak:
    ; catalog does not match the .BAK file
	call fw_d351_if_catalog_matches_name_with_dollars_rename_to_target_extension
	ret c
    ; catalog does not match the .$$$ file
	call fw_d2c3_replace_name_extension_with_the_one_in_openout_header
	call fw_d7d8_user_filename_extent_in_control_block_match_catalog
	ret nc
    ; catalog matches with the original extension:
	push bc
    	ld b, d  ; bc = directory entry/catalog ptr
    	ld c, e
    	call fw_d2af_replace_extension_with_bak
    	jr if_catalog_matches_name_with_dollars_rename_to_target_extension__write_directory_buffer  ; write directory buffer and update checksum


;----------------------------------------------------------------
; If the catalog in 'de' matches the name of the file we are trying to write, except that it ends in .$$$,
; rename the .$$$ to the name we want.
;
; input:
; - bc: openout control block ptr + 1
; - de: directory entry/catalog ptr
fw_d351_if_catalog_matches_name_with_dollars_rename_to_target_extension:         
	call fw_d2ab_replace_extension_with_dollars
	call fw_d7d8_user_filename_extent_in_control_block_match_catalog
	ret nc
    ; The catalog matches the .$$$ filename
	push bc
    	ld b, d  ; bc = directory entry/catalog ptr
    	ld c, e
    	call fw_d2c3_replace_name_extension_with_the_one_in_openout_header  ; restore the original extension
if_catalog_matches_name_with_dollars_rename_to_target_extension__write_directory_buffer:
	pop bc
	jp fw_d97a_write_directory_buffer_and_update_checksum


;----------------------------------------------------------------
; This is the same as "fw_d36e_write_current_sector_and_rename_any_matching_dollar_to_target", 
; except that if there is a file already existing in the disk with the same name as the
; one we are trying to write, it will rename it to .BAK.
;
; input:
; - bc: openout control block ptr + 1
; - de: directory entry/catalog ptr
fw_d362_write_current_sector_and_rename_any_matching_dollar_to_target_and_target_to_bak:
	call fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table
write_current_sector_and_rename_any_matching_dollar_to_target_and_target_to_bak__loop:
	call fw_d6a2_get_next_directory_entry_ptr
	ret nc

	call rename_previous_file_as_bak__rename_dollar_to_target_and_target_to_bak
	jr write_current_sector_and_rename_any_matching_dollar_to_target_and_target_to_bak__loop


;----------------------------------------------------------------
; input:
; - bc: openout control block ptr + 1
; - de: directory entry/catalog ptr
fw_d36e_write_current_sector_and_rename_any_matching_dollar_to_target:
	call fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table
write_current_sector_and_rename_any_matching_dollar_to_target__loop:
	call fw_d6a2_get_next_directory_entry_ptr
	ret nc

	call fw_d37a_paketdsk_rename_previous_dollar_to_target_or_erase_matching_target
	jr write_current_sector_and_rename_any_matching_dollar_to_target__loop


;----------------------------------------------------------------
; input:
; - bc: openout control block ptr + 1
; - de: directory entry/catalog ptr
fw_d37a_paketdsk_rename_previous_dollar_to_target_or_erase_matching_target:
	call fw_d351_if_catalog_matches_name_with_dollars_rename_to_target_extension
	ret c

	call fw_d2c3_replace_name_extension_with_the_one_in_openout_header
	call fw_d7d8_user_filename_extent_in_control_block_match_catalog
	jp c, fw_d4aa_erase_file_and_return_if_error
	ret     


;--------------------------------------------------------------------------
; input:
; - hl: tmp record buffer ptr
fw_d392_read_next_record_from_file_into_the_tmp_record_buffer:
    push hl
    push de
    push bc
        push hl
            ld de, AMSDOS_work_RAM_offset_openin_control_block
            call fw_ca98_de_pluseq_iy  ; de = openin control block ptr
            call fw_d410_get_file_next_record_index
            jr nc, read_next_record_from_file_into_the_tmp_record_buffer__error
            ex de, hl  ; de = index of next record to use; hl = openin control block ptr
            ex (sp), hl  ; hl = tmp record buffer; openin control block ptr goes to the stack
            call fw_d9e8_read_sector_from_current_sector_index
        pop de
        jr write_next_record_to_file_from_the_tmp_record_buffer__inc_accessed_records  ; this increments the number of previously accessed records, and returns
read_next_record_from_file_into_the_tmp_record_buffer__error:
        pop hl
    pop bc
    pop de
    pop hl
    or a
    ret     


;--------------------------------------------------------------------------
; input:
; - hl: tmp record buffer ptr
fw_d3af_write_next_record_to_file_from_the_tmp_record_buffer:
	push hl
	push de
	push bc
    	push hl
        	ld de, AMSDOS_work_RAM_offset_openout_control_block
        	call fw_ca98_de_pluseq_iy  ; de = openout control block ptr
        	call fw_d6c8_get_extent_number_for_a_new_block  ; c = extent number, b = 0
        	jr c, write_next_record_to_file_from_the_tmp_record_buffer__extent_ready
        	jp nz, fw_cdaa_return_with_error  ; "disk full"
            ; We need a new extent:
        	call fw_d78c_init_next_available_directory_from_FCB
        	call fw_d6fa_init_extent_info_in_fcb
write_next_record_to_file_from_the_tmp_record_buffer__extent_ready:
            call fw_d72f_next_record_in_current_block_or_new_block
            ld c, 0
            jr c, write_next_record_to_file_from_the_tmp_record_buffer__block_ready
            ; We need to find a new block!
            ; If we are here, hl = ptr to the block number in the current extent (inside the FCB block)
            push de  ; here de = openin/out FCB block ptr
            	ex de, hl
            	   call fw_d893_find_free_block
            	ex de, hl  ; de = block index of the free block that was found
            	jp nc, fw_cdaa_return_with_error  ; "disk full"
            	ld (hl), e  ; save the index of the free block in the FCB
            	ld a, b  ; here, b = max block number // 256
            	or a
            	jr z, write_next_record_to_file_from_the_tmp_record_buffer__block_number_saved  ; if block numbers are 8bit, no need to update the next byte
            	inc hl
            	ld (hl), d
write_next_record_to_file_from_the_tmp_record_buffer__block_number_saved:
        	pop de
            ; We call this function again, which will give us the record number to use within the new block
        	call fw_d72f_next_record_in_current_block_or_new_block
        	ld c, 2
write_next_record_to_file_from_the_tmp_record_buffer__block_ready:
            ; There are still records available in the current block:
        	ex de, hl  ; hl = openout FCB block ptr, de = record index
        	ex (sp), hl  ; put "openout FCB block ptr" in the stack, and get hl = tmp record buffer ptr
        	call fw_d9f3_write_record_from_record_index_to_next_sector_track
	    pop de  ; recover openout FCB block ptr
    	call fw_d77d_inc_num_records_in_current_extent
write_next_record_to_file_from_the_tmp_record_buffer__inc_accessed_records:
        call fw_d7a7_inc_num_previously_accessed_records
    pop bc
    pop de
    pop hl
    scf     
    ret     


;--------------------------------------------------------------------------
; Get next record index of this file
;
; input:
; - de: openin/out control block ptr
; output:
; - if carry, z: hl: index of next record to use within the last used block
; - if no carry, z: we need to find a new block, hl: ptr to the block number in the current extent (inside the FCB block)
; - if no carry, nz: disk full
fw_d410_get_file_next_record_index:
    call fw_d6c8_get_extent_number_for_a_new_block
    jr c, get_file_next_record_index__extent_set
    ret nz  ; disk full
    ; Something does not add up, clear the extent info, and get the directory entry/catalog
    ; for the current file again from disk:
    call fw_d6fa_init_extent_info_in_fcb
    push de
        ld b, d
        ld c, e
        inc bc
        push bc
            call fw_d7b3_write_current_sector_and_find_matching_directory_entry
            ex de, hl  ; hl = directory entry/catalog ptr
        pop de
        call c, fw_dbdf_ldir32  ; copy the found directory entry/catalog to the openin/out control block
    pop de
get_file_next_record_index__extent_set:
    call c, fw_d70c_are_there_records_yet_to_be_accessed_in_the_file
    jp c, fw_d72f_next_record_in_current_block_or_new_block  ; if there are, get the next record index
    ret     


;------------------------------------------------------------------------
; Erases a file, and returns if the atempt failed (e.g. the file was read-only).
;
; input:
; - de: address of catalog/directory-entry structure
fw_d4aa_erase_file_and_return_if_error:
	call fw_d4b1_erase_file
	jp nc, fw_cdaa_return_with_error
	ret     


;------------------------------------------------------------------------
; Sets the user of a directory entry to #e5, and clears the block allocation table for
; all the blocks associated with a given directory entry.
;
; input:
; - de: address of catalog/directory-entry structure
fw_d4b1_erase_file:
	call fw_d9d9_is_file_read_only  ; get read/write state of file
	ccf     
    ret nc  ; "<filename> is read only"

	xor a
	call fw_d83c_update_block_allocation_table_for_current_extent
	ld a, #e5  ; USER #e5 is used for deleted files
	ld (de), a  ; set user to #e5
	jp fw_d97a_write_directory_buffer_and_update_checksum


;----------------------------------------------------------------------------
; check if filename exists
fw_d651_check_if_filename_exists:
    call fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table
    call fw_d698_directory_entry_search
    jr nc, check_if_filename_exists__set_default_drive_flag
    push hl
        ld hl, 9
        call fw_ca9f_hl_pluseq_iy
        ex de, hl
        call fw_dbdf_ldir32  ; copy 32 bytes from HL to DE 
    pop hl
    ld a, (iy + AMSDOS_work_RAM_offset_default_drive_flag)
    or a
    scf     
    ret nz
check_if_filename_exists__loop:
    call fw_d6a2_get_next_directory_entry_ptr
    jr c, check_if_filename_exists__loop
    scf
check_if_filename_exists__set_default_drive_flag:
    ld (iy + AMSDOS_work_RAM_offset_default_drive_flag), #ff
    ret     


;----------------------------------------------------------------------------
; Writes the current sector, and deletes all files that user/filename-match with 
; the info in the openout/in control block.
fw_d676_write_sector_and_delete_matching_files:
	call fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table
write_sector_and_delete_matching_files__loop:
	call fw_d698_directory_entry_search
	jr nc, check_if_filename_exists__set_default_drive_flag
	call fw_d4aa_erase_file_and_return_if_error
	jr write_sector_and_delete_matching_files__loop


;----------------------------------------------------------------------------
; Writes the current sector to disk, and set track to 0.
; If we wrote to the default drive, clear the DPH allocation table and set current directory to 0.
fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table:
    push bc
        call fw_c51f_write_current_sector_and_reset_track
    pop bc
    ld hl, #ffff
    ld a, (iy + AMSDOS_work_RAM_offset_default_drive_flag)
    or a
    ret nz

    push hl
        call fw_d814_clear_DPH_block_allocation_table
    pop hl
    jp fw_d9a8_set_DPH_set_directory_plus_one  ; current directory = 0


;----------------------------------------------------------------------------
; Searches for a directory entry/catalog that matches the info in the openout/in control block.
;
; input:
; - hl: directory entry/catalog number - 1
; output:
; - de: directory entry/catalog ptr
; - carry: success
; - no carry/: not found
fw_d698_directory_entry_search:
    call fw_d6a2_get_next_directory_entry_ptr
    ret nc

    call fw_d7d8_user_filename_extent_in_control_block_match_catalog
    jr nc, fw_d698_directory_entry_search
    ret     


;----------------------------------------------------------------------------
; Get a pointer to a directory entry/catalog number
; If we are requesting it for a drive that is not the default one, and it corresponds
; to a non-erased file, it'll mark all the used blocks of that file as used in the block allocation table.
;
; input:
; - hl: directory entry/catalog number - 1
; output:
; - de: directory entry/catalog ptr
; - carry: success
; - no carry: error
fw_d6a2_get_next_directory_entry_ptr:
    inc hl
    ld a, (iy + AMSDOS_work_RAM_offset_default_drive_flag)
    or a
    jr nz, get_next_directory_entry_ptr__default_drive  ; if we are in the default drive, jump
    ; we are not in the default drive:
    call fw_d91c_get_next_directory_entry_ptr_internal  ; de = directory ptr
    ret nc

    ld a, (de)
    cp #e5
    scf
    ret z  ; if the directory entry is empty or for an erased file, return (carry set)

    call fw_d9a8_set_DPH_set_directory_plus_one
    ld a, #ff
    jp fw_d83c_update_block_allocation_table_for_current_extent

get_next_directory_entry_ptr__default_drive:
    call fw_d9b8_cp_hl_to_DPH_directory_number
    ret nc  ; return if hl >= DPH directory number

    jp fw_d91c_get_next_directory_entry_ptr_internal


;----------------------------------------------------------------------------
; Only gets the 2 lowest bytes (the number is a 24bit number)
;
; input:
; - de: openin/out control block ptr
; output:
; - hl: previously accessed records
fw_d6c1_get_number_of_previously_accessed_records_from_FCB:
    ld hl, AMSDOS_FCB_offset_num_previously_accessed_records
    add hl, de
    jp fw_dbf9_ld_word_from_hl


;----------------------------------------------------------------------------
; Computes the extend number we have to use to add one more record to a file.
; Note: some of the logic in this method makes no sense.
;
; input:
; - de: openin/out control block ptr
; output:
; - carry: success
; - no carry, nz: disk full
; - no carry: z: disk is not full, but an error occurred (Note: I don't think this is possible if the documentation is correct)
fw_d6c8_get_extent_number_for_a_new_block:
    ld hl, AMSDOS_FCB_offset_num_previously_accessed_records + 2
    add hl, de
    ld a, (hl)  ; (NPAR) number of previously accessed records (highest byte, out of 3)
    or a
    ret nz  ; return if number of previously accessed records >= 65536 (disk full!)
    call fw_d6c1_get_number_of_previously_accessed_records_from_FCB  ; hl = lowest 2 bytes of number of "previously accessed records" in the FCB
    ld a, h
    rra     
    rra     
    rra     
    rra     
    and #0f
    ld b, a  ; b = (NPAR // 128) / 32  [Note: this logic makes no sense, files cannot be that large, and b is always == 0, which is later assumed in the code]
    add hl, hl
    ld a, h
    and #1f
    ld c, a  ; c = (NPAR // 128) % 32  [this is the extent number]
    push bc
        ld hl, 15
        add hl, de
        ld a, (hl)  ; this should be 0 (according to the documentation offsets 14 and 15 are zero).
        xor b  ; should also be 0
        jr nz, get_extent_number_for_a_new_block__failure  ; failure
        ld a, AMSDOS_XDPB_offset_extent_mask
        call fw_da54_read_byte_from_XDPB  ; a = extent mask (should be 0)
        cpl     
        ld b, a  ; b = #ff
        dec hl
        dec hl
        ld a, (hl)  ; a = extent number
        xor c  ; xor it with the extent number we had calculated above
        and b  ; b == 0, so a will always == 0  [Note: as mentioned above, this logic makes no sense.]
        jr nz, get_extent_number_for_a_new_block__failure
        scf     
get_extent_number_for_a_new_block__failure:
    pop bc  ; b = 0, c = extent number
    sbc a, a  ; preserves carry, sets z
    ret     


;----------------------------------------------------------------------------
; input:
; - de: openin/out control block ptr
; - c: extent number
; - b: number of records in the extent
fw_d6fa_init_extent_info_in_fcb:
    ld hl, 13
    add hl, de
    ld (hl), c  ; set extent number
    inc hl
    inc hl
    ld (hl), b  ; set number of records in the extent
    inc hl
    ex de, hl
        ; zero out 17 positions in 'de':
        ; - block numbers in current extent, and
        ; - number of previously accessed records
        ld bc, 17
        call fw_caaf_clear_memory
    ex de, hl
    ret     


;----------------------------------------------------------------------------
; Compares the number of previously accessed records, with the total number of records in this file.
;
; input:
; - de: openin/out control block ptr
; output:
; - carry: previously accessed records < total
; - no carry: previously accessed records >= total
fw_d70c_are_there_records_yet_to_be_accessed_in_the_file:
    push de
        call fw_d6c1_get_number_of_previously_accessed_records_from_FCB  ; hl = previously accessed records
        ld a, h
        and #0f
        ld h, a
        push hl
            ld hl, AMSDOS_FCB_offset_num_records_in_current_extent
            add hl, de
            ld c, (hl)  ; c = number of records in the current extent
            ld b, 0
            dec hl
            dec hl
            dec hl
            ld h, (hl)  ; h = extent number
            ld l, b
            ld a, 1
            call fw_dbeb_shift_hl_right_by_a
            add hl, bc  ; hl = number of records in total in the file (current extent plus previous extents)
        pop de
        inc de
        call fw_dbf3_cp_hl_de
        ccf     
    pop de
    ret     


;----------------------------------------------------------------------------
; Sees if there are still available records in the current block to write to.
;
; input:
; - de: openin/out FCB block ptr
; output:
; - b: max block number // 256
; - if carry, hl: index of next record to use within the last used block
; - it no carry, we need to find a new block, hl: ptr to the block number in the current extent (inside the FCB block)
fw_d72f_next_record_in_current_block_or_new_block:
    call fw_d6c1_get_number_of_previously_accessed_records_from_FCB  ; hl = number of previously accessed records
    ld a, AMSDOS_XDPB_offset_block_mask
    call fw_da54_read_byte_from_XDPB  ; a = Block Mask
    and l
    ld c, a  ; c = record number within the last block
    ld a, AMSDOS_XDPB_offset_block_shift
    call fw_da54_read_byte_from_XDPB  ; a = Block Shift (records per block)
    call fw_dbeb_shift_hl_right_by_a  ; HL shift right by A
    ld a, AMSDOS_XDPB_offset_max_block_number_high
    call fw_da54_read_byte_from_XDPB  ; a = Max Block Number // 256
    ld b, a  ; b = Max Block Number // 256
    or a  ; can blocks number be >= 256?
    ld a, l  ; a = (number of previously accessed records) >> block shift
    ld hl, 17
    add hl, de  ; hl = ptr to block bumbers for current Extent in the FCB
    jr z, next_record_in_current_block_or_new_block__1byte_blocks
    ; each block number is 2 bytes (so, we can only have 8 blocks, as there ar eonly 16 bytes for this):
    and #07
    add a, a
    add a, l
    ld l, a
    adc a, h
    sub l
    ld h, a  ; hl += 2 * a (ptr to the block number in the current extent)
    push hl
        call fw_dbf9_ld_word_from_hl  ; LD HL,(HL)
        jr next_record_in_current_block_or_new_block__continue
next_record_in_current_block_or_new_block__1byte_blocks:
    ; each block number is 1 byte:
    and #0f
    add a, l
    ld l, a
    adc a, h
    sub l
    ld h, a  ; hl += a (ptr to the block number in the current extent)
    push hl
        ld l, (hl)
        ld h, 0
next_record_in_current_block_or_new_block__continue:
        ld a, h
        or l
        jr z, next_record_in_current_block_or_new_block__new_block  ; if the resulting block number is 0, we need to find a new block
    ; If we are here, there still records left within the last block, so, we will use it
    pop af
    ld a, AMSDOS_XDPB_offset_block_shift
    call fw_da54_read_byte_from_XDPB  ; a = Block Shift
next_record_in_current_block_or_new_block__record_to_block_loop:  ; shift hl left 'a' times. (to translate from blocks to records)
    add hl, hl
    dec a
    jr nz, next_record_in_current_block_or_new_block__record_to_block_loop
    ld a, c  ; hl += record within this block
    or l
    ld l, a
    scf
    ; if we return here, hl index of the next record to use within the last used block
    ret     
next_record_in_current_block_or_new_block__new_block:
    ; if we return here, we need to find a new block
    ; hl: ptr to the block number in the current extent (inside the FCB block)
    pop hl
    ret     


;--------------------------------------------------------------------------
; input:
; - de: openin/out FCB block ptr
fw_d77d_inc_num_records_in_current_extent:
	ld hl, AMSDOS_FCB_offset_num_records_in_current_extent
	add hl, de
	ld a, (hl)  ; a = num records in current extent
	inc (hl)  ; increment the number of records in the current extent
	or a
	ret p
      ; if resulting number is >= 128, set it back to 1, and increment the current extent number
	ld (hl), 1
	dec hl
	dec hl
	dec hl
	inc (hl)  ; increment current extent number
	ret     


;--------------------------------------------------------------------------
; Finds the next available directory ptr, and initializes it with the values in the FCB.
; Then writes the directory buffer to disk, and updates the checksums.
;
; input:
; - de: openout control block ptr
fw_d78c_init_next_available_directory_from_FCB:
	push de
    	push de
        	call fw_d7bb_get_next_available_directory_ptr
        	ex (sp), hl  ; hl = openin/out control block ptr
        	inc hl
        	call fw_dbdf_ldir32  ; initialize the new directory entry/catalog ptr to the values in the control block ptr
    	pop hl
    	call fw_d97a_write_directory_buffer_and_update_checksum
	pop de
	ret     


;--------------------------------------------------------------------------
; Sets the previously accessed records in a control header to 0
; see: https://www.cpcwiki.eu/index.php/AMSDOS_Memory_Map
;
; input:
; - de: ptr to a FCB (File Control Block) (either for OPENIN or OPENOUT)
fw_d79c_clear_FCB_previously_accessed_records:
    ld hl, AMSDOS_FCB_offset_num_previously_accessed_records
    add hl, de
    xor a
    ld (hl), a
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    ret     


;--------------------------------------------------------------------------
; input:
; - de: ptr to a FCB (File Control Block) (either for OPENIN or OPENOUT)
fw_d7a7_inc_num_previously_accessed_records:
    ld hl, AMSDOS_FCB_offset_num_previously_accessed_records
    add hl, de
    inc (hl)  ; increment the number of previously accessed records
    ret nz
    ; if we have overflow, incremebr the next byte:
    inc hl
    inc (hl)
    ret nz
    ; if we have overflow, incremebr the final byte:
    inc hl
    inc (hl)
    ret     


;-----------------------------------------------------------------------
; Writes the current sector, and tries to find a directory entry/catalog that matches the info in the openout/in control block.
; Note: if we are not in the default drive, if returns with error.
;
; input:
; - hl: directory entry/catalog number - 1
; output:
; - de: directory entry/catalog ptr
; - carry: directory entry found
; - no carry: directory entry not found
fw_d7b3_write_current_sector_and_find_matching_directory_entry:
    call fw_d683_write_current_sector_and_reset_track_directory_and_allocation_table
    call fw_d698_directory_entry_search
    jr get_next_available_directory_ptr__check_we_are_on_default_drive


;-----------------------------------------------------------------------
; Gets the pointer to the next available directory entry.
; If it's not on the default drive, or the directory is full, it returns with error.
; output:
; - de: ptr to the next available directory
; - a: #e5
fw_d7bb_get_next_available_directory_ptr:
    ld hl, -1
    ; Loop until we find a directory entry that is available (has "#e5" as a user, which means it's available)
get_next_available_directory_ptr__loop:
    inc hl
    call fw_d91c_get_next_directory_entry_ptr_internal
    jp nc, fw_cdaa_return_with_error  ; "Drive <drive>: directory full"

    ld a, (de)
    cp #e5  ; user number associated with erased files (available directory entries)
    jr nz, get_next_available_directory_ptr__loop
get_next_available_directory_ptr__check_we_are_on_default_drive:
    push af
        ld a, (iy + AMSDOS_work_RAM_offset_default_drive_flag)
        or a
        ; ld a, 9
        jp z, fw_cdaa_return_with_error
    pop af
    ret     


;-----------------------------------------------------------------------
; Check if the file information (user, filename, extent) matches that in a
; given directory entry/catalog.
;
; input:
; - bc: openout control block ptr + 1
; - de: directory entry/catalog ptr
fw_d7d8_user_filename_extent_in_control_block_match_catalog:
    push bc
    push de
    push hl
        ld h, b
        ld l, c  ; hl = openout control block ptr + 1
        ld a, (de)  ; a = user number in directory entry/catalog
        xor (hl) ; does it match with the user number in the openout control block?
        jr nz, user_filename_extent_in_control_block_match_catalog__no_match  ; if user does not match, jump
        ; user matches
        inc hl  ; hl = ptr to filename in openout control block
        inc de  ; de = ptr to filename in directory entry/catalog
        ld b, 11  ; 8 + 3 (filename length)
user_filename_extent_in_control_block_match_catalog__filename_loop:
        ld a, (hl)
        cp #3f  ; '?'
        jr z, user_filename_extent_in_control_block_match_catalog__letter_match  ; a '?' matches with any character
        ld a, (de)
        xor (hl)
        and #7f
        jr nz, user_filename_extent_in_control_block_match_catalog__no_match  ; no match!
user_filename_extent_in_control_block_match_catalog__letter_match:
        ; match
        inc hl
        inc de
        djnz user_filename_extent_in_control_block_match_catalog__filename_loop
        ; filename matches
        ld a, (hl)  ; a = extent number
        inc a
        jr z, user_filename_extent_in_control_block_match_catalog__extent_match  ; if #ff, jump
        ; extent number is not #ff
        ld a, AMSDOS_XDPB_offset_extent_mask
        call fw_da54_read_byte_from_XDPB  ; 'a' should be 0 here
        cpl  ; 'a' should be #ff here
        ld b, a
        ld a, (de)  ; a = extent number in dirextory entry/catalog
        xor (hl)
        and b  ; match only the bits that matter, acording to the extent mask
        jr nz, user_filename_extent_in_control_block_match_catalog__no_match  ; extends do not match, jump
        ; extent number matches
user_filename_extent_in_control_block_match_catalog__extent_match:
        inc hl
        inc de
        inc hl
        inc de
        ; Note: according to both:
        ; - https://www.cpcwiki.eu/index.php/AMSDOS_Memory_Map#File_Header_for_OPENIN.2FOPENOUT
        ; - and https://www.cpcwiki.eu/index.php/765_FDC#The_15_FDC_Commands
        ; these bytes (extent high byte) are unused, or just zero. But this code here,
        ; clearly shows that is incorrect.
        ld a, (hl)  ; 'a' = extent high byte (out of 3)
        inc a
        jr z, user_filename_extent_in_control_block_match_catalog__no_match  ; if extent high byte == #ff, fail
        ld a, (de)
        xor (hl)  ; compare with extent high byte in the directory entry/catalog
user_filename_extent_in_control_block_match_catalog__no_match:
    pop hl
    pop de
    pop bc
    ret nz

    scf     
    ret     


;-----------------------------------------------------------------------
; This clears the DPH allocation table to all 0s (except the blocks used for the directory).
fw_d814_clear_DPH_block_allocation_table:
    ld a, AMSDOS_XDPB_offset_max_block_number
    call fw_da45_read_word_from_XDPB  ; hl = max block number
    ld a, 3
    call fw_dbeb_shift_hl_right_by_a  ; hl = max block number / 8
    inc hl  ; number of bytes in the allocation table
    ex de, hl
    ld a, AMSDOS_DPH_offset_allocation_table_ptr
    call fw_da3f_read_word_from_DPH_ptr  ; hl = allocation table ptr
clear_DPH_block_allocation_table__loop:  ; zero out the allocation table
    ld (hl), 0
    inc hl
    dec de
    ld a, d  ; is de == 0?
    or e
    jr nz, clear_DPH_block_allocation_table__loop
    ; Set the blocks that are used by the directory:
    ld a, AMSDOS_XDPB_offset_directory_allocation_table
    call fw_da45_read_word_from_XDPB
    ex de, hl  ; de = XDBP directory allocation table
    ld a, AMSDOS_DPH_offset_allocation_table_ptr
    call fw_da3f_read_word_from_DPH_ptr  ; hl = allocation table ptr
    ld (hl), e
    inc hl
    ld (hl), d
    ret     


;-----------------------------------------------------------------------
; Updates the block allocation table for all the blocks in the current extent.
;
; input:
; - a: #ff if we want to update the block allocation table to 1s,
;      and #00 if we want to update it to 0s.
; - de: ptr to directory entry/catalog number or
;       ptr to file control block (FCB) + 1
fw_d83c_update_block_allocation_table_for_current_extent:
    push hl
    push de
    push bc
        ld c, a
        ld hl, #0010
        add hl, de  ; hl = ptr to Block Numbers for current Extent
        ld b, 16  ; size of the array of blocks in the current extent
update_block_allocation_table_for_current_extent__loop:
        ld e, (hl)  ; block number
        inc hl
        ld a, AMSDOS_XDPB_offset_max_block_number_high
        call fw_da54_read_byte_from_XDPB
        or a
        jr z, update_block_allocation_table_for_current_extent__block_number_set
        ; block numbers are 2 bytes
        dec b
        ld a, (hl)
        inc hl
update_block_allocation_table_for_current_extent__block_number_set:
        ld d, a  ; de = block number
        or e
        jr z, update_block_allocation_table_for_current_extent__skip  ; if block number == 0
        push hl
            ld a, AMSDOS_XDPB_offset_max_block_number
            call fw_da45_read_word_from_XDPB  ; hl = max block number
            ld a, l
            sub e
            ld a, h
            sbc a, d  ; ls de <= hl
            call nc, fw_d86c_update_block_allocation_table_one_block  ; call if de <= hl
        pop hl
update_block_allocation_table_for_current_extent__skip:
        djnz update_block_allocation_table_for_current_extent__loop
    pop bc
    pop de
    pop hl
    scf     
    ret     


;-----------------------------------------------------------------------
; Updates a bit in the block allocation table.
;
; input:
; - c: #ff if we want to set the bit to 1, and #00 if we want it to 0
; - de: block number
fw_d86c_update_block_allocation_table_one_block:
    push bc
    push de
        push de
            ex de, hl
                ld a, 3
                call fw_dbeb_shift_hl_right_by_a  ; hl /= 8
            ex de, hl  ; de = block number // 8
            ld a, AMSDOS_DPH_offset_allocation_table_ptr
            call fw_da3f_read_word_from_DPH_ptr
            add hl, de  ; hl = ptr to the byte of the block in the allocation table
        pop de
        ld a, e
        and #07
        ld e, a  ; e = bit within the byte in the allocation table
        ld a, 1
        inc e
update_block_allocation_table_one_block__bit_mask_loop:
        rrca    
        dec e
        jr nz, update_block_allocation_table_one_block__bit_mask_loop
        ; here 'a' is a mask indicating the bit corresponding to the block
        ld b, a
        and c
        ld c, a  ; b = bit mask, c = c & bit mask
        ld a, b
        cpl     
        and (hl)  ; a = previous value of (hl), but with the target bit zeroed out
        or c  ; we set the bit to the target value
        ld (hl), a  ; update tbe block allocation table
    pop de
    pop bc
    ret     


;-----------------------------------------------------------------------
; Iterates over the block allocation table trying to find one that is available.
; output:
; - hl: index of a free block
; - no carry: disk full
fw_d893_find_free_block:
	push bc
	push de
    	ld a, AMSDOS_XDPB_offset_max_block_number
    	call fw_da45_read_word_from_XDPB
    	ex de, hl  ; de = Max Block Number from XDPB
    	ld a, AMSDOS_DPH_offset_allocation_table_ptr
    	call fw_da3f_read_word_from_DPH_ptr  ; hl = Pointer to Block Allocation Table (ALT)
find_free_block__byte_loop:
    	ld bc, #0880  ; c = 128, b = 8
find_free_block__bit_loop:
    	ld a, (hl)  ; read from the block allocation table
    	and c
    	jr z, find_free_block__found
    	rrca 
    	ld c, a  ; shift the c bitmask to the right (to look for the next bit)
    	ld a, d
    	or e  ; de == 0
    	jr z, find_free_block__done  ; max number of blocks reached, and there wasn't any block available
    	dec de
    	djnz find_free_block__bit_loop  ; bit loop
    	inc hl  ; move to the next byte in the ALT
    	jr find_free_block__byte_loop
find_free_block__found:  ; we found a free block!
    	ld a, (hl)
    	or c
    	ld (hl), a  ; mark it as used (set it's bit to 1)
    	ld a, AMSDOS_XDPB_offset_max_block_number
    	call fw_da45_read_word_from_XDPB  ; hl = Max Block Number
    	or a
    	sbc hl, de  ; hl = index of the block we found to be free
    	scf
find_free_block__done:
	pop de
	pop bc
	ret  


;-----------------------------------------------------------------------
; Gets the pointer to a new directory entry/catalog. If this is the first one of a new block
; (i.e. "catalog number" % 4 == 0), it updates the checksum of the new block that will contain this
; directory entry.
;
; input:
; - de: directory entry/catalog number
fw_d91c_get_next_directory_entry_ptr_internal:
    push hl
    push bc
        ld a, l
        and #03
        jr nz, get_next_directory_entry_ptr_internal__new_block_initialized
        ; first directory number in a new block, we might have to initialize a new block.
        ; Check if directory number is larger than the maximum allowed.
        ex de, hl
        ld a, AMSDOS_XDPB_offset_last_valid_dir_entry_number
        call fw_da45_read_word_from_XDPB
        call fw_dbf3_cp_hl_de
        ccf     
        ex de, hl  ; recover the directory number in hl
        jr nc, get_next_directory_entry_ptr_internal__done  ; maximum directory entry number exceeded
        ; We are still withiin bounds
        call fw_d948_update_directory_checksum
        xor a
get_next_directory_entry_ptr_internal__new_block_initialized:
        ld b, a  ; b = directory entry within the current block (4 entries per block)
        ld a, AMSDOS_DPH_offset_directory_buffer_ptr
        call fw_da3f_read_word_from_DPH_ptr
        ld de, 32  ; size of a catalog/directory entry
        inc b
        jr get_next_directory_entry_ptr_internal__loop_entry_point
get_next_directory_entry_ptr_internal__loop:
        add hl, de
get_next_directory_entry_ptr_internal__loop_entry_point:
        djnz get_next_directory_entry_ptr_internal__loop
        ex de, hl  ; de = ptr to the new directory entry
        scf
get_next_directory_entry_ptr_internal__done:
    pop bc
    pop hl
    ret     


;-----------------------------------------------------------------------
; Updates the checksum for the sector corresponding to the current directory if
; it does not match the current one.
;
; input:
; - hl: directory number (4 per sector)
fw_d948_update_directory_checksum:
    ld a, 2
    call fw_dbeb_shift_hl_right_by_a
    ex de, hl  ; de = record index
    ld a, AMSDOS_DPH_offset_directory_buffer_ptr
    call fw_da3f_read_word_from_DPH_ptr  ; hl = pointer to directory buffer
    call fw_d9e8_read_sector_from_current_sector_index
    ld a, AMSDOS_XDPB_offset_checksum_area_size
    call fw_da45_read_word_from_XDPB  ; hl = checksum area size
    ex de, hl
        call fw_dbf3_cp_hl_de
    ex de, hl
    ret nc  ; if the record index is >= than the checksum area size, return with error

    ld a, AMSDOS_DPH_offset_checksums_ptr
    call fw_da3f_read_word_from_DPH_ptr
    add hl, de  ; hl = ptr to the checksum of the target record
    call fw_d9c8_DPH_directory_buffer_checksum
    cp (hl)
    ret z  ; checksums match, we are done

    push af
        ex de, hl
            add hl, hl
            add hl, hl  ; hl = directory number
            call fw_d9b8_cp_hl_to_DPH_directory_number
        ex de, hl
    pop de
    jp c, fw_d233_close_file_in_current_drive  ; if "current directory number" > hl
    ld (hl), d  ; update the checksum
    ret     


;-----------------------------------------------------------------------
; Writes the directory buffer to disk, and updates the corresponding checksum.
;
; input:
; - hl: directory number (4 per sector)
fw_d97a_write_directory_buffer_and_update_checksum:
	push hl
	push bc
    	ld a, 2
    	call fw_dbeb_shift_hl_right_by_a  ; hl /= 4 (sector corresponding to the directory number)
    	ex de, hl
        	ld a, AMSDOS_DPH_offset_directory_buffer_ptr
        	call fw_da3f_read_word_from_DPH_ptr
        	ld c, 1  ; mark that we want to write the directory buffer in the method below
        	call fw_d9f3_write_record_from_record_index_to_next_sector_track
        	ld a, AMSDOS_XDPB_offset_checksum_area_size
        	call fw_da45_read_word_from_XDPB
    	ex de, hl  ; de = checksum area size, hl = sector index corresponding to the directory number
    	call fw_dbf3_cp_hl_de
    	ex de, hl
    	jr nc, write_directory_buffer_and_update_checksum__checksum_updated  ; if it's outside the checksum area, just return
    	ld a, AMSDOS_DPH_offset_checksums_ptr
    	call fw_da3f_read_word_from_DPH_ptr
    	add hl, de
    	call fw_d9c8_DPH_directory_buffer_checksum
    	ld (hl), a  ; set the checksum
write_directory_buffer_and_update_checksum__checksum_updated:
	pop bc
	pop hl
	call fw_d9b8_cp_hl_to_DPH_directory_number
	ret c


;-----------------------------------------------------------------------
; Set directory number in the DPH for the current drive to hl + 1.
;
; input:
; - hl: directory number - 1
fw_d9a8_set_DPH_set_directory_plus_one:
    push de
    push hl
        ex de, hl
        inc de  ; directory number += 1
        ld a, AMSDOS_DPH_offset_current_directory_number
        call fw_da35_get_DPH_ptr_plus_offset
        ld (hl), e
        inc hl
        ld (hl), d  ; set directory number
    pop hl
    pop de
    scf
    ret


;-----------------------------------------------------------------------
; Compares the number in hl to the directory in the DPH for the current drive.
;
; input:
; - hl: directory number
; output:
; - cp hl, de
fw_d9b8_cp_hl_to_DPH_directory_number:
    push de
        push hl
            ld a, AMSDOS_DPH_offset_current_directory_number
            call fw_da35_get_DPH_ptr_plus_offset
            ld e, (hl)
            inc hl
            ld d, (hl)  ; de = directory number
        pop hl
        call fw_dbf3_cp_hl_de
    pop de
    ret     


;-----------------------------------------------------------------------
; Checksum for the 128 bytes of the directory buffer.
fw_d9c8_DPH_directory_buffer_checksum:
    push bc
    push hl
        ld b, 128
        ld a, AMSDOS_DPH_offset_directory_buffer_ptr
        call fw_da3f_read_word_from_DPH_ptr
        xor a
DPH_directory_buffer_checksum__loop:
        add a, (hl)
        inc hl
        djnz DPH_directory_buffer_checksum__loop
    pop hl
    pop bc
    ret     


;------------------------------------------------------------------------
; get read only, read/write state
;
; input:
; - de: address of catalog/directory-entry structure
; output:
; - carry: read only
; - no carry: read/write 
fw_d9d9_is_file_read_only:
	push hl
    	ld hl, 9
    	add hl, de
    	ld a, (hl)  ; get first character of the extension. bit 7 of this character encodes read or read/write status.
    	add a, a  ; transfer bit 7 into carry
	pop hl
	ret     


;------------------------------------------
; input:
; - de: record index
; - hl: pointer to directory buffer
fw_d9e8_read_sector_from_current_sector_index:
    push bc
    push de
    push hl
        call fw_da06_set_track_and_record_from_record_index
        call fw_c54c_read_sector_and_copy_to_directory_buffer
        jr write_record_from_record_index_to_next_sector_track__entry_point

;------------------------------------------
; Given a record index, it saves the current sector (if necessary) to the next
; available sector/track (notice each sector contains several records, 4, so,
; sector index will only increment when enough records are written).
;
; input:
; - c: save directoy buffer (c == 2), or sector buffer (c == 1)
; - de: record index
; - hl: tmp record buffer ptr
fw_d9f3_write_record_from_record_index_to_next_sector_track:
    push bc
    push de
    push hl
        push bc
            call fw_da06_set_track_and_record_from_record_index
        pop bc
        call fw_c52e_inc_sector_track_and_write_sector
write_record_from_record_index_to_next_sector_track__entry_point:
        or a
        jp nz, fw_cdaa_return_with_error
    pop hl
    pop de
    pop bc
    ret


;------------------------------------------
; Given a record index (in 'de'), computes which track does it
; correspond to (and which record within that track), and sets
; 'memory_be54_track' and 'memory_be55_record' accordingly).
;
; input:
; - de: record index
; - hl: pointer to directory buffer
fw_da06_set_track_and_record_from_record_index:
    push de
        ld b, h
        ld c, l
        call fw_c51a_set_directory_buffer_ptr  ; CP/M function "setdma"
    pop de
    ld a, AMSDOS_XDPB_offset_track_offset
    call fw_da45_read_word_from_XDPB
    ld b, h
    ld c, l  ; bc = Track Offset (should be == 0 for DATA format)
    xor a  ; a = AMSDOS_XDPB_offset_records_per_track
    call fw_da45_read_word_from_XDPB  ; hl = Records per Track (should be 36 for DATA format)
    ; This loop does: bc = offset + de // records-per-track, hl = de % records-per-track
    dec bc
set_track_and_record_from_record_index__loop:
    inc bc
    ld a, e  ; de -= hl
    sub l
    ld e, a
    ld a, d
    sbc a, h
    ld d, a
    jr nc, set_track_and_record_from_record_index__loop
    add hl, de  ; hl = index of the record within the track, bc = track
    push hl
        ld a, c
        ld (memory_be54_track), a
    pop bc
    xor a  ; a = AMSDOS_DPH_offset_skew_factor_translation
    call fw_da3f_read_word_from_DPH_ptr
    ex de, hl  ; de = Skew Factor Translation (physical-to-logical sector)
    ld h, b
    ld l, c  ; hl = index of the record within the track
    ld a, c
    ld (memory_be55_record), a  ; index of the record within the track (8 bit)
    ret     


;------------------------------------------
; input:
; - a: offset
; output:
; - hl = (iy + AMSDOS_work_RAM_offset_DPH_ptr) + a
fw_da35_get_DPH_ptr_plus_offset:
    add a, (iy + AMSDOS_work_RAM_offset_DPH_ptr)
    ld l, a
    adc a, (iy + AMSDOS_work_RAM_offset_DPH_ptr + 1)
    sub l
    ld h, a
    ret


;------------------------------------------
; input:
; - a: offset of the pointer we want inside the XDPB
; output:
; - hl = (pointer)
fw_da3f_read_word_from_DPH_ptr:
    call fw_da35_get_DPH_ptr_plus_offset
    jp fw_dbf9_ld_word_from_hl  ; LD HL, (HL)


;------------------------------------------
; (1) gets the XDPB pointer, (2) adds 'a', and (3) reads a word from that ptr
;
; input:
; - a: offset inside XDPB
; output:
; - hl: word(word(XDPB + 10) + a)
fw_da45_read_word_from_XDPB:
    push af
	    ld a, AMSDOS_DPH_offset_xdpb_ptr
	    call fw_da3f_read_word_from_DPH_ptr  ; hl = Pointer to DPB/XDPB
    pop af
    ; hl += a
    add a, l
    ld l, a
    adc a, h
    sub l
    ld h, a
    jp fw_dbf9_ld_word_from_hl  ; LD HL, (HL)


;------------------------------------------
; input:
; - a: offset inside XDPB
; output:
; - a: (word(XDPB + 10) + a)
fw_da54_read_byte_from_XDPB:
    push hl
        call fw_da45_read_word_from_XDPB
        ld a, l
    pop hl
    ret     


;--------------------------------------------------------------
; Initializes the AMSDOS_work_RAM_offset_tmp_filename_buffer, with
; drive, user number and normalized filename. 
; output:
; - bc: ptr to file control block (filename starts after 2 bytes)
fw_da6a_normalize_file_name_into_FCB_and_clear_extent_info:
	call fw_da6f_normalize_file_name_into_FCB
    ld hl, 13  ; 2 initial bytes + 8 name + 3 extension
    add hl, bc
    ld (hl), #ff  ; extent number
    inc hl
    inc hl
    ld (hl), #ff  ; number of records in current extent
    ret     


;--------------------------------------------------------------
; output:
; - bc: ptr to file control block (filename starts after 2 bytes)
fw_da6f_normalize_file_name_into_FCB:
    ld c, #ff  ; character to initialize normalized filename with
    ld de, AMSDOS_work_RAM_offset_tmp_filename_buffer  ; offset in working memory to write normalized filename
    
    call fw_daa0_normalize_filename_or_error
    push bc
        ld d, 8 + 3  ; filename size + extension size
        inc bc
        ; make sure there are no wildcards:
normalize_file_name_into_FCB__wildcard_loop:
        inc bc
        ld a, (bc)
        cp '?'
        jp z, fw_cdaa_return_with_error  ; display "Bad command" and quit command
        dec d
        jr nz, normalize_file_name_into_FCB__wildcard_loop
    pop bc
    ret     


;------------------------------------------------------------------
; input:
; - hl: ptr to filename
; - de: offset in the working memory to write the normalized filename to
; - c: character to initialize filename with (#ff)
; output:
; - bc: ptr to normalized filename
fw_daa0_normalize_filename_or_error:
    call fw_dab6_normalize_filename
    jp z, fw_cdaa_return_with_error  ; "Bad command"
    ret     


;------------------------------------------------------------------
; input:
; - hl: ptr to filename
; - de: offset in the working memory to write the normalized filename to
; - c: character to initialize filename with (#ff)
; output:
; - bc: ptr to normalized filename
; - z: filename is empty
; - nz: all correct
fw_dab6_normalize_filename:
    push hl
        call fw_ca98_de_pluseq_iy  ; DE = IY + DE
        push de
        	; initialize default drive and user (2 bytes):
            ld a, (iy + AMSDOS_work_RAM_offset_drive)
            ld (de), a
            inc de
            ld a, (iy + AMSDOS_work_RAM_offset_user)
            ld (de), a
            inc de
            push bc
            	; initialize filename to all 'c's (8 + 3 'c's, and 3 0s).
                ld b, c
                ld c, 8  ; filename length
                call fw_db85_fill_de_with_spaces
                ld a, b
                ld c, 3  ; extension length
                call fw_db90_fill_de_with_a
                ld bc, 3
                call fw_caaf_clear_memory
            pop bc
        pop de
    pop hl
    push de
    	call fw_daed_normalize_filename_internal
    pop de
    jp nc, fw_cdaa_return_with_error  ; display "Bad command" and quit command
    ld b, d
    ld c, e
    inc de  ; skip drive
    inc de  ; skip user
    ld a, (de)
    cp ' '  ; check if filename is empty
    ret


;------------------------------------------------------------------
; Normalizes a file name (uppercase, remove illegal characters, replace wildcards, etc.)
;
; input:
; - hl: ptr to the filename
; - b: length of the filename
; - de: ptr to the normalized filename we are writing
; output:
; - carry: success
; - no carry: failure, name format was incorrect
fw_daed_normalize_filename_internal:
    dec hl
    call fw_db97_next_non_space_char_upper
    ccf
    ret c  ; if we have reached the end of the filename, we are done

    ld c, a
    ; See if there is a ":" character in the filename
    ; This is to see if a "user" was specified (from 0 to 15)
    push hl
    push bc
normalize_filename_internal__find_colon_loop:
	    cp ':'
	    jr z, normalize_filename_internal__find_colon_loop_done
	    call fw_dba5_next_char_upper
	    jr c, normalize_filename_internal__find_colon_loop
	    scf
normalize_filename_internal__find_colon_loop_done:
    pop bc
    pop hl
    ld a, c
    jr c, normalize_filename_internal__no_user  ; jump if no ":"
    ; There was a ":" character in the filename, fill the user info
    inc de  ; skip drive number
    cp '0'
    jr c, normalize_filename_internal__done_with_user
    cp '9' + 1
    jr nc, normalize_filename_internal__done_with_user
    ; we have a number:
    sub '0'
    ld c, a
    ld (de), a
    call fw_dba5_next_char_upper
    cp '0'
    jr c, normalize_filename_internal__done_with_user
    cp '9' + 1
    jr nc, normalize_filename_internal__done_with_user
    or a
    dec c
    ret nz  ; if there are two digits, the first must have been a 1

    add a, 10 - '0'
    cp 16  ; we can only have users 0 - 15
    ret nc

    ld (de), a  ; save user
    call fw_dba5_next_char_upper
normalize_filename_internal__done_with_user:
    dec de
    cp 'Q'
    jr nc, normalize_filename_internal__done_with_drive
    cp 'A'
    jr c, normalize_filename_internal__done_with_drive
    sub 'A'
    ld (de), a
    call fw_dba5_next_char_upper
normalize_filename_internal__done_with_drive:
    call next_non_space_char_upper__loop
    xor #3a
    ret nz

    call fw_db97_next_non_space_char_upper
    ccf     
    ret c

normalize_filename_internal__no_user:
    inc de  ; skip drive number
    inc de  ; skip user
    cp '.'
    ret z

    ld c, 8  ; max length of the filename (before extension)
    call fw_db58_normalize_filename_fragment
    ret c

    xor '.'
    ret nz

    call fw_db97_next_non_space_char_upper
    ld c, 3  ; max length of the file extension
    jr nc, fw_db85_fill_de_with_spaces


;--------------------------------------------------------
; Normalize a filename part (extension or name before extension). 
; Normalization involves making names uppercase, removing illegal characters,
; filling with spaces up to max length, or with wildcards, if '*' is found.
;
; input:
; - hl: ptr to filename
; - c: length of the current part
; - de: ptr where we are writing the normalized filename
fw_db58_normalize_filename_fragment:
	; If we have a non printable character (< ' '), just fill with spaces
    cp ' '
    jr c, fw_db85_fill_de_with_spaces
    ; See if we have an illegal character:
    push hl
    push bc
	    ld b, a
	    ld hl, fw_dbb2_invalid_characters_table
normalize_filename_fragment__invalid_character_table_loop:
	    ld a, (hl)
	    inc hl
	    or a
	    jr z, normalize_filename_fragment__invalid_character_table_loop_done
	    cp b
	    jr nz, normalize_filename_fragment__invalid_character_table_loop
	    scf  ; we found an illegal character!
normalize_filename_fragment__invalid_character_table_loop_done:
	    ld a, b
    pop bc
    pop hl
    jr c, fw_db85_fill_de_with_spaces  ; If we had an illegal character, fill with spaces
    dec c
    ret m

    cp '*'
    call z, fw_db8e_fill_de_with_wildcard
    ld (de), a
    inc de
    call fw_dba5_next_char_upper
    jr nc, fw_db85_fill_de_with_spaces
    cp ' '
    jr nz, fw_db58_normalize_filename_fragment
    call next_non_space_char_upper__loop
    ; jp fw_db85_fill_de_with_spaces


;--------------------------------------------------------
; fill with spaces
;
; input:
; - c: count
; - de: buffer
fw_db85_fill_de_with_spaces:
    push af
	    ld a, #20
	    call fw_db90_fill_de_with_a  ; fill with byte
    pop af
    ccf     
    ret     


;------------------------------------------------------------------------
; fill with wildcard token
;
; input:
; - c: count
; - de: buffer
fw_db8e_fill_de_with_wildcard:
    ld a, '?'


;------------------------------------------------------------------------
; Fill DE C + 1 bytes with value A.
;
; input:
; - de: buffer
; - a: byte
; - c: count - 1
fw_db90_fill_de_with_a:
    inc c
fill_de_with_a__loop:
    dec c  ; decrement count
    ret z
    ld (de), a  ; write byte
    inc de  ; increment pointer
    jr fill_de_with_a__loop            


;------------------------------------------------------------------------
; Find the next character that is not a space, and converts it to upper case
;
; input:
; - b: number of bytes left
; - hl: pointer to the previous byte converted
; output:
; - b: number of characters left in the string updated
; - hl: pointing to the first non space
; - a: resulting byte converted to upper case
; - carry: success
; - no carry: we are done
fw_db97_next_non_space_char_upper:
    call fw_dba5_next_char_upper
    ret nc
next_non_space_char_upper__loop:
    cp #20
    scf
    ret nz

    call fw_dba5_next_char_upper
    jr c, next_non_space_char_upper__loop
    ret


;------------------------------------------------------------------------
; Convert bytes to upper case
;
; input:
; - b: number of bytes left
; - hl: pointer to the previous byte converted
; output:
; - b: decremented by one (if > 0)
; - hl: incremented by one
; - a: resulting byte converted to upper case
; - carry: success
; - no carry: we are done
fw_dba5_next_char_upper:
    ld a, b
    or a
    ret z

    inc hl
    dec b
    ld a, (hl)  ; rst #20  ; firmware function: RST 4 - LOW: RAM LAM
    and #7f
    call fw_caa6_char_to_upper_case  ; convert character to upper case
    scf     
    ret


;------------------------------------------------------------------------
; table of invalid characters
fw_dbb2_invalid_characters_table:
    db '<'
    db '>'
    db '.'
    db ','
    db ';'
    db ':'
    db '='
    db '['
    db ']'
    db '_'
    db '%'
    db '|'
    db '('
    db ')'
    db '/'
    db '\'
    db #7f
    db 0


;------------------------------------------------------------------------
; copy 32 bytes from HL to DE 
; hl, de, bc preserved
fw_dbdf_ldir32:
    push hl
    push de
    push bc
        ld bc, 32
        ldir    
    pop bc
    pop de
    pop hl
    ret     


;------------------------------------------------------------------------
; HL shift right by A
fw_dbeb_shift_hl_right_by_a:
    srl h
    rr l
    dec a
    jr nz, fw_dbeb_shift_hl_right_by_a         
    ret     


;------------------------------------------------------------------------
; cp HL, DE
fw_dbf3_cp_hl_de:
    push hl
	    or a
	    sbc hl, de
    pop hl
    ret     


;------------------------------------------------------------------------
; LD HL, (HL)
fw_dbf9_ld_word_from_hl:
    push de
	    ld e, (hl)
	    inc hl
	    ld d, (hl)
	    ex de, hl
    pop de
    ret     


;------------------------------------------------------------------------
; find event in list
;
; input:
; - de: address of event block
; - hl: address of event list
find_event_in_list:
    ld a, (hl)
    cp e
    inc hl
    ld a, (hl)
    dec hl
    jr nz, find_event_in_list__skip
    cp d
    scf
    ret z  ; event found
find_event_in_list__skip:
    or a
    ret z  ; end of list reached (event not found)
    ; move to next element in the list: 
    ld l, (hl)
    ld h, a
    jr find_event_in_list


;------------------------------------------------------------------------
; add event to an event list
;
; input:
; - hl: address of event block
; - de: address of event list
add_event_to_an_event_list:
    ex de, hl
    di
    call find_event_in_list
    jr c, add_event_to_an_event_list__skip  ; event already in list
    ; add to head of list
    ld (hl), e
    inc hl
    ld (hl), d
    inc de
    xor a
    ld (de), a  ; set list.next = 0
add_event_to_an_event_list__skip:
    ei
    ret


;------------------------------------------------------------------------
; delete event from list
;
; input:
; - hl: address of event block
; - de: address of event list
delete_event_from_list:
    ex de, hl
    di
    call find_event_in_list  ; find event in list
    jr nc, delete_event_from_list__skip  ; event was not in the list
    ld a, (de)
    ld (hl), a
    inc de
    inc hl
    ld a, (de)
    ld (hl), a
delete_event_from_list__skip:
    ei
    ret


;------------------------------------------------------------------------

; AMSDOS work RAM2 offsets:
AMSDOS_work_RAM_offset_drive: equ 0
AMSDOS_work_RAM_offset_user: equ 1
AMSDOS_work_RAM_offset_active_drive: equ 2
AMSDOS_work_RAM_offset_DPH_ptr: equ 3  ; (eXtended Disk Parameter Block)
AMSDOS_work_RAM_offset_default_drive_flag: equ 5
AMSDOS_work_RAM_offset_return_address: equ 6
AMSDOS_work_RAM_offset_openin_control_block: equ #0008  ; offset for Extended File Control Block for OPENIN (36 bytes)
AMSDOS_work_RAM_offset_openout_control_block: equ #002c  ; offset for Extended File Control Block for OPENOUT (36 bytes)
AMSDOS_work_RAM_offset_openin_file_header: equ #0050  ; 74 bytes
AMSDOS_work_RAM_offset_openout_file_header: equ #009a  ; 74 bytes
AMSDOS_work_RAM_offset_tmp_record_buffer: equ #00e4
AMSDOS_work_RAM_offset_tmp_filename_buffer: equ #00e4
AMSDOS_work_RAM_offset_XDPB_drive_a: equ #0190
AMSDOS_work_RAM_offset_XDPB_drive_b: equ #01d0
AMSDOS_work_RAM_offset_DPH_drive_a: equ #0210
AMSDOS_work_RAM_offset_DPH_drive_b: equ #0220
AMSDOS_work_RAM_offset_directory_record_buffer: equ #0230
AMSDOS_work_RAM_offset_sector_buffer: equ #02b0  ; offset of sector buffer in AMSDOS work ram 

; XDPB parameter offsets:
AMSDOS_XDPB_offset_records_per_track: equ 0
AMSDOS_XDPB_offset_block_shift: equ 2
AMSDOS_XDPB_offset_block_mask: equ 3
AMSDOS_XDPB_offset_extent_mask: equ 4
AMSDOS_XDPB_offset_max_block_number: equ 5
AMSDOS_XDPB_offset_max_block_number_high: equ 6  ; high byte of AMSDOS_XDPB_offset_max_block_number
AMSDOS_XDPB_offset_last_valid_dir_entry_number: equ 7
AMSDOS_XDPB_offset_directory_allocation_table: equ 9
AMSDOS_XDPB_offset_checksum_area_size: equ 11
AMSDOS_XDPB_offset_track_offset: equ 13 
AMSDOS_XDPB_offset_first_sector_on_track: equ 15
AMSDOS_XDPB_offset_formatting_fillbyte: equ #13
AMSDOS_XDPB_offset_records_per_sector: equ #15
AMSDOS_XDPB_offset_current_track: equ #16
AMSDOS_XDPB_offset_detect_format_flag: equ #18

; DPH parameter offsets:
AMSDOS_DPH_offset_skew_factor_translation: equ 0
AMSDOS_DPH_offset_current_directory_number: equ 2  ; should be current track, but AMSDOS misuses it
AMSDOS_DPH_offset_current_sector: equ 4
AMSDOS_DPH_offset_current_directory_number_unused: equ 6
AMSDOS_DPH_offset_directory_buffer_ptr: equ 8
AMSDOS_DPH_offset_xdpb_ptr: equ 10
AMSDOS_DPH_offset_checksums_ptr: equ 12
AMSDOS_DPH_offset_allocation_table_ptr: equ 14

; Extended File Control Block (FCB) for OPENIN/OPENOUT offsets:
AMSDOS_FCB_offset_num_records_in_current_extent: equ #10
AMSDOS_FCB_offset_num_previously_accessed_records: equ #21

; File Header for OPENIN/OPENOUT
AMSDOS_FILEHEADER_offset_2k_buffer_ptr: equ #01
AMSDOS_FILEHEADER_offset_current_byte_in_2k_buffer_ptr: equ #01
AMSDOS_FILEHEADER_offset_filename: equ #05
AMSDOS_FILEHEADER_offset_file_type: equ #17  ; (#00: BASIC, #01: encrypted BASIC, #02: ninary)
AMSDOS_FILEHEADER_offset_data_length: equ #18
AMSDOS_FILEHEADER_offset_load_address: equ #1a
AMSDOS_FILEHEADER_offset_first_block: equ #1c
AMSDOS_FILEHEADER_offset_file_size_1: equ #1d
AMSDOS_FILEHEADER_offset_execution_entry_point: equ #1f
AMSDOS_FILEHEADER_offset_file_size_2: equ #45  ; filesize in bytes is stored twice for some reason in the header (see: https://www.cpcwiki.eu/index.php/AMSDOS_Memory_Map#File_Header_for_OPENIN.2FOPENOUT)

