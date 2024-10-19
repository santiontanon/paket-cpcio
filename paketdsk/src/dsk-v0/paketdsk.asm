;------------------------------------------------------------------------
; PAKETDsk v0
; Amstrad CPC disk file load/save library for the PAKET engine
; Santiago Ontañón, 2024
;
; PAKETDSK is an assembler library to interface with the disk drive in Amstrad CPC computers,
; without using the firmware. It was created by starting from an AMSDOS disassemly, and extracting
; those functions that were necessary for loading and saving files to disk, and then annotating
; the whole source code to understand what each function did.
;
; This version contains exactly the routines used by the AMSDOS firmware, and the only modifications
; done are:
; - create symbol names for all functions
; - add constant definitions for all the numerical constants
; - remove unnecessary code (including all the error messaging printing, as in PAKETDSK
;   funtions just set the carry flag for success and reset it for failure, like AMSDOS does,
;   but there are no printed messages to screen).
; - all the memory areas have been moved to paketdsk-ram.asm, so that they can be placed anywhere.
; - other than that, this file contains pretty much exactly the AMSDOS routines with little to no
;   modification.
;
; All the original memory areas used by the firmware in fixed addresses have been moved to the
; "paketdsk-ram.asm" file, so that you can place them wherever it is more convenient for your
; application/game.
; 
; How to use:
; - include "paketdsk.asm" (this file), which contains all the assembler routines.
; - include "paketdsk-ram.asm" in your RAM variables area.
; - As soon as your program starts, and before the firmware memory areas have been overwriten,
;   disable interrupts, and call "paketdsk_setup".
; - After "paketdsk_setup" returns and before you enable interrupts again is a good time to
;   install your own custom interrupt routine.
; - In your custom interrupt, all you need to do is to call "paketdsk_interrupt_execute_tickers"
;   at each VSYNC. See "readfile.asm" or "dsktest.asm" for example custom interrupts.
; - After that, you can now just load and save files to disk by calling:
;   - paketdsk_load_file_from_disk
;   - paketdsk_save_file_to_disk
; - The assembler syntax being used is standard Zilog notation, so that this can be used in almost
;   any modern Z80 assembler. No local labels, nor fancy features of modern Z80 assemblers have been
;   used other than IF/ELSE/ENDIF statements, to maximize compatibility. If IF/ELSE/ENDIG statements
;   are not supported by your assembler, just comment the parts that you don't want out.
; - See paketdsk-internal.asm for implementation notes, and explanations about the memory structures
;   used to manage AMSDOS disk files.


;------------------------------------------------------------------------
; Flags to turn on/off different functionalities of PAKETDSK 
SUPPORT_FORMATTING: equ 1  ; set this to 1, if you want to include formatting code. 
SUPPORT_IBM_FORMAT: equ 0


;------------------------------------------------------------------------
paketdsk_start_of_paketdsk:


;------------------------------------------------------------------------
; Addresses where things are when the firmware is active:
AMSDOS_RAM1_bios_start: equ #be40
AMSDOS_RAM1_bios_end: equ #be80
AMSDOS_RAM2_bios_start: equ #a700
AMSDOS_RAM2_bios_ptr: equ #be7d


;------------------------------------------------------------------------
; This method extracts all the info from the firmware, and puts it into the
; PAKETDSK equivalent memory areas. It should be called before the firmware areas
; have been overwritten, and with interrupts disabled.
; 
; Once this method returns, from taht point on, you can feel free to kick the
; firmware away, and use all the memory (it would also be a good idea to install
; your custom interrupt routine before enabling interrupts again).
; 
; input:
; - a: drive
paketdsk_setup:
    ld hl, (AMSDOS_RAM2_bios_ptr)  ; #a700
    ld (hl), a

    ; Init the disk memory areas:
    ld hl, 0
    ld (address_of_the_first_ticker_block_in_chain), hl
    ld hl, AMSDOS_RAM1_bios_start
    ld de, AMSDOS_RAM1_start
    ld bc, AMSDOS_RAM1_end - AMSDOS_RAM1_start
    ldir
    ld hl, (AMSDOS_RAM2_bios_ptr)
    ld de, AMSDOS_RAM2_start
    ld bc, 1280
    ldir

    ; update ptrs:
    ld hl, fw_c9d6_motor_ticker
    ld (memory_be6d_event_block + 4), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_XDPB_drive_a
    ld (memory_be42_drive_0_xdpb_ptr), hl
    ld hl, AMSDOS_RAM2_start + #00e4
    ld (memory_be60_directory_buffer_ptr), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_sector_buffer  ; + #02b0
    ld (memory_be62_sector_buffer_ptr), hl
    ld (memory_be76_sector_buffer_ptr), hl
    ld hl, AMSDOS_RAM2_start
    ld (memory_be7d_AMSDOS_RAM2_ptr), hl
    ; disk parameter header A:
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_directory_record_buffer
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_a + 8), hl
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_b + 8), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_XDPB_drive_a
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_a + 10), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_XDPB_drive_b
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_b + 10), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_XDPB_drive_a + #0019  ; ptr to checksums (CSA)
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_a + 12), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_XDPB_drive_b + #0019  ; ptr to checksums (CSA)
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_b + 12), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_XDPB_drive_a + #0029  ; ptr to allocation table (ALT)
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_a + 14), hl
    ld hl, AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_XDPB_drive_b + #0029  ; ptr to allocation table (ALT)
    ld (AMSDOS_RAM2_start + AMSDOS_work_RAM_offset_DPH_drive_b + 14), hl
    ret


;------------------------------------------------------------------------
; Loads a file from disk. The file is loaded to the address specified in 
; the file header.
; 
; input:
; - b: length of filename
; - hl: address of filename
; - iy: ptr to a 2k buffer for the disk routines
; output:
; - carry: success
; - no carry: error
paketdsk_load_file_from_disk:
    push iy
        ld de, (memory_be7d_AMSDOS_RAM2_ptr)
        push de
        pop iy
    pop de
    call fw_ceaf_cas_in_open
    ret nc
    ex de, hl  ; load file to location stored in the file header
    call fw_cff5_cas_in_direct
    ret nc
    jp fw_d1b6_cas_in_close


;------------------------------------------------------------------------
; Saves a file to disk
; 
; input:
; - b: length of filename
; - de: data size
; - hl: address of filename
; - ix: data to save
; - iy: ptr to a 2k buffer for the disk routines
; output:
; - carry: success
; - no carry: error
paketdsk_save_file_to_disk:
    push ix
    push de
        push iy
            ld de, (memory_be7d_AMSDOS_RAM2_ptr)
            push de
            pop iy
        pop de
        call fw_cf37_cas_out_open
    pop de
    pop hl
    ret nc  ; error
    ld b, h
    ld c, l
    ld a, 2  ; binary data
    call fw_d0d8_cas_out_direct
    ret nc
    jp fw_d1d8_cas_out_close    


;------------------------------------------------------------------------
; Execute tickers.
; - You should call this method in each VSYNC interrupt from your custom interrupt routine. 
;
; Each ticker is 6 bytes (ticker) + 7 bytes (event)
; - 2 bytes: next ticker ptr
; - 2 bytes: countdown to excution
; - 2 bytes: count reset (value that the count will reset to after execution)
; - 7 bytes: event block (see KL_EVENT)
paketdsk_interrupt_execute_tickers:
    xor a
    ex af, af'  ; we set the carry flag of af' off
    ld hl, (address_of_the_first_ticker_block_in_chain)
paketdsk_interrupt_execute_tickers__loop:
    ld a, h
    or a
    ret z  ; if there are no tickers, return

    ld e, (hl)
    inc hl
    ld d, (hl)  ; de = address of the next ticker
    inc hl
    ld c, (hl)
    inc hl
    ld b, (hl)  ; bc = count
    ld a, b
    or c
    jr z, paketdsk_interrupt_execute_tickers__skip  ; this ticker was already executed, skip
    dec bc
    ld a, b
    or c
    jr nz, paketdsk_interrupt_execute_tickers__update_count  ; count has not yet reached 0
    ; execute ticker!
    push de
        inc hl
        inc hl
        push hl
            inc hl  ; hl = ptr to the event block associated with this ticker
            call paketdsk_interrupt_execute_ticker
        pop hl
        ld b, (hl)
        dec hl
        ld c, (hl)  ; bc = count reset
        dec hl
    pop de
paketdsk_interrupt_execute_tickers__update_count:
    ld (hl), b
    dec hl
    ld (hl), c
paketdsk_interrupt_execute_tickers__skip:
    ex de, hl  ; hl = address of the next ticker
    jr paketdsk_interrupt_execute_tickers__loop


;------------------------------------------------------------------------
; Internal functions that are not part of the public API
    include "paketdsk-internal.asm"


paketdsk_end_of_paketdsk:
