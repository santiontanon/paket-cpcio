;------------------------------------------------------------------------
; Amstrad CPC disk file load/save library for the PAKET engine
; Santiago Ontañón, 2024

paketdsk_start_of_ram_paketdsk:

address_of_the_first_ticker_block_in_chain: ds virtual 2

AMSDOS_RAM2_start: ds virtual 1280
AMSDOS_RAM2_end:

AMSDOS_RAM1_start:
; The values after "BIOS:" are the values observed in the BIOS at startup, just for reference.
memory_be40_cpm_dph_ptr_for_drive_0: ds virtual 2       ; BIOS: #a910
memory_be42_drive_0_xdpb_ptr: ds virtual 2              ; BIOS: #a890
memory_be44_motor_spinup_delay: ds virtual 2            ; BIOS: #0032
memory_be46_motor_spin_down_delay: ds virtual 2         ; BIOS: #00fa
memory_be48_formatting_delay: ds virtual 1              ; BIOS: #af
memory_be49_delay: ds virtual 1                         ; BIOS: #0f
memory_be4a_track_settle_delay: ds virtual 1            ; BIOS: #0c
memory_be4b_result_phase_received_n_bytes: ds virtual 1 ; BIOS: #07
memory_be4c_fdc_response: ds virtual 7                  ; BIOS: #40, #80, #00, #02, #00, #c5, #02
memory_be53_drive_hs_us: ds virtual 1                   ; BIOS: #00
memory_be54_track: ds virtual 1                         ; BIOS: #02
memory_be55_record: ds virtual 1                        ; BIOS: #13
memory_be56_drive_hs_us_2: ds virtual 1                 ; BIOS: #00
memory_be57_track_2: ds virtual 1                       ; BIOS: #02
memory_be58_sector_2: ds virtual 1                      ; BIOS: #04
memory_be59_n_records_left: ds virtual 1                ; BIOS: #00
memory_be5a_drive_hs_us_3: ds virtual 1                 ; BIOS: #00
memory_be5b_track_3: ds virtual 1                       ; BIOS: #00
memory_be5c_record_3: ds virtual 1                      ; BIOS: #00
memory_be5d: ds virtual 1                               ; BIOS: #00
memory_be5e_read_write_sector_flag: ds virtual 1        ; BIOS: #ff  (set fo #ff after a successful read/write)
memory_be5f_motor_on_off: ds virtual 1                  ; BIOS: #00  (0: off, !=0: on)
memory_be60_directory_buffer_ptr: ds virtual 2          ; BIOS: #a7e4
memory_be62_sector_buffer_ptr: ds virtual 2             ; BIOS: #a9b0
memory_be64_temporary_stack: ds virtual 2               ; BIOS: #bfd0
memory_be66_retry_count: ds virtual 1                   ; BIOS: #10
memory_be67_ticker_block: ds virtual 6  ; chain ptr (2 bytes), count (2 bytes), count reset (2 bytes): #0000, #00fa, #0000
memory_be6d_event_block: ds virtual 7  ; chain (2 bytes), count (1 byte), class (1 byte: synchronous / asynchronous), routine ptr (2 bytes), bank (1 byte): #0000, #00, #80 #c9d6, #07
memory_be74_desired_track_sector_number: ds virtual 1   ; BIOS: #c5
memory_be75_command: ds virtual 1                       ; BIOS: #66
memory_be76_sector_buffer_ptr: ds virtual 2             ; BIOS: #a9b0
memory_be78: ds virtual 1                               ; BIOS: #00
memory_be79: ds virtual 1                               ; BIOS: #00
memory_be7a: ds virtual 1                               ; BIOS: #00
memory_be7b: ds virtual 2                               ; BIOS: #00
memory_be7d_AMSDOS_RAM2_ptr: ds virtual 2               ; BIOS: #a700
memory_be7f: ds virtual 1                               ; BIOS: #c9
AMSDOS_RAM1_end:

paketdsk_end_of_ram_paketdsk:
