;------------------------------------------------------------------------
; Amstrad CPC disk file load/save library for the PAKET engine
; Santiago Ontañón, 2024
;
; Minimal example of how to load a file from disk.


;------------------------------------------------------------------------
    org #4000

start_of_program:
    di
    ld sp, #c000
    xor a  ; drive = 0
    call paketdsk_setup

    ; set custom interrupt:
    ld hl, custom_interrupt
    ld (#0039), hl
    ei

    ld b, filename_end - filename
    ld hl, filename
    ld iy, buffer  ; default load address in case the file does not specify one
    call paketdsk_load_file_from_disk

loop:
    jp loop


;------------------------------------------------------------------------
custom_interrupt:
    push hl
    push de
    push bc
    push af
        ; execute tickers:
        ld c, 0
        ld b, $f5
        in a, (c)
        rra
        call c, paketdsk_interrupt_execute_tickers  ; only only on VSYNC
    pop af
    pop bc
    pop de
    pop hl
    ei
    ret


;------------------------------------------------------------------------
filename:
    db "DATA.BIN"
filename_end:

    include "dsk/paketdsk.asm"

end_of_binary:


;------------------------------------------------------------------------
; RAM:
    include "dsk/paketdsk-ram.asm"

buffer:  ; default load address in case the file does not specify one
    ds virtual 2048


end_of_ram:


