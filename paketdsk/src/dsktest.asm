;------------------------------------------------------------------------
; Amstrad CPC disk file load/save library for the PAKET engine
; Santiago Ontañón, 2024
;
; Test program that runs a bunch of tests to make sure that PAKETDSK works,
; and also to serve as an example of how to use it.
;
; The different tests being done are:
; - Test 1: Attempts to load a file that does not exist, and verifies that it fails.
; - Test 2: Writes a file to disk, and then reads it again.
; - Test 3: Writes the test2 file again, and if all goes well, and checks that a .BAK file is generated to keep the original test 2 file.
; - Test 4: Like test2, but with a file 20kb in size (so that it uses more than one extent)
; - Test 5: Attempts to save a file with a filename with only spaces, and verifies that it fails.
; 
; If all tests pass, you will see a green screen.
; If any test fails, you will see a red screen.


;------------------------------------------------------------------------
    org #4000

start_of_program:
    di  ; disable interrupts while setting PAKETDSK
    ld sp, end_of_stack  ; initialize the stack (we put it very low, so that we have a big chunk
                         ; of uninterrupted memory to play around with for tests)
    xor a  ; drive = 0
    call paketdsk_setup

    call clear_all_ram  ; just for debugging purposes, to erase all traces of the firmware variables

    ; set custom interrupt:
    ld hl, custom_interrupt
    ld (#0039),hl
    ei

    ; Run tests:
    call test1_read_inexistent_file
    jr nc, error_loop

    call test2_write_then_read_file
    jr nc, error_loop

    call test3_write_again_and_check_bak_file_is_there  ; this test can only be run after test2
    jr nc, error_loop

    call test4_write_then_read_large_file
    jr nc, error_loop

    call test5_write_space_named_file
    jr nc, error_loop

    ; All tests passed!

loop:
    ; set background color to green:
    ld e, #56
    call set_palette
    jp loop

error_loop:
    ; set background color to red:
    ld e, #4c
    call set_palette
    jp error_loop


;------------------------------------------------------------------------
; Example custom interrupt. All you need to do to use the PAKETDSK library
; is to make sure the "interrupt_execute_tickers" method is called at each
; VSYNC. At least while the disk functions are being used.
custom_interrupt:
    push hl
    push de
    push bc
    push af
        ; Check if we are in VSYNC and execute tickers if needed
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
; Sets all 4 colors of mode 1 to the same hardware color (just to have visual feedback)
; input:
; - e: hardware color to use for the whole palette
set_palette:
    di
    ld iyl, 4
    xor a
set_palette_colors_loop:
    push af
        ; set pen "a" to hardware color "palette[a]":
        ld bc, #7f00   ; Gate Array port
        out (c), a     ; Send pen number
        out (c), e     ; Send color
    pop af
    inc hl
    inc a
    cp iyl
    jr nz, set_palette_colors_loop
    ei
    ret


;------------------------------------------------------------------------
clear_all_ram:
    ; clear all the RAM that this program does not use:
    ld de, #40
    ld bc, #4000 - #40
    call clear_memory
    ld de, end_of_ram
    ld bc, (#c000 - 4) - end_of_ram  ; leave 2 bytes for the stack when calling "clear_memory"
    call clear_memory
    ld de, #c000
    ld bc, #4000
    jp clear_memory


;------------------------------------------------------------------------
; Random number generation, see: https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Random
; cycle: 4,294,901,760 (almost 4.3 billion)
; input:
;   - (seed1) contains a 16-bit seed value
;   - (seed2) contains a NON-ZERO 16-bit seed value
; output:
;   - hl: 16 bit random number
prng16:
    push bc
    push af
        ld hl, (seed1)
        ld b, h
        ld c, l
        add hl, hl
        add hl, hl
        inc l
        add hl, bc
        ld (seed1), hl
        ld hl, (seed2)
        add hl, hl
        sbc a, a
        and #2d
        xor l
        ld l, a
        ld (seed2), hl
        add hl, bc
    pop af
    pop bc
    ret


;------------------------------------------------------------------------
; output:
; - carry: test passed
; - no carry: test failed
test1_read_inexistent_file
    ld b, filename_test1_end - filename_test1
    ld hl, filename_test1
    ld iy, 0  ; default loading address
    call paketdsk_load_file_from_disk  ; carry is set upon success
    ccf  ; revert carry flag, as we want this test to pass if the previous method fails.
    ret


;------------------------------------------------------------------------
; output:
; - carry: test passed
; - no carry: test failed
test2_write_then_read_file:
    ; copy data to the place where we want it to be loaded:
    ld hl, data_test2
    ld de, test2_file_address
    ld bc, data_test2_end - data_test2
    ldir

    ; save the file:
    ld b, filename_test2_end - filename_test2
    ld de, data_test2_end - data_test2
    ld hl, filename_test2
    ld ix, test2_file_address
    call paketdsk_save_file_to_disk
    ret nc

    ; clear the memory, so that we can verify if we can load it again:
    ld de, test2_file_address
    ld bc, data_test2_end - data_test2
    call clear_memory

    ; load it again
    ld b, filename_test2_end - filename_test2
    ld hl, filename_test2
    ld iy, 0  ; default loading address
    call paketdsk_load_file_from_disk
    ret nc

test2_write_then_read_file_verify_loaded_content:
    ; Check that the loaded data matches:
    ld hl, test2_file_address
    ld de, data_test2
    ld b, data_test2_end - data_test2
test2_check_loop:
    ld a, (de)
    cp (hl)
    jr nz, test2_fail
    inc de
    inc hl
    djnz test2_check_loop  
    scf
    ret  
test2_fail:
test4_fail:
    scf
    ccf
    ret


;------------------------------------------------------------------------
; output:
; - carry: test passed
; - no carry: test failed
test3_write_again_and_check_bak_file_is_there:
    ; copy data to the place where we want it to be loaded:
    ld hl, data_test2
    ld de, test2_file_address
    ld bc, data_test2_end - data_test2
    ldir

    ; save the file:
    ld b, filename_test2_end - filename_test2
    ld de, data_test2_end - data_test2
    ld hl, filename_test2
    ld ix, test2_file_address
    call paketdsk_save_file_to_disk
    ret nc

    ; clear the memory, so that we can verify if we can load it again:
    ld de, test2_file_address
    ld bc, data_test2_end - data_test2
    call clear_memory

    ; load the .BAK file
    ld b, filename_test3_end - filename_test3
    ld hl, filename_test3
    ld iy, 0  ; default loading address
    call paketdsk_load_file_from_disk
    ret nc

    jp test2_write_then_read_file_verify_loaded_content


;------------------------------------------------------------------------
; output:
; - carry: test passed
; - no carry: test failed
test4_write_then_read_large_file:
    ; Generate random data with a random number generator:
    ld bc, test4_file_size / 2
    ld hl, test4_file_address
test4_write_then_read_large_file_generation_loop1:
    ex de, hl
    call prng16
    ex de, hl
    ld (hl), e
    inc hl
    ld (hl), d
    inc hl
    dec bc
    ld a, b
    or c
    jr nz, test4_write_then_read_large_file_generation_loop1

    ; save the file:
    ld b, filename_test4_end - filename_test4
    ld de, test4_file_size
    ld hl, filename_test4
    ld ix, test4_file_address
    call paketdsk_save_file_to_disk
    ret nc

    ; clear the memory, so that we can verify if we can load it again:
    ld de, test4_file_address
    ld bc, test4_file_size
    call clear_memory

    ; load it again
    ld b, filename_test4_end - filename_test4
    ld hl, filename_test4
    ld iy, 0  ; default loading address
    call paketdsk_load_file_from_disk
    ret nc

    ; Check that the loaded data matches:
    ; Reinitialize the random number generator to the same seeds as when we saved the file,
    ; so we can verify that reading we get the same data:
    ld hl, RNG_SEED1
    ld (seed1), hl
    ld hl, RNG_SEED2
    ld (seed2), hl

    ld hl, test4_file_address
    ld bc, test4_file_size / 2
test4_check_loop:
    ex de, hl
    call prng16
    ex de, hl    
    ld a, (hl)
    inc hl
    cp e
    jp nz, test4_fail
    ld a, (hl)
    inc hl
    cp d
    jp nz, test4_fail
    dec bc
    ld a, b
    or c
    jr nz, test4_check_loop  
    scf
    ret  


;------------------------------------------------------------------------
; output:
; - carry: test passed
; - no carry: test failed
test5_write_space_named_file
    ; copy data to the place where we want it to be loaded:
    ld hl, data_test2
    ld de, test2_file_address
    ld bc, data_test2_end - data_test2
    ldir

    ; save the file:
    ld b, filename_test5_end - filename_test5
    ld de, data_test2_end - data_test2
    ld hl, filename_test5
    ld ix, test2_file_address
    call paketdsk_save_file_to_disk
    ccf  ; revert carry flag, as we want this test to pass if the previous method fails.
    ret


;------------------------------------------------------------------------
; includes:
    include "dsk/paketdsk.asm"


;------------------------------------------------------------------------
; data:
filename_test1:  ; the name of a file that does not exist in the disk
    db "TEST1.BIN"
filename_test1_end:

test2_file_address: equ #b000
filename_test2:  ; the name of the file we will write
    db "TEST2.BIN"
filename_test2_end:
data_test2:  ; the data to be written
    db "Data to write in test2"
data_test2_end:

filename_test3:  ; the name of the file we want to verify exists
    db "TEST2.BAK"
filename_test3_end:

test4_file_address: equ end_of_ram
test4_file_size: equ 20 * 1024 + 64
filename_test4:
    db "LARGE.BIN"
filename_test4_end:

filename_test5:
    db "    "
filename_test5_end:

RNG_SEED1: equ #1234
RNG_SEED2: equ #6789
seed1:
    dw RNG_SEED1
seed2:
    dw RNG_SEED2

end_of_binary:


;------------------------------------------------------------------------
; RAM:
    include "dsk/paketdsk-ram.asm"

stack:
    ds virtual 256  ; 256 bytes for the stack
end_of_stack:


end_of_ram:
