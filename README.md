# PAKETCAS / PAKETDSK: assembler libraries to interface with tape and disk drive in amstrad CPC

PAKETCAS / PAKETDSK where developed for the PAKET engine. An engine for point and click adventure games for Amstrad CPC and MSX computers. 


# PAKETCAS

Amstrad CPC cassette tape load/save library for the PAKET engine
Santiago Ontañón, 2023

PAKETCAS is an assembler library to interface with the cassette tape in Amstrad CPC computers, without using the firmware. It was created by starting from a firmware disassemly, and extracting those functions that were necessary for loading and saving files to tape, annotating the whole source code to understand what each function did, and then simplifying as much as possible.

How to use:
- Include this file in your project
- The file assumes the existence of a label called "general_buffer", where it  can place it's RAM variables (it just needs 6 bytes of RAM to work).
- The two main functions are: paketcas_read and paketcas_write.


# PAKETDSK

Amstrad CPC disk file load/save library for the PAKET engine
Santiago Ontañón, 2024

PAKETDSK is an assembler library to interface with the disk drive in Amstrad CPC computers, without using the firmware. It was created by starting from an AMSDOS disassemly, and extracting those functions that were necessary for loading and saving files to disk, and then annotating the whole source code to understand what each function did. Note that labels are my interpretation of what the firmware functions did, and they could be mistaken. A pdf (paketdsk-v1-call-graph.pdf) that includes my initial investigation of the call graph of all the firmware functions related to disk to understand what did they do is included. Function names might not fully match the final version, since the pdf was created early in the process, and then the sourcefile evolved. But it contains useful notes to understand how things work, and I thought it was worth sharing. An earlier version (dsk-v0) where the original addresses of the AMSDOS functions are preserved in the labels is included in case anyone is interested in using PAKETDSK to understand how the CPC firmware works.

All the original memory areas used by the firmware in fixed addresses have been moved to the "paketdsk-ram.asm" file, so that you can place them wherever it is more convenient for your application/game.

How to use:
- include "paketdsk.asm" (this file), which contains all the assembler routines.
- include "paketdsk-ram.asm" in your RAM variables area.
- As soon as your program starts, and before the firmware memory areas have been overwriten, disable interrupts, and call "paketdsk_setup".
- After "paketdsk_setup" returns and before you enable interrupts again is a good time to install your own custom interrupt routine.
- In your custom interrupt, all you need to do is to call "paketdsk_interrupt_execute_tickers" at each VSYNC. See "readfile.asm" or "dsktest.asm" for example custom interrupts.
- After that, you can now just load and save files to disk by calling:
- paketdsk_load_file_from_disk
- paketdsk_save_file_to_disk
- The assembler syntax being used is standard Zilog notation, so that this can be used in almost any modern Z80 assembler. No local labels, nor fancy features of modern Z80 assemblers have been used other than IF/ELSE/ENDIF statements, to maximize compatibility. If IF/ELSE/ENDIF statements are not supported by your assembler, just comment the parts that you don't want out.
- See paketdsk-internal.asm for implementation notes, and explanations about the memory structures used to manage AMSDOS disk files.

Examples:
- A minimal example "readfile.asm" on how to use the library to read a file from disk is included.
- A thorough test to verify all functions work is included in "dsktest.asm". This can be used both as examples of how to use PAKETDSK, and also as a form of unit test. If you compile, put it in a disk and run it, it will go through a set of tests, writing and reading files to disk, and if all went well, it will turn the screen green. If there was any error, it will turn the screen red.


