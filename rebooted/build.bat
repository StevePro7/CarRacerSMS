@echo off
rem ==========
rem  Compiling batch file for WLA assembler and linker
rem ==========

rem Compile
wla-z80 -o main.asm main.o

rem Link
wlalink -drvs link_file.txt output.sms