set path=d:\hobby3\Programme; d:\hobby3\Programme\as\bin;%path%

call as mon_v8.asm
call as mon_v8-fdc-rfl.asm
call as mon_1088.asm
call as AC1-TURBO.asm

echo on

bdiff mon_v8.rom mon_v8.bin
bdiff mon_1088.rom mon_1088.bin
bdiff mon2010c.rom mon_v8-fdc-rfl.bin
bdiff AC1-TURBO-LOAD-C_2000_284F_2000.rom AC1-TURBO.bin
