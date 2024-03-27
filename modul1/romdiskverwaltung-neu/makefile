#
# Makefile für AC1 Modul-1
# VPohlers 30.01.2023

# Anpassen an den Speicherort des Arnold-Assemblers

AS=d:\\hobby3\\programme\\as\\bin\\as.exe
P2BIN=d:\\hobby3\\programme\\as\\bin\\p2bin.exe
PLIST=d:\\hobby3\\programme\\as\\bin\\plist.exe
PERL=d:\\perl\\bin\\perl.exe



DEPENDFILE = .depend

#make		für Emulator
ALL:	$(DEPENDFILE) paket_x.bin packedroms.bin jkcemu_xrom.bin
		$(PERL) showrom.pl

#make roms	split ROMs
roms:	$(DEPENDFILE) packedroms.bin rom_1.bin rom_2.bin jkcemu_xrom.bin

depend:
	$(PERL) mkdepend.pl *.asm */*.asm z80files.inc > $(DEPENDFILE)

-include $(DEPENDFILE)

z80files.inc:
	$(PERL) create_z80filesinc2.pl 2> unusable_rom_files

# Assemblieren einer Datei
%.bin:	%.asm
	$(AS) -cpu Z80 -L $< -a
	$(P2BIN) -r \$$-\$$ $*.p
#	del $*.p

%.bin:	%.z80
	$(PERL) z802bin.pl $<

# Komprimieren
%.zx7:	%
	zx7.exe -f $<

CLEAN_FILES=*.bin

clean:	
	del /s *.p
	del /s *.inc
	del /s *.lst
	del $(CLEAN_FILES)
#	del $(DEPENDFILE)

clobber: clean
	del $(DEPENDFILE)

DATE=$(strip $(shell date /t))

DISTFILES =  *.asm  */*.asm  */*.z80 */*/*.z80 */*.txt */*/*.txt  */*.pdf */*/*.pdf \
	software1/*.bin software-ralf-haensel/*.bin \
	makefile *.exe *.pl z80files.inc \
	GrafikBasic3.2ROM.bin basicarb_6000_60f6.bin \
	.depend 00liesmich fileXname 

#make dist	Quellarchiv packen
dist: ALL
	zip -r ac1-modul-1.$(DATE).zip $(DISTFILES)

ROMFILES =  jkcemu_xrom.bin paket_x.bin jkcemu-ac1.jar rom_*.bin GrafikBasic3.2ROM.bin \
	00liesmich rominhalt.txt

romdist distrom: roms
	zip -r ac1-modul-1-roms.$(DATE).zip $(ROMFILES)
