;------------------------------------------------------------------------------
; AC1-Modul-1 (1M-Modul)
; (c) V. Pohlers 2023
;------------------------------------------------------------------------------
; Zusammensetzen des ROMs für das Modul-1
;------------------------------------------------------------------------------

        cpu 96C141			; Mikrocontroller mit großem Adressbereich
        maxmode on			; der Z80 reicht hier nicht.

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; MACRO a d d z 8 0 f i l e  für Z80-Programme

; file ist der Filename

; 2800h = 10K = Größe einer Bank
	
addz80file	macro fileXname

		align	0020h	
		BINCLUDE  fileXname

		endm

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; MACRO a d d f i l e
;
;addFile filename, aadr, [eadr], [sadr], typ, ["name"], "kommentar"
;
;filename mit Pfad
;eadr kann bei ungepackten Daten leer bleiben (dann = aadr+filelength)
;sadr kann leer bleiben (dann = aadr)
;Name = max 32 Zeichen; wenn leer, dann von / bis _ in filename.
;Kommentar = max. 16 Zeichen
;'Z' komprimiert, wenn filename ~ .zx7

lfdnr	eval	0
addFile	macro	fileXname, aXadr, eXadr, sXadr, pXtyp, pXname, pXComment
;;	if aXadr < 1900h 
;;	message	 "\{fileXname} wird nicht aufgenommen:  \{aXadr} < 1900h"
;;	else
	
lfdnr		eval	lfdnr+1

		align	40h
;	message	"\{$}"
	ifnb	eXadr
eadr		eval	eXadr
	else
eadr		eval	aXadr+file_bis-file_von
	endif
	ifnb	sXadr
sadr		eval	sXadr
	else
sadr		eval	aXadr
;sadr		eval	0
	endif
	ifnb	pXname
name	eval	pXname
	else
pos1	eval	strstr(fileXname,"/")+1
pos2	eval	strstr(fileXname,"_")-pos1
name	eval	substr(fileXname,pos1,pos2)
;	message "\{pos1} \{pos2} \{fileXname}"
	endif
;	message name
	
	if aXadr < 1900h 
	message	 "\{fileXname}:  \{aXadr} < 1900h"
offs	eval	1900h-aXadr
	else
offs	eval	0
	endif
		
		; Header
		db	0D3h,0D3h,0D3h	; 0 Kopfkenzeichen
		db	pXtyp		; 3 Typkennzeichen
		if STRSTR(fileXname,".zx7") > 0
			db	'Z'	; komprimiert
		else
			db	0	; normal
		endif
		db	"AC1"		; frei
		dw	offs		; offs
		dw	aXadr		; Anfangsadresse 
		dw	eadr	; Endadresse
		dw	sadr	; Startadresse
		db	substr(name+"                                ",0,32)
		db	substr(pXComment + "                ",0,16)
		; file

file_von
		BINCLUDE  fileXname
file_bis

	; Speichertest
	if $ > 8000h*32	; lastbyte
		warning "\{fileXname} passt nicht mehr aufs Modul!"
	endif


;;	endif
	endm

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; die Files
; um ein bin-File komprimiert zu speichern, muss einfach die Endung .zx7 angehängt werden

; aus d:\hobby3\ac1-2010\modul_1\programme3
	if 1=1 ; unkomprimiert
	addfile "software1/CP_M-2.2_D000_FEFF_E600.bin"		,0D000h,0FEFFh,0E600h,'P',"HRDOS12-AC2010","JE600"	; HR: HRDOS12-AC2010_D000_FEFF_E600.BIN
	addfile "software1/HRCPM-12_D000_FEFF_E600.bin"		,0D000h,0FEFFh,0E600h,'P','HRCPM12-AC2010',"JE600"	; HR: HRCPM12-AC2010_D000_FEFF_E600.BIN
	addfile "software1/DVU-21_4000_531F_4000.bin"		,04000h,0531Fh,04000h,'P', ,"d,t"
	addfile	"software1/DVHD-31_4000_57EB_4000.bin"		,04000h,057EBh,04000h,'P', ,"d,t"			; neuer als HR: DVHD3_4000_575E_4000.BIN
	addfile	"software1/Gide-Check_2000_2FE7_2000.bin"	,02000h,02FE7h,02000h,'P', ,"g"				; neuer als HR: GIDE-CHECK_2000.BIN
	addfile	"software1/EDAS-4_4000_544F_4000.bin"		,04000h,0544Fh,04000h,'P', ,"J4000"
	addfile	"software1/ASSEMBLER_4000_5447_4000.bin"		,04000h,05447h,04000h,'P', ,"a"
	addfile	"software1/COMPILER_5600_6000_5600.bin"		,05600h,06000h,05600h,'P', ,"8"
	addfile	"software1/DEBUGGER_4000_5861_4000.bin"		,04000h,05861h,04000h,'P', ,"J4000"
	addfile	"software1/DIASSEMBLER-3_5800_5FFF_5800.bin"	,05800h,05FFFh,05800h,'P', "DISASSEMBLER-3","J5800"
	addfile	"software1/RAM-TEST_2000_26BF_2000.bin"		,02000h,026BFh,02000h,'P', ,"g"
	addfile	"software1/DISK-1F_3000_3BF1_3000.bin"		,03000h,03BF1h,03000h,'P', ,"d"
	addfile	"software1/OCEAC1_4000_5D44_4000.bin"		,04000h,05D44h,04000h,'P', ,"J4000"
	addfile	"software1/FDC-iA_2000_27FF_0000.bin"		,02000h,027FFh,00000h,'P', ,"s,l,d,k,f,i,x"
	else ; komprimiert
	addfile "software1/CP_M-2.2_D000_FEFF_E600.bin.zx7"	,0D000h,0FEFFh,0E600h,'P',"HRDOS12-AC2010","JE600"	; HR: HRDOS12-AC2010_D000_FEFF_E600.BIN
	addfile "software1/HRCPM-12_D000_FEFF_E600.bin.zx7"	,0D000h,0FEFFh,0E600h,'P','HRCPM12-AC2010',"JE600"	; HR: HRCPM12-AC2010_D000_FEFF_E600.BIN
	addfile	"software1/DVU-21_4000_531F_4000.bin.zx7"	,04000h,0531Fh,04000h,'P', ,"d,t"
	addfile	"software1/DVHD-31_4000_57EB_4000.bin.zx7"	,04000h,057EBh,04000h,'P', ,"d,t"			; neuer als HR: DVHD3_4000_575E_4000.BIN
	addfile	"software1/Gide-Check_2000_2FE7_2000.bin.zx7"	,02000h,02FE7h,02000h,'P', ,"g"				; neuer als HR: GIDE-CHECK_2000.BIN
	addfile	"software1/EDAS-4_4000_544F_4000.bin.zx7"	,04000h,0544Fh,04000h,'P', ,"J4000"
	addfile	"software1/ASSEMBLER_4000_5447_4000.bin.zx7"	,04000h,05447h,04000h,'P', ,"a"
	addfile	"software1/COMPILER_5600_6000_5600.bin.zx7"	,05600h,06000h,05600h,'P', ,"8"
	addfile	"software1/DEBUGGER_4000_5861_4000.bin.zx7"	,04000h,05861h,04000h,'P', ,"J4000"
	addfile	"software1/DIASSEMBLER-3_5800_5FFF_5800.bin.zx7"	,05800h,05FFFh,05800h,'P', "DISASSEMBLER-3","J5800"
	addfile	"software1/RAM-TEST_2000_26BF_2000.bin.zx7"	,02000h,026BFh,02000h,'P', ,"g"
	addfile	"software1/DISK-1F_3000_3BF1_3000.bin.zx7"	,03000h,03BF1h,03000h,'P', ,"d"
	addfile	"software1/OCEAC1_4000_5D44_4000.bin.zx7"	,04000h,05D44h,04000h,'P', ,"J4000"
	addfile	"software1/FDC-iA_2000_27FF_0000.bin.zx7"	,02000h,027FFh,00000h,'P', ,"s,l,d,k,f,i,x"
	endif
	

	addfile	"minibasic/minibasic_4000.bin.zx7"	,04000h,,04000h,'P', ,"AC1 Minibasic"
	shared	minibasic
minibasic	equ	lfdnr	

	;software-ralf-haensel
	addfile	"software-ralf-haensel/AC1-TurboSaveLoad_1900.bin.zx7"	,01900h,,,'P', ,"q"
	addfile	"software-ralf-haensel/AC1-V24SaveLoad_1900.bin.zx7"	,01900h,,,'P', ,"q"
	; == "software-ralf-haensel/AC1-V24SAVELOAD.bin.zx7"
	addfile	"software-ralf-haensel/AC1-V24Testtool_1900.bin.zx7"	,01900h,,,'P', ,"Auto"
	addfile	"software-ralf-haensel/Check+CPM-Umgebung_2000_25FF_2000.bin.zx7"	,02000h,025ffh,0000h,'P', ,"t"
	;;addfile	"software-ralf-haensel/DVHD3_4000_575E_4000.bin.zx7"	,04000h,0575eh,04000h,'P', ,"d,t"
	;;addfile	"software-ralf-haensel/DVU21_4000_531F_4000.bin.zx7"	,04000h,0531fh,04000h,'P', ,"d,t"
	;; ==  "software-ralf-haensel/DVU21-221111.bin.zx7"
	;;addfile	"software-ralf-haensel/GIDE-CHECK_2000.bin.zx7"	,02000h,,,'P', ,"g"
	;;addfile	"software-ralf-haensel/HRCPM12-AC2010_D000_FEFF_E600.bin.zx7"	,0d000h,0feffh,0e600h,'P', ,"J E600"
	;;addfile	"software-ralf-haensel/HRDOS12-AC2010_D000_FEFF_E600.bin.zx7"	,0d000h,0feffh,0e600h,'P', ,"J E600"
	addfile	"software-ralf-haensel/HRCPM12-SCCH_D000_FEFF_E600.bin.zx7"	,0d000h,0feffh,0e600h,'P', ,"JE600"
	addfile	"software-ralf-haensel/HRDOS12-SCCH_D000_FEFF_E600.bin.zx7"	,0d000h,0feffh,0e600h,'P', ,"JE600"
	addfile	"software-ralf-haensel/IO-Portwerte_1900.bin.zx7"	,01900h,,,'P', ,"t"
	addfile	"software-ralf-haensel/USB-CHECK_E000.bin.zx7"	,0E000h,,,'P', ,"t"
	addfile	"software-ralf-haensel/Z1013-Monitor-V1.2_F000_FE48_F009.bin.zx7"	,0F000h,0FE48h,0F009h,'P', ,"Y"


	; einfach erstmal alles. Muss später sortiert und kommentiert werden!
	include	z80files.inc

hier 	equ $
	SHARED hier

;; Ende
	org 8000h*32-1

	db 0ffh

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        end
