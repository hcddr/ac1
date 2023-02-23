; File Name   :	d:\hobby3\ac1-2010\modul_1\PaketX_AC1_2010.bin
; Base Address:	0000h Range: E000h - 10000h Loaded length: 2000h
; reass V.Pohlers 10.01.2023

;------------------------------------------------------------------------------
; Modulsoftware für Modul_1
;
; Port 14h, OUT-Port
; 	00 - Modul nicht aktiv
; 	01 - 8K-ROM "PaketX" E000-FFFF
; 	02 - 16K-ROM "Basic" 2000-5FFF
; 	x8 - 512K ROM1 aktiv, jeweils 32K-Bänke, x = 0..F Bank0..Bank15, 8000-FFFF
; 	x9 - 512K ROM2 aktiv, jeweils 32K-Bänke, x = 0..F Bank0..Bank15, 8000-FFFF
;------------------------------------------------------------------------------

		cpu	z80undoc

		include	ac1-2010.asm

		org 0E000h

loc_E000:	jp	menu

UMLAD:		jp	umlad0		; Umladeroutine	für Programm
GETARG:		jp	getarg0		; Argumente

;------------------------------------------------------------------------------
; Anzeige ROM-Disk-Inhalt, Starten von Programmen
;------------------------------------------------------------------------------

menu:		ld	sp, SYSSK	; System-Stack
		ld	hl, GETCO1	; Sprung zur Monitoreingabeschleife
		push	hl		; als Return-Adr auf Stack
		;
menu1:		rst	PRNST
		db 	0Ch,0Dh,"     * * *  ROM - Disk Verwaltung  * * *",0Dh,0Dh+80h
		;
		ld	b, 1		; Prg.nummer auf Bildschirm
		exx
		ld	hl, progtab	; Programmtabelle
		ld	bc, 0FFFFh-progtab	; max. Zahl zu durchsuchender Bytes
		ld	a, 1		;  a''=lfd. Programmnummer (gesamt)
		ex	af, af'		; '
menu3:		ld	a, 8Dh		; suche Prolog
		cpir
		jp	po, menu16	; Hilfe anzeigen, wenn Ende erreicht
		sub	a		; a=0
		cp	(hl)		; folgt 0?
		jr	nz, menu3	; nein
		; Eintrag gefunden
		rst	PRNST
		db 	' ',' '+80h
		ex	af, af'		; '
		call	OUTHEX		; Ausgabe A  hexadezimal (Pgm-Nummer)
		add	a, 1		; Programmnummer hochzählen
		daa			; dezimal
		ex	af, af'		; '
		rst	PRNST
		db 	' ',' '+80h
		; Anzeige Textzeile
		push	bc
		ld	b, 50		; max 50 Zeichen
menu4:		inc	hl
		ld	a, (hl)
		and	a		; 0=Textende?
		jr	z, menu6	; ja
		inc	a		; FF=Tabulator
		jr	z, menu7	; ja
		dec	a
		rst	OUTCH
menu5:		djnz	menu4
		; Ende Textzeile
menu6:		rst	PRNST
		db  	0Dh+80h
		pop	bc
		jr	menu8
		; Tabulator
menu7:		ld	a, (cupos)
		and	0F0h 		; Tabulatorpos aller 16 Stellen
		dec	a
		ld	(cupos), a
		jr	menu5

		; weiter
menu8:		exx
		inc	b		; angezeigte Prg.nummer erhöhen
		ld	a, 25		; Pgm.-Counter
		cp	b
		jr	z, menu9	; max 25 Zeilen anzeigen
		exx
		jr	menu3
menu9:		ld	b, 1		; neue Page, angezeigte Prg.nummer wieder auf 1 
		exx
		call	hilfe2		; Hilfe anzeigen (incl. zurück)
; Aktion		
menu10:		rst	INCH
		cp	0Dh
		jr	z, menu15	; weiter
		cp	'Z'
		jp	z, menu1	; Zurück zum Anfang
		cp	'M'
		jp	z, 0		; Sprung zum Monitor
		cp	'0'
		jr	c, menu10	; Sprung wenn keine Zahl
		cp	':'
		jr	nc, menu10	; Sprung wenn keine Zahl
		; Kennzahl
menu11:		rst	OUTCH
		rst	INCH
		cp	0Dh
		jr	nz, menu11
		call	INHEX		; konv. ASCII-Hex, DE=Hexzahl
		ld	a, l
		;
		ex	af, af'		; Eingabe merken '
		ld	hl, progtab	; Programmtabelle
		ld	bc, 0FFFFh-progtab	; max. Zahl zu durchsuchender Bytes
menu12:		ld	a, 8Dh		; suche Prolog
		cpir
		jp	po, menu13	; wenn Ende erreicht
		xor	a		; a=0
		cp	(hl)		; folgt 0?
		jr	nz, menu12	; nein
		ex	af, af'		; '
		sub	1		; gesuchte Pgm.nummer
		daa			; vermindern (dezimal)
		jr	z, menu14	; Anfang erreicht
		ex	af, af'		; '
		jr	menu12
		; Fehler
menu13:		rst	PRNST
		db 	0Dh,"  falsche Kennzahl !!",0Dh+80h
		ret

; gefunden -> Starten
menu14:		inc	hl
		ld	a, (hl)
		and	a
		jr	nz, menu14
		inc	hl
		jp	(hl)

; weiter anzeigen
menu15:		ld	a, b
		or	c		; bc=0 ?
		jr	z, menu10	; Ende, wenn ges. Speicher durchsucht
		rst	PRNST
		db 	0Bh,0Bh,0Bh,0Bh,6,02h+80h
		jp	menu3

;
menu16:		call	hilfe1		; Hilfe anzeigen
		jr	menu10

;------------------------------------------------------------------------------
; Argumente abfragen
; UP zur Nutzung beim Umladecode
;------------------------------------------------------------------------------

getarg0:	rst	PRNST
		db 	0Dh,0Dh,0Dh,0Dh,0Dh,0Bh,0Bh,0Bh," ARG1",':'+80h
		; arg 1
		call	getarg1
		ld	(ARG1), hl
		; arg2
		rst	PRNST
		db 	" ARG2",':'+80h
		call	getarg1
		; arg 3
		ld	(ARG2), hl
		rst	PRNST
		db 	" ARG3",':'+80h
;
getarg1:	ld	de, (cupos)
getarg2:	rst	INCH
		rst	OUTCH
		cp	0Dh
		jr	nz, getarg2
		call	INHEX		; konv. ASCII-Hex nach HL
		ld	(ARG3), hl
		ret

;------------------------------------------------------------------------------
; UP	UMLAD
; ix+0 XXXX - Programmanfang im EPROM
; ix+2 XXXX - Programmende im EPROM
; ix+4 XXXX - Programmanfang im RAM
; ix+6 XXXX - Sprungadresse Programmstart
; ix+8 XX   - Konfigurationsbyte zum Umladen
; ix+9 XX   - Konfigurationsbyte zum Starten
; ------------------------------------------------------------------------------

umlad0:		pop	ix		; Adr. hinter Call, Parameter
		rst	PRNST
		db 	0Ch+80h
		ld	a, 1
		bit	2, (ix+9)	; Konfigbyte BASIC
		jr	z, umlad1	; wenn BASIC ein
		ld	a, 5
umlad1:		out	(modul1), a
		ld	a, (ix+8)	; Konfigurationsbyte
		ld	hl, copyanf	; verschiebbare	Umladeroutine
		ld	de, copy
		ld	bc, copyend-copyanf
		ldir			; verschieben
umlad2:		jp	copy		; starten

;------------------------------------------------------------------------------
; Umkopier-Routine (ldir)
; wird in RAM kopiert, da beim Kopieren dieser EPROM nicht mehr aktiv ist
;------------------------------------------------------------------------------

copyanf:
		phase 1880h

copy:		bit	2, (ix+9)
		jr	z, copy1
		set	2, a
copy1:		ld	e, (ix+0)	; DE=Anfangsadresse im ROM
		ld	d, (ix+1)
		ld	l, (ix+2)	; HL=Programmende im ROM
		ld	h, (ix+3)
		and	a		; A=Konfigbyte
		sbc	hl, de
		ld	b, h
		ld	c, l
		inc	bc		; BC=Programmlänge
		ld	l, (ix+6)	; Startadresse
		ld	h, (ix+7)
		push	hl		; auf Stack
		ld	h, (ix+9)	; Konfigbyte Start
		push	hl		; auf Stack
		ex	de, hl		; HL=Anfangsadresse im ROM
		ld	e, (ix+4)	; DE=Programmanfang RAM
		ld	d, (ix+5)
		out	(modul1), a	; Konfigbyte ausgeben
		; jetzt ist PaketX-EPROM weggeschaltet,
		; dafür ist ROM1 bzw. ROM2 von 8000..FFFF sichtbar
		ldir			; Umladen
		pop	af		; Konfigbyte Start
		out	(modul1), a	; ausgeben
		; jetzt ist normalerweise ROM1 bzw. ROM2 wieder weggeschaltet
		; dafür ggf. BASIC-ROM zugeschaltet (bei Konfigbyte Start = 02)
		ret			; Ansprung Startadresse

		dephase

copyend

;------------------------------------------------------------------------------
; Hilfe anzeigen
;------------------------------------------------------------------------------

hilfe2:		rst	PRNST
		db 	0Dh,"  Cr = weite",'r'+80h
;
hilfe1:		rst	PRNST
		db 	0Dh,"  Z  = zur}ck",0Dh,"  M  = Monitor",0Dh,"  oder Kennzahl",' '+80h
		ld	de, (cupos)
		ret

; ------------------------------------------------------------------------------
; progtab
; ------------------------------------------------------------------------------
; #8D #00	- Prolog
; "Programmname" - ASCII-Kette
; #FF - Tabulator
; "Adressbereich" - ASCII-Kette
; #FF - Tabulator
; "Startbuchstabe" - ASCII-Code
; #00 - Endekennzeichen
; CALL UP	UMLAD
; XXXX - Programmanfang im EPROM
; XXXX - Programmende im EPROM
; XXXX - Programmanfang im RAM
; XXXX - Sprungadresse Programmstart
; XX   - Konfigurationsbyte zum Umladen
; XX   - 02: Konfigurationsbyte BASIC ein
; ------------------------------------------------------------------------------
; der freie Bereich zwischen den Programmen ist nicht nötig
; ab EDAS wurden immer 30h Bytes pro Programm genommen und der Rest mit FF aufgefüllt
; ------------------------------------------------------------------------------

progtab:

prog1:		db 8Dh,	0
		db "Basic",0FFh,0FFh,"4000-5FFF",0FFh,"b,r",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 4000h		; Umkopieren BASIC in RAM!
		dw 5FFFh
		dw 4000h
		dw 5FCCh
		db    2			; BASIC-ROM zuschalten
		db    0			; BASIC-ROM deaktivieren!
;
		db 2 dup (0FFh)

prog2:		db 8Dh,	0
		db "DVU2",0FFh,0FFh,"4000-4FF4",0FFh,'8',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 8000h
		dw 8FFFh
		dw 4000h
		dw 4000h
		db    8			; ROM1, Bank 0
		db    0
;
		db 7 dup (0FFh)

prog3:		db 8Dh,	0
		db "EDAS4",0FFh,0FFh,"4000-54FF",0FFh,"J4000",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 9000h
		dw 0A44Fh
		dw 4000h
		dw 4000h
		db    8			; ROM1, Bank 0
		db    0

;
		db 10 dup (0FFh)

prog4:		db 8Dh,	0
		db "Requelle 3",0FFh,"5600-5F20",0FFh,'e',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0A450h
		dw 0AD7Fh
		dw 5600h
		dw 5600h
		db    8			; ROM1, Bank 0
		db    0
;
		db 10 dup (0FFh)

prog5:		db 8Dh,	0
		db "Texteditor",0FFh,"5000-60FD",0FFh,'t',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0AD80h
		dw 0C580h
		dw 5000h
		dw 5000h
		db    8			; ROM1, Bank 0
		db    0
;
		db 10 dup (0FFh)

prog6:		db 8Dh,	0
		db "Turbo LOAD/COPY",0FFh,"2000-284F",0FFh,'l',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0C590h
		dw 0CDDFh
		dw 2000h
		dw 2000h
		db    8			; ROM1, Bank 0
		db    0
;
		db 5 dup (0FFh)

prog7:		db 8Dh,	0
		db "Multicopy V 2.0",0FFh,"2000-295F",0FFh,"m,w",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0CDE0h
		dw 0D740h
		dw 2000h
		dw 2005h
		db    8			; ROM1, Bank 0
		db    0
;
		db 3 dup (0FFh)

prog8:		db 8Dh,	0
		db "FDC-Johann",0FFh,"E800-EFFF",0FFh,'d',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0D750h
		dw 0DF50h
		dw 0E800h
		dw 0E800h
		db    8			; ROM1, Bank 0
		db    0
;
		db 10 dup (0FFh)

prog9:		db 8Dh,	0
		db "Disk-HD-V3.0",0FFh,"4000-575F",0FFh,'8',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0DF60h
		dw 0F6BFh
		dw 4000h
		dw 4000h
		db    8			; ROM1, Bank 0
		db    0
;
		db 8 dup (0FFh)

prog10:		db 8Dh,	0
		db "Disk1-FHR",0FFh,0FFh,"3000-3CFF",0FFh,'d',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 8000h
		dw 8C0Fh
		dw 3000h
		dw 3000h
		db  18h			; ROM1, Bank 1
		db    0
;
		db 10 dup (0FFh)

prog11:		db 8Dh,	0
		db "Disk-USB",0FFh,0FFh,"E000-EFFF",0FFh,'9',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 8C10h
		dw 982Fh
		dw 0E000h
		dw 0E000h
		db  18h			; ROM1, Bank 1
		db    0
;
		db 11 dup (0FFh)

prog12:		db  8Dh, 0
		db "HRD-DOS-11",0FFh,"D000-FCFF",0FFh,"JE600",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 9830h
		dw 0C740h
		dw 0D000h
		dw 0E600h
		db  18h			; ROM1, Bank 1
		db    0
;
		db 6 dup (0FFh)

prog13:		db 8Dh,	0
		db "HRCPM11",0FFh,0FFh,"D000-FCFF",0FFh,"JE600",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0C750h
		dw 0F550h
		dw 0D000h
		dw 0E600h
		db  18h			; ROM1, Bank 1
		db    0
;
		db 8 dup (0FFh)

prog14:		db  8Dh, 0
		db "AC1/Save Load",0FFh,"1900-1C11",0FFh,'Q',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0F560h
		dw 0F87Fh
		dw 1900h
		dw 1900h
		db  18h			; ROM1, Bank 1
		db    0
;
		db 7 dup (0FFh)

prog15:		db  8Dh, 0
		db "GEDIT AC1  ",0FFh,"1900-1E5F",0FFh,"f,g",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0F880h
		dw 0FDDFh
		dw 1900h
		dw 1900h
		db  18h			; ROM1, Bank 1
		db    0
;
		db 7 dup (0FFh)

prog16:		db 8Dh,	0
		db "CLIST-Turbo",0FFh,"1900-1C70",0FFh,"1909",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 8000h
		dw 837Fh
		dw 1900h
		dw 1909h
		db  28h			; ROM1, Bank 2
		db    0
;
		db 6 dup (0FFh)

prog17:		db  8Dh, 0
		db "ASSEMBLER",0FFh,0FFh,"4000-5447",0FFh,'a',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 8380h
		dw 97CFh
		dw 4000h
		dw 4000h
		db  28h			; ROM1, Bank 2
		db    0
;
		db 10 dup (0FFh)

prog18:		db 8Dh,	0
		db "COMPILER",0FFh,0FFh,"5600-6000",0FFh,'8',0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 97D0h
		dw 0A1DFh
		dw 5600h
		dw 5600h
		db  28h			; ROM1, Bank 2
		db    0
;
		db 11 dup (0FFh)

prog19:		db 8Dh,	0
		db "DEBUGGER",0FFh,0FFh,"4000-5861",0FFh,"J4000",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0A1E0h
		dw 0BA4Fh
		dw 4000h
		dw 4000h
		db  28h			; ROM1, Bank 2
		db    0
;
		db 7 dup (0FFh)

prog20:		db 8Dh,	0
		db "DIASSEMBLER3.0",0FFh,"5800-5FFF",0FFh,"J5800",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0BA50h
		dw 0C251h
		dw 5800h
		dw 5800h
		db  28h			; ROM1, Bank 2
		db    0
;
		db 2 dup (0FFh)

prog21:		db 8Dh,	0
		db "HRCPM11 PASCAL",0FFh,"D000-FFFF",0FFh,"JE600",0
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 0C260h
		dw 0F160h
		dw 0D000h
		dw 0E600h
		db  28h			; ROM1, Bank 2
		db    0
;
		db 2 dup (0FFh)

; BASIC-Programm, mit Extra Basic-Init
prog22:		db 8Dh,	0
		db "BASIC:Trafoberechnung",0FFh,"6000-71DF",0FFh,0
		call	loc_FFE7	; Basic-Erweiterung
		call	UMLAD		; Umladeroutine	fuer Programm
		dw 8000h
		dw 9DDFh
		dw 6000h
		dw 5FCEh		; Basic-Warmstart ?
		db    9			; ROM2, Bank 0
		db    2			; BASIC-ROM aktivieren !
;

;------------------------------------------------------------------------------
; Autostart f. BASIC
;------------------------------------------------------------------------------

		org 0FFE7h

loc_FFE7:	ld	a, 3		; BASIC+ProgramX-ROM aktiv setzen
		out	(modul1), a
		call	5FD9h		; 
		ld	hl, 5FE0h
		ld	de, 1F80h
		ld	bc, 20h
		ldir
		ld	hl, 5CFDh
		ld	(jp_nmi+1), hl	; nmi-Routine
; zusätzliche Fkt: Adr FFFF = kein Autostart
loc_FFFF:	ret		

		end
