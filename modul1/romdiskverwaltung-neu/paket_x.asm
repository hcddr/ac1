;------------------------------------------------------------------------------
; AC1-2010-Modul-1 (1M-Modul mit max. 2 ROMBANK-ROMs)
; (c) V. Pohlers 2023
;------------------------------------------------------------------------------
; Modulverwaltungssoftware
;------------------------------------------------------------------------------
; 06.02.2023 erste lauffähige Version, noch ohne BASIC-Austostart und ohne Packer
; 13.02.2023 mit ZX7-Unpacker, SCH-Autostart
; 21.02.2023 neu offset, u.a. für minibasic-Programme
; 25.01.2024 Branch Version Spezialmodul f. Dietmar
; 26.03.2024 kleines Problem: wenn Programme in der letzten Bank liegen, wird "weitergeblättert",
; 	d.h. Programme wurden wiederholt angezeigt -> bank =ff als Endekennung genutzt.
; 27.03.2024 Fehler bei Programmen behoben, bei denen der Header am Ende der Bank liegt
;	hier erfolgte die Bankumschaltung zu spät


		include	ac1-2010.asm
		include	packedroms.inc	; wg Nr. minibasic_4000

		CPU 	z80undoc

VERSIONSDATUM	equ	DATE

bnkanf		EQU	8000h
bnkende		equ	0FFFFh
firstbnk	equ	08h
lastbnk		equ	0F9h	; Bänke 0..31, erst ROM1 dann ROM2, muss <> FF sein, sonst menu15+16 ändern!
nxtbnkprc	equ	2	; Bankumschaltungsroutine
				; 1- laufende Banknummern liegen im ROM hintereinander (orig Modul1, jkcemu)
				; 2- Banknummern erst ROM1 x8 dann ROM2 x9 (1-MByte-Modul-1 AC1-2010, 2 ROMs)
				; 3- Variante Banknummern mit Liste

;tstsch		equ	0A7Bh	; in V8, 1088, Test Autostart

; VERSION modul_1_dietmar, 1_dragon@gmx.de
; Die Bänke werden immer im Bereich von C000H bis FFFFH eingeblendet.
; IM ROM ist BASIC + PAKETX mit drin
; bnkanf		EQU	0C000h
; bnkende		equ	0FFFFh
; firstbnk	equ	08h
; lastbnk		equ	0A9h	; Bänke 0..31, erst ROM1 dann ROM2
; nxtbnkprc	equ	3	; Bankumschaltungsroutine Nr 3 (1..3)


;nr_minibasic 	equ 0FH		; aus packedroms.inc


; Portwerte modul1-Port
NOROM		equ	0
XROM		equ	1
BASICROM	equ	2


; port modul1:		
; 	00	Modul1 kein EPROM aktiv, alles RAM
; 	01	Programm X, 	E000-EFFF
; 	02	BASIC,		2000-5FFF
; 	04	CPM,		alle RAM (incl 0..1FFF)
; 	08h EPROM1 Bank0, 09h EPROM2 Bank0
; 	18h EPROM1 Bank1, 19h EPROM2 Bank1
; 	... ...
; 	F8h EPROM1 Bank15, F9h EPROM2 Bank15, jeweils 8000-FFFF

; die Progamme sind im ROM hintereinander weg abgelegt (ROM1 und 2; freie Lücken sind natürlich möglich)
; konkrete Banknummern und absolute Adressen im ROM bzw. releativ in der Bank interessieren nicht. Diese
; Verwaltungssoftware durchsucht den ROM-Bereich und entnimmt alle notwendigen Daten aus dem Header
; 	align 40h
; 	Header
;	binary
; Header (40h lang), auf 
;	db	0D3h,0D3h,0D3h	; +00h 0..2 Kopfkennzeichen 3x D3
;	db	typ		; +03h +3 Typkennzeichen
;	db	compressed	; +04h +4 "Z" komprimiert, 00 nicht komprimiert
;	ds 	3		; +5..7 System "AC1", "LC2", Z13"
;	dw	offs		; +08h +8 Lade-Offset normalerweise 0, wird beim Laden zu aadr addiert und 
				; später Programm verschoben. Wichtig bei aadr < 1900h
;	dw	aadr		; +0ah +10 Anfangsadresse 
;	dw	eadr		; +0ch +12 Endadresse
;	dw	sadr		; +0eh +14 Startadresse
;	db	name		; +10h +16 name. max 32 Zeichen , mit leerzeichen aufgefüllt
;	db 	comment		; +30h +46 kommentar, max 16 Zeichen , mit leerzeichen aufgefüllt


;------------------------------------------------------------------------------
; RAM-Bereich 1880-18FF
;------------------------------------------------------------------------------

		org	1880h

bank		ds	1		; Port-Wert
buffer		ds	40h
;bankpos	ds	2		; aktuelle Adr. in Bank
ramcode		equ $
ramcode2	equ buffer+16		; größerer Bereich, überschreibt Programmname+Kommentar

ramcodend	equ	18FFh			

;------------------------------------------------------------------------------

		org 	0E000h
		
		jp	menu
		jp	such		; suche Programm HL=nr out ix=Pointer in ROM
		jp	show		; Anzeige Header in buffer
		jp	umlad		; umladen. ix = Pointer in ROM, header in buffer
		jp	start		; starten. header in buffer
		
;------------------------------------------------------------------------------
; Anzeige ROM-Disk-Inhalt, Starten von Programmen
;------------------------------------------------------------------------------
menu:		ld	sp, SYSSK	; System-Stack
		ld	hl, GETCO1	; Sprung zur Monitoreingabeschleife
		push	hl		; als Return-Adr auf Stack
		;

; folgt Kommandoparameter (dezimal)?
; dann diese Programmnummer suchen
menu0:		ld	hl,(ARG1)
		ld	a,h
		or	l
		jr	z,menu1		; kein Parameter
		;sonst laden Programm Nr HL
		call	DZKON		; Dez->Hex
		call	such
		jp	c, menu13	; "falsche Kennzahl"
		; sonst Prgm laden + starten..
		jp	menu12

;sonst Menü anzeigen		
menu1:		rst	PRNST
		db 	0Ch,0Dh,"     * * *  ROM - Disk Verwaltung VP 03/2024 * * *",0Dh,0Dh+80h
		;
		
		ld	b, 1		; b'' = Prg.nummer auf Bildschirm
		ld	hl, 1		; hl''=lfd. Programmnummer (gesamt)
		exx

		; init
		ld	a,firstbnk	; bank 0
		ld	(bank),a
		ld	ix,bnkanf-40h	; romanfang
		;
menu2		call	fndhd
		jp	c, menu16	; es kommt nix mehr
		
		exx
		CALL	NRKON		; HL dezimal ausgeben
		exx
		rst	PRNST
		db 	' ',' '+80h
		
		call	show

		; weiter
menu8:		exx
		inc	hl		; Prg.nummer erhöhen
		inc	b		; angezeigte Prg.nummer erhöhen
		ld	a, 25		; Pgm.-Counter
		cp	b
		jr	z, menu9	; max 25 Zeilen anzeigen
		exx
		jr	menu2
;		
menu9:		ld	b, 1		; neue Page, angezeigte Prg.nummer wieder auf 1 
		exx
		call	hilfe2		; Hilfe anzeigen (incl. zurück)

; Aktion		
menu10:		rst	INCH
		cp	0Dh
		jp	z, menu15	; weiter
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
					; de zeigt auf letzte ausgabeposition (hilfe2)
		call	INHEX		; konv. ASCII-Hex, DE=Hexzahl
		call	DZKON		; Dez->Hex
		; suche Nr
		call	such
		jr	c, menu13	; nicht gefunden
		rst	PRNST
		db 	0Dh+80h
;-----------
; Programm laden
menu12:		if 1=1
		ld	a,(bank)	;Bank+Pos merken
		push	af
		push	ix
		call	getsec		; Header in Buffer laden
		ld	a,(buffer+3)	; typ
		cp	'b'		; minibasic?
		jr	nz, menu12a	; nein
; Minibasic
		ld	hl,minibasic	; sonst erst minibasic laden
		call	such
		call	show
		call	umlad
		endif
;
menu12a:	pop	ix		;Bank+Pos restaurieren
		pop	af
		ld	(bank),a
		call	show
		call	umlad
		
;-----------
; Programm starten
		; aadr > ramcodend?
		ld	hl,(buffer+8)	; offs <> 0?
		ld	a,h
		or	l
		jp	z,start		; sonst Starten

; sonst nur umladen		
move:		
		ld	hl,(buffer+12)	; eadr
		ld	de,(buffer+10)	; de=aadr
		or	a
		sbc	hl, de
		inc	hl
		push	hl		; länge
		
		ld	hl,(buffer+8)	; offs
		add	hl,de		; hl = aadr+ofs
		pop	bc		; bc = length
		ldir
		ret

;-----------
; sonst Fehler
menu13:		rst	PRNST
		db 	0Dh,"  falsche Kennzahl !!",0Dh+80h
		ret

; weiter anzeigen
menu15:		ld	a,(bank)	; Ende erreicht (Bank FF) ?
		inc	a		; 
		jp	z,menu10	; ja, dann kein Blättern mehr
		; sonst
		rst	PRNST
		db 	0Bh,0Bh,0Bh,0Bh,6,02h+80h	; 4 Zeilen hoch und bws löschen
		jp	menu2

; Ende erreicht
menu16:		ld	a,0ffh
		ld	(bank),a	; Ende merken als Bank FF
		;
		rst	PRNST
		db 	CR+80h		; statt "weiter"
		call	hilfe1		; Hilfe anzeigen
		jp	menu10

	
;------------------------------------------------------------------------------
; Hilfe anzeigen
;------------------------------------------------------------------------------

hilfe2:		rst	PRNST
		db 	0Dh,"  Cr = weite",'r'+80h
;
hilfe1:		rst	PRNST
		db 	0Dh,"  Z  = zur}ck",0Dh,"  M  = Monitor",0Dh,"  oder Kennzahl",' '+80h
		ld	de, (cupos)	; Position merken (f. INHEX)
		ret

;------------------------------------------------------------------------------
;Konvertieren Filenummer
;HL->(DE) dez.
;------------------------------------------------------------------------------

nrkon		push	bc
		push	hl
		cp	2
		jr	z,nrkon2
;		ld	bc,-10000
;		call	Num1
;		ld	bc,-1000
;		call	Num1
nrkon3		ld	bc,-100
		call	Num1
nrkon2		ld	bc,-10
		call	Num1
		ld	c,-1
		call	Num1
		pop	hl
		pop	bc
		ret
Num1		ld	a,'0'-1
Num2		inc	a
		add	hl,bc
		jr	c,Num2
		sbc	hl,bc
		;ld	(de),a
		;inc	de
		rst 	outch
		ret

;------------------------------------------------------------------------------
;Konvertierung hl bcd --> hl hex
;------------------------------------------------------------------------------

DZKON:		push	bc
		push	de
		ld	a, h
		ld	e, l
		ld	hl,0
		call	DZKONA		; Ausgabe A hexadezimal
		ld	a, e
		call	DZKONA		; Ausgabe A hexadezimal
		pop	de
		pop	bc
		ret
;
DZKONA:		push	af
		rra
		rra
		rra
		rra
		call	DZKON1		; obere Tetrade ausgeben
		pop	af		; und die unter
DZKON1:		push	af
		and	0Fh
		LD	B,H
		LD	C,L
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,BC
		ADD	HL,HL
		LD	B,0
		LD	C,A
		ADD	HL,BC		;HL=HL*10+Zahl
		pop	af
		ret

		if nxtbnkprc = 1 ; Bankumschaltungsroutine Nr 1

;------------------------------------------------------------------------------
; Variante laufende Banknummern liegen im ROM hintereinander (orig Modul1, jkcemu)
; nächste Bank 08,09,18,19,...F8,F9
; out: a=zu setzende banknr
;------------------------------------------------------------------------------

nxtbnk:		ld	a,(bank)
		add	a,1		; 08->09; 09->0A
		bit	0,a	
		jr	nz,nxtbnk1	; wenn aus gerader Bank ungerade wurde
		add	a,10h-2		; bei ungerade-> gerade fehlen noch 10-2
					; 09->0A->18
nxtbnk1: 	ld	(bank),a
		;out	(modul1),a	; erfolgte jetzt immer erst im RAM-Code 
		;;call	OUTHEX		; testweise
		ret

;;bnktab		db	08h,09h,18h,19h,28h,29h,38h,39h
;;		db	48h,49h,58h,59h,68h,69h,78h,79h
;;		db	88h,89h,98h,99h,0A8h,0A9h,0B8h,0B9h
;;		db	0C8h,0C9h,0D8h,0D9h,0E8h,0E9h,0F8h,0F9h

		elseif nxtbnkprc = 2 ; Bankumschaltungsroutine Nr 2   
		
;------------------------------------------------------------------------------
; Variante Banknummern erst ROM1 x8 dann ROM2 x9 (1-MByte-Modul-1 AC1-2010, 2 ROMs)
; nächste Bank 08...F8, dann 09..F9
; out: a=zu setzende banknr
;------------------------------------------------------------------------------

nxtbnk:		ld	a,(bank)
		add	a,10h
		jr	nc,nxtbnk1	; solange <= F8
					; eigentlich auch Test auf < F9 sinnvoll
					; aber nicht nötig
		ld	a,09h
nxtbnk1: 	ld	(bank),a
		;out	(modul1),a	; erfolgte jetzt immer erst im RAM-Code
		;;call	OUTHEX		; testweise
		ret

;;bnktab		db	08h,18h,28h,38h,48h,58h,68h,78h
;;		db	88h,98h,0A8h,0B8h,0C8h,0D8h,0E8h,0F8h
;;		db	09h,19h,29h,39h,49h,59h,69h,79h
;;		db	89h,99h,0A9h,0B9h,0C9h,0D9h,0E9h,0F9h

		elseif nxtbnkprc = 3 ; Bankumschaltungsroutine Nr 2   

;------------------------------------------------------------------------------
; Variante Banknummern mit Liste
;------------------------------------------------------------------------------

nxtbnk:		ld	a,(bank)
		
		ld	hl, bnktab
		ld	bc, 8
		cpir			; suchen
		ld	a, 0
		jr	nz, nxtbnk1	; nicht gefunden -> 0 (??)
		ld	a, (hl)		; sonst nachfolgendes Zeichen aus Liste	
nxtbnk1: 	ld	(bank),a
		;out	(modul1),a	; erfolgte jetzt immer erst im RAM-Code
		;;call	OUTHEX		; testweise
		ret

bnktab		db	08h,09h,28h,29h,88h,89h,0a8h,0a9h



		endif

;------------------------------------------------------------------------------
; suche nächstes file
; in ix=aktuelle Position
; out ix=nächster Header, cy=1 kein weiterer Header
;------------------------------------------------------------------------------

; umladen in RAM, da dieser Code-Bereich weggeschaltet wird
fndhd		push	bc
		ld	hl,rfndhd
		ld	de,ramcode
		ld	bc,rfndhdend-rfndhd
		ldir
		pop	bc
		call	ramcode
		ret

rfndhd
;------------		
		phase	ramcode

		ld	a,(bank)
		out	(modul1),a


		ld	de, 40h		; Headergröße (und = alignment)
		jr	fndhd1
		;
fndhd0:		ld	a,0d3h
		cp	a,(ix+0)
		jr	nz,fndhd1
		cp	a,(ix+1)
		jr	nz,fndhd1
		cp	a,(ix+2)
		jr	z, fndhd2	; Z = 1 was gefunden		
		; nächsten Sektor
fndhd1:		add	ix,de
		jr	nc,fndhd0	; kein Überlauf (FFFF nicht überschritten)
					; dann weitersuchen
		; nächste Bank					
		ld	a,(bank)
		cp	a,lastbnk
		scf
		jr	z, fndhd2	; z=1 letzte Bank erreicht
		;		
		ld	a,XROM
		out	(modul1),a
		call	nxtbnk		; Bank umrechnen
		;;ld	a,(bank)
		out	(modul1),a

		ld	ix,bnkanf
		jr	fndhd0
		
		; modul wieder aktivieren
fndhd2:		ld	a,XROM
		out	(modul1),a
		ret

		if $ > ramcodend
		warning "Prozedur passt nicht mehr in RAM-Bereich"
		endif

		dephase
;------------
rfndhdend


;------------------------------------------------------------------------------
; getsec
; einen Sektor (40h) aus RAMdisk in buffer umladen 
;------------------------------------------------------------------------------

; umladen in RAM, da dieser Code-Bereich weggeschaltet wird
getsec		ld	hl,rgetsec
		ld	de,ramcode
		ld	bc,rgetsecend-rgetsec
		ldir
		call	ramcode
		ret

rgetsec
;------------
		phase	ramcode

		push	ix
		pop	hl
		ld	de, buffer
		ld	bc, 40h 
		
		ld	a,(bank)
		out	(modul1),a
				
		ldir
		
		; modul wieder aktivieren
		ld	a,XROM
		out	(modul1),a
		ret

		if $ > ramcodend
		warning "Prozedur passt nicht mehr in RAM-Bereich"
		endif

		dephase
;------------		
rgetsecend



;------------------------------------------------------------------------------
; Anzeige Header
; Header steht danach in buffer
;------------------------------------------------------------------------------

show		push	bc
		push	hl

		call	getsec

		if 1=0
		; bank
		ld	a,(bank)
		call	outhex
		rst	prnst
		db	spc+80h
		; lfd. banknr
		ld	a,b
		call	outhex
		rst	prnst
		db	spc+80h
		; pos in bank
		call	outhl
		rst	prnst
		db	spc+80h
		endif
		
		; typ
		ld	a,(buffer+3)
		rst 	outch
		rst	prnst
		db	spc+80h
		
		; Name
		ld	hl, buffer+10h		; offs. name
;		ld	b,32
		ld	b,23		; 32 ist zuviel, wenn Nr. und Adr. und Kommentar angezeigt werden
;		ld	b,16		; testweise, wg. Ausgabe banknr etc.
		call	outtxt
		
		; aadr
		rst	prnst
		db	spc+80h
		ld	hl,(buffer+10)
		call	outhl
		; eadr
		rst	prnst
		db	'-'+80h
		ld	hl,(buffer+12)
		call	outhl
		; sadr
		rst	prnst
		db	','+80h
		ld	hl,(buffer+14)
		call	outhl
			
;	dw	aadr		; +0ah +10 Anfangsadresse 
;	dw	eadr		; +0ch +12 Endadresse
;	dw	sadr		; +0eh +14 Startadresse
;	ds	name		; +10h +16 name. max 32 Zeichen , mit leerzeichen aufgefüllt
;	ds 	Commen		; +30h +46 kommentar, max 16 Zeichen , mit leerzeichen aufgefüllt

		; Kommentar
		rst	prnst
		db	" ",spc+80h

		ld	hl, buffer+30h
		ld	b,16
		call	outtxt
		;
		rst	prnst
		db	cr+80h
		;
		pop	hl
		pop	bc
		ret	

; ab hl, b zeichen ausgeben
outtxt:		ld	a,(hl)
		rst	outch
		inc	hl
		djnz	outtxt
		ret	

;------------------------------------------------------------------------------
; suche Programm nr HL
;------------------------------------------------------------------------------

such:		; init
		push	hl
		ld	a,firstbnk	; bank 0
		ld	(bank),a
		ld	ix,bnkanf-40h	; Bankanfang
		;
such1		push	hl
		call	fndhd
		pop	hl
		ret	c		; es kommt nix mehr
		dec	hl
		ld	a,h
		or	l
		jr	nz,such1
		pop	hl
		ret			; cy=0


;------------------------------------------------------------------------------
; umladen
; in:	ix = Pointer in ROM auf Header
;	header in buffer
;------------------------------------------------------------------------------

;	db	typ		; +03h +3 Typkennzeichen
;	db	compressed	; +04h +4 Z komprimiert, 0 nicht komprimiert
;	dw	aadr		; +0ah +10 Anfangsadresse 
;	dw	eadr		; +0ch +12 Endadresse

; umladen in RAM, da dieser Code-Bereich weggeschaltet wird
umlad		ld	a,(buffer+4)	
		cp	'Z'
		jp	z,decompress
		;

;------------------------------------------------------------------------------
; Umladen mit ldir
;------------------------------------------------------------------------------

		ld	hl,rumlad
		ld	de,ramcode2
		ld	bc,rumladend-rumlad
		ldir
		call	ramcode2
		ret

rumlad
;------------
		phase	ramcode2

		ld	a,(bank)
		out	(modul1),a

		ld	hl,(buffer+12)	; hl=eadr
		ld	de,(buffer+10)	; de=aadr
		
		or	a
		sbc	hl,de		;hl=länge
		ld	b,h		;bc=länge
		ld	c,l
		inc	bc		;+1
	
		ld	hl,(buffer+8)	; offs
		add	hl,de
		ex	de,hl		; de=aadr+offs
		;
		ld	a,ixl
		add	a, 40h
		ld	l,a
		ld	a,ixh
		adc	a,0
		ld	h,a		;hl=adr. im ROM(IX+40h)
		;
umlad1		
		push	af		;flag sichern					
		dec	h
		inc	h		;FFFF überschritten?
					;dann nächste Bank
		call	z,umlad2
		pop	af
		ldi			;von (hl) nach (de)
					;schreiben in unterliegenden RAM
		jp	pe, umlad1	;weiter bis bc=0

		; modul wieder aktivieren
		ld	a,XROM
		out	(modul1),a
		ret
;
umlad2:		ld	a,XROM
		out	(modul1),a
		call	nxtbnk		; Bank umrechnen
		;;ld	a,(bank)
		out	(modul1),a
		ld	hl,bnkanf
		ret

		if $ > ramcodend
		warning "Prozedur passt nicht mehr in RAM-Bereich"
		endif

		dephase
;------------		
rumladend
; 

;------------------------------------------------------------------------------
; Umladen mit unpacker
;------------------------------------------------------------------------------

decompress:	ld	hl,dzx7_standard
		ld	de,ramcode2
		ld	bc,dzx7_standardend-dzx7_standard
		ldir

		ld	bc,(buffer+8)	; offs
		ld	hl,(buffer+10)	; de=aadr
		add	hl,bc
		push	hl
		pop	de		; de=aadr

		ld	a,ixl
		add	a, 40h
		ld	l,a
		ld	a,ixh
		adc	a,0
		ld	h,a		;hl=adr. im ROM(IX+40h)

;		call	dzx7_standard
		ld	a,(bank)
		call	ramcode2

		; aktuelle eadr in buffer übernehmen
		ld	(buffer+12), hl	; eadr+offs
		ret

dzx7_standard:
;------------
		phase	ramcode2

; -----------------------------------------------------------------------------
; ZX7 decoder by Einar Saukas, Antonio Villena & Metalbrain
; "Standard" version
; -----------------------------------------------------------------------------
; Parameters:
;   HL: source address (compressed data)
;   DE: destination address (decompressing)
; -----------------------------------------------------------------------------

;;dzx7_standard:
	out	(modul1),a
;
        ld      a, 080h
dzx7s_copy_byte_loop:
       	call	umlad4
        ldi                             ; copy literal byte
        				; schreiben in unterliegenden RAM
dzx7s_main_loop:
        call    dzx7s_next_bit
        jr      nc, dzx7s_copy_byte_loop ; next bit indicates either literal or sequence

; determine number of bits used for length (Elias gamma coding)
        push    de
        ld      bc, 0
        ld      d, b
dzx7s_len_size_loop:
        inc     d
        call    dzx7s_next_bit
        jr      nc, dzx7s_len_size_loop

; determine length
dzx7s_len_value_loop:
        call    nc, dzx7s_next_bit
        rl      c
        rl      b
        jr      c, dzx7s_exit           ; check end marker
        dec     d
        jr      nz, dzx7s_len_value_loop
        inc     bc                      ; adjust length

; determine offset
        ld      e, (hl)                 ; load offset flag (1 bit) + offset value (7 bits)
        inc     hl
        defb    0cbh, 033h                ; opcode for undocumented instruction "SLL E" aka "SLS E"
        jr      nc, dzx7s_offset_end    ; if offset flag is set, load 4 extra bits
        ld      d, 010h                  ; bit marker to load 4 bits
dzx7s_rld_next_bit:
        call    dzx7s_next_bit
        rl      d                       ; insert next bit into D
        jr      nc, dzx7s_rld_next_bit  ; repeat 4 times, until bit marker is out
        inc     d                       ; add 128 to DE
        srl	d			; retrieve fourth bit from D
dzx7s_offset_end:
        rr      e                       ; insert fourth bit into E

; copy previous sequence
        ex      (sp), hl                ; store source, restore destination
        push    hl                      ; store destination
        sbc     hl, de                  ; HL = destination - offset - 1
        pop     de                      ; DE = destination
	;
	push	af
	;ld	a,NOROM
	xor	a			; 0==NOROM
	out	(modul1),a
        ldir
	ld	a,(bank)
	out	(modul1),a
	pop	af
dzx7s_exit:
        pop     hl                      ; restore source address (compressed data)
        jr      nc, dzx7s_main_loop

		; modul wieder aktivieren
		ld	a,XROM
		out	(modul1),a
		ret
        
dzx7s_next_bit:
        add     a, a                    ; check next bit
        ret     nz                      ; no more bits left?
        ld      a, (hl)                 ; load another group of 8 bits
        inc     hl
	;
	call	umlad4
	;
        rla
        ret

; ggf. Bank weiterschalten
umlad4
	dec	h
	inc	h		;FFFF überschritten?
	ret	nz		; cy ist unverändert!
	; ja -> nächste Bank
	push	af
	ld	a,XROM
	out	(modul1),a
	call	nxtbnk		; Bank umrechnen
	;;ld	a,(bank)
	out	(modul1),a
	ld	hl,bnkanf
	pop	af
	ret

		if $ > ramcodend
		warning "Prozedur passt nicht mehr in RAM-Bereich"
		endif

		dephase
;------------		
dzx7_standardend
; 

; -----------------------------------------------------------------------------



;------------------------------------------------------------------------------
; Basic-Warmstart f. GSBASIC 3.2
;------------------------------------------------------------------------------


unk_1F80        equ	1F80h
;
INITAB          equ	40D2h
PRNTOK          equ	43bbh
CLREG           equ	4658h
CLRPTR          equ	4633h
RUNFST		equ	463Eh
RUNCNT          equ	47FDh
loc_5CFD	equ	5CFDh
unk_5FE0        equ	5FE0h
;
STRSPC          equ	6056h
BASSTACK:	equ	6062h		; Initial stack
unk_60A6        equ	60A6h		; ???
LSTRAM          equ	60abh		; Last available RAM
PROGND:		equ	60D2H		; End of program
PROGST          equ	60f6h		; Start of program text area

basini:		; Basic kaltstart
		in	a, (5)
		set	3, a		; setze Bildschim-Mode/Zeichensatz
		out	(5), a
;		jp	cold

COLD:		ld	hl, loc_5CFD	; NMI-Routine
		ld	(jp_nmi+1), hl	; nmi-sprungadr
		ld	hl, unk_5FE0	; Funktionstasten
		ld	de, unk_1F80	; vorbelegen
		ld	bc, 20h
		ldir

		; Systemzellen
		ld	hl,basz
		ld	de,6000h
		ld	bc,basze-basz
		ldir

		ret

basz
		binclude	basicarb_6000_60f6.rom
basze

;------------------------------------------------------------------------------
; starten
; in:	header in buffer
;------------------------------------------------------------------------------

;	db	typ		; +03h +3 Typkennzeichen
;	dw	sadr		; +0eh +14 Startadresse

; Autostart wenn "SCHnnnn" am Anfang steht

; starten in RAM, da dieser Code-Bereich weggeschaltet wird
start		ld	hl,rstart
		ld	de,ramcode2
		ld	bc,rstartend-rstart
		ldir

		ld	sp, SYSSK	; System-Stack
		ld	hl, GETCO1	; Sprung zur Monitoreingabeschleife
		push	hl		; als Return-Adr auf Stack

		jp	ramcode2

rstart
;------------
		phase	ramcode2
		
		ld	a,NOROM
		out	(modul1),a	; modul aus

		ld	a,(buffer+3)	; typ
		cp	'B'
		jr	z,start1	; basic-programm
;minibasic-Start funktioniert so nicht, da aadr < 1900
;		cp	'b'
;		jr	z,start3	; minibasic-programm
		cp	'P'
		ret	nz		; sonstiges Programm -> zurück zum Monitor

		; P-Programm
		; Startadr ?
		ld	hl,(buffer+14)	; sadr
		ld	a,h		; = 0000?
		or	l
		jr	z,start2	; dann zurück zum Monitor
		jp	(hl)		; sonst starten

		; SCH-Autostart ?
start2		ld	de,(buffer+10)	; aadr
		call	tstsch		; Autostart?
		ret	nz		; auch kein Autostart
		jp	(hl)		; sonst starten

		; B-Basic
start1		ld	a,XROM+BASICROM
		out	(modul1),a	; modul BASIC an
		call	basini		; Basic init.
		ld	a,BASICROM
		out	(modul1),a	; modul BASIC an
		ld	hl,(buffer+12)	; eadr
		ld	(PROGND), hl
		;LD	SP, BASSTACK	; Temporary stack
		LD      BC,RUNCNT
		PUSH    BC
		JP      RUNFST
		;ret

;start3		jp	4003h		; minibasic warmstart

		if $ > ramcodend
		warning "Prozedur passt nicht mehr in RAM-Bereich"
		endif

		dephase
;------------		
rstartend

;------------------------------------------------------------------------------
; Test Autostart
;------------------------------------------------------------------------------

aSch:		db 'SCH'

tstsch:		ld	hl, aSch	; "SCH"
		ld	b, 3
tstsch1:	ld	a, (de)
		cp	(hl)
		ret	nz		; nicht gefunden
		inc	hl
		inc	de
		djnz	tstsch1
		; Adresse hinter "SCH" in HL
		ld	a, (de)
		ld	l, a
		inc	de
		ld	a, (de)
		ld	h, a		; HL=Autostartadr.
		ret

		db	"V.Pohlers ",VERSIONSDATUM

		end


normal
		;offs beachten
		ld	hl,(buffer+8)	; offs
		ld	a,h
		or	l
		ret	z		; wenn offs=0
		

		ld	hl,(buffer+12)	; hl=eadr
		ld	de,(buffer+10)	; de=aadr
		or	a
		sbc	hl,de		;hl=länge
		ld	b,h		;bc=länge
		ld	c,l
		inc	bc		;+1		

		ld	hl,(buffer+8)	; offs
		add	hl,de		; hl=aadr+offs
		ldir			; von (hl) nach (de)

uncompress
		;offs beachten
		ld	bc,(buffer+8)	; offs
		ld	a,b
		or	c
		ret	z		; wenn offs=0

		ld	hl,(buffer+10)	; hl=aadr
		push	hl
		add	hl,bc		; hl=aadr+offs
		push	hl
		ld	de,(buffer+12)	; de=eadr+offs
		ex	de,hl
		or	a
		sbc	hl,de		;hl=länge
		ld	b,h		;bc=länge
		ld	c,l
		inc	bc		;+1		
		pop	hl
		pop	de
		ldir			; von (hl) nach (de)

