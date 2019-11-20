		page	0
		CPU	z80

;AC1-MONITOR 3.1, FA 84
;Volker Pohlers, Neustadt i.H., 07.08.2018
;letzte Änderung 24.08.2018

; Makros
hi              function x, (x>>8) & 0ffh	; High-Byte
lo              function x, x & 0ffh		; Low-Byte

; Speicheraufteilung
ROM		equ	0000h
BASIC		equ	0800h
BWS		equ	1000h
RAM		equ	1800h
; BWS
COLS		equ	64
LINES		equ	16

; I/O
;CTC0		equ	0
PIODA		equ	4
PIODB		equ	5
PIOCA		equ	6
PIOCB		equ	7

;
NAMELEN		equ	6		; max. Länge des Dateinamens bei LOAD/SAVE
RAMEND		equ	RAM+800h	; Ende f. Kdo-Suche, auf RAM beschränkt

; Zeitkonstanten (bei 2 MHz-Takt)
ZK1    EQU  49        ;Laenge 1.Halbwelle
ZK2    EQU  46        ;Laenge 2.Halbwelle
;ZKR    EQU  63	      ;ca 2/3 * (ZK1+ZK2)
ZKR    EQU  (ZK1+ZK2)*2/3

; f. mon_v31_32.bin
;LINES		equ	32		; bei 2K BWS
;NAMELEN	equ	16		; max. Länge des Dateinamens bei LOAD/SAVE
;RAMEND		equ	0FFFDh		; Ende f. Kdo-Suche, auf RAM beschränkt
;fkopatch	equ	1		; Korrektur in Find-Kommando

;------------------------------------------------------------------------------
; Reset
;------------------------------------------------------------------------------

		org	ROM

init:		ld	(REGSP), sp	; Stackpointer sichern
		jr	init1
		db 0FFh
		db 0FFh
;RST 8
inch:		jp	jp_rst08	; Zeicheneingabe -> Sprungliste im RAM 1802
rinch:		push	hl		; Standard-Routine Zeicheneingabe 
		push	bc
		jp	rinch1
; RST 10
outch:		jp	jp_rst10	; Zeichenausgabe -> Sprungliste im RAM 1805
routch:		push	af		; Standard-Routine Zeichenausgabe
		push	hl
		push	de
		jr	routch1
; RST 18
prnst:		jp	jp_rst18	; Ausgabe Zeichenkette -> 1808
rprnst:		ex	(sp), hl	; Standard-Routine Zeichenkette
rprnst1:	ld	a, (hl)		; Zeichen holen
		inc	hl		; nächste Adr.
		jr	rprnst2
;RST 20
		jp	jp_rst20	; Sprungliste im RAM 180B
;
init1:		ld	sp, SYSSK	; System-Stack
		jr	init2
; RST 28
		jp	jp_rst28	; Sprungliste im RAM 180E
;
init2:		call	REGA		; Register im Registerrettebereich ablegen
		jr	init3
; RST 30
		jp	jp_rst30	; Sprungliste im RAM 1811
;
init3:		jp	init4
		db 0FFh
		db 0FFh
; RST 38
		jp	jp_rst38	; Sprungliste im RAM 1814

; Standardroutine f. RST 38
rError:		ld	(REGHL), hl
		pop	hl
		ld	(SYSSK), hl	; System-Stack
		ld	hl, (REGHL)
		ld	(REGSP), sp
		ld	sp, SYSSK	; System-Stack
		call	REGA		; Register im Registerrettebereich ablegen
		jp	rError1
;
rprnst2:	rst	10h		; Ausgabe Zeichen
		bit	7, a		; Ende (Bit7 gesetzt)?
		jr	z, rprnst1	; nein -> nächstes Zeichen
		ex	(sp), hl	; neue Return-Adr.
		ret

;------------------------------------------------------------------------------
; testet den Tastaturstatus, kehrt bei gedrückter Taste nach 30 ms 
; mit Wert zurück ( wartet nicht auf loslassen der Taste! )
; wenn keine Taste gedrückt, sofortige Rückkehr mit gesetztem Zero-Flag
;------------------------------------------------------------------------------

TASTE:		in	a, (PIODA)
		bit	7, a		; Taste gedrückt?
		ret	z		; nein
		call	MS30		; 30 ms	warten
		in	a, (PIODA)	; Tastenwert 
		bit	7, a		; Taste noch gedrückt?
		ret
; NMI
		jp	nmi		; Sprungliste im RAM 1817

;------------------------------------------------------------------------------
; RST 8: Zeichenausgabe Register A
; der Anfang der Routine ist oben routch
;------------------------------------------------------------------------------

; 013FFH Bildschirmanfang (links oben !!)
; 01000H Bildschirmende (rechts unten !!)
; 16 Zeilen a 64 Zeichen

; Der BWS arbeitet aufgrund der Hardware invers
; d.h. inc geht ein Zeichen / eine Zeile zurück
;      dec geht ein Zeichen / eine Zeile vor

routch1:	and	7Fh
		ld	hl, (cupos)	; Cursorposition
		cp	0Dh
		jr	z, ocr		; neue Zeile
		cp	0Ch
		jr	z, ocls		; Bildschirm löschen
		cp	8
		jr	z, odel		; Backspace
		cp	' '
routch2:	jr	c, routch5	; sonstige Steuerzeichen übergehen
; Zeichenausgabe
		ld	(hl), a		; Zeichen in BWS schreiben
		dec	hl		; nächste Position
routch3:	ex	de, hl
		ld	hl, BWS-1
		and	a		; Cy=0
		sbc	hl, de		; Bildschirmende erreicht?
		ex	de, hl
		ld	(cupos), hl	; Cursorposition
		jr	c, routch5
; 1 Zeile hochscrollen		
		push	bc
		ld	hl, BWS+(LINES-1)*COLS-1
		ld	de, BWS+LINES*COLS-1
		ld	bc, (LINES-1)*COLS
		lddr
		ld	(cupos), de	; Cursorposition
		ex	de, hl
		inc	hl
routch4:	dec	l
		ld	(hl), ' '
		jr	nz, routch4
		pop	bc
routch5:	pop	de
		pop	hl
		pop	af
		ret
; neue Zeile
ocr:		ld	a, l
		and	0C0h 		; Cursorpos abrunden auf Zeilenende im BWS
		ld	l, a
		dec	hl		; nächstes Zeichen
		jr	routch3
; Bildschirm löschen
ocls:		ld	a, ' '
		ld	hl, BWS
		ld	(hl), a
		ld	d, h
		ld	e, l
		inc	e
		push	bc
		ld	bc, LINES*COLS-1
		ldir
		ld	(cupos), hl	; Cursorposition
		pop	bc
		jr	routch5
; Zeichen löschen		
odel:		inc	hl		; eine Position zurück
		ld	(hl), ' '       ; Cursor löschen
		jr	routch3

;------------------------------------------------------------------------------
; RST 10 Zeicheneingabe
; der Anfang der Routine ist oben rinch
;------------------------------------------------------------------------------

rinch1:		ld	hl, (cupos)	; Cursorposition
rinch2:		ld	(hl), '_'       ; Cursorzeichen
rinch3:		ld	b, 0
rinch4:		push	bc
		ld	b, 0
rinch5:		djnz	rinch5		; kurz warten
		pop	bc
		call	TASTE		; Taste gedrückt?
		jr	nz, rinch6	; ja
		djnz	rinch4		; sonst weiter versuchen
		; Cursor blinken lassen
		ld	a, '_'		; dazu Cursorzeichen
		cp	(hl)
		jr	nz, rinch2
		ld	(hl), ' '	; und Leerzeichen toggeln
		jr	rinch3
;
rinch6:		ld	(hl), ' '	; Cursor ausblenden
		; Entprellung
		push	af
		in	a, (PIODB)
		res	0, a
		out	(PIODB), a
		call	MS30		; 30 ms	warten
		in	a, (PIODB)
		set	0, a
		out	(PIODB), a
rinch7:		in	a, (PIODA)
		bit	7, a
		jr	nz, rinch7
		pop	af
		; Bit 7 rücksetzen (Kennung Taste gedrückt)
		and	7Fh
		pop	bc
		pop	hl
		ret

;------------------------------------------------------------------------------
; zu Standardroutine f. RST 38
;------------------------------------------------------------------------------

rError1:	call	initsv
		rst	18h
		db 0Dh,"ERROR AT",' '+80h
		ld	hl, (SYSSK)	; System-Stack
		dec	hl
		call	OUTHL		; Ausgabe HL hexadezimal
		jp	GETCO1		; Monitoreingabeschleife

;------------------------------------------------------------------------------
; ca. 30 ms warten
;------------------------------------------------------------------------------

MS30:		push	bc
		ld	bc, 903h	; B = 9, C = 3
ms301:		dec	bc
		ld	a, b
		or	c
		jr	nz, ms301
		pop	bc
		ret

;------------------------------------------------------------------------------
; Zeile	eingeben
; liest eine Zeile, die mit cr abgeschlossen wird, auf dem Schirm ein und 
; legt den Anfang der Zeile in SOIL ab
; kein Register wird zerstört
;------------------------------------------------------------------------------

INLINE:		push	af
		rst	18h
		db " #",' '+80h		;Prompt
;		
inlin1:		rst	8		; inch
		rst	10h		; outch
		cp	0Dh		; Enter?
		jr	nz, inlin1	; nein --> weiter eingeben
; Zeilenanfang ermitteln
		push	hl
		ld	hl, (cupos)	; Cursorposition (Anfang der nächsten Zeile)
		ld	a, '#'
inlin2:		cpi			; Anfang der Zeile rückwärts suchen
					; aber da BWS invers arbeitet, inc HL!
		jr	nz, inlin2
		dec	hl		; vor #
		dec	hl		; #
		dec	hl		; Leerzeichen nach #
		ld	(soil),	hl	; erstes Zeichen der Zeile
		pop	hl
		pop	af
		ret

;------------------------------------------------------------------------------
; fuehrende Leerzeichen ueberlesen
; letzen vier Zeichen als Hexzahl konvertieren
; und in DATA ablegen
;------------------------------------------------------------------------------

konvx:		ld	a, (de)
		cp	' '		; Leerzeichen 
		dec	de		
		jr	z, konvx	; überlesen
;
		inc	de		; erstes Zeichen
		xor	a
konvx1:		ld	hl, data
		ld	(hl), a
		inc	hl
		ld	(hl), a
		inc	hl
		ld	(hl), a		; data=0
konvx2:		ld	a, (de)
		dec	hl
		dec	hl
		sub	30h ; '0'       ; Zeichen<"0"?
		ret	m
		cp	0Ah		; Zeichen<="9"?
		jr	c, konvx3
		sub	7
		cp	0Ah		; Zeichen<"A"?
		ret	m
		cp	10h		; Zeichen>"F"?
		ret	p
konvx3:		dec	de
		inc	(hl)
		inc	hl
		rld			; Hexziffer eintragen
		inc	hl
		rld
		jr	konvx2		; nächste Ziffer

;------------------------------------------------------------------------------
; wandelt eine maximal vierstellige in ASCII-Zeichen angegebene Zahl 
; ab (DE) abwärts in deren hexadezimalen Wert um, der dann in HL steht. 
; DE wird entsprechend dekrementiert, der Akku wird zerstört
;------------------------------------------------------------------------------

INHEX:		push	bc
		push	af
		call	konvx		; Konvertierung
		inc	hl
		ld	b, h		; BC=HL=DATA+1
		ld	c, l
		ld	l, (hl)		; unteres Byte
		inc	bc
		ld	a, (bc)
		ld	h, a		; oberes Byte
		pop	bc
		or	l		; Z-Flag setzen
		ld	a, b
		pop	bc
		ret

;------------------------------------------------------------------------------
; Ausgabe (A) hexadezimal
; gibt den Akku als zweistellige Hexzahl auf dem Schirm aus,
; kein Register wird zerstört
;------------------------------------------------------------------------------

OUTHEX:		push	af
		rra
		rra
		rra
		rra
		call	OUTHEX1		; obere Tetrade ausgeben
		pop	af		; und die untere
OUTHEX1:	push	af
		and	0Fh
		add	a, '0' 		; Konvertierung --> ASCII
		cp	':'		; Ziffer "A" ... "F"?
		jr	c, OUTHEX2	; nein
		add	a, 7		; sonst Korrektur
OUTHEX2:	rst	10h		; und Ausgabe
		pop	af
		ret

;------------------------------------------------------------------------------
; Ausgabe HL hexadezimal
; gibt das HL-Register als vierstellige Hexzahl auf dem Schirm aus,
; kein Register wird zerstört
;------------------------------------------------------------------------------

OUTHL:		push	af
		ld	a, h
		call	OUTHEX
		ld	a, l
		call	OUTHEX
		pop	af
		ret

;------------------------------------------------------------------------------
; Register im Registerrettebereich ablegen
;------------------------------------------------------------------------------

REGA:		ld	(data),	sp
		ld	sp, REGPC
		push	iy
		push	ix
		push	hl
		push	de
		push	bc
		push	af
		exx
		ex	af, af'		;'
		push	hl
		push	de
		push	bc
		push	af
		jr	REG1
		
; Register aus Registerrettebereich holen		
REGH:		ld	(data),	sp	
		ld	sp, RSA
		pop	af
		pop	bc
		pop	de
		pop	hl
		exx
		ex	af, af'		;'
		pop	af
		pop	bc
		pop	de
		pop	hl
		pop	ix
		pop	iy
REG1:		ld	sp, (data)
		ret

;------------------------------------------------------------------------------
; Kommandoschleife
;
; Suchen des Kommandos
; wenn ':' folgt, dann alte Parameter nehmen
; sonst 3 Parameter einlesen und in ARG1..ARG3 ablegen
; bei Kdo-Start gilt
; 	BC = Return-Adr
; 	DE zeigt auf ':' oder auf Leezeichen hinter letzem Parameter
; 	HL = Kdo-Adr
;------------------------------------------------------------------------------

kdoerr:		rst	18h
		db "WHAT? ",' '+80h


;Eingang Kommandomodus
kdo1:		call	INLINE		; Zeile	eingeben
;suche Kommandorahmen
		ld	hl, kdo4	; startpos.
		ld	bc, RAMEND-kdo4	; Anzahl (RAM-Ende-Startpos.)
kdo2:		xor	a		; 00
		inc	bc
		cpir
		jr	nz, kdoerr
		ld	a, 9		; 09
		cp	(hl)
		jr	nz, kdo2
		inc	hl
		ld	de, (soil)
		ld	a, (de)		; KDO
		cp	(hl)
		jr	nz, kdo2	; falsches Kdo
		inc	hl
		ld	a, (hl)
		cp	0Dh		; 0D
		jr	nz, kdo2
		inc	hl
		push	hl
; Parameter
		dec	de
		call	INHEX		; 1. Argument
		jr	nz, kdo3
		ld	a, (de)
		cp	':'             ; die alten Werte nehmen ?
		jr	z, kdo4
kdo3:		ld	(ARG1),	hl
		call	INHEX
		ld	(ARG2),	hl
		call	INHEX
		ld	(ARG3),	hl
; starten
kdo4:		pop	hl
		ld	bc, kdo1	; Returnadr. auf Stack
		push	bc
		jp	(hl)		; Kdo starten

;------------------------------------------------------------------------------
;' ' aaaa	bbbb cccc	Eingabe	von Argumenten
;------------------------------------------------------------------------------
		
		db    0, 9, ' ', 0Dh
;
		ret

;------------------------------------------------------------------------------
; init. Sprungverteiler für RST-Aufrufe
; RST xx springt in den RAM. von dort geht es normalerweise zu den Monitor-
; Routinen, aber man kann auch eigene Ein-/Ausgaberoutinen nehmen
;------------------------------------------------------------------------------

sv_rst:		jp	rinch			; RST 8
		jp	routch			; RST 10
		jp	rprnst			; RST 18
		jp	0FFFFh			; RST 20
		jp	0FFFFh			; RST 28
		jp	0FFFFh			; RST 30
		jp	rError			; RST 38
		jp	0FFFFh			; NMI

; Sprungverteiler in RAM kopieren
initsv:		ld	hl, sv_rst
		ld	de, jp_rst08
		ld	bc, 24			; Länge
		ldir
		ret

;------------------------------------------------------------------------------
; weiter nach Reset
;------------------------------------------------------------------------------

init4:		call	initsv		; Sprungverteiler in RAM kopieren
		rst	18h
		db 0Ch,0Dh,"      AC 1   U 880 - MONITOR  V 3.1",0Dh,0Dh+80h
		
		; init PIO
		ld	a, 4Fh 		; Port A Mode 1 Byte-Eingabe 
		out	(PIOCA), a
		ld	a, 0CFh		; Port B Mode 3 Bit-ein/Ausgabe
		out	(PIOCB), a
		ld	a, 80h 		; Port B Bitmaske, Bit 7 = Eingabe
		out	(PIOCB), a
		ld	a, 0FFh		; Port B Ausgabe alle Bits 1
		out	(PIODB), a
		jr	GETCO1		; Monitoreingabeschleife

;------------------------------------------------------------------------------
; I	lnitialize	RSA löschen, SP	initialisieren
;------------------------------------------------------------------------------

		db    0, 9, 'I', 0Dh
;
		ld	hl, data
		ld	b, sysramend-data	; Bereich data..BPOPC+3
iko1:		ld	(hl), 0		; mit 0 füllen	
		inc	hl
		djnz	iko1
;
		ld	hl, RAM+7FFh	; 2K-RAM?
		ld	a, 55h
		ld	(hl), a
		cp	(hl)		; Test auf RAM
		ld	a, hi(RAM+800h)	; hi(ramende 2K)
		jr	z, iko2
		ld	a, hi(RAM+400h)	; hi(ramende 1K)
iko2:		ld	(REGSP+1), a	; RAM-Ende in SP schreiben (2000 bzw. 1C00) 
;
		rst	18h		; Ausgabe Meldung
		db "CLR/RSA",0Dh+80h

;------------------------------------------------------------------------------
; Monitoreingabeschleife
;------------------------------------------------------------------------------

GETCO1:		ld	sp, SYSSK	; SP auf System-Stack setzen
getco2:		jp	kdo1		; Kommandoschleife

;------------------------------------------------------------------------------
;S aadr	eadr sadr name	Save	Datei auf Kassette ausgeben
;s. ac1_berlin/kassettenformate_ac1_mode.htm
;------------------------------------------------------------------------------

		db    0, 9, 'S', 0Dh
;
csave:		dec	de		; de = bws-pos. nach 3. Parameter
		ld	a, (de)
		cp	' '		; Leerzeichen vor Filename
		jr	z, csave	; übergehen
;Vorton 2x256	00-Bytes ausgeben
		ld	b, 0
sav1:		xor	a
		call	sav10		; Ausgabe 1 Byte A
		xor	a
		call	sav10		; Ausgabe 1 Byte A
		djnz	sav1
;Synchronisationsmuster E6h
		ld	a, 0E6h	  	; Synchronisationsmuster
		call	sav10		; Ausgabe 1 Byte A
;name ausgeben 'U' + 6 Zeichen
		ld	a, 55h ; 'U'    ; Kennbyte fuer Namen
		call	sav10		; Ausgabe 1 Byte A
		ld	b, NAMELEN
sav2:		ld	a, (de)
		call	sav10		; Ausgabe 1 Byte A
		dec	de
		djnz	sav2
;256 Nullbytes,	Zeit für Namensdarstellung beim	Einlesen
		ld	b, 0
sav3:		xor	a
		call	sav10		; Ausgabe 1 Byte A
		djnz	sav3
;
		ld	hl, (ARG1)	; aadr
		ld	de, (ARG2)	; eadr
sav4:		push	de
		ex	de, hl
		and	a
		sbc	hl, de
		jr	c, sav8		; wenn Ende erreicht
;
; Block ausgeben: '<' + Blocklänge + max. 256 Byte Daten + Prüfsumme
		ld	a, 3Ch ; '<'    ; Kennung Datenblock
		inc	hl		; HL=restl. Programmlänge
		call	sav10		; Ausgabe 1 Byte A
		ld	a, h
		and	a		; H = 0?
		jr	z, sav5		; wenn HL < 256
		ld	b, 0		; sonst 256 Zeichen
		jr	sav6
sav5:		ld	b, l
sav6:		ex	de, hl
		pop	de
		ld	a, b		; Blocklänge
		call	sav10		; Ausgabe 1 Byte A
		ld	a, l		; Prüfsumme = 0, Blockadr.
		add	a, h		; zur Prüfsumme addieren
		ld	c, a		; Prüfsumme merken
;Blockstartadr
		call	sav9		; Ausgabe 2 Byte L,H
sav7:		ld	a, c		; Prüfsumme erhöhen
		add	a, (hl)
		ld	c, a
		ld	a, (hl)
		call	sav10		; Ausgabe 1 Byte A
		inc	hl
		djnz	sav7		; Prüfsumme erhöhen
		ld	a, c		; Prüfsumme ausgeben
		call	sav10		; Ausgabe 1 Byte A
		jr	sav4
; Programmende 'x' + sadr
sav8:		pop	de
		ld	a, 78h ; 'x'    ; Kennung Ende
		call	sav10		; Ausgabe 1 Byte A
		ld	hl, (ARG3)	; SADR
; Ausgabe 2 Byte L,H
sav9:		ld	a, l
		call	sav10		; Ausgabe 1 Byte A
		ld	a, h
		jr	sav10
;
; Ausgabe 1 Byte A
sav10:		push	bc
		ld	c, a		; Datenbyte nach C
		; C enthaelt die Daten und ist gleichzeitig Bitzaehler
		scf
sav11:		rl	c		; Carry nach Bit 0
		; hoechstwertigstes (Daten)Bit nach Carry
                ; und Test ob das Byte 0 ist
		jr	z, sav15	; wenn 0 dann fertig
		; sonst Bit ausgeben, dazu
		in	a, (PIODB)	; PIO-Daten lesen
		set	6, a		; Bit 6 auf high
		jr	nc, sav12	; war Datenbit 1?
		res	6, a		; nein, Bit 6 low
sav12:		out	(PIODB), a	; Ausgabe 1.Halbw.
		ld	b, ZK1		; Zeitkontante 1
sav13:		djnz	sav13
		add	a, 40h		; Flankenwechsel
		out	(PIODB), a	; Ausgabe 2.Halbw.
		ld	b, ZK2		; Zeitkonstante 2
sav14:		djnz	sav14
		and	a		; Carry auf 0 f. Bitzähler
		jr	sav11		; dann weitere Datenbit ausgeben
;
sav15:		pop	bc
		call	loa20		; Test auf Ctrl-C und ggf. Abbruch
		ret

;------------------------------------------------------------------------------
;V	Verify	Datei von Kassette mit Speicher	vergleichen
;------------------------------------------------------------------------------

		db    0, 9, 'V', 0Dh
;
		ld	b, 0FFh		; LOMOD = FFh
		ld	hl, 0		; Ladeadr.
		jr	loa1

;------------------------------------------------------------------------------
;L offset[-]	Load	Laden einer Datei von Kassette
;------------------------------------------------------------------------------

		db    0, 9, 'L', 0Dh
;
cload:		ld	a, (de)
		ld	de, (ARG1)
		ld	b, 0		; LOMOD = 0
		cp	'-'		; folgt '-' ?
		jr	nz, loa2	; nein
		ld	hl, 0
		sbc	hl, de		; sonst offset negieren
loa1:		ld	(ARG1),	hl	; Ladeadr.
;
loa2:		ld	ix, LOMOD	; load/verify
		ld	(ix+0),	b
;Phasenkorrektur ermitteln
		xor	a
		ld	(phako), a	; Phasenkorrektur
loa3:		call	loa17		; Bitweise lesen
		cp	0E6h ; 'µ'      ; Synchronisationsmuster
		jr	z, loa4
		cp	19h		; Synchronisationsmuster negiert
		jr	nz, loa3	; Vorton überlesen
		ld	a, 0FFh
		ld	(phako), a	; Phasenkorrektur
loa4:		call	loa15		; Lesen	ein Byte A
		cp	55h ; 'U'       ; Kennbyte fuer Namen
		jr	nz, loa4
;Name laden und	anzeigen
		ld	b, NAMELEN
		ld	hl, (cupos)	; Cursorposition
loa5:		call	loa15		; Lesen	ein Byte A
		ld	(hl), a
		dec	hl
		djnz	loa5
		dec	hl
		ld	(cupos), hl	; Cursorposition
		ex	de, hl
;
loa6:		call	loa15		; Lesen	ein Byte A
		cp	78h ; 'x'       ; Kennung Ende
		jr	z, loa10	; Programmende
		cp	3Ch ; '<'       ; Kennung Datenblock
		jr	nz, loa6
		call	loa15		; Lesen	ein Byte A
		ld	b, a		; Anzahl der Bytes im Block
		call	loa12		; Lesen	2 Byte L,H (adr)
		add	a, l		; Prüfsumme
		ld	c, a
		push	de
		ld	de, (ARG1)	; Ladeadr
		add	hl, de
		pop	de
loa7:		call	loa15		; Lesen	ein Byte A
		bit	0, (ix+0)	; Load oder Verify?
		jr	nz, loa8	; bei Verify:
		ld	(hl), a		; kein Speichern
loa8:		cp	(hl)
		jr	z, loa9		; kein Fehler
; Anz. Fehlerstelle Adr Soll-Wert Ist-Wert
		push	af
		ld	a, ' '
		rst	10h
		call	OUTHL		; Ausgabe HL hexadezimal
		ld	a, ' '
		rst	10h
		pop	af
		call	OUTHEX		; Ausgabe Soll-Byte
		ld	a, ' '
		rst	10h
		ld	a, (hl)
		call	OUTHEX		; Ausgabe Ist-Byte
		ld	a, ' '
		rst	10h
		jr	loa21		; Abbruch
; wenn kein RAM-Fehler
loa9:		inc	hl
		add	a, c		; Prüfsumme erhöhen
		ld	c, a		
		djnz	loa7
		call	loa15		; Lesen	ein Byte A
		cp	c		; Prüfsumme vergleichen
		jr	z, loa6		; kein Fehler
		rst	18h		; Fehlerausgabe
		db "CHECKSU",'M'+80h	; wg. Prüfsummendifferenz
		jr	loa21		; Abbruch
; Programmende
loa10:		call	loa12		; Startadr. lesen
		ld	(ARG1),	hl	; als Parameter ablegen
loa11:		call	OUTHL		; und Anzeige
		ld	a, ' '		; Ausgabe Leerzeichen
		jp	outch		; 
;		
; Lesen	2 Byte L,H
loa12:		call	loa15		; Lesen	ein Byte A
		ld	l, a
		call	loa15		; Lesen	ein Byte A
		ld	h, a
; Cursor toggeln		
		ex	de, hl
		bit	1, (hl)
		jr	z, loa13
		ld	(hl), ' '
		jr	loa14
loa13:		ld	(hl), '*'
loa14:		ex	de, hl
		ld	a, h
		ret
;
; Lesen	ein Byte A
loa15:		push	bc
		ld	b, 8		; 8 bit
loa16:		call	loa17		; Lesen
		djnz	loa16
		ld	c, a
		ld	a, (phako)	; Phasenkorrektur
		xor	c		; ggf. Bits negieren
		pop	bc
		ret
; Lesen ein Bit
loa17:		push	bc
		push	af
		in	a, (5)		; Bit 7 lesen
		and	80h
		ld	c, a
loa18:		in	a, (5)
		and	80h
		cp	c		; Flankenwechsel?
		jr	z, loa18	; nein
		ld	c, a		; Bit sichern
		ld	b, ZKR		; Zeitkonstante, ca 2/3 Vollwelle
loa19:		djnz	loa19		; Warten
		call	loa20		; Test auf Ctrl-C und ggf. Abbruch
		pop	af
		rl	c		; Carry <- Bit 7
		rla			; A <- Carry
		pop	bc
		ret

; Test auf Ctrl-C und ggf. Abbruch
loa20:		in	a, (PIODA)	; Tastatur einlesen
		cp	83h 		; CTRL-C gedrueckt?
		ret	nz		; nein
;sonst Abbruch
loa21:		rst	18h
aBrea:		db "BREA",'K'+80h
		jp	GETCO1

;------------------------------------------------------------------------------
;D aadr	eadr	Dump	Hexdump
;------------------------------------------------------------------------------


		db    0, 9, 'D', 0Dh
;
		ld	hl, (ARG1)
dko1:		ld	de, (ARG2)
		push	hl
		sbc	hl, de
		pop	hl
		ret	nc		; wenn EADR<AADR
		call	OUTHL		; Ausgabe HL hexadezimal
		ld	b, 10h		; 16 Byte pro Zeile
d_ko2:		rst	18h
		db ' '+80h
		ld	a, (hl)
		call	OUTHEX		; ausgeben
		inc	hl
		djnz	d_ko2		; nächstes Byte
;		
		rst	18h
		db 0Dh+80h		; neu Zeile
		jr	dko1

;------------------------------------------------------------------------------
;M adr		Modify Memory	Speicher modifizieren
;------------------------------------------------------------------------------

		db    0, 9, 'M', 0Dh
;
mem:		ld	hl, (ARG1)
mem1:		call	OUTHL		; Ausgabe Adresse
		rst	18h
		db ' ',' '+80h
		ld	a, (hl)
		call	OUTHEX		; Ausgabe Byte
		call	INLINE		; Zeile	eingeben
		ld	de, (soil)
		dec	hl
mem2:		inc	hl
		push	hl
		call	INHEX
		jr	z, mem5		; Trennzeichen
mem3:		ld	a, l
		pop	hl
		ld	(hl), a
		cp	(hl)		; RAM-Test
		jr	z, mem2		; i.O.
		rst	18h		; sonst 
		db "ERROR ",' '+80h	; Fehlrt
mem4:		jr	mem1
;
mem5:		ld	a, (de)		; Test Datenbyte=0
		cp	' '		; wenn ja --> Z=1
		jr	z, mem3
		pop	hl
		inc	hl
		ld	(ARG2),	hl	; 1. nichtbearb. Adr.
		cp	'.'
		ret	z		; Return, wenn "." gegeben
;		
		ld	de, (soil)
		ld	a, (de)		; wurde überhaupt was eingegeben?
		cp	' '
		jr	z, mem4		; Z=1 keine Eingabe
		dec	hl		; sonst eine Adresse
		jr	mem4		; zurück

;------------------------------------------------------------------------------
; Argumente uebergeben
;------------------------------------------------------------------------------

para:		ld	hl, (ARG1)
		ld	de, (ARG2)
		ld	bc, (ARG3)
		ret

;------------------------------------------------------------------------------
;P aadr	eadr cc		Pattern		Speicherbereich	mit Muster füllen
;------------------------------------------------------------------------------

		db    0, 9, 'P', 0Dh
;
		call	para		; Argumente uebergeben
		ld	(hl), c
		push	hl
		xor	a		; Cy = 0
		ex	de, hl
		sbc	hl, de
		ld	b, h		; BC = Länge
		ld	c, l
		pop	hl
		ld	d, h
		ld	e, l
		inc	de		; DE = HL + 1
pko1:		ldir			; füllen
		ret

;------------------------------------------------------------------------------
;T aadr	zadr anz	Transfer	Speicherbereich	kopieren
;------------------------------------------------------------------------------

		db    0, 9, 'T', 0Dh
;
		call	para		; Argumente uebergeben
		xor	a		; cy = 0
		push	hl
		sbc	hl, de
		pop	hl
		jr	nc, pko1	; wenn Zieladr. größer
		add	hl, bc		; sonst Werte anpassen
		ex	de, hl
		add	hl, bc
		ex	de, hl
		dec	hl
		dec	de
		lddr			; Rueckwaertstransfer
		ret

;------------------------------------------------------------------------------
; A aaaa bbbb c		Arithmetik	Summe, Differenz, Displ., Dezimalwert
; c ist Länge des Sprungbefehls (für relative Sprungbefehle ist c gleich zwei)
;------------------------------------------------------------------------------

		db    0, 9, 'A', 0Dh
;
		call	para		; Argumente uebergeben
		push	hl
		push	de
		add	hl, bc
		ex	de, hl
		sbc	hl, de
		ld	a, 0
		jr	c, ako1
		cp	h
		jr	nz, ako3
		bit	7, l
		jr	nz, ako3
		jr	ako2
;		
ako1:		cpl
		cp	h
		jr	nz, ako3
		bit	7, l
		jr	z, ako3
;		
ako2:		rst	18h
		db "DSPL",':'+80h
		ld	a, l
		call	OUTHEX
;		
ako3:		pop	de
		pop	hl
		push	hl
		push	hl
		add	hl, de		; Summe
		rst	18h
		db "   SUM",':'+80h
		call	OUTHL		; Ausgabe HL hexadezimal
		pop	hl
		sbc	hl, de		; Differenz
		rst	18h
		db "   DIF",':'+80h
		call	OUTHL		; Ausgabe HL hexadezimal
		pop	bc
; Hezimalwandlung BC -> AHL
		ld	hl, 0
		ld	e, l
ako4:		ld	a, b
		or	c
		jr	z, ako5
		dec	bc
		ld	a, l
		inc	a
		daa
		ld	l, a
		ld	a, h
		adc	a, 0
		daa
		ld	h, a
		jr	nc, ako4
		inc	e
		jr	ako4
;		
ako5:		rst	18h
		db "   DEC",':'+80h
		ld	a, e
		and	0Fh
		or	30h ; '0'	; 5. Stelle (max 6)
		rst	10h
		call	OUTHL		; Ausgabe HL 

;------------------------------------------------------------------------------
; Ausgabe Zeilenvorschub
;------------------------------------------------------------------------------

outcr:		rst	18h
		db 0Dh+80h
		ret
		
;------------------------------------------------------------------------------
; Ausgabe der Registernamen
;------------------------------------------------------------------------------

anzsp:		rst	18h
		db "SP",':'+80h
		jr	OUTHL3
anzpc:		rst	18h
		db "PC",':'+80h
		jr	OUTHL3
anziy:		rst	11000b
		db "IY",':'+80h
		jr	OUTHL3
anzix:		rst	18h
		db "IX",':'+80h
		jr	OUTHL3
anzhl:		rst	18h
		db "HL",':'+80h
		jr	OUTHL3
anzde:		rst	18h
		db "DE",':'+80h
		jr	OUTHL3
anzbc:		rst	18h
		db "BC",':'+80h
		jr	OUTHL3
anzaf:		rst	18h
		db "AF",':'+80h
;
OUTHL3:		call	OUTHL		; Ausgabe HL hexadezimal
out3sp:		rst	18h		; Ausgabe 3 Leerzeichen
		db "   ",' '+80h
		ret

;------------------------------------------------------------------------------
;RXX	Register	Registerinhalt verändern
;R:	Registerinhalte anzeigen
;------------------------------------------------------------------------------

		db    0, 9, 'R', 0Dh
;
		cp	':'
		jp	nz, rko1	; wenn Modifizierung
; Anzeige aller Register
; BP:0000	   BS:000000
; SP:1C00	   PC:0000    IY:0000	 IX:0000
; MAIN  HL:0000	   DE:0000    BC:0000    AF:0000
; EXXR  HL:0000	   DE:0000    BC:0000    AF:0000
; FLAGS:   ()
		rst	18h		; Anzeige Breakpointadresse
		db "BP",':'+80h
		ld	hl, (BPADR)
		call	OUTHL3
		rst	18h		; Ausgabe Operandenfolge
		db "BS",':'+80h
		ld	hl, BPOPC	; 3 Byte
		ld	a, (hl)
		call	OUTHEX
		inc	hl
		ld	a, (hl)
		call	OUTHEX
		inc	hl
		ld	a, (hl)
		call	OUTHEX
		call	outcr
;Sonderregister-Anzeige
		ld	hl, (REGSP)
		call	anzsp
		ld	hl, (REGPC)
		call	anzpc
		ld	hl, (REGIY)
		call	anziy
		ld	hl, (REGIX)
		call	anzix
;1. Registersatz
		rst	18h
		db 0Dh,"MAIN ",' '+80h
		ld	hl, (REGHL)
		call	anzhl
		ld	hl, (REGDE)
		call	anzde
		ld	hl, (REGBC)
		call	anzbc
		ld	hl, (REGAF)
		call	anzaf
;2. Registersatz
		rst	18h
		db  0Dh
		db "EXXR ",' '+80h
		ld	hl, (REGHLX)
		call	anzhl
		ld	hl, (REGDEX)
		call	anzde
		ld	hl, (REGBCX)
		call	anzbc
		ld	hl, (REGAFX)
		call	anzaf
; Flags
		rst	18h
		db 0Dh,"FLAGS: ",' '+80h
		ld	a, (REGAF)
		call	flags
		rst	18h
		db "  ",'('+80h
		ld	a, (REGAFX)
		call	flags
		rst	18h
		db ')',0Dh+80h
		ret
;
flags:		ld	l, a
		bit	7, l
		jr	z, flg1
		rst	18h
		db 'S'+80h
flg1:		bit	6, l
		jr	z, flg2
		rst	18h
		db 'Z'+80h
flg2:		bit	4, l
		jr	z, flg3
		rst	18h
		db 'H'+80h
flg3:		bit	2, l
		jr	z, flg4
		rst	18h
		db 'P'+80h
flg4:		bit	1, l
		jr	z, flg5
		rst	18h
		db 'N'+80h
flg5:		bit	0, l
		jr	z, flg6
		rst	18h
		db 'C'+80h
flg6:		ret

; RXX' Register ändern
rko1:		ld	hl, (soil)
		dec	hl
		dec	hl
		ld	a, (hl)
		ld	de, REGAF
		cp	'A'
		jr	z, rko2		; wenn AF
		inc	de
		inc	de		; DE = REGBC
		cp	'B'		
		jr	z, rko2		; wenn BC
		inc	de
		inc	de		; DE = REGDE
		cp	'D'
		jr	z, rko2		; wenn DE
		inc	de
		inc	de		; DE = REGHL
		cp	'H'
		jr	nz, rko4	; wenn <> HL
rko2:		dec	hl		; 2 Zeichen vor
		dec	hl
		ld	a, (hl)
		cp	27h ; '''	; folgt '?
		ex	de, hl
		jr	nz, rko3	; nein
		ld	de, 8		; sonst 2. Registersatz
		sbc	hl, de
;		
rko3:		ld	e, (hl)		; Wert aus Speicher holen
		inc	hl
		ld	d, (hl)
		ex	de, hl
		call	OUTHL		; Ausgabe
;Eingabe neuer Wert	
		call	INLINE		; Zeile	eingeben
		ld	hl, (soil)
		ex	de, hl
		push	hl
		push	de
		call	INHEX		; HL=neuer Wert
		ex	de, hl
		pop	hl
		ld	a, (hl)
		cp	' '		; wurde was eingegeben?
		pop	hl		; HL=Adr. im RSA
		ret	z		; wenn keine Eingabe
		ld	(hl), d		; sonst Wert
		dec	hl
		ld	(hl), e		; übernehmen
		ret
;		
rko4:		inc	de
		inc	de		; DE = REGIX
		dec	hl
		ld	a, (hl)
		cp	'X'		
		jr	z, rko5		; wenn IX
		inc	de
		inc	de		; DE = REGIY
		cp	'Y'
		jr	z, rko5		; wenn IY
		inc	de
		inc	de		; DE = REGPC
		cp	'C'
		jr	z, rko5		; wenn PC
		inc	de
		inc	de		; DE = REGSP
		cp	'P'
		jp	nz, kdoerr	; wenn <> SP
rko5:		ex	de, hl
		jr	rko3

;------------------------------------------------------------------------------
; Einsprung bei Breakpoint
;------------------------------------------------------------------------------

break:		call	REGA		; Register in RSA ablegen
		pop	hl		; HL=Breakadr.+3
		ld	(REGSP), sp	; SP sichern
		ld	sp, SYSSK	; System-Stack
		rst	18h
		db "BREAK AT",0BAh
		dec	hl
		dec	hl
		dec	hl
		ld	(REGPC), hl	; Breakadresse
		call	OUTHL3		; anzeigen
;		
		ld	hl, (BPADR)	; Adr.
		ld	de, BPOPC	; die originalen 3 Byte
		ld	bc, 3
		ex	de, hl
		ldir			; zurueckbringen
		ld	sp, SYSSK	; System-Stack
		jp	kdo1

;------------------------------------------------------------------------------
;B adr		Breakpoint	Unterbrechung setzen, nur RAM
;------------------------------------------------------------------------------

		db    0, 9, 'B', 0Dh
;
		ld	hl, (ARG1)
		ld	(BPADR), hl
		ld	de, BPOPC	; 3 Byte Operanden
		ld	bc, 3
		ldir			; sichern
		ret

;------------------------------------------------------------------------------
; E adr		Execute		Programm läuft unter Breakpointkontrolle
;------------------------------------------------------------------------------

		db    0, 9, 'E', 0Dh
;
e_kdo:		ld	hl, (BPADR)
		ld	(hl), 0CDh 	; call break
		inc	hl
		ld	(hl), lo(break)	; an Breakpoint
		inc	hl
		ld	(hl), hi(break)	; eintragen

;vp: hier fehlt eigentlich ein jr j_kdo
;die nachfolgende Kommandokennung wird dekodiert als 
;	nop
;	add     hl, bc
;	ld      c, d
;	dec     c
;das bringt zum Glueck das Programm nicht durcheinander

;------------------------------------------------------------------------------
; J aaaa 	Jump
;------------------------------------------------------------------------------

		db    0, 9, 'J', 0Dh
;		
j_kdo:		ld	b, 10
		ld	hl, (REGSP)
jko1:		dec	hl
		ld	a, (hl)		; Test auf RAM
		cpl
		ld	(hl), a
		cp	(hl)
		jr	nz, jkoerr
		cpl
		ld	(hl), a
		djnz	jko1
;		
		ld	hl, (ARG1)	; Startadresse
		ld	(REGPC), hl	; zwischenspeichern
		ld	sp, (REGSP)	; Stack generieren
		push	hl		; Startadresse in Stack
		jp	REGH		; Register aus RSA holen
					; und Pgm. durch RET starten

;------------------------------------------------------------------------------
;G		Go	Sprung in Programm ab Adresse in PC
;------------------------------------------------------------------------------

		db    0, 9, 'G', 0Dh
;
		ld	hl, (REGPC)
		ld	(ARG1),	hl
		ld	de, (BPADR)
		xor	a		; cy=0
		sbc	hl, de
		jr	nz, e_kdo	; wenn nicht Breakpoint
		jr	j_kdo		; starten
;		
jkoerr:		rst	18h
		db "INIT S",0D0h
		ret

;------------------------------------------------------------------------------
;C adr1	adr2 anz	Compare		Vergleich von 2	Speicherbereichen
;------------------------------------------------------------------------------

		db    0, 9, 'C', 0Dh
;
		call	para		; Argumente uebergeben
cko1:		ld	a, (de)
		cp	(hl)		; Vergleich
		jr	nz, cko3	; wenn ungleich
cko2:		dec	bc
		inc	hl
		inc	de
		ld	a, b
		or	c
		ret	z		; wenn alles geprueft
		jr	cko1		; sonst weitertesten
;		
cko3:		call	OUTHL3		; 1. Adresse
		ld	a, (hl)
		call	OUTHEX		; 1. Byte
		call	out3sp
		call	out3sp
		ex	de, hl
		call	OUTHL3		; 2. Adresse
		ex	de, hl
		ld	a, (de)
		call	OUTHEX		; 2. Byte
		call	outcr
		rst	8		; warten auf Tastendruck
		cp	0Dh
		ret	nz		; Abbruch wenn <> ENTER
		jr	cko2		; sonst weitertesten

;------------------------------------------------------------------------------
;F aa bb ce dd ... nn	Find	Suchen nach Bytes oder Zeichenkette
;------------------------------------------------------------------------------

		db    0, 9, 'F', 0Dh
;
		ld	bc, (ARG1)	; Suchadresse
fko1:		ld	de, (soil)
		dec	de
		dec	de
		inc	bc
		call	INHEX		; L = 1. Suchbyte
;		
fko2:		ld	a, (bc)
		cp	l		; L = Suchbyte
		jr	z, fko3		; wenn Bytes gleich
	ifndef fkopatch
		inc	bc		; sonst naechste Suchadresse
		ld	a, b
		or	c
	else
		ld	a, b
		or	c
		inc	bc		; sonst naechste Suchadresse
	endif	
		jr	z, fko5		; wenn Speicherende erreicht
		jr	fko2		; weitersuchen
;		
fko3:		push	bc
fko4:		call	INHEX		; naechstes Suchbyte holen
		inc	bc
		ld	a, (bc)
		cp	l
		jr	z, fko4		; solange gleich
		ld	a, (de)
		cp	' '		; letztes Suchbyte verglichen?
		pop	bc
		jr	z, fko1		; nein -> weitersuchen
; Bytefolge gefunden
		ld	(ARG1),	bc
		jp	mem		; Speicher modifizieren
; Bytefolge nirgends gefunden
fko5:		rst	18h
		db  20h
		db "NOT FOUND",8Dh
		ret

;------------------------------------------------------------------------------
;Z		 Kaltstart des Mini-BASIC-Interpreters
;------------------------------------------------------------------------------

		db    0, 9, 'Z', 0Dh
;
		jr	BASIC

		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; Sprungverteiler
;------------------------------------------------------------------------------

		jp	MS30		; 30 ms	warten
		jp	OUTHEX
		jp	OUTHL		; Ausgabe HL hexadezimal
		jp	INLINE		; Zeile	eingeben
		jp	INHEX
		jp	TASTE
		jp	GETCO1

; end of "ROM"



		org	RAM

;------------------------------------------------------------------------------
; Arbeitsspeicherzellen
;------------------------------------------------------------------------------

cupos:		ds 2			; Cursorposition
; Sprungverteiler f. RST-Aufrufe und NMI
jp_rst08:	ds 3			; jp	rinch
jp_rst10:	ds 3                    ; jp	routch       
jp_rst18:	ds 3                    ; jp	rprnst       
jp_rst20:	ds 3                    ; jp	0FFFFh       
jp_rst28:	ds 3                    ; jp	0FFFFh       
jp_rst30:	ds 3                    ; jp	0FFFFh       
jp_rst38:	ds 3                    ; jp	rError       
nmi:		ds 3                    ; jp	0FFFFh       
;
soil:		ds 2			; Beginn Eingabezeile
		ds 8
;load
phako:		ds 1			; Phasenkorrektur Load
LOMOD:		ds 1			; Load-Mode: Load=0, Verify=FFh
;Stack
		ds 30h			; Stackbereich
SYSSK:		ds 2			; System-Stack 
data:		ds 2			; interner Speicher f. Monitor
		ds 1
;Kdo-Argumente		
ARG1:		ds 2
ARG2:		ds 2
ARG3:		ds 2
;Registerrettebereich (register save area)
;Reihenfolge REGAFX..REGSP ist wichtig! Nicht verändern!
RSA		equ	$
REGAFX:		ds 2
REGBCX:		ds 2
REGDEX:		ds 2
REGHLX:		ds 2
REGAF:		ds 2
REGBC:		ds 2
REGDE:		ds 2
REGHL:		ds 2
REGIX:		ds 2
REGIY:		ds 2
REGPC:		ds 2
REGSP:		ds 2
;
BPADR:		ds 2
BPOPC:		ds 3
sysramend:	equ $

		end
