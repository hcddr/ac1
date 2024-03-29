; File Name   :	AC12010-MON-10122011.BIN, Mein-Monitor.bin
; Base Address:	0000h Range: 0000h - 1000h Loaded length: 1000h

		page	0
		cpu	z80undoc

;AC1-MONITOR V8 E. Ludwig SCCH, Monitor Version Nov. 1987
; ver�ndert auf Version "AC1-2010 * FARBMONITOR * FDC/RFL"
; ver�ndert auf Version "AC1-2010 * MONITOR * 12/2011"
;reass. Volker Pohlers, Neustadt i.H., 31.01.2023
;letzte �nderung 

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------

; Makros
hi              function x, (x>>8) & 0ffh	; High-Byte
lo              function x, x & 0ffh		; Low-Byte

; Speicheraufteilung
ROM		equ	0000h
BWS		equ	1000h		; 2K
RAM		equ	1800h
; BWS
COLS		equ	64		; muss 2er-Potenz sein!
LINES		equ	32		; muss 2er-Potenz sein!

BWSANF		equ	BWS+LINES*COLS-1	; Adresse Home (links oben)
BWSEND		equ	BWS			; letzte Adresse (rechts unten)

; I/O
CTC0		equ	0
CTC1		equ	1
CTC2		equ	2
CTC3		equ	3
PIODA		equ	4		; Tastatur
PIODB		equ	5		; Grafik/Ton
PIOCA		equ	6
PIOCB		equ	7

;Belegung PIOA
;B0..B6 ASCII-Code Zeichen
;B7 Taste gedr�ckt

;Belegung PIOB
;B0 Ton-Ausgang
;B1 Joystick ges. A
;B2 Grafiktaste
;B3 Bildschim-Mode
;B4 frei
;B5 Start/Stop-Schaltung
;B6 TB-Interface-Ausgang
;B7 TB-Interface-Eingang

PIO2DA		equ	8		; b0..b3 V24, b4..b7 IEC
PIO2DB		equ	9
PIO2CA		equ	0ah
PIO2CB		equ	0bh

; PIO 2 / IOSEL2
; A0 Eingang Daten RxD 			serielle Schnittstelle	
; A1 Ausgang Daten TxD 			serielle Schnittstelle
; A2 Eingang empfangsbereit CTS		serielle Schnittstelle
; A3 Ausgang sendebereit DTR		serielle Schnittstelle
; A4 SRQ		IEC - Schnittstelle (in CPM + BASIC)
; A5 ATN		IEC - Schnittstelle (in CPM + BASIC)
; A6 CLK		IEC - Schnittstelle (in CPM + BASIC)
; A7 DATA		IEC - Schnittstelle (in CPM + BASIC)

;EPROM auf PIO2-Karte
PIO2EPROM	equ	0Fh
; 00 RAM
; 10 Bank 1 auf 2000h einblenden (ROM 0000-07FF)
; 20 Bank 2 auf 2000h einblenden (ROM 0800-0FFF)
; 40 Bank 3 auf 2000h einblenden (ROM 1000-17FF)
; 60 Bank 4 auf 2000h einblenden (ROM 0800-1FFF)

modul1		equ	14h		; Konfigurationsbyte f�r SCCH-Modul 1
; Port 14h, OUT-Port
; 	00 - Modul nicht aktiv
; 	01 - 8K-ROM "PaketX" E000-FFFF
; 	02 - 16K-ROM "Basic" 2000-5FFF
;	04 - System aus (RAM 0000-1FFF, CP/M-Modus, erzeugt Signal C22)
; 	x8 - 512K ROM1 aktiv, jeweils 32K-B�nke, x = 0..F Bank0..Bank15, 8000-FFFF
; 	x9 - 512K ROM2 aktiv, jeweils 32K-B�nke, x = 0..F Bank0..Bank15, 8000-FFFF

BWSPORT:	EQU	0F0H		; r�cklesbares BWS Steuerregister
;Bit 0 Taktumschaltung, 0=2Mhz, 1=4Mhz Takt
;Bit 1 BWS INVERS, 0=Hintergrund	dunkel,	1=Hintergrund hell
;Bit 2 Umschaltung FarbRAM <> BWS RAM, 0=BWS RAM, 1=FarbRAM
;Bit 3 ZG Programmierung	0=Normal BWS, 1=ZG lesen/schreiben
;Bit 4 ZG Programmierung	A0 Zeilennummer	Zeichen
;Bit 5 ZG Programmierung	A1 Zeilennummer	Zeichen
;Bit 6 ZG Programmierung	A2 Zeilennummer	Zeichen
;Bit 7 frei
; Aufbau FarbRAM
; untere 4 Bit VordergrundFarbe	Bit 0 =	Rot
;				Bit 1 =	Gr�n
;				Bit 2 =	Blau
;				Bit 3 =	Intensiv
; obere 4Bits Hintergrundfarbe	Bit 4 =	Rot
;				Bit 5 =	Gr�n
;				Bit 6 =	Blau
;				Bit 7 =	Intensiv

;
NAMELEN		equ	40		; max. L�nge des Dateinamens bei LOAD/SAVE
;KDOANF		equ	0400h		; Beginn f. Kdo-Suche
;RAMEND		equ	0BFFFh		; Ende f. Kdo-Suche, auf RAM beschr�nkt
KDOANF		equ	0200h		; Beginn f. Kdo-Suche
RAMEND		equ	0FFFFh		; Ende f. Kdo-Suche, auf RAM beschr�nkt


; Zeitkonstanten (bei 2 MHz-Takt)
ZK1    EQU  49        ;Laenge 1.Halbwelle
ZK2    EQU  46        ;Laenge 2.Halbwelle
ZKR    EQU  (ZK1+ZK2)*2/3

;------------------------------------------------------------------------------
; Reset
;------------------------------------------------------------------------------

		org	ROM
init:		di
		ld	sp, SYSSK	; Stack
		im	2
		jr	init1

		org	8
; RST 8: Eingabekanal, normal Tastatur
inch:		jp	jp_rst08	; -> rinch


init1:		ld	de, warmcod	; Warmstartcode
		jr	init2

		org	10h
; RST 10: Ausgabekanal,	normal Bildschirm
outch:		jp	jp_rst10	; -> routch

routch:		push	hl
		push	de
		jp	routch0

		org	18h
; RST 18: PRNST
prnst:		jp	jp_rst18	; -> rprnst

init5:		call	tstsch		; test auf warmstart, de = warmcod
		jr	nz, init6	; wenn nicht init, d.h. Kaltstart

		org	20h
; RST 20: Warmstart Betriebssystem
warm:		jp	jp_rst20

init2:		call	MS30		; ca. 30 ms warten
		jr	init3

		org	28h
; RST 28: 1 byte von Kassette laden, AC1-Format
loadb:		jp	jp_rst28	; -> loa15

co00:		push	hl
		push	de
		jp	co0

		org	30h
; RST 30: 1 Byte auf Kassette speichern, AC1-Format
savb:		jp	jp_rst30	; -> sav10

; Ausgabe �ber Bildschirm
co:		push	af
		jr	co00
		db 0FFh
		db 0FFh

		org	38h
; RST 38: Breakpoint, Einsprung bei Einzelschritt
sub_38:		jp	jp_rst38	; -> rst38


;------------------------------------------------------------------------------
; Kaltstart
; Sprungverteiler initialisieren, RAM l�schen
;------------------------------------------------------------------------------

init3:		ld	a, hi(inttab)
		ld	i, a
		ld	hl, TABIO	; Tabelle Peripherie
		ld	b, 11h
init4:		ld	c, (hl)		; Port
		inc	hl
		outi			; Wert ausgeben
		jr	nz, init4
		jr	init5		; test auf warmstart

;------------------------------------------------------------------------------
; Kaltstart
; Sprungverteiler initialisieren, RAM l�schen
;------------------------------------------------------------------------------
; Einsprung von init5,
; damit init6 funktioniert, muss das Adresse 220h sein
; und lo(sv_rst) = Bereichsgr��e sv_rstend-sv_rst
; in: hl = 23A, de = 181c
init6:		; Sprungverteiler initialisieren
		ld	bc, lo(sv_rst)	; == L�nge sv_rst-Bereich
		ld	l, c		; hl=220 = sv_rst
		ld	e, h		; de=1802
		ldir
		; RAM l�schen
		ex	de, hl		; hl=1822
		inc	bc		; bc=1
init7:		ld	(hl), 0FFh
		add	hl, bc
		jr	nc, init7	; bis FFFF

;------------------------------------------------------------------------------
; Test auf Autostartmodul auf 2000h
; Befindet sich dort die Bitfolge 53 43 48, erkennt der Computer ein Autostart-
; Modul und springt an die Adresse, die in 2003/2004 steht. Der Autostart kann
; verhindert werden, indem beim Einschalten bzw. RESET die Taste "CR" gedr�ckt
; wird: Sprung in den Monitor, oder Taste "X" gedr�ckt: Sprung in Programmpaket X.
;------------------------------------------------------------------------------

rWarm:		call	UPTAST		; Taste bei Einschalten gedr�ckt?
		jr	nz, rWarm1
		; ist Autostartmodul aktiv?
		call	tstsch2		; Test auf SCH auf 2000h
		jr	nz, init8	; wenn kein Autostartmodul
		jp	(hl)		; sonst dieses starten
		db 0FFh

;------------------------------------------------------------------------------
; MNI-Befehl, Programmunterbrechnung, Breakpoint
;------------------------------------------------------------------------------

		org	66h
		jp	nmi		; -> BREAK

;------------------------------------------------------------------------------
; Taste "X" gedr�ckt: Sprung in Programmpaket X.
; sonst Systemmeldung ausgeben
;------------------------------------------------------------------------------

rWarm1:		cp	'X'		; 'X' bei Einschalten gedr�ckt?
rWarm2:		jp	z, PROGX	; dann zu "Paket X"
;
init8:		;xor	a
		;out	(modul1), a	; Modul1 disablen
		call    sub_2CF
;
		rst	18h		; PRNST
		;db 0Ch,0Dh,0Fh,0Fh,"AC 1 * MONITOR * V.8.0 (C) SCCH ",0Dh,8Dh
		;db 0Ch,0Dh,0Fh,0Fh,"AC1-2010 * FARBMONITOR * FDC/RFL",0Dh,8Dh
		db 0Ch,0Dh,0Fh,0Fh,"* AC1-2010 * MONITOR * 12/2011 *",0Dh,8Dh		
		jp	break2

;------------------------------------------------------------------------------
; Tasten-Piep
;------------------------------------------------------------------------------

PIEP:		ld	(hl), c		; Tasten-Piep
		ld	bc, 0A034h
		call	UPTON		; UP "Ton", Reg, B = Tonlaenge, C = Tonhoehe
; Ende von rinch
ENDE8:		pop	bc
		pop	hl
		ret

;------------------------------------------------------------------------------
; Einsprungpunkt fuer RST 08H
; Eingabekanal auswerten
; iobyt
; Eingabe: b0 Tastatur, b1 V24 (Rs 232c), b2 Reserve, b3 User
;------------------------------------------------------------------------------

rinch:		push	hl		; UP "Eingabekanal"
		push	bc
		ld	a, (IOBYT)	; Ein/Ausgabebyte
		rrca			; b0 gesetzt?
		jr	nc, NOTAST	; nein, anderes Eingabegeraet (keine Tastatur)

; Eingabe von Tastatur		
		ld	hl, repeat	; Hilfsregister	Repetierfunktion Tastatur
		ld	b, (hl)
		ld	(hl), 0Eh
rinch1:		call	UPTAST
		jr	z, rinch2	; keine Taste gedr�ckt
		dec	b
		jr	nz, rinch1
		ld	(hl), 1
rinch2:		ld	hl, (cupos)	; Zwischenspeicher Kursor
		ld	c, (hl)		; Zeichen merken
rinch3:		ld	(hl), 7Fh	; Cursorsymbol anzeigen
rinch4:		call	UPTAST
; Taste gedr�ckt? dann Piep und ENDE8
		jr	nz, PIEP	; Tasten-Piep
; sonst kurz warten
		ld	a, 78h
rinch5:		dec	a
		jr	nz, rinch5
		djnz	rinch4
; und Cursor toggeln
		ld	a, c
		cp	(hl)		; ist Zeichen zu sehen?
		jr	z, rinch3	; dann Cursorsymbol anzeigen
		ld	(hl), c		; sonst Zeichen wieder anzeigen
		jr	rinch4

;------------------------------------------------------------------------------
; Eingabe von V24 (RS232)
;------------------------------------------------------------------------------

NOTAST:		rrca			; IOBYT b1 gesetzt?
		jr	nc, NOV24	; nein, anderes Eingabegeraet (kein V.24)

; Eingabe von V24 (RS232)
v24in0:		ld	a, (kdov24)	; Kommandocode V 24
		and	7		; Baudrate
		ld	b, a
		ld	a, 1
v24in1:		add	a, a
		djnz	v24in1
		ld	h, a		; Baudratenz�hler h=2^b
					; b=1,h=2 19200 Baud, 
					; b=2, h=4 9600 Baud
					; .. b=7, h=128 300 Baud 
		xor	a		; alles 0
		out	(PIO2DA), a	; DTR=0,CTS=0,TxD=0,RxD=0
v24in2:		in	a, (PIO2DA)	; Pegel am Datenport abfragen
		and	1		; nur RxD zulassen (Bits 7-1 ausblenden)
		jr	nz, v24in2	; Dateneingang war L warten auf H-Pegel
		ld	b, h		; Zeitkonstante nach B
v24in3:		ld	a, r		; xx Takte
		ld	a, a		; xx Takte
		djnz	v24in3		; warten
		ld	b, 10		; 10 Bits empfangen (Start - 8Bit - Stop)
v24in4:		ld	l, h		; Baudrate
		rr	c		; gelesenes CarryBit ins Bit 7 schieben
v24in5:		push	hl		; xx Takte
		pop	hl		; xx Takte
		ld	a, a		; xx Takte
		dec	l		; 
		in	a, (PIO2DA)	; Pegelabfrage
		jr	nz, v24in5
		rra			; Bit0=RxD ins Carry schieben
		djnz	v24in4		; weiter bis alle Bits durch
		ld	a, 0Ah		; DTR=1,CTS=0,TxD=1,RxD=0
		out	(PIO2DA), a	; Bereitschaftsbits setzen
		ld	a, c		; gelesenes Byte in A umladen
		jr	ENDE8		; Ende von rinch

;------------------------------------------------------------------------------
; anderes Eingabegeraet	(kein V.24)
;------------------------------------------------------------------------------

NOV24:		ld	hl, ENDE8	; Return-Adr
		push	hl		; auf Stack
		rrca			; b2 gesetzt ?
		jp	c, 0FFFFh	; Reserve --> nicht belegt --> Fehlermeldung
		jp	unk_18F0	; sonst (b3) Sprung zum UP User-Eingabe

;------------------------------------------------------------------------------
; Start vom Programmpaket X
;------------------------------------------------------------------------------

PROGX:		ld	a, 1
		out	(modul1), a	; Modul1 ROM "PaketX" aktivieren
		jp	0E000h		; und starten

;------------------------------------------------------------------------------
; ca. 30 ms warten
;------------------------------------------------------------------------------

MS30:		push	bc
		ld	bc, 903h	; Zeitkonstante	fuer 30ms, ; B = 9, C = 3
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
; kein Register wird zerst�rt
;------------------------------------------------------------------------------

INLINE:		push	af
		push	hl
		rst	18h		; PRNST
aA:		db " #",' '+80h		; Prompt
		nop
inlin1:		rst	8		; INCH
		ld	hl, (cupos)	; Zwischenspeicher Kursor
		rst	10h		; OUTCH
		cp	0Dh		; Enter?
		jr	nz, inlin1	; nein --> weiter eingeben
; Zeilenanfang ermitteln
		ld	a, '#'
inlin2:		inc	hl		; ein zeichen zur�ck
		cp	(hl)		; zeilenanfang?
		jr	nz, inlin2	; nein
		dec	hl		; das '#'
		dec	hl		; Leerzeichen davor
		ld	(soil),	hl	; erstes Zeichen der Zeile
		pop	hl
		pop	af
		ret

;------------------------------------------------------------------------------
; fuehrende Leerzeichen ueberlesen
; letzen vier Zeichen als Hexzahl konvertieren
; und in DATA ablegen
;------------------------------------------------------------------------------

konvx:		ld	a, (de)		; UP - Routine zu InHex
		cp	' '		; Leerzeichen
		dec	de
		jr	z, konvx	; �berlesen
;
		inc	de		; erstes Zeichen
		xor	a
		ld	hl, data	; Hilfsregister
		ld	(hl), a
		inc	hl
		ld	(hl), a
		inc	hl
		ld	(hl), a		; data=0
konvx2:		ld	a, (de)
		dec	hl
		dec	hl
		sub	30h ; '0'	; Zeichen<"0"?
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
		rld			; n�chste Ziffer
		jr	konvx2
		
;------------------------------------------------------------------------------
; wandelt eine maximal vierstellige in ASCII-Zeichen angegebene Zahl
; ab (DE) abw�rts in deren hexadezimalen Wert um, der dann in HL steht.
; DE wird entsprechend dekrementiert, der Akku wird zerst�rt
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
; kein Register wird zerst�rt
;------------------------------------------------------------------------------

OUTHEX:		push	af
		rra
		rra
		rra
		rra
		call	OUTHEX1		; obere Tetrade ausgeben
		pop	af		; und die unter
OUTHEX1:	push	af
		and	0Fh
		add	a, '0'		; Konvertierung --> ASCII
		cp	':'		; Ziffer "A" ... "F"?
		jr	c, OUTHEX2	; nein
		add	a, 7		; sonst Korrektur
OUTHEX2:	rst	10h		; OUTCH ; und Ausgabe
		pop	af
		ret
		
;------------------------------------------------------------------------------
; Ausgabe HL hexadezimal
; gibt das HL-Register als vierstellige Hexzahl auf dem Schirm aus,
; kein Register wird zerst�rt
;------------------------------------------------------------------------------

OUTHL:		push	af
		ld	a, h
		call	OUTHEX		; Ausgabe A hexadezimal
		ld	a, l
		call	OUTHEX		; Ausgabe A hexadezimal
		pop	af
		ret

;------------------------------------------------------------------------------
; Register im Registerrettebereich ablegen/auslesen
; (RSA = register save area)
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
		ex	af, af'
		push	hl
		push	de
		push	bc
		push	af
		jr	regh1

; Register aus Registerrettebereich holen
REGH:		ld	(data),	sp
		ld	sp, RSA
		pop	af
		pop	bc
		pop	de
		pop	hl
		exx
		ex	af, af'
		pop	af
		pop	bc
		pop	de
		pop	hl
		pop	ix
		pop	iy
regh1:		ld	sp, (data)
		ret

;------------------------------------------------------------------------------
; V24 -	Eingabe 
; (f�r externe Nutzung)
;------------------------------------------------------------------------------

V24IN:		push	hl
		push	bc
		jp	v24in0

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

kdoerr:		call	help

;Eingang Kommandomodus
GETCO1:
kdo1:		call	INLINE		; Zeile eingeben
		ld	hl, KDOANF	; erste Adr, aber der Kdo zu suchen sind
		ld	bc, RAMEND-KDOANF+1	; Anzahl (RAM-Ende-Startpos.)
kdo2:		xor	a		; 00
		cpir			; suche 00
		jp	po, kdoerr
		ld	a, 9		; 09
		cp	(hl)		; folgt 09?
		jr	nz, kdo2
		inc	hl
		ld	de, (soil)	; Zwischenspeicher "Inline"
		ld	a, (de)		; KDO
		cp	(hl)		; folgt Kommandbuchstabe?
		jr	nz, kdo2
		inc	hl
		ld	a, 0Dh		; 0d
		cp	(hl)		; folgt 0D?
		jr	nz, kdo2	; falsches Kdo
;
		ld	bc, kdo1	; Returnadr. auf Stack
		push	bc
		inc	hl
		push	hl
; Parameter
		dec	de
		call	INHEX		; 1. Argument
		ld	a, (de)
		cp	':'		; die alten Werte nehmen ?
		jr	z, kdo4
		ld	(ARG1),	hl	; Argument 1
		call	INHEX
		ld	(ARG2),	hl	; Argument 2
		call	INHEX
		ld	(ARG3),	hl	; Argument 3
kdo4:		ret

;------------------------------------------------------------------------------
; init. Sprungverteiler f�r RST-Aufrufe, NMI und Init.
; RST xx springt in den RAM. von dort geht es normalerweise zu den Monitor-
; Routinen, aber man kann auch eigene Ein-/Ausgaberoutinen nehmen
;
; bei Systemstart werden 32 Bytes nach Adresse 1802H kopiert
; Tabelle RST, NMI, Hilfsregister, V24, I/O (Voreinstellung)
;------------------------------------------------------------------------------

; damit init6 funktioniert, muss das Adresse 220h sein
; und lo(sv_rst) = Bereichsgr��e sv_rstend-sv_rst
	
		org 220h

sv_rst:		jp	rinch		; RST 08
		jp	routch		; RST 10
		jp	rprnst		; RST 18
		jp	rWarm		; RST 20 Warmstart
		jp	loa15		; RST 28 Eingabe 1 Byte A Kassette
		jp	sav10		; RST 30 Ausgabe 1 Byte A Kassette
		jp	rst38		; RST 38 error
loc_235:	jp	BREAK		; NMI: Breakpoint
		db 0FFh, 0FFh		; soil
aSch:		db "SCH"		; 181C
		db    0			; 181F FarbCode=KEIN BWS voreingestellt
		db    2			; 1820 Kommandocode V 24 (Adresse 1820H)
		db  11h			; Ein/Ausgabebyte (Adresse 1821H)
sv_rstend:

;------------------------------------------------------------------------------
; Standard-Routine rst 38
; Ausgabe Error, Register sichern, Ausgabe Break
;------------------------------------------------------------------------------

rst38:		call	error

;------------------------------------------------------------------------------
; Einsprung NMI, Register werden in die RSA gerettet, 
; I/O und PIO werden auf Standard gesetzt
;------------------------------------------------------------------------------

; Breakpoint
BREAK:		call	REGA		; Register sichern
		pop	hl		; Returnadr nach Break
		dec	hl		; ein Byte zur�ck (norm. RST 38)
		ld	(REGPC), hl	; merken
break1:		ld	(REGSP), sp
		ld	sp, SYSSK	; Stack
		ld	a, 11h		; Standard Console
		ld	(IOBYT), a	; Ein/Ausgabebyte
		rst	18h		; PRNST
aBrea:		db " BREA",'K'+80h
;
break2:		;ld	a, 0D2h		; auf Standard setzen
		ld	a, 0DAh		; auf Standard setzen
		out	(PIODB), a
		ld	hl, GETCO1	; Kommandoschleife
		push	hl       	; auf Stack
		retn			; und anspringen

;------------------------------------------------------------------------------
; Ausgabe "Error"
;------------------------------------------------------------------------------

error:		rst	18h		; PRNST
		db "  ERROR",0A0h
		ret

;------------------------------------------------------------------------------
; UP "akustisches Signal"
;------------------------------------------------------------------------------

beep:		push	bc
		ld	b, 3
beep1:		push	bc
		ld	bc, 0040h	; 
		call	UPTON		; UP "Ton", Reg, B = Tonlaenge, C = Tonhoehe
		ld	bc, 0F032h
		call	UPTON		; UP "Ton", Reg, B = Tonlaenge, C = Tonhoehe
		pop	bc
		djnz	beep1
		pop	bc
		ret

;------------------------------------------------------------------------------
; UP "Ton", Reg, B = Tonlaenge, C = Tonhoehe
;------------------------------------------------------------------------------

UPTON:		push	af
ton1:		in	a, (PIODB)
		rra
		ccf
		rla
		out	(PIODB), a
		ld	a, c
ton2:		dec	a
		jr	nz, ton2
		djnz	ton1
		pop	af
		ret
		
;------------------------------------------------------------------------------
;UP "Taste", testet Tastaturtatus, kehrt bei gedr�ckter Taste
;nach 18 ms mit dem Kode zum Akku zur�ck, keine Taste-R�ckkehr mit
;gesetztem Z-Flag und A=0, Code steht in $1822
;------------------------------------------------------------------------------

UPTAST:		push	bc
		call	tast6		; Taste gedr�ckt?
tast1:		jr	z, tast5	; nein
		ld	b, 12		; 12x Kontrollesen, Wartezeit
		ld	c, a		; Tastenwert
tast2:		call	tast6
		cp	c		; noch dieselbe Taste?
		jr	nz, tast5
		djnz	tast2
;		cp	0FFh		; in Patches 21h
		cp	21h
		jr	c, tast4	; sprung wenn a < 0ffh (21h, d.h. Steuerzeichen)
					; daher patch auf 21h, so dass f�r
					; ASCII-zeichen der Grafik-Offs. funktioniert
		in	a, (PIODB)
tast3:		bit	2, a		; Grafiktaste ?
		ld	a, c
		jr	nz, tast4	; nein
		add	a, 80h		; sonst Grafik-Offset dazu
		cp	0E0h
		jr	c, tast4
		sub	60h
tast4:		inc	b		; ??
		db  06h			; --> ld b,xx; �berspringt XOR A
tast5:		xor	a
		pop	bc
		ld	(tacod), a	; Tastencode der zuletzt gedr}ckten Taste
		ret

; Test Taste gedr�ckt?
tast6:		ld	a, 94h		; kurz warten (Entprellen)
tast7:		dec	a
		jr	nz, tast7
		in	a, (PIODA)	; Tastenwert
		bit	7, a		; Taste	gedr�ckt?
		res	7, a
		ret

;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh

;------------------------------------------------------------------------------
sub_2CF:	xor	a
		out	(modul1), a	; Modul1 disablen
		jp	loc_E81

;------------------------------------------------------------------------------
; BASIC-Start		
		db 0,9,'r',0Dh
		ld	a, 2
		out	(modul1), a
		jp	5FD5h
;
;------------------------------------------------------------------------------
; UP zu U 555-Eprommer
;------------------------------------------------------------------------------

sub_2E0:	ld	a, d
		out	(PIO2DB), a
		in	a, (PIODB)
		res	4, a
		out	(PIODB), a
		set	4, a		
		out	(PIODB), a
		ld	a, e
		out	(PIO2DB), a
		ex	(sp), hl
		ex	(sp), hl
		in	a, (PIO2DA)
		ret

		db 0FFh

;------------------------------------------------------------------------------
; Interrupt Tabelle PIO/CTC
;------------------------------------------------------------------------------

		align 2

inttab		dw 0FFFFh
inttab1		dw step			; interrupt-Routine CTC0
		dw 2000h		; interrupt-Routine CTC1
		dw 0FFFFh		; interrupt-Routine CTC2
		dw 0FFFFh		; interrupt-Routine CTC3

;------------------------------------------------------------------------------
; Bildschirmtreiber
; im Sprungverteiler wurde die HI-Adresse eingespart, deshalb m�ssen alle
; Steuerzeichenroutinen in derselben Page 03xx liegen! ( hi(cotab),lo(co_fkt) )
;
; 017FFH Bildschirmanfang (links oben !!)
; 01000H Bildschirmende (rechts unten !!)
; 32 Zeilen a 64 Zeichen
;
; Der BWS arbeitet aufgrund der Hardware invers
; d.h. inc geht ein Zeichen / eine Zeile zur�ck
;      dec geht ein Zeichen / eine Zeile vor
;------------------------------------------------------------------------------

		; org 300h
co0:		push	bc
		ld	de, (cupos)	; Zwischenspeicher Kursor
		ld	hl, poscnt	; Hilfsregister Kursorpositionierung (CTRL+N)
		cp	10h		; Steuerzeichen?
		jr	nc, co1		; Sprung, wenn kein Steuerzeichen
		ld	(hl), 1		; poscnt auf 1 setzen
		ld	bc, cotab	; Tabelle f. Steuerzeichen
		add	a, c
		ld	c, a
		ld	a, (bc)
		ld	c, a		; bc = hi(cotab),lo(co_fkt)
		push	bc		; Adresse Steuerzeichenfkt. auf Stack
		ex	de, hl		; hl=cupos, de=poscnt
		ret			; und anspringen

; einfache Zeichenausgabe
co1:		dec	(hl)		; hl=poscnt. ist ctrl-n-Mode aktiv? d.h.
					; sind noch Ziffern zu verarbeiten?
		jr	nz, co5		; poscnt > 1 -> ja, dann dort weiter
co2:		ld	(hl), 1		; sonst wieder poscnt auf 1 setzen
		;
		ex	de, hl		; hl=cupos
		cp	7Fh		; steuerzeichen 7Fh (del)
		jr	z, co8
;		cp	5Fh		; steuerzeichen 5Fh (del)
;		jr	z, co8
;		nop
;		nop
;		nop
;		nop
		cp	7Fh		; steuerzeichen 7Fh (del)
		jr	z, co8
		ld	(hl), a		; sonst Zeichen in BWS schreiben

; Kursor nach rechts
o_cur:		dec	hl		; inc. cupos
co3:		call	scroll
co4:		ld	(cupos), hl	; Zwischenspeicher Kursor
; co Ende
o_00:		pop	bc
		pop	de
		pop	hl
		pop	af
		ret

; direkte Cursorpositionierung
; in hl=poscnt Hilfsregister Kursorpositionierung (CTRL+N)
; de = bwsanf
; es folgen 4 Ziffern dezimal Zeile 00.31, Spalte 00..63
co5:		cp	'0'		; wenn keine Ziffer
		jr	c, co2		; dann Abbruch 
		jr	z, o_00		; '0' �bergehen
		cp	'9'+1		; wenn keine Ziffer
		jr	nc, co2		; dann Abbruch 
		sub	'0'
		ld	b, (hl)		; hl=poscnt
		; offs. berechnen
		; summand ermitteln
		ex	de, hl		; hl = bwsanf
		ld	de, -1
		dec	b
		jr	z, co6		; wenn Einerstelle Spalte
		ld	e, -10
		dec	b
		jr	z, co6		; wenn Zehnerstelle Spalte
		ld	e, -COLS
		dec	b
		jr	z, co6		; wenn Einerstelle Zeile
		ld	de, -COLS*10	; sonst wenn Zehner Zeile
co6:		ld	b, a		; a (=ziffer) mal
co7:		add	hl, de		; aufaddieren
		call	scroll
		djnz	co7
		jr	co4		; ende

; del 7Fh, 5Fh
co8:		inc	hl		; ein Zeichen zur�ck
		call	scroll
		ld	(hl), ' '	; Zeichen l�schen
		jr	co4

; Bildschirm l�schen
o_cls:		ld	hl, BWSANF
		ld	(cupos), hl	; Zwischenspeicher Kursor

; Bildschirm ab Kursorposition l�schen
o_clsc:		ld	(hl), ' '
		dec	hl
		ld	a, h
		cp	0Fh
		jr	nz, o_clsc
		jr	o_00

; Zeile ab Kursorposition l�schen
o_clln:		ld	(hl), ' '	; aktuelles Zeichen l�schen
co9:		dec	hl		; inc cupos
		ld	(hl), ' '	; n�chstes  Zeichen l�schen
		ld	a, COLS-1
		and	l		; Zeilenende erreicht?
		jr	nz, co9		; nein -> weiter
co10:		jr	o_00

; Kursor an den Anfang der Zeile
o_sol:		ld	a, l
		or	COLS-1		 ; setze auf Zeilenanfang
		ld	l, a
		jr	co4

; BEL, akustisches Signal
o_bell:		call	beep
		jr	o_00

; Kursor nach links
o_cul:		inc	hl		; dec cupos
		jr	co3

; Kursor nach unten
o_cud:		ld	de, -COLS
co11:		add	hl, de
		jr	co3

; Kursor nach oben
o_cuu:		ld	de, COLS
		jr	co11

; CR, Kursor an Anfang n�chster Zeile; Enter
o_cr:		ld	a, l
		and	0C0h
		ld	l, a
		jr	o_cur

; Kursor direkt positionieren
o_setc:		ld	a, 5
		ld	(de), a		; de=poscnt Hilfsregister Kursorpositionierung (CTRL+N)
		; jetzt Cursor auf BWSANF setzen (f. Berechnung in co5)

; Home, Kursor oben links
o_home:		ld	hl, BWSANF
		jr	co4

; Delete;	Zeichen	l�schen, Zeile r�ckt nach links
o_del:		ld	d, h
		ld	e, l
		dec	hl
co12:		ld	a, l
		ldd
		and	COLS-1
		jr	nz, co12
		inc	de
co13:		ld	a, ' '
		ld	(de), a
		jr	co10

; Insert;	Space einf�gen,	Zeile r�ckt nach rechts
o_ins:		ld	a, l
		and	0C0h
		ld	e, a
		ld	a, l
		ld	l, e
		ld	d, h
		inc	hl
co14:		cp	e
		jr	z, co13
		ldi
		jr	co14

; Tabelle Steuercodes, lo-teil, hi-teil ist hi(cotab) also, 03xx !
cotab:		db lo(o_00)	; 00 keine Funktion
		db lo(o_home)	; 01 Home, Kursor oben links
		db lo(o_clsc)	; 02 Bildschirm ab Kursorposition l�schen
		db lo(o_clln)	; 03 Zeile ab Kursorposition l�schen
		db lo(o_del)	; 04 Delete; Zeichen l�schen, Zeile r�ckt nach links
		db lo(o_ins)	; 05 Insert; Space einf�gen, Zeile r�ckt nach rechts
		db lo(o_sol)	; 06 Kursor an den Anfang der Zeile
		db lo(o_bell)	; 07 BEL, akustisches Signal
		db lo(o_cul)	; 08 Kursor nach links
		db lo(o_cur)	; 09 Kursor nach rechts
		db lo(o_cud)	; 0A Kursor nach unten
		db lo(o_cuu)	; 0B Kursor nach oben
		db lo(o_cls)	; 0C Bildschirm l�schen
		db lo(o_cr)	; 0D CR, Kursor an Anfang n�chster Zeile; Enter
		db lo(o_setc)	; 0E Kursor direkt positionieren
		db lo(o_tab)	; 0F Tabulator 8 Spalten

; Tabulator 8 Spalten
o_tab:		ld	de, -8
		jr	co11

; eine Zeile hochscrollen
scroll:	ld	a, h
		cp	hi(BWSANF)+1	; BWS-Ende?
		jr	c, scroll1
		ld	h, Hi(BWSEND)
scroll1:	cp	Hi(BWSEND)
		ret	nc
		ld	a, l
		push	de
		push	bc
		ld	hl, BWSANF-COLS
		ld	de, BWSANF
		ld	bc, COLS*(LINES-1)
		lddr			; scrollen
		ex	de, hl
		inc	hl
scroll2:	dec	l
		ld	(hl), ' '
		jr	nz, scroll2
		and	COLS-1
		ld	l, a
		pop	bc
		pop	de
		ret

;------------------------------------------------------------------------------
; N aaaa bbbb CRC - Pr�fsumme
;------------------------------------------------------------------------------

		db 0,9,'N',0Dh

crc:		equ	$-1		; vp: Adresse-1 ist ein Fehler
					; wird 2x angesprungen! passiert gl�cklicherweise nix...
					; da 0Dh == dec c, ohne Wirkung

n_kdo:		ld	hl, (ARG1)	; Argument 1
		ld	de, 0FFFFh
crc1:		ld	b, 80h
crc2:		sla	e
		rl	d
		sbc	a, a
		xor	(hl)
		and	b
		jr	z, crc4
		ld	a, e
crc3:		xor	21h
		ld	e, a
		ld	a, d
		xor	10h
		ld	d, a
crc4:		srl	b
		jr	nc, crc2
		ld	bc, (ARG2)	; Argument 2
		xor	a
		sbc	hl, bc
		add	hl, bc
		inc	hl
		jr	nz, crc1
		ex	de, hl
		rst	18h		; PRNST
		db " CRC",0A0h
		jp	OUTHL		; Ausgabe HL hexadezimal

;------------------------------------------------------------------------------
; F aaaa bbbb cc dd ... Finding String Hex
; F aaaa bbbb 'ABC.... Finding String ASCII
;------------------------------------------------------------------------------

		db 0,9,'F',0Dh

		rst	18h		; PRNST
		db '*'+80h
		ld	bc, (ARG1)	; Argument 1
fkdo1:		push	de
		push	hl
		ld	a, (de)
		cp	27h ; '''
		push	af
		ld	a, (bc)
		jr	z, fkdo2	; suche Ascii
; vergleiche hex-Byte
		cp	l
		jr	fkdo3
; suche Ascii
fkdo2:		dec	de
		ex	de, hl
		cp	(hl)
;
fkdo3:		jr	z, fkdo6
		pop	af
fkdo4:		inc	bc
		and	a
fkdo5:		ld	hl, (ARG2)	; Argument 2
		sbc	hl, bc
		pop	hl
		pop	de
		ret	c
		jr	fkdo1
;		
fkdo6:		pop	af
		push	bc
		jr	z, fkdo9
fkdo7:		call	INHEX		; naechstes Suchbyte holen
		inc	bc
		ld	a, (bc)
		cp	l
		jr	z, fkdo7	; solange gleich
		ld	a, (de)
		cp	' '		; letztes Suchbyte verglichen?
		jr	nz, fkdo10
fkdo8:		pop	bc
		jr	fkdo4
fkdo9:		dec	hl
		inc	bc
		ld	a, (bc)
		cp	(hl)
		jr	z, fkdo9
		ld	a, (hl)
		cp	' '
		jr	nz, fkdo8
fkdo10:		pop	hl
		call	outhlsp		; Ausgabe HL + 4 Leerzeichen
		jr	fkdo4

		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; M aaaa (bbbb) Memory
;------------------------------------------------------------------------------

		db 0,9,'M',0Dh

m_kdo:		call	para		; Argumente uebergeben
mkdo1:		ld	b, 10h
mkdo2:		push	bc
		ld	bc, 1000h
		rst	18h		; PRNST
		db 	" # ",'>'+80h
		call	OUTHL		; Ausgabe HL hexadezimal
		rst	18h		; PRNST
		db	' '+80h
mkdo3:		ld	a, (hl)
		call	OUTHEX		; Ausgabe A hexadezimal
		xor	c
		rst	18h		; PRNST
		db	' '+80h
		and	a
		sbc	hl, de
		add	hl, de
		inc	hl
		ld	c, a
		jr	z, mkdo4
		djnz	mkdo3
mkdo4:		rst	18h		; PRNST
		db 	'*',' '+80h
		call	OUTHEX		; Ausgabe A hexadezimal
		rst	18h		; PRNST
		db 	" *",0Dh+80h
		pop	bc
		ld	(ARG1),	hl	; Argument 1
		ret	z
		djnz	mkdo2
		rst	8		; INCH
		cp	0Dh
		jr	z, mkdo1
		inc	b
		cp	' '
		jr	z, mkdo2
		ret
		
;------------------------------------------------------------------------------
; Register mit Argumenten laden, aaaa=HL, bbbb=DE, cccc=BC
;------------------------------------------------------------------------------

para:		ld	hl, (ARG1)	; Argument 1
		ld	de, (ARG2)	; Argument 2
		ld	bc, (ARG3)
		ret

		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; P aaaa bbbb cc Pattern
;------------------------------------------------------------------------------

		db 0,9,'P',0Dh

		call	para		; Argumente uebergeben
		ld	(hl), c		; Pattern cc
		push	hl
		xor	a
		ex	de, hl
		sbc	hl, de
		ld	b, h
		ld	c, l
		pop	hl
		inc	de
pko1:		ldir
		ret

;------------------------------------------------------------------------------
; T aaaa bbbb cccc Transfer
;------------------------------------------------------------------------------

		db 0,9,'T',0Dh

		call	para		; Argumente uebergeben
		xor	a
		push	hl
		sbc	hl, de
		pop	hl
		jr	nc, pko1
		add	hl, bc
		ex	de, hl
		add	hl, bc
		ex	de, hl
		dec	hl
		dec	de
		lddr
		ret

;------------------------------------------------------------------------------
; A aaaa bbbb c	Arithmetik Summe, Differenz, Displ., Dezimalwert
; c ist	L�nge des Sprungbefehls	(f�r relative Sprungbefehle ist	c gleich zwei)
;------------------------------------------------------------------------------

		db 0,9,'A',0Dh

		call	para		; Argumente uebergeben
		push	hl
		push	de
		inc	hl
		inc	hl
		ex	de, hl
		xor	a
		sbc	hl, de
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
ako2:		rst	18h		; PRNST
		db 	"DSPL",':'+80h
		ld	a, l
		call	OUTHEX		; Ausgabe A hexadezimal
ako3:		pop	de
		pop	hl
		push	hl
		push	hl
		add	hl, de		; Summe
		rst	18h		; PRNST
		db 	"   SUM",':'+80h
		call	OUTHL		; Ausgabe HL hexadezimal
		pop	hl
		sbc	hl, de		; Differenz
		rst	18h		; PRNST
		db 	"   DIF",':'+80h
		call	OUTHL		; Ausgabe HL hexadezimal
		pop	bc
; Hezimalwandlung BC ->	AHL
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
ako5:		rst	18h		; PRNST
		db "   DEC",':'+80h
		ld	a, e
		and	0Fh
		or	30h ; '0'	; 5. Stelle (max 6)
		rst	10h		; OUTCH
		call	OUTHL		; Ausgabe HL hexadezimal
		rst	18h		; PRNST
		db	0Dh+80h
		ret

;------------------------------------------------------------------------------
; Registeranzeige und -Modifikation
;------------------------------------------------------------------------------

flags:		ld	c, a
		ld	hl, flagtab	; Liste	der Flag-K�rzel
		ld	b, 8		; 8 Bit
flags1:		rlc	c
		jr	nc, flags2	; wenn Flag gesetzt
		ld	a, (hl)		; dann dessen K�rzel ausgeben
		rst	10h		; OUTCH
flags2:		inc	hl
		djnz	flags1
		ret

; Ausgabe HL + 4 Leerzeichen
outhlsp:	call	OUTHL		; Ausgabe HL hexadezimal
out4sp:		rst	18h		; PRNST
		db "   ",' ' +80h
		ret

; K�rzel f. Register-Edit
regtab1:	db  'A','B','D','H'
		db  'X','Y','C','P'

; Registeranzeige. Reihenfolge der Register entspricht der RSA (absteigend)!
regtab2:	db "    "
		db "SP"
		db "PC"
		db "IY"
		db "IX"
		;
		db "MAIN"
regtab3:	db "HL"
		db "DE"
		db "BC"
		db "AF"
		;
		db "EXXR"
		db "HL"
		db "DE"
		db "BC"
		db "AF"

; Liste	der Flag-K�rzel
flagtab:	db  'S', 'Z', 0, 'H', 0, 'P', 'N', 'C'

;------------------------------------------------------------------------------
;R XX	Register Registerinhalt ver�ndern
;R:	Registerinhalte	anzeigen

;  BP:FFFF    BS:FFFFFF
;        SP:FFFF    PC:FFFF    IY:FFFF    IX:FFFF
;  MAIN  HL:FFFF    DE:FFFF    BC:FFFF    AF:FFFF
;  EXXR  HL:FFFF    DE:FFFF    BC:FFFF    AF:FFFF
;  FLAGS:  SZHPNC  (SZHPNC)
;------------------------------------------------------------------------------

		db 0,9,'R',0Dh

		cp	':'
		jr	nz, rko1
; Register anzeigen
rka1:		; BP
		rst	18h		; PRNST
		db	 "  BP",':'+80h
		ld	hl, (BPADR)
		call	outhlsp		; Ausgabe HL + 4 Leerzeichen
		; BS
		rst	18h		; PRNST
		db 	"BS",':'+80h
		ld	hl, BPOPC	; RSA: Breakpointsequenz
		ld	b, 3		; 3 Byte Code
rka2:		ld	a, (hl)
		call	OUTHEX		; Ausgabe A hexadezimal
		inc	hl
		djnz	rka2
		; 3 Zeilen a 4 Doppel-Register
		ld	c, 3		; 3 Zeilen
		ld	de, regtab2	; "    "
rka3:		ld	ix, REGSP+1	; Werte, lo-Byte von REGSP
rka4:		rst	18h		; PRNST
		db 	0Dh,' ',' '+80h
		; 4 Zeichen Zeilenbezeichnung
		ld	b, 4
rka5:		ld	a, (de)
		rst	10h		; OUTCH
		inc	de
		djnz	rka5
		rst	18h		; PRNST
		db 	' ',' '+80h
		; 4 Doppelregister
		ld	b, 4
rka6:		ld	a, (de)		; 1. Buchstabe
		rst	10h		; OUTCH
		inc	de
		ld	a, (de)		; 2. Buchstabe	
		rst	10h		; OUTCH
		inc	de
		rst	18h		; PRNST
		db 	':'+80h
rka7:		ld	h, (ix+0)	; Registerinhalte
		dec	ix
		ld	l, (ix+0)
		dec	ix
		call	outhlsp		; Ausgabe HL + 4 Leerzeichen
		djnz	rka6
		;
		dec	c
		jr	nz, rka4
		; und noch die Flags
		rst	18h		; PRNST
		db 	0Dh,"  FLAGS: ",' '+80h
		ld	a, (REGAF)
		call	flags		; Flags auswerten und anzeigen
		rst	18h		; PRNST
		db 	"  ",'('+80h
		ld	a, (REGAFX)
		call	flags		; Flags auswerten und anzeigen
		rst	18h		; PRNST
		db 	')',0Dh+80h
		ret

; Register �ndern
rko1:		ld	hl, (soil)	; HL zeigt auf Zeichen nach 'R'
		dec	hl		; 2 Zeichen vor ('R')
		dec	hl		; Leerzeichen
		ld	a, (hl)		; Registername 1. Buchstabe
		push	hl
		ld	hl, regtab1
		ld	b, 4
		ld	de, REGAF	; Registerinhalte in RSA
rko2:		cp	(hl)		; suche 'A','B','D','H'
		jr	z, rko4		; wenn gefunden
		inc	de		; RSA-Pointer erh�hen (2 Byte)
		inc	de
		inc	hl
		djnz	rko2		; weiter vergleichen
		;
		pop	bc		; bc=Registername 1. Buchstabe
		dec	bc		; 1 Zeichen vor
		ld	a, (bc)		; Registername 2. Buchstabe
		ld	b, 4
rko3:		cp	(hl)		; suche 'X','Y','C','P'
		jr	z, rko6		; wenn gefunden
		inc	de		; RSA-Pointer erh�hen (2 Byte)
		inc	de
		inc	hl
		djnz	rko3		; weiter vergleichen
		;
		jp	rka1		; Register anzeigen

		db 0FFh

; Registerwert �ndern AF..HL [']
rko4:		pop	hl
		dec	hl
		dec	hl
		ld	a, (hl)		; folgt noch ein ' ?
		ex	de, hl		; (Zweitregistersatz)
		cp	27h ; '''
		jr	nz, rko5	; nein
		ld	de, -8		; ja, dann in RSA auf 
		add	hl, de		; Zweitregistersatz gehen
rko5:		ex	de, hl
; Registerwert �ndern IX..SP
rko6:		ld	a, (de)
		ld	l, a
		inc	de
		ld	a, (de)
		ld	h, a		; HL=aktueller Registerwert
		rst	18h		; PRNST
		db 	' ',' '+80h
		call	OUTHL		; Ausgabe HL hexadezimal
;Eingabe neuer Wert
rko7:		call	INLINE		; Zeile	  eingeben
		ld	hl, (soil)	; Zwischenspeicher "Inline"
		ex	de, hl
		push	hl
		push	de
		call	INHEX		; HL=neuer Wert
		ex	de, hl
		pop	hl
		ld	a, (hl)
		pop	hl
		cp	' '		; wurde was eingegeben?
		ret	z		; wenn keine Eingabe
		ld	(hl), d		; sonst Wert
		dec	hl
		ld	(hl), e		; �bernehmen
		ret

;------------------------------------------------------------------------------
; C aaaa bbbb cccc Compare
;------------------------------------------------------------------------------

		db 0,9,'C',0Dh

		call	para		; Argumente uebergeben
cko1:		ld	a, (de)		; bbbb
		cp	(hl)		; vergleich mit aaaa
		jr	z, cko2		; weiter wenn gleich
		; Differenz anzeigen
		; "a-Adr. Wert b-Adr. Wert"
		rst	18h		; PRNST
		db 	' ',' '+80h
		call	outhlsp		; Ausgabe HL + 4 Leerzeichen aaaa
		ld	a, (hl)
		call	OUTHEX		; Ausgabe A hexadezimal Wert
		call	out4sp		; Ausgabe Leerzeichen
		call	out4sp		; Ausgabe Leerzeichen
		ex	de, hl
		call	outhlsp		; Ausgabe HL + 4 Leerzeichen bbbb
		ex	de, hl
		ld	a, (de)
		call	OUTHEX		; Ausgabe A hexadezimal  wert 
		rst	18h		; PRNST
		db 	0Dh+80h
		rst	8		; INCH
		cp	0Dh
		ret	nz
cko2:		dec	bc		; Anzahl cccc
		inc	hl		; Adressen erh�hen
		inc	de
		ld	a, b		; Anzahl cccc abgearbeitet?
		or	c
		ret	z		; dann Ende
		jr	cko1		; sonst weiter vergleichen

;------------------------------------------------------------------------------
; B aaaa Breakpoint
; Setzen eines Softwarehaltepunktes auf Adresse aaaa. 
; Hinweis: Der Breakpoint wird erst bei j_kdo geladen!
;------------------------------------------------------------------------------

		db 0,9,'B',0Dh

		ld	hl, (ARG1)	; Argument 1
bko1:		ld	(BPADR), hl
		ld	de, BPOPC	; RSA: Breakpointsequenz
		ld	bc, 3		; 3 Byte sichern
		ldir
		ld	hl, loc_743	; Breakpoint-Routine
		ld	(jp_rst38+1), hl	; Eintragen bei RST 38
		ret

;------------------------------------------------------------------------------
; G Go on
; Start eines Programms ab Adresse PC, Siehe Befehl 'J'.
;------------------------------------------------------------------------------

		db 0,9,'G',0Dh

g_kdo:		ld	hl, (REGPC)
		ld	(ARG1),	hl	; Argument 1

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

		db 0,9,'J',0Dh		

j_kdo:		ld	a, (jp_rst38+1)	; Routine bei RST 38
		cp	lo(rst38)
		jr	z, jko1
		ld	hl, (BPADR)
		ld	(hl), 0FFh	; RST 38-Befehl eintragen
		; Test auf gen�gend freien RAM ab SP
jko1:		ld	hl, (REGSP)
		ld	b, 10		; 10 Byte RAM?
		dec	hl
jko2:		ld	a, (hl)
		cpl
		ld	(hl), a
		cp	(hl)
		jr	nz, jko3
		cpl
		ld	(hl), a
		djnz	jko2
		ld	hl, (REGSP)
		db 0DDh			; nachfolgenden	Befehl �bergehen, deuten als ld	ix, xxx
jko3:		ld	hl, 1900h	; wenn kein RAM, dann Adr. 18FFh..18F0h nutzen
		ld	sp, hl
		ld	hl, (ARG1)	; Argument 1
		ld	(REGPC), hl
		push	hl
		jp	REGH		; Register aus Registerrettebereich holen

;------------------------------------------------------------------------------
; E aaaa ; E Einzelschritt
;------------------------------------------------------------------------------

		db 0,9,'E',0Dh

		ld	a, (de)
		cp	':'
		jr	z, eko2
;		
		ld	hl, (ARG1)	; Argument 1, aaaa
eko1:		ld	(REGPC), hl
eko2:		ld	sp, (REGSP)
		ld	hl, (REGPC)
		push	hl		; Adr. auf Stack
		call	REGH		; Register aus Registerrettebereich holen
		push	af
		ld	a, 87h
		out	(CTC0), a	; EI,Zeitgeber,Vorteiler 16,ZK folgt
		ld	a, 2
		out	(CTC0), a	; ZK
		ei
		ld	a, (bc)
		pop	af
		ret			; aaaa anspringen

;------------------------------------------------------------------------------
; wird bei Breakpoint aufgerufen (via RST 38)
;------------------------------------------------------------------------------

loc_743:	call	REGA		; CPU-Register ausr�umen nach RSA
		ld	hl, (loc_235)	; wieder BREAK eintragen, Std. Routine RST 38
					; vp: m�sste daher eigentlich loc_235+1 sein !!!
		ld	(jp_rst38+1), hl	; Eintragen bei RST 38
		pop	hl
		dec	hl
		ld	a, (BPOPC)	; RSA: Breakpointsequenz
		ld	(hl), a		; Code-Byte restaurieren
		ld	(REGSP), sp
		jr	eko1		; Code weiter ausf�hren

		db 0FFh
		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; Interrupt-Routine CTC Kanal CTC0
; wird bei Einzelschrittbetrieb 'E' aufgerufen, wenn der n�chste Befehl erreicht ist
;------------------------------------------------------------------------------
; z.B.
; 0637 79             HL:0000 DE:0000 BC:0000 AF:0078 F:ZH
; 0638 05             HL:0000 DE:0000 BC:FF00 AF:00BA F:SHN      
;------------------------------------------------------------------------------


step:		call	REGA		; Register sichern
		pop	de		; Adr. nach "RST 38"
		ld	(REGSP), sp
		ld	hl, (REGPC)	; voherige Adr. merken
		ld	(REGPC), de
		ld	sp, SYSSK	; Stack
		rst	18h		; PRNST
		db 	6,' '+80h	; and zeilenanfang, �berschreibt damit 
					; aktuelle Anzeige
		call	OUTHL		; Ausgabe HL hexadezimal PC aktuelle Adr.
		; max 5 Code-Bytes anzeigen
		ld	b, 5		
step1:		rst	18h		; PRNST
		db	' '+80h		; Leerzeichen
		push	hl
		xor	a
		sbc	hl, de
		pop	hl
		jr	z, step3	; wenn aktuelle Adresse erreicht
					; nichts mehr anzeigen
step2:		ld	a, (hl)		; sonst Codebyte
		inc	hl
		call	OUTHEX		; Ausgabe
		jr	step4		; weiter
step3:		ld	a, 5		; Spezialfall 5 Bytes
		cp	b
		jr	z, step2	; ja -> trotzdem 5 Bytes anzeigen
		rst	18h		; PRNST
		db ' ',' '+80h		; sonst 2 Leerzeichen statt Byte
step4:		djnz	step1
		; die 4 Doppelregister HL .. AF ausgeben
		ld	b, 4		
		ld	de, regtab3	; ab "HL"
		ld	hl, REGHL+1	; Registerinhalte, lo-Byte
step5:		rst	18h		; PRNST
		db	' '+80h
		ld	a, (de)		; Registername 1. Zeichen
		rst	10h		; OUTCH
		inc	de
		ld	a, (de)		; Registername 2. Zeichen
		rst	10h		; OUTCH
		inc	de
		rst	18h		; PRNST
		db 	':' + 80h
		ld	a, (hl)
		dec	hl
		push	hl
		ld	l, (hl)
		ld	h, a		; Registerwert
		call	OUTHL		; Ausgabe HL hexadezimal
		pop	hl
		dec	hl
		djnz	step5		; weiter
		; Flags anzeigen
		rst	18h		; PRNST
		db " F",':'+80h
		ld	a, (REGAF)
		call	flags		; Flags anzeigen
		; Zeilenende
		rst	18h		; PRNST
		db 0Dh,' '+80h
		; aktuelle Ad. in neuer Zeile anzeigen
		ld	hl, (REGPC)
		call	OUTHL		; Ausgabe HL hexadezimal
		; 5 Codebytes anzeigen
		ld	b, 5
step6:		rst	18h		; PRNST
		db	' '+80h
		ld	a, (hl)
		inc	hl
		call	OUTHEX		; Ausgabe A hexadezimal
		djnz	step6
		ld	a, 3		; DI, Reset
		out	(CTC0), a
; interaktiv
step7:		rst	10h		; OUTCH
step8:		rst	8		; INCH
		ld	hl, g_kdo
		cp	'G'		; 'G' Go?
		jr	z, stepend	; -> G
		ld	hl, eko2
		cp	0Dh		; CR?
		jr	z, stepend	; -> E, n�chster Step
		cp	' '
		jr	c, step7	; sonstige Steuerzeichen ignorieren
		;
		ld	hl, kdo1	; Kommandoschleife
		jr	step9		; Sprungverteiler �bergehen

; Einschub
;------------------------------------------------------------------------------
; Sprungverteiler
; an dieser festen Adr. zur Kompatibilit�t mit Ur-AC1
; (Ende 2K-EPROM)
;------------------------------------------------------------------------------

		org	07E8h

		jp	0FFFFh		; Reserve
		jp	MS30		; ca. 30 ms warten
j_OUTHEX	jp	OUTHEX		; Ausgabe A hexadezimal
j_OUTHL		jp	OUTHL		; Ausgabe HL hexadezimal
		jp	INLINE		; Zeile	  eingeben
		jp	INHEX
		jp	TASTE
j_GETCO1	jp	GETCO1		; Kommandoschleife

;------------------------------------------------------------------------------
; weiter mit CTC-ISR
;------------------------------------------------------------------------------

step9:		cp	'Q'		; 'Q' Quit?
		jr	nz, step10	; ja, Ende Schrittbetrieb
stepend:	push	hl
		reti
; 
step10:		cp	'B'		; 'B' Breakpoint?
		jr	nz, step11
		; Breakpoint
		rst	18h		; PRNST
		db " B",'P'+80h
		call	INLINE		; Zeile	  eingeben
		ld	hl, (soil)	; Zwischenspeicher "Inline"
		ld	a, (hl)
		cp	' '
		jr	z, step8
		ex	de, hl
		call	INHEX		; Eingabe Breakpointadr.
		call	bko1		; Breakpoint setzen
		jr	step8
;		
step11:		cp	'R'		; 'R' Register show/edit
		call	z, INLINE	; Zeile	  eingeben
		call	z, rko1
		jr	step8		; weiter in Bearbeitungsschleife

;------------------------------------------------------------------------------
; > �ndert beliebige Anzahl von Byte ab Adresse
;------------------------------------------------------------------------------

		db 0,9,'>',0Dh

gt_kdo:		ld	de, (soil)	; Zwischenspeicher "Inline"
		dec	de
		call	INHEX
		jr	nz, gtko1
		ld	a, (de)
		cp	' '
		ret	nz
gtko1:		push	hl
		call	INHEX
		jr	nz, gtko2
		ld	a, (de)
		cp	' '
		jr	nz, gtko3
gtko2:		ld	a, l
		pop	hl
		ld	(hl), a
		cp	(hl)
		inc	hl
		jr	z, gtko1
		ld	c, 0
		jr	gtko1
;
gtko3:		rst	18h		; PRNST
		db 8Bh
		cp	'*'
		ld	a, c
		ld	de, 10h
		jr	z, gtko4
		ld	e, 8
gtko4:		ld	hl, (ARG1)	; Argument 1
		pop	bc
gtko5:		add	hl, de
		and	a
		sbc	hl, bc
		add	hl, bc
		jr	c, gtko5
		dec	hl
		ld	(ARG2),	hl	; Argument 2
		ld	hl, error	; Ausgabe "Error"
		and	a
		jr	nz, gtko6
		push	hl
gtko6:		bit	3, e
		jp	z, m_kdo
		nop
		add	hl, bc
		ld	b, h
		dec	c
		call	para		; Argumente uebergeben
gtko7:		ld	b, 25
gtko8:		push	bc
		ld	bc, 801h
		rst	18h		; PRNST
		db " # ",0BEh
		call	OUTHL		; Ausgabe HL hexadezimal
		rst	18h		; PRNST
		db ' ',0A0h
		push	hl
gtko9:		ld	a, (hl)
		call	OUTHEX		; Ausgabe A hexadezimal
		rst	18h		; PRNST
		db	' '+80h
		and	a
		sbc	hl, de
		add	hl, de
		inc	hl
		jr	nz, gtko10
		dec	c
gtko10:		djnz	gtko9
		rst	18h		; PRNST
		db 0BAh
		pop	hl
		ld	b, 8
gtko11:		ld	a, (hl)
		cp	' '
		jr	c, gtko12
		cp	5Fh ; '_'
		jr	z, gtko12
		cp	7Fh ; ''
		jr	c, gtko13
gtko12:		ld	a, '.'
gtko13:		rst	10h		; OUTCH
		inc	hl
		djnz	gtko11
		rst	18h		; PRNST
		db ':',8Dh
		ld	a, c
		pop	bc
		dec	a
		ret	nz
		djnz	gtko8
		rst	8		; INCH
		cp	0Dh
		jr	z, gtko7
		inc	b
		cp	' '
		jr	z, gtko8
		ret

;------------------------------------------------------------------------------
; H I/O - Byte
;------------------------------------------------------------------------------

		db 0,9,'H',0Dh

		rst	18h		; PRNST
		db " I/O Byte",0A0h
		ld	de, IOBYT	; Ein/Ausgabebyte
		ld	a, (de)
		call	OUTHEX		; Ausgabe A hexadezimal
		inc	de
		jp	rko7

;------------------------------------------------------------------------------
; Save AC1-Kodierung
; S aadr eadr sadr name	Save	Datei auf Kassette ausgeben
; s. ac1_berlin/kassettenformate_ac1_mode.htm
;------------------------------------------------------------------------------
; File-Format: AC1-Mode, Monitor V 3.1
; 
; 512 Nullbytes zum Einschwingen
; E6h, Polarisationsbyte
; 'U', Kennbyte f�r Namen
; Namen, max. 16 Zeichen, wenn k�rzer wird mit Leerzeichen aufgef�llt
; 216 Nullbytes, Zeit f�r Namensdarstellung beim Einlesen
; es folgen Bl�cke:
; 	'<', Kennbyte f�r Datenblock
; 	XX, Blockl�nge, normal 256, Restblock entsprechend weniger
; 	XXXX, Blockstartadresse
; 	256 Datenbyte, Restblock entsprechend weniger
; 	0XX, Pr�fsumme
; 'x' Kennbyte f�r Endeblock
;  XXXX Startadresse
;------------------------------------------------------------------------------

		db 0,9,'S',0Dh

		call	q_ko21		; Start/Stop-Schaltung start
csave:		dec	de		; de = bws-pos. nach 3. Parameter
		ld	a, (de)
		cp	' '		; Leerzeichen vor Filename
		jr	z, csave	; �bergehen
;Vorton 2x256 00-Bytes ausgeben
		ld	b, 0
sav1:		xor	a
		rst	30h		; 1 Byte speichern, AC1-Format
		xor	a
		rst	30h		; 1 Byte speichern, AC1-Format
		djnz	sav1
;Synchronisationsmuster E6h
		ld	a, 0E6h		; Synchronisationsmuster
		rst	30h		; 1 Byte speichern, AC1-Format
;name ausgeben 'U' + NAMELEN Zeichen
		ld	a, 'U'		; Kennbyte fuer Namen
		rst	30h		; 1 Byte speichern, AC1-Format
		ld	b, NAMELEN
sav2:		ld	a, (de)
		rst	30h		; 1 Byte speichern, AC1-Format
		dec	de
		djnz	sav2
;216 Nullbytes,	Zeit f�r Namensdarstellung beim	Einlesen
		ld	b, 216
sav3:		xor	a
		rst	30h		; 1 Byte speichern, AC1-Format
		djnz	sav3
;		
		ld	hl, (ARG1)	; Argument 1
		ld	de, (ARG2)	; Argument 2
sav4:		push	de
		ex	de, hl
		and	a
		sbc	hl, de
		jr	c, sav8		; wenn Ende erreicht
;
; Block ausgeben: '<' + Blockl�nge + max. 256 Byte Daten + Pr�fsumme
		ld	a, '<'		; Kennung Datenblock
		inc	hl		; HL=restl. Programml�nge
		rst	30h		; 1 Byte speichern, AC1-Format
		ld	a, h
		and	a		; H = 0?
		jr	z, sav5		; wenn HL < 256
		ld	b, 0		; sonst 256 Zeichen
		jr	sav6
sav5:		ld	b, l
sav6:		ex	de, hl
		pop	de
		ld	a, b		; Blockl�nge
		rst	30h		; 1 Byte speichern, AC1-Format
		ld	a, l		; Pr�fsumme = 0, Blockadr.
		add	a, h		; zur Pr�fsumme addieren
		ld	c, a		; Pr�fsumme merken
;Blockstartadr
		call	sav9		; Ausgabe 2 Byte L,H
sav7:		ld	a, c		; Pr�fsumme erh�hen
		add	a, (hl)
		ld	c, a
		ld	a, (hl)
		rst	30h		; 1 Byte speichern, AC1-Format
		inc	hl
		djnz	sav7		; Pr�fsumme erh�hen
		ld	a, c		; Pr�fsumme ausgeben
		rst	30h		; 1 Byte speichern, AC1-Format
		jr	sav4
; Programmende 'x' + sadr
sav8:		pop	de
		ld	a, 'x'		; Kennung Ende
		rst	30h		; 1 Byte speichern, AC1-Format
		ld	hl, (ARG3)	; SADR
		call	sav9		; Ausgabe 2 Byte L,H
;		
loc_94E:	call	crc		; CRC
		jp	q_ko20		; PIO Reset, EI

; Ausgabe 2 Byte L,H
sav9:		ld	a, l
		rst	30h		; 1 Byte speichern, AC1-Format
		ld	a, h
;
; Ausgabe 1 Byte A
sav10:		push	bc
		ld	c, a		; Datenbyte nach C
		; C enthaelt die Daten und ist gleichzeitig Bitzaehler
		sls	c		; undoc opcode sll c
sav11:		in	a, (PIODB)	; PIO-Daten lesen
		set	6, a		; TB-Interface-Ausgang auf high
		jr	nc, sav12	; war Datenbit 1?
		res	6, a		; nein, TB-Interface-Ausgang low
sav12:		out	(PIODB), a	; Ausgabe 1.Halbw.
		ld	b, ZK1		; Zeitkontante 1
sav13:		djnz	sav13
		xor	40h		; Flankenwechsel
		out	(PIODB), a	; Ausgabe 2.Halbw.
		ld	b, ZK2		; Zeitkonstante 2
sav14:		djnz	sav14
		sla	c		; weitere Datenbit ausgeben
		jr	nz, sav11
		pop	bc
		ret

;------------------------------------------------------------------------------
; V = (name) Verify AC1-Kodierung
;------------------------------------------------------------------------------

		db 0,9,'V',0Dh

		sub	a		; a=0
		jr	q_ko1

;------------------------------------------------------------------------------
; Q (aaaaa) +; -; = (name) Laden Programm AC1-Kode
;------------------------------------------------------------------------------

		db 0,9,'Q',0Dh

		ld	a, (de)
		ld	bc, (ARG1)	; Argument 1, offs
		cp	'-'
		ld	a, 0FFh
		jr	nz, q_ko1
		ld	hl, 0
		sbc	hl, bc
		ld	(ARG1),	hl	; Argument 1
q_ko1:		ex	af, af'	; '
		call	q_ko21		; Start/Stop-Schaltung start
		ex	de, hl
		dec	hl
		dec	hl
q_ko2:		ld	de, (cupos)	; Zwischenspeicher Kursor
		rst	18h		; PRNST
		db  8Dh	; �
q_ko3:		ld	c, a
		ld	b, 0
q_ko4:		call	loa17
		cp	c
		jr	nz, q_ko3
		djnz	q_ko4

;Phasenkorrektur ermitteln
loa3:		call	loa17		; Bitweise lesen
		cp	19h		; Synchronisationsmuster negiert
		ld	b, 0FFh		; dann Phasenkorrektur FF
		jr	z, loa4
		cp	0E6h		; Synchronisationsmuster
		jr	nz, loa3
		ld	b, 0		; dann Phasenkorrektur FF
loa4:		ld	a, b
		ld	(phako), a	; Hilfsregister	Phasenkorrektur
		rst	28h		; 1 Byte laden
		cp	'U'		; Kennbyte fuer Namen
		jr	nz, q_ko3
;Name laden und	anzeigen
		ld	bc, NAMELEN*100h+01h
		push	hl
q_ko7:		rst	28h		; 1 Byte laden
		jr	z, q_ko9
		ld	(de), a
		dec	de
		cp	(hl)
		jr	z, q_ko8
		ld	a, ' '
		cp	(hl)
		jr	z, q_ko8
		ld	a, '"'
		cp	b
		jr	nc, q_ko8
		inc	c
q_ko8:		dec	hl
q_ko9:		djnz	q_ko7
		pop	hl
		dec	c
		jr	nz, q_ko2
q_ko10:		ld	b, 0
;
loa6:		rst	28h		; 1 Byte laden
		cp	'x'		; Kennung Ende
		jr	z, loa10	; Programmende
		cp	'<'		; Kennung Datenblock
		jr	z, q_ko12
		djnz	loa6
		jr	q_ko16
; Datenblock		
q_ko12:		rst	28h		; 1 Byte laden
		ld	b, a		; Anzahl der Bytes im Block
		call	q_ko27		; Cursor toggeln, lesen	HL
		add	a, l		; Pr�fsumme
		ld	de, (ARG1)	; Ladeadr
		add	hl, de
;		
		ld	d, a
q_ko13:		rst	28h		; 1 Byte laden
		ld	c, a
		ex	af, af'
		rrca
		jr	nc, q_ko14
		ld	(hl), c
q_ko14:		ex	af, af'
		cp	(hl)
		jr	nz, q_ko15
		inc	hl
		add	a, d
		ld	d, a
		djnz	q_ko13
		rst	28h		; 1 Byte laden
		cp	d
		jr	z, q_ko10
		rst	18h		; PRNST
		db "CR",'C'+80h
		jr	q_ko16
;		
q_ko15:		rst	18h		; PRNST
		db "RA",'M'+80h
q_ko16:		call	error		; Ausgabe "Error"
		jr	loa11
; Programmende	
loa10:		call	q_ko27		; Cursor toggeln, lesen	HL (Startadresse)
		ld	(ARG1),	hl	; als Parameter ablegen
		xor	a
loa11:		call	OUTHL		; und Anzeige
;
q_ko19:		ld	hl, (ARG1)	; Argument 1
		jp	z, loc_DF0

;
q_ko20:		in	a, (PIODB)
		ei
		jp	loc_A98		; Start/Stop-Schaltung stop

; Start/Stop-Schaltung start
q_ko21:		in	a, (PIODB)
		set	5, a		; Start/Stop-Schaltung start
		di
		jp	loc_A9A		; Beep, PIOB ausgeben

		db 0FFh
		
; Lesen ein Bit
loa17:		push	bc
		push	af
		in	a, (PIODB)	; TB-Interface-Eingang lesen
		and	80h		; Bit 7 (TB-Interface-Eingang)
		ld	c, a
loa18:		in	a, (PIODB)
		and	80h
		cp	c		; Flankenwechsel?
		jr	z, loa18	; nein
		ld	c, a		; Bit sichern
		ld	b, ZKR		; Zeitkonstante, ca 2/3 Vollwelle
loa19:		djnz	loa19		; Warten
		pop	af
		rl	c		; Carry <- Bit 7
		rla			; A <- Carry
		pop	bc
		ret

;
; Lesen	ein Byte A
loa15:		push	bc		; Laden	1 Byte
		ld	b, 8		; 8 bit
loa16:		call	loa17		; Lesen
		djnz	loa16
		ld	c, a
		ld	a, (phako)	; Phasenkorrektur
		xor	c		; ggf. Bits negieren
		pop	bc
		ret

; Cursor toggeln, Lesen 2 Byte L,H
q_ko27:		ld	hl, (cupos)	; Zwischenspeicher Kursor
		bit	1, (hl)
		ld	(hl), ' '
		jr	nz, loa12	; Lesen	  2 Byte L,H
		ld	(hl), '*'
; Lesen 2 Byte L,H
loa12:		rst	28h		; Lesen	  2 Byte L,H
		ld	l, a
		rst	28h		; 1 Byte laden
		ld	h, a
		ret

;------------------------------------------------------------------------------
; Test Autostartmodul
; und Test auf Warmstart, beides in einer Routine vermauschelt
;------------------------------------------------------------------------------

; suche "SCH" auf 2000h
tstsch2:	ld	de, 2000h

; Test auf Warmstart; hier wird Autostartadr ignoriert
; Einsprung bei Reset mit DE = warmcod
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

;------------------------------------------------------------------------------
; Standard-Routine Ausgabe Zeichenkette, RST 18
;------------------------------------------------------------------------------

rprnst:		ex	(sp), hl
		push	af
rprnst1:	ld	a, (hl)		; Zeichen holen
		inc	hl		; n�chste Adr.
		rst	10h		; OUTCH
		rlca			; Ende (Bit7 gesetzt)?
		jr	nc, rprnst1	; nein -> n�chstes Zeichen
		pop	af
		ex	(sp), hl	; neue Return-Adr.
		ret

;------------------------------------------------------------------------------
; UP's zu q_ko20 und q_ko21
;------------------------------------------------------------------------------

; Start/Stop-Schaltung stop
loc_A98:	res	5, a		; Start/Stop-Schaltung stop
loc_A9A:	rst	18h		; PRNST
		db 	07+80h		; beep
		out	(PIODB), a
		ret

;------------------------------------------------------------------------------
; I	lnitialize	RSA l�schen, SP	initialisieren
;------------------------------------------------------------------------------

		db 0,9,'I',0Dh

		ld	hl, data	; Hilfsregister	UP "INLINE"
		ld	b, tbb_eadr-data	; Bereich data..BPOPC+3
iko1:		ld	(hl), 0		;  mit 0 f�llen
		inc	hl
		djnz	iko1
		rst	18h		; PRNST
		db " SP", ' '+80h
		call	ramtst		; Ramtest
		ld	(REGSP), hl	; letzte RAM-Adr+1
		jr	oko1		; auch auf Bildschirm ausgeben

;------------------------------------------------------------------------------
; O aaaa RAM - Test
;------------------------------------------------------------------------------

		db 0,9,'O',0Dh

		ld	hl, (ARG1)	; Argument 1
		rst	18h		; PRNST
		db " ME", ' '+80h	; 'memory end'
		call	ramtst		; Ramtest
		dec	hl
oko1:		jp	OUTHL		; Ausgabe HL hexadezimal

		db 0FFh

;------------------------------------------------------------------------------
; Test auf RAM, bis RAM Ende
; in HL = startadr
;------------------------------------------------------------------------------

ramtst:		ld	a, (hl)
		cpl
		ld	(hl), a
		cp	(hl)
		ret	nz		;  Ende	wenn kein RAM mehr
		cpl
		ld	(hl), a
		inc	hl
		jr	ramtst		; Ramtest

;------------------------------------------------------------------------------
; Tabelle Peripherie Initialisierung
;------------------------------------------------------------------------------

; jeweils port, wert, s. init3
TABIO:		db CTC0
		db lo(inttab1)		; Interrupt-Vektor CTC
		db CTC1
		db  37h			; DI,Zeitgeber,Vorteiler 256,ZK folgt
		db CTC1
		db  4Eh			; Zeitkonstante
		db CTC2
		db  47h			; DI,Z�hler,Vorteiler 16,ZK folgt
		db CTC2
		db  32h			; Zeitkonstante
;orig
;		db PIO2CB
;		db 0CFh			; Mode 3 (Bit E/A)
;		db PIO2CB
;		db    0			; alle Bits Ausgabe
;		db PIO2CA
;		db 0CFh			; Mode 3 (Bit E/A)
;		db PIO2CA
;		db    5			; B3 und b0 Eingabe
;		db PIO2DA
;		db  0Ah			; Bits setzen
;mon2010c.bin, unklar!
;		db  16h
;		db  0Fh
;		db  17h
;		db  0Fh
;		db  15h
;		db    0
;		db  13h
;		db  4Fh
;		db  12h
;		db  0Fh
;mon-10122011, wieder orig
		db PIO2CB
		db 0CFh			; Mode 3 (Bit E/A)
		db PIO2CB
		db    0			; alle Bits Ausgabe
		db PIO2CA
		db 0CFh			; Mode 3 (Bit E/A)
		db PIO2CA
		db    5			; B3 und b0 Eingabe
		db PIO2DA
		db  0Ah			; Bits setzen

		db PIOCA		; PIOCA Tastatur
		db 0CFh			; Mode 3 (Bit E/A)
		db PIOCA		; PIOCA Tastatur
		db 0FFh			; alle Bits Eingabe
		db PIOCA		; PIOCA Tastatur
		db    7			; DI
		db PIOCB
		db 0CFh			; Mode 3 (Bit E/A)
		db PIOCB
		db  84h			; b7 und b2 Eingabe
		db PIOCB
		db    7			; DI
;
		db modul1
		db    2			; BASIC-ROM an

		db 0FFh

;------------------------------------------------------------------------------
; Funktion "K" --> Kassette save Turbo
; K aaaa bbbb ("name") Kassette bespielen Programm Turbo-Tape
; K aaaa bbbb * ("name") Kassette bespielen Datei Turbo-Tape
;------------------------------------------------------------------------------
; 
; Allgemeines:
; Daten in Bl�cken zu 32 Byte plus 2 Byte Pr�fsumme analog Z1013
; �bertragungsrate: 4000 Bit/s
; Block:
;	1000Hz Vorton, 2x 1-Bit, 16x0-Bit
; 	32 Byte Daten
; 	2 Byte Pr�fsumme
;
; Namensblock + 1. Datenblock: langer Vorton, sonst kurzer Vorton
; Namensblock: 
; 	32 Byte Namen: 'NNNNNNNN' + 16stelliger Name + ': X' + eadr + aadr. + 'L'
; 	"X" = Filekennung: "P" = Programm, "D" = Speicherabzug
; 	"B" = BASIC-Programm, "F" = BASIC-Feld
;------------------------------------------------------------------------------

		db 0,9,'K',0Dh

		call	tbheader	; Kdo-Parameter aufbereiten
					; ret HL = tb_blkbuf
		call	q_ko21		; Start/Stop-Schaltung start
		dec	hl		; HL = tbh_special ('L')
		ld	(tbb_eadr), hl	; Speicher Save/Load Turbo - Tape
		ld	hl, tb_kopfbuf
		ld	(tbb_aadr), hl
loc_B0F:	call	sub_BE1		; Ausgabe "Warten"
		call	tbsave
		ld	de, tbb_eadr	; Speicher Save/Load Turbo - Tape
		ld	hl, tbh_eadr
		ld	bc, 4
		ldir
		call	tbsave
		jp	loc_94E		; Ausgabe CRC, PIO Reset, EI

;------------------------------------------------------------------------------
; Funktion "Z" --> Kassette verify
; Z ("name") Verify Turbo-Tape
;------------------------------------------------------------------------------

		db 0,9,'Z',0Dh

		xor	a
		dec	a		; A=0FFh
		jr	loc_B33

;------------------------------------------------------------------------------
; Funktion "L" --> Kassette load
; L ("name" Laden Programm Turbo-Tape
; L #("name") Laden Datei Turbo-Tape
;------------------------------------------------------------------------------

		db 0,9,'L',0Dh

		add	a, d
loc_B33:	ex	af, af'	; '
		call	tbheader	; Kdo-Parameter aufbereiten
		call	q_ko21		; Start/Stop-Schaltung start
		ld	(tbb_aadr), hl	; HL = tb_blkbuf
		ld	hl, tb_blkbuf+31
		ld	(tbb_eadr), hl	; eadr => 1 Block
loc_B43:	call	sub_BE1		; Ausgabe "Warten"
; Kopfblock lesen
loc_B46:	call	tbload		; Block lesen nach HL
		; Vergleich Kopfkennung 8x'N'
		ld	hl, tb_blkbuf
		ld	de, tb_kopfbuf
		ld	b, 8
loc_B51:	ld	a, (de)
		cp	(hl)
		jr	nz, loc_B46	; kein Kopfblock -> neu einlesen
		inc	hl
		inc	de
		djnz	loc_B51
		; Vergleich Dateiname + ': ' + Dateityp
		ld	b, 13h
loc_B5B:	ld	a, (de)
		cp	(hl)
		jr	z, loc_B63	; wenn gleich
		cp	' '		; oder wenn ohne Vorgabe Dateiname
		jr	nz, loc_B85	; sonst Fehler
loc_B63:	inc	hl
		inc	de
		djnz	loc_B5B
		;
		ld	e, lo(tbb_eadr)	; hl=tbl_eadr de=tbb_eadr
		ld	c, 4		; bc=4
		ldir			; eadr+aadr nach tbb_eadr+tbb_aadr
		ld	hl, aVerify	; Text "VERIFY"
		ex	af, af'
		inc	a		; A=FF bei Verify
		jr	z, loc_B76	; Sprung, wenn Verify
		ld	l, lo(aLaden)	; sonst Text "LADEN"
loc_B76:	ex	af, af'
		call	sub_D86		; Ausgabe Text
		call	tbload		; Block lesen nach HL
		call	loc_DC9
		rst	18h		; PRNST
		db  8Dh	; �
		jp	q_ko19
; Dateiname stimmt nicht		
loc_B85:	call	loc_DC9
		jr	nz, loc_B43
		call	sub_D83
		jr	loc_B43

		db 0FFh

;------------------------------------------------------------------------------
; UP "Zeitschleife 18 ms" zur Tastaturentprellung
; ungenutzt
;------------------------------------------------------------------------------

		push	bc
		ld	bc, 0481h
		jp	ms301

;------------------------------------------------------------------------------
; UP zu Turbo Save und Load
; Kdo-Parameter aufbereiten, tb-Kopfblock f�llen
;------------------------------------------------------------------------------

tbheader:	ld	hl, (ARG2)	; Argument 2
		ld	(tbh_eadr), hl	; eadr
		ld	hl, (ARG1)	; Argument 1
		ld	(tbh_aadr), hl	; aadr
		ld	c, 'P'		; Typ Programm
tbh1:		ld	a, (de)
		cp	':'		; dieselben Parameter?
		dec	de		; Doppelpunkt
		jr	z, tbh1	; �berlesen
		cp	'*'		; Datei ?
		jr	nz, tbh2
; Datei		
		ld	a, (de)
		dec	de
		ld	c, 'D'		; Typ Datei
; Programm		
tbh2:		cp	'"'		; folgt Name?
		ld	hl, tb_kopfbuf
		ld	b, 8		; 8x'N' eintragen
tbh3:		ld	(hl), 'N'
		inc	hl
		djnz	tbh3
		ld	b, 10h		; 16 Zeichen Namen
		jr	nz, tbh5	; ?? nach djnz ist z immer 1
tbh4:		ld	a, (de)
		cp	'"'		; folgt Name?
		jr	z, tbh5	; nein
		ld	(hl), a		; sonst Name kopieren
		dec	de
		inc	hl
		djnz	tbh4		; max. 16 Zeichen
		jr	tbh6
tbh5:		ld	(hl), ' '	; kein Name oder Rest: Leerzeichen
		inc	hl
		djnz	tbh5
tbh6:		ld	(hl), ':'	; ': '+Typ
		inc	hl
		ld	(hl), ' '
		inc	hl
		ld	(hl), c
		ld	l, lo(tbh_special)	; hl=tbh_special
		ld	(hl), 'L'
		inc	hl
		ret

;------------------------------------------------------------------------------
; UP zu Kassette Load und Save Turbo
;------------------------------------------------------------------------------

; Ausgabe Warten
sub_BE1:	rst	18h		; PRNST
		db 8Dh
		ld	hl, aWarten	; "WARTEN"
; Ausgabe Status-Text HL, invers
loc_BE6:	rst	18h		; PRNST
		db 0Eh,"31",91h		; goto 31[,0]; 11h (invers)
		ld	b, 6		; 6 Zeichen
loc_BED:	ld	a, (hl)
		rst	10h		; OUTCH
		inc	hl
		djnz	loc_BED
		rst	18h		; PRNST
		db ' ',10h,0A0h		; 10h invers Ende
		ret

;------------------------------------------------------------------------------
; Kassette Save Turbo
;------------------------------------------------------------------------------

tbsave:		ld	hl, (tbb_aadr)	; Anfangsadsresse Block
		call	tbsav2		; Ausgabe Block langer Vorton
tbsav1:		ex	de, hl
		ld	hl, (tbb_eadr)	; Endadresse Save-Bereich
		and	a
		sbc	hl, de
		ex	de, hl
		ret	c		; wenn File zu Ende
		call	tbsav3		; Ausgabe ein Block
		jr	tbsav1		; weiter ausgeben

;------------------------------------------------------------------------------
; UPs zu tbsave
; siehe Routinen Monitor Z1013 / POLY-880
;------------------------------------------------------------------------------

; Ausgabe Block langer Vorton
tbsav2:		ld	de, 2000	; langer Vorton
		jr	tbsav4

; Ausgabe ein Block = 20H Bytes
tbsav3:		ld	de, 14		; kurzer Vorton
; Vorton: DE Halbschwingungen a 1000h
tbsav4:		ld	b, 48h
tbsav5:		djnz	tbsav5
		call	tbsav21		; Flanke wechseln
		dec	de
		ld	a, e
		or	d
		jr	nz, tbsav4
;Trennschwingung: 1 Vollschwingung
		ld	c, 2		; Ausgabe Synchron-
tbsav6:		ld	b, 21h		; impulse
tbsav7:		djnz	tbsav7
		call	tbsav21		; Flanke wechseln
		dec	c
		ld	e, l
		ld	d, h
		jr	nz, tbsav6
		nop
;		
		push	de		; DE=IX=0000
		pop	ix
; Kopfinhalt ausgeben
		ld	b, 8		; kurze Pause
tbsav8:		djnz	tbsav8
		call	tbsav14		; Ausgabe DE
		ld	b, 5		; kurze Pause
tbsav9:		djnz	tbsav9
; 20H Bytes ausgeben
		ld	c, 10h		;10H*2 Bytes
tbsav10:		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		add	ix, de
		inc	hl
		push	bc
		call	tbsav14		; Ausgabe DE
		pop	bc
		dec	c
		jr	z, tbsav12
		ld	b, 3
tbsav11:		djnz	tbsav11
		jr	tbsav10
; Pruefsumme ausgeben		
tbsav12:		push	ix
		pop	de		; DE = Pruefsumme
		ld	b, 7		; kurze Pause
tbsav13:		djnz	tbsav13
; Ausgabe 16 Bit DE
tbsav14:		ld	c, 16		; 16 Bit
tbsav15:		srl	d		; Hi-Bit in Cy schieben
		rr	e
		jr	nc, tbsav17	; Cy=1, wenn Bit=1
; 1-Bit 1 Halbschwingung
		ld	b, 3
tbsav16:		djnz	tbsav16
		nop
		jr	tbsav18
; 0-Bit 1 Vollschwingung		
tbsav17:		call	tbsav21		; Flanke wechseln
tbsav18:		ld	b, 0Fh
tbsav19:		djnz	tbsav19
		call	tbsav21		; Flanke wechseln
		dec	c
		ret	z
		ld	b, 0Bh
tbsav20:		djnz	tbsav20
		jr	tbsav15
; Flanke wechseln
tbsav21:		in	a, (PIODB)
		xor	40h		; TB-Interface-Ausgang
		out	(PIODB), a
		ret

;------------------------------------------------------------------------------
; Kassette Load Turbo
;------------------------------------------------------------------------------

tbload:		ld	iy, tb_errarr	; Fehler-Array
		ld	hl, (tbb_aadr)	; Ziel-Adresse Block
		exx
		ld	hl, 100h	; H=1, L=0
					; H=RAM-Fehler-Counter
		exx
tbload1:	call	tb_loa3		; Block lesen (32 Byte) nach (HL)
		exx
		jr	nz, tbload2	; Lesefehler (Pr�fsumme)
		dec	h		; Fehler-Counter > 1 ?
		ld	h, 1		; Fehler-Counter reset
		jr	z, tbload3	; nein, keine Fehler
		inc	l		; sonst Block-Fehler-Counter ++
tbload2:	ld	h, 1		; Fehler-Counter reset
		exx
		ld	(iy+0),	l	; Blockendeadresse in
		ld	(iy+1),	h	; Fehler-Array ablegen
		inc	iy		; Zeiger erh�hen
		inc	iy
		ld	a, iyl		; undoc	code!
		sub	lo(tb_errarr)+12h	; Array-Ende?
		ret	z		; Array-Ende erreicht
		exx
tbload3:	exx
		ex	de, hl
		ld	hl, (tbb_eadr)	; zu lesende Endeadresse 
		and	a		; erreicht?
		sbc	hl, de
		ex	de, hl
		ret	c		; ja
		jr	tbload1		; sonst weiterlesen

;------------------------------------------------------------------------------
;' ' aaaa       bbbb cccc       Eingabe von Argumenten
;------------------------------------------------------------------------------

		db 0,9,' ',0Dh

		ret

;------------------------------------------------------------------------------
; UP zu tbload zu Kassette Load Turbo
; siehe Monitorprogramm Z1013 / POLY-880
;------------------------------------------------------------------------------

; 20H Bytes laden nach (HL)
; ret Z=0 Ladefehler, Cy=1 Endeadresse erreicht

tb_loa3:	call	tb_loa24	; synchronisieren
		call	tb_loa25
		ld	c, 7
tb_loa5:	ld	de, 060Bh	; D=6, E=0Bh
		ld	a, 2
tb_loa6:	dec	a
		jr	nz, tb_loa6
		call	tb_loa24	; synchronisieren
tb_loa7:	call	tb_loa24	; Flanke ?
		jr	nz, tb_loa3	; wenn nicht Vorton
		dec	d
		jr	nz, tb_loa7
		dec	c
		jr	z, tb_loa9
tb_loa8:	in	a, (PIODB)
		xor	b
		bit	7, a		; TB-Interface-Eingang
		jr	nz, tb_loa5
		dec	e
		jr	nz, tb_loa8
		jr	tb_loa3
; Synchronisierimpulse lesen		
tb_loa9:	call	tb_loa25	; Flanke abwarten
		ld	a, 2Ch
tb_loa10:	dec	a
		jr	nz, tb_loa10
		call	tb_loa24	; Flanke ?
		jr	nz, tb_loa9	; wenn nicht
		call	tb_loa25	; Flanke abwarten
		ld	a, 12h
tb_loa11:	dec	a
		jr	nz, tb_loa11
; 2 Bytes Kopf lesen		
		call	tb_loa19	; lesen DE
; 20H Byte Daten lesen		
		ld	c, 10h		; 10H x 2 Bytes
		push	de
		pop	ix		; IX-Pruefsummenzaehler
		ld	a, 0Dh
tb_loa12:	dec	a
		jr	nz, tb_loa12
tb_loa13:	call	tb_loa19	; laden DE
		add	ix, de		; Pruefsumme bilden
		push	bc
		ld	c, l
		ld	b, h
		ld	hl, (tbb_eadr)	; Endadresse
		xor	a
		sbc	hl, bc		; Endadresse erreicht?
		ld	l, c
		ld	h, b
		pop	bc
		jr	c, tb_loa14	; ja --> Leseende
		;
		ex	af, af'
		jr	z, loc_D23	; bei Verify nicht speichern
		;
		ld	(hl), e		; DE auf (HL) ablegen
		inc	hl
		ld	(hl), d
		;
		dec	hl
loc_D23:	ex	af, af'
		; Verify bzw. Test auf RAM
		ld	a, e
		cp	(hl)
		inc	hl
		ld	a, d
		jr	nz, loc_D2D	; E ungleich o. kein RAM
		cp	(hl)
		jr	z, tb_loa16	; D gleich, weiter, sonst
loc_D2D:	exx
		inc	h		; Fehler-Counter
		exx
		jr	tb_loa16
		;
tb_loa14:	ld	a, 4
tb_loa15:	dec	a
		jr	nz, tb_loa15
		dec	hl
tb_loa16:	inc	hl
		ld	a, 2
tb_loa17:	dec	a
		jr	nz, tb_loa17
		dec	c
		jr	nz, tb_loa13	; naechste 2 Byte
;Pruefsumme lesen		
		call	tb_loa19	; lesen DE
		ex	de, hl
		push	ix
		pop	bc
		xor	a
		sbc	hl, bc		; Pr�fsumme gleich?
		ex	de, hl		; Z=0 Ladefehler
		ret

;------------------------------------------------------------------------------
; Funktion f�r Sprungverteiler 7E8h
;------------------------------------------------------------------------------

TASTE:		call	UPTAST
		jr	loc_DAD		; zus�tzlich Bit 7 setzen

;------------------------------------------------------------------------------
; UP zu Kassette Load Turbo
;------------------------------------------------------------------------------

; Warten auf Flankenwechsel
tb_loa25:	in	a, (PIODB)
		xor	b
		bit	7, a		; TB-Interface-Eingang
		jr	z, tb_loa25
		ret
		
;------------------------------------------------------------------------------
; UP zu Kassette Load Turbo
;------------------------------------------------------------------------------

; Portabfrage
tb_loa24:	in	a, (PIODB)
		xor	b
		bit	7, a		; TB-Interface-Eingang
		push	af
		xor	b
		ld	b, a
		pop	af		; Z=0 --> Flanke
		ret

;------------------------------------------------------------------------------
;  UP zu Kassette Load Turbo
;------------------------------------------------------------------------------

; Laden 2 Byte nach DE
tb_loa19:	push	hl
		ld	l, 10h		; 16 Datenbits
tb_loa20:	call	tb_loa24
		jr	nz, tb_loa21
		xor	a		; Cy=0
		jr	tb_loa22
tb_loa21:	scf
tb_loa22:	rr	d
		rr	e
		call	tb_loa25	; Flanke abwarten
		dec	l
		jr	z, tb_loa23	; wenn fertig
		ld	a, 12h
tb_loa26:	dec	a
		jr	nz, tb_loa26
		jr	tb_loa20
tb_loa23:	pop	hl
		ret
		
;------------------------------------------------------------------------------
;  UP zu Kassette Load Turbo
;------------------------------------------------------------------------------

sub_D83:	ld	hl, aFound	; "FOUND "
sub_D86:	call	loc_BE6		; Ausgabe Status
; sub_D89 wird extern in Turbo genutzt
; Anzeige Dateiname, aadr, eadr
sub_D89:	ld	hl, tbl_filename	; Dateiname
		ld	b, 13h
loc_D8E:	ld	a, (hl)
		rst	10h		; OUTCH
		inc	hl
		djnz	loc_D8E
		rst	18h		; PRNST
		db "   ",0A0h
		ld	hl, (tbl_aadr)	; gelesene aadr
		ld	(ARG1),	hl	; in Argument 1 sichern
		call	OUTHL		; Ausgabe HL hexadezimal
		rst	18h		; PRNST
		db ' ',0A0h
		ld	hl, (tbl_eadr)	; gelesene eadr
		ld	(ARG2),	hl	; in Argument 2 sichern
		jp	OUTHL		; Ausgabe HL hexadezimal
loc_DAD:	ret	z
		set	7, a
		ret

;------------------------------------------------------------------------------
; Turbo Meldungen
;------------------------------------------------------------------------------

aWarten:	db "WARTEN"
aVerify:	db "VERIFY"
aLaden:		db "LADEN "
aFound:		db "FOUND "

;------------------------------------------------------------------------------
; UP zu Kassette Load Turbo
;------------------------------------------------------------------------------

; Fehler
loc_DC9:	ld	de, tb_errarr	; Blockfehler-Array
		ld	a, iyl
		sub	e
		ret	z		; ret, wenn keine Eintr�ge
		; sonst Ausgabe Fehlerliste
		rst	18h		; PRNST
		db 8Dh
		call	error		; Ausgabe "Error"
		rrca
		ld	b, a
		exx
		sub	l
		exx
		jr	nz, loc_DE1
		rst	18h		; PRNST
		db "RAM",':'+80h
loc_DE1:	ld	a, (de)
		inc	de
		ld	l, a
		ld	a, (de)
		inc	de
		ld	h, a
		rst	18h		; PRNST
		db	' '+80h
		call	OUTHL		; Ausgabe HL hexadezimal
		djnz	loc_DE1
		dec	b
		ret

;------------------------------------------------------------------------------
; Autostart-Test
;------------------------------------------------------------------------------

loc_DF0:	call	q_ko20		; Start/Stop-Schaltung stopp
		ex	de, hl
		call	tstsch		; Test auf 'SCH' auf Programmanfang (DE)
		ret	nz		; kein Autostart
		jp	(hl)		; sonst starten
;

;------------------------------------------------------------------------------
; routch
; Ausgabe V24
; in hl=kdov24 Kontrollregister
;------------------------------------------------------------------------------
; PIO2 A1 Ausgang Daten TxD 
;      A2 Eingang empfangsbereit CTS 
; Taktraten gelten bei 2.4 MHz Systemtakt
;------------------------------------------------------------------------------

v24out:		push	bc
		push	af
		dec	hl		; hl=kdov24o
		ld	bc, 7FFFh	; Datenmaske 7F= 7 Bit, FF= 8 Bit
		bit	4, (hl)		; 1=7 Datenbit, + P.ungerade
		jr	nz, v24out2	; ja
		bit	5, (hl)		; 1=7 Datenbit, + P.gerade
		jr	z, v24out3	; nein -> 8 Datenbit
		ld	c, b		; ja: c=7F, sonst c=FF
v24out2:	; parity bestimmen
		and	b		; ggf. 7. Bit l�schen
		jp	pe, v24out3
		set	7, a		; Parity setzen
v24out3:	xor	c		; Maske anwenden, bits negieren
		ld	c, a		; c=auszugebendes Zeichen
		;
		ld	a, (hl)		; kdov24
		and	7		; Baudrate
		ld	b, a
		ld	a, 1
v24out4:	add	a, a
		djnz	v24out4
		ld	d, a		; Baudratenz�hler d=2^b
					; b=1,d=2 19200 Baud, 
					; b=2, d=4 9600 Baud
					; .. b=7, d=128 300 Baud 
		;
		bit	6, (hl)		; 1=3-Draht-Handshake
		jr	nz, v24out6	; dann nicht auf CTS warten
v24out5:	in	a, (PIO2DA)	; sonst X-Draht
		bit	2, a		; empfangsbereit CTS ?
		jr	nz, v24out5	; warten auf Freigabe
		;
v24out6:	ld	b, 10		; 10 Bits senden (Start - 8Bit/7Bit+P - Stop)
		bit	3, (hl)		; Stop-bit
		jr	z, v24out7	; 0=1 Stop-bit	
		inc	b		; sonst ein Stop-Bit mehr
v24out7:	scf			; Start mit 1-Bits
		;
v24out8:	ld	e, d		; Baudratenz�hler		4   Takte
		set	1, a		; b1=1				8
		jr	nc, v24out9	;				12/7
		res	1, a		; b1=0				8 
v24out9:	out	(PIO2DA), a	; A1 Ausgang Daten TxD		(11
		push	hl		; Zeitverz�gerung		11
		pop	hl		; Zeitverz�gerung		10
		ld	a, a		; Zeitverz�gerung		4
		dec	e		;				4
		jr	nz, v24out9	;				12) * E - 5
		srl	c		; c=auszugebendes Zeichen	8
					; n�chstes Bit nach Cy
		djnz	v24out8		;				13
					; insg. 251 bzw.248 Takte bei E=4 (9600 Baud)
		inc	hl
		pop	af
		pop	bc
		ret
		

; iobyt
; Ausgabe: b4 Bildschirm, b5 V24 (RS 232c), b6 Reserve, b7 User
		
routch0:	ld	hl, kdov24	; hl=Kommandocode V 24
		jp	routch1		; dann test auf Eingabeger�t

;------------------------------------------------------------------------------
; entf�llt: c Copyrightmeldung
; entf�llt: UP "Joy".Abfrage Joystick 1,keine Taste Z - Flag

; ---------------------------------------------------------------------------
; '6' Bank 1 (FDC)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'6',0Dh

		ld	a, 10h
		jr	loc_E72
; ---------------------------------------------------------------------------
; '7' Bank 2 (ROM-Bank)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'7',0Dh

		ld	a, 20h
		out	(PIO2EPROM), a
		jp	2000h

; ---------------------------------------------------------------------------
; '8' Bank 3 (frei)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'8',0Dh

		ld	a, 40h
		jr	loc_E72

; ---------------------------------------------------------------------------
; '9' Bank 4 (frei)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'9',0Dh

		ld	a, 60h
		jr	loc_E72

; ---------------------------------------------------------------------------
; '0' ROM disablen
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'0',0Dh

		ld	a, 0
loc_E72:	out	(PIO2EPROM), a
		jp	j_GETCO1

; ---------------------------------------------------------------------------
; ! cc Farbbyte setzen
; s.a. Monitorpatch 08.11.2011.LISTING

		db 0,9,'!',0Dh

		ld	a, (ARG1)	; m�gliches FarbByte laden
		or	a		; wenn 00H (keine Angabe) dann FarbByte
		jr	nz, loc_E83
;loc_E81:	ld	a, 0Fh		; bg: schwarz, fg: wei�
loc_E81:	ld	a, 0Bh		; bg: schwarz, fg: hellgelb
loc_E83:	push	af		; FarbByte sichern
		xor	a		; noFarbRAM A=00
		ld	(FARBBWS), a	; noFarbRAM einstellen
		pop	af		; FarbByte nach A zur�ck
		ld	c, BWSPORT
		in	b, (c)		; akt. Portwert einlesen
		inc	b		; FFH wenn kein r�cklesbares Register
		ret	z		; R�ckkehr weil kein BWSPort gefunden
					; wenn hier dann wurde KEIN FFH gelesen, altes B zur�ck
		dec	b		; altes B zur�ck
		ld	d, b		; in B BWS Steuercode f�r Text RAM
		set	2, d		; in D BWS Steuercode f�r FarbRAM
		ld	hl, BWSEND
		ld	e, (hl)		; altes TextByte in E merken
		out	(c), d		; FarbRAM an
		ld	(hl), a		; Farbbyte schreiben
		cp	(hl)		; und vergleichslesen
		out	(c), b		; Text RAM wieder ein (wegen RET)
		ld	(hl), e		; altes Zeichen Text-RAM zur�ck
		ret	nz		; kein phys.RAM (Text- oder FarbRAM!)
		; wenn hier dann unterschiedliche RAM's! damit gilt der FarbRAM
		; als erkannt, das 1.Byte ist bereits in Farbe
		out	(c), d		; FarbRAM an
		push	bc		; BWSPort und TextRAM ein sichern
		ld	de, BWSEND+1	; 2.Byte FarbRAM
		ld	bc, LINES*COLS-1	; restliche Bytezahl
		ldir			; nun FarbRAM schreiben
		ld	a, (hl)		; FarbByte zur�cklesen (von 17FFH)
		pop	bc		; BWSPort und TextRAM ein
		out	(c), b		; Text RAM ein
		ld	(FARBBWS), a	; FarbByte eintragen
		ret

; ---------------------------------------------------------------------------
; minimales V24 HeaderSave Load 
; "* xxxx CR" l�dt HeaderSave-File nach xxxx, "* CR" l�dt auf originale Adr.

		db 0,9,'*',0Dh

		di			; Interrupts sperren
		call	v24hs
		ei			; Interrupts freigeben
		jp	nz, error	; Abbruch bei Fehler & R�cksprung
		jp	j_OUTHL		; letzte LadeAdresse anzeigen
					; und RET von OUTHL als R�cksprung

; ---------------------------------------------------------------------------
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh


;------------------------------------------------------------------------------
;? Help
;------------------------------------------------------------------------------

		db 0,9,'?',0Dh

help:		rst	18h		; PRNST
		db "Help:",8Dh
		ld	hl, KDOANF	; Suchbereich Anfang
		ld	bc, RAMEND-KDOANF+1	; L�nge
help1:		xor	a		; suche 00
		cpir
		ret	po
		ld	a, 9		; folgt 09?
		cp	(hl)
		jr	nz, help1
		inc	hl
		ld	d, (hl)		; kdo merken
		inc	hl
		ld	a, 0Dh		; folgt 0D?
		cp	(hl)
		jr	nz, help1	; wenn Rahmen nicht gefunden
		ld	a, d		; sonst kdo anzeigen
		rst	10h		; OUTCH
		rst	18h		; PRNST
		db "  ",0A0h
		jr	help1

		db 0FFh

;------------------------------------------------------------------------------
; routch
; iobyt
; Ausgabe: b4 Bildschirm, b5 V24 (RS 232c), b6 Reserve, b7 User
;------------------------------------------------------------------------------

; zu routch
routch1:	push	af
		bit	7, (hl)		; hl=kdov24
		jr	nz, routch2
		and	7Fh
routch2:	inc	hl
; zus�tzliche Steuercodes auswerten
		cp	18h		; Drucker ein (V24)
		jr	z, routch3
		cp	19h		; Drucker aus (V24)
		jr	nz, routch4
		ld	(hl), 11h
		db	21h		; ld hl, xxx; n�chsten Befehl �berspringen
routch3:	set	5, (hl)	
		jr	routch5
;		
routch4:	bit	4, (hl)		; b4 ?
		call	nz, co		; Ausgabe �ber Bildschirm
		bit	5, (hl)		; b5 ?
		call	nz, v24out	; V24 (RS 232c)
		bit	6, (hl)		; b6 ? Reserve
		call	nz, 0FFFFh	; dort steht normalerweise FF -> break
		bit	7, (hl)		; b7 ?
		call	nz, unk_18F3	; User-Ausgaberoutine
routch5:	pop	af
		pop	de
		pop	hl
		ret

;------------------------------------------------------------------------------
; UP l�dt ein HeaderSaveFile auf ARG1, ohne ARG1 auf Originale Adresse
; R�ckkehr mit Z wenn alles OK
;------------------------------------------------------------------------------

v24hs:		ld	hl, hsbuf
		ld	de, 20h		; 32Bytes Bytes Header
		call	v24hs3		; Header laden
		ld	hl, hsbuf+13	; HL auf HeaderSave Kennung
		ld	b, 3		; 3 Kennbytes testen
		ld	a, 0D3h		; KennByte
v24hs1:		cp	(hl)		; Vergleich
		inc	hl		; n�chstes Byte
		ret	nz		; <>D3H ->kein HeaderSave = NZ
		djnz	v24hs1		; bis alle 3 Byte fertig
		ld	de, (hsbuf+0)	; AADR
		ld	hl, (hsbuf+2)	; EADR
		xor	a		; Carry 0
		sbc	hl, de		; Bytezahl in HL
		inc	hl		; Korrektur Bytezahl
		ex	de, hl		; nun HL AAdr, in DE Bytezahl
					; Zielladeadresse aus ARG1 holen 0000H �bernimmt aus Header
		ld	bc, (ARG1)	; Ladeziel
		ld	a, c
		or	b
		jr	z, v24hs2	; wenn arg1=0000H, nicht verwenden
		ld	h, b
		ld	l, c		; in HL nun Ladeziel
v24hs2:		call	v24hs3		; hier File holen
					; R�ckkehr mit Z + HL letzte Adr
		ret

; Header laden
v24hs3:		call	v24hsin		; 1 Byte lesen
		ld	(hl), a		; wegschreiben
		dec	de		; 1 Byte weniger zu senden
		ld	a, e
		or	d		; DE = 0000 ?
		ret	z		; ja, Z und HL auf letzter Adresse
		inc	hl		; n�chste Adresse
		jr	v24hs3

; V24 Laderoutine ohne Handshake ! 4800Baud
v24hsin:	push	hl
		push	de
		ld	hl, (cupos)
		ld	e, 8		; Zeitkonstante D=08=4800, 16=2400, 32=1200
					; Zeitkonstante Baudrate fest 4800Baud
v24hsin1:	in	a, (PIO2DA)	; Pegel am Datenport abfragen
		and	1		; nur RxD zulassen (Bits 7-1 ausblenden)
		jr	nz, v24hsin1	; Dateneingang war L warten auf H-Pegel
		; RxD war L = StartBit erkannt
		ld	b, e		; Zeitkonstante nach B
					; halbe Bitl�nge warten
v24hsin2:	rlc	(hl)		; 15 Takte Statusanzeige
		djnz	v24hsin2		; Zeit abwarten
		ld	b, 9		; Startbit + 8 bit
v24hsin3:	ld	d, e		; Zeitkonstante Baudrate
		rr	c		; gelesenes CarryBit ins Bit 7 schieben
					; 19 Takte OK
v24hsin4:	rlc	(hl)		; 15 Takte Statusanzeige
		nop			;  4 Takte
		dec	d		;  4 Takte Zeitz�hler decrementieren
		in	a, (PIO2DA)	; 11 Takte Pegelabfrage
		jr	nz, v24hsin4	; 7 Takte
		rra			; Bit0=RxD ins Carry schieben
		djnz	v24hsin3		; weiter bis alle Bits durch
		ld	a, c		; gelesenes Byte in A umladen
		pop	de
		pop	hl
		ret

; ---------------------------------------------------------------------------
; Kommando "p" Clear RAM
		db 0,9,'p',0Dh
; ---------------------------------------------------------------------------
		ld	hl, data
		ld	(hl), 0FFh	; 1.Byte mit FF
		ld	de, data+1
		ld	bc, 0FEFFh-data	; L�nge bis FEFF wegen DVHD
		ldir			; RAM l�schen
		rst	18h
		db " Clear RAM",8Dh
		ret
; ---------------------------------------------------------------------------
; 'W nn xx' Port nn beschreiben
		db 0,9,'W',0Dh
; ---------------------------------------------------------------------------
		call	para
		ld	b, h
		ld	c, l
		out	(c), e
		ret
; ---------------------------------------------------------------------------
; 'w nn' Port lesen		
		db 0,9,'w',0Dh
; ---------------------------------------------------------------------------
		ld	bc, (ARG1)
		in	a, (c)
		jp	j_OUTHEX
; ---------------------------------------------------------------------------
; '4' Umschaltung 2/4 Mhz
		db 0,9,'4',0Dh
; ---------------------------------------------------------------------------
		in	a, (BWSPORT)
		xor	1
		out	(BWSPORT), a
		ret
;; ---------------------------------------------------------------------------
;; 'z' Umschaltung Bildschirm-Modus
;		db 0,9,'z',0Dh
;; ---------------------------------------------------------------------------
;		in	a, (PIODB)
;		bit	3, a		; Bildschim-Mode
;		jr	z, loc_FD2
;		res	3, a
;		jr	loc_FD4
;loc_FD2:	set	3, a
;loc_FD4:	out	(PIODB), a
;		ret
;; ---------------------------------------------------------------------------
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
;		db 0FFh
	
	db "@HR ralphhaensel@gmx.de 10.12.2011"
		
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; X Exit Sprung in das Betriebssystem Programmpaket X
;------------------------------------------------------------------------------

		db 0,9,'X',0Dh

		jp	rWarm2

;------------------------------------------------------------------------------
; b Basic	Start des Basic-Interpreters V.	3.2
;------------------------------------------------------------------------------
	
		; db 0 fehlt, aber Befehle davor endet auf 00
		; da lo(rWarm2) == 0
		
		db 9,'b',0Dh
		ld	a, 2
		out	(modul1), a	; Modul1 ROM "Basic" aktivieren
		jp	5FCCh		; und starten

;------------------------------------------------------------------------------
; Arbeitszellen Monitor
;------------------------------------------------------------------------------

		org RAM

cupos:		ds 2			; Cursorposition
; Sprungverteiler f. RST-Aufrufe und NMI
; wird initialisiert mit Werten von sv_rst
jp_rst08:	ds 3			; jp	rinch
jp_rst10:	ds 3			; jp	routch
jp_rst18:	ds 3			; jp	rprnst
jp_rst20:	ds 3			; jp	rWarm	Warmstart
jp_rst28:	ds 3			; jp	loa15	Eingabe 1 Byte A Kassette
jp_rst30:	ds 3			; jp	sav10	Ausgabe 1 Byte A Kassette
jp_rst38:	ds 3			; jp	rst38	Einsprung bei Einzelschritt
nmi:		ds 3			; jp	BREAK	NMI: Breakpoint
;Hilfsregister, V24, I/O
soil:		ds 2			; Beginn Eingabezeile	, 0FFFFh
warmcod:	ds 3			; Warmstartcode		, "SCH"
FARBBWS:	ds 1			; 00=wenn KEINE Farbe, sonst akt. Farbbyte
kdov24:		ds 1			; Kommandocode V 24 in	, 2
IOBYT:		ds 1			; Ein/Ausgabebyte	; 11h
			; Eingabe: b0 Tastatur, b1 V24 (Rs 232c), b2 Reserve, b3 User
			; Ausgabe: b4 Bildschirm, b5 V24 (RS 232c), b6 Reserve, b7 User
;
tacod:		ds 1			; Tastencode der zuletzt gedr�ckten Taste
phako:		ds 1			; Hilfsregister	Einlesen, Phasenkorrektur
repeat:		ds 1			; Hilfsregister	Repetierfunktion Tastatur
poscnt:		ds 1			; Hilfsregister Kursorpositionierung (CTRL+N)
					; std. 1, nach ctrln 5 (dh 5-1 Ziffern werden erwartet)
;Stack
		ds 30h			; Stackbereich
SYSSK:		ds 2			; Stack
data:		ds 2			; Hilfsregister	UP "INLINE"
		ds 1
;Kdo-Argumente
ARG1:		ds 2			; Argument 1
ARG2:		ds 2			; Argument 2
ARG3:		ds 2			; Argument 3
;Registerrettebereich (register save area)
;Reihenfolge REGAFX..REGSP ist wichtig! Nicht ver�ndern!
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
BPADR:		ds 2			; RSA: BP
BPOPC:		ds 3			; RSA: Breakpointsequenz

;Speicher Save/Load Turbo-Tape
tbb_eadr:	ds 2			; Endadr. zum Speichern
tbb_aadr:	ds 2			; AAdr. zu speichender Block
tb_errarr:	ds 20			; Speicher f�r 10 Adressen Blockfehler
					; getestet wird auf 12h ! Das �ber-
					; schreibt dann die nachfolgenden 6x'N'
;32 Byte Buffer f�r Turbo Namenskopf
tb_kopfbuf:	ds 8			; 8x'N'
tbh_filename:	ds 16			; Dateiname
tbh_trenner:	ds 2			; Trenner ': '
tbh_filetyp:	ds 1			; Dateityp, 'D'-Datei, 'P'-Programm
tbh_eadr:	ds 2			; eadr
tbh_aadr:	ds 2			; aadr
tbh_special:	ds 1			; 'L'	; L f�r Ludwig ?
;32 Byte Kopfblockbuffer beim Lesen
tb_blkbuf:	ds 8
tbl_filename:	ds 16			; Dateiname
tbl_trenner:	ds 2			; Trenner ': '
tbl_filetyp:	ds 1			; Dateityp
tbl_eadr:	ds 2			; eadr
tbl_aadr:	ds 2			; aadr 
tbl_special:	ds 1			; Kennung

; V24-Headersave-Buffer, 32 Byte
; �berschneidet sich mit tb_errarr..tbh_trenner !!
hsbuf		equ	188Eh		; 32 Byte bis 18ADh

; es sind noch ein paar Bytes frei 18D6..18DF

		org	RAM+0F0h

unk_18F0:	ds 3		; User Eingaberoutine
unk_18F3:	ds 3		; User Ausgaberoutine

sysramend:	equ $

		end

