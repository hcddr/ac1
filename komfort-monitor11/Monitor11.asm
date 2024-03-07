; File Name   :	d:\hobby3\ac1-2010\monitor\komfort-monitor11\MonitorV11.0.BIN
; Base Address:	0000h Range: 0000h - 1000h Loaded length: 1000h


;AC1-MONITOR USB-KomfortMonitorV11.0 Stand 22.02.2015 [c]HR
;reass. Volker Pohlers, Neustadt i.H., 31.01.2023
;letzte Änderung 


		page	0
		cpu	z80undoc


mon11ver	equ	"11";	 11; 11a; 11b

;------------------------------------------------------------------------------
; Patches
;------------------------------------------------------------------------------

version		eval	"V11.0 "
p_setcol	eval	false
p_colkdo	eval	false

		if mon11ver == "11a"
;mon11a		
version		eval	"V11.0a"
p_setcol	eval	true		; Version 110a, Patch in Farb-Kommando

		elseif mon11ver == "11b"
;mon11b		
version		eval	"V11.0b"
p_setcol	eval	true		; Version 110a, Patch in Farb-Kommando
p_colkdo	eval	true

		endif

;------------------------------------------------------------------------------
; div. indiv. Einstellungen
;------------------------------------------------------------------------------

; Geometriedaten der verwendeten HD
; 128MB DOM: 500*16*32

; HR: voreingestellt ist 256MB SSD 984*16*32
; Per 09/2012 hat meine 256MB HDD folgende Aufteilung:
; 1. Spur 0           reserviert für "s/l" Kommando Monitor >= V10.2
; 2. Spur 1..9        reserviert für DiskVerHD bzw. DVHD 
; 3. Spuren 10..399   CPM Laufwerke für HRCPM und HRDOS
;    PART C, 10, 64,2048	; C: mit  64 Spuren ab Spur 10  (16 MB) HR
;    PART D,150, 64,4096	; D: mit  64 Spuren ab Spur 150 (16 MB) HR
;    PART E,300, 64,4096	; E: mit  64 Spuren ab Spur 300 (16 MB) HR
; 4. Spuren 400..     frei zu eigenen Verwendung..

vHDCyls		equ	984		; Anzahl Zylinder
vHDHeads	equ	16		; Anzahl Koepfe
vHDSectors	equ	32		; Anzahl Sektoren

vHDFirstCyl	equ	2		; Startzylinder fuer HD (Standard=2)
					; Beginn des DVHD-Bereichs

; Cursor-Symbol
vCursorSymb	equ	7Fh

; Kommandos nachladen
vKdoProc	equ	"USB"		; USB oder HD

; GSB Methode
vGSBProc	equ	"USB"		; USB oder HD

; Standard-Farben
;vColor		equ	0Fh		; bg: schwarz, fg: weiß
vColor		equ	0Bh		; bg: schwarz, fg: hellgelb

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------

; Makros
hi              function x, (x>>8) & 0ffh	; High-Byte
lo              function x, x & 0ffh		; Low-Byte
; bws(zeile 0..31, spalte 0..63) analog print_at
bwsat		function z, s, 1800h-z*64-s

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
;B7 Taste gedrückt

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

modul1		equ	14h		; Konfigurationsbyte für SCCH-Modul 1
; Port 14h, OUT-Port
; 	00 - Modul nicht aktiv
; 	01 - 8K-ROM "PaketX" E000-FFFF
; 	02 - 16K-ROM "Basic" 2000-5FFF
; 	x8 - 512K ROM1 aktiv, jeweils 32K-Bänke, x = 0..F Bank0..Bank15, 8000-FFFF
; 	x9 - 512K ROM2 aktiv, jeweils 32K-Bänke, x = 0..F Bank0..Bank15, 8000-FFFF

BWSPORT:	EQU	0F0H		; rücklesbares BWS Steuerregister
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
;				Bit 1 =	Grün
;				Bit 2 =	Blau
;				Bit 3 =	Intensiv
; obere 4Bits Hintergrundfarbe	Bit 4 =	Rot
;				Bit 5 =	Grün
;				Bit 6 =	Blau
;				Bit 7 =	Intensiv

;PIO3 f. VDIP/USB
;VDIP1 oder VDIP2 auf der Basisadresse FCh (AC1) bzw. DCh (LLC2).
PIO3A	EQU	0FCh	; Daten A (Datenport, bidirektional)
PIO3B	EQU	0FDh	; Daten B (Steuersignale, Bitbetrieb)
PIO3AS	EQU	0FEh	; Steuer A
PIO3BS	EQU	0FFh	; Steuer B

;GIDE
GIDE	equ	80h		; base address of GIDE board
IdeDOR	equ	GIDE+6		; Digital Output Register
IdeDat	equ	GIDE+8		; Data Register
IdeErr	equ	GIDE+9		; Error Register
IdeWrPc	equ	IdeErr		; Write Precomp REgister
IdeSCnt	equ	GIDE+0Ah	; Sector Count
IdeSNum	equ	GIDE+0Bh	; Sector Number
IdeCLo	equ	GIDE+0Ch	; Cylinder Low
IdeCHi	equ	GIDE+0Dh	; Cylinder High
IdeSDH	equ	GIDE+0Eh	; Drive and Head
IdeCmd	equ	GIDE+0Fh	; Command
IdeStat	equ	IdeCmd		; Status
; Commands for IDE hard disk drives:
CmdRd	equ	20h		; Read Sector
CmdWr	equ	30h		; Write Sector
CmdInit	equ	91h		; Initialize Drive Params

;
NAMELEN		equ	40		; max. Länge des Dateinamens bei LOAD/SAVE
;KDOANF		equ	0400h		; Beginn f. Kdo-Suche
;RAMEND		equ	0BFFFh		; Ende f. Kdo-Suche, auf RAM beschränkt
KDOANF		equ	00010h		; Beginn f. Kdo-Suche
KDOANFH		equ	00020h		; Beginn f. Kdo-Suche im Help-Kommando
RAMEND		equ	0FFFFh		; Ende f. Kdo-Suche, auf RAM beschränkt


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
inch:		jp	jp_rst08

init1:		ld	de, warmcod	; warmstart-Erkennung
		jr	init2

		org	10h
; RST 10: Ausgabekanal,	normal Bildschirm
outch:		jp	jp_rst10

; Leer-Kommando
		db 0,9,' ',0Dh
		ret
		org	18h
; RST 18: PRNST
prnst:		jp	jp_rst18

init2:		ld	hl, TABIO
		jr	init3

		org	20h
; RST 20: PUT
PUT:		jp	jp_rst20


; Leer-Kommando
		db 0,9,0Dh,0Dh
		ret

		org	28h
; RST 28: GET
GET:		jp	jp_rst28

routch:		push	hl
		push	de
		jp	co0


		org	30h
; RST 30: outhl
j_outhl:	jp	jp_rst30

; Ausgabe über Bildschirm
co:		push	af
		jr	routch

hd_cylinder:	dw vHDCyls-1		; Geometriedaten der verwendeten HD. Zylinder-1

		org	38h
; RST 38:		
sub_38:		jp	jp_rst38

;------------------------------------------------------------------------------
; Kaltstart
; Sprungverteiler initialisieren, RAM löschen
;------------------------------------------------------------------------------

init3:		ld	a, 2
		ld	i, a
		ld	b, 11h
init4:		ld	c, (hl)
		inc	hl
		outi
		jr	nz, init4
		call	tstsch		; Test auf Warmstart
; Sprungverteiler initialisieren
		ld	bc, 20h	; ' '   ; lo(sv_rst)      ; == Laenge sv_rst-Bereich
		ld	l, c
		ld	h, 2		; hl=220 = sv_rst
		ld	e, h		; de=1802
		ldir
		jr	z, init5	; wenn Warmstart
		call	erase		; sonst	RAM loeschen
init5:		rst	prnst		; Ausgabe String
		db  8Ch			; CLS
		call	setcolor	; Farb-RAM loeschen
		call	init_gsb
		jr	init8

		db "HR"			; ID Hänsel, Ralph

hd_heads:	db vHDHeads-1		; Geometriedaten der verwendeten HD. Koepfe -1
hd_sectors:	db vHDSectors		; Geometriedaten der verwendeten HD. Sektoren

;------------------------------------------------------------------------------
; MNI-Befehl, Programmunterbrechnung, Breakpoint
;------------------------------------------------------------------------------

		org	66h
		jp	nmi

; zu tbsave
aZ80:		db ".Z80\""

		org	006Eh	; fixe Adresse
; EINSPRUNGPUNKT Rueckkehr aus Programmpaket X
init8:		xor	a
		out	(modul1), a	; Modul1 disablen

		org	0071h	; fixe Adresse
; EINSPRUNGPUNKT Rueckkehr aus Grafik-Sound-Basic 3.2
		rst	prnst		; Ausgabe String
		db 0Ch,0Dh,0Fh,0Fh,"* AC1 USB-KomfortMonitor ", version, "*"
		db  0Dh, 8Dh
		jp	GETCO1

;------------------------------------------------------------------------------
; Tasten-Piep
;------------------------------------------------------------------------------
		
PIEP:		ld	(hl), c
		ld	bc, 0A034h
		call	UPTON
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
		ld	(hl), 18h	; Repeat-Zeit erstmalig lange
rinch1:		call	UPTAST
		jr	z, rinch2	; Taste losgelassen
		dec	b
		jr	nz, rinch1
		ld	(hl), 1		; kurze Repeatzeit für Wiederholung
rinch2:		ld	hl, (cupos)	; Zwischenspeicher Kursor
		ld	c, (hl)		; Zeichen merken
rinch3:		ld	(hl), vCursorSymb	; Cursorsymbol anzeigen
rinch4:		call	UPTAST
; Taste gedrückt? dann Piep und ENDE8
		jr	nz, PIEP	; Tasten-Piep
; sonst kurz warten		
		ld	a, 48h
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
; IOBYT Verteiler Eingabe
;------------------------------------------------------------------------------

NOTAST:		rrca			; IOBYT b1 gesetzt?
		jp	c, v24in0	; ja, -> v24
		rrca			; IOBYT b2 gesetzt?
		call	c, 0FFFFh	; ja -> Reserve
		rrca			; IOBYT b3 gesetzt?	
		call	c, unk_18F0	; ja -> USER-Eingabe
		jr	ENDE8

;------------------------------------------------------------------------------
; routch
; Ausgabe: Druckersteuerung, IO-Byte Verteiler
; Ausgabe: b4 Bildschirm, b5 V24 (RS 232c), b6 Reserve, b7 User
;------------------------------------------------------------------------------

routch0:	push	hl
		push	af
		ld	hl, kdov24
		bit	7, (hl)		; Ausgabe 8 Datenbit?
		jr	nz, routch2	; nein
		and	7Fh		; dann Bit 7 rücksetzen
routch2:	inc	hl		; HL=IOBYT
		;
		cp	18h		; Drucker ein, V 24-Schnittstelle
		jr	z, routch3
		cp	19h		; Ein-/Ausgabe normal, Drucker aus
		jr	nz, routch4
; Drucker aus		
		ld	(hl), 11h	; setze Std-I/O-Byte
		db  21h			; LD HL,xxx; überspringe nachfolgenden Befehl
; Drucker ein
routch3:	set	5, (hl)		; setze V 24 - Ausgabe
		jr	routch5
		
; IOBYT	Verteiler Ausgabe
routch4:	bit	4, (hl)
		call	nz, co		; IOBYT B4 -> BWS
		bit	5, (hl)
		call	nz, v24out	; IOBYT B5 -> V 24 - Ausgabe
		bit	6, (hl)
		call	nz, 0FFFFh	; IOBYT B6 -> Reserve
		bit	7, (hl)
		call	nz, unk_18F3	; IOBYT B7 -> User-Ausgabe
routch5:	pop	af
		pop	hl
		ret

;------------------------------------------------------------------------------
; Funktion für Sprungverteiler 7E8h
;------------------------------------------------------------------------------

TASTE:		call	UPTAST
		ret	z
		set	7, a
		ret

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
; kein Register wird zerstört
;------------------------------------------------------------------------------

INLINE:		push	hl
		push	af
		rst	prnst		; Ausgabe String
aA:		db " #",' '+80h		; Prompt
		nop
inlin1:		rst	inch		; inch; Zeicheneingabe
		ld	hl, (cupos)	; Zwischenspeicher Kursor
		rst	outch		; Zeichenausgabe
		cp	0Dh		; Enter?
		jr	nz, inlin1	; nein --> weiter eingeben
; Zeilenanfang ermitteln
		ld	a, '#'
inlin2:		inc	hl		; ein zeichen zurück
		cp	(hl)		; zeilenanfang?
		jr	nz, inlin2	; nein
		dec	hl		; das '#'
		dec	hl		; Leerzeichen davor
		ld	(soil),	hl	; erstes Zeichen der Zeile
		pop	af
		pop	hl
		ret

;------------------------------------------------------------------------------
; fuehrende Leerzeichen ueberlesen
; letzen vier Zeichen als Hexzahl konvertieren
; und in DATA ablegen
;------------------------------------------------------------------------------

konvx:		ld	a, (de)		; UP - Routine zu InHex
		cp	' '		; Leerzeichen
		dec	de
		jr	z, konvx	; überlesen
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
		rld			; nächste Ziffer
		jr	konvx2

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
		add	a, '0'		; Konvertierung --> ASCII
		cp	'9'+1		; Ziffer "A" ... "F"?
		jr	c, OUTHEX2	; nein
		add	a, 7		; sonst Korrektur
OUTHEX2:	rst	outch		; OUTCH ; und Ausgabe
		pop	af
		ret

;------------------------------------------------------------------------------
; Ausgabe HL hexadezimal
; gibt das HL-Register als vierstellige Hexzahl auf dem Schirm aus,
; kein Register wird zerstört
;------------------------------------------------------------------------------

OUTHL:		push	af
		ld	a, h
		call	OUTHEX		; Ausgabe A hexadezimal
		ld	a, l
		call	OUTHEX		; Ausgabe A hexadezimal
		pop	af
		ret

;------------------------------------------------------------------------------
; Up zu xxx
;------------------------------------------------------------------------------

sub_1A5:	push	hl
		ld	hl, (soil)
		ld	a, l
		or	h
		pop	hl
		ld	a, (hs_filetyp)	; Basic-Mode
		ret

		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; Kommandos nachladen
; Dateiname f. Nachladbare Kommandos
; MO11A_46.Z80
;   | | ^^--------- ASCII-Code des Kommandobuchstabens
;   | +------------ Gerätekenner (A=AC1, L=LLC2)
;   +-------------- Monitorversion
;------------------------------------------------------------------------------

kdo5:		ld	hl, aFNERW	; Dateiname f. Nachladbare Kommandos
		ld	de, filenambuf	; in hsbuf kopieren
		ld	bc, 14		; Länge Dateiname (mit "")
		ldir
		ld	(ARG1),	bc	; 0
		;
		ld	hl, (soil)
		ld	a, (hl)		; Kommandobuchstabe
		call	tohex		; als Hex-Ascii
		ld	(filenambuf+5), hl	; in Dateiname eintragen
		call	outchoff	; outch deaktivieren
		ld	a, 'P'		; Typ Programm
		if vKdoProc == "USB"
		call	ld_usb		; Datei laden, Nachladen via USB 
		else
		call	ld_hd		; Datei laden, Nachladen via HD (lt. Doku 9C4H, das ist falsch!)
		endif
		call	outchon		; outch aktivieren
		jr	z, kdo1a	; wenn geladen, kdo im RAM suchen und ausführen
		rst	prnst		; Ausgabe String
		db ' ','?'+80h		; Fehler (Kommando-Datei nicht geladen)

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
; Kommandorahmen: 00,09,Kdo-Zeichen,0D
;------------------------------------------------------------------------------
;Eingang Kommandomodus

kdo1:		call	INLINE		; Zeile eingeben
kdo1a:		ld	hl, KDOANF	; erste Adr, aber der Kdo zu suchen sind
		ld	bc, RAMEND-KDOANF+1	; Anzahl (RAM-Ende-Startpos.)	; 0FFF0h
kdo2:		xor	a		; 00
		cpir			; suche 00
		jp	po, kdo5	; Kommando nachladen
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
; init. Sprungverteiler für RST-Aufrufe, NMI und Init.
; RST xx springt in den RAM. von dort geht es normalerweise zu den Monitor-
; Routinen, aber man kann auch eigene Ein-/Ausgaberoutinen nehmen
;
; bei Systemstart werden 32 Bytes nach Adresse 1802H kopiert
; Tabelle RST, NMI, Hilfsregister, V24, I/O (Voreinstellung)
;------------------------------------------------------------------------------

; damit init4 funktioniert, muss das Adresse 220h sein
; und lo(sv_rst) = Bereichsgröße sv_rstend-sv_rst
	
		org 220h

sv_rst:		jp	rinch		; rst 08
		jp	routch0		; rst 10
		jp	rprnst		; rst 18
		jp	rPUT		; rst 20
		jp	rGET		; rst 28
		jp	OUTHL		; rst 30
		jp	BREAK		; rst 38
		jp	BREAK		; NMI: Breakpoint
igsbslm:	if vGSBProc == "USB"
		db 0			; Voreinstellung Methode GSB SAVE/Load USB HD 00h 20h
		else
		db 20h
		endif
hd_buf_hi:	db 0FDh			; Voreinstellung HighByte HD-Puffer FD (AC1) FE	(LLC2)
aSch:		db "SCH"		; 181C Autostart-Kennung
		db  0			; 181F FARBBWS init
		db  42h			; 1820 Kommandocode V 24 (Adresse 1820H)
		db  11h			; Ein/Ausgabebyte (Adresse 1821H)

;------------------------------------------------------------------------------
; Einsprung NMI, Register werden in die RSA gerettet, 
; I/O und PIO werden auf Standard gesetzt
;------------------------------------------------------------------------------

; Breakpoint
BREAK:		nop			; ehem CALL REGA
		nop
		nop
break1:		pop	hl		; Returnadr nach Break
		dec	hl		; ein Byte zurück (norm. RST 38)
		ld	(REGPC), hl	; merken
		ld	(REGSP), sp
		ld	sp, SYSSK	; Stack
		ld	a, 11h		; Standard Console
		ld	(IOBYT), a	; Ein/Ausgabebyte
		rst	prnst		; Ausgabe String
		db " BREA",'K'+80h
		
;------------------------------------------------------------------------------
; Rückkehr in Monitor, nach NMI
;------------------------------------------------------------------------------

GETCO1:
break2:		ld	sp, SYSSK	; Stack init
		ld	a, 0DAh		; Standard setzen für
		out	(PIODB), a	; Grafik/Ton
		ld	hl, kdo1	; Kommandoschleife
		push	hl		; auf Stack
		retn			; und anspringen

;------------------------------------------------------------------------------
; Ausgabe "Error"
;------------------------------------------------------------------------------

error:		rst	prnst		; Ausgabe String
		db " ERROR "
		db  87h			; Beep
		ret

;------------------------------------------------------------------------------
; UP "akustisches Signal"
;------------------------------------------------------------------------------

beep:		push	bc
		ld	b, 2
beep1:		push	bc
		ld	bc, 00040h
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
ton1:		in	a, (PIODB)	; Grafik/Ton
		rra
		ccf
		rla
		out	(PIODB), a	; Grafik/Ton
		ld	a, c
ton2:		dec	a
		jr	nz, ton2
		djnz	ton1
		pop	af
		ret

;------------------------------------------------------------------------------
;UP "Taste", testet Tastaturtatus, kehrt bei gedrückter Taste
;nach 18 ms mit dem Kode zum Akku zurück, keine Taste-Rückkehr mit
;gesetztem Z-Flag und A=0, Code steht in $1822
;------------------------------------------------------------------------------

UPTAST:		push	bc
		call	tast6		; Taste gedrückt?
tast1:		jr	z, tast5	; nein
		ld	b, 12		; 12x Kontrollesen, Wartezeit
		ld	c, a		; Tastenwert
tast2:		call	tast6
		cp	c		; noch dieselbe Taste?
		jr	nz, tast5
		djnz	tast2
		cp	21h		; Steuerzeichen ?
		jr	c, tast4	; sprung wenn a < 21h, d.h. Steuerzeichen
					; daher patch auf 21h, so dass für
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
		db  06h			; --> ld b,xx; überspringt XOR A
tast5:		xor	a
		pop	bc
		ld	(tacod), a	; Tastencode der zuletzt gedr}ckten Taste
		ret

; Test Taste gedrückt?
tast6:		ld	a, 48h		; kurz warten (Entprellen)
tast7:		dec	a
		jr	nz, tast7
		in	a, (PIODA)	; Tastenwert
		bit	7, a		; Taste	gedrückt?
		res	7, a
		ret

;------------------------------------------------------------------------------
; rst20 PUT
; Ausgabe zu VDIP1 senden:
; PE:	A	Datenbyte
; VR:	-
;------------------------------------------------------------------------------

rPUT:		out	(PIO3A), a	; Daten	ausgeben
		push	af
rput1:		in	a, (PIO3B)	; Status abfragen
		rrca
		rrca
		jr	c, rput1	; nicht	bereit,	warten!
		ld	a, 0CCh		; WR aktiv
		out	(PIO3B), a
		ld	a, 0C4h		; WR inaktiv
		out	(PIO3B), a
		pop	af
		ret

;------------------------------------------------------------------------------
; Dateiname f. Nachladbare Kommandos
; MO11A_46.Z80
;   | | ^^--------- ASCII-Code des Kommandobuchstabens
;   | +------------ Gerätekenner (A=AC1, L=LLC2)
;   +-------------- Monitorversion
;------------------------------------------------------------------------------

; zu kdo5
aFNERW:		db "\"08Z.xx_A11OM\""	; -> MO11A_xx.Z80

;------------------------------------------------------------------------------
; Zeichensatz umschalten
;------------------------------------------------------------------------------

o_ZS:		in	a, (PIODB); Grafik/Ton
		xor	8
		out	(PIODB), a; Grafik/Ton
		jr	o_00

;------------------------------------------------------------------------------
; Interrupt-Tabelle CTC
;------------------------------------------------------------------------------

inttab:		align 2

		dw 0FFFFh		; interrupt-Routine CTC0 
		dw 0FFFFh		; interrupt-Routine CTC1
		dw 0FFFFh		; interrupt-Routine CTC2
		dw 0FFFFh		; interrupt-Routine CTC3

;------------------------------------------------------------------------------
; Bildschirmtreiber
; im Sprungverteiler wurde die HI-Adresse eingespart, deshalb müssen alle
; Steuerzeichenroutinen in derselben Page 03xx liegen! ( hi(cotab),lo(co_fkt) )
;
; 017FFH Bildschirmanfang (links oben !!)
; 01000H Bildschirmende (rechts unten !!)
; 32 Zeilen a 64 Zeichen
;
; Der BWS arbeitet aufgrund der Hardware invers
; d.h. inc geht ein Zeichen / eine Zeile zurück
;      dec geht ein Zeichen / eine Zeile vor
;------------------------------------------------------------------------------
;
;
;  bwsat(0,0)                                       bwsat(0,63)
;  +--------------------------------------------------------+
;  ¦17FF BSWANF                ...                     17C0 ¦
;  ¦                                                        ¦
;  ¦                     32 Zeilen                          ¦
;  ¦                     64 Spalten                         ¦
;  ¦                                                        ¦
;  ¦                    <-- inc cupos                       ¦
;  ¦                    --> dec cupos                       ¦
;  ¦                                                    .   ¦
;  ¦ .                                                  .   ¦
;  ¦ .                                                  .   ¦
;  ¦ .                                                      ¦
;  ¦                                                        ¦
;  ¦10BF                       ...                     1080 ¦
;  ¦107F                       ...                     1040 ¦
;  ¦103F                       ...              BWSEND 1000 ¦
;  +--------------------------------------------------------+ 
;  bwsat(31,0)                                      bwsat(31,63)
;
;
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
		cp	1Ah		; steuerzeichen 1Ah (Zeichensatz umschalten)
		jr	z, o_ZS
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
		jr	z, o_00		; '0' übergehen
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
co8:		inc	hl		; ein Zeichen zurück
		call	scroll
		ld	(hl), ' '	; Zeichen löschen
		jr	co4

; Bildschirm löschen
o_cls:		ld	hl, BWSANF
		ld	(cupos), hl	; Zwischenspeicher Kursor

; Bildschirm ab Kursorposition löschen
o_clsc:		ld	(hl), ' '	; mil Leerzeichen füllen
		dec	hl		; nächstes Zeichen
		ld	a, h
		cp	hi(BWS-1)	; BWS überschritten ?
		jr	nz, o_clsc	; weiter bis BWS-Ende
		jr	o_00

; Zeile ab Kursorposition löschen
o_clln:		ld	(hl), ' '	; aktuelles Zeichen löschen
co9:		dec	hl		; inc cupos
		ld	(hl), ' '	; nächstes  Zeichen löschen
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

; CR, Kursor an Anfang nächster Zeile; Enter
o_cr:		ld	a, l
		and	0C0h		; Pos=Zeilenende
		ld	l, a
		jr	o_cur

; Kursor direkt positionieren
o_setc:		ld	a, 5
		ld	(de), a		; de=poscnt Hilfsregister Kursorpositionierung (CTRL+N)
		; jetzt Cursor auf BWSANF setzen (f. Berechnung in co5)

; Home, Kursor oben links
o_home:		ld	hl, BWSANF
		jr	co4

; Delete;	Zeichen	löschen, Zeile rückt nach links
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

; Insert;	Space einfügen,	Zeile rückt nach rechts
o_ins:		ld	a, l
		and	0C0h		; Pos=Zeilenende
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
		db lo(o_clsc)	; 02 Bildschirm ab Kursorposition löschen
		db lo(o_clln)	; 03 Zeile ab Kursorposition löschen
		db lo(o_del)	; 04 Delete; Zeichen löschen, Zeile rückt nach links
		db lo(o_ins)	; 05 Insert; Space einfügen, Zeile rückt nach rechts
		db lo(o_sol)	; 06 Kursor an den Anfang der Zeile
		db lo(o_bell)	; 07 BEL, akustisches Signal
		db lo(o_cul)	; 08 Kursor nach links
		db lo(o_cur)	; 09 Kursor nach rechts
		db lo(o_cud)	; 0A Kursor nach unten
		db lo(o_cuu)	; 0B Kursor nach oben
		db lo(o_cls)	; 0C Bildschirm löschen
		db lo(o_cr)	; 0D CR, Kursor an Anfang nächster Zeile; Enter
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
; N aaaa bbbb CRC - Prüfsumme
; schnelle 16-Bit-SDLC-Routine
;------------------------------------------------------------------------------

		db 0,9,'N',0Dh

crc:		call	para
		ex	de, hl
		inc	hl
		xor	a
		sbc	hl, de
		ld	bc, 0FFFFh
crc1:		ld	a, (de)
		xor	b
		ld	b, a
		rrca
		rrca
		rrca
		rrca
		and	0Fh
		xor	b
		ld	b, a
		rrca
		rrca
		rrca
		push	af
		and	1Fh
		xor	c
		ld	c, a
		pop	af
		push	af
		rrca
		and	0F0h
		xor	c
		ld	c, a
		pop	af
		and	0E0h
		xor	b
		ld	b, c
		ld	c, a
		inc	de
		dec	hl
		ld	a, h
		or	l
		jr	nz, crc1
		push	bc
		rst	prnst		; Ausgabe String
		db " CRC",0A0h
		pop	hl
		rst	30h		; outhl
		ret

;------------------------------------------------------------------------------
; UP zu	Dump
; Ausgabe einer Zeile
; "            2008>  FF FF FF FF FF FF FF FF  * ........ *"
;------------------------------------------------------------------------------

dmp_zeile:	ld	b, c		; c = Nr. aktives Byte
		inc	hl
dmp_zeile1:	dec	hl
		djnz	dmp_zeile1
		ld	(ARG2),	hl
		rst	prnst		; Ausgabe String
		db    6
		db  83h			; Zeilenanfang,zeile loeschen
		;12x Kursor nach rechts
		ld	b, 12
dmp_zeile2:	rst	prnst		; Ausgabe String
		db  89h			; Kursor nach rechts
		djnz	dmp_zeile2
		;Adresse
		rst	30h		; outhl
		ld	de, (cupos)
		;>
		rst	prnst		; Ausgabe String
		db '>',' '+80h
		;8 Bytes headezimal
		ld	b, 8
dmp_zeile3:	ld	a, (hl)
		inc	hl
		rst	prnst		; Ausgabe String
		db ' '+80h
		call	j_OUTHEX
		djnz	dmp_zeile3
		;Ascii start "  * "
		rst	prnst		; Ausgabe String
		db "  *",' '+80h
		ld	hl, (ARG2)
		;8 Bytes ASCII
		ld	b, 8
dmp_zeile4:	ld	a, (hl)
		inc	hl
		call	tstzei
		jr	nc, dmp_zeile5	; wenn kein Zeichen
		ld	a, '.'		; dann "."
dmp_zeile5:	rst	outch		; Zeichenausgabe
		djnz	dmp_zeile4
		;
		ld	(ARG1),	hl	; aktuelle Adresse merken (HL+8)
		;Ascii Ende
		rst	prnst		; Ausgabe String
		db " *",6,' ',88h	; 6-Kursor an den Anfang der Zeile
					; 8-Kursor nach links
		ret

;------------------------------------------------------------------------------
; Test auf normales ASCII-Zeichen
;------------------------------------------------------------------------------

tstzei:		cp	7Fh
tstzei1:	jr	z, tstzei3
		cp	' '
		ret	c		; wenn Steuerzeichen
		cp	7Fh
		jr	nc, tstzei3	; wenn kein ACSII
tstzei2:	and	a
		ret
tstzei3:	scf
		ret

;------------------------------------------------------------------------------
; Test auf Hexziffer
;------------------------------------------------------------------------------

tsthex:		cp	'F'+1
		jr	nc, tstzei3
		cp	'0'
		ret	c
		cp	'9'+1
		jr	c, tstzei2
		cp	'A'
		ret

;------------------------------------------------------------------------------
; Ausgabe 12 Leerzeichen an Zeilenanfang
;------------------------------------------------------------------------------

loc_4A1:	rst	prnst		; Ausgabe String
		db 6h +80h		; Kursor an den Anfang der Zeile
		ld	b, 12
loc_4A5:	rst	prnst		; Ausgabe String
		db ' ' +80h
		djnz	loc_4A5
		ret

;------------------------------------------------------------------------------
; Standard-Routine Ausgabe Zeichenkette, RST 18
;------------------------------------------------------------------------------

rprnst:		ex	(sp), hl
		push	af
rprnst1:	ld	a, (hl)		; Zeichen holen
		inc	hl		; nächste Adr.
		rst	OUTCH
		rlca			; Ende (Bit7 gesetzt)?
		jr	nc, rprnst1	; nein -> nächstes Zeichen
		pop	af
		ex	(sp), hl	; neue Return-Adr.
		ret

;------------------------------------------------------------------------------
; outch deaktivieren/aktivieren
;------------------------------------------------------------------------------

; up's zu kdo5
outchoff:	ld	a, 0C9h		; RET
		jr	outchon1
outchon:	ld	a, 0C3h		; JP
outchon1:	ld	(jp_rst10), a	; outch
		ret

;------------------------------------------------------------------------------
; X Prog X Sprung in Programmpaket X via Modul 1
;------------------------------------------------------------------------------

		db 0,9,'X',0Dh

		ld	a, 1
		out	(modul1), a	; Modul1 ROM "PaketX" aktivieren
		jp	0E000h		; und starten

;------------------------------------------------------------------------------
; Register mit Argumenten laden, aaaa=HL, bbbb=DE, cccc=BC
;------------------------------------------------------------------------------

para:		ld	hl, (ARG1)	; Argument 1
		ld	de, (ARG2)	; Argument 2
		ld	bc, (ARG3)
		ret

;------------------------------------------------------------------------------
; U USB Load Laedt Files vom USB
; U aaaa "FILENAME.Z80"                      	USB File laden
;------------------------------------------------------------------------------

		db 0,9,'U',0Dh

		xor	a
ld_usb:		ld	(filetyp), a	; erwarteter Filetyp
		xor	a		; load
		jr	ldsv_usb

;------------------------------------------------------------------------------
; u USB SAVE Speichert/Loescht Files auf USB
; u AADR EADR SADR "FILENAME.Z80" P Fileinfo   USB File speichern 
; u-"FILENAME.Z80"	USB File löschen
; u			Directory auflisten
;------------------------------------------------------------------------------

		db 0,9,'u',0Dh

		cp	'-'		; Datei loeschen?
		jr	z, del_usb

		ld	a, 1		; save

; Datei laden/speichern	
; in DE=Adr. Filename etc. 
; bei Aufruf via Monitor DE=Adr in BWS (Leerzeichen von Filename), 
; Filename steht in umgekehrter Reihenfolge im Speicher
; D=0Fh, Direktmodus

ldsv_usb:	ld	ixl, a		;Modus (0=load, 1=save)
		call	USBInit
		ret	nz
		call	sub_80F		; Teste Filename
		jr	nz, ldsv_usb1	; Filename ab DE
		
		; kein Filename oder ungültige Syntax:
; 'u' List Directory
dir_usb:	rst	prnst		; Ausgabe String
		db "INHALT",':'+80h
		ld	a, 1		; Vinculum-Kdo List files
		rst	PUT
		ld	a, 0Dh		; ohne Parameter
		rst	PUT
		; Anzeigen bis CR
dir_usb1:	rst	GET
		cp	0Dh		; CR
		jr	nz, dir_usb2
		; Ende:
		ld	a, (cupos)	; cursor auf Zeilenende
		and	0F0h
		ld	(cupos), a
		ld	a, ' '		; Leerzeichen ausgeben
dir_usb2:	rst	outch		; Zeichenausgabe
		cp	'>'		; Prompt erkannt?
		jr	nz, dir_usb1	; nein
		rst	GET		; noch CR einlesen
		cp	0Dh
		jp	nz, error1	; kein CR -> Fehler
		ret

; Datei loeschen
del_usb:	call	sub_80F		; Teste Filename
		call	sub_95B		; Headersave-Kopf befüllen
		ld	a, 7		; Vinculum-Kdo  Delete File
		jp	sub_E86		; Kommando mit Filename senden

; Datei laden/speichern
ldsv_usb1:	call	sub_95B		; Headersave-Kopf befüllen
		; gibt es die Datei schon?
		ld	a, 1		; Vinculum-Kdo  List files
		call	sub_6BB		; Kommando mit Filename senden
		ld	a, 0Dh		; CR, Kommando ausführen
		rst	PUT
		rst	GET		; Kdo erfolgreich ausgeführt?
		cp	0Dh
		jr	nz, ldsv_usb5	; nein -> Fehler
ldsv_usb2:	rst	GET		; folgt CR?
		cp	0Dh
		jp	z, ldsv_usb17	; ja - Keine Datei gefunden
		cp	' '
		jr	nz, ldsv_usb2
		ld	b, 4
		ld	hl, hs_gesamt
ldsv_usb3:	rst	GET
		ld	(hl), a
		inc	hl
		djnz	ldsv_usb3
ldsv_usb4:	rst	GET
		cp	0Dh
		jr	nz, ldsv_usb4
		rst	GET
		cp	'>'		; Prompt ?
		jr	nz, ldsv_usb5
		rst	GET
		cp	0Dh
ldsv_usb5:	jr	nz, ldsv_usb11	; nz-> Fehler
		and	ixl		;Modus (0=load, 1=save)
		jp	nz, loc_74D	;bei Save
; load		
		ld	a, 0Eh		; Vinculum-Kdo  Open/read
		call	sub_E86		; Kommando mit Filename senden
		ld	bc, (hs_gesamt)
		xor	a
		ld	h, a
		ld	l, a		; hl=0
		ld	(hs_rest), hl
		ld	l, 20h ;
		sbc	hl, bc
		ld	hl, (ARG1)
		ld	a, 0Bh		; Vinculum-Kdo Read from File
		jr	c, ldsv_usb6	
		call	put_length	; BC senden (Laenge)
		jp	ldsv_usb15

;
ldsv_usb6:	ld	bc, 20h		; Erste 32 Bytes vom File
		call	put_length	; BC senden (Laenge)
		ld	hl, hs_aadr
		ld	de, 20h	
		call	sub_787		; ixl=0 max(bc,de) Bytes ab hl anzeigen
					; ixl<>0 max(bc,de) Bytes nach hl holen
		call	PROMPTTEST	; Prompt, CR testen
		ld	hl, hs_fileinfo
ldsv_usb7:	dec	hl
		ld	a, 0D3h		; Kennung HeaderSaveKopf
		cp	(hl)
		jr	nz, ldsv_usb16	; kein HS-Kopf
		inc	e
		ld	a, e
		cp	3
		jr	nz, ldsv_usb7	; 3xD3 erwartet
		dec	hl
		ld	c, (hl)		; Dateityp
		;
		ld	hl, hs_fileinfo
ldsv_usb8:	ld	a, (hl)
		rst	outch		; Zeichenausgabe
		inc	hl
		inc	e
		ld	a, e
		cp	13h
		jr	nz, ldsv_usb8
		rst	prnst		; Ausgabe String
		db 0BAh
		ld	a, c
		rst	outch		; Zeichenausgabe
		rst	prnst		; Ausgabe String
		db 0A0h
		call	sub_AB7
		jr	z, ldsv_usb14
		call	sub_E84		; Close File
ldsv_usb9:	rst	prnst		; Ausgabe String
		db "TYP",0A0h
ldsv_usb10:	rst	prnst		; Ausgabe String
		db "FIL",0C5h
ldsv_usb11:	jp	error1
;
ldsv_usb12:	rst	prnst		; Ausgabe String
		db "EXIST",0A0h
		jr	ldsv_usb10

;
ldsv_usb13:	rst	prnst		; Ausgabe String
		db "NAME",0A0h
		jr	ldsv_usb10
;		
ldsv_usb14:	ld	hl, (hs_eadr)
		ld	bc, (hs_aadr)
		sbc	hl, bc
		inc	hl
		ld	(hs_laenge), hl	; Laenge
		push	hl
		push	hl
		ld	hl, (hs_gesamt)
		ld	e, 20h ; ' '
		sbc	hl, de
		ld	b, h
		ld	c, l
		ld	a, 0Bh		; Vinculum-Kdo Read from File
		call	put_length	; BC senden (Laenge)
		pop	de
		sbc	hl, de
		ld	(hs_rest), hl	; Restlänge
		pop	bc
		call	sub_6A0
		ld	hl, (ARG1)
		ld	a, l
		or	h
		jr	nz, ldsv_usb15
		ld	hl, (hs_aadr)
ldsv_usb15:	rst	30h		; outhl
		jr	ldsv_usb20

;
ldsv_usb16:	call	sub_E84		; close file
		ld	a, 0Eh		; Vinculum-Kdo  Open/read
		call	sub_E86		; Kommando mit Filename senden
		ld	bc, (hs_gesamt)
		ld	a, 0Bh		; Vinculum-Kdo Read from File
		call	put_length	; BC senden (Laenge)
		ld	hl, (ARG1)
		ld	(hs_sadr), hl
		jr	ldsv_usb15

;
ldsv_usb17:	and	ixl
		jr	z, ldsv_usb13
;		
ldsv_usb18:	call	para
		ex	de, hl
		sbc	hl, de
		jr	c, ldsv_usb11
		ld	a, 9		; Vinculum-Kdo  Open/write
		call	sub_E86		; Kommando mit Filename senden
		jr	nz, ldsv_usb13
		ld	bc, (hs_gesamt)
		call	sub_1A5
		ld	hl, 0
		jr	nz, ldsv_usb19
	; basic		
		cp	'F'
		jr	nz, ldsv_usb19
		ld	hl, (60ABh)	; LSTRAM  Last available RAM
		ld	de, (60BFh)	; STRBOT  Bottom of string space
		sbc	hl, de
		inc	hl
ldsv_usb19:	ld	(hs_rest), hl
		add	hl, bc
		ld	b, h
		ld	c, l
		ld	a, 8		; Vinculum-Kdo Write to File
		call	put_length	; BC senden (Laenge)
		; Lese Kopfblock
		ld	hl, hs_buf
		ld	b, 20h 		; Anzahl
		call	loc_AFA		; Lese b Bytes nach (HL)
		ld	bc, (hs_laenge)	; Laenge
		ld	hl, (ARG1)	; aadr ausgeben
		rst	30h		; outhl
ldsv_usb20:	rst	prnst		; Ausgabe String
		db ' ' + 80h
		; Lese Dateiinhalt
		call	sub_784		; bc Bytes nach hl holen
		call	sub_1A5
		jr	nz, ldsv_usb22
	; Basic
		cp	'B'
		jr	nz, ldsv_usb21
		ld	(60D2h), hl	; PROGND  End of program
ldsv_usb21:	cp	'F'
		jr	nz, ldsv_usb23
		ld	hl, (60BFh)	; STRBOT  Bottom of string space
;		
ldsv_usb22:	ld	bc, (hs_rest)
		call	sub_784		; bc Bytes nach hl holen
ldsv_usb23:	call	PROMPTTEST	; Prompt, CR testen
		call	sub_E84		; close file
		dec	hl		; HL=eadr ausgeben
		rst	30h		; outhl
		ld	hl, (hs_sadr)
		rst	prnst		; Ausgabe String
		db 0A0h
		rst	30h		; outhl startadr
		ret

;		
sub_6A0:	call	sub_1A5
		ret	nz
		cp	'P'		; Programm?
		ret	z
		pop	hl
		ld	hl, (ARG1)
		rst	30h		; outhl
		cp	'F'
		ld	e, 0ABh		; ??
		jr	z, loc_6B4
		ld	e, 0F7h		; ??

loc_6B4:	rst	GET
		dec	bc
		dec	e
		jr	nz, loc_6B4
		jr	ldsv_usb20

; Kommando mit Filename senden
sub_6BB:	push	iy
		rst	PUT		; Kommando
		ld	a, ' '
		rst	PUT
		ld	b, 12
loc_6C3:	ld	a, (iy+0)
		inc	iy
		cp	'"'
		jr	z, loc_6D3
		cp	21h ; '!'
		call	nc, rPUT	; rst20
		djnz	loc_6C3
loc_6D3:	pop	iy
		ret

;------------------------------------------------------------------------------
; Synchronisieren mit Vinculum:
;------------------------------------------------------------------------------

; PIO Port B initialisieren
USBInit:	ld	a, 0CFh		; Bitbetrieb
		out	(PIO3BS), a
		ld	a, 00110011b	; I/O festlegen
		out	(PIO3BS), a
		ld	a, 7		; kein INT
		out	(PIO3BS), a
;VDIP Reset
		ld	a, 84h
		out	(PIO3B), a
; PIO Port A initialisieren
		ld	a, 8Fh		; bidirektional
		out	(PIO3AS), a
		ld	a, 7		; kein INT
		out	(PIO3AS), a
		in	a, (PIO3A)	; Dummy-Eingabe
		ld	a, 0C4h		; #PROG=1, #RESET=0, RD&WR inaktiv
		out	(PIO3B), a
; mehrstufiges Synchronisieren
sync:		ld	hl, 400h
syn1:		in	a, (PIO3B)	; Status abfragen
		rrca
		jr	c, syn2		; keine	Daten vorhanden
		rst	GET		; vorhandene Daten abholen
		jr	sync
syn2:		rrca
		jr	c, sync		; noch nicht bereit, Daten zu schreiben
syn2a:		djnz	syn2a		; kurze	Zeit warten
		dec	hl
		ld	a, h		; Zaehler abwarten
		or	l
		jr	nz, syn1	; nochmals nachschauen...
		in	a, (PIO3B)	; Status abfragen
		and	3		; nur Bit 0 und	1 auswerten
		cp	1		; alle Daten abgeholt und bereit zum schreiben?
		jr	nz, sync	; nein !
		ld	a, 0Dh
		rst	PUT
syn3:		rst	GET
		cp	0Dh		; <cr> muss irgendwie <cr> zurueckgeben
		jr	nz, syn3	; warten bis CR	geliefert wird
		ld	a, 'E'		; E <cr> muss E <cr> zurueckgeben
		rst	PUT
		ld	a, 0Dh
		rst	PUT
syn4:		rst	GET
		cp	'E'
		jr	nz, syn4	; warten bis E geliefert wird
		rst	GET
		cp	0Dh		; auf CR warten
		jr	nz, syn4
; Test,	ob USB-Stick angeschlossen ist:
		ld	a, 10h		; SCS		    ; Short CMD-Modus
		rst	PUT
		call	EXEC		; Kommando ausfuehren
		ld	a, 91h		; IPH               ; HEX- bzw. BIN-Modus
		rst	PUT
		call	EXEC		; Kommando ausfuehren
; und gleich nochmal als Test, ob Stick eingesteckt ?

; UP EXEC VDIP Kommando ausfuehren, Rueckkehr mit Z wenn OK
EXEC:		ld	a, 0Dh		; CR		    ; Kommando ausfuehren
		rst	PUT
; Prompttest, Rueckkehr bei Fehler	mit C
; Prompt, CR testen
PROMPTTEST:	rst	GET
		cp	'>'
		jr	nz, ERR
		rst	GET
		cp	0Dh		; CR		      ;	OK, fertig?
		ret	z
;
ERR:		rst	outch		; Zeichenausgabe
		rst	GET
		cp	0Dh		; CR		      ;	Ende?
		jr	nz, ERR		; nein,	weiter bis CR folgt (Ende)
;
error1:		call	error
		xor	a
		dec	a		; A = FF, Cy=1
		ret

; USB Save
loc_74D:	ld	hl, (cupos)
		rst	prnst		; Ausgabe String
		db 7,"OverWrite?(J)",8,88h
		rst	inch		; inch; Zeicheneingabe
		cp	0Dh
		jr	z, loc_771
		res	5, a		; Umwandlung in Großbuchstabe
		rst	outch		; Zeichenausgabe
		rst	prnst		; Ausgabe String
		db ')',83h
		cp	'J'
		jp	nz, ldsv_usb12
		
loc_771:	ld	(cupos), hl
		ld	a, iyu
		cp	30h		; ?? CmdWr  Write Sector
		ret	z
		ld	a, 7		; Vinculum-Kdo  Delete File
		call	sub_E86		; Kommando mit Filename senden
		call	sub_9A4
		jp	ldsv_usb18

;
sub_784:	ld	de, 0FFFFh
; ixl		;Modus (0=load, 1=save)
; ixl=0 max(bc,de) Bytes ab hl anzeigen
; ixl<>0 max(bc,de) Bytes nach hl holen
sub_787:	ld	a, e
		or	d
		ret	z		; de=0?
		ld	a, c
		or	b
		ret	z		; bc=0?
		xor	a
		cp	ixl		; ixl <> 0 PUT
		jr	nz, loc_796
		rst	GET		; ixl = 0 GET
		ld	(hl), a
		jr	loc_798
loc_796:	ld	a, (hl)
		rst	PUT
loc_798:	push	hl
		ld	hl, (cupos)
		ld	(hl), a
		pop	hl
		inc	hl
		dec	bc
		dec	de
		jr	sub_787		; ixl=0 max(bc,de) Bytes ab hl anzeigen

;------------------------------------------------------------------------------
; rst28 GET
; Eingabe von VDIP1 abholen (mit Break und TimeOut):
; PA:	A	Datenbyte
;	CY=1	TimeOut oder Break
; VR:	AF
;------------------------------------------------------------------------------

rGET:		in	a, (PIODA); Tastatur
		cp	83h		; <STOP>-Taste gedrueckt?
		jp	z, break1
		in	a, (PIO3B)	; Status abfragen
		rrca
		jr	c, rGET
		ld	a, 0C0h		; RD# aktiv
		out	(PIO3B),	a
		in	a, (PIO3A)	; Daten	holen
		push	af
		ld	a, 0C4h		; RD# inaktiv
		out	(PIO3B), a
		pop	af
		ret

;------------------------------------------------------------------------------
; Tausch Buffer-Adr Normal <-> Basic
;------------------------------------------------------------------------------

sub_7BC:	exx
		ld	hl, (hd_buffer)	; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
		ld	de, (hd_bufbas)	; HD-Puffer GS-Basic 00	3E
		ex	de, hl
		ld	(hd_buffer), hl	; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
		ld	(hd_bufbas), de	; HD-Puffer GS-Basic 00	3E
		exx
		ret

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------

init_gsb:	ld	hl, (igsbslm)	; Voreinstellung Methode GSB SAVE/Load USB HD 00h 20h
		ld	(gsbslm), hl	; Methode GSB SAVE/Load	USB HD 00h 20h
		ld	l, 0
		ld	(hd_buffer), hl	; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
		ld	h, 3Eh 
		ld	(hd_bufbas), hl	; HD-Puffer GS-Basic 00	3E

; patcheinschub
; zu colkdo
	if p_colkdo
		ret
;zu colkdo	
colkdo1		ld	l, c
		rst	prnst
		db 0A0h
		call	j_OUTHL		; BC = ARG3
		rst	prnst
		db 8Dh
		ret
	else
		nop
		nop
		nop
		ret

		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
	endif
	
; Einschub
;------------------------------------------------------------------------------
; Sprungverteiler
; an dieser festen Adr. zur Kompatibilität mit Ur-AC1
; (Ende 2K-EPROM)
;------------------------------------------------------------------------------

		org	07E8h	; fixe Adresse

		jp	v24out
		jp	MS30
j_OUTHEX:	jp	OUTHEX
		jp	OUTHL
		jp	INLINE
		jp	INHEX
j_TASTE:	jp	TASTE
		jp	GETCO1

;------------------------------------------------------------------------------
; Kennung Monitor + Sprungverteiler externes SAVE/LOAD
;------------------------------------------------------------------------------

		org 800h	; fixe Adresse
monver:		db "MO11.0"		; Monitorkennung
		
		jp	EX_SA_LO	; XX_SAVE/LOAD
		jp	USBInit
		jp	HDInit


;------------------------------------------------------------------------------
; Teste Filename
;
; in DE=Adr. in BWS auf Leerzeichen vor Filename
; in D=0Fh, wenn KEIN " am Filenamensanfang
; sonst DE=18D4h
; ret z=1 -> kein Filename
; ret z=0 -> a=..
;         -> gültiger Filename, de=Anfang Filename, a=23h
;------------------------------------------------------------------------------

sub_80F:	ld	a, 0Fh		; Direktmodus?
		cp	d		; 
		ret	z		; ja, Ende
		
		; wenn BWS, dann muss hier " folgen
		dec	de		; ein Zeichen vor
		ld	a, (de)
		cp	' '		; Leerzeicheh?	
		ret	z		; ja, Ende, da kein Filename angegeben

		cp	'"'		; steht " am Filenamensanfang?
		ret	nz		; nein, Ende
		
		dec	de		; ein zeichen vor
					; DE zeigt auf Filename
		inc	a		; A=23h
		ret

;------------------------------------------------------------------------------
; BC senden (Laenge)
;------------------------------------------------------------------------------

put_length:	rst	PUT		; Kdo senden
		ld	a, ' '          ; Trennzeichen
		rst	PUT
		xor	a		; 00
		rst	PUT
		rst	PUT
		ld	a, b		; niedrigste Stelle (00xxH)
		rst	PUT
		ld	a, c		; niedrigste Stelle (00xxH)
		rst	PUT
		ld	a, 0Dh		; CR
		rst	PUT
		ret

;------------------------------------------------------------------------------
; D Dump Hexdump und ASCII-Darstellung, sowie Modifikation
; D aaaa Dump normal
; D aaaa eeee Dump drucken
;------------------------------------------------------------------------------

		db 0,9,'D',0Dh

		ld	ix, (ARG2)	; eeee
		xor	a
d_ko1:		ld	(ARG3),	a	;arg3 = Modus (0-Hex, FF-Ascii-Dump)
		ld	hl, ARG2
		ld	a, (ARG2+1)
		or	(hl)
		ld	(ARG3+1), a	; Arg3+1 <> 0 -> eeee angegeben
		;
		rst	prnst		; Ausgabe String
		db '*',8Dh
d_ko2:		ld	c, 1
		ld	hl, (ARG1)
		ld	a, l
		and	0F8h 		; aaaa abrunden auf Vielfaches von 8
		ld	l, a
		call	dmp_zeile	; Ausgabe Zeile
		ld	a, (ARG3+1)
		and	a
		jr	z, d_ko4	; keine Endadresse angegeben
		
		; Ausgbe fortlaufend, z.B. für Druck
		; gleich weiter mit nächster Zeile
		rst	prnst		; Ausgabe String
		db 8Dh			; neue Zeile
d_ko3:		ld	de, (ARG1)
		push	ix
		pop	hl
		and	a
		sbc	hl, de		; eeee erreicht?
		ret	c		; ja
		jr	d_ko2		; sonst weiter ausgeben

		; Einzelausgabe/Editieren
d_ko4:		call	j_TASTE		; Tastaturabfrage
		jr	z, d_ko6	; keine Taste gedrückt
		;
		rst	prnst		; Ausgabe String
		db 8Dh			; neue Zeile
		jr	d_ko2
;		
d_ko5:		call	dmp_zeile	; Ausgabe Zeile
d_ko6:		ld	hl, (ARG2)	; eeee
		; + aktuelles Byte
		add	hl, bc
		dec	hl
		; Anzeige aktuelle Adresse
		rst	prnst		; Ausgabe String
		db ' ','<'+80h
		call	OUTHL
		rst	prnst		; Ausgabe String
		db '>'+80h
		; 
		ld	a, (ARG3)	; Hext-Dump?
		and	a
		jr	nz, d_ko7	; nein
		; Hex-Dump
		rst	prnst		; Ausgabe String
		db " (H)",0A0h
		jr	d_ko8
d_ko7:		; ASCII-Dump
		rst	prnst		; Ausgabe String
		db " (A)",0A0h
;		
d_ko8:		ld	(cupos), de
		ld	b, c
d_ko9:		rst	prnst		; Ausgabe String
		db 9,9,89h		; je Byte 3x rechts
		djnz	d_ko9
		;
		call	j_TASTE		; Tastaturabfrage
		cp	8Bh 		; Kursor nach oben, Taste gedrückt
		jr	z, d_ko19
;		
d_ko10:		rst	inch		; inch; Zeicheneingabe
		cp	8		; Kursor nach links
		jp	z, d_ko26
		cp	3		; Strg-C, Ende
		jr	z, d_ko17	
d_ko11:		cp	0Dh		; CR, Kursor an Anfang nächster Zeile; Enter
		jr	z, d_ko16
		cp	1		; Home, Toggle Modus
		jr	z, d_ko18
d_ko12:		cp	9		; Kursor nach rechts
		jr	z, d_ko22
		cp	0Ah		; Kursor nach unten
		jr	z, d_ko25
		cp	0Bh		; Kursor nach oben
		jr	z, d_ko19
		cp	20h ; ' '
		jr	c, d_ko10	; sonstige Steuerzeichen werden nicht verarbeitet
		; Zeicheneingabe
		ex	af, af'
		ld	a, (ARG3)
		and	a
		jr	nz, d_ko20	; wenn Ascii-Modus
; Zeicheneingabe Hex-Modus
; Eingabe 2 Hex-Zeichen
		ex	af, af'
		call	tsthex		; 1. Zeichen Hex ?
		jr	c, d_ko10	; nein, Neueingabe
		ld	b, 2
d_ko13:		rst	outch		; Zeichenausgabe
		cp	'9'+1
		jr	c, d_ko14
		sub	37h ; '7'
d_ko14:		rld
		dec	b
		jr	z, d_ko21
d_ko15:		rst	inch		; inch; Zeicheneingabe
		call	tsthex		; 2. Zeichen Hex ?
		jr	c, d_ko15	; nein, Neueingabe
		jr	d_ko13
; Enter
d_ko16:		call	loc_4A1		; Ausgabe 12 Leerzeichen an Zeilenanfang
		rst	prnst		; Ausgabe String
		db 8Dh
		jp	d_ko2
; 03 Ende
d_ko17:		rst	prnst		; Ausgabe String
		db 8Dh
		ld	(ARG1),	hl
		ret
; Home, toggle Modus
d_ko18:		ld	a, (ARG3)	; Modus
		cpl
		ld	(ARG3),	a
		jp	d_ko5

; Kursor nach oben	
d_ko19:		ld	de, 8
		and	a
		sbc	hl, de
		jr	d_ko27

; Zeicheneingabe Ascii-Modus
; c = Nr. aktives Byte
d_ko20:		ex	af, af'	; '
		call	tstzei		; Zeichen?
		jr	c, d_ko10	; nein, Neueingabe
		ld	(hl), a		; Zeichen in Speicher
d_ko21:		ld	a, 8		; max 8 Zeichen
		cp	c		; erreicht?
		jr	nz, d_ko22	; nein
		call	dmp_zeile	; ja: Ausgabe Zeile
		dec	hl
d_ko22:		inc	c		; nächstes Byte
		inc	hl
		ld	a, 9
		cp	c		; max 8 Byte
		jr	nz, d_ko24
		ld	c, 1		; erstes Byte
d_ko23:		call	loc_4A1		; Ausgabe 12 Leerzeichen an Zeilenanfang
		rst	prnst		; Ausgabe String
		db 8Ah
d_ko24:		jp	d_ko5

; Kursor nach unten
d_ko25:		ld	de, 8	
		add	hl, de
		jr	d_ko23

; Kursor nach links
d_ko26:		dec	hl
		dec	c
		jr	nz, d_ko24
		ld	c, 8
d_ko27:		call	loc_4A1		; Ausgabe 12 Leerzeichen an Zeilenanfang
		ld	de, (cupos)
		push	hl
		ld	hl, bwsat(1,1)	; 17BFh	
		sbc	hl, de
		pop	hl
		jr	nc, d_ko28
		exx
		; scroll
		ld	hl, BWSEND+COLS
		ld	de, BWSEND
		ld	bc, COLS*(LINES-1)-1
		ldir
		exx
		jr	d_ko24
d_ko28:		rst	prnst		; Ausgabe String
		db 8Bh
		jr	d_ko24

;------------------------------------------------------------------------------
; UP zu USB
; Headersave-Kopf befüllen
; in DE=Adr. Filename (im BWS)  ABCD.." (ohne führendes ")
;------------------------------------------------------------------------------

sub_95B:	ld	b, 16		; Länge Filename
		ld	hl, unk_186E	; Adr.Puffer
		push	hl
		pop	iy
loc_963:	ld	a, d
		cp	hi(BWSEND)-1	; BWS-Ende ?
		jr	z, loc_96D	; ja
		ld	a, (de)		; erstes Zeichen
		cp	'"'		; Namensende erreicht?
		jr	nz, loc_970	; nein -> Zeichen übertragen
loc_96D:	ld	a, ' '		; sonst mit Leerzeichen auffüllen
		inc	de		; BWS-Pointer zurück (bleibt auf ")
loc_970:	ld	(hl), a		; filename kopieren
		inc	hl		; nächste Adr. Buffer
		dec	de		; nächste Adr. BWS
		djnz	loc_963		; weiter bis 16 Zeichen fertig
		;
		ld	a, d
		cp	hi(BWSEND)-1	; BWS-Ende ?
		ret	z		; ja, Ende
		; wenn Eingabe von BWS
		dec	de		; 2 Zeichen weiter	
		dec	de
		ld	a, (de)		; Dateityp
		dec	de		; 2 Zeichen weiter
		dec	de
		push	de		; Beginn Kommentar
		; aaadr, eadr, sadr eintragen
		ld	hl, ARG1
		ld	de, hs_aadr
		ld	bc, 6
		ldir
		; hs_cod mit Monitor-Kennung füllen
		ld	hl, monver	; "MO11.0"
		ld	c, 6
		ldir
		; hs_filetyp Dateityp eintragen
		ld	(de), a
		inc	de
		; hs_kennung 3 x D3 Kennung Headersave-Kopf 
		ld	a, 0D3h	
		ld	(de), a
		inc	de
		ld	(de), a
		inc	de
		ld	(de), a
		inc	de
		; hs_fileinfo Kommentar eintragen
		pop	hl
		ld	b, 16		; Länge
loc_99E:	ld	a, (hl)
		ld	(de), a
		dec	hl
		inc	de
		djnz	loc_99E

; Berechne Dateilänge
sub_9A4:	call	para		; aadr, eadr
		xor	a
		ex	de, hl	
		sbc	hl, de		; eadr-aadr
		inc	hl		; Länge=eadr-aadr+1
		ld	(hs_laenge), hl	; Laenge
		ld	de, 20h		; Länge HS-Block
		add	hl, de
		ld	(hs_gesamt), hl	; Länge incl. Header
		ret

;------------------------------------------------------------------------------
; s HD Save Speichert ein File in DVHD
;------------------------------------------------------------------------------
		db 0,9,'s',0Dh

		cp	'-'
		ret	z

save0_hd:	ld	a, CmdWr	; Write Sector
		jr	loc_9CE

;------------------------------------------------------------------------------
; HD Load	Laedt ein in DVHD gespeichertes File von HD
;------------------------------------------------------------------------------

		db 0,9,'l',0Dh

		xor	a
ld_hd:		ld	(filetyp), a
load0_hd:	xor	a
		ex	af, af'		; '
		ld	a, CmdRd	; Read Sector

;
loc_9CE:	ld	iyu, a
		ld	iyl, CmdRd	; Read Sector
		call	HDInit
		jp	nz, error1
		push	de
		call	sub_A41
		call	sub_A59
		pop	de
		call	sub_80F		; Teste Filename
		ld	(unk_18B1), de	; Adr. Filename
		jp	z, loc_CA2
		jp	loc_CA9

sub_9EE:	push	ix
		rst	prnst		; Ausgabe String
		db 91h
		ld	b, 10h
loc_9F4:	ld	a, (ix+0)
		rst	outch		; Zeichenausgabe
		inc	ix
		djnz	loc_9F4
		rst	prnst		; Ausgabe String
		db ' ',90h
		ld	a, (ix+9)
		rst	outch		; Zeichenausgabe
		rst	prnst		; Ausgabe String
		db 0BAh
		xor	a
		or	e
		jr	z, loc_A1A
		ld	b, 2
loc_A0B:	inc	ix
		ld	l, (ix+0)
		inc	ix
		ld	h, (ix+0)
		rst	30h		; outhl
		rst	prnst		; Ausgabe String
		db 0A0h
		djnz	loc_A0B
loc_A1A:	pop	ix
		ret

	if p_colkdo
;------------------------------------------------------------------------------
; :: Arg. Argument Anzeige
;------------------------------------------------------------------------------
		
		db 0,9,':',0Dh

		call	para
		call	j_OUTHL		; HL = ARG1
		rst	prnst
		db 0A0h
		ex	de, hl
		call	j_OUTHL		; DE = ARG2
		ld	h, b
		jp	colkdo1		; an anderer Stelle geht weiter
	else
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
	endif

;------------------------------------------------------------------------------
; Start/Stop-Schaltung stopp
; UP TB aus (Ton und Interrupt wieder ein)
;------------------------------------------------------------------------------

		org 0A31h	; fixe Adresse
tb_aus:
q_ko20:		in	a, (PIODB)	; Grafik/Ton
		res	5, a
		jr	loc_A3C

;------------------------------------------------------------------------------
;Start/Stop-Schaltung start
; UP TB ein (Ton und Interrupt sperren)
;------------------------------------------------------------------------------

		org 0A37h	; fixe Adresse
tb_ein:
q_ko21:		in	a, (PIODB)	; Grafik/Ton
		set	5, a
		di
loc_A3C:	rst	prnst		; Ausgabe String
		db 87h
		out	(PIODB), a; Grafik/Ton
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

; UP zu HD
sub_A41:	ld	a, 8
sub_A43:	push	de
		exx
		ld	hl, (unk_1885)
		ld	de, (unk_1887)
		ld	h, a
		exx
		ld	hl, (hd_buffer)	; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
		ld	de, 200h	; 512 Byte
		call	ide_rw		; IDE Read/Write 
					; iyl Kommando, e head, dl cylinder, h sector
		pop	de
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

sub_A59:	ld	hl, (hd_buffer)	; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
		ld	de, 1Fh
		add	hl, de
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		ld	(unk_1889), de
		ld	de, 2Eh	; '.'
		add	hl, de
		ld	e, (hl)
		inc	hl
		ld	d, (hl)
		ld	(unk_188B), de
		ret

;------------------------------------------------------------------------------
; J Jump Sprung in Programm ab Adresse
;------------------------------------------------------------------------------

		db 0,9,'J',0Dh

		call	para
		jp	(hl)

;------------------------------------------------------------------------------
; Test auf Autostart
;------------------------------------------------------------------------------

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
; Einsprungschnittstelle, damit kann aus jeder Anwendung
;    via USB und HD (V24 nachladbar) auf LOAD & SAVE Routinen zugegriffen werden.
; XX_SAVE/LOAD
;------------------------------------------------------------------------------

EX_SA_LO:	call	sub_F9F		; Filename in ungekehrter Reihenfolge ab unk_18D4
		ld	a, (de)		; de=unk_18D4
		cp	'"'		; " am Filenamensanfang?
		jr	z, EX_SA_LO1	; ja
		ld	d, 0Fh		; nein, Direktmode;  hi(BWS)-1
					; Adr 0Fxxh ist BWS-Überlauf
EX_SA_LO1:	ld	a, (SA_LO_BYT)	; Methode
		or	a
EX_SA_LO2:	jp	z, ldsv_usb	; -> ldsv_usb mit A=0 (USB Load)
		cp	1
		jr	z, EX_SA_LO2	; -> ldsv_usb mit A=1 (Usb Save)
		cp	10h
		jp	z, EX_SA_LO3	; -> reserve
		cp	11h
		jp	z, EX_SA_LO3	; -> reserve
		cp	21h
		jp	z, save0_hd	; -> HD Save
		cp	20h
		jp	z, load0_hd	; -> HD Load
EX_SA_LO3:	ret

;------------------------------------------------------------------------------
; UP Suche Autostartsequenz in DE !! AADR (DE)
;------------------------------------------------------------------------------

sub_AB7:	ld	a, (filetyp)
		or	a
		ret	z
		cp	c
		ret	z
		cp	'B'		; Basicprogramm
		ret	nz
		ld	a, c
		cp	'P'		; lauffähiges Maschinenprogramm
		ret	nz
		ld	hl, 0
		ld	(ARG1),	hl
		ret

;------------------------------------------------------------------------------
; W IO-Port beschreiben Byte an IO-Port ausgeben
;------------------------------------------------------------------------------

		db 0,9,'W',0Dh

		call	para
		ld	b, h
		ld	c, l
		out	(c), e
		ret

;------------------------------------------------------------------------------
; Tabelle Peripherie Initialisierung
;------------------------------------------------------------------------------

; jeweils port, wert, s. init3
TABIO:		db CTC0
		db lo(inttab)		; Interrupt-Vektor CTC
		db CTC1
		db  37h			; DI,Zeitgeber,Vorteiler 256,ZK folgt
		db CTC1
		db  4Eh			; Zeitkonstante
		db CTC2
		db  47h			; DI,Zähler,Vorteiler 16,ZK folgt
		db CTC2
		db  32h			; Zeitkonstante
;
		db PIO2CB
		db 0CFh			; Mode 3 (Bit E/A)
		db PIO2CB
		db    0			; alle Bits Ausgabe
		db PIO2CA
		db 0CFh			; Mode 3 (Bit E/A)
		db PIO2CA
		db  0C5h		; b7,b6,b3 und b0 Eingabe
		db PIO2DA
		db  0Ah			; Bits setzen
;
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
		db    0			; alles deaktiviert

;------------------------------------------------------------------------------
; UP zu USB ldsv_usb19
; Lese b Bytes nach (HL)
;------------------------------------------------------------------------------

loc_AFA:	ld	a, (hl)
		rst	PUT
		inc	hl
		djnz	loc_AFA
		ret

;------------------------------------------------------------------------------
; Tabelle mit Baudraten-Timings, muss an 00-Adresse liegen
;------------------------------------------------------------------------------
		
		org 0B00h	; fixe Adresse
bdtab:		db    3			; 14400
		db    2			; 19200
		db    5			; 9600
		db  0Bh			; 4800
		db  17h			; 2400
		db  30h			; 1200
		db  61h			; 600
		db 0C2h			; 300

;------------------------------------------------------------------------------
; UP zu tbload
;------------------------------------------------------------------------------

loc_B08:	ld	a, d
		ld	(unk_188D), a
		push	ix
		pop	de
		ld	b, 10h
		ld	hl, (unk_18B1)
loc_B14:	ld	a, (hl)
		cp	'"'
		jr	nz, loc_B1C
		ld	a, ' '
		inc	hl
loc_B1C:	ld	(de), a
		dec	hl
		inc	de
		djnz	loc_B14
		dec	hl
		dec	hl
		ld	a, (hl)
		inc	de
		ld	hl, ARG1
		ld	bc, 6
		ldir
		inc	de
		inc	de
		ld	(de), a
		ld	(ix+10h), 0FFh
		exx
		ld	hl, (unk_1889)
		ld	(ix+1Bh), l
		ld	(ix+1Ch), h
		ld	de, (unk_188B)
		ld	(ix+1Dh), d
		ld	(ix+1Eh), e
		exx
		call	para
		ld	iyl, CmdWr 	; Write Sector
loc_B4F:	call	ide_rw		; IDE Read/Write
					; iyl Kommando, e head, dl cylinder, h sector
		inc	(ix+1Ah)
		push	hl
		xor	a
		sbc	hl, de
		pop	hl
		jr	c, loc_B4F
		call	crc
		ld	(ix+17h), l
		ld	(ix+18h), h
		exx
		push	de
		push	hl
		exx
		ld	a, (unk_188D)
		call	sub_A43
		ld	iyl, CmdRd 	; Read Sector
		call	sub_A41
		ld	hl, (hd_buffer)	; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
		ld	de, 1Fh
		add	hl, de
		pop	de
		ld	(hl), e
		inc	hl
		ld	(hl), d
		ld	de, 2Eh
		add	hl, de
		pop	de
		ld	(hl), e
		inc	hl
		ld	(hl), d
		ld	iyl, CmdWr	; Write Sector
		call	sub_A41
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

HDInit:		ld	hl, vHDFirstCyl	; Startspur fuer HD (Standard=2)	02 00
		ld	a, h
		ld	h, 1
		ld	(unk_1885), hl
		ld	h, a
		ld	l, 0
		ld	(unk_1887), hl
		call	sub_BB9		; wait while busy
		ld	hl, (hd_heads)
		ld	a, h
		out	(IdeSCnt), a	; Sector Count
		ld	a, l		;get head number
		or	0A0h		;+ first IDE disk
		out	(IdeSDH), a	; Cylinder High
		ld	hl, (hd_cylinder)	
		ld	a, l
		out	(IdeCLo), a	; Cylinder Low
		ld	a, h
		out	(IdeCHi), a	; Cylinder High
		ld	a, CmdInit 	; Initialize Drive Params
		out	(IdeCmd), a

;wait while busy
sub_BB9:	push	hl
		ld	hl, 800h
loc_BBD:	djnz	loc_BBD
		dec	hl
		ld	a, l
		or	h
		jr	z, loc_BCB
		;
		in	a, (IdeCmd)	;get status
		rla			;BSY bit to Carry
		ld	a, 1		
		jr	c, loc_BBD	;loop until not BUSY
loc_BCB:	dec	a
		pop	hl
		ret
		
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh

		org 0BD4h	; fixe Adresse
; Direktaufruf aus Basic
; Rest zu tb-Kopfblock füllen
		; in Mon 1088 erfolgte hier noch mehr (anderes Kopfblock-Format)
tbh6:		ld	l, lo(filetyp)	; H=18h -> HL=filetyp
		ld	(hl), c		; 'B' oder 'F'
		ret

; UP zu xx
loc_BD8:	call	sub_9EE
		call	loc_74D
		rst	prnst		; Ausgabe String
		adc	a, l
		ret	nz
		ld	(ix+10h), 0
		ld	a, d
		ld	iyl, CmdWr	; Write Sector
		call	sub_A43
		ld	iyl, CmdRd	; Read Sector
		jp	loc_CC9

		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; Erweiterung der Funktionalitäten von GS-BASIC 3.2
;------------------------------------------------------------------------------

; BASIC-Direktaufrufe in den Monitor
; 0008h			; inch
; 0071h			; (BYE) sprung in Monitor
; 			; Ausgabe Startmeldung. Besser wäre Adr 006E
; 0183h			; OUTHEX
; 0272h			; beep, UP "akustisches Signal"
; 0287h			; UPTON, UP "Ton", Reg, B = Tonlaenge, C = Tonhoehe
; 0297h			; UPTAST, UP "Taste", testet Tastaturtatus
; 0A31h			; q_ko20, Start/Stop-Schaltung stop
; 0A37h			; q_ko21, Start/Stop-Schaltung start
; 0BD4h			; tbh6
; 0BF7h			; tbsave
; 0C81h			; tbload
	
		org	0BF7h	; fixe Adresse
; Kassette Save
; in C: Dateityp
tbsave:		pop	hl
		pop	hl
		ld	a, c
		cp	'F'		; Datenfeld (Basic) ?
		jr	z, loc_C07
		; Programm speichern
		ld	hl, (605Ah)	; BASTXT Pointer to start of program
		ld	de, (60D2h)	; PROGND End of program
		jr	loc_C0E
		; Felder speichern		
loc_C07:	ld	hl, 60ABh	; LSTRAM Last available RAM
		ld	de, (60D6h)	; ARREND End of arrays
		;
loc_C0E:	ld	(ARG3),	hl
		ld	hl, 6000h	; WRKSPC BASIC Work space
		ld	(ARG1),	hl
		dec	de
		ld	(ARG2),	de
		ld	c, 1
loc_C1E:	ld	a, (gsbslm)	; Methode GSB SAVE/Load	USB HD 00h 20h
		add	a, c
		ld	hl, SA_LO_BYT
		ld	(hl), a
		inc	hl		; hl=filenambuf
;headersave-kopf leeren
		push	hl
		ld	(hl), ' '
		ld	de, filenambuf+1
		ld	bc, 20h
		ldir
		pop	hl
		ld	(hl), '"'
		inc	hl
		ld	de, unk_189E
		push	de
		call	loc_E8C
		ld	c, 9
loc_C3F:	ld	a, (de)
		ld	(hl), a
		dec	c
		jr	z, loc_C50
		cp	' '
		jr	z, loc_C50
		cp	'.'
		jr	z, loc_C50
		inc	hl
		inc	de
		jr	loc_C3F
loc_C50:	ld	a, 8
		cp	c
		jr	z, loc_C64
		ld	de, aZ80	; Endung ".Z80"
		ex	de, hl
		ld	c, 5
		ldir
		ex	de, hl
;
loc_C5E:	inc	hl
		ld	a, (filetyp)	; Filetyp eintragen
		ld	(hl), a
		inc	hl
loc_C64:	inc	hl		
		pop	de
		ex	de, hl
		ld	bc, 16		; Länge Kommentar
		ldir
		ld	(soil),	bc
		call	EX_SA_LO	; XX_SAVE/LOAD
		call	sub_7BC		; Tausch Buffer-Adr Normal <-> Basic
		rst	prnst		; Ausgabe String
		db 8Dh
		jp	z, 5DEBh	; CSAVE1
		ld	e, 24h 		; error 24h
		jp	4384h		; ERROR
		
		db 0FFh


		org	0C81h	; fixe Adresse
; Kassette Load
; in C: Dateityp
tbload:		ld	a, (hs_eadr)
		cp	0B6h		; ??
		ret	z
		pop	de
		pop	de
		pop	de
		ld	a, c
		ld	(filetyp), a
		ld	hl, 60ABh	; LSTRAM Last available RAM
		cp	'F'		; Datenfeld (Basic) ?
		jr	z, loc_C9A
		ld	hl, (60D2h)	; PROGND End of program
		dec	hl
		dec	hl
loc_C9A:	ld	(ARG1),	hl
		ld	c, 0
		jp	loc_C1E
loc_CA2:	xor	a
		ld	iyu, a
		rst	prnst		; Ausgabe String
		db 8Dh
		ld	c, 2
loc_CA9:	ld	de, 101h
loc_CAC:	ld	ix, (hd_buffer)	; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
		ld	a, d
		call	sub_A43
loc_CB4:	ld	a, (ix+10h)
		and	a
		jr	z, loc_CC9
		xor	a
		cp	iyu
		jr	nz, loc_CF2
		call	sub_9EE
		rst	prnst		; Ausgabe String
		db 0A0h
		dec	c
		jr	nz, loc_CC9
		ld	c, 2
loc_CC9:	ld	a, iyu
		cp	30h		; ?? CmdWr Write Sector
		jr	nz, loc_CD6
		ld	a, (ix+0)
		and	a
		jp	z, loc_B08
loc_CD6:	ld	b, 20h ; ' '
loc_CD8:	inc	ix
		djnz	loc_CD8
		ld	a, (hd_buffer+1) ; HD-Puffer normal 00 FD (AC1)	00 FE (LLC2)
		inc	a
		inc	a
		cp	ixu
		jr	nz, loc_CB4
		inc	d
		ld	a, 8
		cp	d
		jr	nz, loc_CAC
		xor	a
		cp	iyu
		ret	z
		jp	ldsv_usb13

loc_CF2:	ld	hl, (unk_18B1)
		ld	b, 10h
		push	ix
loc_CF9:	ld	a, (hl)
		cp	'"'
		jr	z, loc_D09
		cp	(ix+0)
		jr	nz, loc_D09
		inc	ix
		dec	hl
		djnz	loc_CF9
		xor	a
loc_D09:	pop	ix
		jr	nz, loc_CC9
		ld	a, iyu
		cp	30h		; ?? CmdWr Write Sector
		jp	z, loc_BD8
		ld	e, 0
		call	sub_9EE
		ld	c, (ix+19h)
		call	sub_AB7
		jp	nz, ldsv_usb9
		call	sub_1A5
		jr	nz, loc_D39
		ex	af, af'
		ld	a, c
		ld	b, 0F7h		; ??
		cp	'B'		; Basicprogramm
		jr	z, loc_D37
		cp	'F'		; Datenfeld (Basic)
		ld	b, 0ABh		; ??
		jr	z, loc_D37
		ld	b, 0
loc_D37:	ld	a, b
		ex	af, af'
loc_D39:	exx
		ld	l, (ix+1Bh)
		ld	h, (ix+1Ch)
		ld	e, (ix+1Eh)
		ld	d, (ix+1Dh)
		exx
		ld	l, (ix+13h)
		ld	h, (ix+14h)
		ld	e, (ix+11h)
		ld	d, (ix+12h)
		sbc	hl, de
		inc	hl
		push	hl
		ld	hl, (ARG1)
		ld	a, l
		or	h
		jr	nz, loc_D5F
		ex	de, hl
loc_D5F:	pop	de
		rst	30h		; outhl
		rst	prnst		; Ausgabe String
		db 0A0h
		ld	a, (ix+1Ah)
loc_D66:	push	af
		call	ide_rw		; IDE Read/Write
					; iyl Kommando, e head, dl cylinder, h sector
		pop	af
		dec	a
		jr	nz, loc_D66
		call	sub_1A5
		jr	nz, loc_D7B
		ld	a, c
		cp	'B'		; Basicprogramm
		jr	nz, loc_D7B
		ld	(60D2h), hl	; PROGND End of program
loc_D7B:	dec	hl
		rst	30h		; outhl
		xor	a
		ret

; IDE Read/Write
; in iyl Kommando, e head, dl cylinder, h sector
; in read: de=Anz Bytes

ide_rw:		push	bc
		ld	a, 1		;1-sector transfer
		out	(IdeSCnt), a	;tell IDE drive
		exx
		ld	a, l		;get low order byte
		out	(IdeCLo), a	;send to Task File
		ld	a, d		;get high-order byte
		out	(IdeCHi), a	;send to task file
		ld	a, e		;get head number
		or	0A0h		;+ first IDE disk
		out	(IdeSDH), a	;send to drive!
		ld	a, h		;get sector number
		out	(IdeSNum), a	;send to Task File
		exx
		ld	bc, IdeDat	; B=0, BC has port
		ld	a, iyl
		out	(IdeCmd), a	;send command
ide_rw1:	in	a, (IdeCmd)	;Wait until data available or ready for data
		bit	3, a
		jr	z, ide_rw1
		;
		ld	a, iyl
		cp	CmdWr 		; Write Sector
		jr	z, ide_rw10
;Read min(de,512) Bytes to hl
		ld	c, 2		;2 x 256 Bytes
ide_rw2:	ld	a, e
		or	d
		in	a, (IdeDat)
		jr	z, ide_rw5	; wenn de=0, dann nicht mehr in speicher
					; nur Block zu Ende lesen
		ex	af, af'
		or	a
		jr	z, ide_rw3
		dec	a
		ex	af, af'
		jr	ide_rw4
ide_rw3:	ex	af, af'		; '
		ld	(hl), a		; Byte ablegen
		inc	hl		; nächste Adresse
ide_rw4:	dec	de
ide_rw5:	djnz	ide_rw2
		dec	c
		jr	nz, ide_rw2
;		
ide_rw6:	exx
		call	sub_BB9		; wait while busy
		ld	bc, (hd_heads)
		ld	a, b
		cp	h
		jr	z, ide_rw7
		inc	h
		jr	ide_rw9		
ide_rw7:	ld	h, 1
		ld	a, c
		cp	e
		jr	z, ide_rw8
		inc	e
		jr	ide_rw9
ide_rw8:	ld	e, 0
		inc	l
		jr	nz, ide_rw9
		inc	d
ide_rw9:	exx
		pop	bc
		in	a, (IdeCmd)	;get status
		and	89h
		ret
; Write 512 Bytes from hl
ide_rw10:	otir			;from (HL) out to (C) port. HL++, B--. until B=0
		otir			;write 512 bytes
		jr	ide_rw6


;------------------------------------------------------------------------------
; w IO-Port lesen	liest den Portwert an IO-Port ein und gibt sie aus
;------------------------------------------------------------------------------

		db 0,9,'w',0Dh

		ld	bc, (ARG1)
		in	a, (c)
		jp	OUTHEX

		db 0FFh

;------------------------------------------------------------------------------
; Ausgabe auf V24 (RS232)
; PIO2 A1 Ausgang Daten TxD 
;      A2 Eingang empfangsbereit CTS 
;------------------------------------------------------------------------------

v24out:		push	hl
		push	bc
		push	de
		push	af
		ld	d, a
		ld	a, (kdov24)	; kdov24
		and	7		; Baudrate
		ld	h, hi(bdtab) 	; Baudratentabelle, liegt auf xx00-Adr.
		ld	l, a
		ld	e, (hl)
		ld	a, d
		ld	hl, kdov24
		ld	bc, 7FFFh	; Datenmaske 7F= 7 Bit, FF= 8 Bit
		bit	4, (hl)		; 1=7 Datenbit, + P.ungerade
		jr	nz, v24out2	; ja
		bit	5, (hl)		; 1=7 Datenbit, + P.gerade
		jr	z, v24out3	; nein -> 8 Datenbit
		ld	c, b		; ja: c=7F, sonst c=FF
v24out2:	; parity bestimmen
		and	b		; ggf. 7. Bit löschen
		jp	pe, v24out3
		set	7, a		; Parity setzen
v24out3:	xor	c		; Maske anwenden, bits negieren
		ld	c, a		; c=auszugebendes Zeichen
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
		ld	hl, (cupos)
		di
v24out8:	ld	d, e		; Baudratenzähler		4   Takte
		set	1, a		; b1=1				8
		jr	nc, v24out9	;				12/7
		res	1, a		; b1=0				8 
v24out9:	out	(PIO2DA), a	; A1 Ausgang Daten TxD		(11
		ld	(hl), b		; Zeitverzögerung
		dec	d		;				4
		jr	nz, v24out9	;				12) * E - 5
		srl	c		; c=auszugebendes Zeichen	8
					; nächstes Bit nach Cy
		djnz	v24out8		;				13
					; insg. xx Takte
		ld	(hl), ' '	; cupos
		pop	af
		jr	v24in6
		
		db 0FFh
		
;------------------------------------------------------------------------------
; Eingabe von V24 (RS232)
;------------------------------------------------------------------------------

v24in:		push	hl
		push	bc
v24in0:		push	de
		di
		ld	a, (kdov24)	; Kommandocode V 24
		and	7		; Baudrate
		ld	h, hi(bdtab)	; Baudratentabelle, liegt auf xx00-Adr.
		ld	l, a
		ld	e, (hl)
		ld	hl, (cupos)
		xor	a		; alles 0
		out	(PIO2DA), a	; DTR=0,CTS=0,TxD=0,RxD=0
v24in2:		in	a, (PIO2DA)	; Pegel am Datenport abfragen
		and	1		; nur RxD zulassen (Bits 7-1 ausblenden)
		jr	nz, v24in2	; Dateneingang war L warten auf H-Pegel
		ld	b, e		; Zeitkonstante nach B
v24in3:		ld	(hl), b
		djnz	v24in3		; warten
		ld	b, 9		; 9 Bits empfangen (Start - 8Bit)
v24in4:		ld	d, e
		rr	c		; gelesenes CarryBit ins Bit 7 schieben
v24in5:		ld	(hl), b
		dec	d
		in	a, (PIO2DA)	; Pegelabfrage
		jr	nz, v24in5
		rra			; Bit0=RxD ins Carry schieben
		djnz	v24in4		; weiter bis alle Bits durch
		ld	(hl), ' ' 	; cupos
		ld	a, 0Ah		; DTR=1,CTS=0,TxD=1,RxD=0
		out	(PIO2DA), a	; Bereitschaftsbits setzen
		ld	a, c		; gelesenes Byte in A umladen
v24in6:		pop	de
		pop	bc
		pop	hl
		ei
		ret

		db 0FFh

;------------------------------------------------------------------------------
; usb close file
;------------------------------------------------------------------------------

sub_E84:	ld	a, 0Ah		; Vinculum-Kdo Close
;
sub_E86:	call	sub_6BB		; Kommando mit Filename senden
		jp	EXEC		; Kommando ausfuehren

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

loc_E8C:	call	sub_7BC		; Tausch Buffer-Adr Normal <-> Basic
		res	0, a
		cp	' '
		ret	nz
		pop	af
		; 16 Zeichen von DE nach HL kopieren
		ld	b, 16
loc_E97:	ld	a, (de)
		ld	(hl), a
		inc	hl
		inc	de
		djnz	loc_E97
		;
		ld	a, 16
		cp	b		; b==0
		jr	nz, loc_EA3	; daher immer jr ??
		inc	hl
loc_EA3:	ld	(hl), '"'	; mit " abschließen
		inc	hl
		jp	loc_C5E

;------------------------------------------------------------------------------
; r Basic Warmstart Warmstart Grafik-Sound-Basic via Modul 1
;------------------------------------------------------------------------------

		db 0,9,'r',0Dh

		ld	ix, 5FD5h	; Warmstart
		jr	bko1		; ROM "Basic" aktivieren, Fkt-Tasten laden

		db 0FFh

;------------------------------------------------------------------------------
;UP "Joy".Abfrage Joystick 1,keine Taste Z - Flag
;	    gesetzt und	A = 0,oben bit 0, unten	bit 1,links
;	    bit	2, rechts bit 3, Feuerknopf bit	4 vom Akku
;	    gesetzt.
;------------------------------------------------------------------------------

		org 0EB4h	; fixe Adresse
joy:		push	bc
		in	a, (PIODB)	; Grafik/Ton
		ld	c, a
		res	1, a
		out	(PIODB), a	; Grafik/Ton
		in	a, (PIODA)	; Tastatur
		ld	b, a
		ld	a, c
		out	(PIODB), a	; Grafik/Ton
		ld	a, b
		nop
		and	1Fh
		pop	bc
		ret

;------------------------------------------------------------------------------
; a to hex hl
;------------------------------------------------------------------------------

; zu kdo5
tohex:		push	af
		rra
		rra
		rra
		rra
		call	tohex1		; obere Tetrade
		ld	h, l		; übergeben in H
		pop	af		; untere Tetrade
tohex1:		and	0Fh
		add	a, '0'
		cp	'9'+1		; Ziffer "A" ... "F"?
		jr	c, tohex2	; nein
		add	a, 7		; sonst Korrektur
tohex2:		ld	l, a		; übergeben in L
		ret

;------------------------------------------------------------------------------
; b Basic Start GraficSoundBasic via Modul 1
;------------------------------------------------------------------------------

		db 0,9,'b',0Dh

		ld	ix, 5FCCh	; COLD
bko1:		ld	a, 2		; BASIC
		out	(modul1), a	; Modul1 ROM "Basic" aktivieren
		ld	hl, 5FE0h	; Funktionstasten
		ld	de, 1F80h	; umladen
		ld	bc, 20h
		ldir
		jp	(ix)		; und starten

;------------------------------------------------------------------------------
; T Transfer Kopiert Speicherbereich in anderen Speicherbereich
;------------------------------------------------------------------------------

		db 0,9,'T',0Dh
		call	para		; Argumente uebergeben
		xor	a
		push	hl
		sbc	hl, de
		pop	hl
		; kopieren mit ldir ?
		jr	nc, pko1	; ldir + ret
		; sonst kopieren mit lddr		
		add	hl, bc
		ex	de, hl
		add	hl, bc
		ex	de, hl
		dec	hl
		dec	de
		lddr
		ret

;------------------------------------------------------------------------------
; P Pattern Fuellt Speicherbereich mit Datenbyte
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
; ? Help Liste der verfuegbaren Kommandos
;------------------------------------------------------------------------------

		db 0,9,'?',0Dh

help:		rst	prnst		; Ausgabe String
		db " Help",0BAh
		ld	hl, KDOANFH	; erste Adr, aber der Kdo zu suchen sind
		ld	bc, RAMEND-KDOANFH+1	; Anzahl (RAM-Ende-Startpos.)
help1:		xor	a
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
		rst	outch		; Zeichenausgabe
		rst	prnst		; Ausgabe String
		db 0A0h
		jr	help1

;------------------------------------------------------------------------------
; C Compare Vergleich von 2 Speicherbereichen
; C aaaa bbbb cccc Compare
;------------------------------------------------------------------------------

		db 0,9,'C',0Dh

		call	para		; Argumente uebergeben
cko1:		ld	a, (de)		; bbbb
		cp	(hl)		; vergleich mit aaaa
		jr	z, cko2		; weiter wenn gleich
		; Differenz anzeigen
		; "a-Adr. Wert b-Adr. Wert"
		rst	prnst		; Ausgabe String
		db ' ',0A0h
		rst	30h		; outhl
		rst	prnst		; Ausgabe String
		db "   "
		db 0A0h	;  
		ld	a, (hl)
		call	OUTHEX		; Ausgabe A hexadezimal Wert
		rst	prnst		; Ausgabe String
		db 8Fh
		ex	de, hl
		rst	30h		; outhl
		rst	prnst		; Ausgabe String
		db "   "
		db 0A0h	;  
		ex	de, hl
		ld	a, (de)
		call	OUTHEX
		rst	prnst		; Ausgabe String
		db 8Dh
		rst	inch		; inch; Zeicheneingabe
		cp	0Dh
		ret	nz
cko2:		dec	bc		; Anzahl cccc
		inc	hl		; Adressen erhöhen
		inc	de
		ld	a, b		; Anzahl cccc abgearbeitet?
		or	c
		ret	z		; dann Ende
		jr	cko1		; sonst weiter vergleichen

		db 0FFh
		db 0FFh
		db 0FFh

;------------------------------------------------------------------------------
; p Clear RAM Loescht RAM bis FF00H mit FFH
;------------------------------------------------------------------------------

		db 0,9,'p',0Dh

; RAM loeschen
erase:		ld	hl, erasebeg	; Beginn Löschbereich
		ld	(hl), 0FFh
		ld	de, erasebeg+1
		ld	bc, 0FF00h-erasebeg-1
		ldir
		rst	prnst		; Ausgabe String
		db " Clear RAM",8Dh
		ret

;------------------------------------------------------------------------------
; Filename in ungekehrter Reihenfolge ab unk_18D4 ablegen/tauschen
; ret: de=unk_18D4
;------------------------------------------------------------------------------

; UP zu EX_SA_LO
sub_F9F:	ld	b, 16		; Länge
		ld	hl, filenambuf
		ld	de, unk_18D4
		push	de
loc_FA8:	ld	c, (hl)
		ld	a, (de)
		ld	(hl), a
		ld	a, c
		ld	(de), a
		inc	hl
		dec	de
		djnz	loc_FA8
		pop	de
		ret

;------------------------------------------------------------------------------
; 4 2/4Mhz schaltet AC1 zwischen 2/4Mhz Takt um (BWS-Port noetig)
;------------------------------------------------------------------------------

		db 0,9,'4',0Dh

		in	a, (BWSPORT)
		xor	1
		out	(BWSPORT), a
		ret

;------------------------------------------------------------------------------
; ! BWS Farbe BWS Farbe setzen (COLOR-BWS noetig)
; ! cc Farbbyte setzen
; s.a. Monitorpatch 08.11.2011.LISTING
;------------------------------------------------------------------------------

		db 0,9,'!',0Dh

		ld	a, (ARG1)	; mögliches FarbByte laden
		or	a		; wenn 00H (keine Angabe) dann FarbByte
		jr	nz, setcolor1

; Farb-RAM loeschen
setcolor:	ld	a, vColor	; 0E = bg: schwarz, fg: hellgelb
setcolor1:	push	af		; FarbByte sichern
		xor	a		; noFarbRAM A=00
		ld	(FARBBWS), a	; noFarbRAM einstellen
		pop	af		; FarbByte nach A zurück
		ld	c, BWSPORT
		in	b, (c)		; akt. Portwert einlesen
		inc	b		; FFH wenn kein rücklesbares Register
		ret	z		; Rückkehr weil kein BWSPort gefunden
					; wenn hier dann wurde KEIN FFH gelesen, altes B zurück
		dec	b		; altes B zurück
		ld	d, b		; in B BWS Steuercode für Text RAM
		set	2, d		; in D BWS Steuercode für FarbRAM
		ld	hl, BWSEND
		ld	e, (hl)		; altes TextByte in E merken

		if p_setcol
;Es wird vor dem Einstellen der Vorder- und Hintergrundfarbe
;geprüft, ob es sich um einen Color-BWS handelt. Das passiert auf der RAM-Zelle
;1000h. Wenn diese den gleichen Hex-Wert, wie das bei "!" übergebene Argument hat
;(also z.B. 20h), passiert bisher nix. Das ist hiermit korrigiert worden.
		cpl
		ld 	(hl), a
		out 	(c), d		; FarbRAM an
		cpl
		ld 	(hl), a		; Farbbyte schreiben
		cp 	(hl)		; und vergleichslesen
		out 	(c), b		; Text RAM wieder ein (wegen RET)
		jr 	nz, setcolor2	; kein phys.RAM (Text- oder FarbRAM!)
		cpl
		cp 	(hl)
setcolor2	ld 	(hl),e		; altes Zeichen Text-RAM zurück
		ret 	nz
		out	(c), d		; FarbRAM an
		else
		out	(c), d		; FarbRAM an
		ld	(hl), a		; Farbbyte schreiben
		cp	(hl)		; und vergleichslesen
		out	(c), b		; Text RAM wieder ein (wegen RET)
		ld	(hl), e		; altes Zeichen Text-RAM zurück
		ret	nz		; kein phys.RAM (Text- oder FarbRAM!)
		; wenn hier dann unterschiedliche RAM's! damit gilt der FarbRAM
		; als erkannt, das 1.Byte ist bereits in Farbe
		out	(c), d		; FarbRAM an
		;
		ld	a, (hl)
		cp	e
		out	(c), b		; Text RAM an
		ret	z
		out	(c), d		; FarbRAM an
		endif
		;
		push	bc		; BWSPort und TextRAM ein sichern
		ld	de, BWSEND+1	; 2.Byte FarbRAM
		ld	bc, LINES*COLS-1	; restliche Bytezahl
		ldir			; nun FarbRAM schreiben
		ld	a, (hl)		; FarbByte zurücklesen (von 17FFH)
		pop	bc		; BWSPort und TextRAM ein
		out	(c), b		; Text RAM ein
		ld	(FARBBWS), a	; FarbByte eintragen
		ret

; end of "ROM"


;------------------------------------------------------------------------------
; Arbeitszellen Monitor
;------------------------------------------------------------------------------
; segment "RAM"

		org RAM
		
cupos:		ds 2			; Cursorposition
; Sprungverteiler f. RST-Aufrufe und NMI
; wird initialisiert mit Werten von sv_rst
jp_rst08:	ds 3			; jp	rinch
jp_rst10:	ds 3			; jp	routch
jp_rst18:	ds 3			; jp	rprnst
jp_rst20:	ds 3			; jp	
jp_rst28:	ds 3			; jp	
jp_rst30:	ds 3			; jp	
jp_rst38:	ds 3			; jp	rst38	Einsprung bei Einzelschritt
nmi:		ds 3			; jp	BREAK	NMI: Breakpoint
;Hilfsregister, V24, I/O
soil:		ds 2			; Beginn Eingabezeile	, 0FFFFh
warmcod:	ds 3			; Warmstartcode		, "SCH"
FARBBWS:	ds 1			; Farbcode		, 0
kdov24:		ds 1			; V24-Kontrollregister	, 42h
IOBYT:		ds 1			; Ein/Ausgabebyte	; 11h
			; Eingabe: b0 Tastatur, b1 V24 (Rs 232c), b2 Reserve, b3 User
			; Ausgabe: b4 Bildschirm, b5 V24 (RS 232c), b6 Reserve, b7 User
;
tacod:		ds 1			; Tastencode der zuletzt gedrückten Taste
;Stack
		ds 33h			; Stackbereich
SYSSK:		equ $			; Stack
;
poscnt:		ds 1			; Hilfsregister Kursorpositionierung (CTRL+N)
					; std. 1, nach ctrln 5 (dh 5-1 Ziffern werden erwartet)
repeat:		ds 1			; Hilfsregister	Repetierfunktion Tastatur
data:		ds 2			; Hilfsregister	UP "INLINE"
		ds 1
;Kdo-Argumente
ARG1:		ds 2			; Argument 1
ARG2:		ds 2			; Argument 2
ARG3:		ds 2			; Argument 3

		; org 1861h
;orig hier Registerrettebereich (register save area)
gsbslm:		ds 1			; Methode GSB SAVE/Load	USB HD 00h 20h
hd_buffer:	ds 2			; HD-Puffer normal 00 FD (AC1) 00 FE (LLC2)
hd_bufbas:	ds 2			; HD-Puffer GS-Basic 00	3E
; ab hier wird in erase: geöscht
erasebeg:	equ	$
;
hs_laenge:	ds 2			; Laenge
hs_rest:	ds 2			; Restlänge
hs_gesamt:	ds 2			; Gesamtlänge incl. Header
;
		ds 2

; Headersave-Buffer für sub_95B, 186E-187Dh
unk_186E:	ds 16			; 16 Byte Buffer Filename 

; noch orig. Adressen in RSA / überlagert unk_186E		
		org unk_186E+7
REGPC:		ds 2			; s. BREAK, write only
REGSP:		ds 2			; s. BREAK, write only
; in V11 ungenutzt
BPADR:		ds 2			; RSA: BP
BPOPC:		ds 3			; RSA: Breakpointsequenz

		; org 187Eh
;hsbuf Speicher Save/Load Turbo-Tape/V24 USB
hs_buf:		ds 32 			; Headersave-Kopf-Buffer
hs_aadr:	equ hs_buf+0		; ds 2 aadr
hs_eadr:	equ hs_buf+2		; ds 2 eadr
hs_sadr:	equ hs_buf+4		; ds 2 sadr
hs_cod:		equ hs_buf+6		; ds 6 "MO11.0" / frei belegbar
hs_filetyp:	equ hs_buf+12		; ds 1 Dateityp / Basic-Modus
hs_kennung:	equ hs_buf+13		; ds 3 3 x D3 Kennung Headersave-Kopf 
hs_fileinfo:	equ hs_buf+16		; ds 16 Kommentar
		
; HD-Speicher, überlagert hsbuf
unk_1885:	equ hs_buf+7		; ds 2
unk_1887:	equ unk_1885+2		; ds 2
unk_1889:	equ unk_1885+4		; ds 2
unk_188B:	equ unk_1885+6		; ds 2
unk_188D:	equ unk_1885+8		; ds 1

; 
unk_189E:	ds 16			; Buffer Filename BASIC
		ds 2
;
filetyp:	ds 1			; Dateityp (00, P, B)
unk_18B1:	ds 2
;
SA_LO_BYT:	ds 1			; Methode fuer XX_SAVE/LOAD
					;	  00h =	USB laden
					;	  01h =	USB sichern
					;	  20h =	HD laden
					;	  21h =	HD sichern
; 32 Byte Puffer Filename+Typ+Info
filenambuf:	ds 32			; Puffer fuer SAVE/LOAD-Arbeit
					;	  1 Byte Hochkomma
					;  	 12 Byte Filename (8 . 3)
					;	  1 Byte Hochkomma
					;	  1 Byte Leerzeichen
					;	  1 Byte Dateityp (P)
					;	  1 Byte Leerzeichen
					;	 16 Bytes Header-Info/Kommentar
unk_18D4:	ds 2

; ab 18D6 freier RAM

		org	18F0h
unk_18F0:	ds 3			; User Eingaberoutine
unk_18F3:	ds 3			; User Ausgaberoutine

; end of "RAM"
		end

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------
