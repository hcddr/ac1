	cpu Z80
;********************************************************
;*							*
;*        WILHELM - PIECK - UNIVERSITAET ROSTOCK	*
;*   Institut fuer sozialistische Wirtschaftsfuehrung	*
;*                    EDV - Labor			*
;*							*
;*   FORMAT85			bearbeitete Version 3.4	*
;*							*
;*   Bearbeiter: M. Ohlerich				*
;*		 K.-D. Kirves		V1.0 (26.10.88)	*
;*		 M. Leubner		V3.0-V3.4	*
;*							*
;* FORMAT.Z80			V3.4  Stand: 16.04.2006	*
;*							*
;*	Assemblieren: A>SLRZ80 FORMAT/A			*
;********************************************************
;*							*
; VERSION AC1-2010 / SCCH  Anpassung Ralph Haensel 2011 *
; Rueckuebersetzung V Pohlers 2024			*
;*							*
;********************************************************

; Version 3.5 AC1 (HR 8.8.2011)
; Motorsteuerung AC1-2010 und SCCH via LATCH
; keine Zeichensatzumschaltung
; geaenderte Routinen: 
; DISK, EXIT Bildschirmmode umschalten
; SP40: Motorsteuerung
; FDSEEK, FDSNS, FDCMD, FDNXT, READY geaendert/anders

; neu in Version 3.4:
; - schnelle Formatierung von Festplatten (nur Verzeichnis loeschen)
; - Abbruch-Moeglichkeit mit ^C oder ESC (wird nach jeder Spur abgefragt)

; neu in Version 3.3: (12.04.1998)
; - verbesserte Optionsabfrage

; neu in Version 3.2 (30.08.1996):
; - Rahmen in eingestellter Bildschirmfarbe zeichnen
; - korrekte Formatanzeige mit Beruecksichtigung der Systemspuren
; - Option /q fuer Quickformat - nur Systemspuren und Directory
; - Pseudoformatierung fuer 'Nicht-Diskettenlaufwerke'

; neu in Version 3.1 (19.07.1996):
; - 16 logische Laufwerke zulaessig
; - Parameter in Kommandozeile, dadurch SUBMIT-tauglich
; - Optionen /f:xxx und /u
; - Hilfeseite fuer Kommandozeile

; neu in Version 3.0 (03.03.1996):
; - Umschaltung auf dt. Zeichensatz
; - Linien statt Sternchen fuer Rahmen
; - keine Verwendung von BIOS-Systemzellen
; - KC-Versionskontrolle fuer alle BIOS-Versionen
; - Anzeige der Kapazitaet im Menue
; - Cursor aus beim Formatiervorgang
; - Laufwerk A: (RAM-Floppy) ignorieren
; - Fehlerauswertung nach RESULT-Meldung vom FDC
; - Formatierung nur wenn Diskettenlaufwerk
; - nur gueltige LW-Bezeichner in Eingabezeile uebernehmen
; - 8"-Laufwerke aus Tabellen entfernt
; - Formatierung von 40-Spur-Disketten in 80-Spur-Laufwerken
; - Format 8*512 fuer alle Disketten eingefuehrt
; - optimierte Sektorreihenfolge bei 16*256 und 9*512
; - Formaterkennung und -vorgabe des angewaehlten Laufwerks
; - neue, mit Cursor steuerbare Formatauswahl

;
BDOS	EQU	5		; Aufruf DOS-Funktion
_DCIO	EQU	6	; Direkte Konsolenein-/ausgabe
_PRINT	EQU	9	; Ausgabe Zeichenkette
_GETDSK	EQU	25	; aktuelles Laufwerk holen
FCB1	EQU	5CH		; erster FCB
DEFDMA	EQU	80H		; Standard DMA-Puffer
;
TAB	EQU	09H
LF	EQU	0AH
CLS	EQU	0CH
CR	EQU	0DH
ESC	EQU	1BH
CON	EQU	02H		; Cursor ein
COF	EQU	03H		; Cursor aus
;
CUL	EQU	8		; Tastencode fuer Cursor nach links
CUR	EQU	9		; Tastencode fuer Cursor nach rechts
CUU	EQU	11		; Tastencode fuer Cursor nach oben
CUD	EQU	10		; Tastencode fuer Cursor nach unten
;
; Portadressen:
;
FDC	EQU	040H		; FDC-Basisadresse
FDCS	EQU	FDC		; Chipselect Hauptstatusregister FDC
FDCD	EQU	FDC+1		; Chipselect Datenpuffer FDC
FDCDR	EQU	FDC+2		; DMA-Register FDC
FDCRI	EQU	FDC+4		; Input-Gate
BINT	EQU	6		; Bit fuer FDC-Interrupt
PRDY	EQU	FDCRI		; Port fuer Abfrage Ready-Signal
BRDY	EQU	5		; Bit fuer Ready-Signal
FDCMT	EQU	FDC+6		; Select-Latch (Laufwerkauswahl)
BPREC	EQU	4		; Bit zur Umschaltung Praekompensation
BTAKT	EQU	6		; Bit zur Taktumschaltung auf 8 MHz
FDCTC	EQU	FDC+8		; Terminalcount FDC
;
PIODB		equ	5		; Grafik/Ton
;

POS	MACRO	LINE,COLUMN
	DB	ESC,LINE+84H,COLUMN+80H
	ENDM
;
	
	org	100h

	JP	START		; Programmbeginn

LATCH:       db 45h		; AC1: IO-Adr. Latch 74LS175
				; 45h - AC1, 48h - SCCH 
;
; Copyright und Hilfetext
;
TITEL:	POS	1,6
	DB	'AC1 - Diskettenformat V3.5 Edition:$'

SCCH:	db	1Bh,85h,0A9h,'SCCH$'
AC1:	db	1Bh,85h,0A9h,'AC1 2010$'
TITEL2:	db	1Bh,87h,8Dh,'(c) 08.08.2011   by HR & ML-Soft$'

HLPTXT:	POS	5,3
	DB	'Aufrufmoeglichkeiten:'
	POS	7,3
	DB	'FORMAT'
	POS	7,23
	DB	'- Auswahl ueber Menue'
	POS	8,3
	DB	'FORMAT //'
	POS	8,23
	DB	'- diese Hilfeseite'
	POS	9,3
	DB	'FORMAT d: /Option Option ...'
	POS	10,5
	DB	'Optionen: F:xxx  - Format (800, 780, 720, 640, 624)'
	POS	11,15
	DB	'U      - ohne Abfrage (fuer SUBMIT)'
	POS	12,15
	DB	'Q      - Schnellformatierung (nur SYSTEM+DIR)'
	POS	14,3
	DB	'HINWEIS: Ohne Option F wird das aktuelle Format benutzt.'

ENDMSG:	DB	ESC,95H,80H,CON,'$'
;
; Break-Test (^C oder ESC)
; PE:	-
; PA:	CY=1	BRK/ESC erkannt
; VR:	AF
;
BRKT:	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	E,0FFH
	LD	C,_DCIO		; direkte Konsoleneingabe
	CALL	BDOS
	AND	A
	JR	Z,BRK2		; keine Tasteneingabe vorhanden
	CP	3		; BRK?
	JR	Z,BRK1
	CP	ESC		; ESC?
	JR	Z,BRK1
	db	06		; ld b,n
BRK1:	SCF
BRK2:	POP	BC
	POP	DE
	POP	HL
	RET
;
; Eingabe eines Zeichens mit Upcase, Echo und BRK-Test
; PE:	-
; PA:	A	Zeichencode
; VR:	AF
;
KBD:	PUSH	HL
	PUSH	DE
	PUSH	BC
KBD0:	LD	E,0FFH
	LD	C,_DCIO		; direkte Konsoleneingabe
	CALL	BDOS
	AND	A
	JR	Z,KBD0		; Tasteneingabe abwarten
	CP	3		; BRK?
	JP	Z,EXIT
	CP	ESC		; ESC?
	JP	Z,EXIT
	CP	'a'
	JR	C,KBD1		; < 'a'
	CP	'z'+1
	JR	NC,KBD3		; > 'z'
	AND	0DFH		; Kleinbuchstaben upcasen
KBD1:	CP	20H
	JR	NC,KBD2		; darstellbare Zeichen
	JR	KBD3		; Steuerzeichen
;
; Ausgabe eines Zeichens
; PE:	A	Zeichencode
; PA:	-
; VR:	-
;
CRT:	PUSH	HL
	PUSH	DE
	PUSH	BC
KBD2:	PUSH	AF
	LD	E,A
	LD	C,_DCIO		; direkte Konsolenausgabe
STRG1:	CALL	BDOS
	POP	AF
KBD3:	POP	BC
	POP	DE
	POP	HL
	RET
;
; Zeichenkette ausgeben
; PE:	DE	Zeichenkette
; PA:	-
; VR:	-
;
STRING:	PUSH	HL
	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	C,_PRINT	; Ausgabe Zeichenkette
	JR	STRG1
;
; Dezimalausgabe des Wertes in HL
; PE:	HL	Zahl
; PA:	B	Anzahl angezeigter Zeichen
; VR:	HL,BC,AF
;
HLDEZ:	LD	B,0		; Zeichenzaehler ruecksetzen
HLDEZ0:	PUSH	DE		; bei Rekursion: letzten Rest retten
	PUSH	BC		; Zeichenzaehler
	LD	BC,-10
	LD	DE,-1		; Zaehler fuer Ergebnis (Anzahl Sub's)
HLDEZ1:	ADD	HL,BC
	INC	DE
	JR	C,HLDEZ1	; DE = HL/10
	LD	BC,10
	ADD	HL,BC		; letzten Abzug korrigieren, HL = Rest
	POP	BC
	EX	DE,HL		; 10er / Rest tauschen
	LD	A,H
	OR	L		; Zehner = 0 ?
	CALL	NZ,HLDEZ0	; Rekursion wenn Zehner vorhanden
	LD	A,E
	ADD	A,'0'		; Rest -> ASCII in A
	CALL	CRT		; Ziffer ausgeben
	POP	DE		; voriger Rest
	INC	B		; Zeichen zaehlen
	RET
;
; Bestaetigung einer JA/NEIN-Abfrage
; PE:	DE	Frage
; PA:	Z=1	Ja
;	Z=0	Nein
;
JANEIN:	CALL	STRING		; Meldung anzeigen
JN1:	CALL	KBD
	CP	'J'		; ja?
	JR	NZ,JN2
	LD	A,'a'		; 'Ja' ausschreiben
	CALL	CRT
	RET
	;
JN2:	CP	'N'		; nein?
	JR	NZ,JN3
	ld	de,ein
	call	string		; 'Nein' ausschreiben
	AND	A		; Z=0
	RET
	;
JN3:	CP	20H
	LD	A,7FH		; darstellbare Zeichen wieder loeschen
	CALL	NC,CRT
	JR	JN1		; Eingabe wiederholen
	;
ein:	db	'ein$'
;
; DOS-Laufwerk wieder im BIOS einstellen
;
DOSLW:	LD	A,(DOSDRV)
	LD	C,A		; und in SELDSK reinlaufen
;
; Aufruf der BIOS-Funktionen
;
SELDSK:	LD	E,18H		; Laufwerk einstellen
	DB	21H		; LD HL,nn
	;
SETTRK:	LD	E,1BH		; Spur einstellen
	DB	21H		; LD HL,nn
	;
SETSEC:	LD	E,1EH		; Sektor einstellen
	DB	21H		; LD HL,nn
	;
SETDMA:	LD	E,21H		; DMA-Adresse einstellen
	DB	21H		; LD HL,nn
	;
WRITE:	LD	E,27H		; Sektor schreiben
	LD	D,0
	LD	HL,(1)		; Adresse WBOOT
	ADD	HL,DE
	JP	(HL)		; Aufruf der BIOS-Funktion
;
; Hilfeseite anzeigen
;
HELP:	CALL	BOX		; CLS, Rahmen zeichnen
	LD	DE,HLPTXT	; Hilfeseite
	CALL	STRING		; anzeigen
	JP	EXIT		; und tschuess
;
; ----- Programmstart -----
;
START:	LD	(STACK),SP	; alten Stack ablegen
	LD	SP,STACK	; eigenen Stack definieren
	XOR	A
	LD	(CMDLN),A
	LD	(HARD),A
	LD	(NOASK),A	; Flags ruecksetzen (fuer GO!)
	LD	(QUICK),A
	LD	C,_GETDSK	; aktuelles Laufwerk holen
	CALL	BDOS
	LD	(DOSDRV),A	; und merken
	LD	C,0		; Laufwerk A:
	CALL	SELDSK
FIND1:	LD	A,(HL)
	CP	'5'
	JR	NZ,FIND2
	DEC	HL
	LD	A,(HL)
	INC	HL
	CP	'8'
	JR	Z,FIND3		; Kennung '85' erkannt
FIND2:	DEC	L
	JR	NZ,FIND1	; weitersuchen bis L=0
BADVER:	CALL	DOSLW		; wieder DOS-Laufwerk einstellen
	LD	DE,VERMSG	; falsches System!
	CALL	STRING
	JP	EXIT0
	;
FIND3:	LD	C,1		; Laufwerk B:
	CALL	SELDSK
	LD	A,H
	OR	L
	JR	Z,BADVER	; Laufwerk B: nicht definiert
	LD	DE,10
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)		; DPB-Adresse
	EX	DE,HL
	DEC	HL
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)		; Treiberadresse IODISK
	LD	(IODSK),DE
	CALL	DOSLW		; wieder DOS-Laufwerk einstellen
;
; Analyse der Kommandozeile
;
	LD	HL,DEFDMA	; Kommandozeile
	LD	B,(HL)		; Anzahl Zeichen
	INC	B
CMDL0:	INC	HL
	DEC	B
	JP	Z,LOOP		; Programmschleife, da ohne Parameter
	LD	A,(HL)
	CP	' '
	JR	Z,CMDL0		; Leerzeichen uebergehen
	CP	TAB
	JR	Z,CMDL0		; Tabulatoren auch
	LD	A,(FCB1)	; Laufwerk angegeben?
	AND	A
	JP	Z,HELP		; HILFE, Laufwerk nicht angegeben!
	LD	(CMDLN),A	; Kommandozeilenmodus (A<>0)
	DEC	A		; Laufwerk 0...15
	PUSH	HL
	CALL	SELECT		; Laufwerk anwaehlen
	POP	HL
	JP	NC,ERRC1	; Laufwerk nicht installiert
	JR	NZ,CMDL1	; nicht Diskette
	LD	A,(TYP)		; Laufwerkstyp
	CP	3
	JP	NZ,ERRC2	; kein 1.6er Laufwerk
CMDL1:	LD	A,(HL)
	INC	HL
	AND	A		; ENDE-Null?
	JR	Z,CMDL5		; fertig mit Kommandozeile
	CP	'/'
	JR	NZ,CMDL1	; vor bis zur Optionskennung
CMDLH:	LD	A,(HL)
	CP	'/'		; Option //
	JP	Z,HELP
	CP	'?'		; Option /?
	JP	Z,HELP
CMDL2:	LD	A,(HL)		; naechste Option holen
	INC	HL
	AND	A
	JR	Z,CMDL5		; fertig
	CP	' '
	JR	Z,CMDL2		; Leerzeichen uebergehen
	CP	TAB
	JR	Z,CMDL2		; TABs auch
	CP	'/'
	JR	Z,CMDLH
	CP	'U'		; Option /U
	JR	NZ,NOTU
	LD	(NOASK),A	; Unterdrueckung Sicherheitsabfrage
	JR	CMDL2
	;
NOTU:	CP	'Q'		; Option '/Q
	JR	NZ,NOTQ
	LD	(QUICK),A	; Schnellformatierung einstellen
	JR	CMDL2
	;
NOTQ:	CP	'F'		; Option /F
	JP	NZ,ERRC3	; falsche Option!
	LD	A,(HL)
	INC	HL
	CP	':'
	JP	NZ,ERRC3	; Doppelpunkt fehlt!
	CALL	GETDEZ		; Dezimalzahl holen
	PUSH	HL
	LD	HL,FMTAB	; Tabelle
	LD	B,5		; 5 definierte Formate
CMDL3:	LD	A,(HL)
	INC	HL
	CP	E
	JR	NZ,CMDL4	; Low falsch
	LD	A,(HL)
	CP	D
	JR	NZ,CMDL4	; High falsch
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	HL,DPB
	EX	DE,HL
	LD	BC,20		; 20 Bytes aus dem
	LDIR			; vorbereiteten DPB uebernehmen
	POP	HL
	JR	CMDL2
	;
CMDL4:	INC	HL		; Rest von Tabelle uebergehen
	INC	HL
	INC	HL
	DJNZ	CMDL3		; alle Formate abarbeiten
	POP	HL
	JP	ERRC3		; kein gueltiges Format!
	;
CMDL5:	CALL	SELTRB
	JP	NZ,ERRC4	; kein Diskettenlaufwerk!
	CALL	DEFLT		; Defaultformat holen
	LD	DE,TXT5		; Laufwerk
	CALL	STRING
	LD	A,(FCB1)
	ADD	A,40H
	CALL	CRT		; X:
	LD	DE,TXT6		; mit
	CALL	STRING
	LD	HL,(DSM)
	INC	HL		; Anzahl Bloecke/Diskette
	LD	A,(BSH)
	SUB	3
	JR	Z,CMDL7		; 1K-Bloecke
	LD	B,A
CMDL6:	ADD	HL,HL
	DJNZ	CMDL6
CMDL7:	CALL	HLDEZ		; Kapazitaet anzeigen
	LD	A,'K'
	CALL	CRT
	LD	DE,TXT7		; schnell
	LD	A,(QUICK)
	AND	A
	CALL	NZ,STRING
	LD	DE,TXT8		; formatieren.
	CALL	STRING
	LD	A,(NOASK)
	AND	A		; Abfrage?
	JR	NZ,EXEC
	LD	DE,CNFTXT	; richtig?
	CALL	JANEIN
	JP	NZ,EXIT		; NEIN
EXEC:	LD	DE,CRLF		; neue Zeile
	CALL	STRING
	JP	JUMP		; weiter wie bei Menueauswahl
;
; Versuch, Dezimalzahl zu decodieren
; PE:	HL	Zeichenkette
; PA:	HL	zeigt auf Zeichen nach Zahl
;	DE	Wert der Zahl
;
GETDEZ:	LD	DE,0		; Startwert
	OR	A		; CY=0
DEZN:	LD	A,(HL)
	SUB	'0'
	RET	C		; Ende, da < '0'
	CP	10
	RET	NC		; Ende, da > '9'
	INC	HL		; Zeiger auf naechstes Zeichen
	EX	DE,HL
	ADD	HL,HL		; *2
	LD	B,H
	LD	C,L
	ADD	HL,HL		; *4
	ADD	HL,HL		; *8
	ADD	HL,BC		; *10
	LD	B,0
	LD	C,A
	ADD	HL,BC		; neuen Wert addieren
	EX	DE,HL
	JR	DEZN
;
; Titel ausgeben
;
sub_4DD:
	push    de
        ld      de, asc_A21     ; CLS
        call    STRING
        pop     de
        ret

BOX:    call    sub_4DD		; Bildschirm loeschen
        ld      de, TITEL       ; "Diskettenformat V3.5 Edition:$"
        call    STRING
        ld      de, SCCH        ; "SCCH$"
        ld      a, (LATCH)
        cp      48H		; SCCH?
        jr      z, BOX1
        ld      de, AC1         ; "AC1 2010"
BOX1:	call    STRING
        ld      de, TITEL2      ; "(c) 08.08.2011   by HR & ML-Soft$"
        call    STRING
        ret

;
; Menueschleife:
;
LOOP:	CALL	BOX		; CLS, Rahmen zeichnen
	;
LOOP0:	LD	DE,POSMSG	; Statuszeile
	CALL	STRING
	LD	B,50
	LD	A,' '		; loeschen
LOOP1:	CALL	CRT
	DJNZ	LOOP1
	XOR	A		; Diskettenlaufwerk
	LD	(HARD),A
LOOPI:	LD	DE,LUNMSG	; welches Laufwerk?
	CALL	STRING
	CALL	KBD
	CP	CR
	JR	Z,LOOP2		; alten Wert bestaetigen
	CP	'A'
	JR	C,LOOPI		; zu klein
	CP	'P'+1
	JR	NC,LOOPI	; zu gro~
	LD	(LUN),A		; Eingabe in Zeichenkette uebernehmen
LOOP2:	LD	A,(LUN)
	SUB	'A'
	JR	C,LOOPI		; zu klein, z.B. Anfangswert
	CALL	SELECT		; Laufwerk waehlen
	JR	NC,LOOPI	; Laufwerk falsch
	JR	NZ,LOOPH	; kein Diskettenlaufwerk
	LD	A,(TYP)		; Laufwerkstyp
	AND	A
	JR	Z,LOOP3
	CP	4		; nur Typen 1, 2 oder 3 definiert
	JR	C,LOOP4
LOOP3:	LD	DE,ERRMSG	; nicht def. LW-Typ
	CALL	STRING
	CALL	KBD		; Eingabe abwarten
	JR	LOOP0
	;
LOOPH:	LD	DE,NODISK	; Warnung! kein Diskettenlaufwerk
	CALL	JANEIN
	JR	NZ,LOOP		; nicht bestaetigt
	LD	(HARD),A	; Nicht-Diskettenlaufwerk einstellen
	ld	de,quickmsg	; Schnellformatierung?
	call	janein
	jr	nz,loopnq
	ld	(quick),a
loopnq:	JR	LOOP5		; und nocheinmal kontrollieren
	;
LOOP4:	CALL	DISK		; Disketten- und Formatauswahl
LOOP5:	LD	DE,CNFMSG	; richtig?
	CALL	JANEIN
	JP	NZ,LOOP		; Auswahl neu starten
	LD	DE,POSMSG
	CALL	STRING		; Cursor in Statuszeile
	LD	A,(HARD)
	AND	A
	JR	NZ,CALX		; keine Diskette
;
; Format-Tabelle waehlen und in IX laden
; (Einsprungstelle aus Kommandozeile)
;
JUMP:	LD	A,(ART)		; Diskettentyp 0...3
	ADD	A,A
	ADD	A,A
	LD	C,A		; je Typ 4 Moeglichkeiten
	LD	A,(FRM)		; Format 1...4
	ADD	A,C		; Formatnummer 1...16
	LD	HL,FMTTAB-12
	LD	DE,12		; Laenge eines Eintrages
LOOP6:	ADD	HL,DE
	DEC	A
	JR	NZ,LOOP6
	PUSH	HL
	POP	IX		; Format-Tabelle in IX laden
;
; Fuelldaten fuer Pseudoformatierung in DMA-Puffer bereitstellen:
;
CALX:	LD	HL,DEFDMA
	LD	B,128
FILL:	LD	(HL),0E5H	; Fuelldaten vorbereiten
	INC	HL
	DJNZ	FILL
;
; Spuranzahl fuer beliebiges Laufwerk berechnen
;
	LD	A,(QUICK)
	AND	A
	JR	NZ,CALQ		; -> Schnellformatierung
	LD	L,(IX+5)
	LD	H,0		; Spuranzahl Diskettenformat
	LD	A,(HARD)
	AND	A
	JR	Z,CALQ3		; Spuranzahl fuer ganze Diskette
	LD	HL,(DSM)
	INC	HL		; Anzahl Bloecke (einschlie~lich DIR)
	LD	DE,0		; DEHL - 32 Bit
	LD	A,(BSH)
	LD	B,A		; Blockschiebefaktor
DRV1:	SLA	L
	RL	H
	RL	E
	RL	D
	DJNZ	DRV1		; Multiplikation mit Blockgroe~e
	LD	BC,(SPT)
	DEC	BC		; aufrunden falls letzte Spur
	ADD	HL,BC		; nicht voll mit Bloecken belegt ist
	JR	NC,DRV2
	INC	DE
DRV2:	PUSH	HL
	LD	HL,0		; Spurzaehler
	EX	(SP),HL		; auf Stack
DRV3:	LD	BC,(SPT)
	AND	A
	SBC	HL,BC		; 16 Bit SUB
	EX	DE,HL
	LD	BC,0
	SBC	HL,BC		; 32 Bit Sub
	EX	DE,HL
	JR	C,DRV4		; alle Sektoren untergebracht
	EX	(SP),HL
	INC	HL		; Spuren zaehlen
	EX	(SP),HL
	JR	DRV3
	;
DRV4:	POP	BC
	JR	CALQ2		; Systemspuren addieren
;
; Spuranzahl fuer Quickformat berechnen
;
CALQ:	LD	HL,(DRM)
	INC	HL		; Anzahl Verzeichniseintraege
	SRL	H
	RR	L
	SRL	H
	RR	L		; Anzahl Sektoren/DIR
	LD	BC,0
	LD	DE,(SPT)	; Sektoren/Spur
	AND	A
CALQ1:	INC	BC		; erforderliche Spuren zaehlen
	SBC	HL,DE
	JR	Z,CALQ2
	JR	NC,CALQ1
CALQ2:	LD	HL,(OFF)
	ADD	HL,BC		; Systemspuren mit formatieren
CALQ3:	LD	(TRACKS),HL	; Anzahl Spuren
;
; weitere Parameter eintragen
;
	LD	A,(HARD)
	AND	A
	JR	NZ,DRV5		; nicht im Pseudo-Modus!
	RES	4,(IX+7)
	LD	A,(TYP)
	DEC	A
	JR	Z,LOOP7		; echtes 40-Spur-Laufwerk
	LD	A,(ART)
	RRA
	JR	C,LOOP7		; DD-Format
	SET	4,(IX+7)	; 40 Spuren simulieren
LOOP7:	LD	A,(PUN)
	LD	(IX+6),A	; physische Einheit eintragen
	CALL	FDCOUT		; Diskettenlaufwerk starten
	JR	Z,ERROR		; Laufwerk nicht bereit
DRV5:	LD	A,COF
	CALL	CRT		; Cursor aus
	CALL	INIT		; Formatierung
	PUSH	AF		; Status retten
	LD	A,CON
	CALL	CRT		; Cursor ein
	POP	AF
	LD	DE,OKMSG	; Fertigmeldung
	JR	Z,LOOP8		; kein Fehler
ERROR:	ADD	A,A
	LD	D,0
	LD	E,A
	LD	HL,ERRTAB	; Tabelle der Fehlertexte
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
LOOP8:	PUSH	DE		; Meldung retten
	XOR	A
	ld      bc, (LATCH)
	out     (c), a		; Motor aus
	LD	A,(CMDLN)
	AND	A		; aus Kommandozeile?
	JR	NZ,ERRC0	; ja, dann anzeigen und fertig
	LD	DE,POSMSG	; Position Statuszeile
	CALL	STRING
	POP	DE		; Meldung
	CALL	STRING
	LD	DE,REPMSG	; nochmal?
	CALL	JANEIN
	JP	Z,LOOP		; Wiederholung
	JR	EXIT
;
; Ende mit (Fehler-)Meldung
;
ERRC0:	LD	DE,CRLF		; neue Zeile
	CALL	STRING
	POP	DE		; Meldung
	JR	ERRCX
	;
ERRC1:	LD	DE,TXT1		; Laufwerk nicht installiert
	JR	ERRCX
	;
ERRC2:	LD	DE,TXT2		; falscher Laufwerkstyp
	JR	ERRCX
	;
ERRC3:	LD	DE,TXT3		; falsche Option
	JR	ERRCX
	;
ERRC4:	LD	DE,TXT4		; kein Diskettenlaufwerk
	;
ERRCX:	CALL	STRING		; Zeichenkette anzeigen
;
; Programmende
;
EXIT:	LD	A,(CMDLN)
	AND	A		; Kommandozeilenmodus?
	LD	DE,ENDMSG
	CALL	Z,STRING	; nein, dann Cursor in letzte Zeile
	CALL	DOSLW		; DOS-Laufwerk anwaehlen
EXIT0:	in      a, (PIODB)
	res     3, a		; B3 Bildschim-Mode
	out     (PIODB), a
	LD	SP,(STACK)	; SP regenerieren und
	RET			; zurueck zum rufenden Programm
;
; Diskettenlaufwerk waehlen
; PE:	A	Laufwerk (A=0, B=1, C=2 ...)
; PA:	CY=0	Laufwerk falsch
;	Z=0	kein Diskettenlaufwerk
;	DPB	Parameter des Laufwerks
;
SELECT:	LD	(DRIVE),A	; Laufwerk ablegen
	LD	C,A
	CALL	SELDSK		; gewuenschtes Laufwerk auswaehlen
	PUSH	HL
	CALL	DOSLW		; sofort wieder DOS-Laufwerk einstellen
	POP	HL
	LD	A,H		; DPH-Adresse
	OR	L
	RET	Z		; LW nicht installiert!
	LD	DE,10
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)		; Adresse DPB
	EX	DE,HL
	DEC	HL
	DEC	HL		; zurueck bis Treiber
	LD	DE,TRB
	LD	BC,28
	LDIR			; DPB komplett kopieren
	LD	A,(TYP)		; Laufwerkstyp
	AND	0FH
	LD	(TYP),A		; zusaetzliche Optionen abschneiden
SELTRB:	LD	HL,(IODSK)
	LD	DE,(TRB)
	AND	A
	SBC	HL,DE
	SCF			; OK
	RET			; Z=1 wenn Diskettenlaufwerk
;
; Auswahl des Diskettentyps
; PA:	ART	Diskettentyp 0...3
;	FRM	gewaehltes Format 1...4
;
DISK:	CALL	DEFLT		; aktuelle Installation holen
	in      a, (PIODB)
	set     3, a		; B3 Bildschim-Mode (INVERS)
	out     (PIODB), a
	LD	DE,FRMT0
	CALL	STRING		; allgemeine Formatliste
DISK1:	LD	DE,ART1
	LD	A,(MAX)
	LD	B,A
DISK2:	CALL	STRING		; moegliche Diskettentypen auflisten
	LD	HL,ART2-ART1
	ADD	HL,DE		; naechster String
	EX	DE,HL
	DJNZ	DISK2
	LD	A,(ART)		; aktueller Diskettentyp
	LD	B,A
	INC	B
	LD	A,122
DISK3:	ADD	A,13
	DJNZ	DISK3
	LD	(MARK1+2),A	; Position fuer linke Markierung
	ADD	A,7
	LD	(MARK2+2),A	; Position fuer rechte Markierung
	LD	DE,MARK1
	CALL	STRING		; gewaehltes Format hervorheben
	LD	A,(ART)
	LD	B,A
	INC	B
	LD	HL,FRMT1-FRMT2+FRMT1
	LD	DE,FRMT2-FRMT1
DISK4:	ADD	HL,DE
	DJNZ	DISK4
	EX	DE,HL
	CALL	STRING		; moegliche Formate ergaenzen
DISK5:	LD	A,(FRM)
	ADD	A,139
	LD	(MARK3+1),A	; Position fuer linke Markierung
	LD	(MARK4+1),A	; Position fuer rechte Markierung
	LD	DE,MARK3
	CALL	STRING		; gewaehltes Format hervorheben
DISK6:	LD	E,0FFH
	LD	C,_DCIO		; direkte Konsoleneingabe
	CALL	BDOS
	CP	3
	JP	Z,EXIT		; Abbruch
	CP	ESC
	JP	Z,EXIT		; Abbruch
	CP	CR
	JR	NZ,DISK7
	RET
	;
DISK7:	CP	CUL
	JR	NZ,DISK9
	LD	A,(ART)
	AND	A
	JR	Z,DISK6		; bereits ganz links
	DEC	A
DISK8:	LD	(ART),A
	JR	DISK1		; neu auflisten
	;
DISK9:	CP	CUR
	JR	NZ,DISK10
	LD	A,(ART)
	INC	A
	LD	HL,MAX
	CP	(HL)
	JR	Z,DISK6		; bereits ganz rechts
	JR	DISK8
	;
DISK10:	CP	CUU
	JR	NZ,DISK12
	LD	A,(FRM)
	DEC	A
	JR	Z,DISK6		; bereits erstes Format
DISK11:	LD	(FRM),A
	LD	DE,UNMARK	; Markierungen loeschen
	CALL	STRING
	JR	DISK5		; neu markieren
	;
DISK12:	CP	CUD
	JR	NZ,DISK6	; falsche Taste
	LD	A,(FRM)
	CP	4
	JR	Z,DISK6		; bereits letztes Format
	INC	A
	JR	DISK11
;
; aktuelle Installation des Laufwerks holen
; PE:	DPB	Diskettenparameterblock
; PA:	MAX	max. moeglicher Diskettentyp
;	ART	akt. Diskettentyp (0..3)
;	FRM	Nummer des akt. Formats (1..4)
;
DEFLT:	LD	A,(TYP)		; Laufwerkstyp 1-3
	CP	3		; MAX=1 bei Laufwerk 1.2
	JR	C,SETMAX	; MAX=2 bei Laufwerk 1.4
	INC	A		; MAX=4 bei Laufwerk 1.6
SETMAX:	LD	(MAX),A
	LD	HL,ART
	LD	(HL),0		; SS/SD
	LD	A,(NTR)
	CP	40
	JR	Z,NOSET0
	SET	0,(HL)		; DD (80 Spuren)
NOSET0:	LD	A,(PSH)
	LD	B,A
	LD	A,(EOT)
CALSPT:	ADD	A,A
	DJNZ	CALSPT		; Anzahl log. Sektoren/Spur berechnen
	LD	BC,(SPT)
	CP	C
	JR	Z,NOSET1
	SET	1,(HL)		; DS (2-seitig)
NOSET1:	LD	A,(EOT)
	LD	C,1
	CP	16		; 16*256?
	JR	Z,SETFRM
	INC	C
	CP	8		; 8*512?
	JR	Z,SETFRM
	INC	C
	CP	9		; 9*512?
	JR	Z,SETFRM
	INC	C
SETFRM:	LD	A,C		; Standard-Format des gewaehlten Laufwerks
	LD	(FRM),A
	RET
;
; Formatieren einer gesamten Diskette
; PA:	Z=1	kein Fehler aufgetreten
;	A	Fehlercode, wenn Z=0
;
INIT:	LD	DE,TRKMSG	; Formatierte Spur:
	CALL	STRING
	LD	HL,0		; HL=Spur
	LD	E,0		; E=Kopf
	LD	A,(HARD)
	AND	A
	JR	Z,INIT1
	LD	HL,(OFF)	; Beim Loeschen SYSTEM erhalten!
INIT1:	PUSH	HL
	CALL	HLDEZ		; Spurnummer anzeigen
INIT2:	LD	A,8
	CALL	CRT		; Cursor zurueck
	DJNZ	INIT2
	POP	HL
	PUSH	HL
	LD	A,(HARD)
	AND	A
	JR	Z,INIT3		; Diskette
	CALL	BIOS		; eine Spur im BIOS formatieren
	POP	HL
	RET	NZ		; Fehler aufgetreten
	JR	INIT4
	;
INIT3:	PUSH	DE
	CALL	TRACK		; eine Spur formatieren
	POP	DE
	POP	HL
	RET	NZ		; Fehler aufgetreten
	LD	A,E
	OR	A
	JR	NZ,INIT4	; Seite 1 war schon drann
	LD	A,(IX+7)
	AND	7
	CP	3
	JR	NZ,INIT4	; einseitiges Format
	LD	E,1		; Kopf 1
	JR	INIT1		; in gleicher Spur
	;
INIT4:	call	brkt		; ESC oder ^C gedrueckt?
	ld	a,5		; Fehlercode "break"
	ret	c
	LD	E,0		; Kopf 0
	INC	HL		; in naechster Spur
	LD	BC,(TRACKS)
	AND	A
	SBC	HL,BC		; letzte Spur?
	ADD	HL,BC
	JR	NZ,INIT1
	XOR	A		; kein Fehler aufgetreten
	RET
;
; Formatierung einer Spur
; PE:	IX	Formatierungstabelle
;	HL	Spurnummer
;	E	Kopfnummer
; PA:	Z=1	kein Fehler aufgetreten
;	A	Fehlercode, wenn Z=0
;
TRACK:	LD	D,L		; 8-Bit-Spurnummer bei Diskette
	PUSH	DE		; D=Spur, E=Kopf
	LD	A,(IX+3)	; GAP3-Luecke
	LD	(FMTGAP),A
	LD	A,(IX+0)	; PSH
	LD	(FMTN),A
	LD	C,A
	LD	A,(IX+2)	; EOT
	LD	(FMTEOT),A
	LD	B,A
	RLCA
	RLCA			; 4 Byte je phys. Sektor
	ld      (byte_1161), a
	DEC	A
	LD	E,A
	LD	D,0
	LD	HL,IDTAB
	ADD	HL,DE		; Ende der ID-Tabelle berechnen
	POP	DE
	LD	A,B
	LD	IY,SECT5	; 5*1025
	CP	5
	JR	Z,SETID
	LD	IY,SECT8	; 8*512
	CP	8
	JR	Z,SETID
	LD	IY,SECT9	; 9*512
	CP	9
	JR	Z,SETID
	LD	IY,SECT16	; 16*256
;
; Aufbau ID-Tabelle
;
SETID:	LD	(HL),C		; N (Sektorgroe~e)
	DEC	HL
	LD	A,(IY)
	INC	IY
	LD	(HL),A		; R (Sektornummer)
	DEC	HL
	LD	(HL),E		; H (Kopfseite)
	DEC	HL
	LD	(HL),D		; C (Spurnummer)
	DEC	HL
	DJNZ	SETID		; fuer jeden Sektor
	ld      hl, IDTAB
	LD	A,D		; Spur
	BIT	4,(IX+7)
	JR	Z,SP40
	ADD	A,A		; 40-Spur-Laufwerk simulieren
SP40:	LD	(CYL),A		; Spur fuer Kopfpositionierung
	LD	A,E
	RLCA			; Kopf in Bit 2
	RLCA
	OR	(IX+6)		; phys. Einheit
	LD	(DR),A		; Laufwerk, Seite (fuer SEEK-Befehl)
	LD	(FMTDR),A	; Laufwerk, Seite (fuer FORMAT-Befehl)
	;
	ld      b, 10		; 10 Versuche
loc_8A9:
	push	bc
	CALL	FDSEEK
	pop	bc
	jr	z, loc_8B6
	djnz    loc_8A9
	
	LD	A,2
	or	a
	ret			; A=2: SEEK-Error
loc_8B6:
	LD	HL,IDTAB
	LD	BC,0
	PUSH	BC
	PUSH	HL
	LD	HL,FMTCMD	; Format-Kommando
	LD	B,6
	DI
	CALL	FDK2		; Kommando ausgeben
	POP	HL
	POP	DE
	ld      a, (byte_1161)
	ld      b, a
	LD	C,FDCD
	call    FDK4
	
        ld      a, (ix+6)	; IX= Formattab, +6 = phys. Einheit PUN
        and     3
        jr      z, loc_8E1	; wenn LW 0
        cp      2		; oder LW 2
        jr      z, loc_8E1
        ld      a, 18h		; LW 1+3: BIT 3 - Motor 1, 0 - AUS, 1 - EIN
        jr      loc_8E3
loc_8E1:
        ld      a, 11h		; LW 0+2: BIT 0 - Motor 0, 0 - AUS, 1 - EIN
loc_8E3:
        push    bc
        ld      bc, (LATCH)	; Motor an
        out     (c), a
        pop     bc

	EI
	LD	B,7		; Ergebnisphase, 7 Byte abholen
	LD	HL,RESBYT
FDCR1:	CALL	FDNXT
	LD	(HL),A
	INC	HL
	DJNZ	FDCR1
	LD	HL,RESBYT
	LD	A,(HL)
	AND	0C0H		; Fehler aufgetreten?
	RET	Z		; O.k.
	XOR	A
	BIT	3,(HL)
	RET	NZ		; A=0: Laufwerk nicht bereit
	INC	HL
	INC	A
	BIT	1,(HL)
	RET	NZ		; A=1: Schreibschutz
	INC	A
	INC	A
	RET			; A=3: defekte Spuren
;
; Spur 0 anfahren (recalibrate drive)
;
FDREC:	LD	BC,207H		; RECALIBRATE
	CALL	FDCMD
	CALL	FDS2		; status
	RET	Z
	LD	BC,207H		; repeat, if 'EC'
	CALL	FDCMD
	JR	FDS2
;
; Spursuche (seek track)
;
FDSEEK:	LD	A,(CYL)
	cp	2
	call	c,FDREC
	LD	BC,30FH		; SEEK
	CALL	FDCMD
;
; Interruptstatus testen
;
FDS2:
FDSNS:	LD	BC,108H		; SENSE INTERRUPT STATUS
	CALL	FDCMD
	CALL	FDNXT
	LD	B,A
	LD	(RESBYT),A
	cp      80h
	call    nz, FDNXT
	bit     5, b
	jr      z, FDSNS
	LD	A,B
	AND	0F0H
	CP	0C0H
	jr      z, FDNXT	; RDY changed
	XOR	20H		; seek o.k.
	RET
;
; Kommando zum FDC aussenden
;
FDCMD:	LD	HL,CMD
	LD	(HL),C		; Kommandocode eintragen
FDK2:	LD	C,FDCD
	;
        push    de	
loc_952:
	ld      de, 3FFFh	; wait
loc_955:ld      a, e
        or      d
        jr      z, loc_968
        ;
	in      a, (FDCS)
	AND	0C0H
	CP	80H		; bereit?
        dec     de
        jr      nz, loc_955
        outi
        jr      nz, loc_952
        ld      b, 0
loc_968:pop     de
        ret
;
; naechstes Byte vom Resultat abholen
;
FDNXT:	push    hl
        ld      hl, 0FFFh
loc_96E:
	ld      a, l
        or      h
        ld      a, 0C0h
        jr      z, loc_97F
        in      a, (FDCS)
        and     0C0h
        cp      0C0h
        dec     hl
        jr      nz, loc_96E
        in      a, (FDCD)
loc_97F:
	pop     hl
        ret
;
;
;
FDK4:	IN	A,(FDCS)
	rlca
	jr	nc,FDK4
	outi
	JR	NZ,FDK4
	RET
;
FDCOUT:	ld      a, (ix+6)
        and     3
        jr      z, loc_99A
        cp      2
        jr      z, loc_99A
        ld      a, 8
        jr      loc_99C
;
loc_99A:
	ld      a, 1
loc_99C:
	push    bc
        ld      bc, (LATCH)
        out     (c), a
        pop     bc
        ld      hl, 0
loc_9A7:
	inc     hl
        ld      a, h
        or      l
        ret     z
        push    bc
        ld      bc, 204h
        call    FDCMD
        call    FDNXT
        pop     bc
        bit     5, a
        ret     nz
        ld      b, 4
loc_9BB:
	djnz    loc_9BB
        jr      loc_9A7

;
; Pseudoformatierung einer Spur (BIOS-WRITE)
; PE:	HL	Spurnummer
; PA:	Z=1	kein Fehler aufgetreten
;	A	Fehlercode 4, wenn Z=0
;
BIOS:	PUSH	HL		; Spurnummer
	LD	A,(DRIVE)
	LD	C,A
	CALL	SELDSK		; Laufwerk anwaehlen
	LD	BC,DEFDMA
	CALL	SETDMA		; DMA-Adresse einstellen
	POP	BC
	CALL	SETTRK		; Spur einstellen
	LD	BC,0
BIOS1:	PUSH	BC
	CALL	SETSEC		; Sektor einstellen
	LD	C,0
	CALL	WRITE		; Daten schreiben
	POP	BC
	AND	A		; Schreibfehler?
	LD	A,4		; Fehlercode fuer Anzeige
	JR	NZ,BIOS2
	INC	BC		; naechster Sektor
	LD	HL,(SPT)
	SBC	HL,BC		; letzter Sektor?
	JR	NZ,BIOS1
BIOS2:	PUSH	AF
	CALL	DOSLW		; zurueck zum DOS-Laufwerk
	POP	AF
	RET
;
; ----- Texte -----
;
VERMSG:	DB	7,'Falsches System, nur fuer AC1 mit ZBIOS CP/M !$'
asc_A21:        db 0Ch,'$'
;
TXT1:	DB	'Laufwerk nicht installiert.$'
TXT3:	DB	'Option fehlerhaft.$'
TXT4:	DB	'Kein Diskettenlaufwerk.$'
;
TXT5:	DB	'Laufwerk $'
TXT6:	DB	': mit $'
TXT7:	DB	' schnell$'
TXT8:	DB	' formatieren.$'
;
LUNMSG:	POS	5,5
	DB	'Auf welchem Laufwerk soll formatiert werden (A-P) ? '
LUN:	DB	' ',8,'$'
;
ART1:	POS	7,7
	DB	'  SS/SD  $'
ART2:	POS	7,20
	DB	'  SS/DD  $'
ART3:	POS	7,33
	DB	'  DS/SD  $'
ART4:	POS	7,46
	DB	'  DS/DD  $'
MARK1:	POS	7,7
	DB	'<',85h
MARK2:	POS	7,14
	DB	84h,'>$'
;
ERRMSG:	POS	13,18
	DB	7
TXT2:	DB	'Falscher Laufwerkstyp !$'
;
POSMSG:	POS	13,5		; Position Statuszeile
	DB	'$'
;
CRLF:	DB	CR,LF,'$'	; neue Zeile
;
NODISK:	POS	8,20
	DB	7,5,'!  W A R N U N G  !',4,7
	POS	10,5
	DB	'Das ausgewaehlte Laufwerk ist kein Diskettenlaufwerk.'
	POS	11,5
	DB	'Soll der Datentraeger physisch geloescht werden ? $'
QUICKMSG:
	POS	12,5
	DB	'Schnellformatierung (nur Direktory loeschen)    ? $'
;
; Tabelle mit Meldungen:
;
ERRTAB:	DW	NRDMSG		; A=0: Laufwerk nicht bereit
	DW	ROMSG		; A=1: Diskette schreibgeschuetzt
	DW	SEEMSG		; A=2: SEEK-Error
	DW	BTRMSG		; A=3: Defekte Spuren
	DW	WREMSG		; A=4: BIOS-Schreibfehler
	DW	BREAK		; A=5: Abgebrochen
;
TRKMSG:	DB	'Formatierte Spur: $'
;
OKMSG:	DB	'Formatierung beendet !$'
;
NRDMSG:	DB	'Laufwerk nicht bereit !$'
;
ROMSG:	DB	'Diskette schreibgeschuetzt !$'
;
SEEMSG:	DB	'Fehler bei Spursuche !$'
;
BTRMSG:	DB	'Defekte Spuren !     $'
;
WREMSG:	DB	'BIOS-Schreibfehler ! $'
;
BREAK:	DB	'Formatierung abgebrochen !$'
;
REPMSG:	POS	13,34
	DB	'   Wiederholung (J/N) ? $'
;
CNFMSG:	POS	13,35
CNFTXT:	DB	CON,'   Richtig (J/N) ? $'
;
FRMT0:	POS	8,9
	DB	'16 *  256 *'
	POS	9,10
	DB	'8 *  512 *'
	POS	10,10
	DB	'9 *  512 *'
	POS	11,10
	DB	'5 * 1024 *'
	DB	COF,'$'
;
; MFS 1.2
FRMT1:	POS	8,21
	DB	'40 * 1'
	POS	8,35
	DB	'(160K, 156K, 152K...)'
	POS	9,21
	DB	'40 * 1'
	POS	9,35
	DB	'(160K, 156K, 152K...)'
	POS	10,21
	DB	'40 * 1'
	POS	10,35
	DB	'(180K, 175K, 171K...)'
	POS	11,21
	DB	'40 * 1'
	POS	11,35
	DB	'(200K, 195K, 190K...)$'
;
; MFS 1.4
FRMT2:	POS	8,21
	DB	'80 * 1'
	POS	8,35
	DB	'(320K, 316K, 312K...)'
	POS	9,21
	DB	'80 * 1'
	POS	9,35
	DB	'(320K, 316K, 312K...)'
	POS	10,21
	DB	'80 * 1'
	POS	10,35
	DB	'(360K, 355K, 351K...)'
	POS	11,21
	DB	'80 * 1'
	POS	11,35
	DB	'(400K, 395K, 390K...)$'
;
; MFS 1.6 mit 40 Spursimulation
FRMT3:	POS	8,21
	DB	'40 * 2'
	POS	8,35
	DB	'(320K, 312K, 304K...)'
	POS	9,21
	DB	'40 * 2'
	POS	9,35
	DB	'(320K, 312K, 304K...)'
	POS	10,21
	DB	'40 * 2'
	POS	10,35
	DB	'(360K, 351K, 342K...)'
	POS	11,21
	DB	'40 * 2'
	POS	11,35
	DB	'(400K, 390K, 380K...)$'
;
; MFS 1.6
FRMT4:	POS	8,21
	DB	'80 * 2'
	POS	8,35
	DB	'(640K, 632K, 624K...)'
	POS	9,21
	DB	'80 * 2'
	POS	9,35
	DB	'(640K, 632K, 624K...)'
	POS	10,21
	DB	'80 * 2'
	POS	10,35
	DB	'(720K, 711K, 702K...)'
	POS	11,21
	DB	'80 * 2'
	POS	11,35
	DB	'(800K, 790K, 780K...)$'
;
MARK3:	POS	8,7
	DB	'<',85h
MARK4:	POS	8,28
	DB	84h,'>$'
;
UNMARK:	POS	8,7
	DB	'  ',8,8,LF
	DB	'  ',8,8,LF
	DB	'  ',8,8,LF
	DB	'  ',8,8,LF
	POS	8,28
	DB	'  ',8,8,LF
	DB	'  ',8,8,LF
	DB	'  ',8,8,LF
	DB	'  ',8,8,LF
	DB	'$'
;-------------------------------
; Tabelle fuer Formaterkennung (Option /F):
; Aufbau: Word fuer Kapazitaet, Zeiger auf DPB
;
FMTAB:	DW	800,DPB800
	DW	780,DPB780
	DW	720,DPB720
	DW	640,DPB640
	DW	624,DPB624

;-------------------------------
; 5 vordefinierte Diskettenparameterbloecke
; (je 20 Bytes lang)

DPB640:	; 16 * 256 * 80 * 2 (OFF=0) => 640K
	DW	64			; SPT
	DB	4,15,0			; BSH,BLM,EXM
	DW	319			; DSM
	DW	127			; DRM
	DB	0C0H,0			; AL0,1
	DW	32			; CKS
	DW	0			; OFF
	DB	1,1			; PSH,PHM
	DB	16			; EOT
	DB	20			; GAP
	DB	80			; NTR

DPB624:	; 16 * 256 * 80 * 2 (OFF=2) => 624K
	DW	64			; SPT
	DB	4,15,0			; BSH,BLM,EXM
	DW	311			; DSM
	DW	127			; DRM
	DB	0C0H,0			; AL0,1
	DW	32			; CKS
	DW	2			; OFF
	DB	1,1			; PSH,PHM
	DB	16			; EOT
	DB	20			; GAP
	DB	80			; NTR

DPB720:	; 9 * 512 * 80 * 2 (OFF=0) => 720K
	DW	72			; SPT
	DB	4,15,0			; BSH,BLM,EXM
	DW	359			; DSM
	DW	127			; DRM
	DB	0C0H,0			; AL0,1
	DW	32			; CKS
	DW	0			; OFF
	DB	2,3			; PSH,PHM
	DB	9			; EOT
	DB	25			; GAP
	DB	80			; NTR

DPB780:	; 5 * 1024 * 80 * 2 (OFF=2) => 780K
	DW	80			; SPT
	DB	4,15,0			; BSH,BLM,EXM
	DW	389			; DSM
	DW	127			; DRM
	DB	0C0H,0			; AL0,1
	DW	32			; CKS
	DW	2			; OFF
	DB	3,7			; PSH,PHM
	DB	5			; EOT
	DB	37			; GAP
	DB	80			; NTR

DPB800:	; 5 * 1024 * 80 * 2 (OFF=0) => 800K
	DW	80			; SPT
	DB	4,15,0			; BSH,BLM,EXM
	DW	399			; DSM
	DW	127			; DRM
	DB	0C0H,0			; AL0,1
	DW	32			; CKS
	DW	0			; OFF
	DB	3,7			; PSH,PHM
	DB	5			; EOT
	DB	37			; GAP
	DB	80			; NTR

;-------------------------------
; Sektorreihenfolge (rueckwaerts):
;	--> Die Sektoranordnung wurde am KC85/4 unter CP/M 2.2 optimiert
;	und entspricht bei jedem Format ca. 3 Umdrehungen fuer das Lesen
;	einer Spur, das sind ca. 7-8 KByte pro Sekunde.
;
SECT5:	DB	4,2,5,3,1				; 5*1024
SECT8:	DB	6,3,8,5,2,7,4,1				; 8*512
SECT9:	DB	9,6,3,8,5,2,7,4,1			; 9*512
SECT16:	DB	6,11,16,5,10,15,4,9,14,3,8,13,2,7,12,1	; 16*256

;-------------------------------
; Tabelle der Formatbeschreibungen (12 Byte je Typ):
;
; 780k:		Name:	Bedeutung:
; 3	IX+0	PSH	physischer Sektorverschiebungsfaktor
; 7	IX+1	PHM	physische Sektormaske
; 5	IX+2	EOT	Nr. des letzten Sektors der Spur
; 110	IX+3	GAP	GAP3-Luecke
; 0FFH	IX+4	DTL	Datensatzlaenge
; 80	IX+5	NTR	Spuren je Seite
; 0	IX+6	PUN	physische Einheit (wird bei LW-Auswahl eingetragen)
; 43h	IX+7	DTYP	Laufwerkstyp
; 0FFH	IX+8	FTP	erste Spur mit Praekompensation
; 0EFH	IX+9	TSS	Schrittzeit
; 20	IX+10	HLT	Kopfladezeit
; 0FFH	IX+11	CUR	erste Spur mit Schreibstrombegrenzung

; MFS 1.2
FMTTAB:	DB	1,1,16, 54,0FFH,40,0,41H,0FFH,0BFH,20,0FFH	; 16* 256*40*1
	DB	2,3, 8,170,0FFH,40,0,41H,0FFH,0BFH,20,0FFH	;  8* 512*40*1
	DB	2,3, 9, 86,0FFH,40,0,41H,0FFH,0BFH,20,0FFH	;  9* 512*40*1
	DB	3,7, 5,110,0FFH,40,0,41H,0FFH,0BFH,20,0FFH	;  5*1024*40*1

; MFS 1.4
	DB	1,1,16, 54,0FFH,80,0,42H,0FFH,0BFH,20,0FFH	; 16* 256*80*1
	DB	2,3, 8,170,0FFH,80,0,42H,0FFH,0BFH,20,0FFH	;  8* 512*80*1
	DB	2,3, 9, 86,0FFH,80,0,42H,0FFH,0BFH,20,0FFH	;  9* 512*80*1
	DB	3,7, 5,110,0FFH,80,0,42H,0FFH,0BFH,20,0FFH	;  5*1024*80*1

; MFS 1.6 mit 40-Spur-Simulation
	DB	1,1,16, 54,0FFH,40,0,53H,0FFH,0EFH,20,0FFH	; 16* 256*40*2
	DB	2,3, 8,170,0FFH,40,0,53H,0FFH,0EFH,20,0FFH	;  8* 512*40*2
	DB	2,3, 9, 86,0FFH,40,0,53H,0FFH,0EFH,20,0FFH	;  9* 512*40*2
	DB	3,7, 5,110,0FFH,40,0,53H,0FFH,0EFH,20,0FFH	;  5*1024*40*2

; MFS 1.6
	DB	1,1,16, 54,0FFH,80,0,43H,0FFH,0EFH,20,0FFH	; 16* 256*80*2
	DB	2,3, 8,170,0FFH,80,0,43H,0FFH,0EFH,20,0FFH	;  8* 512*80*2
	DB	2,3, 9, 86,0FFH,80,0,43H,0FFH,0EFH,20,0FFH	;  9* 512*80*2
	DB	3,7, 5,110,0FFH,80,0,43H,0FFH,0EFH,20,0FFH	;  5*1024*80*2

;
; Kommandofolgen:
;
FMTCMD:	DB	4DH		; 'FORMAT A TRACK MFM'
FMTDR:	DB	00H		; Laufwerk, Kopf
FMTN:	DB	03H		; Sektorlaenge
FMTEOT:	DB	05H		; EOT
FMTGAP:	DB	20H		; GPL
	DB	0E5H		; Fuellbyte
;
CMD:	DB	0		; Kommando
DR:	DB	0		; Laufwerk, Kopf
CYL:	DB	0		; Spurnummer
	DB	6 dup (0)	; frei fuer max. Befehlslaenge
;
RESBYT:	DB	7 dup (0)	; 7 Byte Ablagepuffer fuer Ergebnisphase
;
; ----- nicht initialisierte Arbeitszellen -----
;
IDTAB	EQU	$		; 64	ID-Feld (max. 16*4=64 Bytes)
CMDLN	EQU	IDTAB+64	; 1	Flag: Kommandozeilenmodus
HARD	EQU	CMDLN+1		; 1	Flag: Nicht-Diskettenlaufwerk
NOASK	EQU	HARD+1		; 1	Flag: Abfrage unterdruecken
QUICK	EQU	NOASK+1		; 1	Flag: Schnellformatierung
DRIVE	EQU	QUICK+1		; 1	Laufwerk (Nummer 0..15)
TRACKS	EQU	DRIVE+1		; 2	Anzahl zu formatierender Spuren
IODSK	EQU	TRACKS+2	; 2	Adresse Floppytreiber aus BIOS
TRB	EQU	IODSK+2		; 2	Treiber des aktuellen Laufwerkes
DPB	EQU	TRB+2		; 26	DPB des akt. Laufwerkes
SPT	EQU DPB+0		;	logische Sektoren/Spur
BSH	EQU DPB+2		;	log. Blockschiebefaktor
DSM	EQU DPB+5		;	Anzahl Bloecke/Diskette
DRM	EQU DPB+7		;	Anzahl Verzeichniseintragungen
OFF	EQU DPB+13		;	Anzahl Systemspuren
PSH	EQU DPB+15		;	physische Sektorgroe~e
EOT	EQU DPB+17		;	physische Sektoren/Spur
NTR	EQU DPB+19		;	Anzahl Spuren
PUN	EQU DPB+20		;	phys. Geraetenummer
TYP	EQU DPB+21		;	Laufwerkstyp
;
DOSDRV	EQU	DPB+30H		; 1	aktuelles DOS-Laufwerk
MAX	EQU	DOSDRV+1	; 1	max. moeglicher Diskettentyp (1-4)
ART	EQU	MAX+1		; 1	aktuell gewaehlter Diskettentyp (80/2)
FRM	EQU	ART+1		; 1	gewaehltes Diskettenformat
;
byte_1161	EQU	FRM+1
;
STACK	EQU	FRM+256		; 256 Byte Stack
;

ZSUM		equ	0

	END
