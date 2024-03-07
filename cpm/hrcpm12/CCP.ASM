
;	*********************************************************
;	*							*
;	*  Komandointerpreter f}r CP/M 2.2 	Version 1.4	*
;	*							*
;	* - Kommandos entsprechen weitestgehend KC-MicroDOS,	*
;	*   ein HELP-Kommando listet m|gliche Befehle auf	*
;	* - Parser wurde ZCPR entnommen und gestattet die Form	*
;	*   DU:NAME.TYP in Kommando und Argumenten		*
;	* - USER-Wechsel mit ZSDOS/ZDDOS FCB-USER-Funktion	*
;	* - es werden nur die Standard-Schnittstellen von BDOS	*
;	*   und	BIOS benutzt!					*
;	*							*
;	* CCP.MAC			(c) ML-Soft 04.10.1996	*
;	*********************************************************

; Version AC1 HRDOS Ralph Hänsel 23.11.2011
; reass/adpt V.Pohlers 2023

; [nderungen:
; V1.1 (09.07.1996)
;	- Parser von ZCPR
;	- Datum/Uhrzeit fehlerfei zu stellen
; V1.2 (07.09.1996)
;	- Parameter in DMA-Puffer bei GO-Kommando
;	- Test der Zeichenzahl in $$$.SUB auch f}r Anzeige
; V1.3 (27.09.1996)
;	- ZSDOS-Errormodus FEH w{hrend CCP-Arbeit
;	- CP/M-Errormodus 0 f}r nachladbare Programme
; V1.4 (04.10.1996)
;	- Zwei CCP-Einsprungstellen kompatibel zum Original-CP/M
;	  (erforderlich f}r RSX-Treiber)

	cpu Z80
;	org	0D000h
	
	LISTING OFF
;
; Macros:
;
PRTVAL2	MACRO M1,V1,M2
	MESSAGE	"M1 V1 M2"
	ENDM

PRTVAL	MACRO R,MSG1,VAL,MSG2
	OUTRADIX R
	MESSAGE "\{MSG1} \{VAL} \{MSG2}"
	OUTRADIX 10
	ENDM

DS	MACRO	A,B
	DB	A DUP(B)
	ENDM
;
; Anzeigen:
;
	IF MOMPASS=1
	MESSAGE	"* CCP fuer CP/M 2.2 auf AC1 *"
	ELSE
	PRTVAL	16,"* CCP-Speicherbedarf:",800H,"H *"
	PRTVAL	16,"*   ...davon genutzt:",CCPEND-CCPRO,"H *"
	PRTVAL	10,"* Frei im 2K-Bereich:",CCPRO+800H-CCPEND,"Byte dezimal *"
	ENDIF
	LISTING ON

CDISK	EQU	4
BDOS	EQU	5
TIMEx	EQU	3DH
FCB1	EQU	5CH
FCB2	EQU	6CH
DEFDMA	EQU	80H
TPA	EQU	100H

TAB	EQU	09H
LF	EQU	0AH
CLS	EQU	0CH
CR	EQU	0DH
ESC	EQU	1BH
SPC	EQU	20H

;EXT	ZBIOS			; BIOS-Funktionen erst beim Linken erzeugen

ZBIOS	equ	$+1600h

	;CSEG
; *******************************
; *	CCP-Einsprungstellen	*
; *	CCPRO:	 Kaltstart	*
; *	CCPRO+3: Warmstart	*
; *******************************

CCPRO:	JR	WARM		; Kaltstart und Warmstart identisch!
	DB	14H		; CCP-Versionsnummer
	;
WARM:	LD	A,0FFH		; Datei A0:$$$.SUB suchen
	LD	(SUBFLG),A
CCP0:	LD	SP,CCPSTK	; CCP-Stack neu setzen
	LD	HL,CCP0
	PUSH	HL		; RET-Adresse ablegen
	LD	E,0FEH
	LD	C,45
	CALL	BDOS		; ZSDOS-Errormodus w{hlen
;
; CCP-Laufwerk anw{hlen
;
	LD	A,(CDISK)
	PUSH	AF
	AND	0FH		; Bit 0-3
	LD	D,A		; CCP-Laufwerk (0..15)
	POP	AF
	AND	0F0H		; Bit 4-7
	RLCA
	RLCA
	RLCA
	RLCA
	LD	E,A		; CCP-USER (0..15)
	LD	(CURUSR),DE	; aktuelles CCP-Laufwerk eintragen
	CALL	LOGIN

	LD	A,(SUBFLG)
	OR	A
	JR	Z,CCP5		; A0:$$$.SUB nicht ausf}hren
	XOR	A
	LD	(SUBFCB+12),A	; ex=0
	LD	(SUBFCB+14),A	; s2=0
	LD	(SUBFCB+15),A	; rc=0
	LD	DE,SUBFCB	; A0:$$$.SUB
	LD	C,15
	CALL	BDOS		; Datei |ffnen
	INC	A
	LD	(SUBFLG),A	; 0, wenn A0:$$$.SUB nicht vorhanden
	JR	Z,CCP5
	LD	A,(SUBFCB+15)	; rc
	OR	A
	JR	Z,CCP4		; Datei leer -> l|schen
	DEC	A
	LD	(SUBFCB+32),A	; cr=rc-1 (letzter Sektor der Datei)
	LD	DE,CCBUF+1	; CCP-Puffer
	LD	C,26
	CALL	BDOS		; als DMA-Adresse setzen
	LD	C,20
	LD	DE,SUBFCB	; A0:$$$.SUB
	CALL	BDOS		; sequentiell lesen
	INC	A
	JR	Z,CCP4		; Lesefehler -> Datei l|schen
	LD	HL,SUBFCB+14	; s2
	XOR	A
	LD	(HL),A
	INC	HL		; rc
	DEC	(HL)		; ein Datensatz weniger
	JR	Z,CCP1		; Datei entleert
	LD	C,16
	LD	DE,SUBFCB	; A0:$$$.SUB
	CALL	BDOS		; Datei schlie~en
	INC	A
CCP1:	CALL	Z,ERASUB	; $$$.SUB l|schen
	CALL	PROMPT		; Bereitschaftsmeldung ausgeben
	LD	HL,CCBUF+1
	LD	B,(HL)		; Anzahl Zeichen
	INC	B		; +1
	JR	CCP3		; wenn B=0 war, dann nichts tun!
	;
CCP2:	INC	HL		; n{chstes Zeichen
	LD	A,(HL)		; Zeichencode aus Puffer
	OR	A
	JR	Z,CCP6		; Ende-0 -> interpretieren
	CALL	ACHR		; anzeigen
CCP3:	DJNZ	CCP2		; f}r alle Zeichen wiederholen
	JR	CCP6		; -> interpretieren
	;
CCP4:	CALL	ERASUB		; $$$.SUB l|schen
CCP5:	CALL	PROMPT		; Bereitschaftsmeldung
	LD	DE,CCBUF	; Kommandozeile
	LD	C,10
	CALL	BDOS		; gepufferte Eingabe
CCP6:	LD	DE,DEFDMA
	LD	C,26
	CALL	BDOS		; Standard-DMA-Adresse setzen
	LD	HL,CCBUF+1
	LD	A,(HL)		; Anzahl der Eingabezeichen
	OR	A
	RET	Z		; nichts eingegeben
	LD	B,A		; Anzahl Zeichen
	INC	HL
	LD	A,(HL)		; 1. Zeichen
	CP	';'
	RET	Z		; Kommentarzeile
	CP	SPC
	JP	C,BADCMD	; Steuerzeichen
	CALL	SDELM		; Begrenzer?
	JP	Z,BADCMD
CCP7:	LD	A,(HL)		; alle Zeichen der Eingabezeile
	CALL	UPCASE		; in Gro~buchstaben umwandeln
	LD	(HL),A		; und wieder ablegen
	INC	HL
	DJNZ	CCP7		; bis ganze Zeile bearbeitet ist
	LD	(HL),0		; danach 0 als Ende-Kennung eintragen
	LD	HL,CCBUF+2	; Beginn Kommandozeile
	CALL	PARSER		; FCB's erzeugen
	LD	A,(CMDFCB+1)
	CP	SPC		; Dateiname leer?
	JR	NZ,EXEC		; nein, Kommando ausf}hren
	LD	A,(CMDCOL)
	AND	A		; Laufwerkwechsel?
	RET	Z		; nein
	LD	A,(CMDFCB)	; Laufwerk
	SUB	1
	JR	NC,CCP8		; Laufwerk angegeben
	LD	A,(CURDSK)	; aktuelles Laufwerk verwenden
CCP8:	LD	D,A		; Laufwerk in Register D
	LD	A,(CMDFCB+13)	; USER-Bereich
	AND	7FH
	LD	E,A		; USER in Register E
	PUSH	DE
	CALL	LOGIN		; Verzeichnis anw{hlen
	POP	DE
	LD	A,H
	AND	A
	JP	NZ,BADDIR	; Laufwerk fehlerhaft
	LD	A,E
	RLCA			; CY=0 und Bit0=0
	RLA
	RLA
	RLA			; USER in High-Nibble
	OR	D		; mit Laufwerk kombinieren
	LD	(CDISK),A	; in Page 0 eintragen
	RET

;-------------------------------
; Kommando ausf}hren
;
EXEC:	LD	A,(CMDCOL)
	AND	A
	JR	NZ,LOAD		; Dateikommando, da Laufwerk angegeben
	LD	HL,CMDFCB+1
	LD	C,(HL)		; 1. Zeichen der Kommandozeile holen
	INC	HL
	LD	A,(HL)		; 2. Zeichen der Kommandozeile testen
	CP	SPC
	JR	NZ,LOAD		; Dateikommando, da mehr als 1 Zeichen
	LD	DE,CMDTAB	; Tabelle der CCP-Kommandos
	LD	HL,CMDADR	; Sprungadressen CCP-Kommandos
TABC1:	LD	A,(DE)
	OR	A		; Ende der Tabelle?
	JR	Z,LOAD		; kein CCP-Kommando, von Diskette laden
	CP	C
	JR	Z,TABC2		; Kommando gefunden
	INC	DE
	INC	HL
	INC	HL
	JR	TABC1		; weitersuchen
	;
TABC2:	LD	E,(HL)
	INC	HL
	LD	D,(HL)		; Sprungtabelle lesen
	PUSH	DE		; und auf Stack abspeichern
	RET			; Funktion starten

;-------------------------------
; nachladbares Programm von Diskette laden
;
LOAD:	LD	DE,CMDFCB
	LD	C,15
	CALL	BDOS		; Datei |ffnen
	INC	A
	JR	NZ,LOAD1	; Datei gefunden
	LD	A,H
	AND	A
	RET	NZ		; Diskettenfehler
	JP	NOFILE		; Datei nicht gefunden
	;
LOAD1:	LD	DE,TPA		; Ladeadresse
LOAD2:	PUSH	DE
	LD	C,26
	CALL	BDOS		; DMA-Adresse setzen
	LD	DE,CMDFCB
	LD	C,20
	CALL	BDOS		; sequentiell lesen
	POP	DE
	DEC	A
	JR	Z,LOAD3		; Datei komplett geladen
	INC	A
	RET	NZ		; Fehler
	LD	HL,128
	ADD	HL,DE		; neue DMA-Adresse berechnen
	EX	DE,HL
	LD	HL,CCPRO-128
	SBC	HL,DE		; Programm zu lang?
	JR	NC,LOAD2
	JP	OVER		; ]berlauf
	;
LOAD3:	CALL	OCRLF		; geladen durch Zeilenvorschub anzeigen
	LD	HL,(TAILSV)	; Beginn des Kommandozeilenrests
	LD	B,0		; Zeichenz{hler
	LD	DE,DEFDMA+1
LOAD4:	LD	A,(HL)		; Kommandosatz in DMA-Puffer
	LD	(DE),A		; zur Verf}gung stellen
	OR	A
	JR	Z,LOAD5		; Kommandozeile zu Ende
	INC	B		; Zeichen z{hlen
	INC	HL
	INC	DE
	JR	LOAD4
	;
LOAD5:	LD	HL,DEFDMA
	LD	(HL),B		; Anzahl eintragen
	EX	DE,HL
	LD	C,26
	CALL	BDOS		; Standard-DMA-Adresse setzen
	LD	E,0
	LD	C,45
	CALL	BDOS		; CP/M-Errormodus w{hlen
	LD	SP,COMSTK	; Programm-Stack setzen
	LD	HL,0		; Return zu Warmstartadresse 0
	PUSH	HL		; in Stack ablegen
	JP	TPA		; TPA anspringen

;-------------------------------
; Fehlermeldungen im CCP-Kommando
;
BADCMD:	LD	DE,TXT1		; Kommando?
	JR	PRMSG
	;
BADDIR:	LD	DE,TXT2		; Laufwerk?
	JR	PRMSG
	;
NOFILE:	LD	DE,TXT3		; Datei nicht gefunden
	JR	PRMSG
	;
OVER:	LD	DE,TXT4		; ]berlauf
	JR	PRMSG
	;
NOTF:	LD	DE,TXT5		; Keine Datei
PRMSG:	LD	C,9
	CALL	BDOS		; anzeigen
	JP	CCP0

;-------------------------------
; Systemkommandodatei A0:$$$.SUB l|schen
;
ERASUB:	LD	DE,SUBFCB	; FCB-Adresse
	LD	C,19
	JP	BDOS		; Datei l|schen

;-------------------------------
; Wandelt Kleinbuchstaben in Gro~buchstaben
;
UPCASE:	CP	'a'
	RET	C
	CP	'z'+1
	RET	NC
	AND	5FH		; klein -> gro~
	RET

;-------------------------------
; Laufwerk anzeigen
; PE:	D	Laufwerk
;	E	USER-Bereich
;
LWSHOW:	LD	A,D		; Laufwerk
	ADD	A,'A'		; -> Buchstabe
	CALL	ACHR		; Zeichen anzeigen
	LD	A,E		; USER-Bereich
;
; Dezimalausgabe Byte
; PE:	A	Byte
;
ADEZ:	PUSH	DE		; bei Recursion: letzten Rest (D) retten
	LD	B,10
	LD	E,-1
ADEZ1:	INC	E
	SUB	B		; ganze Zehner abziehen
	JR	NC,ADEZ1
	ADD	A,B
	LD	D,A		; Rest in D merken
	LD	A,E		; Zehner
	AND	A
	CALL	NZ,ADEZ		; Rekursion, wenn Zehner vorhanden
	LD	A,D
	ADD	A,'0'		; Rest -> ASCII in A
	CALL	ACHR		; Zeichen anzeigen
	POP	DE		; voriger Rest
	RET

;-------------------------------
; Zeilenvorschub, Prompt und andere Anzeigen
;
OCRLF:	LD	A,CR		; neue Zeile
	CALL	ACHR
	LD	A,LF
	JR	ACHR
;
PROMPT:	CALL	OCRLF		; Zeilenvorschub
	LD	DE,(CURUSR)	; akt. Laufwerk
	CALL	LWSHOW		; anzeigen
	LD	A,'>'
	DB	11H		; LD DE,..
COLON:	LD	A,':'
	DB	11H		; LD DE,..
TABANZ:	LD	A,TAB
	DB	11H		; LD DE,..
POINT:	LD	A,'.'
	DB	11H		; LD DE,..
SPACE:	LD	A,SPC
ACHR:	PUSH	HL
	PUSH	DE
	PUSH	BC
	CALL	TSTCH		; darstellbares Zeichen?
	JR	NC,ACH1
	PUSH	AF
	LD	E,'^'		; Control-Einleitung
	LD	C,2
	CALL	BDOS
	POP	AF
	OR	40H		; zugeh|riger Buchstabe
ACH1:	LD	E,A
	LD	C,2
	CALL	BDOS
	POP	BC
	POP	DE
	POP	HL
	RET
;
; Test auf darstellbare Zeichen:
;
TSTCH:	CP	CR
	RET	Z
	CP	LF
	RET	Z
	CP	TAB
	RET	Z
	CP	SPC
	RET

;-------------------------------
; Tabelle der CCP-Kommandos
;
CMDTAB:		db '014DEGHLCTSZ',0
;
CMDADR:	DW	EXIT		; 0 OS
	DW	CLRSCR		; 1 CLS
	dw TAKT			; 4
	DW	DIR		; D Directory ausgeben
	DW	ERA		; E Datei l|schen
	DW	LOAD3		; GO
	DW	HELP		; H Kommandos auflisten
	dw CLOAD		; L 
	dw CPTPA		; C
	DW	PTIMEx		; T Datum/Uhrzeit anzeigen
	dw SAVE		; S 
	dw CHRSET		; Z

;-------------------------------
; 0: Exit zum OS
;
EXIT:	CALL	ZBIOS+3CH

;-------------------------------
; 1: Bildschirm l|schen
;
CLRSCR:	LD	C,CLS		; Bildschirm l|schen
	JP	ZBIOS+0CH	; Konsolenausgabe

;-------------------------------
; 4 schaltet AC1 zwischen 2/4Mhz Takt um (IO Port F0H Bit0=1)
TAKT:		in	a, (0F0h)	
		xor	1
		out	(0F0h),	a
		ret

;-------------------------------
; LOAD File mit	Turbo in RAM laden
CLOAD:		jp	ZBIOS+39h		

;-------------------------------
; C TPA von 2000H nach 0100H umladen
CPTPA:		jp	ZBIOS+36h		

;-------------------------------
; S xx FileNam Speichert xx CPM Seite als Datei ab
SAVE:		call	OCRLF		
		ld	hl, FCB1+1
		ld	a, (hl)
		call	GETDEZ
		jp	c, BADCMD
		xor	a
		cp	d
		jp	nz, BADCMD
		cp	e
		jp	z, BADCMD
		sla	e
		rl	d
		push	de
		ld	b, 1Fh
		xor	a
		ld	de, CMDFCB+1Fh
;
SAVE1:		ld	(de), a
		dec	de
		djnz	SAVE1
		ld	c, 0Ch
		ld	hl, FCB2
		ldir
		ld	c, 11h
		call	SAVE3
		inc	a
		jp	nz, BADDIR
		ld	c, 16h
		call	SAVE3
		inc	a
		pop	bc
		jp	z, BADDIR
		ld	de, 100h
;
SAVE2:		push	de
		push	bc
		ld	c, 1Ah
		call	5
		ld	c, 15h
		call	SAVE3
		pop	bc
		pop	de
		inc	a
		jp	z, BADDIR
		dec	a
		jp	nz, BADDIR
		ld	a, '*'
		call	ACHR
		ld	hl, 80h
		add	hl, de
		ex	de, hl
		dec	bc
		ld	a, b
		or	c
		jr	nz, SAVE2
		ld	c, 10h
;
SAVE3:		ld	de, CMDFCB
		call	5
		ret

;-------------------------------
; T: Uhrzeit anzeigen/stellen
;
PTIMEx:	CALL	OCRLF		; neue Zeile
	LD	A,(FCB1+1)
	CP	'C'		; kontinuierliche Anzeige?
	JR	NZ,PTIMEx2
;
; kontinuierliche Anzeige
;
TCONT:	LD	A,CR		; Cursor nach links
	CALL	ACHR
	CALL	TSHOW		; Zeit anzeigen
TCON1:	LD	C,11
	CALL	BDOS		; Konsolenstatus holen
	OR	A
	JR	Z,TCON2		; Ausgabe fortsetzen
	LD	C,1
	JP	BDOS		; Tastencode abholen
	;
TCON2:	LD	DE,TIMEx
	LD	C,0		; Zeit lesen
	CALL	ZBIOS+33H	; Uhrentreiber
	LD	A,(HL)
	CP	E		; Sekunden neu?
	JR	Z,TCON1		; warten
	JR	TCONT		; Anzeige aktualisieren
;
PTIMEx2:	CP	'S'		; Uhr stellen?
	JR	NZ,TSHOW
;
; Uhr stellen
;
TSET1:	LD	DE,TXT7		; Datum eingeben
	CALL	DINPUT
	JR	C,TSET1		; Eingabefehler
	RET	Z		; nichts eingegeben
	LD	A,(HL)
	LD	(TIMEx+2),A	; Tag
	INC	HL
	LD	A,(HL)
	LD	(TIMEx+1),A	; Monat
	INC	HL
	LD	A,(HL)
	LD	(TIMEx),A	; Jahr
	CALL	OCRLF		; neue Zeile
TSET2:	LD	DE,TXT8		; Uhrzeit eingeben
	CALL	DINPUT
	JR	C,TSET2		; Eingabefehler
	RET	Z		; nichts eingegeben
	LD	DE,TIMEx+3
	LD	BC,3
	LDIR			; Zeit
	LD	DE,TIMEx
	LD	C,1		; Uhr stellen
	CALL	ZBIOS+33H	; Uhrentreiber
	CALL	OCRLF		; neue Zeile
;
; Zeit anzeigen
;
TSHOW:	LD	DE,TIMEx
	LD	C,0		; Zeit lesen
	CALL	ZBIOS+33H	; Uhrentreiber
	LD	HL,TIMEx+2	; Datum r}ckw{rts
	LD	B,2
PTIM1:	LD	A,(HL)		; Tag/Monat
	DEC	HL
	CALL	AHEX		; BCD anzeigen
	CALL	POINT		; Trennung mit '.'
	DJNZ	PTIM1
	LD	A,(HL)		; Jahr
	PUSH	AF
	CP	78
	LD	A,19H		; Jahrhundert
	ADC	A,0		; 1978..2077
	DAA			; BCD-Korrektur
	CALL	AHEX
	POP	AF
	CALL	AHEX
	CALL	SPACE		; Leerzeichen zwischen Datum und Zeit
	LD	HL,TIMEx+3	; Zeit vorw{rts
	LD	B,2
PTIM2:	LD	A,(HL)		; Std/Min
	INC	HL
	CALL	AHEX		; BCD anzeigen
	CALL	COLON		; Trennung mit ':'
	DJNZ	PTIM2
	LD	A,(HL)		; Sekunden
AHEX:	PUSH	AF
	RRA
	RRA
	RRA
	RRA
	CALL	AHEX0		; erst aufrufen,
	POP	AF		; dann reinlaufen
AHEX0:	AND	0FH
	ADD	A,90H
	DAA
	ADC	A,40H
	DAA
	JP	ACHR		; Zeichen anzeigen

;-------------------------------
; Datum/Uhrzeit eingeben
; PE:	DE	Zeichenkette f}r Eingabeaufforderung
; PA:	HL	Daten BCD
;	CY=1	Eingabe fehlerhaft
;	Z=1	nichts eingegeben
;
DINPUT:	LD	C,9		; Text anzeigen
	CALL	BDOS
	LD	DE,DBUFF+1
	XOR	A
	LD	(DE),A		; Puffer leer
	DEC	DE
	LD	C,10
	CALL	BDOS		; gepufferte Eingabe
	LD	DE,DBUFF+1
	LD	A,(DE)		; Anzahl
	AND	A
	RET	Z		; nichts eingegeben
	CP	8
	RET	C		; zu wenig Zeichen
	LD	HL,BCD
	LD	B,3
DCONV:	INC	DE
	LD	A,(DE)		; Eingabezeichen holen
	SUB	'0'
	RET	C		; zu klein
	CP	10
	CCF
	RET	C		; zu gro~
	RLCA
	RLCA
	RLCA
	RLCA
	LD	C,A		; H-Teil merken
	INC	DE
	LD	A,(DE)
	SUB	'0'
	RET	C		; zu klein
	CP	10
	CCF
	RET	C		; zu gro~
	OR	C
	LD	(HL),A
	INC	HL		; n{chster BCD-Wert
	INC	DE		; Trennzeichen }bergehen
	DJNZ	DCONV
	LD	HL,BCD		; Daten }bergeben
	OR	1		; OK: CY=0, Z=0
	RET

;-------------------------------
; D: Directory anzeigen
;
DIR:	LD	HL,FCB1+1	; Dateiname
	LD	A,(HL)
	CP	SPC
	JR	NZ,DIR1		; DIR-Maske angegeben
	LD	B,11
DIR0:	LD	(HL),'?'		; alle Dateien (Maske '????????.???')
	INC	HL
	DJNZ	DIR0
DIR1:	LD	E,0		; Spaltenz{hler
	PUSH	DE
	LD	DE,FCB1		; FCB-Adresse
	LD	C,17
	CALL	BDOS		; ersten Eintrag suchen
	CP	0FFH
	JR	NZ,DIR2		; -> Eintrag gefunden
	POP	DE
	JP	NOTF		; Keine Datei gefunden
	;
DIR2:	LD	(DIRCOD),A	; Verzeichniscode
	POP	DE
	LD	A,E
	INC	E		; Spalten z{hlen
	PUSH	DE
	AND	2
	JR	NZ,DIR4		; 4 Spalten nebeneinander
		pop     de
		inc     e
		push    de
	CALL	OCRLF		; danach Zeilenvorschub
	LD	A,(FCB1)	; und
	SUB	1
	JR	NC,DIR3
	LD	A,(CURDSK)
DIR3:	LD	D,A		; Laufwerk
	LD	A,(FCB1+13)
	AND	7FH
	LD	E,A		; USER
	CALL	LWSHOW		; anzeigen
	JR	DIR5
	;
DIR4:	CALL	SPACE		; Leerzeichen zwischen den Spalten
DIR5:	CALL	COLON		; Doppelpunkt vor Dateiname
	LD	B,1		; ab Dateiname
DIR6:	CALL	DIRUP		; Zeichen aus DMA-Puffer holen
	AND	7FH		; Bit 7 r}cksetzen
	CALL	ACHR		; und anzeigen
	INC	B		; n{chstes Zeichen
	LD	A,B
	CP	12
	JR	NC,DIR7		; fertig mit Dateiname
	CP	9
	CALL	Z,POINT		; Punkt zwischen Name und Typ
	JR	DIR6
	;
DIR7:	CALL	SPACE		; Leerzeichen nach Typ
	LD	B,9
	CALL	DIRUP		; 9. Attribut
	AND	80H
	LD	A,'R'		; Anzeige R/O
	JR	NZ,DIR8
	LD	A,SPC		; oder leer
DIR8:	CALL	ACHR
	INC	HL
	LD	A,(HL)		; 10. Attribut
	AND	80H
	LD	A,'S'		; Anzeige SYS
	JR	NZ,DIR9
	LD	A,SPC		; oder leer
DIR9:	CALL	ACHR
	INC	HL
	LD	A,(HL)		; 11. Attribut
	AND	80H
	LD	A,'A'		; Anzeige Archiv
	JR	NZ,DIR10
	LD	A,SPC		; oder leer
DIR10:	CALL	ACHR
	LD	C,11
	CALL	BDOS		; Konsolenstatus holen
	OR	A
	JR	NZ,DIR11	; Abbruch
	LD	C,18
	CALL	BDOS		; n{chsten Eintrag suchen
	CP	0FFH
	Jp	NZ,DIR2		; -> weiterer Eintrag vorhanden
	POP	DE
	RET
	;
DIR11:	LD	C,1
	CALL	BDOS		; Tastencode abholen
	POP	DE
	RET
;
; Bytes aus Verzeichniseintrag lesen
; PE:	B	Bytenummer
;
DIRUP:	LD	HL,DEFDMA
DIRCOD	EQU	$+1
	LD	A,0		; Verzeichniscode
	RRCA
	RRCA
	RRCA
	ADD	A,B		; Bytenummer
	LD	D,0
	LD	E,A
	ADD	HL,DE		; Position berechnen
	LD	A,(HL)		; und Zeichen holen
	RET

;-------------------------------
; E: Datei l|schen
;
ERA:	LD	DE,TXT6
	LD	C,9
	CALL	BDOS		; L|schen?
	LD	C,1
	CALL	BDOS		; Eingabe
	CALL	UPCASE
	CP	'J'		; JA?
	JR	Z,ERA1
	CP	'Y'		; alternatives JA?
	RET	NZ
ERA1:	LD	DE,FCB1
	LD	C,19
	CALL	BDOS		; Datei l|schen
	CP	0FFH
	RET	NZ		; -> Funktion korrekt ausgef}hrt
	LD	A,H
	AND	A
	RET	NZ		; Diskettenfehler
	JP	NOTF		; keine Datei

;-------------------------------
; H: Kommandos auflisten
;
HELP:	LD	DE,TXT9		; Kommandos
	LD	C,9
	CALL	BDOS
	LD	HL,CMDTAB	; Tabelle
HELP1:	LD	A,(HL)
	CALL	ACHR		; Kommando anzeigen
	INC	HL
	LD	A,(HL)
	AND	A
	RET	Z		; fertig
	LD	A,','
	CALL	ACHR		; Trennzeichen
	LD	A,' '
	CALL	ACHR
	JR	HELP1

;-------------------------------
; Z schaltet den Zeichengeneratur um (PIO B3 Port)
;
CHRSET:		in	a, (5)		; 
		xor	8
		out	(5), a
		ret

;=============================================================================
;  Z C P R  3 . 4     C O M M A N D    L I N E     P A R S I N G    C O D E
;=============================================================================

; Dieser Programmteil enth{lt einen Auszug aus dem Code von ZCPR 3.4, welcher
; die Kommandozeile untersucht und daraus drei FCB's (Kommando, FCB1, FCB2)
; erzeugt. Erlaubt sind Angaben der Form DU:NAME.TYP f}r alle drei Token.
; Das Laufwerk wird dabei im Byte 0, der USER-Bereich im s1-Feld des FCB
; eingetragen.
;
; Kommandozeile analysieren
; PE:	HL	Zeiger auf Kommandozeile
; PA:	CMDFCB	FCB f}r Kommando
;	FCB1	FCB f}r zweiten Token
;	FCB2	FCB f}r dritten Token
;
PARSER:	LD	DE,CMDFCB	; FCB f}r Kommandodatei
	PUSH	DE
	CALL	INIFCB		; FCB initialisieren
	POP	DE		; INIFCB endet mit A=0
	LD	(DUERFLG),A	; Fehlerflag r}cksetzen
	CALL	SCANNER		; ersten Token analysieren
	JP	NZ,BADCMD	; Jokerzeichen im Kommando!
DUERFLG	EQU	$+1
	LD	A,0		; DU-Error?
	OR	A
	JP	NZ,BADDIR	; Fehler in Laufwerk/User!
	EX	DE,HL
	LD	HL,CMDFCB+9	; Dateityp
	LD	A,(HL)
	CP	' '		; mu~ leer sein
	JP	NZ,BADCMD
	LD	(HL),'C'
	INC	HL
	LD	(HL),'O'	; .COM eintragen
	INC	HL
	LD	(HL),'M'
	EX	DE,HL		; HL wieder Zeiger in Kommamdozeile
	LD	(TAILSV),HL	; Beginn Kommandozeilenrest merken
	LD	A,(TMPCOL)	; Doppelpunkt erkannt von SCANNER?
	LD	(CMDCOL),A	; f}r Kommandoauswertung merken

	LD	DE,FCB1
	PUSH	DE
	CALL	INIFCB		; FCB1 und FCB2 initialisieren
	POP	DE		; Zeiger auf 1. FCB
	CALL	SKSP		; Leerzeichen }bergehen
	CALL	NZ,SCANNER	; wenn nicht Zeilenende, FCB1 erzeugen

	CALL	SKSP		; Leerzeichen }bergehen
	RET	Z		; Zeilenende
	LD	DE,FCB2		; Zeiger auf 2. FCB
				; ..und reinlaufen in SCANNER
;-------------------------------
; einen Token der Kommandozeile analysieren und FCB erzeugen
; (Angaben der Form DU:NAME.TYP werden akzeptiert)
; PE:	HL	Zeiger in Kommandozeile
;	DE	FCB-Adresse
; PA:	HL	zeigt auf Begrenzungszeichen am Ende des Tokens
;	Z=0	Jokerzeichen enthalten
;
SCANNER:XOR	A
	LD	(TMPCOL),A	; kein Doppelpunkt
	LD	BC,(CURUSR)	; aktuelles Laufwerk/USER holen
	INC	B		; Laufwerk 0..15 -> 1..16
	LD	(TMPUSR),BC	; DU merken
	CALL	SCANF8		; m|glichen Dateiname holen
	CP	':'		; mit Doppelpunkt beendet?
	JR	NZ,SCAN1	; nein, weiter mit Dateityp
	LD	(TMPCOL),A	; Doppelpunkt erkannt
	INC	HL		; Zeiger nach Doppelpunkt
	PUSH	DE		; FCB retten
	INC	DE		; Beginn Name
	PUSH	HL		; Zeiger auf Kommandozeile retten
	PUSH	DE		; Zeiger auf FCB+1 retten
	EX	DE,HL		; HL=FCB+1
	CALL	DUSCAN		; DU-Angabe behandeln
	POP	DE		; FCB+1
	POP	HL		; Kommandozeile
	LD	(DUERFLG),A	; DU-Errorflag }bergeben
	LD	C,A		; und in C merken
	CALL	IFCB		; Dateiname in FCB l|schen
	LD	A,C		; DU-Errorcode zur}ckholen
	DEC	DE		; d0-Feld
	DEC	DE		; rc-Feld (FCB+15)
	LD	(DE),A		; DU-Error dort ablegen
	POP	DE		; FCB-Adresse
 	LD	A,(TMPDRV)	; gew{hltes Laufwerk
	LD	(DE),A		; im FCB eintragen
	CALL	SCANF8		; jetzt nach eigentlichem Dateinamen suchen
SCAN1:	EX	DE,HL		; FCB-Adresse nach HL
	LD	BC,8		; weiter bis zum Dateityp
	ADD	HL,BC
	CP	'.'		; Dateityp angegeben?
	JR	NZ,SCAN2	; nein
	EX	DE,HL		; FCB wieder nach DE
	INC	HL		; '.' }bergehen
	LD	B,3		; Maximal 3 Zeichen f}r Dateityp
	CALL	SCANFB		; Dateityp in FCB eintragen
	EX	DE,HL		; FCB-Adresse wieder nach HL
SCAN2:	LD	BC,5		; Offset von Dateityp bis s1 im FCB
	ADD	HL,BC
	LD	A,(TMPUSR)	; gew{hlter USER-Bereich
	OR	80H		; USER anw{hlen
	LD	(HL),A		; im FCB eintragen (Feld s1)
	EX	DE,HL		; FCB wieder nach DE
SCAN3:	LD	A,(HL)		; n{chstes Zeichen von Kommandozeile holen
	CP	' '+1
	JR	C,SCAN4		; fertig wenn Space, Steuerzeichen oder Null
	CP	';'
	JR	Z,SCAN4		; fertig, weil Kommandoende
	INC	HL		; n{chstes Zeichen
	CP	'='
	JR	NZ,SCAN3	; wenn kein '=' weitersuchen
QMCNT	EQU	$+1
SCAN4:	LD	A,0		; Anzahl Fragezeichen im Dateinamen
	OR	A		; Joker erkannt?
	RET

;-------------------------------
; SCANFIELD f}r Dateiname aufrufen und Fragezeichenz{hler r}cksetzen
; PE:	HL	Kommandozeile
;	DE	FCB-Adresse
;
SCANF8:	XOR	A		; Fragezeichenz{hler r}cksetzen
	LD	(QMCNT),A
	LD	B,8		; bis zu 8 Zeichen (Dateiname) testen
SCANFB:	PUSH	DE		; FCB retten
	CALL	SCANF		; Parse field of (B) characters into FCB
	POP	DE		; FCB regenerieren
	RET

;-------------------------------
; Feld von Zeichen testen, Jokerzeichen aufbereiten, FCB eintragen
; PE:	HL	Kommandozeile
;	DE	FCB-Adresse
;	B	max. Zeichenzahl
; PA:	HL	zeigt auf Begrenzerzeichen
;
SCANF:	CALL	SDELM		; Begrenzer?
	RET	Z
	CP	'*'		; Jokerzeichen expandieren?
	JR	Z,SCANF1
	INC	HL		; zum n{chsten Zeichen
	CP	'?'		; Single-Joker?
	JR	NZ,SCANF2
SCANF1:	PUSH	HL
	LD	HL,QMCNT
	INC	(HL)		; Fragezeichen z{hlen
	POP	HL
	LD	A,'?'		; '?' anstatt '*' eintragen
SCANF2:	INC	DE
	LD	(DE),A		; in FCB eintragen
	DJNZ	SCANF		; bis alle Zeichen bearbeitet wurden
SCANF3:	CALL	SDELM
	RET	Z		; Begrenzerzeichen gefunden
	INC	HL
	JR	SCANF3		; weitersuchen

;-------------------------------
; Versuch, den Teil vor dem Doppelpunkt als DU zu interpretieren
; PE:	HL	Kommandozeile
; PA:	TMPDRV	erkanntes Laufwerk
;	TMPUSR	erkannter USER-Bereich
;	A=0,Z=1	DU erkannt
;
DUSCAN:	LD	BC,(CURUSR)	; C = aktueller USER, B = aktuelles Laufwerk
	LD	A,(HL)		; m|gliche Laufwerksbezeichnung holen
	SUB	'A'		; konvertieren nach 0..15
	JR	C,DUSC1		; < 0, Register B beh{lt seinen Wert
	CP	16
	JR	NC,DUSC1	; > 15, Register B beh{lt seinen Wert
	LD	B,A		; B mit gew{hltem Laufwerk laden
	INC	HL		; und Zeiger auf n{chstes Zeichen
DUSC1:	INC	B		; Laufwerk 1..16
	LD	A,(HL)		; m|gliche Userbezeichnung holen
	CP	' '		; leer?
	JR	Z,DUSC2		; wenn leer, Register C beh{lt seinen Wert
	PUSH	BC
	CALL	GETDEZ		; m|gliche USER-Nummer nach DE holen
	POP	BC
	JR	C,DUERR		; Formatfehler in Dezimalzahl
	LD	A,D
	OR	A
	RET	NZ		; High-Byte nicht Null
	LD	C,E		; C mit gew{hlter USER-Nummer laden
DUSC2:	LD	HL,(CURUSR)	; aktuelle Werte nach HL
	INC	H		; Laufwerk 1..16
	SBC	HL,BC
	JR	Z,SETDU		; OK, entspricht den aktuellen Werten
	LD	A,15
	CP	C
	JR	C,DUERR		; USER-Nummer > 15
SETDU:	LD	(TMPUSR),BC
	XOR	A		; A=0 und Z=1
	RET
	;
DUERR:	OR	255		; Return NZ to show failure
	RET

;-------------------------------
; Leerzeichen und TAB vor Token }bergehen
; PE:	HL	Kommandozeile
; PA:	HL	zeigt auf erstes Zeichen, das nicht leer ist
;	Z=1	Zeile oder Kommando zu Ende
;
SKSP1:	INC	HL		; n{chstes Zeichen
SKSP:	LD	A,(HL)
	CP	SPC		; Space?
	JR	Z,SKSP1		; ja, }bergehen
	CP	TAB		; Tabulator?
	JR	Z,SKSP1		; ja, }bergehen
	OR	A		; Zeilenende?
	RET	Z		; ja
	CP	';'		; Kommandoende?
	RET

;-------------------------------
; zwei FCBs initialisieren (l|schen)
; PE:	DE	FCB-Adresse
;
INIFCB:	XOR	A
	LD	(DE),A		; aktuelles Laufwerk
	INC	DE		; Dateiname
	CALL	IFCB		; 1. FCB vorbereiten
				; ..und reinlaufen f}r 2. FCB
IFCB:	LD	B,11
	LD	A,' '
	CALL	FILL		; 11 Leerzeichen f}r Name und Typ
	XOR	A
	LD	(DE),A		; ex=0
	INC	DE
	LD	A,(CURUSR)
	OR	80H
	LD	(DE),A		; s1=USER
	INC	DE
	LD	B,3		; 3mal 0 eintragen f}r
	XOR	A		; s2, rc und d0 (bzw. aktuelles LW in FCB2)
FILL:	LD	(DE),A		; Wert eintragen
	INC	DE		; n{chstes Byte
	DJNZ	FILL
	RET

;-------------------------------
; Directory anw{hlen
; PE:	D	Laufwerk
;	E	USER-Bereich
; PA:	siehe BDOS-Funktion!
;
LOGIN:	PUSH	DE
	RES	7,E		; r}cksetzen, falls aus FCB
	LD	C,32
	CALL	BDOS		; USER-Bereich einstellen
	POP	DE
	LD	E,D		; Laufwerk in Register E
	LD	C,14
	JP	BDOS		; Laufwerk anw{hlen

;-------------------------------
; eingegebene Dezimalzahl aufbereiten
; PE:	HL	Zeichenkette
; PA:	HL	zeigt auf Begrenzungszeichen nach Zahl
;	A	Begrenzungszeichen
;	DE	Wert der Zahl
;	CY=1	Formatfehler
;
GETDEZ:	LD	DE,0		; Startwert
	OR	A		; CY=0
DEZN:	CALL	SDELM		; Begrenzer?
	RET	Z		; ja
	SUB	'0'
	RET	C		; Fehler: < '0'
	CP	10
	CCF
	RET	C		; Fehler: > '9'
	INC	HL		; Zeiger auf n{chstes Zeichen
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

;-------------------------------
; Test auf Begrenzungszeichen
; PE:	HL	Kommandozeile
; PA:	A	Zeichen von (HL)
;	Z=1	A ist Begrenzer
;
SDELM:	LD	A,(HL)		; Zeichen holen
	EXX			; Zweitregistersatz
	LD	HL,DELDAT	; Begrenzerliste
	LD	BC,DELEND-DELDAT; L{nge
	CPIR			; Zeichen enthalten?
	EXX			; Register wiederherstellen
	RET

DELDAT:	DEFB	' =.:;,^<>',0	; Liste Begrenzerzeichen
DELEND:

;-------------------------------
; Texte:
;
TXT1:	DB	CR,LF,'Kommando?$'
TXT2:	DB	CR,LF,'Laufwerk?$'
TXT3:	DB	CR,LF,'Datei nicht gefunden$'
TXT4:	DB	CR,LF,'Ueberlauf$'
TXT5:	DB	CR,LF,'Keine Datei$'
TXT6:	DB	CR,LF,'Loeschen (j/n)? $'
TXT7:	DB	24,'Eingabe Datum   (TT.MM.JJ): $'
TXT8:	DB	24,'Eingabe Uhrzeit (HH:MM:SS): $'
TXT9:	DB	CR,LF,'Kommandos: $'
;
; CCP-Datenbereich:
;
CURUSR:	DB	0		; aktueller USER-Bereich
CURDSK:	DB	0		; aktuelles Laufwerk
DBUFF:	DB	9		; 8+1 Byte Puffer f}r Zeiteingabe
	DS	10,0
BCD:	DS	3,0		; Ablage f}r Eingabedaten im BCD-Format
CCBUF:	DB	127		; 127 Byte Kommandozeilenpuffer
	DS	128,0
;
TAILSV:	DW	0		; zeigt auf Parameter nach Kommando
TMPCOL:	DB	0		; Flag: Doppelpunkt erkannt
CMDCOL:	DB	0		; Flag: Doppelpunkt in Kommando erkannt
TMPUSR:	DB	0		; von Scannerroutine erkannter USER-Bereich
TMPDRV:	DB	0		; von Scannerroutine erkanntes Laufwerk
;
SUBFLG:	DB	0		; Flag: $$$.SUB-Datei abarbeiten
SUBFCB:	DB	1,'$$$     SUB'	; FCB f}r A0:$$$.SUB
	DB	0,80H		; Userbereich 0
	DS	22,0
CMDFCB:	DS	33,0		; FCB f}r nachladbares COM-Programm
;	DS	100,0		; 100 Byte Stack f}r CCP
CCPEND:	
	DS	CCPRO+800H-40-$,0	; Rest frei

	DB	"ZSDOS CCP 23.11.2011 Version HR    Stack"

CCPSTK	equ	$
COMSTK	EQU	$		; COM-Stack am Ende des CCP
				; und damit am Ende des TPA

unk_D7D1	equ CMDFCB+1Fh

;	END
