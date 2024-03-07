;
; Floppy-Treiber fuer ML-DOS am KC85 (FDC U8272 bzw. u765)
;
; Include zu ZBIOS.MAC		(c) M. Leubner 28.02.2009
; -------------------------------------------------------
; Rekonstruktion/Anpassung an HRDOS 2011 R. Hänsel
; V. Pohlers 2023

	TITLE	"ZBIOS - physischer Floppy-Treiber"
	NEWPAGE
;
; Einsprungstelle des FDC-Treibers
;
; PE:	BC=0
;
; PA:	ERRFLG = Fehlercode, wenn OK dann unveraendert
;
FDISK:	LD	A,(TRKPAR+1)	; Spur-High
	LD	HL,SECPAR+1	; Sektor-High
	OR	(HL)		; beides Null?
	JP	NZ,FDERR	; Spur nicht vorhanden
	CALL	GETPSH
	LD	(N),A		; phys. Sektorgroesse (PSH)
	OR	A
	LD	A,0FFH		; DTL bei PSH>0
	JR	Z,LDPAR1
	LD	A,128		; 128 Byte bei PSH=0
LDPAR1:	LD	(DTL),A		; Datenlaenge
	INC	HL
	INC	HL		; Diskettendefinitionsblock DDB
	LD	A,(HL)		; EOT aus DDB holen
	LD	(EOT),A
	LD	A,(SECPAR)	; Sektor (0...)
	CP	(HL)
	JR	C,LDPAR2	; Sektor > EOT
	SUB	(HL)		; Sektor auf 2. Diskettenseite!
	LD	BC,0104H	; Kopf 1 der 2-seitigen Diskette (Version 1)
LDPAR2:	INC	A
	LD	(REC),A		; Sektor (1...)
	INC	HL
	LD	A,(HL)		; GAP3-Luecke
	LD	(GPL),A
	INC	HL
	LD	A,(TRKPAR)	; Spur
	CP	(HL)		; NTR
	JR	C,LDPAR3	; Spur > NTR
	SUB	(HL)		; Spur auf 2. Diskettenseite!
	LD	BC,0104H	; Kopf 1 der 2-seitigen Diskette (Version 2)
LDPAR3:	LD	(CYL),A		; Spurnummer
	INC	HL		; Laufwerkparameterblock DRPB
	LD	A,(HL)		; PUN
	OR	C
	LD	(HDDS),A	; Kopf- und Laufwerkauswahl
	LD	A,B
	LD	(HEAD),A	; Kopfnummer
	INC	HL
	LD	A,(CMD)		; Kommando (4=lesen, 6=schreiben)
	LD	D,WRCMD		; FDC-Befehlscode: WRITE
	CP	6		; Schreiben?
	JR	Z,LDPAR4
	LD	D,RDCMD		; FDC-Befehlscode: READ
LDPAR4:	LD	A,(HL)		; DTYP
	AND	40H		; FM/MFM ausfiltern
	OR	D		; in Befehlscode einbauen
	LD	(CODE1),A	; FDC-Befehlscode
	LD	DE,DTYP
	LD	BC,4
	LDIR			; DTYP bis HLT aus DRPB kopieren
	LD	C,2		; 2 mal wiederholen mit Laufwerk HOME
WDHB1:	LD	B,2		; 2 Versuche ohne HOME
WDHB2:	PUSH	BC
	CALL	BEFEHL		; FDC-Befehl ausfuehren
	POP	BC
	OR	A
	RET	Z		; OK
	DJNZ	WDHB2		; Befehl gleich noch mal wiederholen
	DEC	C
	RET	Z		; Fehler trat immer wieder auf
	PUSH	BC
	CALL	RECAL		; zu Spur 0
	POP	BC
	JR	WDHB1		; und weiterversuchen
;
; *******************************************************
; *	physische Schreib/Leseroutine fuer den 8272	*
; *******************************************************
; PA:	Z=1	kein Fehler
;	ERFLAG	Fehlercode
; SP:	12
;

; genutzte FDC-Befehle:
SPCMD	EQU	3	; SPECIFY (Zeiten und DMA-Betriebsart einstellen)
WRCMD	EQU	5	; WRITE SECTOR (Daten schreiben)
RDCMD	EQU	6	; READ SECTOR (Daten lesen)
RCCMD	EQU	7	; RECALIBRATE (Spur 0 anfahren)
INCMD	EQU	8	; SENSE INTERRUPT STATUS (Interruptstatus pruefen)
SKCMD	EQU	15	; SEEK (Spur suchen)
;
BEFEHL:	LD	A,SPCMD		; FDC-Befehl "SPECIFY"
	LD	B,2		; 2 Byte
	LD	HL,TSS		; Schrittimpulsrate, Kopfentladezeit,
	OUT	(CSDAT),A	; Kopfladezeit und Non-DMA-Modus
	CALL	FDK1		; einstellen
	CALL	SEEK		; Kopf auf Spur positionieren
	JR	NZ,FDERR	; Spur nicht gefunden
	LD	HL,128/2
	LD	A,(N)
	INC	A
MULT:	ADD	HL,HL		; phys. Sektorgroesse berechnen
	DEC	A		; = Anzahl der Datenbytes
	JR	NZ,MULT
	DEC	HL
	INC	H		; Schleifenanzahl und
	INC	L		; Restanzahl ermitteln
	PUSH	HL		; und merken
	LD	HL,(DMAPAR)	; Pufferadresse
	PUSH	HL
	LD	A,(CODE1)	; FDC-Befehlscode
	LD	C,A
	LD	B,9		; Befehlslaenge
	RRCA			; Lesen oder Schreiben?
	LD	A,51H		; ED A2 = INI	(Lesen)
	ADC	A,A		; ED A3 = OUTI	(Schreiben)
	LD	(FDIO+1),A	; Programmcode modifizieren!
		ld	(loc_F270+1), a
	DI
	CALL	FDCMD		; FDC-Befehl "READ/WRITE DATA"
	POP	HL		; DMA-Puffer
	POP	DE		; Anzahl
	LD	B,E		; Schleifenanzahl
	LD	C,DAK		; Portadresse "DMA-Acknowledge FDC"
	CALL	FDCIO		; Datentransfer (lesen/schreiben)
	EI			; alles im DI !
	CALL	FDCRES		; Ergebnisphase
	LD	HL,RESULT	; und auswerten
	LD	A,(HL)		; ST0
	AND	0C0H
	JR	Z,FDRET		; OK
FDERR:	LD	A,0FFH
FDRET:	LD	(ERFLAG),A	; Fehlercode ablegen
	OR	A		; Z-Flag setzen?
	RET
;
; Ergebnisphase, Statusinformationen abholen
; SP:	4
;
FDCRES:	LD	B,7		; 7 Bytes
	LD	HL,RESULT	; im Resultatspuffer
FDCR1:	CALL	STATUS		; FDC-Status lesen
	LD	(HL),A		; und ablegen
	INC	HL
	DJNZ	FDCR1
	RET
;
; Laufwerk ruecksetzen (Spur 0 anfahren)
; SP:	10
;
RECAL:	LD	BC,2*256+RCCMD	; FDC-Befehl "RECALIBRATE"
	CALL	FDCMD
	CALL	SENSE		; Status abfragen
	RET	Z		; fertig
	LD	BC,2*256+RCCMD	; FDC-Befehl "RECALIBRATE"
	CALL	FDCMD		; wiederholen
	JR	SENSE
;
; Kopf positionieren
; SP:	10
;
SEEK:	LD	A,(CYL)		; Spurnummer
	LD	(RCYL),A	; merken
	OR	A
	JR	Z,RECAL		; zu Spur 0
	LD	BC,3*256+SKCMD	; FDC-Befehl "SEEK"
	LD	HL,DTYP
	BIT	4,(HL)		; simuliertes 40-Spur-Laufwerk?
	JR	Z,SEEK1
	ADD	A,A		; Spurnummer * 2
	LD	(CYL),A
SEEK1:	CALL	FDCMD		; Spur suchen mit FDC-Befehl "SEEK"
	LD	A,(RCYL)	; alte Spurnummer
	LD	(CYL),A		; wiederherstellen
SENSE:	
;	IN	A,(INP)
;	BIT	6,A		; Interrupt?
;	JR	NZ,SENSE	; nein, warten
	LD	BC,1*256+INCMD	; FDC-Befehl "SENSE INTERRUPT STATUS"
	CALL	FDCMD		; Kommando ausgeben
	CALL	STATUS		; FDC-Status lesen
	LD	B,A
	LD	(RESULT),A	; ST0 ablegen
;	ADD	A,A
;	RET	Z		; ok
;	CALL	STATUS		; FDC-Status lesen
		cp	80h ; '€'
		call	nz, STATUS
		bit	5, b
		jr	z, SENSE
	LD	A,B
	AND	0F0H
	CP	0C0H
	JR	Z,SENSE		; Bit 6 und 7 gesetzt, warten
	XOR	20H		; sonst Fehler
	RET
;
; FDC-Kommando ausgeben:
; PE:	Kommando in C, Anzahl in B
; SP:	6
;
FDCMD:	PUSH	BC		; Kommando in C, Anzahl in B
	CALL	READY		; Laufwerk starten, bereit?
	POP	BC
FDCMD1:	LD	HL,CODE2	; Datenbereich
	LD	(HL),C		; Befehlscode
;FDK1:	LD	C,CSDAT
;FDK2:	IN	A,(CSHSR)	; FDC-Hauptstatusregister
;	AND	0C0H
;	CP	80H		; warten bis FDC bereit ist
;	JR	NZ,FDK2
;	OUTI			; Daten ausgeben
;	JR	NZ,FDK2		; bis B=0
;	RET
FDK1:		ld	c, CSDAT
		push	de
FDK2:		ld	de, 1FFFh
FDK3:		ld	a, e
		or	d
		jr	z, FDK4
		in	a, (CSHSR)
		and	0C0h
		cp	80h
		dec	de
		jr	nz, FDK3
		outi
		jr	nz, FDK2
FDK4:		ld	b, 0
		pop	de
		ret


;
; FDC-Status lesen:
; PA:	A	Lesedaten
; SP:	2
;
;STATUS:	IN	A,(CSHSR)	; Hauptstatusregister lesen
;	AND	0C0H
;	CP	0C0H		; Lesedaten bereit?
;	JR	NZ,STATUS	; nein
;	IN	A,(CSDAT)	; Daten
;	RET
STATUS:		push	hl
		ld	hl, 1FFFh
loc_F24B:	ld	a, l
		or	h
		ld	a, 0C0h
		jr	z, loc_F25C
		in	a, (CSHSR)
		and	0C0h
		cp	0C0h
		dec	hl
		jr	nz, loc_F24B
		in	a, (CSDAT)
loc_F25C:	pop	hl
		ret
;
; Datentransfer (komplett im DI - weil zeitkritisch !!!):
; PE:	C	Portadresse DAK
;	HL	DMA-Puffer
;	DB	Anzahl
; SP:	2
;
;FDCIO:	IN	A,(INP)		; Input-Gate lesen
;	ADD	A,A		; INT -> P-Flag, DRQ -> CY-Flag 
;	RET	P		; Interrupt von FDC erkannt
;	JR	C,FDCIO		; warten bis FDC bereit ist (DRQ=0)
;FDIO:	INI			; INI oder OUTI !!! (fuer FDC = DMA-Zyklus)
;	JR	NZ,FDCIO	; bis B=0
;	DEC	D
;	JR	NZ,FDCIO	; bis D=0
;	OUT	(TCFDC),A	; Ausgabe an TC des FDC
;	RET

FDCIO:		ld	a, (INPxx)
		set	1, a
		out	(LATCH), a
loc_F265:	in	a, (CSHSR)
		rlca
		jr	nc, loc_F265
FDIO:		ini
		nop
loc_F26D:	out	(FLWAIT), a
		nop
loc_F270:	ini
		jp	nz, loc_F26D
		dec	d
		jp	nz, loc_F26D
		ld	a, (INPxx)
		set	4, a
		out	(LATCH), a
		ret
;
; Select-Latch vorbereiten, Laufwerk starten und RDY abfragen
; SP:	2
;
;READY:	LD	A,(HDDS)
;	AND	3		; phys. Laufwerksnummer 0...3
;	LD	B,80H
;	INC	A
;RDY1:	RLC	B		; entsprechendes Bit 0...3 setzen
;	DEC	A
;	JR	NZ,RDY1
;	LD	HL,DTYP
;	BIT	7,(HL)		; 8"-Laufwerk?
;	JR	Z,RDY2
;	SET	6,B		; TAKTSEL auf 8 MHz umschalten
;RDY2:	LD	A,(CYL)		; Spur
;	LD	HL,FTP
;	CP	(HL)		; Pr{kompensation?
;	JR	NC,RDY3
;	SET	4,B		; nicht von Hardware unterst}tzt!
;RDY3:	LD	HL,LUNIT	; letztes Laufwerk
;	LD	A,(HDDS)	; mit aktuellem
;	AND	3		; phys. Laufwerk
;	CP	(HL)		; vergleichen
;	LD	(HL),A		; neues Laufwerk eintragen
;	INC	HL		; MTIME
;	LD	C,(HL)
;	LD	(HL),48H	; 72*16ms=1.15sek
;	LD	A,B
;	OUT	(SEL),A		; Motor ein
;	JR	NZ,RDY4		; Laufwerkwechsel!
;	LD	A,C
;	AND	A
;	JR	NZ,RDY5		; Motor l{uft noch
;RDY4:	LD	A,48H		; sonst warten
;	SRL	A
;	SRL	A
;	CP	(HL)		; 288ms abgelaufen?
;	JR	C,RDY4		; Anlaufzeit abwarten
;RDY5:	LD	A,(HL)
;	AND	A
;	RET	Z		; Timeout
;	IN	A,(INP)
;	BIT	5,A		; Laufwerk bereit?
;	JR	Z,RDY5		; warten
;	LD	(HL),90H	; 144*16ms=2.3sek Motor ein
;	RET
READY:		ld	hl, LUNIT
		ld	a, (HDDS)
		and	3	; phys. Laufwerksnummer 0...3
		cp	(hl)
		ld	(hl), a
		inc	hl
		ld	c, (hl)
		ld	(hl), 48h ; 'H'
		push	af
		push	hl
		ld	hl, INPxx
		ld	a, (HDDS)
		and	3
		jr	z, loc_F2A3
		cp	2
		jr	z, loc_F2A3
		set	3, (hl)
		jr	loc_F2A5
loc_F2A3:	set	0, (hl)
loc_F2A5:	ld	a, (hl)
		out	(LATCH), a
		pop	hl
		pop	af
		jr	nz, loc_F2B0
		ld	a, c
		and	a
		jr	nz, RDY5
loc_F2B0:	ld	a, 48h ; 'H'
		srl	a
		srl	a
		cp	(hl)
		jr	c, loc_F2B0
RDY5:		ld	a, (hl)
		and	a
		jr	nz, loc_F2C5
		push	hl
		ld	hl, aDisc	; " DISC?\b\b\b\b\b\b"
		call	print		; Textausgabe
		pop	hl
loc_F2C5:	push	bc
		push	hl
		ld	bc, 204h
		call	FDCMD1
		call	STATUS
		pop	hl
		pop	bc
		bit	5, a
		jr	z, RDY5
		ld	(hl), 90h ; 144*16ms=2.3sek Motor ein
		ret

aDisc:		db " DISC?",8,8,8,8,8,8,0

;
; ISR CTC-Kanal 3:		Interrupt ca. alle 16ms (250ns*256*256)
; ...schaltet Laufwerksmotor zeitgesteuert nach letztem Zugriff ab
;
;ISRC3:	PUSH	AF
;	LD	A,(MTIME)
;	AND	A		; l{uft Motor?
;	JR	Z,IC31		; nein, nichts tun
;	DEC	A		; Zeit runterz{hlen
;	LD	(MTIME),A	; und ablegen
;	JR	NZ,IC31		; Zeit abgelaufen?
;	OUT	(SEL),A		; Motor aus
;IC31:	POP	AF
;IRET:	EI
;	RETI                          
ISRC3:		push	af
		ld	a, (MTIME)
		and	a
		jr	z, IC31
		dec	a
		ld	(MTIME), a
		jr	nz, IC31
		push	hl
		ld	hl, INPxx
		res	0, (hl)
		res	3, (hl)
		ld	a, (hl)
		out	(LATCH), a
		pop	hl
IC31:		pop	af
IRET:		ei			; Dummy-Routine
		reti
