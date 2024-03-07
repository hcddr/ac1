;
; GIDE-Treiber fuer ML-DOS am KC85
;
; Include zu ZBIOS.MAC		(c) M. Leubner 28.02.2009
; -------------------------------------------------------
;
	TITLE	"ZBIOS - physischer Treiber f}r GIDE-Festplatten"
	NEWPAGE
; ***************************************************************
; *	physische Schreib/Leseroutine f}r IDE-Festplatte	*
; *	nach GIDEIDE.MAC von Tilmann Reh	(c) 05.09.1995	*
; ***************************************************************

CmdRd	equ	20h		; Read Sector
CmdWr	equ	30h		; Write Sector

; Macros: wait for hard disk ready (non-busy) resp. DRQ active.

WaitRdy	macro
	;local	wait
wait:	in	a,(IdeCmd)
	rla
	jr	c,wait
	endm

WaitDrq	macro
	;local	wait
wait:	in	a,(IdeCmd)
	bit	3,a
	jr	z,wait
	endm
;
; Einsprungstelle des HDD-Treibers
; PE:
; PA:	ERRFLG = FFh wenn Fehler aufgetreten, sonst unveraendert
;
HDISK:	WAITRDY			; warten bis Platte bereit ist
	LD	L,25
	CALL	GETPAR		; phys. Sektoren/Spur aus DPH holen
	LD	C,A
	LD	HL,(SECPAR)	; Sektor (ab 0)
	INC	HL		; f}r weitere Nutzung ab 1!
	XOR	A
	LD	B,A		; BC=Anzahl Sektoren/Spur
HDI1:	INC	A		; Aufteilen in Kopf (Quotient)
	SBC	HL,BC		; und Sektor (Rest)
	JR	Z,HDI2		; letzter Sektor
	JR	NC,HDI1		; noch einen Kopf weiter
HDI2:	ADD	HL,BC		; Korrekturrechnung
	DEC	A		; Kopfnummer (ab 0)
	LD	D,A		; D=Kopfnummer
	LD	E,L		; E=Sektornummer
	LD	L,17
	CALL	GETPAR		; Master/Slave-Kennung holen
	OR	D		; mit Kopfnummer verkn}pfen
	OUT	(IDESDH),A	; SDH-Register ausgeben
	LD	A,E
	OUT	(IDESNUM),A	; Sektornummer ausgeben
	INC	HL
	LD	E,(HL)		; Partitionsbeginn Low
	INC	HL
	LD	D,(HL)		; Partitionsbeginn High
	LD	HL,(TRKPAR)	; Spurnummer
	ADD	HL,DE		; und Partition addieren
	LD	A,L
	OUT	(IDECLO),A	; Zylinder-Low
	LD	A,H
	OUT	(IDECHI),A	; Zylinder-High (ab 0)
	LD	A,1
	OUT	(IDESCNT),A	; einen Sektor
	LD	A,(CMD)		; Kommando
	CP	6		; Schreiben?
	JR	Z,WRIDE
;
; Daten von Festplatte lesen
; PA:	A=1	Fehler
;	A=0	OK
;
RDIDE:	LD	A,CMDRD
	OUT	(IDECMD),A	; Befehl: Sektor lesen
	WAITDRQ			; DRQ abwarten
	LD	HL,(DMAPAR)	; Pufferadresse
	LD	BC,IDEDAT	; B = 0 (Z{hler), C = I/O-Adresse
	INIR
	INIR			; 2x 256 Datenbytes lesen
	JR	HDIST		; gemeinsames Ende
;
; Daten auf Festplatte schreiben
;
WRIDE:	LD	A,CMDWR
	OUT	(IDECMD),A	; Befehl: Sektor schreiben
	WAITRDY			; warten bis Laufwerk bereit
	WAITDRQ			; warten auf Datenanforderung
	LD	HL,(DMAPAR)	; Pufferadresse
	LD	BC,IDEDAT	; B = 0 (Z{hler), C = I/O-Adresse
	OTIR
	OTIR			; 2x 256 Datenbytes ausgeben
HDIST:	WAITRDY			; warten bis Laufwerk fertig ist
	IN	A,(IDECMD)	; Status testen
	AND	10001001B	; Busy, DRQ, oder Error?
	RET	Z		; nein: alles OK
	LD	A,0FFH
	LD	(ERFLAG),A	; Error: 'allgemeiner Fehler'
	RET
