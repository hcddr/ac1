; File Name   :	d:\hobby3\ac1-2010\hrcpm12\HRDOS12-SCCH_D000_FEFF_E600.BIN
		cpu	z80undoc
		MACEXP_DFT NOIF,NOMACRO
;
;	*********************************************************
;	*	BIOS-Funktionen fuer CP/M-Z-System auf KC85	*
;	*							*
;	* - Ein-/Ausgaberoutinen ueber Koppel-RAM		*
;	* - Treiber fuer RAM-Disk ueber Koppel-RAM		*
;	* - Treiber fuer Floppy-Laufwerke mit U8272DC		*
;	* - Treiber fuer 2 Festplatten (Master/Slave) am	*
;	*   GIDE-Interface					*
;	* - Treiber fuer GIDE-Echtzeituhr mit RTC72421		*
;	* - alternativer Uhrentreiber mit CTC-Interrupt		*
;	* - Statusabfrage V.24-Kanaele (13.02.1998)		*
;	* - 5 bis 16 Laufwerke, davon bis zu 6 Festplattenpar-	*
;	*   titionen einstellbar				*
;	*							*
;	* Systemvoraussetzung:					*
;	*  RAM-Floppy mit mindestens 2 Spuren und Spur 0 als	*
;	*  Systemspur mit CCP und BDOS.				*
;	*							*
;	* erforderliche Include-Dateien:			*
;	*  OPTION.INC	Usereinstellungen (nur hier vornehmen!)	*
;	*  CALDPB.INC	Berechnung der Disketten-DPB's		*
;	*  CALHDD.INC	Berechnung der Festplatten-DPB's	*
;	*  IO_RAM.INC	RAM-Floppy-Treiber			*
;	*  IO_FDC.INC	Floppy-Treiber fuer U8272 / u765	*
;	*  IO_GIDE.INC	GIDE-Treiber (fuer Festplatten)		*
;	*							*
;	* Uebersetzung:						*
;	*  M80 ZBIOS=ZBIOS					*
;	*  LINK131 SYSTEM.SYS=CCP,ZSDOS,ZBIOS [D1600,OS,NR]	*
;	*  SYSGEN -> weiter nach Menue!				*
;	*							*
;	* ZBIOS.MAC	(c) ML-Soft 31.08.1996 bis 28.02.2009	*
;	*********************************************************
;	*							*
;	* BIOS Ralph Hänsel 2011				*
; 	* HRDOS/HRCPM AC1/SCCH					*
; 	* Stand 03.12.11 V1.2					*
;	* reass/adpt V.Pohlers 2023				*
;	*							*
;	*********************************************************


; Makros
high            function x, (x>>8) & 0ffh	; High-Byte
low             function x, x & 0ffh		; Low-Byte

; Vereinbarungen:
;
IOBYTE	EQU	3		; CP/M-IOBYTE
CDISK	EQU	4		; CCP-Laufwerk (und USER)
BDOS	EQU	5		; BDOS-Einsprung
;
	cpu	Z80
;	CSEG
;	org	0E600h

DOS	EQU	$-0E00H		; BDOS 3,5 KByte
CCP	EQU	$-1600H		; CCP 2 KByte

LAUFW	MACRO	A,B,C,D,E,F	; Macro f}r Diskettenlaufwerke
PHYS_A	EQU	B		; phys. Format
BLO_A	EQU	C		; Blockgroesse
DIR_A	EQU	D		; Verzeichnisgroesse
OFF_A	EQU	E		; Anzahl Systemspuren
UNIT_A	EQU	F		; phys. Laufwerk
	ENDM

PART	MACRO	A,B,C,D,E,F,G	; Macro f}r Plattenpartitionen
	IF B=0			; Master oder Slave?
CYLS_A	EQU	CYLS0
HEAD_A	EQU	HEAD0		; Daten von Masterplatte
SECS_A	EQU	SECS0
MAST_A	EQU	0A0H
	ELSE
CYLS_A	EQU	CYLS1
HEAD_A	EQU	HEAD1		; Daten von Slaveplatte
SECS_A	EQU	SECS1
MAST_A	EQU	0B0H
	ENDIF
CYL0_A	EQU	C		; erste Spur
CYL_A	EQU	D		; Spuranzahl der Partition
OFFH_A	EQU	E		; Anzahl Systemspuren
BLOH_A	EQU	F		; Blockgroesse
DIRH_A	EQU	G		; Verzeichnisgroesse
	ENDM

	INCLUDE	OPTION.INC	; USER-Definitionen

	LISTING OFF
	INCLUDE	CALDPB.INC	; Berechnung der Disketten-DPB's
	INCLUDE	CALHDD.INC	; Berechnung der Festplatten-DPB's
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

;
; Anzeigen:
;
	IF MOMPASS=1
		MESSAGE	"* ZBIOS fuer CP/M-System auf AC1 *"
	if (ndrives >= 5) && (ndrives <= 16)
		PRTVAL	10,"*",NDRIVES,"logische Laufwerke installiert *"
	else
		MESSAGE	"* Anzahl der Laufwerke nicht zulaessig! *"
	endif
	if hard
		PRTVAL	10,"*",HARD,"Festplattenpartitionen installiert *"
	if (hard+2 > ndrives) || (hard > 6)
		MESSAGE	"* Zu viele Festplattenpartitionen! *"
	endif
	endif
	ELSE
		PRTVAL	16,"* ZBIOS-Programmcode  :",DATEN-ZBIOS,"H *"
		PRTVAL	16,"* ZBIOS-Speicherbedarf:",ENDE-ZBIOS,"H *"
		PRTVAL	10,"*",ENDE-LAST,"Bytes frei (dezimal) *"
	ENDIF
	LISTING ON

; Portadressen (hier nichts {ndern, hardwarespezifisch!):
;
	IF	HARD		; nur wenn Festplatte vorhanden
GIDE	EQU	080h		; Basisadresse GIDE-Interface
RTC	EQU	GIDE+5		; I/O-Adresse Echtzeituhr
IDEDOR	EQU	GIDE+6		; Digital Output Register
IDEDAT	EQU	GIDE+8		; Data Register
IDEERR	EQU	GIDE+9		; Error Register
IDESCNT	EQU	GIDE+0AH	; Sector Count
IDESNUM	EQU	GIDE+0BH	; Sector Number
IDECLO	EQU	GIDE+0CH	; Cylinder Low
IDECHI	EQU	GIDE+0DH	; Cylinder High
IDESDH	EQU	GIDE+0EH	; Drive and Head
IDECMD	EQU	GIDE+0FH	; Command/Status
	ENDIF	; HARD
;
CSHSR	EQU	040H		; Chipselect Hauptstatusregister FDC
CSDAT	EQU	041H		; Chipselect Datenpuffer FDC
DAK	EQU	041H		; DMA-Acknowledge FDC
;INP	EQU	0F4H		; Input-Gate FDC
;SEL	EQU	0F6H		; Select-Latch
;TCFDC	EQU	0F8H		; Terminalcount FDC
;
CTC0	EQU	0H		; Vorteiler f}r CTC-Uhr - ohne GIDE, sonst frei
CTC1	EQU	1H		; Interruptkanal CTC-Uhr - ohne GIDE, sonst frei
CTC2	EQU	2H		; frei
CTC3	EQU	3H		; Kanal f}r Laufwerksteuerung


;AC1-System

OUTHL   EQU  07F1H
RSA     EQU  01A5H        ;CPU-Register nach RSA - nicht Monitor 11 !
REGIST  EQU  05CEH        ;Registeranzeige - nicht Monitor 11 !
MV24A   EQU  0DF9H        ;MO-UP V24-Out
;
;
CUPOS   EQU  1800H
BREAK   EQU  1818H        ;NMI Sprungziel
CBWSB   EQU  181FH        ;Merkzelle fuer das Farbbyte im AC1-Monitor
;                         ;0=kein Color-BWS, sonst der eingestellte Farbwert
ARG1    EQU  185BH
ARG2    EQU  185DH
;
;
WARMBT  EQU  0000H        ;WBOOT der Grundseite
BDOSF   EQU  0005H        ;BDOS-Aufruf + TPA-MAX
RST38   EQU  0038H        ;CP/M-Break
NMI     EQU  0066H
;
HPRDSK  EQU  0B000H       ;Hilfsprogramm: Format RAM-Disk
COMLEN  EQU  CCP+07       ;CCP-Kommandopuffer-Laenge
;
P04     EQU  04H          ;PIO_A-Daten
P05     EQU  05H          ;PIO_B-Daten
P06     EQU  06H          ;PIO_A-Control
;
	if vscch = 0	; FA-Mode (AC2010)
cpmumsch	equ	1Eh	; CP/M - Umschalter, Bit0=1 64K RAM aktiv (1)
	else
cpmumsch	equ	14h	; CP/M - Umschalter, Bit2=1 64K RAM aktiv (4)
	endif
;
RAF     EQU  0E0H         ;Grundadresse der RAM-Disk
BWSP    EQU  0F0H         ;BWS-CPLD-(Steuer)Port
;
CFDC    EQU  40H          ;Hauptstatusregister FDC
DFDC    EQU  41H          ;Datenport FDC

	if vscch = 0	; FA-Mode (AC2010)
FLWAIT  EQU  43H          ;IO-Adr. /WAIT Monoflop
LATCH   EQU  45H          ;IO-Adr. Latch 74LS175
	else
FLWAIT  EQU  50H          ;IO-Adr. /WAIT Monoflop (SCCH)
LATCH   EQU  48H          ;IO-Adr. Latch 74LS175 (SCCH)
	endif
;

CUZEI   EQU  0FH          ;KursorSymbol
;


ZBIOS:
;---------------------------------------------------------------
; Initialisierung
	JP	BOOT		;0	BIOS-Kaltstart
	JP	WBOOT		;1	BIOS-Warmstart
;---------------------------------------------------------------
; Zeichen Ein-/Ausgabe
	JP	CONST		;2	CONSOLE-Status abfragen
	JP	CONIN		;3	CONSOLE-Eingabe
	JP	CONOUT		;4	CONSOLE-Ausgabe
	JP	LIST		;5	LIST-Ausgabe
	JP	PUNCH		;6	PUNCH-Ausgabe
	JP	READER		;7	READER-Eingabe
;---------------------------------------------------------------
; Laufwerksfunktionen
	JP	HOME		;8	Spur Null einstellen
	JP	SELDSK		;9	Laufwerk ausw{hlen
	JP	SETTRK		;10	Spur ausw{hlen
	JP	SETSEC		;11	Sektor ausw{hlen
	JP	SETDMA		;12	Datenpufferadresse setzen
	JP	READ		;13	Sektor lesen
	JP	WRITE		;14	Sektor schreiben
;---------------------------------------------------------------
; sonstige Funktionen
	JP	LISTST		;15	LIST-Status abfragen
	JP	SECTRAN		;16	Sektornummer umrechnen
;---------------------------------------------------------------
; KC-spezifische Funktionen

	if vZSDOS=0	; CPM
VERS:	db 'C85AC'		; Kennung KC85
	jp      EXIT		; BIOS+38h
	jp      CLOAD		; BIOS+3Bh
	jp      0		; BIOS+3Eh
	else		; ZSDOS
	JP	RTCIO		;17	Treiber f}r RTC-Uhr
	jp	CPTPA		;18
	jp	CLOAD		;19
	jp	EXIT		;20
VERS:	DB	'85'		; Kennung KC85
	endif
;
CCPADR:	DW	CCP		; CCP-Ladeadresse
;
NMBDRV:	DB	NDRIVES		; Anzahl logischer Laufwerke (5-16)
;
DPHPTR:	DW	DPHTAB		; Beginn DPH-Tabelle
;
; Tabelle der Parameterk|pfe
;
DPHTAB:	DW	DPHA	; DPH f}r RAM-Floppy
	DW	DPHB	; Bootlaufwerk 780K
	DW	DPHC	; (Festplatte 1. Partition)
	DW	DPHD	; (Festplatte 2. Partition)
	DW	DPHE	; (Festplatte 3. Partition)
	if ndrives >= 6
	DW	DPHF	; F: bis P: frei konfigurierbar
	endif
	if ndrives >= 7
	DW	DPHG
	endif
	if ndrives >= 8
	DW	DPHH
	endif
	if ndrives >= 9
	DW	DPHI
	endif
	if ndrives >= 10
	DW	DPHJ
	endif
	if ndrives >= 11
	DW	DPHK
	endif
	if ndrives >= 12
	DW	DPHL
	endif
	if ndrives >= 13
	DW	DPHM
	endif
	if ndrives >= 14
	DW	DPHN
	endif
	if ndrives >= 15
	DW	DPHO
	endif
	if ndrives >= 16
	DW	DPHP
	endif
;
; Diskettenparameterk|pfe:
;
DPHA:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBA,CSVA,ALVA
;
DPHB:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBB,CSVB,ALVB
;
DPHC:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBC,CSVC,ALVC
;
DPHD:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBD,CSVD,ALVD
;
DPHE:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBE,CSVE,ALVE
;
	if ndrives >= 6
DPHF:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBF,CSVF,ALVF
	endif
;
	if ndrives >= 7
DPHG:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBG,CSVG,ALVG
	endif
;
	if ndrives >= 8
DPHH:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBH,CSVH,ALVH
	endif
;
	if ndrives >= 9
DPHI:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBI,CSVI,ALVI
	endif
;
	if ndrives >= 10
DPHJ:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBJ,CSVJ,ALVJ
	endif
;
	if ndrives >= 11
DPHK:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBK,CSVK,ALVK
	endif
;
	if ndrives >= 12
DPHL:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBL,CSVL,ALVL
	endif
;
	if ndrives >= 13
DPHM:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBM,CSVM,ALVM
	endif
;
	if ndrives >= 14
DPHN:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBN,CSVN,ALVN
	endif
;
	if ndrives >= 15
DPHO:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBO,CSVO,ALVO
	endif
;
	if ndrives >= 16
DPHP:	DB	8 DUP (0)
	DW	DIRBUF
	DW	DPBP,CSVP,ALVP
	endif
;
; Diskettenparameterbl|cke:
;

	DW	IORAM		; Zugriff auf RAM-Floppy
DPBA:	DW	128		; SPT = 128 Sektoren/Spur = 16 KByte-Bloecke
	DB	4,15		; BSH, BLM-1	2k-Bloecke
	DB	0		; EXM		weniger als 256 Bloecke
	DW	503		; DSM		503 Bloecke
	DW	63		; DRM		64 DIR-Eintraege
	DB	80H,0		; AL01		Verzeichnis-Belegungsvektor
	DW	0		; CKS		festes Laufwerk
	DW	1		; OFFS		1 Systemspur
;
	DW	IODISK		; Zugriff auf Diskette
DPBB:	CALDPB	9,2,128,2,0	; B: 780K, Lw 0 (Systemlaufwerk)

; Laufwerk C:

	IF	HARD			; Festplatte ist vorhanden
	DW	IOHARD		; C: Festplatte 1. Partition
DPBC:	CALHDD	C

	ELSE	; HARD
	DW	IODISK		; C: alternativ als Diskettenlaufwerk
DPBC:	CALDPB	PHYS_C,BLO_C,DIR_C,OFF_C,UNIT_C
	ENDIF	; HARD

; Laufwerk D:

	IF	HARD >= 2
	DW	IOHARD		; D: Festplatte 2. Partition
DPBD:	CALHDD	D
	ELSE	; HARD >= 2
	DW	IODISK		; D: alternativ als Diskettenlaufwerk
DPBD:	CALDPB	PHYS_D,BLO_D,DIR_D,OFF_D,UNIT_D
	ENDIF	; HARD >= 2

; Laufwerk E:

	IF	HARD >= 3
	DW	IOHARD		; E: Festplatte 3. Partition
DPBE:	CALHDD	E
	ELSE	; HARD >= 3
	DW	IODISK		; E: alternativ als Diskettenlaufwerk
DPBE:	CALDPB	PHYS_E,BLO_E,DIR_E,OFF_E,UNIT_E
	ENDIF	; HARD >= 3

; Laufwerk F:

	if ndrives >= 6
	IF	HARD >= 4
	DW	IOHARD		; F: Festplatte 4. Partition
DPBF:	CALHDD	F
	ELSE	; HARD >= 4
	DW	IODISK		; F: als Diskettenlaufwerk
DPBF:	CALDPB	PHYS_F,BLO_F,DIR_F,OFF_F,UNIT_F
	ENDIF	; HARD >= 4

	endif	; ndrives >= 6

; Laufwerk G:

	if ndrives >= 7
	IF	HARD >= 5
	DW	IOHARD		; G: Festplatte 5. Partition
DPBG:	CALHDD	G
	ELSE	; HARD >= 5
	DW	IODISK		; G: als Diskettenlaufwerk
DPBG:	CALDPB	PHYS_G,BLO_G,DIR_G,OFF_G,UNIT_G
	ENDIF	; HARD >= 5

	endif	; ndrives >= 7

; Laufwerk H:

	if ndrives >= 8
	IF	HARD >= 6
	DW	IOHARD		; H: Festplatte 6. Partition
DPBH:	CALHDD	H
	ELSE	; HARD >= 6
	DW	IODISK		; H: als Diskettenlaufwerk
DPBH:	CALDPB	PHYS_H,BLO_H,DIR_H,OFF_H,UNIT_H
	ENDIF	; HARD >= 6

	endif	; ndrives >= 8

; Laufwerk I:

	if ndrives >= 9
	DW	IODISK
DPBI:	CALDPB	PHYS_I,BLO_I,DIR_I,OFF_I,UNIT_I
	endif

; Laufwerk J:

	if ndrives >= 10
	DW	IODISK
DPBJ:	CALDPB	PHYS_J,BLO_J,DIR_J,OFF_J,UNIT_J
	endif

; Laufwerk K:

	if ndrives >= 11
	DW	IODISK
DPBK:	CALDPB	PHYS_K,BLO_K,DIR_K,OFF_K,UNIT_K
	endif

; Laufwerk L:

	if ndrives >= 12
	DW	IODISK
DPBL:	CALDPB	PHYS_L,BLO_L,DIR_L,OFF_L,UNIT_L
	endif

; Laufwerk M:

	if ndrives >= 13
	DW	IODISK
DPBM:	CALDPB	PHYS_M,BLO_M,DIR_M,OFF_M,UNIT_M
	endif

; Laufwerk N:

	if ndrives >= 14
	DW	IODISK
DPBN:	CALDPB	PHYS_N,BLO_N,DIR_N,OFF_N,UNIT_N
	endif

; Laufwerk O:

	if ndrives >= 15
	DW	IODISK
DPBO:	CALDPB	PHYS_O,BLO_O,DIR_O,OFF_O,UNIT_O
	endif

; Laufwerk P:

	if ndrives >= 16
	DW	IODISK
DPBP:	CALDPB	PHYS_P,BLO_P,DIR_P,OFF_P,UNIT_P
	endif

	TITLE	"ZBIOS - Bootroutinen"
	NEWPAGE

;
; BIOS 0 (Kaltstart)	- Interrupts starten
;			- CDISK, IOBYTE r}cksetzen
;			- BDOS nachladen
;			- weiter wie bei WBOOT

	OUTRADIX 16
aInfo:		if vZSDOS = 0
		db 0Ch,"HRCPM AC1 V1.2 CPA24.08.86 "
		else
		db 0Ch,"HRDOS AC1 V1.2 ZSDOS "
		endif
		if vscch = 0
		db "FA-MODE"
		else
		db "SCCH-MODE"
		endif
		db " ConIn INT",0Fh,' '
		db "Stand 03.12.11",0Dh,0Ah
		db 0Dh,0Ah
		if HARD > 0
		db "GIDE(\{GIDE}H) C0:16MB,  D0: 16MB  E0: 16MB",0Dh,0Ah
		endif
		db "FDC (\{LATCH}H) B0:5*1024 F0:16*256 G1:5*1024",0Dh,0Ah
		db 0Dh,0Ah
		db "RFL Praeci.Port \{RAF}H A: 1024k -->Format A:(J)?",0
	OUTRADIX 10

boot:		di
		ld	sp, BOOTSP
		xor	a
		ld	(INPxx), a
		ld	a, 82h ; '‚'
		ld	(CUFLAG), a
		ld	a, ' '
		ld	(CHAR), a
		call	BWSINI
		in	a, (P05)
		res	3, a		;ACC-Zeichensatz ein (PIO1B, Bit3)
		out	(P05), a
		ld	hl, (BREAK)	;Sprungtabelle NMI Monitor
		ld	(OLDNMI), hl	;sichern
		ld	hl, ZBIOS+3	;CP/M-Warmstart-Einsprungadresse
		ld	(BREAK), hl
		call	CPMON
		xor	a
		ld	(HSTACT), a
		ld	(IOBYTE), a
		ld	(CDISK), a
		ld	(ZEIPUF), a	;Init. Tastaturpuffer
		call	BOOT3		;CP/M "Grundseite" INIT
;
		ld	a, 3		; Reset	CTC
		out	(CTC0), a
		out	(CTC1), a
		out	(CTC2), a
		out	(CTC3), a
		ld	a, HIGH(ISRTAB)
		ld	i, a
		im	2
		ld	hl, IRET	; Dummy-Routine
		ld	(ISR0),	hl
		ld	(ISR1),	hl
		ld	(ISR2),	hl
		ld	(ISR1),	hl
		ld	hl, ISRC3	;Einsprungpunkt CTC-Interrupt - Steuerung Nachlauf Floppy-LW
		ld	(ISR3),	hl	;Prog. Einsprung IV fuer CTC-Kanal 3
		ld	a, LOW(ISR0)
		out	(CTC0), a	;CTC-Interruptadresse NWT (= 80h) eintragen
		ld	a, 0FFh
		ld	(MTIME), a	;Init. CTC-Counter = 255 --> Zusatzzaehler fuer CTC-Interrupt
		ld	a, 0A7h
		out	(CTC3), a	;Programmierung der CTC K3 als Zeitgeber mit Interrupt, Vorteiler 256
		xor	a
		out	(CTC3), a	;Programmierung der CTC K3 - Hauptteiler = 256
					;d.h. bei 2 MHz CPU-Takt wird alle 33ms ein CTC-Interrupt ausgeloest
;
		ld	hl, ISRTAS	;1. Einsprungpunkt PIO-Interrupt --> Taste in Tastaturpuffer schreiben
		ld	(ISR4),	hl
		ld	hl, loc_EA01	;2. Einsprungpunkt PIO-Interrupt
		ld	(ISR5),	hl
		ld	bc, 5*100h+P06
		ld	hl, PIOTAB	;PIO Init auf ISRTAS
		otir
;
		ld	hl, aInfo	; "\fHRDOS AC1 V1.2 ZSDOS SCCH-MODE ConIn I"...
		call	print		; Textausgabe
;
		ei
		call	conin
		res	5, a		;Upcase
		cp	'J'		;J --> RAM-Disk formatieren
		call	z, RDFORM	; Format RAM-Floppy
		jr	BOOT1		;READCCP ueberspringen
;
; BIOS 1 (Warmstart)	- Puffer leeren
;			- CCP nachladen und starten
;
wboot:		LD	SP, BOOTSP	; Boot-Stack
		call	cpmon
		LD	A, (HSTWRT)	; Puffer mit Schreibdaten gef}llt?
		OR	A
		CALL	NZ, WRHST	; wenn ja, Puffer schreiben
		LD	B, 44		; Anzahl Sektoren f}r CCP
		LD	HL, READ
		ld	(WRCCP2+1), hl
		call	WRCCP		;hier READ_CCP!
		ld	hl, WRITE
		ld	(WRCCP2+1), hl
;		
BOOT1:		call	BOOT3		;CP/M "Grundseite" INIT
		ld	a, 1		;Kursor einschalten
		ld	(CUFLAG), a
		ld	a, (COLO2)
		ld	(COLOR), a	;Arbeitszelle Farbbyte
		and	77h		;Intensivdarstellung maskieren
		ld	(COLO1), a	;Arbeitszelle "Normaldarstellung"
		ld	a, (CDISK)	; CCP-Laufwerk
		and	1Fh
		ld	c, a
		call	SELDSK  	; LW einstellen
		ld	hl, (CCPADR)	; Adresse CCP
		jp	(hl)		; CCP aufrufen

;RST38
;Eintritt bei Code FF (rst 38h)
RESTR:		push	af
		push	hl
		call	EXIT1
		rst	18h
		db " CP",'M' + 80H
		pop	hl
		pop	af
		call	1A5h		;  Register im Registerrettebereich ablegen/auslesen
		pop	hl
		dec	hl
		call	7F1h		; OUTHL Ausgabe HL hexadezimal
		rst	18h
		db 8Dh
		call	5CEh		; Register anzeigen
		rst	38h

;CP/M "Grundseite" INIT
BOOT3:		ld	a, 0C3h		; JUMP-Befehl
		ld	(0), a		; Warmstart
		ld	(BDOS), a	; BDOS-Call
		ld	(38h), a	; RST38
		ld	(66h), a	; NMI
		ld	hl, ZBIOS+3	; BIOS-Warmstart regenerieren
		ld	(1), hl
		ld	(67h), hl	; auch bei NMI eintragen
		ld	hl, DOS+6	; BDOS-Eintritt regenerieren
		ld	(BDOS+1), hl
		ld	hl, RESTR	; RST38
		ld	(39h), hl
		ld	hl, 80h		; Standard-DMA-Buffer
		ld	(SEKDMA), hl
		ret
;
; CPM-Modus
;
CPMON:		push	af
		if vscch = 0
		ld	a, 1
		else
		ld	a, 4
		endif
		jr	cpmoff1
;
; AC1-Modus
;
CPMOFF:		push	af
		xor	a
cpmoff1:	out	(cpmumsch), a
		pop	af
		ret
;
; CRC-SDLC-Berechnung (für RAM-Disk)
; in de=adr, hl=laenge
; out bc=crc16 (sdlc)
;
crc:		ld	bc, 0FFFFh
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
		and	0F0h ; 'ð'
		xor	c
		ld	c, a
		pop	af
		and	0E0h ; 'à'
		xor	b
		ld	b, c
		ld	c, a
		inc	de
		dec	hl
		ld	a, h
		or	l
		jr	nz, crc1
		ret
;
;EXIT - Routine
;
EXIT:		ld	hl, 0		;Ruecksprungadresse fuer RET
		push	hl
EXIT1:		ld	a, 0		;CPMOFF
		out	(cpmumsch), a
		xor	a
		out	(11h), a	;??
		ld	hl, (OLDNMI)	
		ld	(BREAK), hl	;alten NMI wiederherstellen
		ld	a, 3		; CTC-Reset
		out	(CTC0), a
		out	(CTC1), a
		out	(CTC2), a
		out	(CTC3), a
		OUT  (P06),A	;PIO1	A: Interrupt auschalten
		LD   A,0CFH
		OUT  (P06),A	;PIO1	A: "Mode 3" + "Set Mode"
		LD   A,0FFH
		OUT  (P06),A	;PIO1	A: PA0 bis PA7 sind Eingaenge
		RET		;zurueck zum Monitor
;
;Interrupt-Einsprung fuer die Tastatur
;
ISRTAS:		di
		push	af
		push	bc
		push	hl
		in	a, (P04)	;PIO_A abfragen
		bit	7, a		;Taste gedrueckt?
		jr	z, ISREND	;nein, ISR beenden
		ld	c, a
;		
		ld	b, 1Eh		;Tastaturentprellung
PRELL1:		in	a, (P04)
		cp	c
		jr	nz, ISREND
		djnz	PRELL1		;30 Durchlaeufe
;
		cp	80h
		jr	nz, loc_E9C6	;?? wozu
;
loc_E9C6:	ld	hl, PIOTAB2	;PIO Init auf ISR5 / loc_EA01
		ld	bc, 2 * 100h + P06	; 2 Byte auf P06 ausgeben
		otir
;		
		ld	c, a
		ld	hl, ZEIPUF
		ld	a, (hl)		;Fuellstand Zeichenpuffer
		inc	a
		cp	1Fh
		jr	z, ISREND	;Puffer voll?
		ld	(hl), a		;Anzahl der Zeichen rueckschreiben
		push	bc
		ld	c, a
		add	hl, bc		;HL = Position im Zeichenpuffer
		pop	bc
		res	7, c		;7. Bit loeschen und
		ld	(hl), c		;Taste in Zeichenpuffer eintragen
;
		ld	bc, 8034h	;Initialisierung Tastenpiep --> 2 MHz
BEEP1:		ld	a, c
BEEP2:		dec	a
		jr	nz, BEEP2
		in	a, (P05)	;PIO_B-Daten
		xor	1		;Tastenpiep
		out	(P05), a
		djnz	BEEP1
		and	0FEh
		out	(P05), a
;		
		if vscch = 0
		ld	bc, 200h	;Tastaturentprellung
		else
		ld	bc, 1000h	;Tastaturentprellung
		endif
PRELL2:		dec	bc
		ld	a, b
		or	c
		jr	nz, PRELL2
;		
ISREND:		pop	hl
		pop	bc
		pop	af
		ei
		reti

; ISR5 PIO
;2. Einsprungpunkt PIO-Interrupt
loc_EA01:	di
		push	af
		push	bc
		push	hl
		ld	hl, PIOTAB
		ld	bc, 5 * 100h + P06
		otir
		jr	ISREND

;PIO 1A: Init. auf ISR4 / ISRTAS
PIOTAB:		db  LOW(ISR4)
		db 0CFh	; "Mode 3" + "Set Mode" (kein Handshaking, next Byte = I/O-Zuordnung)
		db 0FFh	; PA0 bis PA7 sind Eingaenge
		db 0B7h	; Setting Interrupt Control Word --> mit nachfolgender Maskierung
		db  7Fh	; Bit7=L --> nur PA7 erzeugt Interrupts


;PIO Init auf ISR5 / loc_EA01
PIOTAB2:	db  LOW(ISR5)
		db  87h	; Setting Interrupt Control Word
;
; Textausgabe
;
print:		push	bc
		push	af
print1:		ld	c, (hl)
		ld	a, c
		or	a
		jr	z, print2
		call	conout
		inc	hl
		jr	print1
print2:		pop	af
		pop	bc
		ret
		xor	a
		ret

	TITLE	"ZBIOS - Zeichenein/ausgabe"
	NEWPAGE
;
; BIOS 2 (Konsolenstatus abfragen)
; PE:	-
; PA:	A=0	kein Zeichen bereit
;	A=FFh	Zeichen von Eingabe bereit
;
const:		ld	a, (ZEIPUF)
		or	a
		ret	z
		ld	a, 0FFh
		ret
;
; BIOS 7 (Zusatzeingabe)
; PE:	-
; PA:	A	empfangenes Zeichen
;
READER:
;
; BIOS 3 (Konsoleneingabe)
; PE:	-
; PA:	A	eingegebenes Zeichen
;
CONIN:  DI
	LD   (SPCIN),SP
	LD   SP,CONINSP
	PUSH BC
	PUSH DE
	PUSH HL
CONIN1: XOR  A
	DI
	LD   HL,(ZEIPUF)  ;L = Anz. der Zeichen, H = 1. Zeichen
	OR   L
	JR   NZ,CONIN3    ;Zeichen im Tastaturpuffer vorhanden
	EI
	DJNZ CONIN1       ;256*LOOP: warte auf ein Zeichen
	DI
	out	(cpmumsch), a	;AC1 Mode
	LD   HL,(CUPOS)
	INC  C
	BIT  6,C          ;?
	LD   A,(CHAR)     ;bisheriges Zeichen auf der Kursorposition
	JR   Z,CONIN2
	LD   A,CUZEI      ;Kursor = 0FH
CONIN2: LD   (HL),A       ;"Kursor-Blinken" - Bildschirm-Ausgabe
	CALL CPMON       ;CP/M Mode
	JR   CONIN1
;
CONIN3: DEC  A            ;DEC Anzahl_der_Zeichen_im_Tastaturpuffer
	LD   (ZEIPUF),A
	LD   A,H          ;1. Zeichen aus dem Tastaturpuffer --> A
	LD   HL,ZEIPUF+2
	LD   DE,ZEIPUF+1
	LD   BC,001FH
	LDIR              ;alle Zeichen im Puffer ruecken 1 Pos. n. links
	call	UPCASE
	POP  HL
	POP  DE
	POP  BC
	LD   SP,(SPCIN)
	EI
	RET

; Zeichen in Grossbuchstaben
UPCASE:		cp	'A'
		ret	c
		ret	nc
		cp	5Fh
		jr	z, upcase1
		ret
		cp	5Eh
		jr	c, upcase1
		cp	'a'
		ret	c
		cp	7Eh
		ret	nc
upcase1:	xor	20h ; upcase
		ret
;
; BIOS 5 (Druckerausgabe)
; PE:	C	Zeichen
; PA:	-
;
list:		ret
;
; BIOS 6 (Zusatzausgabe, V.24 beim KC)
; PE:	C	Zeichen
; PA:	-
;
punch:		ret
;
; BIOS 4 (Konsolenausgabe)
; PE:	C	Zeichen
; PA:	-
;
CONOUT: DI
	LD   (SYSSK),SP  ;Stack bei MON-Aufruf retten
	LD   SP,CONOUTSP
	PUSH AF
	PUSH BC
	PUSH DE
	PUSH HL
	CALL CPMOFF
	PUSH HL
	LD   HL,(CUPOS)   ;Cursorposition
	LD   A,(buff)
	LD   (HL),A       ;altes Zeichen auf Position
	LD   A,(STZ00)    ;Durchlauf bei Steuerzeichen?
	AND  A
	JP   NZ,KONV10    ;NZ= Steuerzeichen 2. oder 3. Durchlauf
	LD   A,C          ;Byte
	CP   20H          ;kleinstes Zeichen
	JR   C,KONVTB
	CP   7FH          ;groesstes Zeichen
	JR   NC,KONVTB
	LD   HL,(CUPOS)
	JP   CCHAR        ;Ausgabe einzelnes Zeichen
;
KONVT9: LD   HL,(CUPOS)
	LD   A,(HL)
	LD   (buff),A
	LD   A,(CUFLAG)
	cp	1
	JR   NZ,COEN1      ;Kursor nicht darstellen
	LD   (HL),CUZEI
COEN1:  CALL CPMON
	POP  HL
	POP  DE
	POP  BC
	POP  AF
	LD   SP,(SYSSK)
	EI
	RET
;
;
;Verzweigung bei Steuerzeichen
;INPUT:  A=Zeichen
;
KONVTB: CP   1BH          ;ESC ?
	JR   NZ,KONVT1
	LD   A,02H        ;wenn ja
	LD   (STZ00),A    ;Zeiger auf 2
	JR   KONVT9       ;nichts ausgeben
;
KONVT1: LD   HL,(CUPOS)
	CP   01H          ;^A Cursor home
	JR   Z,CHOME
	CP   08H          ;^H Cursor links
	JP   Z,CLEFT
	CP   0AH          ;^J Zeilenvorschub
	JP   Z,CLF
	CP   0CH          ;^L CLS
	JR   Z,BCLS
	CP   0DH          ;^M Wagenruecklauf
	JP   Z,CCRET
	CP   14H          ;^T BS ab Cursor loeschen
	JP   Z,CBLOE
	CP   15H          ;^U Cursor nach rechts
	JP   Z,CRIGH
	CP   16H          ;^V Zeile ab Cursor loeschen
	JP   Z,CZLOE
	CP   18H          ;^X Zeile ab Cursor loeschen + CR
	JP   Z,CZLCR
	CP   1AH          ;^Z Cursor hoch
	JP   Z,CHOCH
;
	LD   B,A          ;Zeichen merken
	LD   A,(COLOR)
	AND  A            ;Test auf Color-BWS
	JP   Z,KONVT9     ;Z = monochrom
	LD   A,B
;
	cp	2
	JP   Z,CUON
	CP   82H          ;Kursor ein
	JP   Z,CUON
	cp	3
	JP   Z,CUOFF
	CP   83H          ;Kursor aus
	JP   Z,CUOFF
	CP   84H          ;normale Darstellung
	JP   Z,BNORM
	CP   85H          ;invers
	JP   Z,BINVN
	CP   86H          ;intensiv - HihgLight
	JP   Z,BINTE
	CP   87H          ;intensiv + invers
	JP   Z,BIVIN
	JP   KONVT9
;
CHOME:  LD   HL,17FFH
	LD   (CUPOS),HL
	JP   KONVT9
;
BCLS:   LD   A,20H        ;0Ch  Bildschirm loeschen
	CALL BCL1         ;Loeschroutine
	LD   (CUPOS),HL   ;Cursorposition
	LD   A,(COLOR)    ;Farbbyte
	AND  77H          ;Farb-BWS vorhanden?
	JP   Z,KONVT9     ;Z = nein, dann Ende
	CALL BWSCOL       ;Farbspeicher ein
	LD   A,(COLOR)
	CALL BCL1
	CALL BWSZEI       ;Farbspeicher aus
	JP   KONVT9
;
CLEFT:  INC  HL           ;08h  Cursor links
	JP   CHOLI
;
CLF:    LD   DE,0040H     ;0Ah  Zeilenvorschub
	AND  A
	SBC  HL,DE
	JP   SCROL
;
BCL1:   LD   HL,1000H     ;Anfang BWS
	LD   (HL),A
	LD   D,H
	LD   E,L
	INC  E
	PUSH BC
	LD   BC,07FFH     ;Laenge BWS
	LDIR
	POP  BC
	RET
;
CCRET:  LD   A,L          ;0Dh  Wagenruecklauf
	OR   3FH
	LD   L,A
	JP   SCROL
;
CBLOE:  PUSH HL
	LD   A,CUZEI      ;14h  BS ab Cursor loeschen
CBLO1:  LD   (HL),20H
	DEC  HL
	CP   H
	JR   NZ,CBLO1
	POP  HL
	LD   A,(COLOR)
	LD   B,A
	AND  77H
	JP   Z,KONVT9
	CALL BWSCOL       ;Farbspeicher ein
	LD   A,CUZEI
CBLO2:  LD   (HL),B
	DEC  HL
	CP   H
	JR   NZ,CBLO2
	CALL BWSZEI       ;Farbspeicher aus
	JP   KONVT9
;
CRIGH:  DEC  HL           ;15h  Cursor nach rechts
	JP   SCROL
;
CZLOE:  PUSH HL           ;16h  Zeile ab Cursor loeschen
	LD   A,L
	AND  3FH
	LD   B,A
	INC  B
CZLO1:  LD   (HL),20H
	DEC  HL
	DJNZ CZLO1
	POP  HL
	LD   A,(COLOR)
	LD   C,A
	AND  77H
	JP   Z,KONVT9
	CALL BWSCOL       ;Farbspeicher ein
	LD   A,L
	AND  3FH
	LD   B,A
	INC  B
CZLO2:  LD   (HL),C
	DEC  HL
	DJNZ CZLO2
	CALL BWSZEI       ;Farbspeicher aus
	JP   KONVT9
;
CZLCR:  PUSH HL           ;18h  Zeile ab Cursor loeschen + CR
	LD   A,L
	OR   3FH
	LD   L,A
	LD   (CUPOS),HL
	PUSH HL
	LD   B,40H        ;Zeilenlaenge
CZLC1:  LD   (HL),20H
	DEC  HL
	DJNZ CZLC1
	POP  HL
	LD   A,(COLOR)
	LD   C,A
	AND  77H
	JP   Z,KONVT9
	CALL BWSCOL       ;Farbspeicher ein
	LD   B,40H
CZLC2:  LD   (HL),C
	DEC  HL
	DJNZ CZLC2
	CALL BWSZEI       ;Farbspeicher aus
	JP   KONVT9
;
CHOCH:  LD   DE,0040H     ;1Ah     Cursor hoch
	ADD  HL,DE
;
CHOLI:  LD   A,18H        ;Begrenzung Cursor hoch und links
	CP   H
	JP   Z,KONVT9
	LD   (CUPOS),HL
	JP   KONVT9
;
CCHAR:  LD   (HL),A
	LD   A,(COLOR)
	LD   C,A
	AND  77H
	JR   Z,CCHA1
	CALL BWSCOL       ;Farbspeicher ein
	LD   (HL),C
	CALL BWSZEI       ;Farbspeicher aus
CCHA1:  DEC  HL
;
SCROL:  LD   (CUPOS),HL
	EX   DE,HL        ;Test ob Scrollen noetig
	LD   HL,0FFFH
	AND  A
	SBC  HL,DE        ;DE = akt. Cursorpos.
	EX   DE,HL
	JP   C,KONVT9     ;CY = nicht scrollen
	LD   A,20H        ;Leerzeichen
	CALL SCR11        ;Scrollen
	LD   A,(COLOR)
	AND  77H
	JP   Z,KONVT9     ;Z = kein Farbspeicher
	CALL BWSCOL       ;Farbspeicher ein
	LD   A,(COLOR)    ;Farbbyte
	CALL SCR11
	CALL BWSZEI       ;Farbspeicher aus
	JP   KONVT9
;
SCR11:  LD   HL,17BFH     ;Umladeroutine Scrollen
	LD   DE,17FFH
	LD   BC,07C0H
	LDDR
	EX   DE,HL
	LD   (CUPOS),HL
	INC  HL
	LD   B,40H        ;eine Zeilenlaenge
SCR12:  DEC  L
	LD   (HL),A       ;Leerzeile schreiben
	DJNZ SCR12
	RET
;
BNORM:  LD   A,(COLO1)    ;84h  normale Darstellung
	AND  77H
	LD   (COLOR),A
	LD   (COLO1),A
	JP   KONVT9
;
BINVN:  LD   A,(COLO1)    ;85h  invers EIN
	AND  77H
	RRCA
	RRCA
	RRCA
	RRCA
	LD   (COLOR),A
	JP   KONVT9
;
BINTE:  LD   A,(COLO1)    ;86h  intensiv EIN
	AND  77H
	OR   08H
	LD   (COLOR),A
	JP   KONVT9
;
BIVIN:  LD   A,(COLOR)    ;87h  intensiv + invers
	AND  77H
	OR   08H
	RRCA
	RRCA
	RRCA
	RRCA
	LD   (COLOR),A
	JP   KONVT9
;
CUON:   LD   A,01H        ;82h Cursor EIN
	LD   (CUFLAG),A
	JP   KONVT9
;
CUOFF:  XOR  A            ;83h Cursor AUS
	LD   (CUFLAG),A
	JP   KONVT9
;
; Behandlung ESC-Sequenzen
;
; Input: ESC + 2 Steuerzeichen
; wenn 1. Zeichen > 80h dann Cursorposition
; sonst Auswertung Farbe / Grafikzeichen
; (stz00) = Zaehler fuer Zeichenanzahl
;
KONV10: LD   A,(STZ00)
	CP   02H          ;2 = 1. Steuerzeichen (Zeile)
	JR   NZ,KONV20    ;sonst 3. Durchlauf (Spalte)
	LD   A,C
	LD   (STZ01),A    ;erstes Zeichen merken
	LD   A,01H        ;Zeiger fuer 3. Durchlauf
	LD   (STZ00),A
	JP   KONVT9       ;nichts ausgeben
;
; Verzweigung Cursorposition / weitere Funktionen
;
KONV20: LD   A,C
	LD   (STZ02),A    ;zweites Zeichen merken
	XOR  A
	LD   (STZ00),A    ;Durchlaufzaehler ruecksetzen
	LD   HL,STZ01
	LD   A,(HL)
	BIT  7,A          ;welche Funktion? NZ = Cursor
	JR   Z,KONV22     ;Z = weitere Funktionen

; Berechnung direkte Cursorposition
; OUT:    HL = BWS-Adresse
;
	PUSH AF
	LD   A,C
	AND  3FH          ;max. Spaltenzahl
	LD   L,A
	POP  AF
	AND  1FH          ;max. Zeilenzahl
	LD   D,00H
	LD   H,D
	LD   E,A
	LD   B,06H
KONV21: SLA  E
	RL   D
	DJNZ KONV21
	ADD  HL,DE
	EX   DE,HL
	LD   HL,17FFH     ;Ende BWS-RAM
	AND  A
	SBC  HL,DE
;
; Cursor direkt positionieren
; IN:    HL = Adresse Cursor
;
	PUSH HL
	LD   HL,(CUPOS)	  ;alte Position
	LD   A,(buff)	  ;Zeichen holen
	LD   (HL),A	      ;auf BS schreiben
	POP  HL
	LD   (CUPOS),HL   ;neue Position setzen
	LD   A,(HL)       ;neues Zeichen holen
	LD   (buff),A     ;und merken
	JP   KONVT9
;
; Auswertung weiterer ESC-Funktionen
; IN:      A = 1. Steuerzeichen
;
; Ausgabe Grafikzeichen aus AC1-ZG ( > 80h )
;
KONV22: CP   5FH          ;Grafikzeichen?
	JR   NZ,KONV24    ;NZ = nein
	INC  HL
	LD   A,(HL)       ;2. Zeichen holen
	LD   HL,(CUPOS)
	JP   CCHAR        ;Zeichen ausgeben
;
; Einstellen der Farbe (nur Color-BWS)
;
KONV24: CP   5DH          ;Farbe?
	JP   NZ,KONVT9    ;NZ = nein
	LD   A,(COLOR)    ;Color-BWS?
	AND  A
	JP   Z,KONVT9     ;Z = nein
	INC  HL
	LD   A,(HL)       ;2. Zeichen holen
	AND  77H          ;intensiv maskieren
	LD   B,A
	RRCA
	RRCA
	RRCA
	RRCA
	CP   B            ;Zeichen- und HG-Farbe gleich?
	JP   Z,KONVT9     ;Z = Ja, dann ignorieren
	LD   A,B
	LD   (COLOR),A    ;Farbe setzen
	AND  77H          ;intensiv maskieren
	LD   (COLO1),A    ;Backup
	JP   KONVT9
;
BWSCOL: IN   A,(BWSP)      ;bwsport
	SET  2,A          ;Farbspeicher ein
	JR   BWSZ1
;
BWSZEI: IN   A,(BWSP)      ;bwsport
	RES  2,A          ;Farbspeicher aus
BWSZ1:  AND  07H
	OUT  (BWSP),A
	RET
;
BWSINI: XOR  A            ;Farbbyte auf Monochrom setzen
	LD   (COLO2),A    ;Arbeitszelle Farbbyte im BIOS
	ld      a, (CBWSB)    ;Farbbyte aus dem AC1-Monitor
	and     0Fh
	LD   C,0F0H       ;I/O-Adresse COLOR-BWS
	IN   B,(C)        ;Konfigbyte COLOR-BWS lesen
	INC  B            ;fuer COLOR-BWS muss B<255 sein
	RET  Z            ;kein COLOR-BWS vorhanden --> Ende
	;hier wird die Routine verlassen, wenn kein CPLD vorh. ist
	DEC  B            ;Konfigbyte COLOR-BWS: Farb-RAM aus
	LD   D,B
	SET  2,D          ;Konfigbyte COLOR-BWS: Farb-RAM ein
	LD   HL,1000H
	LD   E,(HL)       ;Zeichen aus 1000h
	OUT  (C),D        ;Farb-RAM einschalten
	LD   (HL),A       ;Farbe auf 1000h setzen
	CP   (HL)         ;ist Farb-RAM gesteckt?
	OUT  (C),B        ;Farb-RAM ausschalten
	LD   (HL),E       ;Zeichen auf 1000h zurueckschreiben
	RET  NZ           ;Farbe hat nicht geklappt --> Ende
	;hier wird die Routine verlassen, wenn kein Farb-RAM steckt
	OUT  (C),D        ;Farb-RAM einschalten
	PUSH BC
	LD   DE,1001H
	LD   BC,07FFH
	LDIR              ;Farb-RAM initialisieren
	LD   A,(HL)       ;Farbe aus 17FFh ruecklesen
	POP  BC
	OUT  (C),B        ;Farb-RAM ausschalten
	LD   (COLO2),A    ;Merkzelle fuer WBOOT im BIOS
	RET

	xor     a
	dec     a
	ret

;
; BIOS 15 (LIST-Status abfragen)
; PE:	-
; PA:	A=0	Druckpuffer voll
;	A=FFh	Druckpuffer aufnahmef{hig
;
LISTST:		xor	a
		dec	a
		ret

	TITLE	"ZBIOS - logischer Diskettentreiber"
	NEWPAGE
;
; BIOS 9 (Laufwerk ausw{hlen)
; PE:	C	Laufwerk
; PA:	HL	DPH-Adresse
;	HL=0	Laufwerk nicht installiert
; SP:	2
;
SELDSK:	LD	HL,NMBDRV	; Anzahl inst. Laufwerke
	LD	A,C		; mit anzuw{hlendem Laufwerk
	CP	(HL)		; vergleichen und
	LD	HL,0		; bei Fehler mit HL=0
	RET	NC		; zur}ckkehren
	LD	HL,(DPHPTR)	; Tabelle
	LD	B,0
	ADD	HL,BC
	ADD	HL,BC
	LD	E,(HL)		; heraussuchen
	INC	HL
	LD	D,(HL)
	EX	DE,HL		; DPH-Adresse nach HL
	LD	A,H
	OR	L
	RET	Z		; 0 in DPH-Tabelle
	LD	A,C
	LD	(SEKDSK),A	; Laufwerk und dazugeh|rige
	LD	(SEKDPH),HL	; DPH-Adresse f}r sp{ter merken
DUMMY:	RET
;
; BIOS 8 (Positionieren zu Spur 0)
; PE:	-
; PA:	-
; SP:	2
;
HOME:	LD	BC,0		; Spur 0
;
; BIOS 10 (Spur ausw{hlen)
; PE:	BC	Spurnummer
; PA:	-
; SP:	2
;
SETTRK:	LD	(SEKTRK),BC	; Spur merken
	RET
;
; BIOS 11 (Sektor ausw{hlen)
; PE:	BC	Sektornummer
; PA:	-
; SP:	2
;
SETSEC:	LD	(SEKSEC),BC	; Sektor merken
	RET
;
; BIOS 12 (DMA-Adresse setzen)
; PE:	BC	DMA-Adresse
; PA:	-
; SP:	2
;
SETDMA:	LD	(SEKDMA),BC	; DMA-Adresse merken
	RET
;
; BIOS 16 (Sektornummer umrechnen)
; PE:	BC	Sektornummer
;	DE	Adresse der Umwandlungstabelle
; PA:	HL	umgewandelte Sektornummer
; SP:	2
;
SECTRAN:LD	H,B
	LD	L,C
	LD	A,D
	OR	E
	RET	Z		; bei DE=0 keine Umrechnung
	EX	DE,HL
	LD	B,0		; Umrechnungstabellen sind 8 Bit
	ADD	HL,BC		; Tabellenposition berechnen
	LD	L,(HL)		; L-Teil aus Tabelle lesen
	LD	H,D		; H-Teil regenerieren
	RET
;
; BIOS 13 (Sektor von Diskette lesen)
; PE:	-
; PA:	A=0	kein Fehler
;	A=1	Fehler
; SP:	2 (bei RAM-Floppy)
;	16 (bei Festplatte oder Diskette)
;
READ:	LD	C,2		; Daten lesen
	LD	A,4		; 'read'
	DB	21H		; LD HL,nn
;
; BIOS 14 (Sektor auf Diskette schreiben)
; PE:	C=0	Daten schreiben
;	C=1	Directory schreiben (Sektor wird sofort zur}ckgeschrieben!)
;	C=2	Neue Daten schreiben (nicht unterst}tzt)
; PA:	A=0	kein Fehler
;	A=1	Fehler
;
WRITE:	LD	A,6		; 'write'
	LD	(SEKSTB),A	; Steuerbyte eintragen
	LD	A,C
	LD	(WRTYPE),A	; Schreibmodus
	LD	HL,IORET
	PUSH	HL		; RET-Adresse
	LD	HL,(SEKDPH)	; DPH-Adresse des akt. Laufwerkes
	LD	DE,10		; DPB-Adresse
	ADD	HL,DE
	LD	A,(HL)		; holen
	INC	HL
	LD	H,(HL)
	LD	L,A		; HL=DPB
	DEC	HL
	LD	A,(HL)
	DEC	HL
	LD	L,(HL)
	LD	H,A
	PUSH	HL		; Treiberadresse (IORAM, IODISK, IOHARD)
	LD	HL,SEKSTB	; Parameter mitgeben
	RET			; Treiber starten
	;
IORET:	OR	A		; Fehler aufgetreten?
	RET	Z		; nein
	LD	A,1		; Errorcode
	RET

	INCLUDE	IO_RAM.INC	; RAM-Floppy-Treiber einbinden

; AC1-Bios-Erweiterung
; BIOS 18 / HRDOS Befehl C
; TPA von 2000H nach 0100H umladen
aOkTpaVon2000hN:db " OK TPA von 2000H nach 0100H umgeladen!",0

CPTPA:		ld	hl, 2000h
		ld	de, 0100h
		ld	bc, 0B000h
		ldir
		ld	hl, aOkTpaVon2000hN ; "	OK TPA von 2000H nach 0100H umgeladen!"...
		call	print		; Textausgabe
		ret

; AC1-Bios-Erweiterung
; BIOS 19 / HRDOS Befehl L
; File mit Turbo in RAM laden
CLOAD:		di
		ld	(word_F4D9), sp	; SP sichern
		ld	sp, LOADSP
		push	af
		push	bc
		push	de
		push	hl
		xor	a		; CPMOFF
		out	(cpmumsch), a
		;
		ld	de, 0FFFh
		rst	18h
		db  8Dh	; 
		ld	a, ' '
		ld	(CHAR), a
		ld	hl, 2000h	; Laden nach 2000h
		ld	(ARG1), hl
		call	0B32h		; LOAD im SCCH-Monitor V8
		ld	de, (ARG1)	; AADR
		ld	hl, (ARG2)	; EADR
		push	de
		and	a
		sbc	hl, de
		push	hl		; Programmlaenge
		ld	a, l
		ld	l, 0
		and	a
		jr	z, CLOAD1
		inc	h		; aufrunden auf volle xx00h
		;Umrechnen h->dez.
CLOAD1:		xor	a
CLOAD2:		add	a, 1
		daa
		jr	nc, CLOAD3
		inc	l
CLOAD3:		dec	h
		jr	nz, CLOAD2
		ld	h, l		; h=Anz. 100h-Blöcke hex
		ld	l, a		; L=Anz. 100h-Blöcke dezimal
		rst	18h
		db "Anzahl 100H Bloecke:",0A0h
		call	CPMON
		ld	a, h		; Ausgabe Anz. Blöcke hez
		call	OUTAX
		ld	a, l		; Ausgabe Anz. Blöcke Dez
		call	OUTAX
		;
		pop	bc		; Programmlaenge
		ld	de, 100h	; nach TPA
		pop	hl		; von AADR (2000h)
		ldir			; umladen
		pop	hl
		pop	de
		pop	bc
		pop	af
		ld	sp, (word_F4D9)	; SP restaurieren
		ei
		ret

; Ausgabe A hexadezimal
OUTAX:		push	af
		rra
		rra
		rra
		rra
		call	OUTAX1
		pop	af
OUTAX1:		and	0Fh
		add	a, 90h
		daa
		adc	a, 40h
		daa
		jp	OUTAX2
;
OUTAX2:		push	hl
		push	de
		push	bc
		ld	e, a
		ld	c, 2
		call	5
		pop	bc
		pop	de
		pop	hl
		ret


	TITLE	"ZBIOS - Treiber f}r Laufwerke"
	NEWPAGE
;
; Parameterblock w{hlen und Blockung testen
; PA:	HL	Zeiger auf Parameter
;	A	Parameter aus DPB
;	Z=1	wenn A=0
; VR:	HL,AF
; SP:	6
;
CHKBLK:	LD	HL,(SEKDPH)	; aktuellen Parameterkopf
	LD	(DPHPAR),HL	; in Arbeitszelle }bernehmen
GETPSH:	LD	L,15		; phys. Sektorgr|~e PSH
GETPAR:	PUSH	DE
	LD	DE,10		; Offset zu DPB
	LD	H,D		; H=0
	PUSH	HL
	LD	HL,(DPHPAR)
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	POP	HL		; Index
	ADD	HL,DE		; addieren
	POP	DE
	LD	A,(HL)		; Parameter holen
	OR	A		; und auf 0 testen
	RET
;
; Harddisktreiber:
; PE:	HL=SEKSTB
; SP:	14
;
	IF	HARD
IOHARD:	INC	HL		; SEKDSK
	SET	7,(HL)		; Kennung Festplatte eintragen
	DEC	HL
	ENDIF	; HARD
;
; Diskettentreiber:
; PE:	HL=SEKSTB
; SP:	14
;
IODISK:	XOR	A
	LD	(ERFLAG),A	; Fehler r}cksetzen
	CALL	IODRV
	LD	A,(ERFLAG)	; Fehlercode holen
	RET
;
; gemeinsamer Treiber f}r Diskette/Festplatte:
;
IODRV:	LD	C,(HL)		; Kommando (lesen oder schreiben)
	CALL	CHKBLK		; Parameterblock w{hlen
	JP	Z,NOBLK		; keine Blockung -> direkter Zugriff
	LD	A,C		; Kommando
	CP	6
	LD	A,1		; Arbeitsrichtung: Lesen
	JR	NZ,RWOPER
	XOR	A		; Arbeitsrichtung: Schreiben
RWOPER:	LD	(READOP),A
	CALL	GETPSH		; PSH holen
	LD	B,A
	LD	HL,(SEKSEC)	; log. Sektornummer
RWLOOP:	SRL	H
	RR	L
	DJNZ	RWLOOP
	LD	(PHYSEC),HL	; phys. Sektornummer
	LD	HL,HSTACT
	LD	A,(HL)
	LD	(HL),1		; Pufferdaten werden jetzt g}ltig
	OR	A
	JR	Z,FILHST	; es waren bisher keine Daten im Puffer
	LD	A,(SEKDSK)
	LD	HL,HSTDSK
	CP	(HL)
	JR	NZ,NOMTCH	; anderes Laufwerk
	LD	HL,(HSTTRK)
	LD	DE,(SEKTRK)
	SBC	HL,DE		; gleiche Spur?
	JR	NZ,NOMTCH	; nein
	LD	HL,(HSTSEC)
	LD	DE,(PHYSEC)
	SBC	HL,DE		; gleicher phys. Sektor?
	JR	Z,MATCH		; ja
NOMTCH:	LD	A,(HSTWRT)	; noch Schreibdaten im Puffer?
	OR	A
	CALL	NZ,WRHST	; Puffer schreiben
FILHST:	LD	HL,SEKDSK
	LD	DE,HSTDSK
	LD	BC,9
	LDIR			; aktuelle Pufferdaten kopieren
	LD	HL,(PHYSEC)
	LD	(HSTSEC),HL	; phys. Sektornummer eintragen
	CALL	RDHST		; Daten in Puffer lesen
	XOR	A
	LD	(HSTWRT),A	; keine Schreibdaten im Puffer
MATCH:	CALL	GETPSH
	INC	HL		; zeigt auf PHM
	LD	A,(SEKSEC)	; LOW-Teil der log. Sektornummer
	AND	(HL)		; phys. Sektormaske
	RRA
	LD	H,A
	LD	A,0
	RRA
	LD	L,A
	LD	DE,HSTBUF	; 1K-Puffer
	ADD	HL,DE		; Beginn im Puffer
	LD	DE,(SEKDMA)
	LD	A,(READOP)
	OR	A		; Lesen oder schreiben?
	JR	NZ,RWMOVE	; Lesebefehl
	INC	A
	LD	(HSTWRT),A	; Schreibdaten im Puffer
	EX	DE,HL		; Quelle <-> Ziel
RWMOVE:	LD	BC,128
	LDIR			; log. Sektor }bertragen
	LD	A,(ERFLAG)	; Fehlercode
	OR	A
	JR	NZ,RWERR	; Lesefehler aufgetreten
	LD	A,(WRTYPE)
	DEC	A
	RET	NZ		; nicht Directory-Schreiben
	LD	(HSTWRT),A	; Pufferdaten sind geschrieben
	CALL	WRHST		; Daten in Puffer schreiben
	LD	A,(ERFLAG)
	OR	A
	RET	Z		; keine Fehler
RWERR:	XOR	A
	LD	(HSTACT),A	; Pufferdaten sind ung}ltig!
	RET
;
; Puffer schreiben/lesen:
; SP:	6 bei Festplatte
;	14 bei Diskette
;
WRHST:	LD	A,6		; Schreibkommando
	DB	21H		; LD HL,nn
RDHST:	LD	A,4		; Lesekommando
	LD	(CMD),A
	LD	HL,HSTBUF	; 1K-Puffer als
	LD	(HSTDMA),HL	; DMA-Puffer f}r phys. Sektoren > 128 Byte
	LD	HL,HSTDSK	; Puffer-Parameterblock verwenden
	JR	LODPAR
	;
NOBLK:	LD	A,(SEKSTB)	; Kommando
	LD	(CMD),A		; f}r direkten Zugriff kopieren
	LD	HL,SEKDSK	; BIOS-Parameterblock verwenden
LODPAR:	LD	DE,DSKPAR	; Arbeitsparameter
	LD	BC,9		; }bernehmen
	LDIR			; (danach ist BC=0)
	IF	HARD
	LD	A,(DSKPAR)	; Laufwerkscode
	RLA			; Disk oder Festplatte?
	JP	C,HDISK		; Zugriff auf Festplatte
	ENDIF	; HARD
;	JR	FDISK		; Zugriff auf Diskette

	INCLUDE	IO_FDC.INC	; FDC-Treiber einbinden

	IF HARD
	INCLUDE	IO_GIDE.INC	; GIDE-Treiber einbinden
	ENDIF


	TITLE	"ZBIOS - Treiber f}r RTC-Uhr des GIDE"
	NEWPAGE
;
; BIOS 17 (Uhrzeit holen/einstellen f}r RTC-Uhrentreiber)
; PE:	C=0	Zeit holen
;	C=1	Uhr stellen
;	DE	Zeiger auf 6-Byte-Feld im DateStamper-Format
; PA:	A=1	Zugriff erfolgreich
;	A=FFh	Fehler
;	HL	DE+5 (Sekunden-Feld)
;	E	Sekunden des }bergebenen Puffers
;
	IF	HARD

	if	vscch=0
RTCIO:	ex      de, hl
	inc     hl
	inc     hl
	inc     hl
	inc     hl
	inc     hl
	ld      a, 1
	ret
	else
RTCIO:	EX	DE,HL		; Feld nach HL }bergeben
	DEC	C
	JR	Z,SETRTC	; Uhr stellen
	INC	C
	JR	Z,GETRTC	; Zeit holen
	LD	A,0FFH		; C-Register falsch
	RET
	endif
;
; Zeit holen:
;
GETRTC:	CALL	RTCWT		; RTC-Zugriff vorbereiten
	LD	B,0BH		; mit Jahres-Zehner beginnen
RTCRD1:	IN	A,(C)		; Zehnerstelle holen
	AND	0FH
	RLCA
	RLCA
	RLCA
	RLCA
	LD	E,A
	DEC	B
	IN	A,(C)		; Einerstelle holen
	AND	0FH
	OR	E		; mit Zehnerstelle kombinieren
	LD	E,(HL)		; alten Wert nach E
	LD	(HL),A		; und in RAM-Variable eintragen
	INC	HL
	DEC	B
	JP	P,RTCRD1	; wiederholen bis Sekunden bearbeitet
	JR	RTCST2
;
; Uhr stellen:
;
SETRTC:	CALL	RTCINI		; RTC initialisieren
	LD	B,0BH		; mit Jahres-Zehner beginnen
RTCST1:	LD	A,(HL)		; BCD-Wert holen
	RRCA
	RRCA
	RRCA
	RRCA
	AND	0FH		; nur Zehner
	OUT	(C),A		; Zehner stellen
	LD	A,(HL)		; BCD-Wert nochmal
	AND	0FH		; nur Einer
	DEC	B
	OUT	(C),A		; Einer stellen
	LD	E,(HL)		; E mit R}ckgabewert laden
	INC	HL
	DEC	B
	JP	P,RTCST1	; wiederholen bis Sekunden bearbeitet
RTCST2:	DEC	HL		; zur}ck auf Sekundenfeld
	LD	B,0DH
	XOR	A
	OUT	(C),A		; HOLD-Bit r}cksetzen
	INC	A		; OK
	RET
;
; RTC-Schaltkreis f}r Zugriff vorbereiten
;
RTCINI:	LD	BC,0F00H+RTC	; Register F
	IN	A,(C)
	AND	0FH
	CP	4		; Uhr OK?
	JR	Z,RTCWT
	LD	A,5
	OUT	(C),A		; 24H-Modus, Reset
	DEC	A
	OUT	(C),A		; Reset verlassen
RTCWT:	LD	BC,0D00H+RTC	; Register D
	LD	A,1
	OUT	(C),A		; HOLD-Bit setzen
	IN	A,(C)
	BIT	1,A		; BUSY?
	RET	Z		; OK
	XOR	A
	OUT	(C),A		; HOLD-Bit r}cksetzen
	JR	RTCWT

	ELSE	; HARD
;
; BIOS 17 (Uhrzeit holen/einstellen f}r CTC-Uhrentreiber)
; PE:	C=0	Zeit holen
;	C=1	Uhr stellen
;	DE	Zeiger auf 6-Byte-Feld im DateStamper-Format
; PA:	A=1	Zugriff erfolgreich
;	A=FFh	Fehler
;	HL	DE+5 (Sekunden-Feld)
;	E	Sekunden des }bergebenen Puffers
;
RTCIO:	LD	HL,TIME		; Zeitablage
	EX	DE,HL
	DI
	LD	B,6		; 6 Byte }bertragen
	DEC	C
	JR	Z,SETTIM	; Uhr stellen
	INC	C
	JR	Z,GETTIM	; Zeit holen
	LD	A,0FFH		; C-Register falsch
	EI
	RET
;
; Zeit holen:
;
GETT1:	INC	HL
	INC	DE
GETTIM:	LD	A,(DE)		; TIME lesen
	LD	C,(HL)		; alter Pufferinhalt
	LD	(HL),A		; in Puffer }bergeben
	DJNZ	GETT1
	LD	E,C		; Sekunden alt
	LD	A,1		; OK
	EI
	RET
;
; Uhr stellen:
;
SETT1:	INC	HL
	INC	DE
SETTIM:	LD	A,(HL)		; Pufferinhalt lesen
	LD	(DE),A		; in TIME ablegen
	DJNZ	SETT1
	LD	E,A		; Sekunden
	LD	A,1
	EI
	RET
;
; Interrupt CTC-Kanal 1 (Systemuhr, wenn kein GIDE-Interface vorhanden ist)
;
ISRC1:	LD	(ISTACK),SP	; SP merken
	LD	SP,ISTACK	; eigener Stack
	PUSH	HL
	PUSH	AF
	LD	HL,TIME+5	; Sekundenfeld
	LD	A,(HL)
	ADD	A,1		; SEK+1
	DAA
	LD	(HL),A
	CP	60H
	JR	NZ,TIMEOK
	LD	(HL),0		; SEK=0
	DEC	HL
	LD	A,(HL)
	ADD	A,1		; MIN+1
	DAA
	LD	(HL),A
	CP	60H
	JR	NZ,TIMEOK
	LD	(HL),0		; MIN=0
	DEC	HL
	LD	A,(HL)
	ADD	A,1		; STD+1
	DAA
	LD	(HL),A
	CP	24H
	JR	NZ,TIMEOK
	LD	(HL),0		; STD=0
	DEC	HL
	LD	A,(HL)
	ADD	A,1		; TAG+1
	DAA
	LD	(HL),A
	CP	32H		; alle Monate haben 31 Tage!
	JR	NZ,TIMEOK
	LD	(HL),1		; TAG=1
	DEC	HL
	LD	A,(HL)
	ADD	A,1		; MONAT+1
	DAA
	LD	(HL),A
	CP	13H
	JR	NZ,TIMEOK
	LD	(HL),1		; MONAT=1
	DEC	HL
	LD	A,(HL)
	ADD	A,1		; JAHR+1
	DAA
	LD	(HL),A
TIMEOK:	POP	AF
	POP	HL
	LD	SP,(ISTACK)
	EI
	RETI
;
; Datum/Uhrzeit bei CTC-Uhr:
;
TIME:	DB	96H,1,1		; 01.01.1996
	DB	0,0,0		; 00:00:00
	Db	6 dup(0)		; Stack fuer CTC-Interrupt
ISTACK:	DW	0		; Ablage alter SP

	ENDIF	; HARD

; ????
		di
		push	af
		call	CPMOFF
		call	7EEh
		call	CPMON
		pop	af
		ei
		ret
; ????
		di
		push	af
		call	CPMOFF
		call	7F1h
		rst	8
		call	CPMON
		pop	af
		ei
		ret
; Stackbereiche
; Console Input 26 Byte
		db "CONIN-ZBIOS-STACK==26BYTESCONIN-ZBIOS-STACK==26BYTES"
CONINSP:
; Console Output
		db "CONOUT-STACK_CONOUT-STACK_CONOUT-STACK"
CONOUTSP:
; Load 48 Bytes
		db "LOAD-ZBIOS-STACK=48BYTESLOAD-ZBIOS-STACK=48BYTES"
LOADSP:
;
; Stack fuer Bootprogramme:
;
		db "ZBIOS(C)ML-SOFT 2003 & AC1-BIOS(C)HR 2011"
BOOTSP:
		db    0
RAFTRK:		db 0	; RD Tracknummer (max. 128 bei 1M = Bit 0-6)
RAFSEC:		db 0	; RD Sektor
		db    0
		db    0
		db    0
		db    0
		db    0
		db    0
word_F4C7:	dw 0	; Hilfsspeicher in RDSKWR
		db    0
CUFLAG:		db 1	; 1 - Cursor on, 0 - Cursor off
CHAR:		db 20h
STZ00:		db 0
STZ01:		db 0
STZ02:		db 0
buff:		db 0
COLOR:		db 0	;Arbeitszelle Farbbyte
COLO1:		db 0	;Arbeitszelle "Normaldarstellung"
COLO2:		db 0	;Arbeitszelle "Intensiv", 0 - Monochrom

		db    0
		db    0
SPCIN:		dw 0
SYSSK:		dw 0
word_F4D9:	dw 0	; orig. SP bei CLOAD
OLDNMI:	dw 0
		db    0
		db    0

;
; Floppy-Datenbereich:
; --------------------
;
DATEN:			; ab hier Datenbereich (nicht initialisiert)
CODE1:	DB	0	; Befehlscode lesen/schreiben
CODE2:	DB	0	; FDC-Befehlscode
HDDS:	DB	0	; Kopfauswahl in Bit 2, phys. Lw in Bit 0/1
CYL:	DB	0	; Zylindernummer (Spur)
HEAD:	DB	0	; Kopf
REC:	DB	0	; Sektornummer
N:	DB	0	; Zahl der Datenbytes/Sektor
EOT:	DB	0	; letzte Sektornummer/Zylinder
GPL:	DB	0	; GPL-L}ckenl{nge
DTL:	DB	0	; Datenl{nge
DTYP:	DB	0	; Drive-Typ
FTP:	DB	0	; erste Spur mit Pr{kompensation
TSS:	DB	0	; Schrittzeit
HLT:	DB	0	; Kopfladezeit
RESULT:	Db	7 dup(0)	; Raum f}r Ergebnisdaten (ST0/ST1/ST2/C/H/R/N)
RCYL:	DB	0	; Zwischenspeicher Spurnummer
LUNIT:	DB	0	; letztes physisches Laufwerk
MTIME:	DB	0	; Motoreinschaltzeit (f}r CTC)

INPxx:		db 0

;BIOS-Parameterblock (Vorgabewerte)
SEKSTB:	DB	4	; Steuerbyte (4=lesen, 6=schreiben)
SEKDSK:	DB	0	; Laufwerk (Bit7=1 bei Festplatte)
SEKTRK:	DW	0	; Spurnummer
SEKSEC:	DW	0	; log. Sektornummer
SEKDMA:	DW	0	; DMA-Adresse f}r log. Sektor
SEKDPH:	DW	0	; DPH-Adresse

;Parameterblock (Daten des Pufferinhalts)
HSTDSK:	DB	0	; Laufwerk (Bit7=1 bei Festplatte)
HSTTRK:	DW	0	; Spurnummer
HSTSEC:	DW	0	; phys. Sektornummer
HSTDMA:	DW	0	; Adresse des 1K-Sektorpuffers
HSTDPH:	DW	0	; DPH-Adresse

;Parameterblock (Arbeitszellen)
DSKPAR:	DB	0	; Laufwerk (Bit7=1 bei Festplatte)
TRKPAR:	DW	0	; Spur
SECPAR:	DW	0	; Sektor
DMAPAR:	DW	0	; DMA-Adresse
DPHPAR:	DW	0	; DPH-Adresse

;Arbeitszellen FD/HD-Treiber:
CMD:	DB	4	; 4=lesen, 6=schreiben
PHYSEC:	DW	0	; phys. Sektornummer
HSTACT:	DB	0	; g}ltige Daten im Puffer
HSTWRT:	DB	0	; Schreibdaten im Puffer vorhanden
ERFLAG:	DB	0	; FDC-Fehlercode
READOP:	DB	0	; 0=schreiben, 1=lesen
WRTYPE:	DB	0	; Schreibmodus
			; (0=Daten, 1=Directory, 2=neuer Block oder Lesen)
;
HSTBUF:	DB	1024 dup(0)	; Sektorpuffer f}r Blockein/ausgabe
;
DIRBUF:	DB	128 dup (0)	; Verzeichnispuffer
;
; Belegungsvektoren:
;
ALV:	; ab hier die Belegungsvektoren

ALVB:	DB	50 DUP(0)		; 400 Bl|cke (800K bei 2K-Bl|cken)

	IF	HARD >= 1
ALVC:	DB	(BLOCK_C+7)/8 dup (0)	; Bl|cke f}r Partition Festplatte C:
	ELSE
ALVC:	DB	50 DUP(0)
	ENDIF

	IF	HARD >= 2
ALVD:	DB	(BLOCK_D+7)/8 dup (0)	; Bl|cke f}r Partition Festplatte D:
	ELSE
ALVD:	DB	50 DUP(0)
	ENDIF

	IF	HARD >= 3
ALVE:	DB	(BLOCK_E+7)/8 dup (0)	; Bl|cke f}r Partition Festplatte E:
	ELSE
ALVE:	DB	50 DUP(0)
	ENDIF

	IF ndrives >= 6
	IF	HARD >= 4
ALVF:	DB	(BLOCK_F+7)/8 dup (0)	; Bl|cke f}r Partition Festplatte F:
	ELSE
ALVF:	DB	50 DUP(0)
	ENDIF
	ENDIF

	IF ndrives >= 7
	IF	HARD >= 5
ALVG:	DB	(BLOCK_G+7)/8 dup (0)	; Bl|cke f}r Partition Festplatte G:
	ELSE
ALVG:	DB	50 DUP(0)
	ENDIF
	ENDIF

	IF ndrives >= 8
	IF	HARD >= 6
ALVH:	DB	(BLOCK_H+7)/8 dup (0)	; Bl|cke f}r Partition Festplatte H:
	ELSE
ALVH:	DB	50 DUP(0)
	ENDIF
	ENDIF

	IF ndrives >= 9
ALVI:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk I:
	ENDIF

	IF ndrives >= 10
ALVJ:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk J:
	ENDIF

	IF ndrives >= 11
ALVK:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk K:
	ENDIF

	IF ndrives >= 12
ALVL:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk L:
	ENDIF

	IF ndrives >= 13
ALVM:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk M:
	ENDIF

	IF ndrives >= 14
ALVN:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk N:
	ENDIF

	IF ndrives >= 15
ALVO:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk O:
	ENDIF

	IF ndrives >= 16
ALVP:	DB	50 DUP(0)		; Bl|cke f}r Laufwerk P:
	ENDIF

;
; Pr}fsummenvektoren f}r die Diskettenlaufwerke:
;
CSV:	; ab hier die Pruefvektoren

CSVA:				; RAM-Floppy ist festes Laufwerk

CSVB:	DB	32 DUP(0)		; Pr}fvektoren f}r 128 Eintr{>=

CSVC:
	IF	HARD = 0
	DB	32 DUP(0)		; nur wenn C: Diskette ist
	ENDIF

CSVD:
	IF	HARD <= 1
	DB	32 DUP(0)		; nur wenn D: Diskette ist
	ENDIF

CSVE:
	IF	HARD <= 2
	DB	32 DUP(0)		; nur wenn E: Diskette ist
	ENDIF

	IF ndrives >= 6
CSVF:
	IF	HARD <= 3
	DB	32 DUP(0)		; nur wenn F: Diskette ist
	ENDIF
	ENDIF

	IF ndrives >= 7
CSVG:
	IF	HARD <= 4
	DB	32 DUP(0)		; nur wenn G: Diskette ist
	ENDIF
	ENDIF

	IF ndrives >= 8
CSVH:
	IF	HARD <= 5
	DB	32 DUP(0)		; nur wenn H: Diskette ist
	ENDIF
	ENDIF

	IF ndrives >= 9
CSVI:	DB	32 DUP(0)
	ENDIF

	IF ndrives >= 10
CSVJ:	DB	32 DUP(0)
	ENDIF

	IF ndrives >= 11
CSVK:	DB	32 DUP(0)
	ENDIF

	IF ndrives >= 12
CSVL:	DB	32 DUP(0)
	ENDIF

	IF ndrives >= 13
CSVM:	DB	32 DUP(0)
	ENDIF

	IF ndrives >= 14
CSVN:	DB	32 DUP(0)
	ENDIF

	IF ndrives >= 15
CSVO:	DB	32 DUP(0)
	ENDIF

	IF ndrives >= 16
CSVP:	DB	32 DUP(0)
	ENDIF

LAST:	DB	LOW(ZBIOS-$) Dup(1Ah)	; Rest bis zur n{chsten NEWPAGE mit 0 f}llen
ENDE:				; physische Endadresse (incl. ISRTAB)

ALVA	equ $

ISRTAB		org 0FF80h
ISR0:		ds 2		;Interrupt-Einsprungpunkt CTC Kanal 0
ISR1:		ds 2            ;Interrupt-Einsprungpunkt CTC Kanal 1
ISR2:		ds 2            ;Interrupt-Einsprungpunkt CTC Kanal 2
ISR3:		ds 2            ;Interrupt-Einsprungpunkt CTC Kanal 3	ISRC3
ISR4:		ds 2            ;Interrupt-Einsprungpunkt Nr.1 PIO1A	ISRTAS
ISR5:		ds 2            ;Interrupt-Einsprungpunkt Nr.2 PIO1A	loc_EA01

		org 0FFD0h
ZEIPUF:		ds 1		;1 + 32 Byte Puffer fuer Tastatur

;		end
