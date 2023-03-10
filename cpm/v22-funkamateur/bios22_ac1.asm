; File Name   :	d:\hobby3\cpm11\CPM64_FUA_bios_F600.bin
; Format      :	Binary file
; Base Address:	0000h Range: F600h - FCC0h Loaded length: 06C0h



;------------------------------------------------------------------------------
; BIOS V2.2 CP/M FA  ist die im FA 10/89 abgedruckte Version
; basiert auf cbios.asm aus 
; Manfred Kramer, "Praktische Mikrocomputertechnik", Milit‰rverlag DDR,1987
;
; 1 Laufwerk MP-RAM-Floppy 256K
;
; Port 1Eh CPM-Umschaltung
; Ports 20,22h,24h,26h	Drucker
; Ports E0..E7, EC..EF 256K-RAM-Disk
;------------------------------------------------------------------------------

		cpu	z80

;hi              function x, (x>>8) & 0ffh	; High-Byte
;CCP		equ	0E000h		; base of ccp
;BDOS       	equ	CCP+0800h	; base of bdos
;BIOS		equ	CCP+1600h	; base of bios
;IOLOC	EQU	3		;i/o definition byte.
;DISKA	EQU	4		;current drive name and user number.

;		org bios

NDISK:	EQU	1		;ANZAHL LAUFWERKE

CURS	equ	07Fh		;Zeichen f. Cursor

CPMON		macro
		ld	a, 0FFh
		out	(1Eh), a	; CPM-Mode on
		endm
CPMOFF		macro
		ld	a, 0
		out	(1Eh), a	; CPM-Mode off
		endm

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

entry:		jp	boot
WBOTE:		jp	wboot
		jp	const
		jp	conin
		jp	conout
		jp	list
		ld	a,1Ah
		ret			; punch
		ld	a,1Ah
		ret			; reader
		jp	HOME
		jp	SDISK
		jp	STRCK
		jp	SSEC
		jp	SDMA
		jp	RRDSK
		jp	WRDSK
		jp	lstst
		jp	STRAN

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

		db "CPMAC"

		jp	exit		; residentes Zusatzkommando CCP
		jp	load		; residentes Zusatzkommando CCP

unk_F63E	ds	6		; wboot1-Kopie
					; wozu wird das genutzt?


		align	100h		; warum?

;------------------------------------------------------------------------------
; KOPF AUF SPUR NULL STELLEN
;------------------------------------------------------------------------------

HOME:		ld	bc, 0
		jr	STRCK

;------------------------------------------------------------------------------
;LAUFWERK AUSWAEHLEN
;IN:C LAUFWERK
;OUT:HL DPH-VEKTOR
;------------------------------------------------------------------------------

SDISK:		ld	hl, 0
		push	bc
		ld	b, 0
		ld	a, c
		cp	NDISK
		jp	c, seldsk1

		;Fehler
		xor	a
		pop	bc
		ret

seldsk1:	ld	l, c
		ld	h, b
		add	hl, hl
		add	hl, hl
		add	hl, hl
		add	hl, hl
		ld	de, DPBAS
		add	hl, de
		ld	(DISKA), a
		pop	bc
		ret

;------------------------------------------------------------------------------
;SECTOR TRANSFOR.
;------------------------------------------------------------------------------

STRAN:		ld	b, 0
		ld	a, d
		or	e
		jr	nz, sectran1
		ld	h, b
		ld	l, c
		inc	hl
		ret
sectran1:		ex	de, hl
		add	hl, bc
		ld	a, (hl)
		ld	(SEC1),	a
		ld	l, a
		ret

;------------------------------------------------------------------------------
;SECTOR EINSTELLEN
;IN: C SECTOR
;OUT: (SEC1) =C
;------------------------------------------------------------------------------

SSEC:		ld	a, c
		ld	(SEC1),	a
		ret

;------------------------------------------------------------------------------
;SPUR ANWAEHLEN
;IN: C SPUR
;OUT (TRACK)=C
;------------------------------------------------------------------------------

STRCK:		ld	a, c
		ld	(TRACK), a
		ret

;------------------------------------------------------------------------------
;DMA SETZEN
;IN: BC DMA
;------------------------------------------------------------------------------

SDMA:		ld	(DMA), bc
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

inimsg:		db 0Ch,1Bh,3,0Ah
		db "CP/M 2.2  V 2.2  auf AC 1"
		db 1Bh,5,0Ah
		db "RAM - Disk neu formatieren (Y/n)?",0
asc_F786:	db 0Dh,0Ah,0Ah,0

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

boot:		ld	sp, 0
		ld	hl, CCP		; Kopie von CCP+BDOS
		ld	de, 1800h	; in RAM	
		ld	bc, 800h
		ldir
		ld	de, unk_F63E	; HL=f801, wboot1
		ld	c, 6		; in unk_F63E kopieren
		ldir
		ld	a, 1		; CPMON
		out	(1Eh), a
		ld	a, 0C3h		; jp
		ld	(0), a
		ld	hl, WBOTE
		ld	(1), hl
		ld	(5), a
		ld	hl, BDOS+6
		ld	(6), hl
		ld	(38h), a	;HALTEPUNKT VORBEREITEN
		ld	hl, RESTR
		ld	(39h), hl
		ld	hl, inimsg	; "CP/M   2.2   V 1.5  auf AC 1"
		call	STR
		call	conin
		and	5Fh
		cp	'Y'
		call	z, FRDSK	; Ramdisk formatieren
		ld	hl, asc_F786	; "\r\n\n"
		call	STR
		;
		xor	a
		ld	(lstflg), a
		ld	(IOLOC), a	; IOBYTE
		ld	(DISKA), a	; DISKA

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

wboot:		ld	bc, 80h
		call	SDMA
		xor	a
		out	(1Eh), a	; CPMOFF
		; restore CCP+BDOS
		ld	hl, 1800h
		ld	de, CCP
		ld	bc, 800h
		ldir
		ld	hl, unk_F63E	; rest von wboot
		ld	c, 6
		ldir
		ld	(1807h), a	; a=0
		inc	a		; a=1
		out	(1Eh), a	; CPMON
wboot1:		ld	a, (DISKA)
		ld	c, a
		jp	CCP

;------------------------------------------------------------------------------
; RAMDISK
;------------------------------------------------------------------------------

rafofs		dw	0

aEinenMomentBit:db 0Dh,0Ah
		db 0Ah
		db "Einen Moment bitte !",0Dh,0Ah
		db 0Ah,0
aEsStehen252Kby:db "Es stehen 252 KByte zu Verfuegung !",0Dh,0Ah
		db 0Ah,0


;------------------------------------------------------------------------------
; RAMDISK formatieren
;------------------------------------------------------------------------------

FRDSK:		ld	hl, aEinenMomentBit ; "\r\n\nEinen Moment bitte	!\r\n\n"
		call	STR
		ld	hl, 1
		ld	(SEC1),	hl
		ld	b, 2
		ld	l, 0
		ld	(TRACK), hl
		ld	de, 40h
frdsk1:		add	hl, de
		djnz	frdsk1
		ld	de, 2
		xor	a
		sbc	hl, de
		ld	(rafofs), hl	; merken
		ld	hl, 80h
		ld	(DMA), hl
		ld	de, 81h
		ld	bc, 7Fh
		ld	(hl), 0E5h 	; erased
		ldir

frdsk2:		call	WRDSK
		ld	a, (SEC1)
		inc	a
		ld	(SEC1),	a
		sub	11h
		jr	nz, frdsk2
		inc	a
		ld	(SEC1),	a
		ld	hl, (TRACK)
		inc	hl
		ld	(TRACK), hl
		ld	de, (rafofs)
		sbc	hl, de
		jr	c, frdsk2
		ld	hl, aEsStehen252Kby ; "Es stehen 252 KByte zu Verfuegung !\r\n\n"
		jp	STR

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------

rafpos:		dw 0			; Zwischenspeicher Adresse

;------------------------------------------------------------------------------
; RAMDISK schreiben
;------------------------------------------------------------------------------

WRDSK:		call	ADRE
		push	hl
		otir
		pop	hl
		call	crc
		call	sub_F913
		out	(c), e
		out	(c), d
		xor	a
		ret

;------------------------------------------------------------------------------
; RAMDISK lesen
;------------------------------------------------------------------------------

RRDSK:		call	ADRE
		push	hl
		inir
		pop	hl
		call	crc
		call	sub_F913
		in	l, (c)
		in	h, (c)
		xor	a
		sbc	hl, de
		ld	a, h
		or	l
		ret	z
		inc	a
		ret

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------

ADRE:		ld	hl, (TRACK)
		ld	de, 2		; +2, Offs. CRCs
		add	hl, de
		xor	a
		add	hl, hl
		add	hl, hl
		add	hl, hl
		add	hl, hl
		ld	de, (SEC1)
		dec	de
		add	hl, de
		ld	(rafpos), hl	; Adr. merken
		rr	h
		rr	l
		rr	a
		out	(0E7h),	a	; GADDR+7
		ld	a, l
		out	(0E6h),	a	; GADDR+6
		ld	a, h
		bit	2, a
		jr	nz, ADRE1
		out	(0EDh),	a
		jr	ADRE2
ADRE1:		out	(0ECh),	a
ADRE2:		bit	3, a
		jr	nz, ADRE3
		out	(0EFh),	a
		jr	ADRE4
ADRE3:		out	(0EEh),	a
ADRE4:		and	3
		or	0E0h
		ld	c, a
		ld	b, 80h
		ld	hl, (DMA)
		ret

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------

sub_F913:	ld	hl, (rafpos)	; Adr.
		ld	bc, 20h
		xor	a
		sbc	hl, bc
		add	hl, hl
		ld	a, l
		out	(0E7h),	a	; GADDR+7 
		ld	a, h
		out	(0E6h),	a	; GADDR+6
		ld	c, 0E0h		; GADDR
		ret

;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------

crc:		ld	hl, (DMA)
		ld	bc, 80h	
		ld	de, 0FFFFh
crc1:		ld	a, (hl)
		xor	d
		ld	d, a
		rrca
		rrca
		rrca
		rrca
		and	0Fh
		xor	d
		ld	d, a
		rrca
		rrca
		rrca
		push	af
		and	1Fh
		xor	e
		ld	e, a
		pop	af
		push	af
		rrca
		and	0F0h
		xor	e
		ld	e, a
		pop	af
		and	0E0h
		xor	d
		ld	d, e
		ld	e, a
		inc	hl
		dec	bc
		ld	a, b
		or	c
		jr	nz, crc1
		ret

;???
		ld	a, 1
		ret

;------------------------------------------------------------------------------
; Disk Parameter Header
;------------------------------------------------------------------------------

DPBAS:		
DPHA:		dw 0
		dw 0
		dw 0
		dw 0
		dw DIRBF
		dw DPBR			;GERAETEBESCHREIBUNG
		dw CHK0			;DIRECTORY-KONTROLLE
		dw ALL0			;BELEGUNG DISKETTE

;------------------------------------------------------------------------------
;DISK-PARAMETER-BLOCK RAM-DISK
; 2KByte pro Spur, 128 Spuren -> 256K
;
; Drive Characteristics
; 2048:	128 Byte Record	Capacity
; 252:	Kilobyte Drive Capacity		(-4k f. CRC-Bereich)
; 64:	32 Byte	Directory Entries
; 0:	Checked	Directory Entries
; 128:	Records/Extent
; 8:	Records/Block
; 16:	Sectors/Track			
; 0:	Reserved Tracks
;------------------------------------------------------------------------------

DPBR:		DW	16		;SECTOREN -> 2K pro Spur
		DB	4		;BLOCKGROESSE 2K
		DB	15		;BLM block mask
		DB	0		;EXM 16 Bit
		DW	125		;BLOCKANZAHL-1 -> 252K Diskkapazitaet
		DW	63		;64 DIRECTORY-ENTR.
		DB	0C0H		;1 BELEGTER BLOCK DURCH DIR.
		DB	0
		DW	0		;CKS check size
		DW	0		;0 SPUREN DURCH SYSTEM BELEGT

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

TRACK:		dw 0
SEC1:		dw 1		;BDOS-SEKTOR
DMA:		dw 80h		;BDOS-DMA
;
DIRBF:		ds 128		;INHALTSVERZ. DISKETTE
ALL0:		ds 128
CHK0:		ds 17

;------------------------------------------------------------------------------
; Console Status
;------------------------------------------------------------------------------

const:		in	a, (4)
		bit	7, a
		jr	nz, loc_FA97
		xor	a
		ret
loc_FA97:	xor	a
		dec	a
		ret

;------------------------------------------------------------------------------
;AUSGABE EINER ZEICHENFOLGE,ABSCHLUSS DER FOLGE MIT 00
;IN: ANFANGSADR. IN HL
;------------------------------------------------------------------------------

STR:		push	af
		push	bc
str1:		ld	a, (hl)
		or	a
		jr	z, str2
		ld	c, a
		call	conout
		inc	hl
		jr	str1
str2:		pop	bc
		pop	af
		ret

;------------------------------------------------------------------------------
; HALTEPUNKT
;------------------------------------------------------------------------------

RESTR:		push	af
		CPMOFF
		ld	hl, 1800h
		ld	de, CCP
		ld	bc, 800h
		ldir
		rst	18h
		db "CPM-BREAK",8Dh
		pop	af
		rst	38h		; durchreichen an Monitor
		;
sub_FAC7:	in	a, (4)
		bit	7, a
		ret	z
		call	MS30
		in	a, (4)
		bit	7, a
		ret

;------------------------------------------------------------------------------
; Console Ausgabe
;------------------------------------------------------------------------------

conout:		di
		ld	(SYSSK), sp
		ld	sp, load
		push	af
		push	hl
		push	de
		push	bc
		CPMOFF
		ld	hl, (cupos)
		ld	a, (buff)	; Zeichen unter	Cursor
		ld	(hl), a
		xor	a
		ld	hl, escmod
		or	(hl)
		jr	z, co4
		res	7, c
		dec	(hl)
		jr	z, co1
		dec	hl
		ld	(hl), c
		jp	routch5
co1:		dec	hl
		ld	a, (hl)
		ld	l, c
		cp	' '
		jr	c, co2
		push	af
		ld	a, c
		sub	' '
		and	3Fh ; '?'
		ld	l, a
		pop	af
		sub	20h ; ' '
		and	1Fh
co2:		ld	d, 0
		ld	h, d
		ld	e, a
		ld	b, 6
co3:		sla	e
		rl	d
		djnz	co3
		add	hl, de
		ex	de, hl
		ld	hl, 17FFh
		and	a
		sbc	hl, de
		ld	(cupos), hl
		jr	co5
co4:		ld	hl, (cupos)

; Bildschirm-Steuerzeichen, SCP-kompatibel
; 00h            NOP (keine Wirkung)
; 01h            Cursor links oben (home)
; 07h            akustisches  Zeichen an  Tastatur
; 08h            Cursor zurueck
; 0ah            Linefeed (neue Zeile)
; 0ch            Bildschirm  loeschen (verzoegert zum Lesen  der
;                zuletzt ausgegebenen Bildschirmzeilen), Cursor
;                links oben
; 0dh            Carriage Return (an Zeilenanfang)
; 14h            Rest des Bildschirms loeschen
; 15h            Cursor nach rechts
; 16h            Rest der Zeile loeschen
; 18h            Zeile loeschen, Cursor an Zeiilenanfang
; 1ah            Cursor eine Zeile hoch
; 1bh            Einleitung  Cursorpositionierfolge, die naech-
;                sten beiden Bytes beinhalten Zeile und  Spalte
;                Offset 00h oder 80h
; 7fh            Delete (streichen Zeichen links vom Kursor)

		ld	a, c
		cp	1Bh
		jr	z, oesc
		cp	1
		jr	z, ohome
		cp	7
		jr	z, obell
		cp	0Ah
		jr	z, olf
		cp	0Dh
		jr	z, ocr		; neue Zeile
		cp	15h
		jr	z, oright
		cp	16h
		jp	z, oclrln
		cp	18h
		jp	z, oclln
		cp	1Ah
		jr	z, oup
		cp	7Fh
		jr	z, odel
		cp	14h
		jp	z, clrscr
		cp	0Ch
		jr	z, ocls		; Bildschirm lîschen
		cp	8
		jr	z, oback	; Backspace
		cp	20h ; ' '
		jr	c, co5		; sonstige Steuerzeichen Åbergehen
		jp	co10
;neue Zeile
ocr:		ld	a, l
		or	3Fh
		ld	l, a
		jp	routch3
;Zeichen lˆschen
oback:		inc	hl
		jr	co7
;Bildschirm lˆschen		
ocls:		ld	a, ' '
		ld	hl, 1000h
		ld	(hl), a
		ld	d, h
		ld	e, l
		inc	e
		push	bc
		ld	bc, 7FFh
		ldir
		ld	(cupos), hl
		pop	bc
co5:		jp	routch5
;
oesc:		ld	a, 2
		ld	(escmod), a
		jp	routch5
;		
ohome:		ld	hl, 17FFh
		ld	(cupos), hl
		jp	routch5
;
obell:		ld	c, 0FFh
		in	a, (5)
		res	0, a
		push	af
		out	(5), a
		ld	b, 30h
co6:		djnz	co6
		in	a, (5)
		xor	40h
		out	(5), a
		dec	c
		jr	nz, co6
		pop	af
		set	0, a
		out	(5), a
		jr	routch5
; line feed
olf:		ld	de, 64		
		and	a
		sbc	hl, de
		jr	routch3
;
oright:		dec	hl
		jr	routch3
;
odel:		inc	hl
		ld	a, 18h
		cp	h
		jr	z, routch5
		ld	(hl), ' '
		jr	routch3
;
oup:		ld	de, 64
		add	hl, de
co7:		ld	a, 18h
		cp	h
		jr	z, routch5
		ld	(cupos), hl
		jr	routch5
;
clrscr:		ld	a, 0Fh
co8:		ld	(hl), ' '
		dec	hl
		cp	h
		jp	nz, co8
		jr	routch5
;
oclrln:		ld	a, l
		and	3Fh ; '?'
		ld	b, a
		inc	b
co9:		ld	(hl), ' '
		dec	hl
		djnz	co9
		jr	routch5
;
oclln:		ld	a, l
		or	3Fh ; '?'
		ld	l, a
		ld	(cupos), hl
		ld	b, 40h ; '@'
		jr	co9
; Zeichenausgabe
co10:		ld	(hl), a
		dec	hl
routch3:	ex	de, hl
		ld	hl, 0FFFh
		and	a
		sbc	hl, de
		ex	de, hl
		ld	(cupos), hl
		jr	c, routch5
; 1 Zeile hochscrollen
		ld	hl, 17BFh
		ld	de, 17FFh
		ld	bc, 7C0h
		lddr
		ld	(cupos), de
		ex	de, hl
		inc	hl
routch4:	dec	l
		ld	(hl), ' '
		jr	nz, routch4
routch5:	pop	bc
		pop	de
		ld	hl, (cupos)
		ld	a, (hl)
		ld	(buff),	a	; Zeichen unter	Cursor
		ld	(hl), CURS
		pop	hl
		CPMON
		pop	af
		ld	sp, (SYSSK)
		ei
		ret

;------------------------------------------------------------------------------
; Console Eingabe
;------------------------------------------------------------------------------

conin:		di
		ld	(SYSSK), sp
		ld	sp, load
		CPMOFF
		push	hl
		push	bc
		ld	hl, (cupos)
ci1:		ld	(hl), CURS
ci2:		ld	b, 0
ci3:		push	bc
		ld	b, 0
ci4:		djnz	ci4
		pop	bc
		call	sub_FAC7
		jr	nz, ci5
		djnz	ci3
		ld	a, CURS
		cp	(hl)
		jr	nz, ci1
		ld	a, (buff)	; Zeichen unter	Cursor
		ld	(hl), a
		jr	ci2
ci5:		push	af
		ld	a, (buff)	; Zeichen unter	Cursor
		ld	(hl), a
		in	a, (5)
		res	0, a
		out	(5), a
		call	MS30
		in	a, (5)
		set	0, a
		out	(5), a
ci6:		in	a, (4)
		bit	7, a
		jr	nz, ci6
		ld	a,0FFh
		out	(1Eh), a	; CPMON
		pop	af
		and	7Fh
		;
		cp	41h ; 'A'
		jr	c, loc_FC99
		cp	7Bh ; '{'
		jr	nc, loc_FC99
		cp	5Bh ; '['
		jr	c, loc_FC97
		cp	61h ; 'a'
		jr	c, loc_FC99
loc_FC97:	xor	20h ; ' '
loc_FC99:	;
		pop	bc
		pop	hl
		ld	sp, (SYSSK)
		ei
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

MS30:		push	bc
		ld	bc, 903h
ms301:		dec	bc
		ld	a, b
		or	c
		jr	nz, ms301
		pop	bc
		ret


;------------------------------------------------------------------------------
; LIST
; vermutlich parallel, PIO, Centronics
;------------------------------------------------------------------------------

lstflg:		db 0			; <> 0 wenn Schnittstelle init. ist

list:		push	af
		push	bc
		ld	a, (lstflg)
		or	a		; schon initinialisiert?
		jr	nz, list1	; ja
		; Initialisierung
		ld	a, 0Fh
		out	(26h), a
		ld	a, 0FFh
		out	(22h), a
		ld	a, 0FDh	; '≤'
		out	(22h), a
		ld	(lstflg), a
		;
list1:		in	a, (20h)
		and	1
		jr	nz, list1
		ld	a, 0FFh
		out	(20h), a
		ld	a, c
		out	(24h), a
		xor	a
		out	(20h), a
		dec	a
		out	(20h), a
		pop	bc
		pop	af
		ret

;
lstst:		in	a, (20h)
		and	1
		ret	z
		ld	a, 0FFh
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

cupos:		dw 17FFh
		db    0
escmod:		db 0
buff:		db 0		; Zeichen unter	Cursor
SYSSK:		dw 0		; ZW.Speicher f. Stackpointer

		ds	128
		
;------------------------------------------------------------------------------
; residentes Zusatzkommando CCP
; Programm von Kassette laden, AC1-Format
;------------------------------------------------------------------------------

load:		di
		ld	(sysskl), sp	; Stackspeicher	f. load
		ld	sp, loc_FED1
		push	af
		push	bc
		push	de
		push	hl
		xor	a
		ld	(phako), a
loa3:		call	loa17		; Lesen	ein Bit
		cp	0E6h
		jr	z, loa4
		cp	19h
		jr	nz, loa3
		ld	a, 0FFh
		ld	(phako), a
loa4:		call	loa15		; Lesen	ein Byte A
		cp	55h ; 'U'
		jr	nz, loa3
		ld	b, 10h
		CPMOFF
		ld	hl, (cupos)
loa5:		call	loa15		; Lesen	ein Byte A
		ld	(hl), a
		dec	hl
		djnz	loa5
		dec	hl
		CPMON
		ld	(cupos), hl
		ex	de, hl
		ld	hl, 100h
loa6:		call	loa15		; Lesen	ein Byte A
		cp	'x'
		jr	z, loa11a
		cp	'<'
		jr	nz, loa6
		call	loa15		; Lesen	ein Byte A
		ld	b, a
		push	hl
		call	loa12		; Lesen	2 Byte L,H
		add	a, l
		ld	c, a
		pop	hl
loa7:		call	loa15		; Lesen	ein Byte A
		ld	(hl), a
loa9:		inc	hl
		add	a, c
		ld	c, a
		djnz	loa7
		call	loa15		; Lesen	ein Byte A
		cp	c
		jr	z, loa6
		ld	hl, aChecksumerror ; " Checksumerror\n\r"
; Programmende
loa10:		call	STR
		ld	hl, COMLEN	; 0E007h, CCP.COMLEN
		ld	(hl), 0
		pop	hl
		pop	de
		pop	bc
		pop	af
		ld	sp, (sysskl)	; Stackspeicher	f. load
		ei
		jp	RETCOM		; 0E78Fh, CCP.RETCOM
;		
loa11a:		and	a
		ld	de, 101h
		sbc	hl, de
		ld	a, l
		or	a
		jr	z, loa11b
		inc	h

loa11b:		xor	a
		ld	b, h
		ld	h, a
loa11c:		add	a, 1
		daa
		jr	nc, loa11d
		inc	h
loa11d:		djnz	loa11c
		push	af
		ld	a, h
		ld	hl, a0SectorsLoad ; "  0 Sectors load\n\r"
		ld	(hl), ' '
		or	a
		jr	z, loa11e
		add	a, 30h ; '0'
		ld	(hl), a
loa11e:		inc	hl
		ld	(hl), ' '
		pop	af
		push	af
		rlca
		rlca
		rlca
		rlca
		and	0Fh
		jr	z, loa11f
		or	30h ; '0'
		ld	(hl), a
loa11f:		inc	hl
		pop	af
		and	0Fh
		or	30h ; '0'
		ld	(hl), a
		ld	hl, asc_FC68	; " "
		jr	loa10
; Lesen	2 Byte L,H
loa12:		call	loa15		; Lesen	ein Byte A
		ld	l, a
		call	loa15		; Lesen	ein Byte A
		ld	h, a
		ex	de, hl
		CPMOFF
		bit	1, (hl)
		jr	z, loa13
		ld	(hl), ' '
		jr	loa14
loa13:		ld	(hl), '*'
loa14:		ex	de, hl
		CPMON
		ld	a, h
		ret
; Lesen	ein Byte A
loa15:		push	bc
		ld	b, 8
loa16:		call	loa17		; Lesen	ein Bit
		djnz	loa16
		ld	c, a
		ld	a, (phako)
		xor	c
		pop	bc
		ret
; Lesen	ein Bit
loa17:		push	bc
		push	af
		in	a, (5)
		and	80h
		ld	c, a
loa18:		in	a, (5)
		and	80h
		cp	c
		jr	z, loa18
		ld	c, a
		ld	b, 3Fh
loa19:		djnz	loa19
		pop	af
		rl	c
		rla
		pop	bc
		ret

phako:		db 0
aChecksumerror:	db " Checksumerror",0Ah
		db 0Dh,0
asc_FC68:	db ' '
a0SectorsLoad:	db " 0  Sectors load",0Ah
		db 0Dh,0

		
;------------------------------------------------------------------------------
; residentes Zusatzkommando CCP
;------------------------------------------------------------------------------

exit:		xor	a		; CPMOFF
		out	(1Eh), a
		ld	hl, 1800h
		ld	de, CCP
		ld	bc, 800h
		ldir
		ld	hl, unk_F63E	; wboot1-kopie
		ld	c, 6
		ldir
		rst	0


;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------
sysskl:		dw 0			; ZW.Speicher f. Stackpointer
		
		ds 26h	
loc_FED1:				; Stack f. Load

;		end
