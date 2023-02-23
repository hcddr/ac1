; File Name   :	CP_M-V1.6  E000 FCBF F600.BIN
; Format      :	Binary file

;------------------------------------------------------------------------------
; BIOS V1.6 CP/M FA
; basiert auf cbios.asm aus 
; Manfred Kramer, "Praktische Mikrocomputertechnik", Milit‰rverlag DDR,1987
;
; 1 Laufwerk RAM-Floppy, liegt vor CCP im Speicher
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

CURS	equ	0Fh		;Zeichen f. Cursor

CPMON		macro
		ld	a, 1
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
		jp 	list
		jp	punch
		jp	reader
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
		db "CP/M   2.2   V 1.6  auf AC 1"
SizeMsg:	db 1Bh,5,0Ah
		db "RAM - Disk size (1=4K ... 7=28K) ? ",0
rdskmsg:	db 1Bh,8,0Ah
		db "RAM - Disk neu formatieren (Y/n) ? ",0

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

boot:		ld	sp, 0FFFFh
		CPMON
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
		ld	hl, inimsg	; "CP/M   2.2   V 1.6  auf AC 1"
		call	STR

rsiz:		ld	hl, SizeMsg	; "RAM - Disk size (1=4K ... 7=28K) ?"
		call	STR
		call	conin
		ld	c, a
		call	conout
		ld	hl, CCP-16*1024	; RDSK f¸r 16k
		cp	0Dh		; CR=16K A000..DFFF
		jr	z, rsiz2
		cp	'1'		; 1=4K D000...DFFF, 2=8k C000..DFFF ... 7=24k 7000..DFFF
		jr	c, rsiz
		cp	'8'
		jr	nc, rsiz
		and	7
		ld	b, a
		ld	hl, CCP		; RAF Ende+1
		ld	de, 4*1024	; -4K
rsiz1:		sbc	hl, de
		djnz	rsiz1
rsiz2:		ld	(RDSK),	hl	; RAF Anfang
		ld	hl, rdskmsg	; "RAM - Disk neu formatieren (Y/n) ?"
		call	STR
		call	conin
		ld	c, a
		call	conout
		cp	'Y'
		call	z, FRDSK	; Ramdisk formatieren
		call	IRDSK		; init.
		;
		xor	a
		ld	(IOLOC), a	; IOBYTE
		ld	(DISKA), a	; DISKA

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

wboot:		jp	GOCPM

;------------------------------------------------------------------------------
; RAMDISK INITIALISIEREN
;------------------------------------------------------------------------------

IRDSK:	; bios-Startleiste vor RAF kopieren
		ld	hl, (RDSK)	; RAF Anfang
		ld	bc, 80h		; -80h
		sbc	hl, bc
		push	hl		; neue bios-adr. merken
		; bios sprungleiste kopieren
		ld	de, entry
		ex	de, hl
		ld	bc, 21*3-1
		ldir
		;
		pop	hl		; neue bios-Adr
		inc	hl
		inc	hl
		inc	hl		; +3
		ld	(1), hl		; neuer wboot-einsprung
		ld	de, 0E00h-3	; l‰nge bdos
		sbc	hl, de
		ld	(6), hl		; bdos-einsprung
		ld	(hl), 0C3h 	; jp
		ld	de, BDOS + 6
		inc	hl
		ld	(hl), e
		inc	hl
		ld	(hl), d
		;
		ld	hl, CCP		; RAF Ende+1
		ld	de, (RDSK)	; RAF Anfang
		sbc	hl, de
		ld	a, h		; a=RAFgrˆﬂe in 1/4K-Einheiten
		srl	a		; /2
		srl	a		; /2	a=RAFgrˆﬂe in 1K-Einheiten
		dec	a		; -1
		ld	(rafdbp_drm), a	; RAF-Grˆﬂe eintragen
		ret

;------------------------------------------------------------------------------
; RAMDISK FORMATIEREN
;------------------------------------------------------------------------------

FRDSK:		ld	hl, CCP		; RAF Ende + 1
		ld	de, (RDSK)	; RAF Anfang
		sbc	hl, de
		ld	b, h
		ld	c, l
		dec	bc
		ex	de, hl
		ld	d, h
		ld	e, l
		inc	de
		ld	(hl), 0E5h
		ldir
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

GOCPM:		ld	bc, 80h	
		call	SDMA
		ld	a, (4)		;AKTUELLES LAUFWERK HOLEN
		ld	c, a
		jp	CCP

;------------------------------------------------------------------------------
;RAM-DISK
;SCHREIBEN
;------------------------------------------------------------------------------

WRDSK:		call	RADR		;SEKTORADR BERECHNEN
		jr	c, BDERR	;BEREICH UEBERSCHRITTEN
		ex	de, hl		;ZIEL-/QUELLADR. RAM-DISK
		ld	hl, (DMA)	;ZIEL-/QUELLADR. BDOS
RDSK1:		ld	bc, 80h		;=1 SEKTOR
		ldir
		xor	a
		ret

;------------------------------------------------------------------------------
;LESEN
;------------------------------------------------------------------------------

RRDSK:		call	RADR		; Berechnung Adresse
		jr	c, BDERR	;FEHLER
		ld	de, (DMA)
		jr	RDSK1
BDERR:		ld	a, 1
		ret

;------------------------------------------------------------------------------
;SEKTORADR. IM RAM BERECHNEN
;DE AKT. SPUR
;C AKT. SEKTOR
;B MAX. SEKTORANZAHL/DISK
;------------------------------------------------------------------------------

RADR:		ld	d, 0
		ld	a, (TRACK)
		ld	e, a
		ld	a, (SEC1)
		ld	c, a
		ld	hl, 0FFFFh
		ld	b, 26		; Sectors/Track
radr1:		add	hl, de		;UMWANDLUNG SPUREN IN SEKTOREN
		djnz	radr1
		add	hl, bc		;AKT.SEKT.DAZU
		add	hl, hl		;WANDLUNG SEKTOR IN BYTES
		add	hl, hl
		add	hl, hl
		add	hl, hl
		add	hl, hl
		add	hl, hl
		add	hl, hl
		ld	bc, (RDSK)	;RAM-DISK-ANFANG
		add	hl, bc
		ld	a, hi(CCP-1)	;HIGH-ADR RAM-DISK
		cp	h
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
		dw DPBR			;GERABTEBESCHREIBUNG
		dw CHK0			;DIRECTORY-KONTROLLE
		dw ALL0			;BELEGUNG DISKETTE

;------------------------------------------------------------------------------
;DISK-PARAMETER-BLOCK RAM-DISK
;26 Sektoren/Track, BLS 1K
;------------------------------------------------------------------------------

DPBR:		DW	26		;SECTOREN
		DB	3		;BLOCKGROESSE 1K
		DB	7		;(SECTOREN/BLOCK)-1
		DB	00		;NICHT BENUTZT
rafdbp_drm:	DW	1		;(BLOECKE/DISK)-1
		DW	31		;32 DIRECTORY-ENTR.
		DB	080H		;1 BELEGTER BLOCK DURCH DIR.
		DB	0
		DW	4		;DIR/4
		DW	0		;0SPUREN DURCH SYSTEM BELEGT

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

TRACK:		db 0
SEC1:		db 1		;BDOS-SEKTOR
DMA:		dw 80h		;BDOS-DMA
;
DIRBF:		ds 128		;INHALTSVERZ. DISKETTE
ALL0:		ds 31
CHK0:		ds 16

RDSK:		dw 0B000h	; RAF Anfang

;------------------------------------------------------------------------------
; Console Status
;------------------------------------------------------------------------------

const:		in	a, (4)
		and	80h
		ret	z
		ld	a, 0FFh
		and	a
		ret

;------------------------------------------------------------------------------
;AUSGABE EINER ZEICHENFOLGE,ABSCHLUSS DER FOLGE MIT 00
;IN: ANFANGSADR. IN HL
;------------------------------------------------------------------------------

STR:		push	af
		push	bc
str1:		ld	a, (hl)
		cp	0
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
		rst	18h
		db "CPM-BREAK",8Dh
		pop	af
		rst	38h		; durchreichen an Monitor
		;
sub_F905:	in	a, (4)
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
		res	7, c
		ld	hl, escmod
		or	(hl)
		jr	z, co4
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
; 07h            akustisches  Zeichen an  Tastatur  (i.a. nicht
;                vorh.,  dann  Blinken  der Lampen  neben  Stop-
;                Taste bzw. der Statuszeile beim PC1715)
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
		jr	z, clrscr
		cp	0Ch
		jr	z, ocls		; Bildschirm lîschen
		cp	8
		jr	z, oback	; Backspace
		cp	20h ; ' '
		jr	c, co5		; sonstige Steuerzeichen Åbergehen
		jp	co10
ocr:		ld	a, l		; neue Zeile
		or	3Fh ; '?'
		ld	l, a
		jp	routch3
;Zeichen lˆschen
oback:		inc	hl
		jr	co7
; Bildschirm lˆschen		
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
		jr	routch5
;
obell:		ld	c, 0FFh
co61:		ld	b, 90h
co6:		djnz	co6
		in	a, (5)
		xor	40h
		out	(5), a
		dec	c
		jr	nz, co61
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
		call	sub_F905
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
		CPMON
		pop	af
		and	7Fh
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
; 
;------------------------------------------------------------------------------

cupos:		dw 17FFh
		db    0
escmod:		db 0
buff:		db 5Bh		; Zeichen unter	Cursor
SYSSK:		dw 185Dh	; ZW.Speicher f. Stackpointer

		ds	128
		
;------------------------------------------------------------------------------
; residentes Zusatzkommando CCP
; Programm von Kassette laden, AC1-Format
;------------------------------------------------------------------------------

load:		di
		ld	(sysskl), sp	; Stackspeicher	f. load
		ld	sp, exit
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
a0SectorsLoad:	db "  0 sectors load",0Ah
		db 0Dh,0

sysskl:		dw 117Eh		; ZW.Speicher f. Stackpointer


		ds	38		; frei
		
;------------------------------------------------------------------------------
; residentes Zusatzkommando CCP
;------------------------------------------------------------------------------

exit:		pop	hl
		ld	hl, COMLEN	; 0E007h, CCP.COMLEN
		ld	(hl), 0
		CPMOFF
		ld	hl, 103Fh
		ld	(1800h), hl	; cupos
		jp	7FDh		; GETCO1


;------------------------------------------------------------------------------
list:		jp	ms30

;------------------------------------------------------------------------------
punch:		ret

;------------------------------------------------------------------------------
reader:		ret

;------------------------------------------------------------------------------
lstst:		ld	a, 0FFh
		ret

;		end

