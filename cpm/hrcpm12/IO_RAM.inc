;
; RAM-Floppy-Treiber fuer HR-DOS am AC1
;
; Include zu ZBIOS.MAC		(c) HR
; -------------------------------------------------------
;
	TITLE	"ZBIOS - Treiber f}r RAM-Floppy"
	NEWPAGE

IORAM:		ld	c, (hl)		;RWBYTE: 04 = Lesen; 06 = Schreiben
		inc	hl		;DRIVE wird uebersprungen
		inc	hl
		ld	a, (hl)		;TRACK NWT
		ld	(RAFTRK), a
		inc	hl
		ld	a, (hl)		;TRACK HWT
		and	a
		jr	nz, IORDERR	;muss 0 sein
		inc	hl
		ld	a, (hl)		;SEKTOR NWT
		ld	(RAFSEC), a
		bit	7, a
		jr	nz, IORDERR	;Bit 7 muss 0 sein
		inc	hl
		ld	a, (hl)		;SEKTOR HWT
		and	a
		jr	nz, IORDERR	;muss 0 sein
		inc	hl
		inc	hl		;DMA wird uebersprungen
		ld	a, c
		ld	bc, 80h
		cp	6
		jr	z, RDSKWR	;C = 06 --> RDSK Schreiben
;RAM-Disk lesen
		call	ADRRAF
		inir
		nop
		xor	a		;in Ordnung
		ret

;RAM-Disk schreiben
RDSKWR:		ld	hl, 80h
		ld	de, (SEKDMA)
		call	crc
		ld	(word_F4C7), bc	; crc sichern
		call	ADRRAF
		otir			; schreiben
		nop
		;verify
		call	ADRRAF
		inir			; Rücklesen in DMA
		nop
		ld	hl, 80h
		ld	de, (SEKDMA)
		call	crc
		ld	hl, (word_F4C7)	; gesicherte crc
		sbc	hl, bc		; vergleichen
		jr	nz, IORDERR
		xor	a
		ret

;Fehler
IORDERR:	ld	a, 0FFh
		ret

;		
; Format RAM-Floppy
; UP zu BOOT
;
aInit__:	db " =>INIT..",0
aOk:		db "OK",0
aRflError:	db "RFL ERROR!",0

RDFORM:		ld	hl, aInit__	; " =>INIT.."
		call	print		; Textausgabe
		; Generiere Formatierprogramm HPRDSK
		ld	hl, HPRDSK+2	; Hilfsprogramm von 0B000H bis 0B800H
		ld	d, h
		ld	e, l
		dec	hl
		ld	(hl), RAF 	;0B001H = Grundadresse der RAM-Disk
		dec	hl
		ld	(hl), 0D3h 	;0B000H = 0D3H --> OUT n
		ld	bc, 200h
		ldir			;0D3H,0E0H von 0B000H bis 0B1FFH
		inc	hl
		ld	(hl), RAF+1
		dec	hl
		ld	bc, 200h
		ldir			;0D3H,0E1H von 0B200H bis 0B3FFH
		inc	hl
		ld	(hl), RAF+2
		dec	hl
		ld	bc, 200h
		ldir			;0D3H,0E2H von 0B400H bis 0B5FFH
		inc	hl
		ld	(hl), RAF+3
		dec	hl
		ld	bc, 200h
		ldir			;0D3H,0E3H von 0B600H bis 0B7FFH
		ld	(hl), 0C9h 	;RET
;
		xor	a
		out	(RAF+7h), a	;L-Adresse setzen
		out	(RAF+6h), a	;H-Adresse setzen
		out	(RAF+5h), a	;Extended-Adresse
		ld	b, a
		ld	a, 0E5h		;RD wird mit 0E5H formatiert
		ld	c, RAF+6h
loc_EEA0:	call	HPRDSK		;Hilfsprog. Format RAM-Disk auf B000H
		out	(c), b
		djnz	loc_EEA0
;		
		ld	hl, CCP
		ld	(SEKDMA), hl
		ld	b, 2Ch 		;44 Bloecke CCP+BDOS
		call	WRCCP
		jp	nz, RESTR
		ld	hl, aOk		; "OK"
		call	print		; Textausgabe
		ret

;CCP+BDOS in Systemtrack schreiben
WRCCP:		push	bc
		ld	c, 0		;LW A:
		call	SELDSK
		pop	bc
		ld	hl, 0
		ld	(SEKTRK), hl	;Track 0
		ex	de, hl
		ld	hl, (CCPADR)	; Adresse CCP
WRCCP1:		ld	(SEKSEC), de
		ld	(SEKDMA), hl
		push	bc
WRCCP2:		call	WRITE
		pop	bc
		jr	nz, RDERRO
		ld	hl, (SEKDMA)
		ld	de, 80h
		add	hl, de
		ld	de, (SEKSEC)
		inc	de
		djnz	WRCCP1
		xor	a
		ret
;		
RDERRO:		ld	hl, aRflError	; "RFL ERROR!"
		call	print		; Textausgabe
		xor	a
		dec	a
		ret
;		
;Adressarithmetik RAF: L=RADR 0-7 , H=RADR 8-15 , A=RADR 16-19
;
ADRRAF:		xor	a
		ld	l, a
		ld	a, (RAFSEC)	;8 bit
		ld	h, a		;H=Sektor (max. 64 = Bit 0-5)
		rr	h
		rr	l		;H0 -> L7 ...... H5 -> H4
		ld	a, (RAFTRK)	;8 bit , A=Track (max. 128 bei 1M = Bit 0-6)
		rr	a		;A0 -> CY
		jr	nc, ADRAF1	
		set	6, h		;H6 = A0 (RADR13)
ADRAF1:		rr	a		;A1 -> CY
		jr	nc, ADRAF2
		set	7, h		;H7 = A1 (RADR14)
ADRAF2:		push	af
		and	3
		or	RAF
		ld	c, a
		ld	a, l
		out	(RAF+7h), a	;L-Adresse
		ld	a, h
		out	(RAF+6h), a	;H-Adresse
		pop	af
		rr	a
		rr	a
		and	0Fh
		out	(RAF+5h), a	;Extended-Adresse
		ld	b, 80h
		ld	hl, (SEKDMA)
		ret

