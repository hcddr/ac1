; File Name   :	d:\hobby3\ac1\turbo\AC1-TURBO-LOAD-C_2000_284F_2000.bin
; Base Address:	0000h Range: 2000h - 2850h Loaded length: 0850h

		cpu	z80undoc

		include	ac1-2010.asm

;------------------------------------------------------------------------------
; genutzte interne Systemroutinen und Adressen aus Monitor V8
;------------------------------------------------------------------------------

INIT8		equ 006Eh	; Systemstart
BREAK		equ 0243h
UPTON		equ 0287h
UPTAST		equ 0297h
crc		equ 0404h
q_ko20		equ 0A31h	; Start/Stop-Schaltung stop
q_ko21		equ 0A37h	; Start/Stop-Schaltung start
tbsave		equ 0BF7h	; Kassette Save Turbo
tbsav8		equ 0C33h	; Kassette Save Turbo ab Kopfinhalt ausgeben
tbsav21		equ 0C7Ah	; Save: Flanke wechseln
tbload		equ 0C81h	; Kassette Load Turbo
tb_loa3		equ 0CBDh	; Block laden
tb_loa12	equ 0D06h	; Rest Block lesen
tb_loa24	equ 0D5Ah	; Load: Bit einlesen,Z=0 --> Flanke
tb_loa25	equ 0D52h	; Warten auf Flankenwechsel
tb_loa19	equ 0D64h	; lesen DE
sub_D89:	equ 0D89h	; Anzeige Dateiname, aadr, eadr
loc_DE1		equ 0DE1h	; Rest Fehleranzeige zu Kassette Load Turbo
nmi		equ 1817h	; CALL aktuelle NMI-Routine
phako		equ 1823h	; Hilfsregister (AC1-Format Einlesen, Phasenkorrektur)
tbb_eadr:	equ 187Eh	; Endadr. Block
tbb_aadr:	equ 1880h	; AAdr. Block
tb_errarr:	equ 1882h	; Speicher für 10 Adressen Blockfehler
tb_kopfbuf:	equ 1896h	; 32 Byte Buffer für Turbo Namenskopf
tb_blkbuf:	equ 18B6h	; 32 Byte Kopfblockbuffer beim Lesen
tbl_special:	equ 18D5h	; tb_blkbuf+31		; Kennung

;------------------------------------------------------------------------------
; Kommando 'l'
;------------------------------------------------------------------------------
		org 2000h

		db 0,9,'l',0Dh

		rst	prnst; Ausgabe String
		db 8Ch
;
		in	a, (PIODB)	; Grafik/Ton
		set	3, a
		out	(PIODB), a	; Grafik/Ton
;
		xor	a
		out	(modul1), a	; Modul 1 aus
;
		ld	hl, 1000h	; BWSEND
loc_2012:	call	sub_2639	; window
		jr	nz, loc_2019
		ld	(hl), 0C7h 	; Bildschirm mit Zeichen füllen
loc_2019:	inc	hl
		ld	a, h
		cp	hi(1800h)	; BWSANF erreicht?
		jr	nz, loc_2012
;		
		rst	prnst; Ausgabe String
		; 11h invers an, 10h invers aus, 0Eh setcursor
		db 0Eh,"0512",11h
		db 0Eh,"0552",10h
		db 0Eh,"0612",11h," * AC 1 * T U R B O  LOAD/COPY * V.1 * ",10h
		db 0Eh,"0712",11h
		db 0Eh,"0752",10h
		db 0Eh,"2920",11h,"(C)1988 E.Ludwig, Tel.Halle 41082 "
		db 0Eh,"0912Das Ladeprogramm erm|glicht eine Fehler-"
		db 0Eh,"1012korrektur  beim  Laden von  TURBO TAPE -"
		db 0Eh,"1112Aufzeichnungen sowie ein verschiebliches"
		db 0Eh,"1212Laden in beliebige Adressbereiche."
		db 0Eh,"1312Damit eine universelle Anwendbarkeit ge-"
		db 0Eh,"1412w{hrleistet ist, sind Filetypen P, D u.B"
		db 0Eh,"1512ladbar."
		db 0Eh,"1612Die Fehlerkorrektur ist nur m|glich  bei"
		db 0Eh,"1712Aufzeichnungen des MONITOR V.8 (AC1) V.9"
		db 0Eh,"1812(LLC2) der }berarbeiteten Version 01/88!"
		db 0Eh,"1912Tritt ein Ladefehler auf, wird  gestoppt"
		db 0Eh,"2012und der Computer fragt REPEAT J/N ? ."
		db 0Eh,"2112Mit \"J\" kann das Laden wiederholt werden"
		db 0Eh,"2212(Band St}ck zur}ck).  Mit  Erreichen des"
		db 0Eh,"2312fehlerhaften Block wird das Laden  fort-"
		db 0Eh,"2412gesetzt. Mit \"N\" wird der Block  fehler-"
		db 0Eh,"2512haft geladen! Nach Load entsprechend dem"
		db 0Eh,"2612Men} weiter !      ",11h
		db 0Eh,"2644",90h

		 ; kurze Wartezeit oder Enter drücken
loc_2380:	ld	b, 0A0h
loc_2382:	call	UPTAST
		cp	0Dh		; CR
		jr	z, loc_23B2
		djnz	loc_2382
		;
		inc	c
		bit	1, c
		rst	prnst; Ausgabe String
		db 0Eh,"263",0B3h
		jr	nz, loc_23A4
;		
		rst	prnst; Ausgabe String
		db "START (CR)",0A0h
		jr	loc_2380
;		
loc_23A4:	rst	prnst; Ausgabe String
		db "          ",0A0h
		jr	loc_2380
; Load next
loc_23B2:	ld	bc, 0E040h
		call	UPTON
		ld	hl, 1600h
loc_23BB:	call	sub_2639	; window
		jr	z, loc_23C2
		ld	(hl), ' '
loc_23C2:	dec	hl
		ld	a, h
		cp	hi(1000h)	; BWSEND
loc_23C6:	jr	nz, loc_23BB
		rst	prnst; Ausgabe String
		db 0Eh,"0912Load Adresse (nur CR Original):"
		db 0Eh,"101",0B1h
		;
		call	INLINE		; Zeile eingeben, Adr steht in soil
		ld	hl, (soil)	; Beginn Eingabezeile
		ex	de, hl
		call	INHEX		; konv. ASCII-Hex ab (DE) abwärts nach HL
		ld	(ARG3), hl	; Kdo-Argument 3
		rst	prnst		; Ausgabe String
		db 0Eh,"1212Programmname eingeben :",0A0h
		ld	hl, tb_kopfbuf	; 32 Byte Buffer für Turbo Namenskopf
		ld	b, 8
loc_2422:	ld	(hl), 'N'	; 8x'N' schreiben
		inc	hl
		djnz	loc_2422
		ld	b, 10h		; 16 Zeichen Name
loc_2429:	rst	inch		; Zeicheneingabe
		cp	0Bh		; UP
		jr	z, loc_23B2	; Load next
		cp	0Dh		; CR
		jr	z, loc_2454
		cp	8		; LEFT
		jr	z, loc_2449
		cp	' '
		jr	c, loc_2429
		cp	5Fh 		; rubout
		jr	z, loc_2449
		cp	7Fh 		; rubout
		jr	z, loc_2449
		rst	outch		; Zeichenausgabe
		ld	(hl), a
		inc	hl
		djnz	loc_2429
		jr	loc_2459
;		
loc_2449:	ld	a, b
		cp	10h
		jr	z, loc_2429
		inc	b
		dec	hl
		rst	prnst; Ausgabe String
		db 0DFh
		jr	loc_2429
loc_2454:	ld	(hl), ' '	; Rest des Namens mit Leerzeichen auffüllen
		inc	hl
		djnz	loc_2454
		;
loc_2459:	call	UPTAST
		jr	nz, loc_2459
		rst	prnst; Ausgabe String
		db 0Eh,"1425",10h
		db 0Eh,"1412",91h
loc_246B:	ld	b, 0A0h	; ' '
loc_246D:	call	UPTAST
		cp	0Bh		; UP
		jp	z, loc_23B2	; Load next
		cp	0Dh		; CR
loc_2477:	jr	z, loc_24A2
		djnz	loc_246D
		inc	c
		bit	1, c
		rst	prnst; Ausgabe String
		db 0Eh,"141",0B4h
		jr	z, loc_2494
		rst	prnst; Ausgabe String
		db "START (CR)",0A0h
		jr	loc_246B
loc_2494:	rst	prnst; Ausgabe String
		db "          ",0A0h
		jr	loc_246B
; Turbo-Load		
loc_24A2:	and	a
		ex	af, af'		; '
		call	q_ko21		; Start/Stop-Schaltung start
		ld	hl, loc_283D
		ld	(nmi+1), hl	; aktuelle NMI-Routine
		ld	hl, tb_blkbuf	; 32 Byte Kopfblockbuffer beim Lesen
		ld	(tbb_aadr), hl	; AAdr. Block
		ld	l, lo(tbl_special)
loc_24B5:	ld	(tbb_eadr), hl	; Endadr. Block
		rst	prnst; Ausgabe String
		db 0Eh,"1412Searching for",0A0h
		ld	b, 10h
		ld	l, lo(tb_kopfbuf+8)	; tbh_filename, Dateiname
loc_24D0:	ld	a, (hl)
		rst	outch; Zeichenausgabe
		inc	hl
		djnz	loc_24D0
		;
loc_24D5:	call	tbload		; Kassette Load Turbo
		ld	l, lo(tb_blkbuf)	; 32 Byte Kopfblockbuffer beim Lesen
		ld	de, tb_kopfbuf	; 32 Byte Buffer für Turbo Namenskopf
		ld	b, 8		; 8 x 'N' vergleichen
loc_24DF:	ld	a, (de)
		cp	(hl)
		jr	nz, loc_24D5
		inc	hl
		inc	de
		djnz	loc_24DF
		;		
		ld	b, 10h		; 16 Zeichen Name vergleichen
loc_24E9:	ld	a, (de)
		cp	(hl)
		jr	z, loc_24F1
		cp	' '
		jr	nz, loc_254A
loc_24F1:	inc	hl
		inc	de
		djnz	loc_24E9
		;
		ld	l, lo(tb_blkbuf+26)	; tbl_filetyp
		ld	a, (hl)
		cp	'F'
		jr	z, loc_254A
		inc	hl
		ld	e, lo(tbb_eadr)	; Endadr.
		ld	bc, 4
		ldir
		ld	bc, 0A033h
		call	UPTON
		rst	prnst; Ausgabe String
		db 0Eh,"1612Loading",0A0h
		call	sub_D89		; Anzeige Dateiname, aadr, eadr
		ld	a, (tbl_special)
		cp	'L'		; Kennung 'L' ?
		jr	z, loc_2542
		rst	prnst; Ausgabe String
		db 0Eh,"1712File nicht korrekturf{hig",0A1h
loc_2542:	call	loc_2653
		call	sub_277C	; Fehler anzeigen
		jr	loc_2569
;
loc_254A:	ld	bc, 0A040h
		call	UPTON
		call	sub_277C	; Fehler anzeigen
		jr	nz, loc_24D5
		rst	prnst; Ausgabe String
		db 0Eh,"1612Found  ",0A0h
		call	sub_D89		; Anzeige Dateiname, aadr, eadr
		jp	loc_24D5
;		
loc_2569:	ld	a, 7
		sub	c
		jr	z, loc_2577
		ld	b, a
loc_256F:	rst	prnst; Ausgabe String
		db "    ",0A0h
		djnz	loc_256F
; hier auch Einsprung bei NMI
loc_2577:	call	q_ko20		; Start/Stop-Schaltung stop
		rst	prnst; Ausgabe String
		db 0Eh,"2512C.CRC  K.Save File  L.Load next  Q.Quit"
		db 0Eh,"261",0B2h
loc_25AC:	rst	inch; Zeicheneingabe
		cp	'L'
		jp	z, loc_23B2	; Load next
		cp	'Q'
		jr	z, loc_2617	; Quit
		cp	'C'
		jr	z, loc_25FF	; CRC
		cp	'K'
		jr	nz, loc_25AC	

; Save File
		call	q_ko21		; Start/Stop-Schaltung start
		ld	hl, tb_blkbuf	; 32 Byte Kopfblockbuffer beim Lesen
		ld	(tbb_aadr), hl	;  AAdr. Block
		ld	l, lo(tbl_special)
		ld	(tbb_eadr), hl	; Endadr. Block
		ld	a, 'L'		; Kennung
		ld	(hl), a
		rst	prnst; Ausgabe String
		db 0Eh,"2612Saving ",0A0h
		ld	hl, (ARG1); Kdo-Argument 1
		push	hl
		ld	hl, (ARG2); Kdo-Argument 2
		push	hl
		call	sub_D89		; Anzeige Dateiname, aadr, eadr
		call	tbsave		; Kassette Save Turbo
		pop	hl
		ld	(ARG2), hl; Kdo-Argument 2
		ld	(tbb_eadr), hl	; Endadr. Block
		pop	hl
		ld	(ARG1), hl; Kdo-Argument 1
		ld	(tbb_aadr), hl	; AAdr. Block
		call	sub_2736	; Kassette Save Turbo (lokales tbsave)
		call	q_ko20		; Start/Stop-Schaltung stop

; CRC
loc_25FF:	rst	prnst; Ausgabe String
		db 0Eh,"2712Checksum",0A0h
		call	crc
		rst	prnst; Ausgabe String
		db "  ",0A0h
		jr	loc_25AC	; Save File

; Quit
loc_2617:	rst	prnst; Ausgabe String
		db 0Ch,0Eh,"1627Tsch}ss...",87h
		ld	hl, BREAK	; orig. NMI-Routine
		ld	(nmi+1), hl	; restaurieren
		ld	b, 50h
loc_2631:	call	MS30; 50x30 ms warten
		djnz	loc_2631
		jp	INIT8		; Systemstart

;------------------------------------------------------------------------------
; window
;------------------------------------------------------------------------------

sub_2639:	ld	a, h
		cp	11h
		jr	c, loc_2651
		cp	17h
		jr	nc, loc_2651
		ld	a, l
		and	0C0h ; 'À'
		add	a, 7
		cp	l
		jr	nc, loc_2651
		add	a, 30h ; '0'
		cp	l
		jr	c, loc_2651
		and	a
		ret
		;
loc_2651:	xor	a
		ret

;------------------------------------------------------------------------------
; 
;------------------------------------------------------------------------------

loc_2653:	ld	iy, tb_errarr	; Speicher für 10 Adressen Blockfehler
		ld	de, (ARG3)	; Kdo-Argument 3
		ld	a, e
		or	d
		jr	z, loc_267E
		ld	hl, (tbb_aadr)	; AAdr. Block
		ex	de, hl
		push	hl
		sbc	hl, de
		ld	(ARG3), hl	; Kdo-Argument 3
		ld	hl, (tbb_eadr)	; Endadr. Block
		xor	a
		sbc	hl, de
		pop	de
		add	hl, de
		ld	(tbb_eadr), hl	; Endadr. Block
		ld	(ARG2), hl	; Kdo-Argument 2
		ex	de, hl
		ld	(ARG1), hl	; Kdo-Argument 1
		ld	(tbb_aadr), hl	; AAdr. Block
loc_267E:	ld	a, (tbl_special)	; tb_blkbuf+31		; Kennung
		cp	'L'
		ld	hl, (tbb_aadr)	; AAdr. Block
		rst	prnst		; Ausgabe String
		db 0Eh,"201",0B2h
		jp	nz, loc_2713
loc_268F:	ld	(tbb_aadr), hl	; AAdr. Block
		call	sub_27A6
		jr	z, loc_26EF
		ld	a, '?'
		call	sub_2808	; Scrollen, Zeichenausgabe
loc_269C:	sub	a
		ld	(phako), a	; Hilfsregister
		ld	hl, (cupos)
		push	hl
		call	q_ko20		; Start/Stop-Schaltung stop
		rst	prnst		; Ausgabe String
		db 0Eh,"1812Load Error",0A0h
		ld	hl, (tbb_aadr)	; AAdr. Block
		call	OUTHL		; Ausgabe HL hexadezimal
		rst	prnst		; Ausgabe String
		db "  Repeat J/N ?",0A0h
loc_26CE:	rst	inch		; Zeicheneingabe
		cp	'J'
		jr	z, loc_26E5
		cp	0Dh
		jr	z, loc_26E5
		cp	'N'
		jr	nz, loc_26CE
; kein repeat		
loc_26DB:	call	sub_2796
		ld	bc, 20h		; Blocklänge
		add	hl, bc		; Adr. nächster Block
		ld	(tbb_aadr), hl	; AAdr. Block
; repeat
loc_26E5:	ex	de, hl
		pop	hl
		ld	(cupos), hl
		call	q_ko21		; Start/Stop-Schaltung start
		jr	loc_2708
;		
loc_26EF:	jr	nc, loc_26FF
		call	sub_2808	; Scrollen, Zeichenausgabe
		ld	a, (phako)	; Hilfsregister
		and	a
		jr	nz, loc_269C
		ld	hl, (tbb_aadr)	; AAdr. Block
		jr	loc_268F
;
loc_26FF:	ld	a, '>'
		ld	(phako), a	; Hilfsregister
		ex	de, hl
		call	sub_2808	; Scrollen, Zeichenausgabe
loc_2708:	ld	hl, (tbb_eadr)	; Endadr. Block
		and	a
		sbc	hl, de
		ex	de, hl
		jp	nc, loc_268F
		ret
;
loc_2713:	ld	hl, (tbb_aadr)	; AAdr. Block
loc_2716:	call	tb_loa3		; Block laden
		ld	a, '>'
		jr	z, loc_2728
		ex	de, hl
		ld	hl, -20h
		add	hl, de
		call	sub_2796
		ex	de, hl
		ld	a, '?'
loc_2728:	ex	de, hl
		call	sub_2808	; Scrollen, Zeichenausgabe
		ld	hl, (tbb_eadr)	; Endadr. Block
		and	a
		sbc	hl, de
		ex	de, hl
		jr	nc, loc_2716
		ret

;------------------------------------------------------------------------------
; Kassette Save Turbo
;------------------------------------------------------------------------------

sub_2736:	ld	hl, (tbb_aadr)	; AAdr. Block
		call	sub_274A	; Ausgabe Block langer Vorton
loc_273C:	ex	de, hl
		ld	hl, (tbb_eadr)	; Endadr. Block
		and	a
		sbc	hl, de
		ex	de, hl
		ret	c		; wenn File zu Ende
		call	sub_274F	; Ausgabe ein Block
		jr	loc_273C	; weiter ausgeben

;------------------------------------------------------------------------------
; lokales tbsave mit leicht geänderten Timing gegenüber Monitor
;------------------------------------------------------------------------------

; Ausgabe Block langer Vorton
sub_274A:	ld	de, 2000	; langer Vorton
		jr	loc_2752

; Ausgabe ein Block = 20H Bytes
sub_274F:	ld	de, 14		; kurzer Vorton
loc_2752:	ld	b, 48h
loc_2754:	djnz	loc_2754
		call	tbsav21		; Save: Flanke wechseln
		dec	de
		ld	a, e
		or	d
		jr	nz, loc_2752
		ld	c, 2
loc_2760:	ld	b, 22h
loc_2762:	djnz	loc_2762
		call	tbsav21		; Save: Flanke wechseln
		dec	c
		jr	nz, loc_2760
		push	hl
		ld	de, (ARG3)	; Kdo-Argument 3
		and	a
		sbc	hl, de
		ex	de, hl
		pop	hl
		push	de
		pop	ix
		ld	b, 2
		jp	tbsav8		; weiter im Monitor
					; Kassette Save Turbo ab Kopfinhalt ausgeben

;------------------------------------------------------------------------------
; UP zu Kassette Load Turbo
;------------------------------------------------------------------------------

; Fehler anzeigen
sub_277C:	ld	de, tb_errarr	; Fehler-Array
		ld	a, iyl
		sub	e
		rrca
		ld	c, a
		rst	prnst; Ausgabe String
		db 0Eh,"181",0B2h
		ret	z
		rst	prnst; Ausgabe String
		db "Error",0A0h
		ld	b, a
		jp	loc_DE1		; Weiter mit Code im Monitor
					; Rest Fehleranzeige zu Kassette Load Turbo

;------------------------------------------------------------------------------
; Blockendeadresse in Fehler-Array ablegen
;------------------------------------------------------------------------------

sub_2796:	ld	a, iyl
		sub	lo(tb_errarr+14)	; Ende Fehlerarray
		ret	z
		ld	(iy+0),	l
		ld	(iy+1),	h
		inc	iy		; Zeiger erhöhen
		inc	iy
		ret

;------------------------------------------------------------------------------
; Block laden, lokales tb_loa3
;------------------------------------------------------------------------------

sub_27A6:	call	tb_loa24	; synchronisieren
		call	tb_loa25
		ld	c, 5
loc_27AE:	ld	de, 60Bh	; D=6, E=0Bh
		ld	a, 2
loc_27B3:	dec	a
		jr	nz, loc_27B3
		call	tb_loa24	; synchronisieren
loc_27B9:	call	tb_loa24	; Flanke ?
		jr	nz, sub_27A6
		dec	d
		jr	nz, loc_27B9
		dec	c
		jr	z, loc_27D0
loc_27C4:	in	a, (PIODB)	; Grafik/Ton
		xor	b
		bit	7, a		; TB-Interface-Eingang
		jr	nz, loc_27AE
		dec	e
		jr	nz, loc_27C4
		jr	sub_27A6
; Synchronisierimpulse lesen
loc_27D0:	call	tb_loa25	; Flanke abwarten
		ld	a, 2Ch
loc_27D5:	dec	a
		jr	nz, loc_27D5
		call	tb_loa24	; Flanke ?
		jr	nz, loc_27D0	; wenn nicht
		call	tb_loa25	; Flanke abwarten
		ld	a, 12h
loc_27E2:	dec	a
		jr	nz, loc_27E2
; 2 Bytes Kopf lesen		
		call	tb_loa19	; lesen DE
		push	de
		pop	ix
		push	hl
		ld	hl, (ARG3)	; Kdo-Argument 3
		add	hl, de
		pop	de
		and	a
		sbc	hl, de
		jr	nz, loc_27FE
		ex	de, hl
; 20H Byte Daten lesen
		ld	c, 10h		; 10H x 2 Bytes
		ld	a, 8
		jp	0D06h		; weiter im Monitor
					; Rest Block lesen
		
;
loc_27FE:	ld	d, '-'
		jr	nc, loc_2804
		ld	d, '+'
loc_2804:	xor	a
		ld	a, d
		scf
		ret

;------------------------------------------------------------------------------
; Scrollen im Fenster, Zeichenausgabe
;------------------------------------------------------------------------------

sub_2808:	push	af
		ld	hl, (cupos)
loc_280C:	ld	a, l
		and	0C0h
		add	a, 0Bh
		cp	l
		jr	nc, loc_2819
		add	a, 28h
		cp	l
		jr	nc, loc_281C
loc_2819:	dec	hl
		jr	loc_280C
loc_281C:	ld	a, h
		cp	11h
		call	z, sub_2829
		pop	af
		ld	(hl), a
		dec	hl
		ld	(cupos), hl
		ret
sub_2829:	push	de
		push	hl
		ld	hl, 12BFh
		ld	de, 12FFh
		ld	bc, 100h
		lddr
		pop	hl
		ld	de, 40h
		add	hl, de
		pop	de
		ret
		
;------------------------------------------------------------------------------
; NMI-Routine		
;------------------------------------------------------------------------------

loc_283D:	ld	hl, loc_2577
		push	hl
		reti
		
		end

