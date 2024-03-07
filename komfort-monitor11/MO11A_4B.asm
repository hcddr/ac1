; File Name   :	d:\hobby3\ac1-2010\monitor\komfort-monitor11\MO11A_4B.Z80
; Format      :	Binary file
; Base Address:	0000h Range: FEA0h - FF00h Loaded length: 0060h


		cpu	z80undoc

; reass. V.Pohlers 01.02.2023

; Nachladbare Kommandodatei
; =========================
; 
; K:	MO11A_4B.z80		V.24-SAVE (HEADERSAVE-Format)
; ----------------------------------------------------------------------------
; Syntax:	K aaaa bbbb cccc ("name")    SAVE Programm
; 	K aaaa bbbb cccc *("name")   SAVE Daten
; 
; SENDEN eines Files von Adresse aaaa bis bbbb mit Startadresse cccc via V24.
; Der Name kann bis zu 16 Zeichen lang sein und MUSS mit " beginnen!
; Das FILE wird als HEADERSAVE-FILE (also mit 32Byte Vorblock) gesendet. 
; 
; Dabei wird die Einstellung vom V24-Kommandobyte auf fest 4.800Baud
; mit Protokoll: 8N1 ohne Handshake eingestellt.
; Auf diese Parameter ist der "Empfänger-PC" ebenfalls einzustellen.
; Am Ende wird die CRC-Prüfsumme berechnet und ausgeschrieben.
; 
; Die Routine ist verschieblich und wird nach FE00H geladen, kann aber auch
; mit dem Aufruf # U xxxx "MO11A_4B.z80" in den Speicher ab xxxx geladen werden.
; 

		include	ac1-2010.asm

;interne Monitor-Funktionen
crc		equ	0405h		; crc anzeigen, Kommando 'N'
;		equ	080Fh
;		equ	095Bh
q_ko20		equ	0A37h		; Start/Stop-Schaltung start
q_ko21		equ	0A31h		; Start/Stop-Schaltung stopp
v24out		equ	0DF9h		; v24out

;Monitor-Speicherzellen
kdov24		equ	1820h		; v24-Modus
;		equ	1866h		; Länge
;		equ	187Eh		; Speicher Save/Load Turbo-Tape/V24


		org 0FEC0h - 20h
		
; z80-Header		
		dw anf
		dw ende
		dw start
		db "MO11.0"
		db  'P'
		db 0D3h,0D3h,0D3h
		db "ext.Komm K      "

; ---------------------------------------------------------------------------
; Syntax:	K aaaa bbbb cccc ("name")    SAVE Programm
; 		K aaaa bbbb cccc *("name")   SAVE Daten
; ---------------------------------------------------------------------------

anf:		db 0,9,'K',0Dh

start:		ld	c, 'P'		; Dateityp 'P'
		;
		ld	a, (kdov24)	; orig. Wert merken
		push	af
		ld	a, 43h 		; 4800 Baud
		ld	(kdov24), a
		; Header vorbereiten
		call	80Fh
		call	95Bh
		call	q_ko20		; Start/Stop-Schaltung start
		ld	hl, 187Eh	; Speicher Save/Load Turbo-Tape/V24
		; Header ausgeben
		ld	b, 20h 		; 32 Byte Header
kko1:		ld	a, (hl)
		call	v24out
		inc	hl
		djnz	kko1
		; Daten ausgeben
		ld	hl, (ARG1)	; Kdo-Argument 1
		ld	de, (1866h)	; Länge
kko2:		ld	a, (hl)
		call	v24out
		inc	hl
		dec	de
		ld	a, e
		or	d
		jr	nz, kko2
		;
		pop	af
		ld	(kdov24), a	; orig Wert restaurieren
		call	q_ko21		; Start/Stop-Schaltung stopp
		jp	crc		; crc anzeigen, Kommando 'N'

ende:		db 0FFh

		end
