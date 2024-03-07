; File Name   :	d:\hobby3\ac1-2010\monitor\komfort-monitor11\MO11A_59.Z80
; Format      :	Binary file
; Base Address:	0000h Range: 18A0h - 18D9h Loaded length: 0039h

		cpu	z80undoc

; reass. V.Pohlers 01.02.2023

; Nachladbare Kommandodatei
; =========================
; 
; Y:	MO11A_59.z80		RAM-Monitor booten
; -----------------------------------------------------------------------------
; Syntax:	Y aaaa
; 
; Kommando lädt 4k großen RAM-Bereich ab aaaa auf Adresse 0000H und bootet neu,
; um z.B. einen alternativen Monitor zu verwenden. Achtung!! RAM wird gelöscht !!
; Wichtig: Das "Durchschreiben" unter den Monitor-ROM muss funktionieren, sonst
; klappt das nicht.
; 
; Die Routine ist verschieblich und wird nach 1900H geladen, kann aber auch
; mit dem Aufruf # U xxxx "MO11A_59.z80" in den Speicher ab xxxx geladen werden.
; 

		include	ac1-2010.asm

;interne Monitor-Funktionen

;Monitor-Speicherzellen
unk_181C	equ	181Ch		; Warmstart-Erkennung

		org 18c0h - 20h
		
; z80-Header
		dw anf
		dw ende
		dw start
		db "MO11.0"
		db  'P'
		db 0D3h,0D3h,0D3h
		db "ext.Komm Y      "

; ---------------------------------------------------------------------------
; Y aaaa
; ---------------------------------------------------------------------------

anf:		db 0,9,'Y',0Dh

start:		ld	hl, (ARG1)	; Kdo-Argument 1
		ld	a, h
		or	l
		ret	z		; wenn kein Arg.
		;
		ld	(unk_181C), hl	; Warmstart verhindern:
					; am Monitor-Anfang steht kein "SCH"
		ld	bc, 1000h	; Länge 4K
		ld	d, c		; de = 0 = ziel
		ld	e, c
;		
		out	(016h), a	; "durchschreiben" an
		ldir			; umladen
yko1:		jp	0		; starten

ende		equ $-1

		end

; wieso Port 16?? und nicht 1E??
