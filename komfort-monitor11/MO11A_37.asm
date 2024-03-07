; File Name   :	d:\hobby3\ac1-2010\monitor\komfort-monitor11\MO11A_37.Z80
; Format      :	Binary file
; Base Address:	0000h Range: 18A0h - 18F0h Loaded length: 0050h

; reass. V.Pohlers 01.02.2023

; Nachladbare Kommandodatei
; =========================
; 
; 7:	MO11A_37.z80		ROMBANK AC1-2010 anfen
; -----------------------------------------------------------------------------
; Syntax: 7
; 
; In diesem mit dem Kommando 7 CR nachladbaren Kommando können die Startaufrufe
; (6 7 8 9 0) für die ROMBANK nachgeladen werden und stehen dann zur Verfügung.
; Die Routinen werden auf 18C0H geladen.
; 

		cpu	z80undoc

		org 18C0h - 20h
; z80-Header
		dw anf
		dw ende
		dw 0			; kein Autostart
		db "MO11.0"
		db  'P'
		db 0D3h,0D3h,0D3h
		db "ext.Komm 6 7 8 9"

;
anf:

; ---------------------------------------------------------------------------
; '6' Bank 1 (FDC)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'6',0Dh
	
		ld	a, 10h
		jr	l1

; ---------------------------------------------------------------------------
; '7' Bank 2 (ROM-Bank)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'7',0Dh
	
		ld	a, 20h
		jr	l1

; ---------------------------------------------------------------------------
; '8' Bank 3 (frei)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'8',0Dh
	
		ld	a, 40h
		jr	l1

; ---------------------------------------------------------------------------
; '9' Bank 4 (frei)
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'9',0Dh
	
		ld	a, 80h
		jr	l1

; ---------------------------------------------------------------------------
; '0' ROM disablen
; 8K-Eprom auf PIO2-Karte 
		db 0,9,'0',0Dh
	
		xor	a
l1:		out	(0Fh), a
		ret

		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
		db 0FFh
ende:		db 0FFh

		end
