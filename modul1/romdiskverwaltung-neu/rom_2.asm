;------------------------------------------------------------------------------
; AC1-Modul-1 (1M-Modul)
; (c) V. Pohlers 2023
;------------------------------------------------------------------------------
; letzter Schritt
; Zerlegen und neu Zusammensetzen des ROMs passend für die Speicher des Moduls
;
;------------------------------------------------------------------------------

; ROM2, x9

	cpu 96C141			; Mikrocontroller mit großem Adressbereich
	maxmode on			; der Z80 reicht hier nicht.

	org 0000h

size	EQU 8000h*16

	BINCLUDE  "packedRoms.bin", size, size		; rom2 x9

	end
