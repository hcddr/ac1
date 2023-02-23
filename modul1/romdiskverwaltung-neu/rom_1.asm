;------------------------------------------------------------------------------
; AC1-Modul-1 (1M-Modul)
; (c) V. Pohlers 2023
;------------------------------------------------------------------------------
; letzter Schritt
; Zerlegen und neu Zusammensetzen des ROMs passend für die Speicher des Moduls
;
;------------------------------------------------------------------------------

; ROM1, x8

	cpu 96C141			; Mikrocontroller mit großem Adressbereich
	maxmode on			; der Z80 reicht hier nicht.

	org 0000h

size	EQU 8000h*16

	BINCLUDE  "packedRoms.bin", 0 ,size		; rom1 x8
	
	end
