;------------------------------------------------------------------------------
; AC1-Modul-1 (1M-Modul)
; (c) V. Pohlers 2023
;------------------------------------------------------------------------------
; letzter Schritt
; Zerlegen und neu Zusammensetzen des ROMs passend für die Speicher des Moduls
;
; Für den JKCEMU müssen die beiden ROMs verschachtelt werden. JKCEMU
; nutzt den klassischen SCCH-Aufbau: 08,09,18,19,28,29,..78,79
; 88 ist wieder 08, hier sind nur 2x8 Bänke möglich
; Der ROM ist 64K-scheibenweise aufgebaut: 
;------------------------------------------------------------------------------


        cpu 96C141			; Mikrocontroller mit großem Adressbereich
        maxmode on			; der Z80 reicht hier nicht.

        org 0000h

size	EQU 8000h
counter EVAL 0

	while counter < 16	; 2x16=32 Bänke
	;while counter < 8	; 2x8 Bänke

		BINCLUDE  "packedRoms.bin", counter*size,size		; aus rom1 x8
		BINCLUDE  "packedRoms.bin", (counter+16)*size,size	; aus rom2 x9

counter	EVAL counter+1

	endm

        end
