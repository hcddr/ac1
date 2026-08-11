;------------------------------------------------------------------------------
; AC1-Modul-1 (1M-Modul)
; (c) V. Pohlers 2023
;------------------------------------------------------------------------------
; 1MB-Testrom
;------------------------------------------------------------------------------

        cpu 96C141			; Mikrocontroller mit großem Adressbereich
        maxmode on			; der Z80 reicht hier nicht.
	
	listing off

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; 16K-Böcke
size	EQU 4000h

counter EVAL 0
	while $ < 100000h ; 1 MB
here		eval $
		while $<here + size
			dq	counter, here
		endm
counter	EVAL counter+1
	ENDM
	
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	listing on

        end
