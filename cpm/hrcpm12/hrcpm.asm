;	*********************************************************
;	*							*
;	* HRDOS/CPM Ralph Hänsel 2011				*
; 	* HRDOS/HRCPM AC1/SCCH					*
; 	* Stand 03.12.11 V1.2					*
;	* reass/adpt V.Pohlers 2023				*
;	*							*
;	*********************************************************

	cpu	z80

vscch	equ	0	; 0 - FA-Mode (AC2010)
			; 1 - SCCH-Mode (floppy-latch, modul1 cpm-Umschaltung anders, rtc)

vZSDOS	equ	0	; 0 - CPM
			; 1 - ZSDOS/MLDOS

	org	0D000h
ccp
	section ccp
	if vZSDOS=0
	include ccp_cpa0.asm	; CPA 24.08.86 + Anpassung AC1-Bios!
	else
	include CCP.asm		; MLDOS-CCP
	endif
	endsection	

	if $ > ccp + 800h
	error "ccp too big"
	endif
	
	org	ccp + 800h
bdos
	section bbos
	if vZSDOS=0
	include bdos_cpa0.asm	; CPA 24.08.86
	else
	include ZSDOS.ASM	; ZSDOS
	endif
	endsection	

	if $ > ccp + 1600h
	error "bdos too big"
	endif

	org	ccp + 1600h
bios
	section bios
	include hrbios12.asm
	endsection

	if vZSDOS = 0
	message "--> HRCPM AC1 V1.2 CPA24.08.86 "
	else
	message "--> HRDOS AC1 V1.2 ZSDOS "
	endif
	if vscch = 0
	message "--> FA-MODE (AC2010)"
	else
	message "--> SCCH-MODE"
	endif

	end
