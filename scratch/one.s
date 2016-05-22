	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 14, 5
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	push	rax
Ltmp0:
	.cfi_def_cfa_offset 16
	mov	edi, 42
	mov	esi, 1
	call	_add
	mov	edi, eax
	call	_putchar
	pop	rax
	ret
	.cfi_endproc

	.globl	_add
	.align	4, 0x90
_add:                                   ## @add
	.cfi_startproc
## BB#0:
                                        ## kill: ESI<def> ESI<kill> RSI<def>
                                        ## kill: EDI<def> EDI<kill> RDI<def>
	lea	eax, dword ptr [rdi + rsi]
	ret
	.cfi_endproc


.subsections_via_symbols
