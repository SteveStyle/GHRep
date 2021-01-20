; AddTwo.asm - adds two 32-bit integers.
; Chapter 3 example
includelib libcmt.lib
includelib libvcruntime.lib
includelib libucrt.lib
includelib legacy_stdio_definitions.lib

.386
.model flat,c
.stack 4096

printf  PROTO arg1:Ptr Byte

.data

msg1     byte    'Hello, World again', 10, 0

varb	db	3
varw	dw	100
vard	dd	5000000

.stack

.code
main proc
    INVOKE printf, ADDR msg1

    mov	eax,9
	add	eax,11			

	ret
main endp
end 