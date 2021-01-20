.386
.model flat,stdcall
.stack 4096
ExitProcess proto,dwExitCode:dword

.data
msg	db	'Hello world!$'

.code
public main
main proc
	mov	ah, 09h   ; Display the message
	lea	dx, msg
	int	21h
	mov	ax, 4C00h  ; Terminate the executable
	int	21h

main endp
end main