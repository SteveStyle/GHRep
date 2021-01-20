;-----------------------------------
;Descr  : My First 64-bit MASM program
;ml64 prog.asm /c
;golink /console /entry main prog.obj msvcrt.dll, or
;gcc -m64 prog.obj -o prog.exe (main/ret). This needs 64-bit GCC.
;-----------------------------------
includelib libcmt.lib
includelib libvcruntime.lib
includelib libucrt.lib
includelib legacy_stdio_definitions.lib

extrn printf:proc
extrn exit:proc
extrn MessageBoxA: PROC

.data
hello   db 'Hello 64-bit world!',10,'%s',10,'%u',10,0
msg1    db 'Hello',0
var1    dq 5

msg     db 'this is a message',0
caption db 'this is a caption',0


dValue dd 0
bNum db 42
wNum dw 5000
dNum dd 73000
qNum dq 73000000
bAns db 0
wAns dw 0
dAns dd 0
qAns dq 0

.code
main proc

        push    rcx
        push    rdx
        push    r8
        push    r9
        push    r10
        push    r11

        sub     rsp,    28h  
        mov     rcx,    0       ; hWnd = HWND_DESKTOP
        lea     rdx,    msg     ; LPCSTR lpText
        lea     r8,     caption   ; LPCSTR lpCaption
        mov     r9d,    0       ; uType = MB_OK
        call    MessageBoxA
        add     rsp,    28h  
        mov     rcx,    rax     ; uExitCode = MessageBox(...)

        pop     r11
        pop     r10
        pop     r9
        pop     r8
        pop     rdx
        pop     rcx

;        call    exit

        mov     rcx,offset hello
        mov     rdx, offset msg1
        mov     rsi, 5
        mov     rax,0
        sub     rsp,20h
        push 23
        call    printf
        add     rsp,20h

        push 5
        push 3
        call add2
        pop rax
        pop rax

        mov     rcx, 11h

        mov     [dValue],27
        
        mov     al,[bNum]
        mov     [bAns],al

;        mov     [wAns]

        movzx     rcx,[bAns]

main endp


add2 proc

    mov rcx, [rsp+8]
    add rcx, [rsp+16]
    ret

add2 endp

end