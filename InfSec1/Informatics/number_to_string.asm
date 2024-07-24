.586
.MODEL FLAT, STDCALL
PUBLIC fun2
_DATA SEGMENT
    StrMas db 12 dup(12), 0
    dec10 dd 10
_DATA ENDS
_TEXT SEGMENT
fun2 PROC par1: DWORD 
    Lea EBX, StrMas 
    mov ECX, 12
    metka1:
       mov BYTE PTR[EBX], ' '
       inc EBX 
       Loop metka1 
       mov EAX, par1
       push EAX
       OR EAX,EAX 
       jns metka2 
       neg EAX 
       metka2: 
         XOR EDX,EDX 
         div dec10 
         add DL,'0'
         dec EBX
         mov BYTE PTR[EBX], DL 
         inc ECX 
         OR EAX, EAX
         jnz metka2
         pop EAX
         OR EAX, EAX 
         jns metka3
         dec EBX
         mov BYTE PTR[EBX], '-'
         metka3:
         mov EAX, EBX
         ret
fun2 ENDP
_TEXT ENDS
END