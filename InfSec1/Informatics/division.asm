;ДЕЛЕНИЕ, возвращение неполного частного, div1. числитель в 2 раза больше знаменателя. остаток EDX, неполное частное EAX
.586
.MODEL FLAT, STDCALL
PUBLIC fun
_DATA SEGMENT
	dec1 dd 15
_DATA ENDS
_TEXT SEGMENT
fun PROC par1:DWORD,par2:DWORD
	mov EAX,par1
	mov EDX,par2
	ADD EAX,EDX
	Lea EBX,mas
	XOR EDX,EDX
	div dec1
	ret
fun ENDP
_TEXT ENDS
END