.586
.MODEL FLAT, STDCALL
PUBLIC fun
_DATA SEGMENT
	mas db 'Memory good',0
_DATA ENDS
_TEXT SEGMENT
fun PROC par1:DWORD,par2:DWORD
	mov EAX,par1
	mov EDX,par2
	ADD EAX,EDX
	Lea EBX,mas
	mov EAX,EBX
	ret
fun ENDP
_TEXT ENDS
END
