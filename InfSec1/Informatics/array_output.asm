.MODEL FLAT, STDCALL
PUBLIC fun7
_DATA SEGMENT
	mas dd 32767, 255, -1, -32767, -1024
	mas1 db 'Memory good', 0
_DATA ENDS
_TEXT SEGMENT
fun7 PROC
	Lea EBX, mas ; комментим чтобы свичить то с чем реботаем
	; Lea EBX, mas1
	mov EAX, EBX
	ret
fun7 ENDP
_TEXT ENDS
END