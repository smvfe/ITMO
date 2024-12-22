.686
.MODEL FLAT, STDCALL
PUBLIC fun1
_DATA SEGMENT
	mas dd
	key db 1
_DATA ENDS
_TEXT SEGMENT
fun1 PROC
	Lea EBX, mas
	mov ECX, 4
	metka5:
	mov key, 1
	push EBX
	push ECX
	metka1:
		mov EAX, [EBX]
		cmp [EBX + 4], EAX
		jg metka2
		xchg EAX, [EBX+4] ;zamena 3 instrucii, polny obmen tuda suda
		mov key, -1
		mov [EBX], EAX
		metka2:
			add EBX, 4
			loop metka1
			pop ECX
			pop EBX
			dec ECX
			neg key
			js metka6
			jecxz metka6
			jmp metka5
		metka6:
			mov EAX, EBX
			ret
fun1 ENDP
_TEXT ENDS
END