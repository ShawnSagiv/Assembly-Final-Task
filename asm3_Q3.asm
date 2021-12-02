;students: 
;	1. Roi Shemesh - 316305416 
;	2. Shawn Sagiv - 204816631

;This program checks if string 'num' in the include file,
;is an add sequence.
;if it is, it breaks num to it's additive elements and print
;them to the string.
;if num is not an additive sequence, it prints "false"

;Register uses:
;the main only uses EDX as to pass arguments to
;Irvine's 'writeString' procedure
;each procedure written in this file, describes its use
;of the registers enlisted to it during the code excecution

INCLUDE Irvine32.inc
INCLUDE asm3_Q3.inc

.data
nameAndID1 byte "Roi Shemesh - 316305416",10,13,0
nameAndID2 byte "Shawn Sagiv - 204816631",10,13,0
seperator byte "---------------------------",10,13,0

strIn byte "Input: ",0
strOut byte "Output: ",0
strFalse byte "false",0


.code
main proc

	mov edx, offset nameAndID1
	call writestring
	mov edx, offset nameAndID2
	call writestring
	mov edx, offset seperator
	call writestring
	call crlf
	
	mov edx, offset strIn
	call writestring
	mov edx, offset num
	call writestring
	call crlf
	
	push offset num
	push N
	push offset res
	call isAddSeq
	
	cmp al, 0
	je printFalse
	
	
	mov edx, offset strOut
	call writestring
	mov edx, offset res
	call writestring
	call crlf
	jmp finMain
	
	printFalse:
		mov edx, offset strOut
		call writestring
		mov edx, offset strFalse
		call writestring
	
	finMain:
		call exitProcess
main endp

;---------------------------------------------
;string addString(string a, string b)
;input: &strA, int sizeA, &strB, int sizeB, &sum
;output: sum.size() in ECX
;this procedure calculates the decimal value of strA + strB and place it in sum
	AddString:

	sumptr = 20
	sizeB = sumptr+ 4
	addrB = sizeB + 4
	sizeA = addrB + 4
	addrA = sizeA + 4
	carry = -4
	counter = carry -4

	push eax
	push ebx
	push edx
	push ebp
	mov ebp, esp
	sub esp, 8
	push esi


	mov esi, sumptr[ebp]
	mov eax,0
	mov edx,0
	mov ebx,0
	mov ecx,0

	;check with size is bigger: 
	mov dword ptr carry[ebp], 0
	mov dword ptr counter[ebp], 1
	mov ebx,[ebp + sizeA]
	mov edx,[ebp + sizeB]
	mov ecx,edx
	cmp edx,ebx
	jge continue
	mov ecx,ebx

	continue:
	add ecx,1
	mov ebx,0
	mov edx,0

	loopy:
	;dl += A[sizeA-counter]
		mov ebx,[ebp + addrA]
		push ebx
		mov ebx,0
		mov ebx,[ebp + sizeA]
		push ebx
		sub ebx,counter[ebp]
		push ebx
		mov ebx,0
		call val
		mov edx,0
		add dl, al

	;dl += B[sizeB-counter]

		mov ebx,[ebp + addrB]
		push ebx
		mov ebx,0
		mov ebx,[ebp + sizeB]
		push ebx
		sub ebx,counter[ebp]
		push ebx
		mov ebx,0
		call val
		add dl, al
		add dl,carry[ebp] ;=[ebp + carry]


	;  div %
		mov eax,0
		mov ebx,0
		mov al, dl
		mov dx,0
		mov bx,10
		div bx
		add dx,'0'
		mov [esi], dl
		inc esi
		mov carry[ebp],ax

	;update counter
		inc dword ptr counter[ebp]
		cmp ecx,counter[ebp]
		je finAddStr
		jmp loopy

	finAddStr:

	cmp dword ptr carry[ebp],0
	je endd
	add dword ptr carry[ebp],'0'
	push eax
	mov eax, carry[ebp]
	mov [esi], al
	;add [ebp+sumptr], eax ;?
	pop eax
	inc ecx


	endd:
	mov ebx, [ebp+sumptr]
	;mov ecx, sizeof ebx
	;dec ecx
	mov esi, [ebp+sumptr]
	push sumptr[ebp]
	push ecx
	call ReverseString

	mov esi, sumptr[ebp]
	add esi, ecx
	dec esi
	mov byte ptr [esi],0

	; return the reverse string with any reg.

	pop esi
	mov esp, ebp
	pop ebp
	pop edx
	pop ebx
	pop eax
	ret 20
	;--------------------------------------------
	;string subString(string a, int sizeA, int pos, int len)
	;input: &strA, intA, int pos, int len, &strRes
	;output: 0 in AL if failed
	;		 1 in AL if success
	;this procedure recursively checks if pos is a legal index in strA,
	;and if pos + len doesn't exceeds strA.size
	;if good, it cuts the substring in strA, starting from index pos,
	;and with length len, and copy it to strRes

	SubString PROC
		resPtr = 8
		len = resPtr + 4
		pos = len + 4
		numLen = pos + 4
		numPtr = numLen + 4
	
		push ebp
		mov ebp, esp
		push ebx 
		push ecx
		push edx
		mov eax, 0				;for invalid ret
		push eax
	
		push [ebp + numPtr]		;offset num
		push [ebp + numLen]		;N
		push [ebp + pos]		;val
		call Val
	
		push [ebp + numPtr]		;offset num
		push [ebp + numLen]		;N
		mov ebx, [ebp + pos]	;pos
		add ebx, [ebp+len]		;pos+len
		push ebx
		call Val

		mov ebx, [ebp + len]
		cmp ebx , 0  			; check len > 0
		jne valid
	
		mov eax, 1
		jmp done
	
	valid:							
	mov ecx, [ebp + numPtr]
		mov edx, [ebp + pos]
		mov cl, [ecx + edx]				; cl = orgStrPtr+pos  
		mov ebx, [ebp +resPtr]	;ebx = destinationPtrPtr
		mov [ebx], cl
	
		
		push [ebp + numPtr]				;push num				
		push [ebp + numLen]				;push N
		mov edx, [ebp + pos] 
		inc edx
		push edx						;push i
		mov edx, [ebp + len] 
		dec edx
		push edx						;push j
		mov edx, [ebp + resPtr]			
		inc edx							; inc destinationPtr++
		push edx						;push curNum
	
		call SubString	
	
	done:
		pop edx
		pop ecx
		pop ebx 
		mov esp, ebp
		pop ebp
	
		ret 20
	SubString ENDP
	;-------------------------------------------
	;int val(string a, int size, int index)
	;input: &strA, int sizeA, int index
	;output: the decimal value of the char in strA[index] in AL
	;this procedure calculates the decimal value of the char in strA[index] if possible
	;else, returns 0
	val proc

	N = 12  ;index
	N1 = 16 ;size
	N2 = 20 ;address

	push ebx
	push ebp
	mov ebp, esp
	mov ebx, [ebp +N]
	cmp ebx, 0
	jl isFalse

	mov eax, [ebp+N1]
	cmp eax, [ebp+N]
	jge true1 ; if size >= index => jump to label "true"

	isFalse:
	mov al,0
	jmp fin


	true1: 
	mov ebx, [ebp+N2]
	add ebx, [ebp+N]
	mov eax,0
	mov al, byte ptr[ebx]
	sub al, '0'


	fin:
	mov esp, ebp
	pop ebp
	pop ebx
	ret 12
	val endp

	;------------------------------------------------
	;boolean isValid(string a, int sizeA)
	;input: &strA, int sizeA
	;output: 0 in AL if the string is illeagl
	;		 1 in AL if the string is ok
	;this procedure checks if strA doesn't start with '0'
	;and if (sizeA>1 or (sizeA==1 and strA[0] != 0)
	;if the above is set true, strA deemed leagal

	isValid proc
		N = 12			;top in stack is the size of the string
		strPtr = N + 4	;after that is the string ptr 
		push esi
		push ebp
	mov ebp, esp
	
	mov eax, N[ebp]
	mov esi, strPtr[ebp]
	
	cmp eax, 1			;check if size<1
	jl isFalse

	cmp byte ptr[esi], 0	;check if size=1 && str[0]==null
	je isFalse

	cmp byte ptr[esi],'0'	;check if str[0]=='0'
	je isFalse
	
	isTrue:
		mov eax,0
		inc eax
		jmp fin

	isFalse:
		mov eax,0
		
	fin:
		mov esp, ebp
		pop ebp
		pop esi
	ret 8
isValid ENDP
;-----------------------------------------

;void ReverseString(string a)
;the function input:
;1.offset
;2.size
;the function reverse the string
ReverseString proc USES EBP EAX EBX ECX

    push ebp
    mov ebp,esp

;ECX=size
xor ecx,ecx
mov cl, [ebp+24]
sub cl,2

;EAX point to begin the string
mov eax, [ebp+28]

;EBX point to end of the string
mov ebx,eax
add ebx,ecx

;ECX=0
xor ecx,ecx

  ;while (eax!=ebx)
  REVERSE:  cmp eax,ebx
  JA FIN
  
  mov cl,[eax]
  push ecx

  mov cl,[ebx]
  mov [eax],cl

  pop ecx
  mov [ebx],cl

  ;EAX++
  add eax,1

  ;EBX--
  sub ebx,1
  
  jmp REVERSE

FIN:

  mov esp,ebp
	pop ebp
	ret 8
ReverseString endp
;---------------------------------------------
;boolean cmpStr(string a, string b)
;the function input:
;1.offset of str1
;2.offset of str2
;the function return
;if str1==str2  AL=1
;if str1!=str2 AL=0
CmpStr proc USES EBP EBX EDX

    push ebp
    mov ebp,esp

	;EAX=offset str1
	mov eax,[ebp+20]

	;EBX=offset str2
	mov ebx,[ebp+24]
    
	       ;check if str1 finCmpStr
   LOOP1:  mov dl,[eax]
	       cmp dl,0
           JE EndOfStr1

           ;check if str2 finCmpStr
           mov dl,[ebx]
           cmp dl,0
           JE FalseCase

          JMP CHECK_IF_EQUAL;str1 and str2 not finCmpStr

              ;check if str1 and str2 finCmpStr
               
   EndOfStr1:  mov dl,[ebx]
               cmp dl,0
               JE TrueCase

                 ;if [eax]!=[ebx] return false
   CHECK_IF_EQUAL:mov dl,[eax]
                  mov dh,[ebx]
                  cmp dl,dh
                  JNE FalseCase
                  inc eax
                  inc ebx
                  jmp LOOP1

FalseCase: xor eax,eax
jmp finCmpStr 

TrueCase:mov eax,1

finCmpStr: mov esp,ebp
	pop ebp
	ret 8
CmpStr endp
;-------------------------------------------------------------
;void pushFront(string dst, string src)
;input: &str1, &str2
;this procedure concats str1 to str2 and saves the result in str1
;NOTICE! str1 should have enough capacity for both str1.size() + str2.size()+1
;THE RESPONSABILITY FOR THIS CHECK IS ON THE USER!

pushFront proc uses esi edi eax ebx ecx

	string2 = 28
	string1 = string2 + 4
	size1 = -4
	size2 = size1 - 4
	
	push ebp
	mov ebp, esp
	sub esp, 8

	mov esi, string1[ebp]
	mov edi, string2[ebp]
	mov byte ptr size1[ebp], 0
	mov byte ptr size2[ebp], 0

	loop1:
		cmp byte ptr[esi], 0
			je loop2
		inc esi
		inc byte ptr size1[ebp]
		jmp loop1

	loop2:
		cmp byte ptr[edi], 0
			je preShiftLoop
		inc edi
		inc byte ptr size2[ebp]
		jmp loop2

	preShiftLoop:
		mov esi, string1[ebp]
		add esi, size1[ebp]		;esi = &str1[size]
		dec esi
		mov ebx, size2[ebp]
		mov ecx, size1[ebp]

	shiftLoop:
		mov al, [esi]
		mov byte ptr [esi + ebx], al
		dec esi
		loop shiftLoop

	mov esi, string1[ebp]
	mov edi, string2[ebp]
	mov ecx, size2[ebp]
	copyLoop:
		mov al, [edi]
		mov [esi], al
		inc esi
		inc edi
		loop copyloop

		mov esi, string1[ebp]
		add esi, size1[ebp]
		add esi, size2[ebp]
		mov byte ptr[esi], 0

		mov esp, ebp
		pop ebp
		ret 8
	pushFront endp
;-------------------------------------------------------
;void pushBack(strDst, strSrc)
;This sub-routine recieves 2 null-terminated strings: 
;strSrc & strDst, and concatenating strSrc to strDst.
;WARNING!
;The responsability is on the user to send strDst which is 
;big enough to hold strSrc as well,
;meanning: strDst.capacity >= strDst.size + strSrc.size + 1

pushBack proc uses esi edi
	
	strSrc = 16
	strDst = strSrc + 4
	
	push ebp
	mov ebp, esp
	
	mov esi, strSrc[ebp]
	mov edi, strDst[ebp]

	startIndexLoop:
		cmp byte ptr [edi] , 0
			je concatLoop
		inc edi
		jmp startIndexLoop

	concatLoop:
		cmp byte ptr[esi], 0
			je finPshBck
		mov al, [esi]
		mov [edi], al
		inc esi
		inc edi
		loop concatLoop
	
	finPshBck:	
	mov esp, ebp
	pop ebp
	ret 8
	pushBack endp

;-------------------------------------------------------
;string chkAddition (list<String> res, string a, string b, string c)
;input: &srcStr, int srcSize, &strRes,int i, int j, int i+j
;output: 0 in AL if srcStr isn't an additive sequence accomulation
;		 1 in AL if it is
;this procedure recursively checks if strA = srcStr.substr(0,i) + strB = srcStr.substr(i,j)
;equals strC = srcStr.substr(i+j), or if its a sub string of strC
;if they are equal, it returns 'true'
;if strA + strB equals to strC.substr(0,addition length), it calls recursively with strB, sumStr(A+B), and strC
;(in both cases above the procedure concats the sum result to resStr)
;else, the procedure returns 'false'

chkAddition proc uses ebx ecx esi
	sumij = 20
	j = sumij+4
	i = j + 4
	resStr = i + 4
	strSize = resStr + 4
	strPtr = strSize + 4
	substrptr = -4
	sumSize = substrptr - 4
	sumptr = sumSize - 4

	push ebp
	mov ebp, esp
	sub esp, 12				;create space for 4 dword local variables
	sub esp, strSize[ebp]	;locally allocating space for sumStr
	mov sumPtr[ebp], esp	;esp = pointer to the 1st byte in sumptr

	mov ecx, strSize[ebp]
	sub ecx, sumij[ebp]		;eax = strsize - (i+j) == c.size
	sub esp, ecx			;locally alocating c.size bytes on the heap for substrptr
	mov substrptr[ebp], esp		;esp = pointer to the 1st byte in a locally allocated c.size string

	mov esi, substrptr[ebp]
	initLoop:					;initalizing the string substrptr with 0's
		mov byte ptr[esi], 0
		inc esi
		loop initLoop
		

	;check if string a is valid
	mov ebx, strPtr[ebp]
	push ebx
	push i[ebp] 
	call isValid
	cmp al, 0
		je retFalse
	
	;check if string b is valid
	add ebx, i[ebp] ;ebx is now pointing to strptr + i
	push ebx
	push j[ebp]
	call isValid
	cmp al,0
		je retFalse
	
	;strsum = addstring(a,b)
	mov esi, strPtr[ebp]	;strA
	push esi		
	push i[ebp]				;sizeof strA
	add esi, i[ebp]			;strB
	push esi		
	push j[ebp]				;sizeof strB
	push sumPtr[ebp]		;sumPtr
	call addstring			;ecx holds sumPtr.size()
	mov sumSize[ebp], ecx
	
	mov esi, strPtr[ebp]
	add esi, sumij[ebp]		;strptr + i + j = c
	
	push sumPtr[ebp]
	push esi
	call cmpStr
	cmp al, 1		;if (sum == c)
		jne con
	push resStr[ebp]	;res.push_back(sum)
	push sumPtr[ebp]     
	call pushBack
	jmp retTrue			;return true
	
	con:
		mov eax, strSize[ebp]
		sub eax, sumij[ebp]		;eax = c.size
		cmp eax, sumSize[ebp]		;if(c.size() <= sum.size())
			jle retFalse			;return false

		mov eax, strSize[ebp]
		sub eax, sumij[ebp]		;strsize - (i + j)
		push esi
		push eax
		push 0
		mov eax, sumSize[ebp]
		dec eax					;numsize - 1 (excluding the null char at the end)
		push eax
		push substrptr[ebp]
		call subString			;substrptr = c.subString(0,sumsize)
		
		push sumPtr[ebp]
		push substrptr[ebp]
		call cmpStr				;compare sumPtr & substrptr
		
		cmp al, 0		;if (sum != c.substr(0, sum.size())
			je retFalse		;return false
		
		;else
		;if the function made it all the way over here,
		;then sum == c.subStr(0,sumSize)

		mov esi, sumptr[ebp]
		add esi, sumSize[ebp]
		dec esi
		mov byte ptr [esi], ' '
		inc esi
		mov byte ptr[esi], 0

		push resStr[ebp]
		push sumPtr[ebp]
		call pushBack			;res.push_back(sum)


		;recursive call
		mov esi, strPtr[ebp]
		add esi, i[ebp]			;esi = strptr + i
		mov eax, strsize[ebp]
		sub eax, i[ebp]			;eax = strsize - i
		push esi
		push eax
		push resStr[ebp]
		push j[ebp]
		mov ecx, sumSize[ebp]	;ecx = sumSize
		dec ecx					;sumSize - 1 (excluding the null char at the end)
		push ecx				
		add ecx, j[ebp]			;ecx = sumSize + j
		push ecx
		call chkAddition
		jmp finChkAdd
	
	retFalse:
		mov al,0
		jmp finChkAdd
	
	retTrue:
		mov al, 1
		
	finChkAdd:
		mov esp, ebp
		pop ebp
		ret 24
chkAddition endp	

;-------------------------------------------------
;boolean isAddSeq(string src,int srcSize, string dst)
;input: &strSrc, int sizeSrc, &strDst
;output: 0 in AL if no Additive sequence permutation was found
;		 1 in AL if found such sequence from strSrc
;this procedure searches for possible additive sequnces startings,
;and sends them to chkAddition
;if the return result is false, it keeps looping
;if the return result is true, it stops the loops and completes the sequnces returned
;in strDst from chkAddition, with beginnings it sent, and return true
;if finished looping over strSrc, and no additive sequence was found, returns 'false'

isAddSeq proc uses ecx esi edi

	strDst = 20
	srcSize = strDst + 4
	strsrc = srcSize + 4
	
	i = -4
	j = i - 4
	subStrA = j - 4
	subStrB = subStrA - 4
	
	push ebp
	mov ebp, esp
	sub esp, 16

	
	mov ecx, srcSize[ebp]
	shr ecx, 1				;ecx = size/2
	sub esp, ecx			;allocating size/2 bytes on the stack for subStrA
	mov subStrA[ebp], esp

	sub esp, ecx			;allocating size/2 bytes on the stack for subStrB
	mov subStrB[ebp], esp

	;initialization loop for substrI & subStrB (filling them with 0's)
	mov esi, subStrA[ebp]
	mov edi, subStrB[ebp]
	initIJLoop:
		mov byte ptr[esi], 0
		mov byte ptr[edi], 0
		inc esi
		inc edi
		loop initIJLoop
	;end of prolouge
	
	mov dword ptr i[ebp], 1		;i = 1
	mov ecx, srcSize[ebp]
	shr ecx, 1						;ecx = size / 2
	
	outloop:			;for (int i = 1; i <= size/2; i++)
		push ecx
		mov dword ptr j[ebp], 1		;j = 1
		mov ecx, srcSize[ebp]
		sub ecx, i[ebp]
		shr ecx, 1					;ecx = (size - i)/2
			
		innerLoop:	;for (int j = 1; j <= (size - i)/2; j++)
			
			push strsrc[ebp]
			push srcSize[ebp]
			push strDst[ebp]
			push i[ebp]
			push j[ebp]
			mov eax, i[ebp]
			add eax, j[ebp]	;eax = i+j
			push eax
			call chkAddition
			cmp al, 1
				je foundAddSeq
			
			inc dword ptr j[ebp]
			loop innerLoop
		
		pop ecx
		inc dword ptr i[ebp]
		loop outloop

	; If code execution reaches here, then string
	;doesn't have any additive sequence
	mov esi, strDst[ebp]
	mov byte ptr [esi], 0		;strDest.clear()
	jmp finAddSeq

	foundAddSeq:
	push strsrc[ebp]	;substrB =  num.substr(i, j)
	push strSize[ebp]
	push i[ebp]
	push j[ebp]
	push subStrB[ebp]
	call substring

	mov esi, subStrB[ebp]
	add esi, j[ebp]				
	mov byte ptr[esi], ' '		;add ' ' at the end of substrB

	push strDst[ebp]
	push substrB[ebp]
	call pushFront				;res.push_front(num.substr(i, j))

	push strsrc[ebp]			;subStrA = num.substr(0,i)
	push strsize[ebp]
	push 0
	push i[ebp]
	push subStrA[ebp]
	call subString

	mov esi, subStrA[ebp]
	add esi, i[ebp]
	mov byte ptr[esi], ' '		;add ' ' at the end of substrA

	push strDst[ebp]
	push substrA[ebp]
	call pushFront				;res.push_front(num.substr(0, i))
		
	finAddSeq:
		
		mov esp, ebp
		pop ebp
		ret 12
isAddSeq endp

;-------------------------------------------------
end main	