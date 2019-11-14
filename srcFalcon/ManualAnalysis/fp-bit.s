	.file	"fp-bit.c"
	.text
	.p2align 4,,15
	.def	__fpadd_parts;	.scl	3;	.type	32;	.endef
__fpadd_parts:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	movl	%ecx, %esi
	pushl	%ebx
	movl	%eax, %ebx
	subl	$8, %esp
	movl	(%eax), %ecx
	cmpl	$1, %ecx
	jbe	L1
	movl	(%edx), %edi
	movl	%edx, %eax
	cmpl	$1, %edi
	jbe	L1
	cmpl	$4, %ecx
	je	L34
	cmpl	$4, %edi
	je	L1
	cmpl	$2, %edi
	je	L35
	cmpl	$2, %ecx
	je	L1
	movl	8(%ebx), %eax
	movl	8(%edx), %ebp
	movl	12(%ebx), %edi
	movl	%eax, %ecx
	subl	%ebp, %ecx
	movl	%edi, (%esp)
	movl	12(%edx), %edi
	js	L36
	cmpl	$31, %ecx
	jg	L7
	testl	%ecx, %ecx
	je	L8
	movl	%edi, %ebp
	shrl	%cl, %ebp
	movl	%ebp, 4(%esp)
	movl	$-1, %ebp
	sall	%cl, %ebp
	xorl	%ecx, %ecx
	notl	%ebp
	testl	%edi, %ebp
	movl	4(%esp), %edi
	setne	%cl
	orl	%ecx, %edi
L8:
	movl	4(%ebx), %ecx
	cmpl	4(%edx), %ecx
	je	L9
	movl	(%esp), %ebx
	movl	%edi, %edx
	subl	%ebx, %edx
	subl	%edi, %ebx
	testl	%ecx, %ecx
	movl	%ebx, %edi
	cmovne	%edx, %edi
	testl	%edi, %edi
	js	L12
	movl	$0, 4(%esi)
	movl	%eax, 8(%esi)
	movl	%edi, 12(%esi)
L13:
	leal	-1(%edi), %eax
	cmpl	$1073741822, %eax
	ja	L14
	movl	8(%esi), %eax
	subl	$1, %eax
	.p2align 4,,10
L15:
	addl	%edi, %edi
	movl	%eax, %ecx
	subl	$1, %eax
	leal	-1(%edi), %edx
	cmpl	$1073741822, %edx
	jbe	L15
	movl	%edi, 12(%esi)
	movl	%ecx, 8(%esi)
L14:
	movl	$3, (%esi)
L32:
	movl	%esi, %eax
L1:
	addl	$8, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L34:
	movl	%ebx, %eax
	cmpl	$4, %edi
	jne	L1
	movl	4(%edx), %esi
	movl	$___thenan_sf, %edx
	cmpl	%esi, 4(%ebx)
	cmovne	%edx, %eax
	jmp	L1
	.p2align 4,,10
L35:
	movl	%ebx, %eax
	cmpl	$2, %ecx
	jne	L1
	movl	$2, (%esi)
	movl	4(%ebx), %ecx
	movl	%ecx, 4(%esi)
	movl	8(%ebx), %ecx
	movl	4(%ebx), %eax
	andl	4(%edx), %eax
	movl	%ecx, 8(%esi)
	movl	12(%ebx), %ecx
	movl	%eax, 4(%esi)
	movl	%esi, %eax
	movl	%ecx, 12(%esi)
	jmp	L1
	.p2align 4,,10
L36:
	movl	%ebp, %ecx
	subl	%eax, %ecx
	cmpl	$31, %ecx
	jle	L37
L7:
	cmpl	%ebp, %eax
	jle	L38
	xorl	%edi, %edi
	jmp	L8
	.p2align 4,,10
L9:
	addl	(%esp), %edi
	movl	%ecx, 4(%esi)
	movl	%eax, 8(%esi)
	movl	$3, (%esi)
	movl	%edi, 12(%esi)
	jns	L32
	addl	$1, 8(%esi)
	movl	%edi, %eax
	shrl	%edi
	andl	$1, %eax
	orl	%eax, %edi
	movl	%esi, %eax
	movl	%edi, 12(%esi)
	jmp	L1
	.p2align 4,,10
L38:
	movl	%ebp, %eax
	movl	$0, (%esp)
	jmp	L8
	.p2align 4,,10
L37:
	movl	(%esp), %eax
	shrl	%cl, %eax
	movl	%eax, 4(%esp)
	movl	$-1, %eax
	sall	%cl, %eax
	notl	%eax
	testl	%eax, (%esp)
	setne	%al
	movzbl	%al, %eax
	orl	4(%esp), %eax
	movl	%eax, (%esp)
	movl	%ebp, %eax
	jmp	L8
	.p2align 4,,10
L12:
	negl	%edi
	movl	$1, 4(%esi)
	movl	%eax, 8(%esi)
	movl	%edi, 12(%esi)
	jmp	L13
	.p2align 4,,15
	.globl	___pack_f
	.def	___pack_f;	.scl	2;	.type	32;	.endef
___pack_f:
	pushl	%esi
	pushl	%ebx
	subl	$4, %esp
	movl	16(%esp), %edx
	movl	(%edx), %ecx
	movl	12(%edx), %eax
	movl	4(%edx), %ebx
	cmpl	$1, %ecx
	jbe	L59
	cmpl	$4, %ecx
	je	L54
	testl	%eax, %eax
	je	L52
	cmpl	$2, %ecx
	jne	L60
L52:
	xorl	%edx, %edx
	xorl	%eax, %eax
	jmp	L41
	.p2align 4,,10
L54:
	movl	$-1, %edx
	xorl	%eax, %eax
L41:
	movzbl	%dl, %edx
	andl	$8388607, %eax
	sall	$31, %ebx
	sall	$23, %edx
	orl	%edx, %eax
	orl	%ebx, %eax
	movl	%eax, (%esp)
	flds	(%esp)
	addl	$4, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L59:
	orl	$1048576, %eax
	movl	$-1, %edx
	andl	$8388607, %eax
	jmp	L41
	.p2align 4,,10
L60:
	movl	8(%edx), %edx
	cmpl	$-126, %edx
	jl	L61
	cmpl	$127, %edx
	jg	L54
	movl	%eax, %ecx
	andl	$127, %ecx
	cmpl	$64, %ecx
	je	L62
	addl	$63, %eax
L48:
	testl	%eax, %eax
	js	L49
	addl	$127, %edx
L50:
	shrl	$7, %eax
	andl	$8388607, %eax
	jmp	L41
	.p2align 4,,10
L49:
	shrl	%eax
	subl	$-128, %edx
	jmp	L50
	.p2align 4,,10
L62:
	leal	64(%eax), %ecx
	testb	$-128, %al
	cmovne	%ecx, %eax
	jmp	L48
	.p2align 4,,10
L61:
	movl	$-126, %ecx
	subl	%edx, %ecx
	cmpl	$25, %ecx
	jg	L52
	movl	$-1, %esi
	movl	%eax, %edx
	sall	%cl, %esi
	shrl	%cl, %edx
	notl	%esi
	testl	%eax, %esi
	setne	%al
	movzbl	%al, %eax
	orl	%eax, %edx
	movl	%edx, %eax
	andl	$127, %eax
	cmpl	$64, %eax
	je	L44
	addl	$63, %edx
L58:
	movl	%edx, %eax
	shrl	$7, %eax
	andl	$8388607, %eax
	cmpl	$1073741823, %edx
	seta	%dl
	jmp	L41
L44:
	testb	$-128, %dl
	je	L58
	addl	$64, %edx
	jmp	L58
	.p2align 4,,15
	.globl	___unpack_f
	.def	___unpack_f;	.scl	2;	.type	32;	.endef
___unpack_f:
	pushl	%ebx
	movl	8(%esp), %ebx
	movl	12(%esp), %ecx
	movl	(%ebx), %edx
	movzwl	2(%ebx), %eax
	movzbl	3(%ebx), %ebx
	andl	$8388607, %edx
	shrw	$7, %ax
	shrb	$7, %bl
	andl	$255, %eax
	movzbl	%bl, %ebx
	movl	%ebx, 4(%ecx)
	jne	L64
	testl	%edx, %edx
	je	L72
	movl	%edx, %eax
	movl	$3, (%ecx)
	movl	$-127, %edx
	sall	$7, %eax
	.p2align 4,,10
L67:
	addl	%eax, %eax
	movl	%edx, %ebx
	subl	$1, %edx
	cmpl	$1073741823, %eax
	jbe	L67
	movl	%ebx, 8(%ecx)
	movl	%eax, 12(%ecx)
	popl	%ebx
	ret
	.p2align 4,,10
L64:
	cmpl	$255, %eax
	je	L73
	subl	$127, %eax
	movl	$3, (%ecx)
	movl	%eax, 8(%ecx)
	movl	%edx, %eax
	sall	$7, %eax
	orl	$1073741824, %eax
	movl	%eax, 12(%ecx)
	popl	%ebx
	ret
	.p2align 4,,10
L72:
	movl	$2, (%ecx)
	popl	%ebx
	ret
	.p2align 4,,10
L73:
	testl	%edx, %edx
	jne	L69
	movl	$4, (%ecx)
	popl	%ebx
	ret
	.p2align 4,,10
L69:
	movl	%edx, %eax
	movl	%edx, 12(%ecx)
	shrl	$20, %eax
	andl	$1, %eax
	movl	%eax, (%ecx)
	popl	%ebx
	ret
	.p2align 4,,15
	.globl	___addsf3
	.def	___addsf3;	.scl	2;	.type	32;	.endef
___addsf3:
	pushl	%esi
	pushl	%ebx
	subl	$72, %esp
	leal	24(%esp), %ebx
	flds	84(%esp)
	leal	16(%esp), %eax
	fstps	16(%esp)
	flds	88(%esp)
	movl	%ebx, 4(%esp)
	leal	40(%esp), %esi
	movl	%eax, (%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	20(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_f
	leal	56(%esp), %ecx
	movl	%esi, %edx
	movl	%ebx, %eax
	call	__fpadd_parts
	movl	%eax, (%esp)
	call	___pack_f
	addl	$72, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___subsf3
	.def	___subsf3;	.scl	2;	.type	32;	.endef
___subsf3:
	pushl	%esi
	pushl	%ebx
	subl	$72, %esp
	leal	24(%esp), %ebx
	flds	84(%esp)
	leal	16(%esp), %eax
	fstps	16(%esp)
	flds	88(%esp)
	movl	%ebx, 4(%esp)
	leal	40(%esp), %esi
	movl	%eax, (%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	20(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_f
	leal	56(%esp), %ecx
	movl	%esi, %edx
	movl	%ebx, %eax
	xorl	$1, 44(%esp)
	call	__fpadd_parts
	movl	%eax, (%esp)
	call	___pack_f
	addl	$72, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___mulsf3
	.def	___mulsf3;	.scl	2;	.type	32;	.endef
___mulsf3:
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$72, %esp
	leal	16(%esp), %eax
	flds	88(%esp)
	leal	24(%esp), %ebx
	fstps	16(%esp)
	flds	92(%esp)
	movl	%eax, (%esp)
	leal	40(%esp), %esi
	movl	%ebx, 4(%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	20(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_f
	movl	24(%esp), %eax
	cmpl	$1, %eax
	ja	L79
L83:
	movl	44(%esp), %eax
	movl	%ebx, %edx
	cmpl	%eax, 28(%esp)
	setne	%al
	movzbl	%al, %eax
	movl	%eax, 28(%esp)
L80:
	movl	%edx, (%esp)
	call	___pack_f
	addl	$72, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L79:
	movl	40(%esp), %edi
	cmpl	$1, %edi
	jbe	L85
	cmpl	$4, %eax
	jne	L82
	movl	$___thenan_sf, %edx
	cmpl	$2, %edi
	je	L80
	jmp	L83
	.p2align 4,,10
L82:
	cmpl	$4, %edi
	jne	L84
	movl	$___thenan_sf, %edx
	cmpl	$2, %eax
	je	L80
L85:
	movl	44(%esp), %eax
	movl	%esi, %edx
	cmpl	%eax, 28(%esp)
	movl	%edx, (%esp)
	setne	%al
	movzbl	%al, %eax
	movl	%eax, 44(%esp)
	call	___pack_f
	addl	$72, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L84:
	movl	28(%esp), %ecx
	cmpl	%ecx, 44(%esp)
	setne	%cl
	movzbl	%cl, %ecx
	cmpl	$2, %eax
	je	L107
	cmpl	$2, %edi
	je	L108
	movl	48(%esp), %ebx
	movl	%ecx, 60(%esp)
	addl	32(%esp), %ebx
	movl	52(%esp), %eax
	mull	36(%esp)
	leal	2(%ebx), %esi
	testl	%edx, %edx
	movl	%edx, %edi
	movl	%esi, 64(%esp)
	jns	L88
	leal	3(%ebx), %ecx
	movl	%ecx, 64(%esp)
	movl	%eax, %ecx
	shrl	%ecx
	orl	$-2147483648, %ecx
	testb	$1, %dl
	cmovne	%ecx, %eax
	shrl	%edi
L90:
	movl	%edi, %ecx
	andl	$127, %ecx
	cmpl	$64, %ecx
	je	L109
L93:
	movl	%edi, 68(%esp)
	leal	56(%esp), %edx
	movl	$3, 56(%esp)
	jmp	L80
L88:
	cmpl	$1073741823, %edx
	ja	L90
	movl	%esi, %ecx
	.p2align 4,,10
L92:
	addl	%edi, %edi
	subl	$1, %ecx
	movl	%edi, %ebx
	orl	$1, %ebx
	testl	%eax, %eax
	cmovs	%ebx, %edi
	addl	%eax, %eax
	cmpl	$1073741823, %edi
	jbe	L92
	movl	%ecx, 64(%esp)
	jmp	L90
	.p2align 4,,10
L107:
	movl	%ecx, 28(%esp)
	movl	%ebx, %edx
	jmp	L80
L108:
	movl	%ecx, 44(%esp)
	movl	%esi, %edx
	jmp	L80
L109:
	testl	$128, %edi
	jne	L93
	testl	%eax, %eax
	leal	64(%edi), %eax
	setne	%cl
	andl	$-128, %eax
	testb	%cl, %cl
	cmovne	%eax, %edi
	jmp	L93
	.p2align 4,,15
	.globl	___divsf3
	.def	___divsf3;	.scl	2;	.type	32;	.endef
___divsf3:
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$56, %esp
	leal	16(%esp), %eax
	flds	72(%esp)
	leal	24(%esp), %esi
	fstps	16(%esp)
	flds	76(%esp)
	movl	%eax, (%esp)
	leal	40(%esp), %ebx
	movl	%esi, 4(%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	20(%esp), %eax
	movl	%ebx, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_f
	movl	24(%esp), %eax
	cmpl	$1, %eax
	ja	L111
L130:
	movl	%esi, %ebx
L112:
	movl	%ebx, (%esp)
	call	___pack_f
	addl	$56, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L111:
	movl	40(%esp), %edx
	cmpl	$1, %edx
	jbe	L112
	movl	44(%esp), %ecx
	xorl	%ecx, 28(%esp)
	leal	-2(%eax), %ecx
	andl	$-3, %ecx
	jne	L113
	movl	$___thenan_sf, %ebx
	cmpl	%edx, %eax
	jne	L130
	jmp	L112
	.p2align 4,,10
L113:
	cmpl	$4, %edx
	je	L131
	cmpl	$2, %edx
	je	L132
	movl	32(%esp), %edx
	movl	36(%esp), %eax
	subl	48(%esp), %edx
	movl	52(%esp), %ebx
	movl	%edx, 32(%esp)
	cmpl	%ebx, %eax
	jnb	L117
	subl	$1, %edx
	addl	%eax, %eax
	movl	%edx, 32(%esp)
L117:
	movl	$31, %edx
	xorl	%edi, %edi
	movl	$1073741824, %ecx
	.p2align 4,,10
L119:
	cmpl	%eax, %ebx
	ja	L118
	orl	%ecx, %edi
	subl	%ebx, %eax
L118:
	shrl	%ecx
	addl	%eax, %eax
	subl	$1, %edx
	jne	L119
	movl	%edi, %edx
	andl	$127, %edx
	cmpl	$64, %edx
	je	L133
L120:
	movl	%edi, 36(%esp)
	jmp	L130
	.p2align 4,,10
L131:
	movl	$0, 36(%esp)
	movl	%esi, %ebx
	movl	$0, 32(%esp)
	jmp	L112
L132:
	movl	$4, 24(%esp)
	movl	%esi, %ebx
	jmp	L112
L133:
	testl	$128, %edi
	jne	L120
	testl	%eax, %eax
	leal	64(%edi), %eax
	setne	%dl
	andl	$-128, %eax
	testb	%dl, %dl
	cmovne	%eax, %edi
	jmp	L120
	.p2align 4,,15
	.globl	___fpcmp_parts_f
	.def	___fpcmp_parts_f;	.scl	2;	.type	32;	.endef
___fpcmp_parts_f:
	pushl	%esi
	movl	$1, %eax
	pushl	%ebx
	movl	12(%esp), %ebx
	movl	16(%esp), %esi
	movl	(%ebx), %edx
	cmpl	$1, %edx
	jbe	L134
	movl	(%esi), %ecx
	cmpl	$1, %ecx
	jbe	L134
	cmpl	$4, %edx
	jne	L136
	cmpl	$4, %ecx
	je	L174
L136:
	cmpl	$4, %edx
	je	L175
	cmpl	$4, %ecx
	je	L140
	cmpl	$2, %edx
	jne	L158
	cmpl	$2, %ecx
	je	L150
L158:
	cmpl	$2, %edx
	je	L140
	movl	4(%ebx), %edx
	cmpl	$2, %ecx
	je	L173
	cmpl	%edx, 4(%esi)
	jne	L173
	movl	8(%esi), %eax
	cmpl	%eax, 8(%ebx)
	jg	L173
	jl	L172
	movl	12(%esi), %eax
	cmpl	%eax, 12(%ebx)
	ja	L173
	movl	$0, %eax
	jnb	L134
L172:
	cmpl	$1, %edx
	sbbl	%eax, %eax
	orl	$1, %eax
L134:
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L175:
	cmpl	$1, 4(%ebx)
	popl	%ebx
	popl	%esi
	sbbl	%eax, %eax
	andl	$2, %eax
	subl	$1, %eax
	ret
	.p2align 4,,10
L173:
	cmpl	$1, %edx
	popl	%ebx
	sbbl	%eax, %eax
	popl	%esi
	andl	$2, %eax
	subl	$1, %eax
	ret
	.p2align 4,,10
L140:
	cmpl	$1, 4(%esi)
	popl	%ebx
	popl	%esi
	sbbl	%eax, %eax
	orl	$1, %eax
	ret
	.p2align 4,,10
L174:
	movl	4(%esi), %eax
	subl	4(%ebx), %eax
	jmp	L134
	.p2align 4,,10
L150:
	xorl	%eax, %eax
	jmp	L134
	.p2align 4,,15
	.globl	___cmpsf2
	.def	___cmpsf2;	.scl	2;	.type	32;	.endef
___cmpsf2:
	pushl	%esi
	pushl	%ebx
	subl	$56, %esp
	leal	24(%esp), %ebx
	flds	68(%esp)
	leal	16(%esp), %eax
	fstps	16(%esp)
	flds	72(%esp)
	movl	%ebx, 4(%esp)
	leal	40(%esp), %esi
	movl	%eax, (%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	20(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_f
	movl	%esi, 4(%esp)
	movl	%ebx, (%esp)
	call	___fpcmp_parts_f
	addl	$56, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___eqsf2
	.def	___eqsf2;	.scl	2;	.type	32;	.endef
___eqsf2:
	pushl	%esi
	pushl	%ebx
	subl	$56, %esp
	leal	16(%esp), %eax
	flds	68(%esp)
	leal	24(%esp), %ebx
	fstps	16(%esp)
	flds	72(%esp)
	movl	%eax, (%esp)
	leal	40(%esp), %esi
	movl	%ebx, 4(%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	20(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_f
	cmpl	$1, 24(%esp)
	movl	$1, %eax
	jbe	L178
	cmpl	$1, 40(%esp)
	jbe	L178
	movl	%esi, 4(%esp)
	movl	%ebx, (%esp)
	call	___fpcmp_parts_f
L178:
	addl	$56, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___nesf2
	.def	___nesf2;	.scl	2;	.type	32;	.endef
___nesf2:
	jmp	___eqsf2
	.p2align 4,,15
	.globl	___gtsf2
	.def	___gtsf2;	.scl	2;	.type	32;	.endef
___gtsf2:
	pushl	%esi
	pushl	%ebx
	subl	$56, %esp
	leal	16(%esp), %eax
	flds	68(%esp)
	leal	24(%esp), %ebx
	fstps	16(%esp)
	flds	72(%esp)
	movl	%eax, (%esp)
	leal	40(%esp), %esi
	movl	%ebx, 4(%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	20(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_f
	cmpl	$1, 24(%esp)
	jbe	L187
	cmpl	$1, 40(%esp)
	jbe	L187
	movl	%esi, 4(%esp)
	movl	%ebx, (%esp)
	call	___fpcmp_parts_f
L184:
	addl	$56, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L187:
	movl	$-1, %eax
	jmp	L184
	.p2align 4,,15
	.globl	___gesf2
	.def	___gesf2;	.scl	2;	.type	32;	.endef
___gesf2:
	jmp	___gtsf2
	.p2align 4,,15
	.globl	___ltsf2
	.def	___ltsf2;	.scl	2;	.type	32;	.endef
___ltsf2:
	jmp	___eqsf2
	.p2align 4,,15
	.globl	___lesf2
	.def	___lesf2;	.scl	2;	.type	32;	.endef
___lesf2:
	jmp	___eqsf2
	.p2align 4,,15
	.globl	___unordsf2
	.def	___unordsf2;	.scl	2;	.type	32;	.endef
___unordsf2:
	subl	$56, %esp
	leal	24(%esp), %eax
	flds	60(%esp)
	movl	%eax, 4(%esp)
	leal	16(%esp), %eax
	fstps	16(%esp)
	flds	64(%esp)
	movl	%eax, (%esp)
	fstps	20(%esp)
	call	___unpack_f
	leal	40(%esp), %eax
	movl	%eax, 4(%esp)
	leal	20(%esp), %eax
	movl	%eax, (%esp)
	call	___unpack_f
	cmpl	$1, 24(%esp)
	movl	$1, %eax
	jbe	L192
	xorl	%eax, %eax
	cmpl	$1, 40(%esp)
	setbe	%al
L192:
	addl	$56, %esp
	ret
	.p2align 4,,15
	.globl	___floatsisf
	.def	___floatsisf;	.scl	2;	.type	32;	.endef
___floatsisf:
	subl	$20, %esp
	movl	24(%esp), %eax
	movl	$3, 4(%esp)
	movl	%eax, %edx
	shrl	$31, %edx
	movl	%edx, 8(%esp)
	testl	%eax, %eax
	jne	L198
	movl	$2, 4(%esp)
L199:
	leal	4(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_f
L197:
	addl	$20, %esp
	ret
	.p2align 4,,10
L198:
	movl	$30, 12(%esp)
	jns	L202
	flds	LC1
	cmpl	$-2147483648, %eax
	je	L197
	fstp	%st(0)
	negl	%eax
L202:
	bsrl	%eax, %ecx
	xorl	$31, %ecx
	subl	$1, %ecx
	je	L207
	sall	%cl, %eax
	movl	%eax, 16(%esp)
	movl	$30, %eax
	subl	%ecx, %eax
	movl	%eax, 12(%esp)
	jmp	L199
	.p2align 4,,10
L207:
	movl	%eax, 16(%esp)
	jmp	L199
	.p2align 4,,15
	.globl	___floatunsisf
	.def	___floatunsisf;	.scl	2;	.type	32;	.endef
___floatunsisf:
	subl	$20, %esp
	movl	24(%esp), %eax
	movl	$0, 8(%esp)
	testl	%eax, %eax
	jne	L209
	leal	4(%esp), %eax
	movl	$2, 4(%esp)
	movl	%eax, (%esp)
	call	___pack_f
	addl	$20, %esp
	ret
	.p2align 4,,10
L209:
	bsrl	%eax, %ecx
	movl	$3, 4(%esp)
	xorl	$31, %ecx
	movl	$30, 12(%esp)
	subl	$1, %ecx
	cmpl	$-1, %ecx
	je	L214
	testl	%ecx, %ecx
	je	L215
	sall	%cl, %eax
	movl	%eax, 16(%esp)
	movl	$30, %eax
	subl	%ecx, %eax
	movl	%eax, 12(%esp)
	leal	4(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_f
	addl	$20, %esp
	ret
	.p2align 4,,10
L214:
	movl	%eax, %edx
	andl	$1, %eax
	movl	$31, 12(%esp)
	shrl	%edx
	orl	%edx, %eax
	movl	%eax, 16(%esp)
	leal	4(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_f
	addl	$20, %esp
	ret
	.p2align 4,,10
L215:
	movl	%eax, 16(%esp)
	leal	4(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_f
	addl	$20, %esp
	ret
	.p2align 4,,15
	.globl	___fixsfsi
	.def	___fixsfsi;	.scl	2;	.type	32;	.endef
___fixsfsi:
	pushl	%ebx
	subl	$40, %esp
	leal	24(%esp), %eax
	flds	48(%esp)
	movl	%eax, 4(%esp)
	leal	20(%esp), %eax
	movl	%eax, (%esp)
	fstps	20(%esp)
	call	___unpack_f
	movl	24(%esp), %edx
	xorl	%eax, %eax
	cmpl	$2, %edx
	jbe	L216
	cmpl	$4, %edx
	je	L227
	movl	32(%esp), %edx
	testl	%edx, %edx
	js	L216
	cmpl	$30, %edx
	movl	28(%esp), %ebx
	jg	L228
	movl	36(%esp), %eax
	movl	$30, %ecx
	subl	%edx, %ecx
	shrl	%cl, %eax
	movl	%eax, %edx
	negl	%edx
	testl	%ebx, %ebx
	cmovne	%edx, %eax
L216:
	addl	$40, %esp
	popl	%ebx
	ret
	.p2align 4,,10
L227:
	movl	28(%esp), %edx
	xorl	%eax, %eax
	testl	%edx, %edx
	setne	%al
	addl	$40, %esp
	addl	$2147483647, %eax
	popl	%ebx
	ret
	.p2align 4,,10
L228:
	xorl	%eax, %eax
	testl	%ebx, %ebx
	setne	%al
	addl	$40, %esp
	addl	$2147483647, %eax
	popl	%ebx
	ret
	.p2align 4,,15
	.globl	___negsf2
	.def	___negsf2;	.scl	2;	.type	32;	.endef
___negsf2:
	pushl	%ebx
	subl	$40, %esp
	leal	24(%esp), %ebx
	flds	48(%esp)
	leal	20(%esp), %eax
	movl	%ebx, 4(%esp)
	movl	%eax, (%esp)
	fstps	20(%esp)
	call	___unpack_f
	movl	28(%esp), %edx
	xorl	%eax, %eax
	movl	%ebx, (%esp)
	testl	%edx, %edx
	sete	%al
	movl	%eax, 28(%esp)
	call	___pack_f
	addl	$40, %esp
	popl	%ebx
	ret
	.p2align 4,,15
	.globl	___make_fp
	.def	___make_fp;	.scl	2;	.type	32;	.endef
___make_fp:
	subl	$20, %esp
	movl	24(%esp), %eax
	movl	%eax, 4(%esp)
	movl	28(%esp), %eax
	movl	%eax, 8(%esp)
	movl	32(%esp), %eax
	movl	%eax, 12(%esp)
	movl	36(%esp), %eax
	movl	%eax, 16(%esp)
	leal	4(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_f
	addl	$20, %esp
	ret
	.p2align 4,,15
	.globl	___extendsfdf2
	.def	___extendsfdf2;	.scl	2;	.type	32;	.endef
___extendsfdf2:
	pushl	%ebx
	xorl	%ebx, %ebx
	subl	$72, %esp
	leal	48(%esp), %eax
	flds	80(%esp)
	movl	%eax, 4(%esp)
	leal	44(%esp), %eax
	movl	%eax, (%esp)
	fstps	44(%esp)
	call	___unpack_f
	movl	60(%esp), %ecx
	movl	%ebx, %edx
	movl	%ecx, %eax
	shldl	$30, %ecx, %edx
	sall	$30, %eax
	movl	%edx, 16(%esp)
	movl	%eax, 12(%esp)
	movl	56(%esp), %eax
	movl	%eax, 8(%esp)
	movl	52(%esp), %eax
	movl	%eax, 4(%esp)
	movl	48(%esp), %eax
	movl	%eax, (%esp)
	call	___make_dp
	addl	$72, %esp
	popl	%ebx
	ret
	.globl	___thenan_sf
	.section .rdata,"dr"
	.align 4
___thenan_sf:
	.space 16
	.align 4
LC1:
	.long	-822083584
	.ident	"GCC: (x86_64-posix-seh-rev0, Built by MinGW-W64 project) 8.1.0"
	.def	___make_dp;	.scl	2;	.type	32;	.endef
