	.file	"dp-bit.c"
	.text
	.p2align 4,,15
	.def	__fpadd_parts;	.scl	3;	.type	32;	.endef
__fpadd_parts:
	pushl	%ebp
	movl	%ecx, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	%eax, %ebx
	subl	$36, %esp
	movl	(%eax), %ecx
	movl	%edx, (%esp)
	cmpl	$1, %ecx
	jbe	L1
	movl	(%edx), %esi
	movl	%edx, %eax
	cmpl	$1, %esi
	jbe	L1
	cmpl	$4, %ecx
	je	L33
	cmpl	$4, %esi
	je	L1
	cmpl	$2, %esi
	je	L34
	movl	(%esp), %edi
	movl	%edi, %eax
	cmpl	$2, %ecx
	je	L1
	movl	%edi, %ecx
	movl	8(%edi), %eax
	movl	16(%ebx), %esi
	movl	20(%ebx), %edi
	movl	8(%ebx), %edx
	movl	%esi, 24(%esp)
	movl	16(%ecx), %esi
	movl	%edi, 28(%esp)
	movl	20(%ecx), %edi
	movl	%edx, %ecx
	movl	%edx, 16(%esp)
	subl	%eax, %ecx
	movl	%esi, 8(%esp)
	movl	%edi, 12(%esp)
	js	L35
	cmpl	$63, %ecx
	jg	L7
	testl	%ecx, %ecx
	je	L8
	movl	12(%esp), %edx
	xorl	%esi, %esi
	movl	8(%esp), %eax
	shrdl	%cl, %edx, %eax
	shrl	%cl, %edx
	testb	$32, %cl
	cmovne	%edx, %eax
	cmovne	%esi, %edx
	movl	%eax, %esi
	movl	$-1, %eax
	movl	%edx, %edi
	movl	$-1, %edx
	shldl	%cl, %eax, %edx
	sall	%cl, %eax
	testb	$32, %cl
	je	L39
	movl	%eax, %edx
	xorl	%eax, %eax
L39:
	movl	%eax, %ecx
	movl	%edx, %eax
	notl	%ecx
	notl	%eax
	andl	8(%esp), %ecx
	andl	12(%esp), %eax
	movl	%edi, 12(%esp)
	orl	%eax, %ecx
	setne	%al
	movzbl	%al, %eax
	orl	%esi, %eax
	movl	%eax, 8(%esp)
L8:
	movl	(%esp), %edi
	movl	4(%ebx), %eax
	cmpl	4(%edi), %eax
	je	L9
	testl	%eax, %eax
	jne	L36
	movl	24(%esp), %eax
	movl	28(%esp), %edx
	subl	8(%esp), %eax
	sbbl	12(%esp), %edx
L11:
	testl	%edx, %edx
	js	L12
	movl	16(%esp), %edi
	movl	$0, 4(%ebp)
	movl	%eax, 16(%ebp)
	movl	%edx, 20(%ebp)
	movl	%edi, 8(%ebp)
L13:
	movl	%eax, %ecx
	movl	%edx, %ebx
	movl	$-2, %esi
	addl	$-1, %ecx
	adcl	$-1, %ebx
	movl	%ecx, (%esp)
	movl	$268435455, %ecx
	cmpl	(%esp), %esi
	movl	%ebx, 4(%esp)
	sbbl	4(%esp), %ecx
	jc	L14
	movl	8(%ebp), %edi
	movl	%ebp, (%esp)
	leal	-1(%edi), %ecx
	.p2align 4,,10
L15:
	shldl	$1, %eax, %edx
	addl	%eax, %eax
	movl	$-2, %ebp
	movl	%eax, %esi
	movl	%ecx, %ebx
	movl	%edx, %edi
	subl	$1, %ecx
	addl	$-1, %esi
	adcl	$-1, %edi
	cmpl	%esi, %ebp
	movl	$268435455, %ebp
	sbbl	%edi, %ebp
	jnc	L15
	movl	(%esp), %ebp
	movl	%eax, 16(%ebp)
	movl	%ebp, %eax
	movl	%edx, 20(%ebp)
	movl	%ebx, 8(%ebp)
	movl	$3, 0(%ebp)
L1:
	addl	$36, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L33:
	movl	%ebx, %eax
	cmpl	$4, %esi
	jne	L1
	movl	4(%edx), %edi
	movl	$___thenan_df, %edx
	cmpl	%edi, 4(%ebx)
	cmovne	%edx, %eax
	jmp	L1
	.p2align 4,,10
L34:
	movl	%ebx, %eax
	cmpl	$2, %ecx
	jne	L1
	movl	$2, 0(%ebp)
	movl	4(%ebx), %ecx
	movl	(%esp), %edi
	movl	%ecx, 4(%ebp)
	movl	8(%ebx), %ecx
	movl	4(%ebx), %eax
	andl	4(%edi), %eax
	movl	%ecx, 8(%ebp)
	movl	12(%ebx), %ecx
	movl	%ecx, 12(%ebp)
	movl	16(%ebx), %ecx
	movl	%ecx, 16(%ebp)
	movl	20(%ebx), %ecx
	movl	%eax, 4(%ebp)
	movl	%ebp, %eax
	movl	%ecx, 20(%ebp)
	jmp	L1
	.p2align 4,,10
L35:
	movl	%eax, %ecx
	subl	%edx, %ecx
	cmpl	$63, %ecx
	jle	L37
L7:
	cmpl	%eax, 16(%esp)
	jle	L38
	movl	$0, 8(%esp)
	movl	$0, 12(%esp)
	jmp	L8
	.p2align 4,,10
L9:
	movl	%eax, 4(%ebp)
	movl	16(%esp), %eax
	movl	28(%esp), %edx
	movl	%eax, 8(%ebp)
	movl	24(%esp), %eax
	addl	8(%esp), %eax
	adcl	12(%esp), %edx
	movl	%eax, 16(%ebp)
	movl	%edx, 20(%ebp)
L14:
	movl	$3, 0(%ebp)
	cmpl	$536870911, %edx
	jbe	L24
	addl	$1, 8(%ebp)
	movl	%eax, %ecx
	shrdl	$1, %edx, %eax
	andl	$1, %ecx
	movl	%eax, %ebx
	movl	%ecx, %eax
	shrl	%edx
	orl	%ebx, %eax
	movl	%edx, 20(%ebp)
	movl	%eax, 16(%ebp)
	movl	%ebp, %eax
	jmp	L1
	.p2align 4,,10
L36:
	movl	8(%esp), %eax
	movl	12(%esp), %edx
	subl	24(%esp), %eax
	sbbl	28(%esp), %edx
	jmp	L11
	.p2align 4,,10
L38:
	movl	%eax, 16(%esp)
	movl	$0, 24(%esp)
	movl	$0, 28(%esp)
	jmp	L8
	.p2align 4,,10
L37:
	movl	28(%esp), %edi
	xorl	%edx, %edx
	movl	24(%esp), %esi
	shrdl	%cl, %edi, %esi
	shrl	%cl, %edi
	testb	$32, %cl
	cmovne	%edi, %esi
	cmovne	%edx, %edi
	xorl	%edx, %edx
	movl	%esi, 16(%esp)
	movl	$-1, %esi
	movl	%edi, 20(%esp)
	movl	$-1, %edi
	shldl	%cl, %esi, %edi
	sall	%cl, %esi
	testb	$32, %cl
	cmovne	%esi, %edi
	cmovne	%edx, %esi
	movl	%edi, %ecx
	movl	%esi, %edi
	notl	%ecx
	andl	28(%esp), %ecx
	notl	%edi
	andl	24(%esp), %edi
	movl	16(%esp), %esi
	movl	%eax, 16(%esp)
	orl	%ecx, %edi
	movl	20(%esp), %edi
	setne	%cl
	movzbl	%cl, %ecx
	orl	%esi, %ecx
	movl	%ecx, 24(%esp)
	movl	%edi, 28(%esp)
	jmp	L8
	.p2align 4,,10
L12:
	movl	16(%esp), %edi
	negl	%eax
	movl	$1, 4(%ebp)
	adcl	$0, %edx
	movl	%eax, 16(%ebp)
	negl	%edx
	movl	%edx, 20(%ebp)
	movl	%edi, 8(%ebp)
	jmp	L13
L24:
	movl	%ebp, %eax
	jmp	L1
	.p2align 4,,15
	.globl	___pack_d
	.def	___pack_d;	.scl	2;	.type	32;	.endef
___pack_d:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$20, %esp
	movl	40(%esp), %esi
	movl	(%esi), %ecx
	movl	16(%esi), %eax
	movl	20(%esi), %edx
	cmpl	$1, %ecx
	jbe	L60
	subl	$2, %ecx
	andl	$-3, %ecx
	je	L53
	movl	%eax, %edi
	orl	%edx, %edi
	jne	L61
L53:
	xorl	%edi, %edi
	xorl	%ebp, %ebp
L42:
	movl	4(%esp), %eax
	movl	%ebp, %edx
	movl	%edi, (%esp)
	andl	$1048575, %edx
	andl	$-1048576, %eax
	orl	%edx, %eax
	movl	%eax, 4(%esp)
	fldl	(%esp)
	addl	$20, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L60:
	movl	%eax, %edi
	movl	%edx, %eax
	orl	$524288, %eax
	movl	%edi, %ebx
	movl	%eax, %esi
L58:
	movl	%esi, %eax
	movl	%ebx, %edi
	andl	$1048575, %eax
	movl	%eax, %ebp
	jmp	L42
	.p2align 4,,10
L61:
	movl	8(%esi), %esi
	cmpl	$-1022, %esi
	jl	L62
	cmpl	$1023, %esi
	jg	L53
	cmpb	$-128, %al
	je	L63
	addl	$127, %eax
	adcl	$0, %edx
L49:
	cmpl	$536870911, %edx
	jbe	L50
	shrdl	$1, %edx, %eax
	shrl	%edx
L50:
	shrdl	$8, %edx, %eax
	shrl	$8, %edx
	movl	%edx, %ebp
	movl	%eax, %edi
	movl	%ebp, %eax
	andl	$1048575, %eax
	movl	%eax, %ebp
	jmp	L42
	.p2align 4,,10
L63:
	movl	%eax, %ecx
	andl	$256, %ecx
	je	L49
	addl	$128, %eax
	adcl	$0, %edx
	jmp	L49
	.p2align 4,,10
L62:
	movl	$-1022, %ecx
	xorl	%edi, %edi
	xorl	%ebp, %ebp
	subl	%esi, %ecx
	cmpl	$56, %ecx
	jg	L42
	movl	%eax, %edi
	movl	%edx, %ebp
	xorl	%ebx, %ebx
	shrl	%cl, %ebp
	shrdl	%cl, %edx, %edi
	testb	$32, %cl
	cmovne	%ebp, %edi
	movl	$-1, %esi
	cmovne	%ebx, %ebp
	movl	$-1, %ebx
	shldl	%cl, %ebx, %esi
	sall	%cl, %ebx
	testb	$32, %cl
	je	L65
	movl	%ebx, %esi
	xorl	%ebx, %ebx
L65:
	movl	%ebx, %ecx
	movl	%esi, %ebx
	movl	%ecx, %esi
	movl	%ebx, %ecx
	movl	%ebp, %ebx
	notl	%esi
	notl	%ecx
	andl	%eax, %esi
	andl	%edx, %ecx
	orl	%ecx, %esi
	setne	%al
	movzbl	%al, %eax
	orl	%edi, %eax
	movl	%eax, %ecx
	cmpb	$-128, %al
	jne	L64
	movl	%eax, %esi
	andl	$256, %eax
	movl	%ebp, %edi
	je	L59
	addl	$128, %esi
	adcl	$0, %edi
	shrdl	$8, %edi, %esi
	shrl	$8, %edi
	movl	%edi, %eax
	movl	%esi, %edi
	andl	$1048575, %eax
	movl	%eax, %ebp
	jmp	L42
L64:
	addl	$127, %ecx
	adcl	$0, %ebx
	movl	%ecx, %esi
	movl	%ebx, %edi
L59:
	shrdl	$8, %edi, %esi
	shrl	$8, %edi
	movl	%esi, %ebx
	movl	%edi, %esi
	jmp	L58
	.p2align 4,,15
	.globl	___unpack_d
	.def	___unpack_d;	.scl	2;	.type	32;	.endef
___unpack_d:
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$8, %esp
	movl	24(%esp), %ecx
	movl	28(%esp), %ebx
	movl	4(%ecx), %edi
	movl	(%ecx), %eax
	movzwl	8(%ecx), %esi
	movzbl	9(%ecx), %ecx
	andl	$1048575, %edi
	movl	%eax, (%esp)
	movl	%edi, 4(%esp)
	shrb	$3, %cl
	andl	$1, %ecx
	andl	$2047, %esi
	movl	%ecx, 4(%ebx)
	jne	L67
	movl	4(%esp), %ecx
	movl	(%esp), %edx
	movl	%ecx, %eax
	orl	%edx, %eax
	je	L75
	movl	(%esp), %eax
	movl	$3, (%ebx)
	movl	$-1023, %ecx
	movl	4(%esp), %edx
	shldl	$8, %eax, %edx
	sall	$8, %eax
	.p2align 4,,10
L70:
	shldl	$1, %eax, %edx
	movl	%ecx, %esi
	addl	%eax, %eax
	subl	$1, %ecx
	cmpl	$268435455, %edx
	jbe	L70
	movl	%esi, 8(%ebx)
	movl	%eax, 16(%ebx)
	movl	%edx, 20(%ebx)
	addl	$8, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L67:
	cmpl	$2047, %esi
	je	L76
	movl	(%esp), %eax
	subl	$1023, %esi
	movl	$3, (%ebx)
	movl	4(%esp), %edx
	movl	%esi, 8(%ebx)
	shldl	$8, %eax, %edx
	sall	$8, %eax
	movl	%eax, 16(%ebx)
	movl	%edx, %eax
	orl	$268435456, %eax
	movl	%eax, 20(%ebx)
L66:
	addl	$8, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L75:
	movl	$2, (%ebx)
	addl	$8, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L76:
	movl	4(%esp), %ecx
	movl	(%esp), %edx
	movl	%ecx, %eax
	orl	%edx, %eax
	jne	L72
	movl	$4, (%ebx)
	jmp	L66
	.p2align 4,,10
L72:
	movl	(%esp), %eax
	shrl	$19, %edi
	movl	4(%esp), %edx
	movl	%edi, (%ebx)
	movl	%eax, 16(%ebx)
	movl	%edx, 20(%ebx)
	jmp	L66
	.p2align 4,,15
	.globl	___adddf3
	.def	___adddf3;	.scl	2;	.type	32;	.endef
___adddf3:
	pushl	%esi
	pushl	%ebx
	subl	$124, %esp
	movsd	136(%esp), %xmm0
	leal	48(%esp), %ebx
	movsd	144(%esp), %xmm1
	leal	16(%esp), %eax
	movl	%ebx, 4(%esp)
	movl	%eax, (%esp)
	leal	72(%esp), %esi
	movsd	%xmm0, 16(%esp)
	movsd	%xmm1, 32(%esp)
	call	___unpack_d
	leal	32(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_d
	leal	96(%esp), %ecx
	movl	%esi, %edx
	movl	%ebx, %eax
	call	__fpadd_parts
	movl	%eax, (%esp)
	call	___pack_d
	addl	$124, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___subdf3
	.def	___subdf3;	.scl	2;	.type	32;	.endef
___subdf3:
	pushl	%esi
	pushl	%ebx
	subl	$124, %esp
	movsd	136(%esp), %xmm0
	leal	48(%esp), %ebx
	movsd	144(%esp), %xmm1
	leal	16(%esp), %eax
	movl	%ebx, 4(%esp)
	movl	%eax, (%esp)
	leal	72(%esp), %esi
	movsd	%xmm0, 16(%esp)
	movsd	%xmm1, 32(%esp)
	call	___unpack_d
	leal	32(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_d
	leal	96(%esp), %ecx
	movl	%esi, %edx
	movl	%ebx, %eax
	xorl	$1, 76(%esp)
	call	__fpadd_parts
	movl	%eax, (%esp)
	call	___pack_d
	addl	$124, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___muldf3
	.def	___muldf3;	.scl	2;	.type	32;	.endef
___muldf3:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$156, %esp
	movsd	176(%esp), %xmm0
	leal	48(%esp), %eax
	movsd	184(%esp), %xmm1
	leal	80(%esp), %esi
	movl	%eax, (%esp)
	movl	%esi, 4(%esp)
	leal	104(%esp), %edi
	movsd	%xmm0, 48(%esp)
	movsd	%xmm1, 64(%esp)
	call	___unpack_d
	leal	64(%esp), %eax
	movl	%edi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_d
	movl	80(%esp), %eax
	cmpl	$1, %eax
	ja	L82
L86:
	movl	108(%esp), %eax
	movl	%esi, %edx
	cmpl	%eax, 84(%esp)
	setne	%al
	movzbl	%al, %eax
	movl	%eax, 84(%esp)
L83:
	movl	%edx, (%esp)
	call	___pack_d
	addl	$156, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L82:
	movl	104(%esp), %ecx
	cmpl	$1, %ecx
	jbe	L88
	cmpl	$4, %eax
	jne	L85
	movl	$___thenan_df, %edx
	cmpl	$2, %ecx
	je	L83
	jmp	L86
	.p2align 4,,10
L85:
	cmpl	$4, %ecx
	jne	L87
	movl	$___thenan_df, %edx
	cmpl	$2, %eax
	je	L83
	.p2align 4,,10
L88:
	movl	108(%esp), %eax
	movl	%edi, %edx
	cmpl	%eax, 84(%esp)
	setne	%al
	movzbl	%al, %eax
	movl	%eax, 108(%esp)
	jmp	L83
	.p2align 4,,10
L87:
	movl	84(%esp), %ebx
	cmpl	%ebx, 108(%esp)
	setne	%bl
	movzbl	%bl, %ebx
	movl	%ebx, 12(%esp)
	cmpl	$2, %eax
	je	L120
	cmpl	$2, %ecx
	je	L121
	movl	120(%esp), %ebx
	movl	96(%esp), %esi
	movl	124(%esp), %ebp
	movl	100(%esp), %ecx
	movl	%ebx, %eax
	mull	%esi
	movl	%eax, 24(%esp)
	movl	%esi, %eax
	movl	%edx, 28(%esp)
	mull	%ebp
	movl	%eax, %esi
	movl	%ebp, %eax
	movl	%edx, %edi
	mull	%ecx
	movl	%eax, 32(%esp)
	movl	%ecx, %eax
	movl	$0, %ecx
	movl	%edx, 36(%esp)
	mull	%ebx
	movl	$0, %ebx
	addl	%esi, %eax
	adcl	%edi, %edx
	movl	%eax, 16(%esp)
	cmpl	%esi, %eax
	movl	%edx, %eax
	movl	%edx, 20(%esp)
	sbbl	%edi, %eax
	jnc	L91
	xorl	%ecx, %ecx
	movl	$1, %ebx
L91:
	movl	16(%esp), %esi
	movl	24(%esp), %eax
	movl	28(%esp), %edx
	movl	%esi, %edi
	xorl	%esi, %esi
	movl	%edi, %ebp
	movl	%esi, %edi
	addl	%eax, %edi
	adcl	%edx, %ebp
	cmpl	%eax, %edi
	movl	%ebp, %esi
	sbbl	%edx, %esi
	jnc	L92
	addl	$1, %ecx
	adcl	$0, %ebx
L92:
	movl	20(%esp), %edx
	movl	%edx, %eax
	xorl	%edx, %edx
	addl	32(%esp), %eax
	adcl	36(%esp), %edx
	addl	%ecx, %eax
	movl	112(%esp), %ecx
	adcl	%ebx, %edx
	addl	88(%esp), %ecx
	movl	12(%esp), %ebx
	leal	4(%ecx), %esi
	movl	%esi, 136(%esp)
	movl	%ebx, 132(%esp)
	cmpl	$536870911, %edx
	jbe	L93
	addl	$5, %ecx
	movl	%ecx, %esi
	.p2align 4,,10
L95:
	movl	%eax, %ecx
	movl	%esi, 12(%esp)
	andl	$1, %ecx
	je	L94
	movl	%ebp, %ebx
	movl	%edi, %ecx
	shrl	%ebx
	shrdl	$1, %ebp, %ecx
	orl	$-2147483648, %ebx
	movl	%ecx, %edi
	movl	%ebx, %ebp
L94:
	shrdl	$1, %edx, %eax
	shrl	%edx
	addl	$1, %esi
	cmpl	$536870911, %edx
	ja	L95
	movl	12(%esp), %ebx
L119:
	movl	%ebx, 136(%esp)
L97:
	cmpb	$-128, %al
	je	L122
L100:
	movl	%edx, 148(%esp)
	leal	128(%esp), %edx
	movl	%eax, 144(%esp)
	movl	$3, 128(%esp)
	jmp	L83
L93:
	cmpl	$268435455, %edx
	ja	L97
	addl	$3, %ecx
	.p2align 4,,10
L99:
	shldl	$1, %eax, %edx
	addl	%eax, %eax
	testl	%ebp, %ebp
	movl	%ecx, %ebx
	jns	L98
	movl	%eax, %esi
	orl	$1, %esi
	movl	%esi, 12(%esp)
	movl	12(%esp), %eax
L98:
	shldl	$1, %edi, %ebp
	subl	$1, %ecx
	addl	%edi, %edi
	cmpl	$268435455, %edx
	jbe	L99
	jmp	L119
	.p2align 4,,10
L120:
	movl	%ebx, 84(%esp)
	movl	%esi, %edx
	jmp	L83
L121:
	movl	12(%esp), %eax
	movl	%edi, %edx
	movl	%eax, 108(%esp)
	jmp	L83
L122:
	movl	%eax, %ecx
	shrdl	$8, %edx, %ecx
	xorl	$1, %ecx
	andl	$1, %ecx
	je	L100
	movl	%edi, %ebx
	orl	%ebp, %ebx
	je	L100
	movl	%eax, %ecx
	movl	%edx, %ebx
	addl	$128, %ecx
	movl	%ecx, %edi
	adcl	$0, %ebx
	andl	$-256, %edi
	movl	%ebx, %edx
	movl	%edi, %eax
	jmp	L100
	.p2align 4,,15
	.globl	___divdf3
	.def	___divdf3;	.scl	2;	.type	32;	.endef
___divdf3:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$124, %esp
	movsd	144(%esp), %xmm0
	leal	40(%esp), %eax
	movsd	152(%esp), %xmm1
	leal	72(%esp), %ebx
	movl	%eax, (%esp)
	movl	%ebx, 4(%esp)
	leal	96(%esp), %esi
	movsd	%xmm0, 40(%esp)
	movsd	%xmm1, 56(%esp)
	call	___unpack_d
	leal	56(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_d
	movl	72(%esp), %eax
	cmpl	$1, %eax
	ja	L124
L144:
	movl	%ebx, %esi
L125:
	movl	%esi, (%esp)
	call	___pack_d
	addl	$124, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L124:
	movl	96(%esp), %edx
	cmpl	$1, %edx
	jbe	L125
	movl	100(%esp), %ecx
	xorl	%ecx, 76(%esp)
	leal	-2(%eax), %ecx
	andl	$-3, %ecx
	jne	L126
	movl	$___thenan_df, %esi
	cmpl	%edx, %eax
	jne	L144
	jmp	L125
	.p2align 4,,10
L126:
	cmpl	$4, %edx
	je	L145
	cmpl	$2, %edx
	je	L146
	movl	112(%esp), %edi
	movl	88(%esp), %eax
	movl	92(%esp), %edx
	movl	116(%esp), %ebp
	movl	80(%esp), %ecx
	movl	%edi, 24(%esp)
	subl	104(%esp), %ecx
	cmpl	%edi, %eax
	movl	%edx, %esi
	sbbl	%ebp, %esi
	movl	%ebp, 28(%esp)
	movl	%ecx, 80(%esp)
	jnc	L130
	subl	$1, %ecx
	shldl	$1, %eax, %edx
	addl	%eax, %eax
	movl	%ecx, 80(%esp)
L130:
	movl	$61, %ecx
	movl	%ebx, 36(%esp)
	movl	28(%esp), %ebx
	xorl	%edi, %edi
	movl	%ecx, %esi
	movl	24(%esp), %ecx
	movl	$268435456, %ebp
	movl	$0, 16(%esp)
	movl	$0, 20(%esp)
	movl	%edi, 8(%esp)
	movl	%ebp, 12(%esp)
	.p2align 4,,10
L132:
	cmpl	%ecx, %eax
	movl	%edx, %edi
	sbbl	%ebx, %edi
	jc	L131
	movl	16(%esp), %edi
	orl	8(%esp), %edi
	movl	20(%esp), %ebp
	orl	12(%esp), %ebp
	subl	%ecx, %eax
	sbbl	%ebx, %edx
	movl	%edi, 16(%esp)
	movl	%ebp, 20(%esp)
L131:
	movl	12(%esp), %ebp
	shldl	$1, %eax, %edx
	addl	%eax, %eax
	movl	8(%esp), %edi
	shrdl	$1, %ebp, %edi
	shrl	%ebp
	subl	$1, %esi
	movl	%edi, 8(%esp)
	movl	%ebp, 12(%esp)
	jne	L132
	cmpb	$-128, 16(%esp)
	movl	36(%esp), %ebx
	je	L147
L133:
	movl	16(%esp), %eax
	movl	20(%esp), %edx
	movl	%eax, 88(%esp)
	movl	%edx, 92(%esp)
	jmp	L144
	.p2align 4,,10
L145:
	movl	$0, 88(%esp)
	movl	%ebx, %esi
	movl	$0, 92(%esp)
	movl	$0, 80(%esp)
	jmp	L125
L146:
	movl	$4, 72(%esp)
	movl	%ebx, %esi
	jmp	L125
L147:
	movl	16(%esp), %esi
	movl	20(%esp), %edi
	shrdl	$8, %edi, %esi
	xorl	$1, %esi
	andl	$1, %esi
	je	L133
	movl	%eax, %ecx
	orl	%edx, %ecx
	je	L133
	movl	16(%esp), %eax
	movl	20(%esp), %edx
	addl	$128, %eax
	movl	%eax, %ecx
	adcl	$0, %edx
	xorb	%cl, %cl
	movl	%edx, 20(%esp)
	movl	%ecx, 16(%esp)
	jmp	L133
	.p2align 4,,15
	.globl	___fpcmp_parts_d
	.def	___fpcmp_parts_d;	.scl	2;	.type	32;	.endef
___fpcmp_parts_d:
	pushl	%edi
	movl	$1, %eax
	pushl	%esi
	pushl	%ebx
	movl	16(%esp), %ebx
	movl	20(%esp), %esi
	movl	(%ebx), %edx
	cmpl	$1, %edx
	jbe	L148
	movl	(%esi), %ecx
	cmpl	$1, %ecx
	jbe	L148
	cmpl	$4, %edx
	jne	L150
	cmpl	$4, %ecx
	je	L188
L150:
	cmpl	$4, %edx
	je	L189
	cmpl	$4, %ecx
	je	L154
	cmpl	$2, %edx
	jne	L172
	cmpl	$2, %ecx
	je	L164
L172:
	cmpl	$2, %edx
	je	L154
	movl	4(%ebx), %edx
	cmpl	$2, %ecx
	je	L187
	cmpl	%edx, 4(%esi)
	jne	L187
	movl	8(%esi), %eax
	cmpl	%eax, 8(%ebx)
	jg	L187
	jl	L186
	movl	16(%ebx), %edi
	movl	20(%ebx), %ecx
	movl	20(%esi), %eax
	movl	16(%esi), %ebx
	movl	%eax, %esi
	cmpl	%edi, %ebx
	sbbl	%ecx, %esi
	jc	L187
	cmpl	%ebx, %edi
	sbbl	%eax, %ecx
	movl	$0, %eax
	jnc	L148
L186:
	cmpl	$1, %edx
	sbbl	%eax, %eax
	orl	$1, %eax
L148:
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L189:
	cmpl	$1, 4(%ebx)
	popl	%ebx
	popl	%esi
	popl	%edi
	sbbl	%eax, %eax
	andl	$2, %eax
	subl	$1, %eax
	ret
	.p2align 4,,10
L187:
	cmpl	$1, %edx
	popl	%ebx
	sbbl	%eax, %eax
	popl	%esi
	andl	$2, %eax
	popl	%edi
	subl	$1, %eax
	ret
	.p2align 4,,10
L154:
	cmpl	$1, 4(%esi)
	popl	%ebx
	popl	%esi
	popl	%edi
	sbbl	%eax, %eax
	orl	$1, %eax
	ret
	.p2align 4,,10
L188:
	movl	4(%esi), %eax
	subl	4(%ebx), %eax
	jmp	L148
	.p2align 4,,10
L164:
	xorl	%eax, %eax
	jmp	L148
	.p2align 4,,15
	.globl	___cmpdf2
	.def	___cmpdf2;	.scl	2;	.type	32;	.endef
___cmpdf2:
	pushl	%esi
	pushl	%ebx
	subl	$92, %esp
	movsd	104(%esp), %xmm0
	leal	40(%esp), %ebx
	movsd	112(%esp), %xmm1
	leal	8(%esp), %eax
	movl	%ebx, 4(%esp)
	movl	%eax, (%esp)
	leal	64(%esp), %esi
	movsd	%xmm0, 8(%esp)
	movsd	%xmm1, 24(%esp)
	call	___unpack_d
	leal	24(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_d
	movl	%esi, 4(%esp)
	movl	%ebx, (%esp)
	call	___fpcmp_parts_d
	addl	$92, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___eqdf2
	.def	___eqdf2;	.scl	2;	.type	32;	.endef
___eqdf2:
	pushl	%esi
	pushl	%ebx
	subl	$92, %esp
	movsd	104(%esp), %xmm0
	leal	8(%esp), %eax
	movsd	112(%esp), %xmm1
	leal	40(%esp), %ebx
	movl	%eax, (%esp)
	movl	%ebx, 4(%esp)
	leal	64(%esp), %esi
	movsd	%xmm0, 8(%esp)
	movsd	%xmm1, 24(%esp)
	call	___unpack_d
	leal	24(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_d
	cmpl	$1, 40(%esp)
	movl	$1, %eax
	jbe	L192
	cmpl	$1, 64(%esp)
	jbe	L192
	movl	%esi, 4(%esp)
	movl	%ebx, (%esp)
	call	___fpcmp_parts_d
L192:
	addl	$92, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___nedf2
	.def	___nedf2;	.scl	2;	.type	32;	.endef
___nedf2:
	jmp	___eqdf2
	.p2align 4,,15
	.globl	___gtdf2
	.def	___gtdf2;	.scl	2;	.type	32;	.endef
___gtdf2:
	pushl	%esi
	pushl	%ebx
	subl	$92, %esp
	movsd	104(%esp), %xmm0
	leal	8(%esp), %eax
	movsd	112(%esp), %xmm1
	leal	40(%esp), %ebx
	movl	%eax, (%esp)
	movl	%ebx, 4(%esp)
	leal	64(%esp), %esi
	movsd	%xmm0, 8(%esp)
	movsd	%xmm1, 24(%esp)
	call	___unpack_d
	leal	24(%esp), %eax
	movl	%esi, 4(%esp)
	movl	%eax, (%esp)
	call	___unpack_d
	cmpl	$1, 40(%esp)
	jbe	L201
	cmpl	$1, 64(%esp)
	jbe	L201
	movl	%esi, 4(%esp)
	movl	%ebx, (%esp)
	call	___fpcmp_parts_d
L198:
	addl	$92, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L201:
	movl	$-1, %eax
	jmp	L198
	.p2align 4,,15
	.globl	___gedf2
	.def	___gedf2;	.scl	2;	.type	32;	.endef
___gedf2:
	jmp	___gtdf2
	.p2align 4,,15
	.globl	___ltdf2
	.def	___ltdf2;	.scl	2;	.type	32;	.endef
___ltdf2:
	jmp	___eqdf2
	.p2align 4,,15
	.globl	___ledf2
	.def	___ledf2;	.scl	2;	.type	32;	.endef
___ledf2:
	jmp	___eqdf2
	.p2align 4,,15
	.globl	___unorddf2
	.def	___unorddf2;	.scl	2;	.type	32;	.endef
___unorddf2:
	subl	$92, %esp
	leal	40(%esp), %eax
	movsd	96(%esp), %xmm0
	movsd	104(%esp), %xmm1
	movl	%eax, 4(%esp)
	leal	8(%esp), %eax
	movl	%eax, (%esp)
	movsd	%xmm0, 8(%esp)
	movsd	%xmm1, 24(%esp)
	call	___unpack_d
	leal	64(%esp), %eax
	movl	%eax, 4(%esp)
	leal	24(%esp), %eax
	movl	%eax, (%esp)
	call	___unpack_d
	cmpl	$1, 40(%esp)
	movl	$1, %eax
	jbe	L206
	xorl	%eax, %eax
	cmpl	$1, 64(%esp)
	setbe	%al
L206:
	addl	$92, %esp
	ret
	.p2align 4,,15
	.globl	___floatsidf
	.def	___floatsidf;	.scl	2;	.type	32;	.endef
___floatsidf:
	pushl	%esi
	pushl	%ebx
	subl	$44, %esp
	movl	56(%esp), %eax
	movl	$3, 16(%esp)
	movl	%eax, %edx
	shrl	$31, %edx
	movl	%edx, 20(%esp)
	testl	%eax, %eax
	jne	L212
	movl	$2, 16(%esp)
L213:
	leal	16(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_d
L211:
	addl	$44, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L212:
	jns	L216
	cmpl	$-2147483648, %eax
	je	L217
	negl	%eax
L216:
	bsrl	%eax, %ecx
	xorl	%esi, %esi
	xorl	%ebx, %ebx
	xorl	$31, %ecx
	movl	%esi, %edx
	addl	$29, %ecx
	shldl	%cl, %eax, %edx
	sall	%cl, %eax
	testb	$32, %cl
	cmovne	%eax, %edx
	cmovne	%ebx, %eax
	movl	%edx, 36(%esp)
	movl	%eax, 32(%esp)
	movl	$60, %eax
	subl	%ecx, %eax
	movl	%eax, 24(%esp)
	jmp	L213
	.p2align 4,,10
L217:
	flds	LC1
	jmp	L211
	.p2align 4,,15
	.globl	___floatunsidf
	.def	___floatunsidf;	.scl	2;	.type	32;	.endef
___floatunsidf:
	pushl	%esi
	pushl	%ebx
	subl	$44, %esp
	movl	56(%esp), %eax
	movl	$0, 20(%esp)
	testl	%eax, %eax
	jne	L220
	movl	$2, 16(%esp)
L221:
	leal	16(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_d
	addl	$44, %esp
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L220:
	bsrl	%eax, %ecx
	xorl	%esi, %esi
	xorl	%ebx, %ebx
	movl	$3, 16(%esp)
	xorl	$31, %ecx
	movl	%esi, %edx
	addl	$29, %ecx
	shldl	%cl, %eax, %edx
	sall	%cl, %eax
	testb	$32, %cl
	cmovne	%eax, %edx
	cmovne	%ebx, %eax
	movl	%edx, 36(%esp)
	movl	%eax, 32(%esp)
	movl	$60, %eax
	subl	%ecx, %eax
	movl	%eax, 24(%esp)
	jmp	L221
	.p2align 4,,15
	.globl	___fixdfsi
	.def	___fixdfsi;	.scl	2;	.type	32;	.endef
___fixdfsi:
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$56, %esp
	leal	32(%esp), %eax
	movsd	72(%esp), %xmm0
	movl	%eax, 4(%esp)
	leal	16(%esp), %eax
	movl	%eax, (%esp)
	movsd	%xmm0, 16(%esp)
	call	___unpack_d
	movl	32(%esp), %edx
	xorl	%eax, %eax
	cmpl	$2, %edx
	jbe	L223
	cmpl	$4, %edx
	je	L234
	movl	40(%esp), %edx
	testl	%edx, %edx
	js	L223
	cmpl	$30, %edx
	movl	36(%esp), %ebx
	jg	L235
	movl	$60, %ecx
	movl	48(%esp), %eax
	subl	%edx, %ecx
	movl	52(%esp), %edx
	shrdl	%cl, %edx, %eax
	shrl	%cl, %edx
	testb	$32, %cl
	cmovne	%edx, %eax
	movl	%eax, %edx
	negl	%edx
	testl	%ebx, %ebx
	cmovne	%edx, %eax
L223:
	addl	$56, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L234:
	movl	36(%esp), %edi
	xorl	%eax, %eax
	testl	%edi, %edi
	setne	%al
	addl	$56, %esp
	popl	%ebx
	addl	$2147483647, %eax
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,10
L235:
	xorl	%eax, %eax
	testl	%ebx, %ebx
	setne	%al
	addl	$56, %esp
	popl	%ebx
	addl	$2147483647, %eax
	popl	%esi
	popl	%edi
	ret
	.p2align 4,,15
	.globl	___negdf2
	.def	___negdf2;	.scl	2;	.type	32;	.endef
___negdf2:
	pushl	%ebx
	subl	$56, %esp
	movsd	64(%esp), %xmm0
	leal	32(%esp), %ebx
	leal	16(%esp), %eax
	movl	%ebx, 4(%esp)
	movl	%eax, (%esp)
	movsd	%xmm0, 16(%esp)
	call	___unpack_d
	movl	36(%esp), %edx
	xorl	%eax, %eax
	movl	%ebx, (%esp)
	testl	%edx, %edx
	sete	%al
	movl	%eax, 36(%esp)
	call	___pack_d
	addl	$56, %esp
	popl	%ebx
	ret
	.p2align 4,,15
	.globl	___make_dp
	.def	___make_dp;	.scl	2;	.type	32;	.endef
___make_dp:
	subl	$44, %esp
	movl	48(%esp), %eax
	movl	64(%esp), %edx
	movl	%eax, 16(%esp)
	movl	52(%esp), %eax
	movl	%edx, 36(%esp)
	movl	%eax, 20(%esp)
	movl	56(%esp), %eax
	movl	%eax, 24(%esp)
	movl	60(%esp), %eax
	movl	%eax, 32(%esp)
	leal	16(%esp), %eax
	movl	%eax, (%esp)
	call	___pack_d
	addl	$44, %esp
	ret
	.p2align 4,,15
	.globl	___truncdfsf2
	.def	___truncdfsf2;	.scl	2;	.type	32;	.endef
___truncdfsf2:
	pushl	%ebx
	subl	$72, %esp
	leal	40(%esp), %eax
	movsd	80(%esp), %xmm0
	movl	%eax, 4(%esp)
	leal	24(%esp), %eax
	movl	%eax, (%esp)
	movsd	%xmm0, 24(%esp)
	call	___unpack_d
	movl	56(%esp), %ecx
	movl	60(%esp), %ebx
	movl	%ecx, %eax
	andl	$1073741823, %ecx
	shrdl	$30, %ebx, %eax
	movl	%eax, %edx
	orl	$1, %edx
	testl	%ecx, %ecx
	cmovne	%edx, %eax
	movl	%eax, 12(%esp)
	movl	48(%esp), %eax
	movl	%eax, 8(%esp)
	movl	44(%esp), %eax
	movl	%eax, 4(%esp)
	movl	40(%esp), %eax
	movl	%eax, (%esp)
	call	___make_fp
	addl	$72, %esp
	popl	%ebx
	ret
	.globl	___thenan_df
	.section .rdata,"dr"
	.align 8
___thenan_df:
	.space 24
	.align 4
LC1:
	.long	-822083584
	.ident	"GCC: (x86_64-posix-seh-rev0, Built by MinGW-W64 project) 8.1.0"
	.def	___make_fp;	.scl	2;	.type	32;	.endef
