	.file	"libgcc2.c"
	.text
	.p2align 4,,15
	.globl	___lshrdi3
	.def	___lshrdi3;	.scl	2;	.type	32;	.endef
___lshrdi3:
	pushl	%esi
	pushl	%ebx
	movl	16(%esp), %esi
	movl	20(%esp), %ecx
	movl	12(%esp), %eax
	movl	%esi, %edx
	testl	%ecx, %ecx
	je	L1
	movl	$32, %ebx
	subl	%ecx, %ebx
	testl	%ebx, %ebx
	jle	L8
	shrl	%cl, %eax
	shrl	%cl, %edx
	movl	%ebx, %ecx
	sall	%cl, %esi
	orl	%esi, %eax
L1:
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L8:
	negl	%ebx
	movl	%esi, %eax
	xorl	%edx, %edx
	movl	%ebx, %ecx
	popl	%ebx
	shrl	%cl, %eax
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___ashldi3
	.def	___ashldi3;	.scl	2;	.type	32;	.endef
___ashldi3:
	pushl	%esi
	pushl	%ebx
	movl	12(%esp), %esi
	movl	20(%esp), %ecx
	movl	16(%esp), %edx
	movl	%esi, %eax
	testl	%ecx, %ecx
	je	L9
	movl	$32, %ebx
	subl	%ecx, %ebx
	testl	%ebx, %ebx
	jle	L15
	sall	%cl, %edx
	sall	%cl, %eax
	movl	%ebx, %ecx
	shrl	%cl, %esi
	orl	%esi, %edx
L9:
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L15:
	negl	%ebx
	xorl	%eax, %eax
	movl	%ebx, %ecx
	popl	%ebx
	sall	%cl, %esi
	movl	%esi, %edx
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___ashrdi3
	.def	___ashrdi3;	.scl	2;	.type	32;	.endef
___ashrdi3:
	pushl	%esi
	pushl	%ebx
	movl	16(%esp), %esi
	movl	20(%esp), %ecx
	movl	12(%esp), %eax
	movl	%esi, %edx
	testl	%ecx, %ecx
	je	L16
	movl	$32, %ebx
	subl	%ecx, %ebx
	testl	%ebx, %ebx
	jle	L22
	shrl	%cl, %eax
	sarl	%cl, %edx
	movl	%ebx, %ecx
	sall	%cl, %esi
	orl	%esi, %eax
L16:
	popl	%ebx
	popl	%esi
	ret
	.p2align 4,,10
L22:
	negl	%ebx
	movl	%esi, %eax
	sarl	$31, %edx
	movl	%ebx, %ecx
	popl	%ebx
	sarl	%cl, %eax
	popl	%esi
	ret
	.p2align 4,,15
	.globl	___muldi3
	.def	___muldi3;	.scl	2;	.type	32;	.endef
___muldi3:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	20(%esp), %ecx
	movl	28(%esp), %edx
	movzwl	%cx, %eax
	movl	%ecx, %ebx
	movl	%edx, %esi
	shrl	$16, %ebx
	movzwl	%dx, %edi
	shrl	$16, %esi
	movl	%eax, %ebp
	imull	%edi, %ebp
	imull	%esi, %eax
	imull	%ebx, %edi
	imull	%esi, %ebx
	leal	(%eax,%edi), %esi
	movl	%ebp, %eax
	movzwl	%bp, %ebp
	shrl	$16, %eax
	addl	%esi, %eax
	leal	65536(%ebx), %esi
	cmpl	%eax, %edi
	cmova	%esi, %ebx
	movl	%eax, %esi
	sall	$16, %eax
	imull	32(%esp), %ecx
	shrl	$16, %esi
	imull	24(%esp), %edx
	addl	%esi, %ebx
	addl	%ebp, %eax
	addl	%ebx, %ecx
	popl	%ebx
	addl	%ecx, %edx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,15
	.globl	___divdi3
	.def	___divdi3;	.scl	2;	.type	32;	.endef
___divdi3:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$36, %esp
	movl	60(%esp), %ebx
	movl	$0, 16(%esp)
	movl	56(%esp), %ecx
	movl	64(%esp), %esi
	movl	68(%esp), %edi
	testl	%ebx, %ebx
	movl	%ebx, 12(%esp)
	movl	%ebx, %edx
	movl	%ecx, 8(%esp)
	jns	L27
	negl	%ecx
	movl	$-1, 16(%esp)
	adcl	$0, %ebx
	movl	%ecx, 8(%esp)
	negl	%ebx
	movl	%ebx, 12(%esp)
	movl	%ebx, %edx
L27:
	testl	%edi, %edi
	movl	%edi, %ebp
	jns	L28
	notl	16(%esp)
	negl	%esi
	adcl	$0, %edi
	negl	%edi
	movl	%edi, %ebp
L28:
	movl	%esi, %ecx
	movl	%esi, 4(%esp)
	movl	%ebp, %ebx
	movl	%edx, %eax
	testl	%ebp, %ebp
	jne	L29
	cmpl	%edx, %esi
	jbe	L30
	cmpl	$65535, %esi
	jbe	L139
	movl	4(%esp), %edi
	cmpl	$16777215, %edi
	jbe	L33
	movl	%edi, %esi
	movl	$24, %ebx
	shrl	$24, %esi
L32:
	movzbl	___clz_tab(%esi), %ecx
	movl	$32, %esi
	addl	%ecx, %ebx
	subl	%ebx, %esi
	je	L34
	movl	8(%esp), %edi
	movl	%esi, %ecx
	movl	%edx, %eax
	sall	%cl, 4(%esp)
	sall	%cl, %eax
	movl	%ebx, %ecx
	movl	%edi, %edx
	shrl	%cl, %edx
	movl	%esi, %ecx
	sall	%cl, %edi
	orl	%edx, %eax
	movl	%edi, 8(%esp)
L34:
	movl	4(%esp), %ebx
	xorl	%edx, %edx
	movl	8(%esp), %edi
	movl	%ebx, %ebp
	movzwl	%bx, %esi
	shrl	$16, %ebp
	movl	%esi, %ecx
	shrl	$16, %edi
	divl	%ebp
	imull	%eax, %ecx
	sall	$16, %edx
	movl	%eax, 20(%esp)
	orl	%edi, %edx
	cmpl	%edx, %ecx
	jbe	L35
	addl	%ebx, %edx
	setc	%bl
	movzbl	%bl, %ebx
	cmpl	%edx, %ecx
	jbe	L94
	testl	%ebx, %ebx
	je	L38
L94:
	subl	$1, %eax
	movl	%eax, 20(%esp)
L35:
	movl	%edx, %eax
	xorl	%edx, %edx
	subl	%ecx, %eax
	movzwl	8(%esp), %ecx
	divl	%ebp
	imull	%eax, %esi
	sall	$16, %edx
	movl	%eax, %edi
	orl	%ecx, %edx
	cmpl	%edx, %esi
	jbe	L40
	xorl	%ecx, %ecx
	addl	4(%esp), %edx
	setc	%cl
	cmpl	%edx, %esi
	jbe	L95
	leal	-2(%eax), %edi
	testl	%ecx, %ecx
	sete	%dl
	testb	%dl, %dl
	jne	L40
L95:
	leal	-1(%eax), %edi
L40:
	movl	20(%esp), %ebx
	xorl	%esi, %esi
	sall	$16, %ebx
	orl	%edi, %ebx
	jmp	L45
	.p2align 4,,10
L30:
	movl	4(%esp), %esi
	testl	%esi, %esi
	je	L140
	cmpl	$65535, 4(%esp)
	jbe	L141
	cmpl	$16777216, 4(%esp)
	sbbl	%ebx, %ebx
	andl	$-8, %ebx
	addl	$24, %ebx
L48:
	movl	4(%esp), %edi
	movl	%ebx, %ecx
	movl	%edi, %eax
	shrl	%cl, %eax
	movzbl	___clz_tab(%eax), %ecx
	movl	$32, %eax
	addl	%ecx, %ebx
	subl	%ebx, %eax
	jne	L49
	movl	%edi, %ebx
	movl	%edx, %eax
	movl	$1, %esi
	shrl	$16, %ebx
	subl	%edi, %eax
	movzwl	%di, %edi
	movl	%ebx, 20(%esp)
L50:
	xorl	%edx, %edx
	movl	8(%esp), %ebx
	divl	20(%esp)
	shrl	$16, %ebx
	movl	%eax, %ecx
	sall	$16, %edx
	movl	%eax, %ebp
	imull	%edi, %ecx
	orl	%ebx, %edx
	cmpl	%edx, %ecx
	jbe	L61
	addl	4(%esp), %edx
	jc	L98
	cmpl	%edx, %ecx
	ja	L64
L98:
	leal	-1(%eax), %ebp
L61:
	movl	%edx, %eax
	xorl	%edx, %edx
	subl	%ecx, %eax
	movzwl	8(%esp), %ecx
	divl	20(%esp)
	imull	%eax, %edi
	sall	$16, %edx
	movl	%eax, %ebx
	orl	%ecx, %edx
	cmpl	%edx, %edi
	jbe	L66
	addl	4(%esp), %edx
	jc	L99
	leal	-2(%eax), %ebx
	cmpl	%edx, %edi
	seta	%dl
	testb	%dl, %dl
	jne	L66
L99:
	leal	-1(%eax), %ebx
L66:
	movl	%ebp, %edi
	sall	$16, %edi
	orl	%edi, %ebx
	jmp	L45
	.p2align 4,,10
L29:
	cmpl	%edx, %ebp
	ja	L92
	movl	%ebp, %eax
	cmpl	$65535, %ebp
	jbe	L142
	cmpl	$16777215, %ebp
	jbe	L73
	shrl	$24, %eax
	movl	$24, %ecx
L72:
	movzbl	___clz_tab(%eax), %eax
	addl	%ecx, %eax
	movl	$32, %ecx
	movl	%ecx, %edi
	subl	%eax, %edi
	jne	L74
	cmpl	%edx, %ebp
	movl	8(%esp), %edi
	setb	%bl
	cmpl	%edi, 4(%esp)
	setbe	%al
	xorl	%esi, %esi
	orl	%eax, %ebx
	movzbl	%bl, %ebx
	jmp	L45
	.p2align 4,,10
L92:
	xorl	%esi, %esi
	xorl	%ebx, %ebx
L45:
	movl	16(%esp), %ecx
	movl	%ebx, %eax
	movl	%esi, %edx
	testl	%ecx, %ecx
	je	L26
	negl	%eax
	adcl	$0, %edx
	negl	%edx
L26:
	addl	$36, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L142:
	xorl	%ecx, %ecx
	cmpl	$255, %ebp
	jbe	L72
	shrl	$8, %eax
	movl	$8, %ecx
	jmp	L72
	.p2align 4,,10
L139:
	cmpl	$255, %esi
	jbe	L32
	shrl	$8, %ecx
	movl	$8, %ebx
	movl	%ecx, %esi
	jmp	L32
	.p2align 4,,10
L140:
	ud2
	.p2align 4,,10
L74:
	movl	4(%esp), %esi
	movl	%edi, %ecx
	sall	%cl, %ebp
	movl	%eax, %ecx
	movl	%esi, %ebx
	shrl	%cl, %ebx
	movl	%edi, %ecx
	sall	%cl, %esi
	orl	%ebp, %ebx
	movl	%edx, %ebp
	movl	%esi, 20(%esp)
	movl	8(%esp), %esi
	sall	%cl, %ebp
	movl	%eax, %ecx
	shrl	%cl, %esi
	movl	%eax, %ecx
	shrl	%cl, %edx
	orl	%esi, %ebp
	movl	%ebx, %esi
	movl	%edx, %eax
	shrl	$16, %ebx
	xorl	%edx, %edx
	movl	%esi, 4(%esp)
	divl	%ebx
	movzwl	4(%esp), %esi
	movl	%ebx, 24(%esp)
	movl	%esi, %ecx
	movl	%eax, 28(%esp)
	movl	%eax, %ebx
	imull	%eax, %ecx
	movl	%ebp, %eax
	sall	$16, %edx
	shrl	$16, %eax
	orl	%eax, %edx
	cmpl	%edx, %ecx
	jbe	L75
	xorl	%eax, %eax
	addl	4(%esp), %edx
	setc	%al
	cmpl	%edx, %ecx
	jbe	L100
	testl	%eax, %eax
	je	L78
L100:
	movl	28(%esp), %ebx
	subl	$1, %ebx
L75:
	movl	%edx, %eax
	xorl	%edx, %edx
	movzwl	%bp, %ebp
	subl	%ecx, %eax
	divl	24(%esp)
	imull	%eax, %esi
	sall	$16, %edx
	movl	%eax, %ecx
	orl	%edx, %ebp
	cmpl	%ebp, %esi
	jbe	L80
	addl	4(%esp), %ebp
	jc	L101
	cmpl	%ebp, %esi
	ja	L83
L101:
	leal	-1(%eax), %ecx
L80:
	sall	$16, %ebx
	subl	%esi, %ebp
	orl	%ecx, %ebx
	movl	20(%esp), %ecx
	movl	%ebp, 4(%esp)
	movzwl	%bx, %esi
	movl	%ebx, %eax
	movl	%esi, %ebp
	shrl	$16, %eax
	movzwl	%cx, %edx
	shrl	$16, %ecx
	imull	%edx, %ebp
	imull	%ecx, %esi
	imull	%eax, %edx
	imull	%ecx, %eax
	movl	%ebp, %ecx
	shrl	$16, %ecx
	addl	%edx, %esi
	addl	%ecx, %esi
	leal	65536(%eax), %ecx
	cmpl	%esi, %edx
	movl	%esi, %edx
	cmova	%ecx, %eax
	shrl	$16, %edx
	addl	%edx, %eax
	cmpl	%eax, 4(%esp)
	jb	L86
	movl	8(%esp), %edx
	movl	%edi, %ecx
	sall	$16, %esi
	movzwl	%bp, %edi
	addl	%edi, %esi
	sall	%cl, %edx
	cmpl	%esi, %edx
	jnb	L138
	cmpl	%eax, 4(%esp)
	jne	L138
L86:
	subl	$1, %ebx
L138:
	xorl	%esi, %esi
	jmp	L45
	.p2align 4,,10
L49:
	movl	8(%esp), %ebp
	movl	%eax, %ecx
	movl	%edx, %esi
	sall	%cl, 4(%esp)
	sall	%cl, %esi
	movl	%ebx, %ecx
	movl	4(%esp), %edi
	shrl	%cl, %ebp
	movl	%eax, %ecx
	movl	%edx, %eax
	sall	%cl, 8(%esp)
	orl	%esi, %ebp
	movl	%ebx, %ecx
	xorl	%edx, %edx
	movl	%edi, %esi
	shrl	%cl, %eax
	movzwl	4(%esp), %edi
	shrl	$16, %esi
	movl	%ebp, %ecx
	divl	%esi
	shrl	$16, %ecx
	movl	%esi, 20(%esp)
	movl	%edi, %ebx
	imull	%eax, %ebx
	sall	$16, %edx
	movl	%eax, %esi
	orl	%ecx, %edx
	cmpl	%edx, %ebx
	jbe	L51
	addl	4(%esp), %edx
	jc	L96
	cmpl	%edx, %ebx
	ja	L54
L96:
	leal	-1(%eax), %esi
L51:
	movl	%edx, %eax
	xorl	%edx, %edx
	movzwl	%bp, %ebp
	subl	%ebx, %eax
	movl	%edi, %ebx
	divl	20(%esp)
	imull	%eax, %ebx
	sall	$16, %edx
	movl	%eax, %ecx
	orl	%ebp, %edx
	cmpl	%edx, %ebx
	jbe	L56
	addl	4(%esp), %edx
	movl	%edx, 24(%esp)
	setc	%dl
	movzbl	%dl, %edx
	movl	%edx, %ebp
	movl	24(%esp), %edx
	cmpl	%edx, %ebx
	jbe	L97
	testl	%ebp, %ebp
	je	L59
L97:
	leal	-1(%eax), %ecx
L56:
	movl	%edx, %eax
	sall	$16, %esi
	subl	%ebx, %eax
	orl	%ecx, %esi
	jmp	L50
L141:
	cmpl	$256, 4(%esp)
	movl	$8, %eax
	cmovnb	%eax, %ebx
	jmp	L48
	.p2align 4,,10
L33:
	movl	4(%esp), %esi
	movl	$16, %ebx
	shrl	$16, %esi
	jmp	L32
	.p2align 4,,10
L73:
	shrl	$16, %eax
	movl	$16, %ecx
	jmp	L72
	.p2align 4,,10
L64:
	subl	$2, %ebp
	addl	4(%esp), %edx
	jmp	L61
	.p2align 4,,10
L38:
	subl	$2, 20(%esp)
	addl	4(%esp), %edx
	jmp	L35
	.p2align 4,,10
L78:
	subl	$2, %ebx
	addl	4(%esp), %edx
	jmp	L75
	.p2align 4,,10
L54:
	subl	$2, %esi
	addl	4(%esp), %edx
	jmp	L51
	.p2align 4,,10
L83:
	subl	$2, %ecx
	addl	4(%esp), %ebp
	jmp	L80
	.p2align 4,,10
L59:
	subl	$2, %ecx
	addl	4(%esp), %edx
	jmp	L56
	.p2align 4,,15
	.globl	___moddi3
	.def	___moddi3;	.scl	2;	.type	32;	.endef
___moddi3:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$36, %esp
	movl	60(%esp), %edi
	movl	$0, 16(%esp)
	movl	56(%esp), %esi
	movl	64(%esp), %eax
	movl	68(%esp), %edx
	testl	%edi, %edi
	movl	%edi, 8(%esp)
	jns	L144
	negl	%esi
	movl	$-1, 16(%esp)
	adcl	$0, %edi
	negl	%edi
	movl	%edi, 8(%esp)
L144:
	testl	%edx, %edx
	movl	%edx, %ebx
	jns	L145
	negl	%eax
	adcl	$0, %edx
	negl	%edx
	movl	%edx, %ebx
L145:
	movl	8(%esp), %ebp
	movl	%eax, 4(%esp)
	movl	%ebx, %ecx
	movl	%esi, 12(%esp)
	movl	%ebp, 20(%esp)
	testl	%ebx, %ebx
	jne	L146
	cmpl	%ebp, %eax
	jbe	L147
	cmpl	$65535, %eax
	jbe	L263
	movl	4(%esp), %eax
	cmpl	$16777215, %eax
	jbe	L150
	movl	%eax, %ebx
	movl	$24, %ecx
	shrl	$24, %ebx
L149:
	movzbl	___clz_tab(%ebx), %ebx
	addl	%ecx, %ebx
	movl	$32, %ecx
	movl	%ecx, %edi
	subl	%ebx, %edi
	je	L151
	movl	12(%esp), %esi
	movl	%edi, %ecx
	movl	8(%esp), %eax
	sall	%cl, 4(%esp)
	movl	%esi, %edx
	sall	%cl, %eax
	movl	%ebx, %ecx
	shrl	%cl, %edx
	movl	%edi, %ecx
	orl	%eax, %edx
	sall	%cl, %esi
	movl	%edx, 20(%esp)
	movl	%esi, 12(%esp)
L151:
	movl	4(%esp), %ebp
	xorl	%edx, %edx
	movl	20(%esp), %eax
	movl	%ebp, %ecx
	movzwl	%bp, %ebx
	shrl	$16, %ecx
	divl	%ecx
	imull	%ebx, %eax
	movl	%eax, %esi
	movl	%edx, %eax
	movl	12(%esp), %edx
	sall	$16, %eax
	shrl	$16, %edx
	orl	%eax, %edx
	cmpl	%edx, %esi
	jbe	L152
	xorl	%eax, %eax
	addl	%ebp, %edx
	setc	%al
	cmpl	%edx, %esi
	jbe	L152
	testl	%eax, %eax
	sete	%al
	addl	%edx, %ebp
	testb	%al, %al
	cmovne	%ebp, %edx
L152:
	movl	%edx, %eax
	xorl	%edx, %edx
	subl	%esi, %eax
	divl	%ecx
	movzwl	12(%esp), %ecx
	imull	%ebx, %eax
	sall	$16, %edx
	orl	%ecx, %edx
	cmpl	%edx, %eax
	jbe	L155
	movl	4(%esp), %esi
	addl	%esi, %edx
	jc	L155
	cmpl	%edx, %eax
	seta	%cl
	addl	%edx, %esi
	testb	%cl, %cl
	cmovne	%esi, %edx
L155:
	subl	%eax, %edx
	movl	%edx, %ebx
L158:
	movl	%edi, %ecx
	xorl	%edx, %edx
	shrl	%cl, %ebx
	movl	%ebx, %eax
	jmp	L176
	.p2align 4,,10
L147:
	movl	4(%esp), %ebx
	testl	%ebx, %ebx
	je	L264
	cmpl	$65535, 4(%esp)
	jbe	L265
	cmpl	$16777216, 4(%esp)
	sbbl	%ecx, %ecx
	andl	$-8, %ecx
	addl	$24, %ecx
L161:
	movl	4(%esp), %esi
	movl	%esi, %eax
	shrl	%cl, %eax
	movzbl	___clz_tab(%eax), %eax
	addl	%ecx, %eax
	movl	$32, %ecx
	movl	%ecx, %edi
	subl	%eax, %edi
	jne	L162
	movl	8(%esp), %eax
	movl	%esi, %ebx
	shrl	$16, %ebx
	subl	%esi, %eax
	movzwl	%si, %esi
L163:
	xorl	%edx, %edx
	divl	%ebx
	imull	%esi, %eax
	sall	$16, %edx
	movl	%eax, %ecx
	movl	12(%esp), %eax
	shrl	$16, %eax
	orl	%edx, %eax
	cmpl	%eax, %ecx
	jbe	L170
	movl	4(%esp), %ebp
	addl	%ebp, %eax
	jc	L170
	cmpl	%eax, %ecx
	seta	%dl
	addl	%eax, %ebp
	testb	%dl, %dl
	cmovne	%ebp, %eax
L170:
	subl	%ecx, %eax
	xorl	%edx, %edx
	divl	%ebx
	imull	%eax, %esi
	movl	%edx, %eax
	movzwl	12(%esp), %edx
	sall	$16, %eax
	orl	%eax, %edx
	cmpl	%edx, %esi
	jbe	L173
	movl	4(%esp), %ecx
	xorl	%eax, %eax
	addl	%ecx, %edx
	setc	%al
	cmpl	%edx, %esi
	jbe	L173
	testl	%eax, %eax
	sete	%al
	addl	%edx, %ecx
	testb	%al, %al
	cmovne	%ecx, %edx
L173:
	subl	%esi, %edx
	movl	%edx, %ebx
	jmp	L158
	.p2align 4,,10
L146:
	movl	8(%esp), %ebp
	movl	%esi, %ecx
	cmpl	%ebp, %ebx
	jbe	L177
	movl	%esi, %eax
	movl	%ebp, %edx
L176:
	movl	16(%esp), %ecx
	testl	%ecx, %ecx
	je	L143
	negl	%eax
	adcl	$0, %edx
	negl	%edx
L143:
	addl	$36, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L177:
	movl	%ebx, %eax
	cmpl	$65535, %ebx
	jbe	L266
	cmpl	$16777215, %ebx
	jbe	L180
	shrl	$24, %eax
	movl	$24, %esi
L179:
	movzbl	___clz_tab(%eax), %eax
	addl	%esi, %eax
	movl	$32, %esi
	subl	%eax, %esi
	movl	%eax, 24(%esp)
	movl	%esi, 20(%esp)
	jne	L181
	cmpl	8(%esp), %ebx
	jb	L208
	movl	12(%esp), %edi
	cmpl	%edi, 4(%esp)
	jbe	L208
L182:
	movl	%ecx, %eax
	movl	8(%esp), %edx
	jmp	L176
	.p2align 4,,10
L266:
	xorl	%esi, %esi
	cmpl	$255, %ebx
	jbe	L179
	shrl	$8, %eax
	movl	$8, %esi
	jmp	L179
	.p2align 4,,10
L263:
	movl	%eax, %ebx
	cmpl	$255, %eax
	jbe	L149
	shrl	$8, %ebx
	movl	$8, %ecx
	jmp	L149
	.p2align 4,,10
L264:
	ud2
	.p2align 4,,10
L162:
	movl	8(%esp), %edx
	movl	%edi, %ecx
	movl	12(%esp), %esi
	sall	%cl, 4(%esp)
	movl	%edx, %ebx
	sall	%cl, %ebx
	movl	%esi, %ebp
	movl	%eax, %ecx
	shrl	%cl, %ebp
	movl	%ebp, %ecx
	orl	%ebx, %ecx
	movl	%ecx, %ebp
	movl	%ecx, 8(%esp)
	movl	%edi, %ecx
	sall	%cl, %esi
	movl	%eax, %ecx
	movl	%esi, 12(%esp)
	movl	4(%esp), %esi
	shrl	%cl, %edx
	movl	%edx, %eax
	xorl	%edx, %edx
	movl	%esi, %ebx
	movzwl	4(%esp), %esi
	shrl	$16, %ebx
	divl	%ebx
	imull	%esi, %eax
	movl	%eax, %ecx
	movl	%edx, %eax
	movl	%ebp, %edx
	sall	$16, %eax
	shrl	$16, %edx
	orl	%edx, %eax
	cmpl	%eax, %ecx
	jbe	L164
	movl	4(%esp), %ebp
	addl	%ebp, %eax
	jc	L164
	cmpl	%eax, %ecx
	seta	%dl
	addl	%eax, %ebp
	testb	%dl, %dl
	cmovne	%ebp, %eax
L164:
	subl	%ecx, %eax
	xorl	%edx, %edx
	movzwl	8(%esp), %ecx
	divl	%ebx
	imull	%esi, %eax
	sall	$16, %edx
	orl	%ecx, %edx
	cmpl	%edx, %eax
	jbe	L167
	movl	4(%esp), %ebp
	xorl	%ecx, %ecx
	addl	%ebp, %edx
	setc	%cl
	cmpl	%edx, %eax
	jbe	L167
	testl	%ecx, %ecx
	sete	%cl
	addl	%edx, %ebp
	testb	%cl, %cl
	cmovne	%ebp, %edx
L167:
	subl	%eax, %edx
	movl	%edx, %eax
	jmp	L163
	.p2align 4,,10
L181:
	movl	20(%esp), %edi
	movl	4(%esp), %edx
	movl	24(%esp), %esi
	movl	%edi, %ecx
	sall	%cl, %ebx
	movl	%edx, %eax
	movl	%edx, %ebp
	movl	8(%esp), %edx
	movl	%esi, %ecx
	shrl	%cl, %eax
	movl	%edi, %ecx
	orl	%ebx, %eax
	movl	12(%esp), %ebx
	sall	%cl, %ebp
	movl	%edx, %edi
	movl	%ebp, 28(%esp)
	sall	%cl, %edi
	movl	%esi, %ecx
	movl	%eax, 4(%esp)
	movl	%ebx, %ebp
	shrl	%cl, %ebp
	movzbl	20(%esp), %ecx
	orl	%edi, %ebp
	movl	%eax, %edi
	shrl	$16, %edi
	sall	%cl, %ebx
	movl	%esi, %ecx
	movl	%ebp, %esi
	shrl	%cl, %edx
	movl	%ebx, 12(%esp)
	movzwl	4(%esp), %ebx
	shrl	$16, %esi
	movl	%edx, %eax
	xorl	%edx, %edx
	divl	%edi
	movl	%eax, %ecx
	sall	$16, %edx
	movl	%eax, 8(%esp)
	imull	%ebx, %ecx
	orl	%esi, %edx
	cmpl	%edx, %ecx
	jbe	L186
	addl	4(%esp), %edx
	jc	L209
	cmpl	%edx, %ecx
	ja	L189
L209:
	subl	$1, %eax
	movl	%eax, 8(%esp)
L186:
	movl	%edx, %eax
	xorl	%edx, %edx
	subl	%ecx, %eax
	divl	%edi
	movzwl	%bp, %edi
	imull	%eax, %ebx
	sall	$16, %edx
	movl	%eax, %ecx
	orl	%edi, %edx
	cmpl	%edx, %ebx
	jbe	L191
	movl	%edx, %esi
	xorl	%edx, %edx
	addl	4(%esp), %esi
	setc	%dl
	movl	%edx, %edi
	movl	%esi, %edx
	cmpl	%esi, %ebx
	jbe	L210
	testl	%edi, %edi
	je	L194
L210:
	leal	-1(%eax), %ecx
L191:
	movl	8(%esp), %eax
	subl	%ebx, %edx
	movl	28(%esp), %edi
	movl	%edx, %ebp
	sall	$16, %eax
	orl	%ecx, %eax
	movzwl	%di, %ebx
	shrl	$16, %edi
	movzwl	%ax, %edx
	shrl	$16, %eax
	movl	%edx, %ecx
	imull	%edi, %edx
	imull	%ebx, %ecx
	imull	%eax, %ebx
	imull	%edi, %eax
	movl	%ecx, %edi
	movzwl	%cx, %ecx
	addl	%ebx, %edx
	shrl	$16, %edi
	addl	%edi, %edx
	leal	65536(%eax), %edi
	cmpl	%edx, %ebx
	movl	%edx, %ebx
	cmova	%edi, %eax
	shrl	$16, %ebx
	sall	$16, %edx
	addl	%ebx, %eax
	addl	%ecx, %edx
	cmpl	%eax, %ebp
	jb	L197
	jne	L198
	cmpl	%edx, 12(%esp)
	jnb	L198
L197:
	movl	28(%esp), %edi
	xorl	%ecx, %ecx
	cmpl	%edi, %edx
	setb	%cl
	addl	4(%esp), %ecx
	subl	%edi, %edx
	subl	%ecx, %eax
L198:
	movl	12(%esp), %edi
	movzbl	24(%esp), %ecx
	movl	20(%esp), %esi
	movl	%edi, %ebx
	subl	%edx, %ebx
	sbbl	%eax, %ebp
	movl	%ebp, %edi
	sall	%cl, %edi
	movl	%esi, %ecx
	shrl	%cl, %ebx
	shrl	%cl, %ebp
	orl	%ebx, %edi
	movl	%ebp, %edx
	movl	%edi, %eax
	jmp	L176
L265:
	cmpl	$256, 4(%esp)
	movl	$8, %eax
	cmovnb	%eax, %ecx
	jmp	L161
	.p2align 4,,10
L180:
	shrl	$16, %eax
	movl	$16, %esi
	jmp	L179
	.p2align 4,,10
L150:
	movl	4(%esp), %ebx
	movl	$16, %ecx
	shrl	$16, %ebx
	jmp	L149
	.p2align 4,,10
L208:
	movl	12(%esp), %esi
	xorl	%edx, %edx
	movl	4(%esp), %edi
	movl	8(%esp), %eax
	cmpl	%edi, %esi
	setb	%dl
	subl	%ebx, %eax
	subl	%edi, %esi
	subl	%edx, %eax
	movl	%esi, %ecx
	movl	%eax, 8(%esp)
	jmp	L182
	.p2align 4,,10
L189:
	subl	$2, 8(%esp)
	addl	4(%esp), %edx
	jmp	L186
	.p2align 4,,10
L194:
	subl	$2, %ecx
	addl	4(%esp), %edx
	jmp	L191
	.p2align 4,,15
	.globl	___umoddi3
	.def	___umoddi3;	.scl	2;	.type	32;	.endef
___umoddi3:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$28, %esp
	movl	48(%esp), %ebx
	movl	56(%esp), %esi
	movl	60(%esp), %edi
	movl	52(%esp), %edx
	movl	%ebx, 4(%esp)
	movl	%esi, (%esp)
	testl	%edi, %edi
	jne	L268
	movl	%edi, %ecx
	cmpl	%edx, %esi
	jbe	L269
	movl	%edx, %eax
	movl	%esi, %edi
	cmpl	$65535, %esi
	jbe	L380
	cmpl	$16777215, %esi
	jbe	L272
	shrl	$24, %edi
	movl	$24, %ecx
L271:
	movzbl	___clz_tab(%edi), %ebp
	movl	$32, %edi
	addl	%ecx, %ebp
	subl	%ebp, %edi
	je	L273
	movl	%edi, %ecx
	movl	%ebx, %eax
	sall	%cl, %esi
	sall	%cl, %edx
	movl	%ebp, %ecx
	shrl	%cl, %eax
	movl	%edi, %ecx
	movl	%esi, (%esp)
	sall	%cl, %ebx
	orl	%edx, %eax
	movl	%ebx, 4(%esp)
L273:
	movl	(%esp), %ebp
	xorl	%edx, %edx
	movl	%ebp, %ecx
	movzwl	%bp, %esi
	shrl	$16, %ecx
	divl	%ecx
	imull	%esi, %eax
	movl	%eax, %ebx
	movl	%edx, %eax
	movl	4(%esp), %edx
	sall	$16, %eax
	shrl	$16, %edx
	orl	%eax, %edx
	cmpl	%edx, %ebx
	jbe	L274
	xorl	%eax, %eax
	addl	%ebp, %edx
	setc	%al
	cmpl	%edx, %ebx
	jbe	L274
	testl	%eax, %eax
	sete	%al
	movl	%eax, %ebp
	movl	(%esp), %eax
	addl	%edx, %eax
	movl	%eax, 8(%esp)
	movl	%ebp, %eax
	testb	%al, %al
	cmovne	8(%esp), %edx
L274:
	movl	%edx, %eax
	xorl	%edx, %edx
	subl	%ebx, %eax
	divl	%ecx
	imull	%eax, %esi
	movzwl	4(%esp), %eax
	sall	$16, %edx
	orl	%edx, %eax
	cmpl	%eax, %esi
	jbe	L295
	movl	(%esp), %ebx
	addl	%ebx, %eax
	jc	L295
	cmpl	%eax, %esi
	seta	%cl
	addl	%eax, %ebx
	testb	%cl, %cl
	cmovne	%ebx, %eax
	.p2align 4,,10
L295:
	subl	%esi, %eax
	movl	%edi, %ecx
	shrl	%cl, %eax
	xorl	%edx, %edx
L267:
	addl	$28, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L269:
	testl	%esi, %esi
	je	L381
	cmpl	$65535, %esi
	jbe	L382
	cmpl	$16777216, %esi
	sbbl	%ecx, %ecx
	andl	$-8, %ecx
	addl	$24, %ecx
L283:
	movl	%esi, %eax
	movl	$32, %edi
	shrl	%cl, %eax
	movzbl	___clz_tab(%eax), %eax
	addl	%ecx, %eax
	subl	%eax, %edi
	jne	L284
	movl	%edx, %eax
	movl	%esi, %ebx
	movzwl	%si, %ebp
	subl	%esi, %eax
	shrl	$16, %ebx
L285:
	xorl	%edx, %edx
	divl	%ebx
	imull	%ebp, %eax
	sall	$16, %edx
	movl	%eax, %ecx
	movl	4(%esp), %eax
	shrl	$16, %eax
	orl	%edx, %eax
	cmpl	%eax, %ecx
	jbe	L292
	addl	(%esp), %eax
	jc	L292
	cmpl	%eax, %ecx
	seta	%dl
	movl	%edx, %esi
	movl	(%esp), %edx
	addl	%eax, %edx
	movl	%edx, 8(%esp)
	movl	%esi, %edx
	testb	%dl, %dl
	cmovne	8(%esp), %eax
L292:
	subl	%ecx, %eax
	xorl	%edx, %edx
	divl	%ebx
	imull	%ebp, %eax
	sall	$16, %edx
	movl	%eax, %esi
	movzwl	4(%esp), %eax
	orl	%edx, %eax
	cmpl	%eax, %esi
	jbe	L295
	movl	(%esp), %ebx
	xorl	%edx, %edx
	addl	%ebx, %eax
	setc	%dl
	cmpl	%eax, %esi
	jbe	L295
	testl	%edx, %edx
	sete	%cl
	addl	%eax, %ebx
	testb	%cl, %cl
	cmovne	%ebx, %eax
	jmp	L295
	.p2align 4,,10
L268:
	movl	%ebx, 4(%esp)
	movl	%ebx, %eax
	cmpl	%edx, %edi
	ja	L267
	movl	%edx, %ebp
	movl	%edi, %ecx
	cmpl	$65535, %edi
	jbe	L383
	cmpl	$16777215, %edi
	jbe	L302
	shrl	$24, %ecx
	movl	$24, %eax
L301:
	movzbl	___clz_tab(%ecx), %ecx
	addl	%eax, %ecx
	movl	%ecx, (%esp)
	movl	$32, %ecx
	movl	%ecx, %eax
	subl	(%esp), %eax
	movl	%eax, 8(%esp)
	jne	L303
	cmpl	%edx, %edi
	jb	L328
	cmpl	%ebx, %esi
	jbe	L328
L304:
	movl	4(%esp), %eax
	addl	$28, %esp
	movl	%ebp, %edx
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L383:
	xorl	%eax, %eax
	cmpl	$255, %edi
	jbe	L301
	shrl	$8, %ecx
	movl	$8, %eax
	jmp	L301
	.p2align 4,,10
L380:
	cmpl	$255, %esi
	jbe	L271
	shrl	$8, %edi
	movl	$8, %ecx
	jmp	L271
	.p2align 4,,10
L381:
	ud2
	.p2align 4,,10
L284:
	movl	%edi, %ecx
	sall	%cl, %esi
	movl	%esi, %ebp
	movl	%edx, %esi
	sall	%cl, %esi
	movl	%eax, %ecx
	movl	%esi, (%esp)
	movl	%ebx, %esi
	shrl	%cl, %esi
	movl	(%esp), %ecx
	movl	%ebp, (%esp)
	orl	%esi, %ecx
	movl	%edx, %esi
	xorl	%edx, %edx
	movl	%ecx, 8(%esp)
	movl	%edi, %ecx
	sall	%cl, %ebx
	movl	%eax, %ecx
	shrl	%cl, %esi
	movl	%ebx, 4(%esp)
	movl	%ebp, %ebx
	movzwl	(%esp), %ebp
	movl	%esi, %eax
	shrl	$16, %ebx
	divl	%ebx
	imull	%ebp, %eax
	movl	%eax, %esi
	movl	%edx, %eax
	movl	8(%esp), %edx
	sall	$16, %eax
	shrl	$16, %edx
	orl	%edx, %eax
	cmpl	%eax, %esi
	jbe	L286
	movl	(%esp), %edx
	addl	%edx, %eax
	jc	L286
	cmpl	%eax, %esi
	seta	%cl
	addl	%eax, %edx
	testb	%cl, %cl
	cmovne	%edx, %eax
L286:
	subl	%esi, %eax
	xorl	%edx, %edx
	movzwl	8(%esp), %ecx
	divl	%ebx
	imull	%ebp, %eax
	sall	$16, %edx
	orl	%ecx, %edx
	movl	%eax, %esi
	cmpl	%edx, %eax
	jbe	L289
	movl	(%esp), %eax
	xorl	%ecx, %ecx
	addl	%eax, %edx
	setc	%cl
	cmpl	%edx, %esi
	jbe	L289
	testl	%ecx, %ecx
	sete	%cl
	addl	%edx, %eax
	testb	%cl, %cl
	cmovne	%eax, %edx
L289:
	movl	%edx, %eax
	subl	%esi, %eax
	jmp	L285
	.p2align 4,,10
L303:
	movl	8(%esp), %ebp
	movl	%esi, %eax
	movl	%ebp, %ecx
	sall	%cl, %edi
	movzbl	(%esp), %ecx
	shrl	%cl, %eax
	movl	%ebp, %ecx
	orl	%eax, %edi
	movl	%ebp, %eax
	sall	%cl, %esi
	movl	%edx, %ebp
	movl	%eax, %ecx
	movl	%esi, 4(%esp)
	movl	%eax, %esi
	sall	%cl, %ebp
	movzbl	(%esp), %ecx
	movl	%ebx, %eax
	shrl	%cl, %eax
	movl	%esi, %ecx
	movl	%edi, %esi
	sall	%cl, %ebx
	movzbl	(%esp), %ecx
	orl	%eax, %ebp
	movl	%edx, %eax
	shrl	$16, %esi
	xorl	%edx, %edx
	movl	%ebx, 12(%esp)
	movzwl	%di, %ebx
	movl	%esi, 16(%esp)
	shrl	%cl, %eax
	movl	%ebx, %ecx
	divl	%esi
	sall	$16, %edx
	imull	%eax, %ecx
	movl	%eax, %esi
	movl	%edx, 20(%esp)
	movl	%ebp, %edx
	shrl	$16, %edx
	orl	20(%esp), %edx
	cmpl	%edx, %ecx
	jbe	L308
	addl	%edi, %edx
	jc	L329
	cmpl	%edx, %ecx
	ja	L311
L329:
	leal	-1(%eax), %esi
L308:
	movl	%edx, %eax
	xorl	%edx, %edx
	movzwl	%bp, %ebp
	subl	%ecx, %eax
	divl	16(%esp)
	imull	%eax, %ebx
	sall	$16, %edx
	movl	%eax, %ecx
	orl	%edx, %ebp
	cmpl	%ebp, %ebx
	jbe	L313
	xorl	%edx, %edx
	addl	%edi, %ebp
	setc	%dl
	cmpl	%ebp, %ebx
	jbe	L330
	testl	%edx, %edx
	je	L316
L330:
	leal	-1(%eax), %ecx
L313:
	movl	%esi, %eax
	movl	4(%esp), %esi
	subl	%ebx, %ebp
	sall	$16, %eax
	orl	%ecx, %eax
	movzwl	%ax, %edx
	shrl	$16, %eax
	movzwl	%si, %ebx
	movl	%edx, %ecx
	shrl	$16, %esi
	imull	%ebx, %ecx
	imull	%esi, %edx
	imull	%eax, %ebx
	imull	%esi, %eax
	movl	%ecx, %esi
	movzwl	%cx, %ecx
	shrl	$16, %esi
	addl	%ebx, %edx
	addl	%esi, %edx
	leal	65536(%eax), %esi
	cmpl	%edx, %ebx
	movl	%edx, %ebx
	cmova	%esi, %eax
	shrl	$16, %ebx
	sall	$16, %edx
	addl	%eax, %ebx
	addl	%ecx, %edx
	cmpl	%ebx, %ebp
	jb	L319
	jne	L320
	cmpl	%edx, 12(%esp)
	jnb	L320
L319:
	movl	4(%esp), %esi
	xorl	%eax, %eax
	cmpl	%esi, %edx
	setb	%al
	subl	%esi, %edx
	addl	%edi, %eax
	subl	%eax, %ebx
L320:
	movl	12(%esp), %eax
	movzbl	(%esp), %ecx
	movl	8(%esp), %edi
	movl	%eax, %esi
	subl	%edx, %esi
	movl	%ebp, %edx
	sbbl	%ebx, %edx
	movl	%esi, %ebx
	addl	$28, %esp
	movl	%edx, %eax
	sall	%cl, %eax
	movl	%edi, %ecx
	shrl	%cl, %ebx
	shrl	%cl, %edx
	orl	%ebx, %eax
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
L382:
	cmpl	$256, %esi
	movl	$8, %eax
	cmovnb	%eax, %ecx
	jmp	L283
	.p2align 4,,10
L302:
	shrl	$16, %ecx
	movl	$16, %eax
	jmp	L301
	.p2align 4,,10
L272:
	shrl	$16, %edi
	movl	$16, %ecx
	jmp	L271
	.p2align 4,,10
L328:
	xorl	%ecx, %ecx
	cmpl	%esi, %ebx
	movl	%ebx, %eax
	setb	%cl
	subl	%edi, %edx
	subl	%esi, %eax
	subl	%ecx, %edx
	movl	%eax, 4(%esp)
	movl	%edx, %ebp
	jmp	L304
	.p2align 4,,10
L311:
	subl	$2, %esi
	addl	%edi, %edx
	jmp	L308
	.p2align 4,,10
L316:
	subl	$2, %ecx
	addl	%edi, %ebp
	jmp	L313
	.p2align 4,,15
	.globl	___udivdi3
	.def	___udivdi3;	.scl	2;	.type	32;	.endef
___udivdi3:
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$28, %esp
	movl	48(%esp), %esi
	movl	56(%esp), %ebx
	movl	60(%esp), %edi
	movl	52(%esp), %edx
	movl	%esi, 4(%esp)
	movl	%ebx, 8(%esp)
	testl	%edi, %edi
	jne	L385
	movl	%edi, %ecx
	cmpl	%edx, %ebx
	jbe	L386
	movl	%edx, %eax
	movl	%ebx, %edi
	cmpl	$65535, %ebx
	jbe	L489
	cmpl	$16777215, %ebx
	jbe	L389
	shrl	$24, %edi
	movl	$24, %ecx
L388:
	movzbl	___clz_tab(%edi), %ebp
	movl	$32, %edi
	addl	%ecx, %ebp
	subl	%ebp, %edi
	je	L390
	movl	%edi, %ecx
	movl	%edx, %eax
	movl	%esi, %edx
	sall	%cl, %ebx
	sall	%cl, %eax
	movl	%ebp, %ecx
	shrl	%cl, %edx
	movl	%edi, %ecx
	movl	%ebx, 8(%esp)
	sall	%cl, %esi
	orl	%edx, %eax
	movl	%esi, 4(%esp)
L390:
	movl	8(%esp), %esi
	xorl	%edx, %edx
	movl	4(%esp), %ebp
	movl	%esi, %ebx
	shrl	$16, %ebx
	shrl	$16, %ebp
	movl	%ebx, %edi
	movl	%ebx, 12(%esp)
	movl	%esi, %ebx
	movzwl	%si, %esi
	divl	%edi
	movl	%esi, %edi
	imull	%eax, %edi
	sall	$16, %edx
	movl	%eax, %ecx
	orl	%ebp, %edx
	cmpl	%edx, %edi
	jbe	L391
	addl	%ebx, %edx
	setc	%bl
	movzbl	%bl, %ebx
	cmpl	%edx, %edi
	jbe	L448
	testl	%ebx, %ebx
	je	L394
L448:
	leal	-1(%eax), %ecx
L391:
	movl	%edx, %eax
	xorl	%edx, %edx
	movzwl	4(%esp), %ebp
	subl	%edi, %eax
	divl	12(%esp)
	imull	%eax, %esi
	sall	$16, %edx
	movl	%eax, %ebx
	orl	%ebp, %edx
	cmpl	%edx, %esi
	jbe	L396
	xorl	%ebx, %ebx
	addl	8(%esp), %edx
	setc	%bl
	cmpl	%edx, %esi
	jbe	L449
	testl	%ebx, %ebx
	leal	-2(%eax), %ebx
	sete	%dl
	testb	%dl, %dl
	jne	L396
L449:
	leal	-1(%eax), %ebx
L396:
	movl	%ecx, %eax
	addl	$28, %esp
	xorl	%esi, %esi
	sall	$16, %eax
	movl	%esi, %edx
	orl	%ebx, %eax
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L386:
	testl	%ebx, %ebx
	je	L490
	cmpl	$65535, %ebx
	jbe	L491
	cmpl	$16777216, %ebx
	sbbl	%ecx, %ecx
	andl	$-8, %ecx
	addl	$24, %ecx
L404:
	movl	%ebx, %eax
	shrl	%cl, %eax
	movzbl	___clz_tab(%eax), %ebp
	movl	$32, %eax
	addl	%ecx, %ebp
	subl	%ebp, %eax
	jne	L405
	movl	%ebx, %esi
	movl	%edx, %eax
	shrl	$16, %esi
	subl	%ebx, %eax
	movl	%esi, 12(%esp)
	movzwl	%bx, %esi
	movl	%esi, 16(%esp)
	movl	$1, %esi
L406:
	xorl	%edx, %edx
	movl	16(%esp), %ecx
	divl	12(%esp)
	movl	4(%esp), %edi
	shrl	$16, %edi
	imull	%eax, %ecx
	sall	$16, %edx
	movl	%eax, %ebx
	orl	%edi, %edx
	cmpl	%edx, %ecx
	jbe	L417
	addl	8(%esp), %edx
	jc	L452
	cmpl	%edx, %ecx
	ja	L420
L452:
	leal	-1(%eax), %ebx
L417:
	movl	%edx, %eax
	xorl	%edx, %edx
	movzwl	4(%esp), %ebp
	subl	%ecx, %eax
	movl	16(%esp), %ecx
	divl	12(%esp)
	imull	%eax, %ecx
	sall	$16, %edx
	movl	%eax, %edi
	orl	%ebp, %edx
	cmpl	%edx, %ecx
	jbe	L422
	addl	8(%esp), %edx
	jc	L453
	leal	-2(%eax), %edi
	cmpl	%edx, %ecx
	seta	%dl
	testb	%dl, %dl
	jne	L422
L453:
	leal	-1(%eax), %edi
L422:
	movl	%ebx, %eax
	addl	$28, %esp
	movl	%esi, %edx
	sall	$16, %eax
	popl	%ebx
	orl	%edi, %eax
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L385:
	cmpl	%edx, %edi
	ja	L446
	movl	%edi, %eax
	cmpl	$65535, %edi
	jbe	L492
	cmpl	$16777215, %edi
	jbe	L429
	shrl	$24, %eax
	movl	$24, %ecx
L428:
	movzbl	___clz_tab(%eax), %eax
	addl	%ecx, %eax
	movl	$32, %ecx
	subl	%eax, %ecx
	movl	%ecx, 4(%esp)
	jne	L430
	cmpl	%edx, %edi
	setb	%al
	cmpl	%esi, %ebx
	setbe	%cl
	addl	$28, %esp
	xorl	%esi, %esi
	orl	%ecx, %eax
	popl	%ebx
	movl	%esi, %edx
	movzbl	%al, %eax
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L446:
	addl	$28, %esp
	xorl	%esi, %esi
	xorl	%eax, %eax
	popl	%ebx
	movl	%esi, %edx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L492:
	xorl	%ecx, %ecx
	cmpl	$255, %edi
	jbe	L428
	shrl	$8, %eax
	movl	$8, %ecx
	jmp	L428
	.p2align 4,,10
L489:
	cmpl	$255, %ebx
	jbe	L388
	shrl	$8, %edi
	movl	$8, %ecx
	jmp	L388
	.p2align 4,,10
L490:
	ud2
	.p2align 4,,10
L430:
	movl	4(%esp), %ebp
	movl	%ebp, %ecx
	sall	%cl, %edi
	movl	%eax, %ecx
	movl	%edi, 8(%esp)
	movl	%ebx, %edi
	shrl	%cl, %edi
	movl	%edi, %ecx
	movl	8(%esp), %edi
	orl	%ecx, %edi
	movl	%ebp, %ecx
	movl	%esi, %ebp
	sall	%cl, %ebx
	movl	%ebx, 16(%esp)
	movl	%edx, %ebx
	sall	%cl, %ebx
	movl	%eax, %ecx
	shrl	%cl, %ebp
	movl	%eax, %ecx
	orl	%ebp, %ebx
	shrl	%cl, %edx
	movzwl	%di, %ebp
	movl	%ebx, 8(%esp)
	movl	%edi, %ebx
	movl	%edx, %eax
	xorl	%edx, %edx
	shrl	$16, %ebx
	divl	%ebx
	movl	%ebx, 20(%esp)
	movl	8(%esp), %ebx
	shrl	$16, %ebx
	movl	%eax, %ecx
	sall	$16, %edx
	movl	%eax, 12(%esp)
	imull	%ebp, %ecx
	orl	%ebx, %edx
	cmpl	%edx, %ecx
	jbe	L431
	xorl	%ebx, %ebx
	addl	%edi, %edx
	setc	%bl
	cmpl	%edx, %ecx
	jbe	L454
	testl	%ebx, %ebx
	je	L434
L454:
	subl	$1, %eax
	movl	%eax, 12(%esp)
L431:
	movl	%edx, %eax
	xorl	%edx, %edx
	movzwl	8(%esp), %ebx
	subl	%ecx, %eax
	divl	20(%esp)
	imull	%eax, %ebp
	sall	$16, %edx
	movl	%eax, %ecx
	orl	%ebx, %edx
	cmpl	%edx, %ebp
	jbe	L436
	addl	%edi, %edx
	jc	L455
	cmpl	%edx, %ebp
	ja	L439
L455:
	leal	-1(%eax), %ecx
L436:
	movl	12(%esp), %eax
	subl	%ebp, %edx
	movl	16(%esp), %ebx
	movl	%edx, 8(%esp)
	sall	$16, %eax
	orl	%ecx, %eax
	movzwl	%bx, %ebp
	shrl	$16, %ebx
	movl	%eax, %edi
	movzwl	%ax, %ecx
	shrl	$16, %edi
	movl	%edi, %edx
	movl	%ecx, %edi
	imull	%ebp, %edi
	imull	%ebx, %ecx
	imull	%edx, %ebp
	imull	%ebx, %edx
	movl	%edi, %ebx
	shrl	$16, %ebx
	addl	%ebp, %ecx
	addl	%ecx, %ebx
	leal	65536(%edx), %ecx
	cmpl	%ebx, %ebp
	movl	8(%esp), %ebp
	cmova	%ecx, %edx
	movl	%ebx, %ecx
	shrl	$16, %ecx
	addl	%ecx, %edx
	cmpl	%edx, %ebp
	jb	L442
	movzbl	4(%esp), %ecx
	sall	$16, %ebx
	movzwl	%di, %edi
	addl	%edi, %ebx
	sall	%cl, %esi
	cmpl	%ebx, %esi
	jnb	L488
	cmpl	%edx, %ebp
	jne	L488
L442:
	subl	$1, %eax
L488:
	addl	$28, %esp
	xorl	%esi, %esi
	popl	%ebx
	movl	%esi, %edx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
	.p2align 4,,10
L405:
	movl	%eax, %ecx
	movl	%edx, %edi
	sall	%cl, %edi
	sall	%cl, %ebx
	movl	%ebp, %ecx
	movl	%edi, 4(%esp)
	movl	%esi, %edi
	shrl	%cl, %edi
	movl	%ebx, 8(%esp)
	movl	%edi, %ecx
	movl	4(%esp), %edi
	orl	%ecx, %edi
	movl	%eax, %ecx
	sall	%cl, %esi
	movl	%ebp, %ecx
	movl	%esi, 4(%esp)
	movl	%ebx, %esi
	shrl	%cl, %edx
	movl	%edi, %ecx
	shrl	$16, %ebx
	movzwl	%si, %eax
	shrl	$16, %ecx
	movl	%eax, 16(%esp)
	movl	%edx, %eax
	xorl	%edx, %edx
	divl	%ebx
	movl	%ebx, 12(%esp)
	movzwl	%si, %ebx
	imull	%eax, %ebx
	sall	$16, %edx
	movl	%eax, %ebp
	orl	%ecx, %edx
	cmpl	%edx, %ebx
	jbe	L407
	addl	%esi, %edx
	jc	L450
	cmpl	%edx, %ebx
	ja	L410
L450:
	leal	-1(%eax), %ebp
L407:
	movl	%edx, %eax
	xorl	%edx, %edx
	subl	%ebx, %eax
	movl	16(%esp), %ebx
	divl	12(%esp)
	movl	%edx, %esi
	imull	%eax, %ebx
	movzwl	%di, %edx
	sall	$16, %esi
	movl	%eax, %ecx
	orl	%esi, %edx
	cmpl	%edx, %ebx
	jbe	L412
	movl	%edx, %edi
	xorl	%edx, %edx
	addl	8(%esp), %edi
	setc	%dl
	movl	%edx, %esi
	movl	%edi, %edx
	cmpl	%edi, %ebx
	jbe	L451
	testl	%esi, %esi
	je	L415
L451:
	leal	-1(%eax), %ecx
L412:
	movl	%ebp, %esi
	movl	%edx, %eax
	sall	$16, %esi
	subl	%ebx, %eax
	orl	%ecx, %esi
	jmp	L406
L491:
	cmpl	$256, %ebx
	movl	$8, %eax
	cmovnb	%eax, %ecx
	jmp	L404
	.p2align 4,,10
L389:
	shrl	$16, %edi
	movl	$16, %ecx
	jmp	L388
	.p2align 4,,10
L429:
	shrl	$16, %eax
	movl	$16, %ecx
	jmp	L428
	.p2align 4,,10
L420:
	subl	$2, %ebx
	addl	8(%esp), %edx
	jmp	L417
	.p2align 4,,10
L394:
	subl	$2, %ecx
	addl	8(%esp), %edx
	jmp	L391
	.p2align 4,,10
L434:
	subl	$2, 12(%esp)
	addl	%edi, %edx
	jmp	L431
	.p2align 4,,10
L410:
	subl	$2, %ebp
	addl	8(%esp), %edx
	jmp	L407
	.p2align 4,,10
L439:
	subl	$2, %ecx
	addl	%edi, %edx
	jmp	L436
	.p2align 4,,10
L415:
	subl	$2, %ecx
	addl	8(%esp), %edx
	jmp	L412
	.p2align 4,,15
	.globl	___fixunssfsi
	.def	___fixunssfsi;	.scl	2;	.type	32;	.endef
___fixunssfsi:
	subl	$4, %esp
	flds	LC0
	flds	8(%esp)
	fcomi	%st(1), %st
	jnb	L500
	fstp	%st(1)
	fisttpl	(%esp)
	movl	(%esp), %eax
	addl	$4, %esp
	ret
	.p2align 4,,10
L500:
	fsubp	%st, %st(1)
	fisttpl	(%esp)
	movl	(%esp), %eax
	addl	$4, %esp
	addl	$-2147483648, %eax
	ret
	.section .rdata,"dr"
	.align 4
LC0:
	.long	1325400064
	.ident	"GCC: (x86_64-posix-seh-rev0, Built by MinGW-W64 project) 8.1.0"
