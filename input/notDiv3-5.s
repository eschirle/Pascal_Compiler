	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// for (i := 1 to 100)
	pushq	$1
	popq	%rax
	movq	%rax, (i)
	jmp	lab043
lab042:
// ((3 % i) > 0)
	pushq	$3
	pushq	(i)
	popq	%rbx
	popq	%rax
	movq	%rbx, -8(%rbp)
	movq	%rax, -4(%rbp)
	movl	-8(%rbp), %eax
	cltd
	idivl	-4(%rbp)
	movl	%edx, %eax
	pushq	%rax
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rax, %rbx
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab044
	jmp	lab045
lab044:
// ((5 % i) > 0)
	pushq	$5
	pushq	(i)
	popq	%rbx
	popq	%rax
	movq	%rbx, -8(%rbp)
	movq	%rax, -4(%rbp)
	movl	-8(%rbp), %eax
	cltd
	idivl	-4(%rbp)
	movl	%edx, %eax
	pushq	%rax
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rax, %rbx
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab047
	jmp	lab048
lab047:
// print (i);
	pushq	(i)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab049
lab048:
// dummy := 0;
	pushq	$0
	popq	%rax
	movq	%rax, (dummy)
lab049:
	jmp	lab046
lab045:
// dummy := i;
	pushq	(i)
	popq	%rax
	movq	%rax, (dummy)
lab046:
	movq	(i), %rax
	addq	$1, %rax
	movq	%rax, (i)
lab043:
	pushq	$100
	popq	%rbx
	movq	(i), %rax
	cmpq	%rbx, %rax
	jle	lab042
	popq	%rbp
	ret

// footer
	.section .rodata
	.output:
	.string "%d\n"

	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

	.globl	dummy
	.data
	.align	8
	.size	dummy, 8
dummy:
	.quad	0

