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
	jmp	lab036
lab035:
// print (i);
	pushq	(i)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	movq	(i), %rax
	addq	$1, %rax
	movq	%rax, (i)
lab036:
	pushq	$100
	popq	%rbx
	movq	(i), %rax
	cmpq	%rbx, %rax
	jle	lab035
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

