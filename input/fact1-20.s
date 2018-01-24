	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// n := 20
	pushq	$20
	popq	%rax
	movq	%rax, (n)
// for (i := 1 to n)
	pushq	$1
	popq	%rax
	movq	%rax, (i)
	jmp	lab038
lab037:
// print (fact(i));
	pushq	(i)
	call	fact
	addq	$8, %rsp
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	movq	(i), %rax
	addq	$1, %rax
	movq	%rax, (i)
lab038:
	pushq	(n)
	popq	%rbx
	movq	(i), %rax
	cmpq	%rbx, %rax
	jle	lab037
	popq	%rbp
	ret

	.text
	.globl	fact
	.type	fact, @function
fact:
	pushq	%rbp
	movq	%rsp, %rbp
// (n = 1)
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sete	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab039
	jmp	lab040
lab039:
// fact := n;
	pushq	16(%rbp)
	popq	%rax
	popq	%rbp
	ret
	jmp	lab041
lab040:
// fact := (n * fact((n - 1)));
	pushq	16(%rbp)
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	call	fact
	addq	$8, %rsp
	pushq	%rax
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rax
	popq	%rbp
	ret
lab041:
	popq	%rbp
	ret
// footer
	.section .rodata
	.output:
	.string "%d\n"

	.globl	n
	.data
	.align	8
	.size	n, 8
n:
	.quad	0

	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

