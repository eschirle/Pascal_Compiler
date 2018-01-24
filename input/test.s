	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// x := 5
	pushq	$5
	popq	%rax
	movq	%rax, (x)
// (1 > 0)
	pushq	$1
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rax, %rbx
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab001
	jmp	lab002
lab001:
// print (20000);
	pushq	$20000
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab003
lab002:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab003:
// while ((x > 0))
	jmp	lab005
lab004:
// print (x);
	pushq	(x)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// x := (x - 1);
	pushq	(x)
	pushq	$1
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (x)
lab005:
	pushq	(x)
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rax, %rbx
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab004
// print (1111111111);
	pushq	$1111111111
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// for (x := 0 to 20)
	pushq	$0
	popq	%rax
	movq	%rax, (x)
	jmp	lab007
lab006:
// print (x);
	pushq	(x)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	movq	(x), %rax
	addq	$1, %rax
	movq	%rax, (x)
lab007:
	pushq	$20
	popq	%rbx
	movq	(x), %rax
	cmpq	%rbx, %rax
	jle	lab006
// print (-3333333);
	pushq	$-3333333
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// print (sum(3sum(21)));
	pushq	$1
	pushq	$2
	call	sum
	addq	$16, %rsp
	pushq	%rax
	pushq	$3
	call	sum
	addq	$16, %rsp
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// print (sum(x100));
	pushq	$100
	pushq	(x)
	call	sum
	addq	$16, %rsp
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// print ((3 + -3));
	pushq	$3
	pushq	$-3
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	popq	%rbp
	ret

	.text
	.globl	sum
	.type	sum, @function
sum:
	pushq	%rbp
	movq	%rsp, %rbp
// sum := (a + b);
	pushq	16(%rbp)
	pushq	24(%rbp)
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	popq	%rbp
	ret
	popq	%rbp
	ret
// footer
	.section .rodata
	.output:
	.string "%d\n"

	.globl	x
	.data
	.align	8
	.size	x, 8
x:
	.quad	0

