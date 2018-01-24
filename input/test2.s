	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// (1 >= 1)
	pushq	$1
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	setge	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab008
	jmp	lab009
lab008:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab010
lab009:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab010:
// (1 <= 1)
	pushq	$1
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	setle	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab011
	jmp	lab012
lab011:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab013
lab012:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab013:
// (1 = 1)
	pushq	$1
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sete	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab014
	jmp	lab015
lab014:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab016
lab015:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab016:
// (1 <> 0)
	pushq	$1
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	setne	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab017
	jmp	lab018
lab017:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab019
lab018:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab019:
// (1 <> 1)
	pushq	$1
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	setne	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab020
	jmp	lab021
lab020:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab022
lab021:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab022:
// (1 < 0)
	pushq	$1
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab023
	jmp	lab024
lab023:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab025
lab024:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab025:
// (1 <= 0)
	pushq	$1
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	setle	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab026
	jmp	lab027
lab026:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab028
lab027:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab028:
// (1 >= 0)
	pushq	$1
	pushq	$0
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	setge	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab029
	jmp	lab030
lab029:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab031
lab030:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab031:
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
	jne	lab032
	jmp	lab033
lab032:
// print (0);
	pushq	$0
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	jmp	lab034
lab033:
// print (1);
	pushq	$1
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
lab034:
	popq	%rbp
	ret

// footer
	.section .rodata
	.output:
	.string "%d\n"

