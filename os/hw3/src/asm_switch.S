	.text
	.global asm_thread_switch
asm_thread_switch:
	movl 4(%esp), %eax
	movl 8(%esp), %edx

	pushl %ebp
	pushl %ebx
	pushl %esi
	pushl %edi
	pushfl

	movl %esp, (%eax)
	movl %edx, %esp

	popfl
	popl %edi
	popl %esi
	popl %ebx
	popl %ebp

	ret
	