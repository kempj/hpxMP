//  z_Linux_asm.s:  - microtasking routines specifically
//                    written for Intel platforms running Linux* OS

// <copyright>
//    Copyright (c) 1997-2014 Intel Corporation.  All Rights Reserved.
//
//    Redistribution and use in source and binary forms, with or without
//    modification, are permitted provided that the following conditions
//    are met:
//
//      * Redistributions of source code must retain the above copyright
//        notice, this list of conditions and the following disclaimer.
//      * Redistributions in binary form must reproduce the above copyright
//        notice, this list of conditions and the following disclaimer in the
//        documentation and/or other materials provided with the distribution.
//      * Neither the name of Intel Corporation nor the names of its
//        contributors may be used to endorse or promote products derived
//        from this software without specific prior written permission.
//
//    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// </copyright>
//------------------------------------------------------------------------

#define KMP_LABEL(x) .L_##x 

.macro ALIGN size
    .align 1<<(\size)
.endm
.macro DEBUG_INFO proc
    .cfi_endproc
    .align 16
    .type  \proc,@function
        .size  \proc,.-\proc
.endm
.macro PROC proc
    ALIGN  4
        .globl \proc
\proc:
    .cfi_startproc
.endm
.macro KMP_CFI_DEF_OFFSET sz
    .cfi_def_cfa_offset \sz
.endm
.macro KMP_CFI_OFFSET reg, sz
    .cfi_offset \reg,\sz
.endm
.macro KMP_CFI_REGISTER reg
    .cfi_def_cfa_register   \reg
.endm
.macro KMP_CFI_DEF reg, sz
    .cfi_def_cfa    \reg,\sz
.endm


        .data
        .comm .gomp_critical_user_,32,8
        .data
	ALIGN 8
        .global __kmp_unnamed_critical_addr
__kmp_unnamed_critical_addr:
        .8byte .gomp_critical_user_
        .type __kmp_unnamed_critical_addr,@object
        .size __kmp_unnamed_critical_addr,8

    .ident "Intel Corporation"
    .data
    ALIGN 4



//------------------------------------------------------------------------
//
// typedef void	(*microtask_t)( int *gtid, int *tid, ... );
//
// int
// __kmp_invoke_microtask( void (*pkfn) (int gtid, int tid, ...),
//		           int gtid, int tid,
//                         int argc, void *p_argv[] ) {
//    (*pkfn)( & gtid, & tid, argv[0], ... );
//    return 1;
// }
//
// note:
//	at call to pkfn must have %rsp 128-byte aligned for compiler
//
// parameters:
//      %rdi:  	pkfn
//	%esi:	gtid
//	%edx:	tid
//	%ecx:	argc
//	%r8:	p_argv
//
// locals:
//	__gtid:	gtid parm pushed on stack so can pass &gtid to pkfn
//	__tid:	tid parm pushed on stack so can pass &tid to pkfn
//
// reg temps:
//	%rax:	used all over the place
//	%rdx:	used in stack pointer alignment calculation
//	%r11:	used to traverse p_argv array
//	%rsi:	used as temporary for stack parameters
//		used as temporary for number of pkfn parms to push
//	%rbx:	used to hold pkfn address, and zero constant, callee-save
//
// return:	%eax 	(always 1/TRUE)
//

__gtid = -16
__tid = -24

// -- Begin __kmp_invoke_microtask
// mark_begin;
        .text
	PROC  __kmp_invoke_microtask

	pushq 	%rbp		// save base pointer
	KMP_CFI_DEF_OFFSET 16
	KMP_CFI_OFFSET rbp,-16
	movq 	%rsp,%rbp	// establish the base pointer for this routine.
	KMP_CFI_REGISTER rbp
	pushq 	%rbx		// %rbx is callee-saved register
	pushq	%rsi		// Put gtid on stack so can pass &tgid to pkfn
	pushq	%rdx		// Put tid on stack so can pass &tid to pkfn

	movq	%rcx, %rax	// Stack alignment calculation begins; argc -> %rax
	movq	$0, %rbx	// constant for cmovs later
	subq	$4, %rax	// subtract four args passed in registers to pkfn
	cmovsq	%rbx, %rax	// zero negative value in %rax <- max(0, argc-4)

	movq	%rax, %rsi	// save max(0, argc-4) -> %rsi for later
	shlq 	$3, %rax	// Number of bytes used on stack: max(0, argc-4)*8

	movq 	%rsp, %rdx	//
	subq 	%rax, %rdx	// %rsp-(max(0,argc-4)*8) -> %rdx --
				// without align, stack ptr would be this
	movq 	%rdx, %rax	// Save to %rax

	andq 	$0xFFFFFFFFFFFFFF80, %rax  // mask off lower 7 bits (128 bytes align)
	subq 	%rax, %rdx	// Amount to subtract from %rsp
	subq 	%rdx, %rsp	// Prepare the stack ptr --
				// now %rsp will align to 128-byte boundary at call site

				// setup pkfn parameter reg and stack
	movq	%rcx, %rax	// argc -> %rax
	cmpq	$0, %rsi
	je	KMP_LABEL(kmp_invoke_pass_parms)	// jump ahead if no parms to push
	shlq	$3, %rcx	// argc*8 -> %rcx
	movq 	%r8, %rdx	// p_argv -> %rdx
	addq	%rcx, %rdx	// &p_argv[argc] -> %rdx

	movq	%rsi, %rcx	// max (0, argc-4) -> %rcx

KMP_LABEL(kmp_invoke_push_parms):
	// push nth - 7th parms to pkfn on stack
	subq	$8, %rdx	// decrement p_argv pointer to previous parm
	movq	(%rdx), %rsi	// p_argv[%rcx-1] -> %rsi
	pushq	%rsi		// push p_argv[%rcx-1] onto stack (reverse order)
	subl	$1, %ecx

	jecxz   KMP_LABEL(kmp_invoke_pass_parms)  // stop when four p_argv[] parms left
	jmp	KMP_LABEL(kmp_invoke_push_parms)
	ALIGN 3
KMP_LABEL(kmp_invoke_pass_parms):	// put 1st - 6th parms to pkfn in registers.
				// order here is important to avoid trashing
				// registers used for both input and output parms!
	movq	%rdi, %rbx	// pkfn -> %rbx
	leaq	__gtid(%rbp), %rdi // &gtid -> %rdi (store 1st parm to pkfn)
	leaq	__tid(%rbp), %rsi  // &tid -> %rsi (store 2nd parm to pkfn)

	movq	%r8, %r11	// p_argv -> %r11

	cmpq	$4, %rax	// argc >= 4?
	cmovnsq	24(%r11), %r9	// p_argv[3] -> %r9 (store 6th parm to pkfn)

	cmpq	$3, %rax	// argc >= 3?
	cmovnsq	16(%r11), %r8	// p_argv[2] -> %r8 (store 5th parm to pkfn)

	cmpq	$2, %rax	// argc >= 2?
	cmovnsq	8(%r11), %rcx	// p_argv[1] -> %rcx (store 4th parm to pkfn)

	cmpq	$1, %rax	// argc >= 1?
	cmovnsq	(%r11), %rdx	// p_argv[0] -> %rdx (store 3rd parm to pkfn)

	call	*%rbx		// call (*pkfn)();
	movq	$1, %rax	// move 1 into return register;

	movq	-8(%rbp), %rbx	// restore %rbx	using %rbp since %rsp was modified
	movq 	%rbp, %rsp	// restore stack pointer
	popq 	%rbp		// restore frame pointer
	KMP_CFI_DEF rsp,8
	ret

	DEBUG_INFO __kmp_invoke_microtask
// -- End  __kmp_invoke_microtask
