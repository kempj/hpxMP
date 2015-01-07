	#  /home/jeremy/openUH/lib/gcc-lib/x86_64-open64-linux/5.0/be::5.0

	#-----------------------------------------------------------
	# Compiling syncs.c (syncs.B)
	#-----------------------------------------------------------

	#-----------------------------------------------------------
	# Options:
	#-----------------------------------------------------------
	#  Target:Core, ISA:ISA_1, Endian:little, Pointer Size:64
	#  -O0	(Optimization level)
	#  -g2	(Debug level)
	#  -m2	(Report advisories)
	#-----------------------------------------------------------



	.text
	.align	2
	.section .text
	.p2align 5,,

	# Program Unit: sync_left
.globl	sync_left
	.type	sync_left, @function
sync_left:	# 0x0
	# .frame	%rbp, 144, %rbp
	# _temp___save_expr0 = -44
	# _temp___vla_bound1 = -56
	# _temp___save_expr2 = -64
	# _temp___save_expr3 = -72
	# _temp___save_expr4 = -76
	# _temp___vla_bound5 = -88
	# _temp___save_expr6 = -92
	# _temp___vla_bound7 = -104
	# _temp___save_expr8 = -112
	# _temp___save_expr9 = -120
	# _temp___save_expr10 = -124
	# _temp___vla_bound11 = -136
	# _temp___save_expr12 = -140
	# neigh = -144
	# _temp_reserved_spill0 = -40
 #  38  // Thread synchronization for pipeline operation
 #  39  //---------------------------------------------------------------------
 #  40  void sync_left(int ldmx, int ldmy, int ldmz,
 #  41                 double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5])
 #  42  {
.LBB1_sync_left:
.LEH_pushbp_sync_left:
	pushq %rbp                    	# 
.LEH_movespbp_sync_left:
	movq %rsp,%rbp                	# 
.LEH_adjustsp_sync_left:
	addq $-144,%rsp               	# 
	movl %edi,-32(%rbp)           	# ldmx
	movl %esi,-24(%rbp)           	# ldmy
	movl %edx,-16(%rbp)           	# ldmz
	movq %rcx,-8(%rbp)            	# v
	movl -32(%rbp),%r11d          	# ldmx
	movl %r11d,%r10d              	# 
	sarl $31,%r10d                	# 
	movl $1,%eax                  	# 
	andl %r10d,%eax               	# 
	addl %eax,%r11d               	# 
	sarl $1,%r11d                 	# 
	addl %r11d,%r11d              	# 
	addl $1,%r11d                 	# 
	movl %r11d,-44(%rbp)          	# _temp___save_expr0
	movslq -44(%rbp),%r9          	# _temp___save_expr0
	addq $-1,%r9                  	# 
	movq %r9,-56(%rbp)            	# _temp___vla_bound1
	movslq -44(%rbp),%rcx         	# _temp___save_expr0
	movq $40,%rdx                 	# 
	imulq %rcx,%rdx               	# 
	movq %rdx,-64(%rbp)           	# _temp___save_expr2
	movq -64(%rbp),%r8            	# _temp___save_expr2
	movq %r8,-64(%rbp)            	# _temp___save_expr2
	movslq -44(%rbp),%rsi         	# _temp___save_expr0
	movq $40,%rdi                 	# 
	imulq %rsi,%rdi               	# 
	movq %rdi,-72(%rbp)           	# _temp___save_expr3
	movl -24(%rbp),%r10d          	# ldmy
	movl %r10d,%eax               	# 
	sarl $31,%eax                 	# 
	movl $1,%r11d                 	# 
	andl %eax,%r11d               	# 
	addl %r11d,%r10d              	# 
	sarl $1,%r10d                 	# 
	addl %r10d,%r10d              	# 
	addl $1,%r10d                 	# 
	movl %r10d,-76(%rbp)          	# _temp___save_expr4
	movslq -76(%rbp),%r9          	# _temp___save_expr4
	addq $-1,%r9                  	# 
	movq %r9,-88(%rbp)            	# _temp___vla_bound5
	movl -32(%rbp),%ecx           	# ldmx
	movl %ecx,%edx                	# 
	sarl $31,%edx                 	# 
	movl $1,%r8d                  	# 
	andl %edx,%r8d                	# 
	addl %r8d,%ecx                	# 
	sarl $1,%ecx                  	# 
	addl %ecx,%ecx                	# 
	addl $1,%ecx                  	# 
	movl %ecx,-92(%rbp)           	# _temp___save_expr6
	movslq -92(%rbp),%rsi         	# _temp___save_expr6
	addq $-1,%rsi                 	# 
	movq %rsi,-104(%rbp)          	# _temp___vla_bound7
	movslq -92(%rbp),%rdi         	# _temp___save_expr6
	movq $40,%rax                 	# 
	imulq %rdi,%rax               	# 
	movq %rax,-112(%rbp)          	# _temp___save_expr8
	movq -112(%rbp),%r11          	# _temp___save_expr8
	movq %r11,-112(%rbp)          	# _temp___save_expr8
	movslq -92(%rbp),%r10         	# _temp___save_expr6
	movq $40,%r9                  	# 
	imulq %r10,%r9                	# 
	movq %r9,-120(%rbp)           	# _temp___save_expr9
	movl -24(%rbp),%edx           	# ldmy
	movl %edx,%r8d                	# 
	sarl $31,%r8d                 	# 
	movl $1,%ecx                  	# 
	andl %r8d,%ecx                	# 
	addl %ecx,%edx                	# 
	sarl $1,%edx                  	# 
	addl %edx,%edx                	# 
	addl $1,%edx                  	# 
	movl %edx,-124(%rbp)          	# _temp___save_expr10
	movslq -124(%rbp),%rsi        	# _temp___save_expr10
	addq $-1,%rsi                 	# 
	movq %rsi,-136(%rbp)          	# _temp___vla_bound11
	movl -16(%rbp),%edi           	# ldmz
	movl %edi,-140(%rbp)          	# _temp___save_expr12
 #  43    int neigh;
 #  44  
 #  45    if (iam > 0 && iam <= mthreadnum) {
	.globl	iam
	movq iam@GOTTPOFF(%rip),%rax  	# iam
	movl %fs:0(%rax),%eax         	# 
	testl %eax,%eax               	# 
	jle .L_0_2306                 	# 
.LBB2_sync_left:
	movq iam@GOTTPOFF(%rip),%rax  	# iam
	movl %fs:0(%rax),%eax         	# 
	.globl	mthreadnum
	movq mthreadnum@GOTTPOFF(%rip),%rdi	# mthreadnum
	movl %fs:0(%rdi),%edi         	# 
	cmpl %edi,%eax                	# 
	jg .L_0_2306                  	# 
.L_0_2562:
 #  46      neigh = iam - 1;
	movq iam@GOTTPOFF(%rip),%rsi  	# iam
	movl %fs:0(%rsi),%esi         	# 
	addl $-1,%esi                 	# 
	movl %esi,-144(%rbp)          	# neigh
 #  47      while (isync[neigh] == 0) {
	.globl	isync
	movabsq $(isync),%rdi         	# isync
	movslq -144(%rbp),%rax        	# neigh
	shlq $2,%rax                  	# 
	addq %rdi,%rax                	# 
	movl 0(%rax),%eax             	# 
	testl %eax,%eax               	# 
	jne .L_0_3074                 	# 
.L_0_2818:
.Lt_0_514:
 #  48        #pragma omp flush(isync)
	mfence                        	# 
.Lt_0_258:
	movabsq $(isync),%rdi         	# isync
	movslq -144(%rbp),%rax        	# neigh
	shlq $2,%rax                  	# 
	addq %rdi,%rax                	# 
	movl 0(%rax),%eax             	# 
	testl %eax,%eax               	# 
	je .L_0_2818                  	# 
.L_0_3074:
.Lt_0_770:
 #  49      }
 #  50      isync[neigh] = 0;
	movl $0,%eax                  	# 
	movabsq $(isync),%rsi         	# isync
	movslq -144(%rbp),%rdi        	# neigh
	shlq $2,%rdi                  	# 
	addq %rsi,%rdi                	# 
	movl %eax,0(%rdi)             	# 
 #  51      #pragma omp flush(isync,v)
	mfence                        	# 
.L_0_2306:
	leave                         	# 
	ret                           	# 
.LDWend_sync_left:
	.size sync_left, .LDWend_sync_left-sync_left
	.section .text
	.p2align 5,,

	# Program Unit: sync_right
.globl	sync_right
	.type	sync_right, @function
sync_right:	# 0x1dc
	# .frame	%rbp, 144, %rbp
	# _temp___save_expr13 = -44
	# _temp___vla_bound14 = -56
	# _temp___save_expr15 = -64
	# _temp___save_expr16 = -72
	# _temp___save_expr17 = -76
	# _temp___vla_bound18 = -88
	# _temp___save_expr19 = -92
	# _temp___vla_bound20 = -104
	# _temp___save_expr21 = -112
	# _temp___save_expr22 = -120
	# _temp___save_expr23 = -124
	# _temp___vla_bound24 = -136
	# _temp___save_expr25 = -140
	# _temp_reserved_spill1 = -40
 #  57  // Thread synchronization for pipeline operation
 #  58  //---------------------------------------------------------------------
 #  59  void sync_right(int ldmx, int ldmy, int ldmz,
 #  60                  double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5])
 #  61  {
.LBB1_sync_right:
.LEH_pushbp_sync_right:
	pushq %rbp                    	# 
.LEH_movespbp_sync_right:
	movq %rsp,%rbp                	# 
.LEH_adjustsp_sync_right:
	addq $-144,%rsp               	# 
	movl %edi,-32(%rbp)           	# ldmx
	movl %esi,-24(%rbp)           	# ldmy
	movl %edx,-16(%rbp)           	# ldmz
	movq %rcx,-8(%rbp)            	# v
	movl -32(%rbp),%edi           	# ldmx
	movl %edi,%r11d               	# 
	sarl $31,%r11d                	# 
	movl $1,%eax                  	# 
	andl %r11d,%eax               	# 
	addl %eax,%edi                	# 
	sarl $1,%edi                  	# 
	addl %edi,%edi                	# 
	addl $1,%edi                  	# 
	movl %edi,-44(%rbp)           	# _temp___save_expr13
	movslq -44(%rbp),%r10         	# _temp___save_expr13
	addq $-1,%r10                 	# 
	movq %r10,-56(%rbp)           	# _temp___vla_bound14
	movslq -44(%rbp),%r8          	# _temp___save_expr13
	movq $40,%rcx                 	# 
	imulq %r8,%rcx                	# 
	movq %rcx,-64(%rbp)           	# _temp___save_expr15
	movq -64(%rbp),%r9            	# _temp___save_expr15
	movq %r9,-64(%rbp)            	# _temp___save_expr15
	movslq -44(%rbp),%rdx         	# _temp___save_expr13
	movq $40,%rsi                 	# 
	imulq %rdx,%rsi               	# 
	movq %rsi,-72(%rbp)           	# _temp___save_expr16
	movl -24(%rbp),%r11d          	# ldmy
	movl %r11d,%eax               	# 
	sarl $31,%eax                 	# 
	movl $1,%edi                  	# 
	andl %eax,%edi                	# 
	addl %edi,%r11d               	# 
	sarl $1,%r11d                 	# 
	addl %r11d,%r11d              	# 
	addl $1,%r11d                 	# 
	movl %r11d,-76(%rbp)          	# _temp___save_expr17
	movslq -76(%rbp),%r10         	# _temp___save_expr17
	addq $-1,%r10                 	# 
	movq %r10,-88(%rbp)           	# _temp___vla_bound18
	movl -32(%rbp),%r8d           	# ldmx
	movl %r8d,%ecx                	# 
	sarl $31,%ecx                 	# 
	movl $1,%r9d                  	# 
	andl %ecx,%r9d                	# 
	addl %r9d,%r8d                	# 
	sarl $1,%r8d                  	# 
	addl %r8d,%r8d                	# 
	addl $1,%r8d                  	# 
	movl %r8d,-92(%rbp)           	# _temp___save_expr19
	movslq -92(%rbp),%rdx         	# _temp___save_expr19
	addq $-1,%rdx                 	# 
	movq %rdx,-104(%rbp)          	# _temp___vla_bound20
	movslq -92(%rbp),%rsi         	# _temp___save_expr19
	movq $40,%rax                 	# 
	imulq %rsi,%rax               	# 
	movq %rax,-112(%rbp)          	# _temp___save_expr21
	movq -112(%rbp),%rdi          	# _temp___save_expr21
	movq %rdi,-112(%rbp)          	# _temp___save_expr21
	movslq -92(%rbp),%r11         	# _temp___save_expr19
	movq $40,%r10                 	# 
	imulq %r11,%r10               	# 
	movq %r10,-120(%rbp)          	# _temp___save_expr22
	movl -24(%rbp),%ecx           	# ldmy
	movl %ecx,%r9d                	# 
	sarl $31,%r9d                 	# 
	movl $1,%r8d                  	# 
	andl %r9d,%r8d                	# 
	addl %r8d,%ecx                	# 
	sarl $1,%ecx                  	# 
	addl %ecx,%ecx                	# 
	addl $1,%ecx                  	# 
	movl %ecx,-124(%rbp)          	# _temp___save_expr23
	movslq -124(%rbp),%rdx        	# _temp___save_expr23
	addq $-1,%rdx                 	# 
	movq %rdx,-136(%rbp)          	# _temp___vla_bound24
	movl -16(%rbp),%esi           	# ldmz
	movl %esi,-140(%rbp)          	# _temp___save_expr25
 #  62    if (iam < mthreadnum) {
	movq iam@GOTTPOFF(%rip),%rax  	# iam
	movl %fs:0(%rax),%eax         	# 
	movq mthreadnum@GOTTPOFF(%rip),%rdi	# mthreadnum
	movl %fs:0(%rdi),%edi         	# 
	cmpl %edi,%eax                	# 
	jge .L_1_2050                 	# 
.LBB2_sync_right:
 #  63      #pragma omp flush(isync,v)
	mfence                        	# 
 #  64      while (isync[iam] == 1) {
	movabsq $(isync),%rdi         	# isync
	movq iam@GOTTPOFF(%rip),%rax  	# iam
	movslq %fs:0(%rax),%rax       	# 
	shlq $2,%rax                  	# 
	addq %rdi,%rax                	# 
	movl 0(%rax),%eax             	# 
	cmpl $1,%eax                  	# 
	jne .L_1_2562                 	# 
.L_1_2306:
.Lt_1_514:
 #  65        #pragma omp flush(isync)
	mfence                        	# 
.Lt_1_258:
	movabsq $(isync),%rdi         	# isync
	movq iam@GOTTPOFF(%rip),%rax  	# iam
	movslq %fs:0(%rax),%rax       	# 
	shlq $2,%rax                  	# 
	addq %rdi,%rax                	# 
	movl 0(%rax),%eax             	# 
	cmpl $1,%eax                  	# 
	je .L_1_2306                  	# 
.L_1_2562:
.Lt_1_770:
 #  66      }
 #  67      isync[iam] = 1;
	movl $1,%eax                  	# 
	movabsq $(isync),%rsi         	# isync
	movq iam@GOTTPOFF(%rip),%rdi  	# iam
	movslq %fs:0(%rdi),%rdi       	# 
	shlq $2,%rdi                  	# 
	addq %rsi,%rdi                	# 
	movl %eax,0(%rdi)             	# 
 #  68      #pragma omp flush(isync)
	mfence                        	# 
.L_1_2050:
	leave                         	# 
	ret                           	# 
.LDWend_sync_right:
	.size sync_right, .LDWend_sync_right-sync_right
	.section .text
	.align	4

	.section .debug_info, "",@progbits
	.byte	0xe3, 0x02, 0x00, 0x00, 0x02, 0x00
	.4byte	.debug_abbrev
	.4byte	0x79730108, 0x2e73636e, 0x34540063, 0x282e3032
	.4byte	0x656e6f6e, 0x682f3a29, 0x2f656d6f, 0x6572656a
	.4byte	0x632f796d, 0x2f65646f, 0x65736572, 0x68637261
	.4byte	0x7870682f, 0x6f2f504d, 0x742d706d, 0x73747365
	.4byte	0x554e532f, 0x42504e5f, 0x302e312d, 0x4e2f332e
	.4byte	0x2e334250, 0x4d4f2d33, 0x2f432d50, 0x6f00554c
	.byte	0x70, 0x65, 0x6e, 0x63, 0x63, 0x20, 0x35, 0x2e
	.byte	0x30, 0x00, 0x01, 0x00
	.quad	.LBB1_sync_left
	.quad	.LDWend_sync_right
	.4byte	.debug_line
	.4byte	0x746e6902, 0x02040500, 0x62756f64, 0x0400656c
	.4byte	0x2a010308, 0x00000091, 0x0000ab01, 0x04000400
	.4byte	0x6f6c0200, 0x7520676e, 0x6769736e, 0x2064656e
	.4byte	0x00746e69, 0x6c020807, 0x20676e6f, 0x00746e69
	.4byte	0x01050805, 0x00009b2a, 0x01050100, 0x0000cc2a
	.4byte	0xd4060100, 0x08000000, 0x2b010500, 0x0000009b
	.4byte	0x2b010501, 0x000000e3, 0x00eb0601, 0x00080000
	.4byte	0x8a330103, 0x01000000, 0x0000010a, 0x000c0004
	.4byte	0x9b3d0105, 0x01000000, 0x0a3d0105, 0x01000001
	.4byte	0x00011206, 0x05000800, 0x009b4401, 0x05010000
	.4byte	0x01214401, 0x06010000, 0x00000129, 0x01030008
	.4byte	0x00009b44, 0x01480100, 0x00040000, 0x0103000c
	.4byte	0x00013844, 0x01580100, 0x00040000, 0x0103000c
	.4byte	0x00014844, 0x01680100, 0x00040000, 0x0103000b
	.4byte	0x00009b44, 0x01780100, 0x00040000, 0x0103000b
	.4byte	0x00009144, 0x01880100, 0x00040000, 0x0103000c
	.4byte	0x00017844, 0x01980100, 0x00040000, 0x0103000c
	.4byte	0x00018844, 0x01a80100, 0x00040000, 0x0103000b
	.4byte	0x00009b44, 0x01b80100, 0x00040000, 0x01030004
	.4byte	0x0001a844, 0x01c80100, 0x00040000, 0x0103000c
	.4byte	0x0001b844, 0x01d80100, 0x00040000, 0x0103000b
	.4byte	0x00017844, 0x01e80100, 0x00040000, 0x01070004
	.4byte	0x020a0444, 0x01080000, 0x6c616644, 0x00006573
	.4byte	0x08000000, 0x72744401, 0x01006575, 0x00000000
	.4byte	0x6c440109, 0x6369676f, 0xe8006c61, 0x0a000001
	.4byte	0x79732a01, 0x6c5f636e, 0x00746665, 0x56010101
	.quad	.LBB1_sync_left
	.quad	.LDWend_sync_left
	.4byte	0x00000289, 0x6c2a010b, 0x00786d64, 0x0000008a
	.4byte	0x0b609102, 0x646c2a01, 0x8a00796d, 0x02000000
	.4byte	0x010b6891, 0x6d646c2a, 0x008a007a, 0x91020000
	.4byte	0x2a010b70, 0x00dc0076, 0x91020000, 0x2b010c78
	.4byte	0x6769656e, 0x008a0068, 0x91030000, 0x0d007ef0
	.4byte	0x79733d01, 0x725f636e, 0x74686769, 0x01010100
	.byte	0x56
	.quad	.LBB1_sync_right
	.quad	.LDWend_sync_right
	.4byte	0x6c3d010b, 0x00786d64, 0x0000008a, 0x0b609102
	.4byte	0x646c3d01, 0x8a00796d, 0x02000000, 0x010b6891
	.4byte	0x6d646c3d, 0x008a007a, 0x91020000, 0x3d010b70
	.byte	0x76, 0x00, 0x1a, 0x01, 0x00, 0x00, 0x02, 0x91
	.byte	0x78, 0x00, 0x00, 0x00

	.section .debug_frame, "",@progbits
.LCIE:
	.4byte 0x10
	.4byte 0xffffffff
	.byte	0x01, 0x00, 0x01, 0x78, 0x10, 0x0c, 0x07, 0x08
	.byte	0x90, 0x01, 0x00, 0x00
	.4byte 0x28
	.4byte	.LCIE
	.quad	.LBB1_sync_left
	.quad	.LDWend_sync_left - .LBB1_sync_left
	.byte	0x04
	.4byte	.LEH_movespbp_sync_left - .LBB1_sync_left
	.byte	0x0e, 0x10, 0x86, 0x02, 0x04
	.4byte	.LEH_adjustsp_sync_left - .LEH_movespbp_sync_left
	.byte	0x0d, 0x06, 0x00, 0x00, 0x00, 0x00
	.4byte 0x28
	.4byte	.LCIE
	.quad	.LBB1_sync_right
	.quad	.LDWend_sync_right - .LBB1_sync_right
	.byte	0x04
	.4byte	.LEH_movespbp_sync_right - .LBB1_sync_right
	.byte	0x0e, 0x10, 0x86, 0x02, 0x04
	.4byte	.LEH_adjustsp_sync_right - .LEH_movespbp_sync_right
	.byte	0x0d, 0x06, 0x00, 0x00, 0x00, 0x00

	.section .debug_aranges, "",@progbits
	.byte	0x2c, 0x00, 0x00, 0x00, 0x02, 0x00
	.4byte	.debug_info
	.byte	0x08, 0x00, 0x00, 0x00, 0x00, 0x00
	.quad	.LBB1_sync_left
	.quad	.LDWend_sync_right - .LBB1_sync_left
	.4byte	0x00000000, 0x00000000, 0x00000000, 0x00000000

	.section .debug_pubnames, "",@progbits
	.byte	0x2b, 0x00, 0x00, 0x00, 0x02, 0x00
	.4byte	.debug_info
	.4byte	0x000002e7, 0x00000219, 0x636e7973, 0x66656c5f
	.4byte	0x02890074, 0x79730000, 0x725f636e, 0x74686769
	.byte	0x00, 0x00, 0x00, 0x00, 0x00

	.section .debug_typenames, "",@progbits
	.byte	0x2d, 0x00, 0x00, 0x00, 0x02, 0x00
	.4byte	.debug_info
	.4byte	0x000002e7, 0x000001f0, 0x736c6166, 0x01fd0065
	.4byte	0x72740000, 0x0a006575, 0x6c000002, 0x6369676f
	.byte	0x61, 0x6c, 0x00, 0x00, 0x00, 0x00, 0x00

	.section .debug_varnames, "",@progbits
	.byte	0x18, 0x00, 0x00, 0x00, 0x02, 0x00
	.4byte	.debug_info
	.4byte	0x000002e7, 0x00000277, 0x6769656e, 0x00000068
	.byte	0x00, 0x00

	.section .eh_frame, "a",@progbits
.LEHCIE:
	.4byte	.LEHCIE_end - .LEHCIE_begin
.LEHCIE_begin:
	.4byte 0x0
	.byte	0x01, 0x00, 0x01, 0x78, 0x10, 0x0c, 0x07, 0x08
	.byte	0x90, 0x01
	.align 8
.LEHCIE_end:
	.4byte	.LFDE1_end - .LFDE1_begin
.LFDE1_begin:
	.4byte	.LFDE1_begin - .LEHCIE
	.quad	.LBB1_sync_left
	.quad	.LDWend_sync_left - .LBB1_sync_left
	.byte	0x04
	.4byte	.LEH_movespbp_sync_left - .LBB1_sync_left
	.byte	0x0e, 0x10, 0x86, 0x02, 0x04
	.4byte	.LEH_adjustsp_sync_left - .LEH_movespbp_sync_left
	.byte	0x0d, 0x06
	.align 8
.LFDE1_end:
	.4byte	.LFDE2_end - .LFDE2_begin
.LFDE2_begin:
	.4byte	.LFDE2_begin - .LEHCIE
	.quad	.LBB1_sync_right
	.quad	.LDWend_sync_right - .LBB1_sync_right
	.byte	0x04
	.4byte	.LEH_movespbp_sync_right - .LBB1_sync_right
	.byte	0x0e, 0x10, 0x86, 0x02, 0x04
	.4byte	.LEH_adjustsp_sync_right - .LEH_movespbp_sync_right
	.byte	0x0d, 0x06
	.align 8
.LFDE2_end:

	.section .debug_line, ""

	.section .debug_abbrev, "",@progbits
	.4byte	0x03011101, 0x25081b08, 0x420b1308, 0x1201110b
	.4byte	0x00061001, 0x00240200, 0x0b3e0803, 0x00000b0b
	.4byte	0x3a010103, 0x490b3b0b, 0x010c3c13, 0x04000013
	.4byte	0x0b220021, 0x00000b2f, 0x3a000105, 0x490b3b0b
	.4byte	0x000c3c13, 0x000f0600, 0x0b0b1349, 0x00000b33
	.4byte	0x3a010407, 0x0b0b3b0b, 0x0013010b, 0x00280800
	.4byte	0x0b3b0b3a, 0x061c0803, 0x16090000, 0x3b0b3a00
	.4byte	0x4908030b, 0x0a000013, 0x0b3a012e, 0x08030b3b
	.4byte	0x0c340c3f, 0x01110a40, 0x13010112, 0x050b0000
	.4byte	0x3b0b3a00, 0x4908030b, 0x000a0213, 0x00340c00
	.4byte	0x0b3b0b3a, 0x13490803, 0x00000a02, 0x3a012e0d
	.4byte	0x030b3b0b, 0x340c3f08, 0x110a400c, 0x00011201
	.byte	0x00, 0x00, 0x00
	.section	.note.GNU-stack,"",@progbits
	.ident	"#Open64 Compiler Version 5.0 : syncs.c compiled with : -g2 -WOPT:warn_uninit=on -O0 -TENV:mcmodel=medium -march=core -msse2 -msse3 -mno-3dnow -mno-sse4a -mno-ssse3 -mno-sse41 -mno-sse42 -mno-aes -mno-pclmul -mno-avx -mno-xop -mno-fma -mno-fma4 -m64"

