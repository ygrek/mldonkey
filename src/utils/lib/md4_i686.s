	.text
       .align    4,0x90
	.globl   MD4Init
	.type	MD4Init,@function
MD4Init:
.L11:
        movl      4(%esp), %edx
        xorl      %eax, %eax
        movl      %eax, 20(%edx)
        movl      %eax, 16(%edx)
        movl      $1732584193, (%edx)
        movl      $-271733879, 4(%edx)
        movl      $-1732584194, 8(%edx)
        movl      $271733878, 12(%edx)
        ret
        .align    4,0x90


	.size	MD4Init,.-MD4Init
	.data

	.data
	.text


       .align    4,0x90



	.globl   MD4Update
	.type	MD4Update,@function
MD4Update:
.L21:
        pushl     %edi
        pushl     %esi
        pushl     %ebp
        pushl     %ebx
        subl      $20, %esp
        movl      40(%esp), %ebp
        movl      48(%esp), %ecx
        movl      16(%ebp), %edx
        movl      %edx, %eax
        shrl      $3, %eax
        andl      $63, %eax
        lea       (%ecx,%ecx), %ebx
        addl      %ebx, %ebx
        addl      %ebx, %ebx
        lea       (%edx,%ecx,8), %edx
        movl      %edx, 16(%ebp)
        cmpl      %ebx, %edx
        jae       .L23

.L22:
        movl      20(%ebp), %edx
        incl      %edx
        jmp       .L24

.L23:
        movl      20(%ebp), %edx

.L24:
        movl      48(%esp), %esi
        movl      %esi, %ebx
        shrl      $29, %ebx
        addl      %ebx, %edx
        movl      %edx, 20(%ebp)
        movl      %eax, %edx
        negl      %edx
        lea       64(%edx), %ebx
        movl      %edx, 12(%esp)
        cmpl      %ebx, %esi
        jae       .L26

.L25:
        xorl      %ebx, %ebx
        jmp       .L214

.L26:
        lea       24(%ebp,%eax), %edi
        movl      44(%esp), %esi
        movl      %ebx, %ecx
        rep
        movsb

.L27:
        movl      %ebp, (%esp)
        lea       24(%ebp), %eax
        movl      %eax, 4(%esp)
        call      MD4Transform

.L28:
        movl      12(%esp), %edx
        addl      $127, %edx
        movl      48(%esp), %eax
        cmpl      %edx, %eax
        jbe       .L213

.L29:
        lea       63(%ebx), %edi
        movl      44(%esp), %eax
        lea       (%ebx,%eax), %esi
        

.L210:
        movl      %ebp, (%esp)
        movl      %esi, 4(%esp)
        call      MD4Transform

.L211:
        addl      $64, %edi
        addl      $64, %esi
        addl      $64, %ebx
        movl      48(%esp), %eax
        cmpl      %edi, %eax
        ja        .L210

.L213:
        xorl      %eax, %eax

.L214:
        lea       24(%ebp,%eax), %edi
        movl      44(%esp), %eax
        lea       (%eax,%ebx), %esi
        movl      48(%esp), %eax
        subl      %ebx, %eax
        movl      %eax, %ecx
        rep
        movsb

.L215:
        addl      $20, %esp
        popl      %ebx
        popl      %ebp
        popl      %esi
        popl      %edi
        ret
        .align    4,0x90


	.size	MD4Update,.-MD4Update
	.data

	.data
	.type	PADDING,@object
PADDING:
	.byte 128	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.byte 0	
	.size	PADDING,64
	.text


       .align    4,0x90


	.globl   MD4Final
	.type	MD4Final,@function
MD4Final:
.L31:
        pushl     %ebp
        pushl     %ebx
        subl      $28, %esp
        movl      40(%esp), %ebp
        movl      44(%esp), %ebx
        lea       16(%esp), %eax
        lea       16(%ebx), %edx
        movl      %eax, (%esp)
        movl      %edx, 4(%esp)
        movl      $8, 8(%esp)
        call      Encode

.L32:
        movl      16(%ebx), %eax
        shrl      $3, %eax
        andl      $63, %eax
        cmpl      $56, %eax
        jae       .L34

.L33:
        negl      %eax
        addl      $56, %eax
        jmp       .L35

.L34:
        negl      %eax
        addl      $120, %eax

.L35:
        movl      %ebx, (%esp)
        movl      $PADDING, 4(%esp)
        movl      %eax, 8(%esp)
        call      MD4Update

.L36:
        movl      %ebx, (%esp)
        lea       16(%esp), %eax
        movl      %eax, 4(%esp)
        movl      $8, 8(%esp)
        call      MD4Update

.L37:
        movl      %ebp, (%esp)
        movl      %ebx, 4(%esp)
        movl      $16, 8(%esp)
        call      Encode

.L38:
        xorl      %eax, %eax
        andl      $255, %eax
        movl      %eax, %edx
        shll      $8, %edx
        orl       %edx, %eax
        movl      %eax, %ecx
        shll      $16, %ecx
        orl       %ecx, %eax
        movl      %eax, (%ebx)
        movl      %eax, 4(%ebx)
        movl      %eax, 8(%ebx)
        movl      %eax, 12(%ebx)
        movl      %eax, 16(%ebx)
        movl      %eax, 20(%ebx)
        movl      %eax, 24(%ebx)
        movl      %eax, 28(%ebx)
        movl      %eax, 32(%ebx)
        movl      %eax, 36(%ebx)
        movl      %eax, 40(%ebx)
        movl      %eax, 44(%ebx)
        movl      %eax, 48(%ebx)
        movl      %eax, 52(%ebx)
        movl      %eax, 56(%ebx)
        movl      %eax, 60(%ebx)
        movl      %eax, 64(%ebx)
        movl      %eax, 68(%ebx)
        movl      %eax, 72(%ebx)
        movl      %eax, 76(%ebx)
        movl      %eax, 80(%ebx)
        movl      %eax, 84(%ebx)

.L39:
        addl      $28, %esp
        popl      %ebx
        popl      %ebp
        ret
        .align    4,0x90


	.size	MD4Final,.-MD4Final
	.data

	.data
	.text


       .align    4,0x90


	.type	MD4Transform,@function
MD4Transform:
.L41:
        pushl     %edi
        pushl     %esi
        pushl     %ebp
        pushl     %ebx
        subl      $124, %esp
        movl      144(%esp), %edx
        movl      148(%esp), %eax
        movl      (%edx), %ebp
        movl      4(%edx), %ecx
        movl      8(%edx), %edi
        movl      12(%edx), %edx
        movzbl    (%eax), %ebx
        movzbl    1(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    2(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    3(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 116(%esp)
        movl      %ebx, (%esp)
        movzbl    4(%eax), %ebx
        movzbl    5(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    6(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    7(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 68(%esp)
        movl      %ebx, 4(%esp)
        movzbl    8(%eax), %ebx
        movzbl    9(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    10(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    11(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 80(%esp)
        movl      %ebx, 8(%esp)
        movzbl    12(%eax), %ebx
        movzbl    13(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    14(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    15(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 64(%esp)
        movl      %ebx, 12(%esp)
        movzbl    16(%eax), %ebx
        movzbl    17(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    18(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    19(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 88(%esp)
        movl      %ebx, 16(%esp)
        movzbl    20(%eax), %ebx
        movzbl    21(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    22(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    23(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 72(%esp)
        movl      %ebx, 20(%esp)
        movzbl    24(%eax), %ebx
        movzbl    25(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    26(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    27(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 84(%esp)
        movl      %ebx, 24(%esp)
        movzbl    28(%eax), %ebx
        movzbl    29(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    30(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    31(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 76(%esp)
        movl      %ebx, 28(%esp)
        movzbl    32(%eax), %ebx
        movzbl    33(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    34(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    35(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 104(%esp)
        movl      %ebx, 32(%esp)
        movzbl    36(%eax), %ebx
        movzbl    37(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    38(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    39(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 92(%esp)
        movl      %ebx, 36(%esp)
        movzbl    40(%eax), %ebx
        movzbl    41(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    42(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    43(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 100(%esp)
        movl      %ebx, 40(%esp)
        movzbl    44(%eax), %ebx
        movzbl    45(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    46(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    47(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 96(%esp)
        movl      %ebx, 44(%esp)
        movzbl    48(%eax), %ebx
        movzbl    49(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    50(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    51(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 112(%esp)
        movl      %ebx, 48(%esp)
        movzbl    52(%eax), %ebx
        movzbl    53(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    54(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    55(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 108(%esp)
        movl      %ebx, 52(%esp)
        movzbl    56(%eax), %ebx
        movzbl    57(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    58(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    59(%eax), %esi
        shll      $24, %esi
        orl       %esi, %ebx
        movl      %ebx, 120(%esp)
        movl      %ebx, 56(%esp)
        movzbl    60(%eax), %ebx
        movzbl    61(%eax), %esi
        shll      $8, %esi
        orl       %esi, %ebx
        movzbl    62(%eax), %esi
        shll      $16, %esi
        orl       %esi, %ebx
        movzbl    63(%eax), %eax
        shll      $24, %eax
        orl       %eax, %ebx
        movl      %ecx, %esi
        andl      %edi, %esi
        movl      %ebx, 60(%esp)
        movl      %ecx, %eax
        xorl      $-1, %eax
        addl      116(%esp), %ebp
        andl      %edx, %eax
        orl       %eax, %esi
        addl      %esi, %ebp
        roll      $3, %ebp
        movl      %ecx, %esi
        andl      %ebp, %esi
        movl      %ebp, %eax
        xorl      $-1, %eax
        andl      %edi, %eax
        orl       %eax, %esi
        addl      68(%esp), %esi
        addl      %edx, %esi
        roll      $7, %esi
        addl      80(%esp), %edi
        movl      %ebp, %edx
        andl      %esi, %edx
        movl      %esi, %eax
        xorl      $-1, %eax
        andl      %ecx, %eax
        orl       %eax, %edx
        addl      %edx, %edi
        roll      $11, %edi
        addl      64(%esp), %ecx
        movl      %edi, %edx
        andl      %esi, %edx
        movl      %edi, %eax
        xorl      $-1, %eax
        andl      %ebp, %eax
        orl       %eax, %edx
        addl      %edx, %ecx
        roll      $19, %ecx
        addl      88(%esp), %ebp
        movl      %ecx, %edx
        andl      %edi, %edx
        movl      %ecx, %eax
        xorl      $-1, %eax
        andl      %esi, %eax
        orl       %eax, %edx
        addl      %edx, %ebp
        roll      $3, %ebp
        movl      %ecx, %eax
        andl      %ebp, %eax
        movl      %ebp, %edx
        xorl      $-1, %edx
        andl      %edi, %edx
        orl       %edx, %eax
        addl      72(%esp), %eax
        addl      %esi, %eax
        roll      $7, %eax
        addl      84(%esp), %edi
        movl      %ebp, %esi
        andl      %eax, %esi
        movl      %eax, %edx
        xorl      $-1, %edx
        andl      %ecx, %edx
        orl       %edx, %esi
        addl      %esi, %edi
        roll      $11, %edi
        addl      76(%esp), %ecx
        movl      %edi, %esi
        andl      %eax, %esi
        movl      %edi, %edx
        xorl      $-1, %edx
        andl      %ebp, %edx
        orl       %edx, %esi
        addl      %esi, %ecx
        roll      $19, %ecx
        addl      104(%esp), %ebp
        movl      %ecx, %esi
        andl      %edi, %esi
        movl      %ecx, %edx
        xorl      $-1, %edx
        andl      %eax, %edx
        orl       %edx, %esi
        addl      %esi, %ebp
        roll      $3, %ebp
        movl      %ecx, %edx
        andl      %ebp, %edx
        movl      %ebp, %esi
        xorl      $-1, %esi
        andl      %edi, %esi
        orl       %esi, %edx
        addl      92(%esp), %edx
        addl      %eax, %edx
        roll      $7, %edx
        addl      100(%esp), %edi
        movl      %ebp, %esi
        andl      %edx, %esi
        movl      %edx, %eax
        xorl      $-1, %eax
        andl      %ecx, %eax
        orl       %eax, %esi
        addl      %esi, %edi
        roll      $11, %edi
        addl      96(%esp), %ecx
        movl      %edi, %esi
        andl      %edx, %esi
        movl      %edi, %eax
        xorl      $-1, %eax
        andl      %ebp, %eax
        orl       %eax, %esi
        addl      %esi, %ecx
        roll      $19, %ecx
        addl      112(%esp), %ebp
        movl      %ecx, %esi
        andl      %edi, %esi
        movl      %ecx, %eax
        xorl      $-1, %eax
        andl      %edx, %eax
        orl       %eax, %esi
        addl      %esi, %ebp
        roll      $3, %ebp
        movl      %ecx, %eax
        andl      %ebp, %eax
        movl      %ebp, %esi
        xorl      $-1, %esi
        andl      %edi, %esi
        orl       %esi, %eax
        addl      108(%esp), %eax
        addl      %edx, %eax
        roll      $7, %eax
        addl      120(%esp), %edi
        movl      %ebp, %esi
        andl      %eax, %esi
        movl      %eax, %edx
        xorl      $-1, %edx
        andl      %ecx, %edx
        orl       %edx, %esi
        addl      %esi, %edi
        roll      $11, %edi
        movl      %edi, %edx
        andl      %eax, %edx
        addl      %ebx, %ecx
        movl      %edi, %esi
        xorl      $-1, %esi
        andl      %ebp, %esi
        orl       %edx, %esi
        addl      %esi, %ecx
        roll      $19, %ecx
        movl      %edi, %esi
        orl       %eax, %esi
        andl      %ecx, %esi
        orl       %edx, %esi
        lea       1518500249(%ebp,%esi), %edx
        movl      %ecx, %esi
        addl      116(%esp), %edx
        roll      $3, %edx
        orl       %edi, %esi
        andl      %edx, %esi
        movl      %ecx, %ebp
        andl      %edi, %ebp
        orl       %ebp, %esi
        lea       1518500249(%esi,%eax), %esi
        movl      %ecx, %ebp
        addl      88(%esp), %esi
        roll      $5, %esi
        orl       %edx, %ebp
        andl      %esi, %ebp
        movl      %ecx, %eax
        andl      %edx, %eax
        orl       %eax, %ebp
        lea       1518500249(%edi,%ebp), %eax
        movl      %edx, %edi
        addl      104(%esp), %eax
        roll      $9, %eax
        orl       %esi, %edi
        andl      %eax, %edi
        movl      %edx, %ebp
        andl      %esi, %ebp
        orl       %ebp, %edi
        lea       1518500249(%ecx,%edi), %ebp
        movl      %eax, %edi
        addl      112(%esp), %ebp
        roll      $13, %ebp
        orl       %esi, %edi
        andl      %ebp, %edi
        movl      %eax, %ecx
        andl      %esi, %ecx
        orl       %ecx, %edi
        lea       1518500249(%edx,%edi), %ecx
        movl      %ebp, %edi
        addl      68(%esp), %ecx
        roll      $3, %ecx
        orl       %eax, %edi
        andl      %ecx, %edi
        movl      %ebp, %edx
        andl      %eax, %edx
        orl       %edx, %edi
        lea       1518500249(%edi,%esi), %edx
        movl      %ebp, %edi
        addl      72(%esp), %edx
        roll      $5, %edx
        orl       %ecx, %edi
        andl      %edx, %edi
        movl      %ebp, %esi
        andl      %ecx, %esi
        orl       %esi, %edi
        lea       1518500249(%eax,%edi), %esi
        movl      %ecx, %edi
        addl      92(%esp), %esi
        roll      $9, %esi
        orl       %edx, %edi
        andl      %esi, %edi
        movl      %ecx, %eax
        andl      %edx, %eax
        orl       %eax, %edi
        lea       1518500249(%ebp,%edi), %eax
        movl      %esi, %edi
        addl      108(%esp), %eax
        roll      $13, %eax
        orl       %edx, %edi
        andl      %eax, %edi
        movl      %esi, %ebp
        andl      %edx, %ebp
        orl       %ebp, %edi
        lea       1518500249(%ecx,%edi), %ecx
        movl      %eax, %edi
        addl      80(%esp), %ecx
        roll      $3, %ecx
        orl       %esi, %edi
        andl      %ecx, %edi
        movl      %eax, %ebp
        andl      %esi, %ebp
        orl       %ebp, %edi
        lea       1518500249(%edi,%edx), %ebp
        movl      %eax, %edi
        addl      84(%esp), %ebp
        roll      $5, %ebp
        orl       %ecx, %edi

.L45:
        andl      %ebp, %edi
        movl      %eax, %edx
        andl      %ecx, %edx
        orl       %edx, %edi
        lea       1518500249(%esi,%edi), %edx
        movl      %ecx, %edi
        addl      100(%esp), %edx
        roll      $9, %edx
        orl       %ebp, %edi
        andl      %edx, %edi
        movl      %ecx, %esi
        andl      %ebp, %esi
        orl       %esi, %edi
        lea       1518500249(%eax,%edi), %eax
        movl      %edx, %edi
        addl      120(%esp), %eax
        roll      $13, %eax
        orl       %ebp, %edi
        andl      %eax, %edi
        movl      %edx, %esi
        andl      %ebp, %esi
        orl       %esi, %edi
        lea       1518500249(%ecx,%edi), %ecx
        movl      %eax, %edi
        addl      64(%esp), %ecx
        roll      $3, %ecx
        orl       %edx, %edi
        andl      %ecx, %edi
        movl      %eax, %esi
        andl      %edx, %esi
        orl       %esi, %edi
        lea       1518500249(%edi,%ebp), %edi
        movl      %eax, %esi
        addl      76(%esp), %edi
        roll      $5, %edi
        orl       %ecx, %esi
        andl      %edi, %esi
        movl      %eax, %ebp
        andl      %ecx, %ebp
        orl       %ebp, %esi
        lea       1518500249(%edx,%esi), %esi
        movl      %ecx, %ebp
        addl      96(%esp), %esi
        roll      $9, %esi
        orl       %edi, %ebp
        andl      %esi, %ebp
        movl      %ecx, %edx
        andl      %edi, %edx
        orl       %edx, %ebp
        lea       1518500249(%eax,%ebp), %ebp
        addl      %ebx, %ebp
        roll      $13, %ebp
        movl      %ebp, %eax
        xorl      %esi, %eax
        xorl      %edi, %eax
        lea       1859775393(%ecx,%eax), %ecx
        addl      116(%esp), %ecx
        roll      $3, %ecx
        movl      %ecx, %eax
        xorl      %ebp, %eax
        xorl      %esi, %eax
        lea       1859775393(%eax,%edi), %edx
        addl      104(%esp), %edx
        roll      $9, %edx
        movl      %edx, %eax
        xorl      %ecx, %eax
        xorl      %ebp, %eax
        lea       1859775393(%esi,%eax), %eax
        addl      88(%esp), %eax
        roll      $11, %eax
        movl      %eax, %esi
        xorl      %edx, %esi
        xorl      %ecx, %esi
        lea       1859775393(%ebp,%esi), %ebp
        addl      112(%esp), %ebp
        roll      $15, %ebp
        movl      %ebp, %esi
        xorl      %eax, %esi
        xorl      %edx, %esi
        lea       1859775393(%ecx,%esi), %ecx
        addl      80(%esp), %ecx
        roll      $3, %ecx
        movl      %ecx, %esi
        xorl      %ebp, %esi
        xorl      %eax, %esi
        lea       1859775393(%esi,%edx), %edx
        addl      100(%esp), %edx
        roll      $9, %edx
        movl      %edx, %esi
        xorl      %ecx, %esi
        xorl      %ebp, %esi
        lea       1859775393(%eax,%esi), %esi
        addl      84(%esp), %esi
        roll      $11, %esi
        movl      %esi, %eax
        xorl      %edx, %eax
        xorl      %ecx, %eax
        lea       1859775393(%ebp,%eax), %ebp
        addl      120(%esp), %ebp
        roll      $15, %ebp
        movl      %ebp, %eax
        xorl      %esi, %eax
        xorl      %edx, %eax
        lea       1859775393(%ecx,%eax), %eax
        addl      68(%esp), %eax
        roll      $3, %eax
        movl      %eax, %ecx
        xorl      %ebp, %ecx
        xorl      %esi, %ecx
        lea       1859775393(%ecx,%edx), %edi
        addl      92(%esp), %edi
        roll      $9, %edi
        movl      %edi, %edx
        xorl      %eax, %edx
        xorl      %ebp, %edx
        lea       1859775393(%esi,%edx), %ecx
        addl      72(%esp), %ecx
        roll      $11, %ecx
        movl      %ecx, %edx
        xorl      %edi, %edx
        xorl      %eax, %edx
        lea       1859775393(%ebp,%edx), %edx
        addl      108(%esp), %edx
        roll      $15, %edx
        movl      %edx, %ebp
        xorl      %ecx, %ebp
        xorl      %edi, %ebp
        lea       1859775393(%eax,%ebp), %esi
        addl      64(%esp), %esi
        roll      $3, %esi
        movl      %esi, %eax
        xorl      %edx, %eax
        xorl      %ecx, %eax
        lea       1859775393(%eax,%edi), %ebp
        addl      96(%esp), %ebp
        roll      $9, %ebp
        movl      %ebp, %eax
        xorl      %esi, %eax
        xorl      %edx, %eax
        lea       1859775393(%ecx,%eax), %eax
        addl      76(%esp), %eax
        roll      $11, %eax
        movl      %eax, %ecx
        xorl      %ebp, %ecx
        xorl      %esi, %ecx
        lea       1859775393(%edx,%ecx), %edx
        addl      %ebx, %edx
        roll      $15, %edx
        movl      144(%esp), %ebx
        addl      %esi, (%ebx)
        addl      %edx, 4(%ebx)
        addl      %eax, 8(%ebx)
        xorl      %eax, %eax
        addl      %ebp, 12(%ebx)
        lea       (%esp), %ecx
        andl      $255, %eax
        movl      %eax, %edx
        shll      $8, %edx
        orl       %edx, %eax
        movl      %eax, %edx
        shll      $16, %edx
        orl       %edx, %eax
        movl      %eax, (%ecx)
        movl      %eax, 4(%ecx)
        movl      %eax, 8(%ecx)
        movl      %eax, 12(%ecx)
        movl      %eax, 16(%ecx)
        movl      %eax, 20(%ecx)
        movl      %eax, 24(%ecx)
        movl      %eax, 28(%ecx)
        movl      %eax, 32(%ecx)
        movl      %eax, 36(%ecx)
        movl      %eax, 40(%ecx)
        movl      %eax, 44(%ecx)
        movl      %eax, 48(%ecx)
        movl      %eax, 52(%ecx)
        movl      %eax, 56(%ecx)
        movl      %eax, 60(%ecx)

.L42:
        addl      $124, %esp
        popl      %ebx
        popl      %ebp
        popl      %esi
        popl      %edi
        ret
        .align    4,0x90


	.size	MD4Transform,.-MD4Transform
	.data

	.text


       .align    4,0x90



	.type	Encode,@function
Encode:
.L51:
        pushl     %esi
        pushl     %ebx
        subl      $12, %esp
        movl      24(%esp), %ebx
        movl      28(%esp), %esi
        movl      32(%esp), %eax
        xorl      %edx, %edx
        xorl      %ecx, %ecx
        testl     %eax, %eax
        jbe       .L55

.L52:
        movl      %ecx, 4(%esp)
        

.L53:
        movl      28(%esp), %ecx
        movb      (%ecx,%edx,4), %bl
        movl      24(%esp), %esi
        movl      4(%esp), %eax
        movl      28(%esp), %ecx
        movb      %bl, (%eax,%esi)
        movl      (%ecx,%edx,4), %ecx
        shrl      $8, %ecx
        movb      %cl, 1(%eax,%esi)
        movl      28(%esp), %ecx
        movb      2(%ecx,%edx,4), %cl
        movb      %cl, 2(%eax,%esi)
        movl      28(%esp), %ecx
        movb      3(%ecx,%edx,4), %cl
        movb      %cl, 3(%eax,%esi)
        incl      %edx
        addl      $4, %eax
        movl      %eax, 4(%esp)
        movl      32(%esp), %ecx
        cmpl      %ecx, %eax
        jb        .L53

.L55:
        addl      $12, %esp
        popl      %ebx
        popl      %esi
        ret
        .align    4,0x90


	.size	Encode,.-Encode
	.data

	.data

