	.meta source "\"autos/neg.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	seti r16, #0
	seti r17, #1
	sub r16, r16, r17
	mul r15, r15, r16
	set r15, r5
	seti r13, #0
	seti r14, #1
	sub r13, r13, r14
	mul r12, r12, r13
	set r12, r6
	seti r10, #0
	seti r11, #1
	sub r10, r10, r11
	mul r7, r7, r10
	invoke 4, 7, 0
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
