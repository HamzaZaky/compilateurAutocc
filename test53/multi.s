	.meta source "\"test53/multi.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	set r0, r5
	set r0, r6
	set r0, r7
	set r10, r5
	set r11, r6
	add r9, r10, r11
	set r12, r7
	add r8, r9, r12
	set r13, r0
	goto_ne L2, r8, r13
	goto L3
L2:
	set r14, r1
	invoke 4, 14, 0
	goto L4
L3:
	set r15, r0
	invoke 4, 15, 0
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
