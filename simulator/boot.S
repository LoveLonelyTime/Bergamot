/*
 * Boot ROM
 *
 * Compile as boot.hex
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

.text
.globl	_start

/* . = 0xffff0000; */
_start:
li a2, 0x80000000 /* Entry point */
csrr a0, mhartid /* Hart id */
li a1, 0x8ff00000 /* DT address */
jalr ra, 0(a2) /* Go */