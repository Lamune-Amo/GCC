This repository contains a **non-official port**. Original notices are kept in `README.upstream` and `COPYING*`.

[**English**](README.md) · [한국어](README_ko.md)

# AMO GCC (port)

GNU GCC port for the **AMO architecture**.

Target triple: `amo-linux` (`amo-unknown-linux`)

## Target & ABI (summary)

**Word model**: ILP32

**Endianness**: Little-endian

**Alignment:** word-aligned preferred; unaligned loads are **hardware-supported by the SoC** (one CPU cycle; handled by a 2-beat merge in the memory subsystem).

**Calling convention**:

| Register   | Special | Width  | Notes                                |
|------------|---------|--------|--------------------------------------|
| R0         |         | 32-bit | Caller-saved/Result register         |
| R1-R15     |         | 32-bit | Caller-saved registers               |
| R16-R25    |         | 32-bit | Callee-saved registers               |
| R26-R27    |         | 32-bit | Interrupt registers                  |
| R28        |         | 32-bit | Argument register                    |
| R29        | FP      | 32-bit | Frame pointer                        |
| R30        | SP      | 32-bit | Stack pointer                        |
| R31        | LR      | 32-bit | Link register                        |
| PC         |         | 32-bit | Program Counter                      |
| CPSR       |         | 4-bit  | Current Program Status Register      |
| IDTR       |         | 32-bit | Interrupt Descriptor Table Register  |

`PC`, `CPSR` are non-allocatable.

**Atomics**: lock-mask based critical sections (no SMP)

## Software Integer Arithmetic

This port references a minimal set of `libgcc-compatible` helpers for `32-bit integer` math.

> **Important**: Only the symbol references are emitted by GCC; you must provide the actual implementations.
> See the design and implementations in [lib/soft/](https://github.com/Lamune-Amo/Lamune/tree/master/lib/soft)

| Symbol       | Prototype                                                | Notes                            |
|--------------|----------------------------------------------------------|----------------------------------|
| `__mulsi3`   | unsigned int __mulsi3 (unsigned int a, unsigned int b)   | Unsigned 32-bit multiply         |
| `__divsi3`   | long __divsi3 (long a, long b)                           | Signed 32-bit divide             |
| `__udivsi3`  | long __udivsi3 (long a, long b)                          | Unsigned 32-bit divide           |
| `__modsi3`   | long __modsi3 (long a, long b)                           | Signed 32-bit remainder          |
| `__umodsi3`  | long __umodsi3 (long a, long b)                          | Unsigned 32-bit remainder        |

Names follow `libgcc convention` so the compiler can call them without extra flags.

## Compiler

```c
int kill (pid_t pid, int sig)
{
	struct task_struct *task;

	if (pid == 1)
		return 0;

	task = task_find_by_pid (pid);
	if (!task)
		return -1;
	task->state = DIED;
	task->pc = (uint32_t) task_destructor;
	return 0;
}
```

```
amo-linux-gcc -fno-builtin -fno-exceptions -fno-stack-protector -nostdlib -nodefaultlibs -ffreestanding -S kill.c -o kill.s
```

You can compile C to assembly with `gcc` as shown below.

```asm
	.file	"kill.c"
	.text
	.align	4
	.global	kill
	.type	kill, @function
kill:
	sub		sp, sp, $8
	str		[sp, $4], lr
	str		[sp], fp
	add		sp, sp, $-12
	mov		fp, sp
	str		[fp, $4], r2
	str		[fp, $8], r3
	ldr		r1, [fp, $4]
	mov		r0, $1
	bne		r1, r0, .L2
	mov		r0, $0
	jmp		.L3
.L2:
	ldr		r0, [fp, $4]
	mov		r2, r0
	jal		task_find_by_pid
	str		[fp], r0
	ldr		r1, [fp]
	mov		r0, $0
	bne		r1, r0, .L4
	mov		r0, $-1
	jmp		.L3
.L4:
	ldr		r0, [fp]
	mov		r1, $2
	str		[r0], r1
	mov		r1, task_destructor
	ldr		r0, [fp]
	str		[r0, $128], r1
	mov		r0, $0
.L3:
	mov		sp, fp
	add		sp, sp, $12
	ldr		fp, [sp]
	ldr		lr, [sp, $4]
	add		sp, sp, $8
	jmp		lr
.ltorg
	.size	kill, .-kill
	.ident	"GCC: (GNU) 8.4.0"
```

## Build

```
configure --target=amo-linux --prefix=/path/install --disable-bootstrap --disable-checking --disable-multilib --without-glib --enable-languages=c
make all-gcc -j8
make check-gcc RUNTESTFLAGS="compile.exp"
make install 
```