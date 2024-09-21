/* Definitions for target machine AMO.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* little endian */
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

/* 32-bit architecture */
#define UNITS_PER_WORD 4
#define BITS_PER_WORD 32
#define MAX_BITS_PER_WORD 32

#define POINTER_BOUNDARY 32
#define PARM_BOUNDARY 32
#define STACK_BOUNDARY 32
#define FUNCTION_BOUNDARY 32
#define STRUCTURE_SIZE_BOUNDARY 8
#define EMPTY_FIELD_BOUNDARY 32

#define BIGGEST_ALIGNMENT 32
#define FASTEST_ALIGNMENT 32

/* since move instruction fails for unalignment address, this must be non-zero */
#define STRICT_ALIGNMENT 0

/* Align definitions of arrays, unions and structures so that
   initializations and copies can be made more efficient.  This is not
   ABI-changing, so it only affects places where we can see the
   definition. Increasing the alignment tends to introduce padding,
   so don't do this when optimizing for size/conserving stack space. */
#define AMO_EXPAND_ALIGNMENT(COND, EXP, ALIGN)				\
  (((COND) && ((ALIGN) < BITS_PER_WORD)					\
    && (TREE_CODE (EXP) == ARRAY_TYPE					\
	|| TREE_CODE (EXP) == UNION_TYPE				\
	|| TREE_CODE (EXP) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* Align global data. */
#define DATA_ALIGNMENT(EXP, ALIGN)			\
  AMO_EXPAND_ALIGNMENT(!optimize_size, EXP, ALIGN)

/* Similarly, make sure that objects on the stack are sensibly aligned.  */
#define LOCAL_ALIGNMENT(EXP, ALIGN)				\
  AMO_EXPAND_ALIGNMENT(!flag_conserve_stack, EXP, ALIGN)


#define MAX_FIXED_MODE_SIZE 32

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

#define DEFAULT_SIGNED_CHAR 1

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "unsigned int"

/* ues integers as pointers and function addresses */
#define FUNCTION_MODE SImode
#define Pmode SImode

#define AMO_NUM_INTS(X) (((X) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
#define AMO_NUM_REGS(MODE) AMO_NUM_INTS(GET_MODE_SIZE(MODE))

/* available register classes */
enum reg_class {
  NO_REGS,
  GP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES /* finalizer */
};

/* names of register classes */
#define REG_CLASS_NAMES                                                         \
{                                                                               \
  "NO_REGS",                                                                    \
  "GENERAL_REGS",                                                               \
  "ALL_REGS"                                                                    \
}

/* register names, lots to type */
#define REGISTER_NAMES                                                         \
{                                                                              \
  "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",                       \
  "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",                      \
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",                      \
  "r24", "r25", "r26", "r27", "r28", "sp", "fp", "lr"						   \
}

/* register class content */
#define REG_CLASS_CONTENTS                                                     \
{                                                                              \
  { 0x00000000 }, /* no registers */										   \
  { 0xffffffff }, /* general register */									   \
  { 0xffffffff }  /* all registers */										   \
}

/* list of registers not available for allocation */
#define FIXED_REGISTERS                                                        \
{                                                                              \
  /* r0-r7 */                                                                  \
  0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r8-r15 */                                                                 \
  0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r16-r23 */                                                                \
  0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r24-r31 */                                                                \
  0, 0, 0, 0, 0, 1, 1, 1                                                       \
}

/* list of registers potentially clobbered by callee's */
#define CALL_USED_REGISTERS                                                    \
{                                                                              \
  /* r0-r7 */                                                                  \
  1, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r8-r15 */                                                                 \
  0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r16-r23 */                                                                \
  0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r24-r31 */                                                                \
  0, 0, 0, 0, 0, 1, 1, 1                                                       \
}

/* suggest a register allocation order */
#define REG_ALLOC_ORDER {                                                      \
	1, 2, 3, 4, 5, 6, 7, 8,													   \
	9, 10, 11, 12, 13, 14, 15, 16,													   \
	17, 18, 19, 20, 21, 22, 23, 24,													   \
	25, 26, 27, 28, 0, 29, 30, 31													   \
}

#define N_REG_CLASSES (int) reg_class::LIM_REG_CLASSES
#define GENERAL_REGS reg_class::GP_REGS

#define REGNO_REG_CLASS(REGNO) amo_regno_to_class (REGNO)
#define REGNO_OK_FOR_BASE_P(REGNO) amo_valid_regno_for_base_p (REGNO)
#define REGNO_OK_FOR_INDEX_P(REGNO) amo_valid_regno_for_index_p (REGNO)

#define BASE_REG_CLASS reg_class::GP_REGS
#define INDEX_REG_CLASS reg_class::GP_REGS

#define FIRST_ARG_REGNUM 1
#define LAST_ARG_REGNUM 4
#define FIRST_CALLEE_SAVED_REGNUM 8
#define LAST_CALLEE_SAVED_REGNUM 14

#define STACK_POINTER_REGNUM 29
#define FRAME_POINTER_REGNUM 30
#define RET_VALUE_REGNUM 0
#define RET_ADDRESS_REGNUM 31
#define FIRST_PSEUDO_REGISTER 32
#define MAX_REGS_PER_ADDRESS 1
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM
#define NUM_ARG_REGISTERS LAST_ARG_REGNUM

#define ELIMINABLE_REGS {{ FRAME_POINTER_REGNUM }}
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = amo_initial_elimination_offset((FROM), (TO))

/******************************************************************************/
/* Memory, stack, function args                                               */
/******************************************************************************/

#define MOVE_MAX 4
#define SLOW_BYTE_ACCESS 0

#define PUSH_ARGS 1
//#define PUSH_ARGS_REVERSED 1
#define ACCUMULATE_OUTGOING_ARGS 1

#define STACK_GROWS_DOWNWARD 1
#define FRAME_GROWS_DOWNWARD 1
#define STACK_POINTER_OFFSET 16
#define EXIT_IGNORE_STACK 1
#define MAX_ARGS_IN_REGISTERS 6

#define WORD_REGISTER_OPERATIONS 1
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

#define ADDR_VEC_ALIGN(ADDR_VEC) 2

//TODO:
//#define CONSTANT_ADDRESS_P(X)

/* cumulative argument info */
typedef struct
{
  int num_reg_args;
  int num_args;
} amo_cumulative_arg_info;

#define CUMULATIVE_ARGS amo_cumulative_arg_info

/* init cumulative args */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS)     \
  amo_init_cumulative_args(&CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS);

/* misc. function stuff */
#define FUNCTION_ARG_REGNO_P(N)                                                \
  ((N >= FIRST_ARG_REGNUM) && (N <= LAST_ARG_REGNUM))

#define FIRST_PARM_OFFSET(FNDECL) 0

/******************************************************************************/
/* Misc.                                                                      */
/******************************************************************************/

#define STORE_FLAG_VALUE 1

/* for nested functions only */
#define TRAMPOLINE_SIZE 32
#define TRAMPOLINE_ALIGNMENT 64

/* treat 'case' labels as integers */
#define CASE_VECTOR_MODE SImode

/* no profiler support yet */
#define FUNCTION_PROFILER(FILE, LABELNO)                                       \
  do {                                                                         \
  } while(0)

/* target CPU builtins */
#define TARGET_CPU_CPP_BUILTINS()                                            \
  do {                                                                       \
    builtin_assert ("cpu=amo");                                              \
    builtin_assert ("machine=amo");                                          \
    builtin_define ("__amo__");                                              \
    builtin_define ("__AMO__");                                              \
    builtin_define ("__AMO_SOFT_FLOAT__");                                   \
  } while (0)

/******************************************************************************/
/* Assembler                                                                  */ 
/******************************************************************************/

/* how to output alignment directives */
#define ASM_OUTPUT_ALIGN(STREAM, LOG)                                          \
  do {                                                                         \
    if (LOG != 0)                                                              \
      fprintf (STREAM, "\t.align\t%d\n", 1 << (LOG));                          \
  } while (0)

/* how to output labels */
#define ASM_OUTPUT_LABEL(FILE, NAME)                                           \
  do {                                                                         \
    assemble_name (FILE, NAME);                                                \
    fputs (":\n", FILE);                                                       \
  } while (0)

#define TEXT_SECTION_ASM_OP "\t.text"
#define DATA_SECTION_ASM_OP "\t.data"

//#define READONLY_DATA_SECTION_ASM_OP "\t.rodata"
#define BSS_SECTION_ASM_OP "\t.bss"

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP "\t.global\t"

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

