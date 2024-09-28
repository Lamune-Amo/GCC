/* Definitions of target machine for GNU compiler, for AMO.
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
   Contributed by KPIT Cummins Infosystems Limited.

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
   
#ifndef GCC_AMO_H
#define GCC_AMO_H

#define OBJECT_FORMAT_ELF

/* Run-time target specification.  */
#ifndef TARGET_CPU_CPP_BUILTINS
#define TARGET_CPU_CPP_BUILTINS()               \
do                                              \
  {                                             \
    builtin_assert ("cpu=amo");                 \
    builtin_assert ("machine=amo");             \
    builtin_define ("__amo__");                 \
    builtin_define ("__AMO__");                 \
    builtin_define ("__AMO_SOFT_FLOAT__");      \
  }                                             \
  while (0)
#endif

#define AMO_TARGET_DATA_NEAR   amo_is_data_model (DM_NEAR)
#define AMO_TARGET_DATA_MEDIUM amo_is_data_model (DM_DEFAULT)
#define AMO_TARGET_DATA_FAR    amo_is_data_model (DM_FAR)

/* Storage layout.  */
#define BITS_BIG_ENDIAN     0

#define BYTES_BIG_ENDIAN    0

#define WORDS_BIG_ENDIAN    0

#define UNITS_PER_WORD      4

/* Units per 32-bit (DWORD).  */
#define AMO_UNITS_PER_DWORD 64 // not for precompile

#define POINTER_SIZE        32

#define PARM_BOUNDARY       32

#define STACK_BOUNDARY      (MAX (BIGGEST_ALIGNMENT, PARM_BOUNDARY))

#define FUNCTION_BOUNDARY   BIGGEST_ALIGNMENT

/* Biggest alignment on AMOC+ is 32-bit as internal bus is AMBA based 
   where as AMOC is proprietary internal bus architecture.  */
#define BIGGEST_ALIGNMENT   32

#define MAX_FIXED_MODE_SIZE 64

/* Align definitions of arrays, unions and structures so that
   initializations and copies can be made more efficient.  This is not
   ABI-changing, so it only affects places where we can see the
   definition.  Increasing the alignment tends to introduce padding,
   so don't do this when optimizing for size/conserving stack space.  */
#define AMO_EXPAND_ALIGNMENT(COND, EXP, ALIGN)			\
  (((COND) && ((ALIGN) < BITS_PER_WORD)					\
    && (TREE_CODE (EXP) == ARRAY_TYPE					\
	|| TREE_CODE (EXP) == UNION_TYPE				\
	|| TREE_CODE (EXP) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* Align global data.  */
#define DATA_ALIGNMENT(EXP, ALIGN)			\
  AMO_EXPAND_ALIGNMENT (!optimize_size, EXP, ALIGN)

  /* Similarly, make sure that objects on the stack are sensibly aligned.  */
#define LOCAL_ALIGNMENT(EXP, ALIGN)				\
  AMO_EXPAND_ALIGNMENT (!flag_conserve_stack, EXP, ALIGN)

#define STRICT_ALIGNMENT 0

#define PCC_BITFIELD_TYPE_MATTERS 1

/* Layout of source language data types.  */
#define SHORT_TYPE_SIZE     16

#define INT_TYPE_SIZE       32

#define LONG_TYPE_SIZE      32

#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE     32

#define DOUBLE_TYPE_SIZE    64

#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE           "unsigned int"
 
#define PTRDIFF_TYPE        "int"

#define WCHAR_TYPE          "unsigned int"

/* By default, the C++ compiler will use the lowest bit of the pointer
   to function to indicate a pointer-to-member-function points to a
   virtual member function.  However, in CR architecture FUNCTION_BOUNDARY
   indicates function addresses are always even, but function pointers can be
   odd (after right-shifting them when loading them into a register), and the
   default doesn't work.  In that case, the lowest bit of the delta
   field will be used (the remainder of the field is shifted to the left).  */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION     ptrmemfunc_vbit_in_delta

/* Define DEFAULT_PCC_STRUCT_RETURN to 1 if all structure and union return
   values must be in memory.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Register usage.  */

/* First 32-bit register is R12.  */
#define AMO_FIRST_DWORD_REGISTER   12 // TODO

#define FIRST_PSEUDO_REGISTER      32

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the AMO, only the stack pointer (r29) is such.  */
#define FIXED_REGISTERS                                                          \
  {                                                                              \
  /* r0 r1 r2 r3 r4 r5 r6 r7 */                                                  \
    0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r8 r9 r10 r11 r12 r13 r14 r15 */                                            \
    0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r16 r17 r18 r19 r20 r21 r22 r23 */                                          \
    0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r24 r25 r26 r27 r28 fp sp lr */                                             \
    0, 0, 0, 0, 0, 0, 1, 1                                                       \
  }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
 
   On the AMO, calls clobbers r0-r6 (scratch registers), 
   ra (the return address) and sp (the stack pointer).  */
#define CALL_USED_REGISTERS                                                    \
  {                                                                              \
  /* r0 r1 r2 r3 r4 r5 r6 r7 */                                                  \
    1, 1, 1, 1, 1, 1, 1, 1,                                                      \
  /* r8 r9 r10 r11 r12 r13 r14 r15 */                                            \
    1, 1, 1, 1, 1, 1, 1, 1,                                                      \
  /* r16 r17 r18 r19 r20 r21 r22 r23 */                                          \
    0, 0, 0, 0, 0, 0, 0, 0,                                                      \
  /* r24 r25 r26 r27 r28 fp sp lr */                                             \
    0, 0, 0, 0, 0, 0, 1, 1                                                       \
  }

/* Returns 1 if the register is longer than word size, 0 otherwise.  */
#define LONG_REG_P(REGNO)                                                    \
  (targetm.hard_regno_nregs (REGNO,                                          \
			     GET_MODE_WIDER_MODE (word_mode).require ()) == 1)

#define NOTICE_UPDATE_CC(EXP, INSN) \
   notice_update_cc ((EXP))

/* Interrupt functions can only use registers that have already been 
   saved by the prologue, even if they would normally be call-clobbered 
   Check if sizes are same and then check if it is possible to rename.  */
#define HARD_REGNO_RENAME_OK(SRC, DEST)                 \
  (!amo_interrupt_function_p () || (df_regs_ever_live_p (DEST)))

/* Exception handling stuff.  */

#define gen_rtx_RA	gen_rtx_REG (Pmode, RETURN_ADDRESS_REGNUM)


/* TODO */
/* Use (r8,r7) and (r10,r9) to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) (((N) < 2) ? (N*2 + 7) : INVALID_REGNUM)

/* (r5,r4) holds a stack adjustment for returning to a handler.  */
#define EH_RETURN_STACKADJ_RTX 		gen_rtx_REG (Pmode, 4)

#define EH_RETURN_HANDLER_RTX \
  gen_rtx_MEM (Pmode, plus_constant (Pmode, arg_pointer_rtx, -4))

#define INCOMING_RETURN_ADDR_RTX	gen_rtx_RA
/* TODO_END */

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.  */
#define RETURN_ADDR_RTX(COUNT, FRAME) 			  		\
  (COUNT == 0)	?  gen_rtx_PLUS (Pmode, gen_rtx_RA, gen_rtx_RA)		\
		:  const0_rtx

enum reg_class
{
  NO_REGS,
  SHORT_REGS,
  LONG_REGS,
  NOSP_REGS,
  DOUBLE_BASE_REGS,
  GENERAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES	\
  {			\
    "NO_REGS",		\
    "SHORT_REGS",	\
    "LONG_REGS",	\
    "NOSP_REGS",	\
    "DOUBLE_BASE_REGS",	\
    "GENERAL_REGS",	\
    "ALL_REGS"		\
  }

#define REG_CLASS_CONTENTS			     		\
  {						     		\
    {0x00000000}, /* NO_REGS		             */  	\
    {0x00000000}, /* SHORT_REGS 	: x    */   	\
    {0xFFFFFFFF}, /* LONG_REGS 		: 0 - 31    */  	\
    {0x1FFFFFFF}, /* NOSP_REGS 		: 0 - 28     */   	\
    {0x00000000}, /* DOUBLE_BASE_REGS   : x */  	\
    {0xFFFFFFFF}, /* GENERAL_REGS	: 0 - 31     */  	\
    {0xFFFFFFFF}  /* ALL_REGS 		: 0 - 31     */  	\
  }

#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P  hook_bool_mode_true 

#define REGNO_REG_CLASS(REGNO)  amo_regno_reg_class (REGNO)

#define BASE_REG_CLASS      GENERAL_REGS

//#define MODE_BASE_REG_CLASS(MODE) \
  (GET_MODE_SIZE (MODE) <= 4 ?  (BASE_REG_CLASS) :  (DOUBLE_BASE_REGS))

#define INDEX_REG_CLASS     GENERAL_REGS

#define AMO_REGNO_OK_FOR_BASE_P(REGNO)                  \
  (((REGNO) < FIRST_PSEUDO_REGISTER)                     \
     || (reg_renumber && ((unsigned) reg_renumber[REGNO] \
                        < FIRST_PSEUDO_REGISTER)))

/* TODO: For now lets not support index addressing mode.  */

#define REGNO_OK_FOR_INDEX_P(REGNO)        \
  (REGNO >= 0 && REGNO < FIRST_PSEUDO_REGISTER)

#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

/* TODO */
/* The maximum number of consecutive registers of class CLASS needed to
   hold a value of mode MODE.
   On the CompactRISC architecture, the size of MODE in words.
   The size of MODE in double words for the class LONG_REGS.

   The following check assumes if the class is not LONG_REGS, then
   all (NO_REGS, SHORT_REGS, NOSP_REGS and GENERAL_REGS) other classes are 
   short.  We may have to check if this can cause any degradation in 
   performance.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  (CLASS == LONG_REGS \
   ? (GET_MODE_SIZE (MODE) + AMO_UNITS_PER_DWORD - 1) / AMO_UNITS_PER_DWORD\
   : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Macros to check the range of integers . These macros were used across
   the port, majorly in constraints.md, predicates.md files. */
#define SIGNED_INT_FITS_N_BITS(imm, N)           \
  ((((imm) < ((HOST_WIDE_INT) 1 << ((N) - 1)))       \
      && ((imm) >= -((HOST_WIDE_INT) 1 << ((N) - 1)))) ? 1 : 0)

#define UNSIGNED_INT_FITS_N_BITS(imm, N) \
  (((imm) < ((HOST_WIDE_INT) 1 << (N)) && (imm) >= (HOST_WIDE_INT) 0) ? 1 : 0)

#define IN_RANGE_P(VALUE, LOW, HIGH)                            \
  ((((HOST_WIDE_INT)(VALUE)) >= (HOST_WIDE_INT)(LOW))           \
   && (((HOST_WIDE_INT)(VALUE)) <= ((HOST_WIDE_INT)(HIGH))))

#define IN_RAN(VALUE, LOW, HIGH)                             \
  (((((HOST_WIDE_INT)(VALUE)) >= (HOST_WIDE_INT)(LOW))       \
   && (((HOST_WIDE_INT)(VALUE)) <= ((HOST_WIDE_INT)(HIGH)))) ? 1 : 0)

/* This check is for sbit/cbit instruction.  */
#define OK_FOR_Z(OP) \
  ((GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == CONST_INT) \
   || (GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == REG) \
   || (GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == PLUS \
       && GET_CODE (XEXP ((XEXP (OP, 0)), 0)) == REG \
       && GET_CODE (XEXP ((XEXP (OP, 0)), 1)) == CONST_INT))
/* TODO_END */

/* Stack layout and calling conventions.  */
#define STACK_GROWS_DOWNWARD 1

#define ARG_POINTER_REGNUM      28

#define FRAME_POINTER_REGNUM    29

#define STACK_POINTER_REGNUM    30

#define STATIC_CHAIN_REGNUM     1

#define RETURN_ADDRESS_REGNUM   31

#define FIRST_PARM_OFFSET(FNDECL) 0

#define ELIMINABLE_REGS                            \
  {                                                \
    { ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM}, \
    { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM}, \
    { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}  \
  }

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)              \
  do                                                              \
    {                                                             \
      (OFFSET) = amo_initial_elimination_offset ((FROM), (TO));  \
    }                                                             \
  while (0)

/* Passing function arguments.  */

#define ACCUMULATE_OUTGOING_ARGS 0

#define PUSH_ARGS 1

#define PUSH_ROUNDING(BYTES) amo_push_rounding (BYTES)

#ifndef CUMULATIVE_ARGS
struct cumulative_args
{
  int ints;
  int last_parm_in_reg;
};

#define CUMULATIVE_ARGS struct cumulative_args
#endif

/* On the AMO architecture, Varargs routines should receive their parameters 
   on the stack.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  amo_init_cumulative_args (&(CUM), (FNTYPE), (LIBNAME))

#define FUNCTION_ARG_REGNO_P(REGNO)  amo_function_arg_regno_p (REGNO)

/* Generating code for profiling - NOT IMPLEMENTED.  */
#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM, LABELNO)      \
{                                               \
  sorry ("profiler support for AMO");          \
}

/* Trampolines for nested functions - NOT SUPPORTED.  */
#define TRAMPOLINE_SIZE    32

/* ADDRESSING MODES.  */

#define CONSTANT_ADDRESS_P(X)       \
  (GET_CODE (X) == LABEL_REF        \
   || GET_CODE (X) == SYMBOL_REF    \
   || GET_CODE (X) == CONST         \
   || GET_CODE (X) == CONST_INT)

#define MAX_REGS_PER_ADDRESS    1

#define HAVE_POST_INCREMENT     0
#define HAVE_POST_DECREMENT     0
#define HAVE_POST_MODIFY_DISP   0
#define HAVE_POST_MODIFY_REG    0

//#ifdef REG_OK_STRICT
//#define AMO_REG_OK_FOR_BASE_P(X)	AMO_REGNO_OK_FOR_BASE_P (REGNO (X))
//#define REG_MODE_OK_FOR_BASE_P(X, MODE)	\
  REGNO_MODE_OK_FOR_BASE_P (REGNO(X), MODE)
//#define REG_OK_FOR_INDEX_P(X)   REGNO_OK_FOR_INDEX_P (REGNO (X))
//#else /* not REG_OK_STRICT.  */
#define AMO_REG_OK_FOR_BASE_P(X)	1
#define REG_MODE_OK_FOR_BASE_P(X, MODE)	1
#define REG_OK_FOR_INDEX_P(X)   1
//#endif /* not REG_OK_STRICT.  */
/* TODO */

/* Assume best case (branch predicted).  */
#define BRANCH_COST(speed_p, predictable_p)       2

#define SLOW_BYTE_ACCESS  1

/* It is as good or better to call a constant function address than to
   call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/* Dividing the output into sections.  */

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#define BSS_SECTION_ASM_OP  "\t.bss"

/* Position independent code (PIC).  */
/* NEAR_PIC for -fpic option.  */

/* TODO */
#define NEAR_PIC 1
                                      
/* FAR_PIC for -fPIC option.  */                                                                                       

#define FAR_PIC  2
/* TODO_END */

#define PIC_OFFSET_TABLE_REGNUM  28

#define LEGITIMATE_PIC_OPERAND_P(X) legitimate_pic_operand_p (X)       

/* Assembler format.  */

/* Character to start a comment.  */
#define ASM_COMMENT_START "#"

#define GLOBAL_ASM_OP "\t.global\t"

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
  asm_fprintf (STREAM, "%U%s", (*targetm.strip_name_encoding) (NAME))

#define ASM_OUTPUT_SYMBOL_REF(STREAM, SYMBOL)   \
  do                                            \
    {                                           \
      const char *rn = XSTR (SYMBOL, 0);        \
      assemble_name (STREAM, rn);               \
      if (SYMBOL_REF_FUNCTION_P (SYMBOL))       \
      {                                         \
        fprintf ((STREAM), "@c");               \
      }                                         \
    }                                           \
  while (0)

#undef ASM_APP_ON
#define ASM_APP_ON   "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF  "#NO_APP\n"

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION	default_elf_asm_named_section

/* TODO */
#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP		"\t.section\t.init"

#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP		"\t.section\t.fini"
/* TODO_END */

/* Instruction output.  */
#define REGISTER_NAMES                                                         \
  {                                                                              \
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",                       \
    "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",                      \
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",                      \
    "r24", "r25", "r26", "r27", "r28", "fp", "sp", "lr"						   \
  }

/* Output of dispatch tables.  */

/* Revisit. No PC relative case as label expressions are not 
   properly supported in binutils else we could have done this:
   #define CASE_VECTOR_PC_RELATIVE (optimize_size ? 1 : 0).  */
#define CASE_VECTOR_PC_RELATIVE 1

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)    \
  ((GET_MODE (BODY) == QImode)                              \
   ? fprintf ((FILE), "\t.byte (.L%d-.L%d) >> 1\n",         \
              VALUE, REL)                                   \
   : fprintf ((FILE), "\t.word (.L%d-.L%d) >> 1\n",         \
              VALUE, REL))

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
  asm_fprintf ((STREAM), "\t.long\t.L%d@c\n", (VALUE))

/* Alignment in assembler file.  */

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  asm_fprintf ((STREAM), "\t.align\t%d\n", 1 << (POWER))

/* Miscellaneous parameters.  */

#define CASE_VECTOR_MODE  Pmode

#define MOVE_MAX 4

#define STORE_FLAG_VALUE  1

#define Pmode SImode

#define FUNCTION_MODE SImode

#endif /* End of GCC_AMO_H.  */
