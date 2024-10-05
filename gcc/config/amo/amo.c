/* Output routines for AMO processor.
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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "regs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "calls.h"
#include "conditions.h"
#include "output.h"
#include "expr.h"
#include "explow.h"
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

/* Definitions.  */

/* Maximum number of register used for passing parameters.  */
#define MAX_REG_FOR_PASSING_ARGS  6

/* Minimum number register used for passing parameters.  */
#define MIN_REG_FOR_PASSING_ARGS  2

/* The maximum count of words supported in the assembly of the architecture in
   a push/pop instruction.  */
#define MAX_COUNT  8

/* Predicate is true if the current function is a 'noreturn' function, 
   i.e. it is qualified as volatile.  */
#define FUNC_IS_NORETURN_P(decl) (TREE_THIS_VOLATILE (decl))

/* Predicate that holds when we need to save registers even for 'noreturn'
   functions, to accommodate for unwinding.  */
#define MUST_SAVE_REGS_P() \
  (flag_unwind_tables || (flag_exceptions && !UI_SJLJ))

/* Nonzero if the rtx X is a signed const int of n bits.  */
#define RTX_SIGNED_INT_FITS_N_BITS(X, n)                \
  ((GET_CODE (X) == CONST_INT                          \
   && SIGNED_INT_FITS_N_BITS (INTVAL (X), n)) ? 1 : 0)

/* Nonzero if the rtx X is an unsigned const int of n bits.  */
#define RTX_UNSIGNED_INT_FITS_N_BITS(X, n)               \
  ((GET_CODE (X) == CONST_INT                            \
   && UNSIGNED_INT_FITS_N_BITS (INTVAL (X), n)) ? 1 : 0)

/* Structure for stack computations.  */

/* variable definitions in the struture
   args_size             Number of bytes saved on the stack for local 
			 variables

   reg_size		 Number of bytes saved on the stack for 
			 non-scratch registers

   total_size 		 The sum of 2 sizes: locals vars and padding byte 
			 for saving the registers. Used in expand_prologue() 
			 and expand_epilogue()

   last_reg_to_save      Will hold the number of the last register the 
			 prologue saves, -1 if no register is saved

   save_regs[16]	 Each object in the array is a register number. 
			 Mark 1 for registers that need to be saved

   num_regs		 Number of registers saved

   initialized		 Non-zero if frame size already calculated, not 
			 used yet

   function_makes_calls  Does the function make calls ? not used yet.  */

struct amo_frame_info
{
  unsigned long var_size;
  unsigned long args_size;
  unsigned int  reg_size;
  unsigned long total_size;
  long          last_reg_to_save;
  long          save_regs[FIRST_PSEUDO_REGISTER];
  int           num_regs;
  int           initialized;
  int           function_makes_calls;
};

/* Current frame information calculated by amo_compute_frame_size.  */
static struct amo_frame_info current_frame_info;

/* Static Variables.  */

/* TARGETM Function Prototypes and forward declarations  */
static void amo_print_operand (FILE *, rtx, int);
static void amo_print_operand_address (FILE *, machine_mode, rtx);

/* Stack layout and calling conventions.  */
#undef  TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX		amo_struct_value_rtx
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY		amo_return_in_memory

/* Target-specific uses of '__attribute__'.  */
#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE 		amo_attribute_table
#undef TARGET_NARROW_VOLATILE_BITFIELD
#define TARGET_NARROW_VOLATILE_BITFIELD hook_bool_void_false

/* EH related.  */
#undef TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE		amo_unwind_word_mode

/* Override Options.  */
#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE  	amo_override_options 

/* Conditional register usuage.  */
#undef TARGET_CONDITIONAL_REGISTER_USAGE 
#define TARGET_CONDITIONAL_REGISTER_USAGE amo_conditional_register_usage

/* Controlling register spills.  */
#undef TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P	amo_class_likely_spilled_p

/* Passing function arguments.  */
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG 		amo_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE 	amo_function_arg_advance
#undef TARGET_RETURN_POPS_ARGS
#define TARGET_RETURN_POPS_ARGS 	amo_return_pops_args

/* Initialize the GCC target structure.  */
#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED	amo_frame_pointer_required
#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE 		amo_can_eliminate
#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS 	amo_legitimize_address
#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P    amo_legitimate_constant_p
#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P     amo_legitimate_address_p

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

/* Returning function value.  */
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE 		amo_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE 		amo_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P 	amo_function_value_regno_p

/* printing the values.  */
#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND 		amo_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS 	amo_print_operand_address

/* Relative costs of operations.  */
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST 		amo_address_cost
#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST 	amo_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST 	amo_memory_move_cost

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT	constant_alignment_word_strings

/* Table of machine attributes.  */
static const struct attribute_spec amo_attribute_table[] = {
  /* ISRs have special prologue and epilogue requirements.  */
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude }.  */
  {"interrupt", 0, 0, false, true, true, false, NULL, NULL},
  {NULL, 0, 0, false, false, false, false, NULL, NULL}
};

/* TARGET_ASM_UNALIGNED_xx_OP generates .?byte directive
   .?byte directive along with @c is not understood by assembler.
   Therefore, make all TARGET_ASM_UNALIGNED_xx_OP same
   as TARGET_ASM_ALIGNED_xx_OP.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP 	TARGET_ASM_ALIGNED_HI_OP
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP 	TARGET_ASM_ALIGNED_SI_OP
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP 	TARGET_ASM_ALIGNED_DI_OP

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS		amo_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK	amo_hard_regno_mode_ok
#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P		amo_modes_tieable_p

/* Target hook implementations.  */

/* Implements hook TARGET_RETURN_IN_MEMORY.  */
static bool
amo_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return ((size == -1) || (size > 8));
}

/* Implement TARGET_CLASS_LIKELY_SPILLED_P.  */
static bool
amo_class_likely_spilled_p (reg_class_t rclass)
{
  if ((rclass) == LONG_REGS || (rclass) == GENERAL_REGS)
    return true;

  return false;
}

static poly_int64
amo_return_pops_args (tree, tree, poly_int64)
{
  return 0;
}

/* Parse relevant options and override.  */
static void
amo_override_options (void)
{
  /* Disable -fdelete-null-pointer-checks option for AMO target.
     Programs which rely on NULL pointer dereferences _not_ halting the 
     program may not work properly with this option. So disable this 
     option.  */
  flag_delete_null_pointer_checks = 0;

  /* FIXME: To avoid spill_failure ICE during exception handling,
   * disable cse_fllow_jumps. The spill error occurs when compiler
   * can't find a suitable candidate in GENERAL_REGS class to reload
   * a 32bit register.
   * Need to find a better way of avoiding this situation. */
  if (flag_exceptions)
    flag_cse_follow_jumps = 0;

  /* If -fpic option  */
  if (flag_pic == NEAR_PIC)
    {
      printf ("flags: flag_pic == NEAR_PIC\n");
    }
}

/* Implements the macro  TARGET_CONDITIONAL_REGISTER_USAGE.  */
static void
amo_conditional_register_usage (void)
{
  if (flag_pic)
    {
      fixed_regs[28] = call_used_regs[28] = 1;
    }
}

/* Stack layout and calling conventions routines.  */

/* Return nonzero if the current function being compiled is an interrupt
   function as specified by the "interrupt" attribute.  */
int
amo_interrupt_function_p (void)
{
  tree attributes;

  attributes = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  return (lookup_attribute ("interrupt", attributes) != NULL_TREE);
}

/* Compute values for the array current_frame_info.save_regs and the variable 
   current_frame_info.reg_size. The index of current_frame_info.save_regs 
   is numbers of register, each will get 1 if we need to save it in the 
   current function, 0 if not. current_frame_info.reg_size is the total sum 
   of the registers being saved.  */
static void
amo_compute_save_regs (void)
{
  unsigned int regno;

  /* Initialize here so in case the function is no-return it will be -1.  */
  current_frame_info.last_reg_to_save = -1;

  /* Initialize the number of bytes to be saved. */
  current_frame_info.reg_size = 0;

  /* No need to save any registers if the function never returns.  */
  if (FUNC_IS_NORETURN_P (current_function_decl) && !MUST_SAVE_REGS_P ())
    return;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (fixed_regs[regno])
	{
	  current_frame_info.save_regs[regno] = 0;
	  continue;
	}

      /* If this reg is used and not call-used (except RA), save it.  */
      if (amo_interrupt_function_p ())
	{
	  if (!crtl->is_leaf && call_used_regs[regno])
	    /* This is a volatile reg in a non-leaf interrupt routine - save 
	       it for the sake of its sons.  */
	    current_frame_info.save_regs[regno] = 1;
	  else if (df_regs_ever_live_p (regno))
	    /* This reg is used - save it.  */
	    current_frame_info.save_regs[regno] = 1;
	  else
	    /* This reg is not used, and is not a volatile - don't save.  */
	    current_frame_info.save_regs[regno] = 0;
	}
      else
	{
	  /* If this reg is used and not call-used (except RA), save it.  */
	  if (df_regs_ever_live_p (regno)
	      && (!call_used_regs[regno] || regno == RETURN_ADDRESS_REGNUM))
      current_frame_info.save_regs[regno] = 1;
	  else
	    current_frame_info.save_regs[regno] = 0;
	}
    }

  /* Save registers so the exception handler can modify them.  */
  if (crtl->calls_eh_return)
    {
      unsigned int i;

      for (i = 0;; ++i)
	{
	  regno = EH_RETURN_DATA_REGNO (i);
	  if (INVALID_REGNUM == regno)
	    break;
	  current_frame_info.save_regs[regno] = 1;
	}
    }

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (current_frame_info.save_regs[regno] == 1)
      {
        current_frame_info.last_reg_to_save = regno;
        current_frame_info.reg_size += UNITS_PER_WORD;
      }
}

/* Compute the size of the local area and the size to be adjusted by the
   prologue and epilogue.  */
static void
amo_compute_frame (void)
{
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;

  /* Padding needed for each element of the frame.  */
  current_frame_info.var_size = get_frame_size ();

  /* Align to the stack alignment.  */
  padding_locals = current_frame_info.var_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  current_frame_info.var_size += padding_locals;
  current_frame_info.total_size
    = (current_frame_info.var_size
       + (ACCUMULATE_OUTGOING_ARGS
	  ? (HOST_WIDE_INT) crtl->outgoing_args_size : 0));
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */
int
amo_initial_elimination_offset (int from, int to)
{
  /* Compute this since we need to use current_frame_info.reg_size.  */
  amo_compute_save_regs ();

  /* Compute this since we need to use current_frame_info.var_size.  */
  amo_compute_frame ();

  if (((from) == FRAME_POINTER_REGNUM) && ((to) == STACK_POINTER_REGNUM))
    return (ACCUMULATE_OUTGOING_ARGS
	    ? (HOST_WIDE_INT) crtl->outgoing_args_size : 0);
  else if (((from) == ARG_POINTER_REGNUM) && ((to) == FRAME_POINTER_REGNUM))
    return (current_frame_info.reg_size + current_frame_info.var_size);
  else if (((from) == ARG_POINTER_REGNUM) && ((to) == STACK_POINTER_REGNUM))
    return (current_frame_info.reg_size + current_frame_info.var_size 
	    + (ACCUMULATE_OUTGOING_ARGS
	       ? (HOST_WIDE_INT) crtl->outgoing_args_size : 0));
  else
    gcc_unreachable ();
}

/* Register Usage.  */

/* Return the class number of the smallest class containing reg number REGNO.
   This could be a conditional expression or could index an array.  */
enum reg_class
amo_regno_reg_class (int regno)
{
  if ((regno >= 0) && (regno < FIRST_PSEUDO_REGISTER))
    return LONG_REGS;

  return NO_REGS;
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
amo_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  On the AMO architecture, all
   registers can hold all modes, except that double precision floats
   (and double ints) must fall on even-register boundaries.  */

static bool
amo_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if ((GET_MODE_SIZE (mode) >= 4) && (regno == 11))
    return false;
 
  if (mode == DImode || mode == DFmode)
    {
      if ((regno > 8) || (regno & 1))
	return false;
      return true;
    }

  if ((TARGET_INT32)
       && ((regno >= 12) && (GET_MODE_SIZE (mode) < 4 )))
     return false;

  /* CC can only hold CCmode values.  */
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return false;
  return true;
}

/* Implement TARGET_MODES_TIEABLE_P.  */
static bool
amo_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  return GET_MODE_CLASS (mode1) == GET_MODE_CLASS (mode2);
}

/* Returns register number for function return value.*/
static inline unsigned int
amo_ret_register (void)
{
  return 0;
}

/* Implements hook TARGET_STRUCT_VALUE_RTX.  */
static rtx
amo_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
                       int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, amo_ret_register ());
}

/* Returning function value.  */

/* Worker function for TARGET_FUNCTION_VALUE_REGNO_P.  */
static bool
amo_function_value_regno_p (const unsigned int regno)
{
  return (regno == amo_ret_register ());
}

/* Create an RTX representing the place where a
   library function returns a value of mode MODE.  */
static rtx
amo_libcall_value (machine_mode mode,
		    const_rtx func ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, amo_ret_register ());
}

/* Create an RTX representing the place where a
   function returns a value of data type VALTYPE.  */
static rtx
amo_function_value (const_tree type,
		     const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		     bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (type), amo_ret_register ());
}

/* Passing function arguments.  */

/* If enough param regs are available for passing the param of type TYPE return
   the number of registers needed else 0.  */
static int
enough_regs_for_param (CUMULATIVE_ARGS * cum, const_tree type,
		       machine_mode mode)
{
  int type_size;
  int remaining_size;

  if (mode != BLKmode)
    type_size = GET_MODE_BITSIZE (mode);
  else
    type_size = int_size_in_bytes (type) * BITS_PER_UNIT;

  remaining_size = BITS_PER_WORD * (MAX_REG_FOR_PASSING_ARGS
				    - (MIN_REG_FOR_PASSING_ARGS + cum->ints) +
				    1);

  /* Any variable which is too big to pass in two registers, will pass on
     stack.  */
  if ((remaining_size >= type_size) && (type_size <= 2 * BITS_PER_WORD))
    return (type_size + BITS_PER_WORD - 1) / BITS_PER_WORD;

  return 0;
}

/* Implements the macro FUNCTION_ARG defined in amo.h.  */
static rtx
amo_function_arg (cumulative_args_t cum_v, machine_mode mode,
		   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  cum->last_parm_in_reg = 0;

  /* function_arg () is called with this type just after all the args have 
     had their registers assigned. The rtx that function_arg returns from 
     this type is supposed to pass to 'gen_call' but currently it is not 
     implemented.  */
  if (type == void_type_node)
    return NULL_RTX;

  if (targetm.calls.must_pass_in_stack (mode, type) || (cum->ints < 0))
    return NULL_RTX;

  if (mode == BLKmode)
    {
      /* Enable structures that need padding bytes at the end to pass to a
         function in registers.  */
      if (enough_regs_for_param (cum, type, mode) != 0)
	{
	  cum->last_parm_in_reg = 1;
	  return gen_rtx_REG (mode, MIN_REG_FOR_PASSING_ARGS + cum->ints);
	}
    }

  if ((MIN_REG_FOR_PASSING_ARGS + cum->ints) > MAX_REG_FOR_PASSING_ARGS)
    return NULL_RTX;
  else
    {
      if (enough_regs_for_param (cum, type, mode) != 0)
	{
	  cum->last_parm_in_reg = 1;
	  return gen_rtx_REG (mode, MIN_REG_FOR_PASSING_ARGS + cum->ints);
	}
    }

  return NULL_RTX;
}

/* Implements the macro INIT_CUMULATIVE_ARGS defined in amo.h.  */
void
amo_init_cumulative_args (CUMULATIVE_ARGS * cum, tree fntype,
			   rtx libfunc ATTRIBUTE_UNUSED)
{
  tree param, next_param;

  cum->ints = 0;

  /* Determine if this function has variable arguments.  This is indicated by
     the last argument being 'void_type_mode' if there are no variable
     arguments.  Change here for a different vararg.  */
  for (param = (fntype) ? TYPE_ARG_TYPES (fntype) : 0;
       param != NULL_TREE; param = next_param)
    {
      next_param = TREE_CHAIN (param);
      if ((next_param == NULL_TREE) && (TREE_VALUE (param) != void_type_node))
	{
	  cum->ints = -1;
	  return;
	}
    }
}

/* Implements the macro FUNCTION_ARG_ADVANCE defined in amo.h.  */
static void
amo_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS * cum = get_cumulative_args (cum_v);

  /* l holds the number of registers required.  */
  int l = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;

  /* If the parameter isn't passed on a register don't advance cum.  */
  if (!cum->last_parm_in_reg)
    return;

  if (targetm.calls.must_pass_in_stack (mode, type) || (cum->ints < 0))
    return;

  if ((mode == SImode) || (mode == HImode)
      || (mode == QImode) || (mode == DImode))
    {
      if (l <= 1)
	cum->ints += 1;
      else
	cum->ints += l;
    }
  else if ((mode == SFmode) || (mode == DFmode))
    cum->ints += l;
  else if ((mode) == BLKmode)
    {
      if ((l = enough_regs_for_param (cum, type, mode)) != 0)
	cum->ints += l;
    }
  return;
}

/* Implements the macro FUNCTION_ARG_REGNO_P defined in amo.h.
   Return nonzero if N is a register used for passing parameters.  */
int
amo_function_arg_regno_p (int n)
{
  return ((n <= MAX_REG_FOR_PASSING_ARGS) && (n >= MIN_REG_FOR_PASSING_ARGS));
}

/* Addressing modes. 
   Following set of function implement the macro GO_IF_LEGITIMATE_ADDRESS
   defined in amo.h.  */

/* Helper function to check if is a valid base register that can
   hold address.  */
static int
amo_addr_reg_p (rtx addr_reg)
{
  rtx reg;

  if (REG_P (addr_reg))
    reg = addr_reg;
  else if ((GET_CODE (addr_reg) == SUBREG)
	   && REG_P (SUBREG_REG (addr_reg))
	   && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (addr_reg)))
	       <= UNITS_PER_WORD))
    reg = SUBREG_REG (addr_reg);
  else
    return FALSE;

  if (GET_MODE (reg) != Pmode)
    return FALSE;

  return TRUE;
}

/* Decompose Address
   This function decomposes the address returns the type of address
   as defined in enum amo_addrtype.  It also fills the parameter *out.
   The decomposed address can be used for two purposes.  One to 
   check if the address is valid and second to print the address
   operand.

   Following tables list valid address supported in AMOC/C+ architectures.
   Legend: 
   aN : Absoulte address N-bit address
   R  : One 16-bit register
   RP : Consecutive two 16-bit registers or one 32-bit register
   I  : One 32-bit register
   dispN : Signed displacement of N-bits

   ----Code addresses----
   Branch operands:
   disp9        : AMO_ABSOLUTE       (disp)
   disp17       : AMO_ABSOLUTE       (disp)
   disp25       : AMO_ABSOLUTE       (disp)
   RP + disp25  : AMO_REGP_REL       (base, disp)

   Jump operands:
   RP           : AMO_REGP_REL       (base, disp=0)
   a24          : AMO_ABSOLUTE       (disp)

   ----Data addresses----
   a20          : AMO_ABSOLUTE       (disp)                near (1M)
   a24          : AMO_ABSOLUTE       (disp)                medium  (16M)
   R  + d20     : AMO_REG_REL        (base,  disp)         near (1M+64K)
   RP + d4      : AMO_REGP_REL       (base,  disp)         far  (4G)
   RP + d16     : AMO_REGP_REL       (base,  disp)         far  (4G)
   RP + d20     : AMO_REGP_REL       (base,  disp)         far  (4G)
   I            : *** Valid but port does not support this
   I  + a20     : *** Valid but port does not support this
   I  + RP + d14: AMO_INDEX_REGP_REL (base,  index, disp)  far  (4G)
   I  + RP + d20: AMO_INDEX_REGP_REL (base,  index, disp)  far  (4G) */

enum amo_addrtype
amo_decompose_address (rtx addr, struct amo_address *out,
			bool debug_print, bool treat_as_const)
{
  rtx base = NULL_RTX, disp = NULL_RTX, index = NULL_RTX;
  int code = -1;
  enum amo_addrtype retval = AMO_INVALID;

  switch (GET_CODE (addr))
    {
    case CONST_INT:
      fprintf (stderr, "\nconst_int:");
      debug_rtx (addr);

    case LABEL_REF:
      retval = AMO_ABSOLUTE;
      disp = addr;
      /* 1 - indicates non-function symbol.  */
      code = 1;
      break;

    case SYMBOL_REF:
      /* Absolute address (known at link time).  */
      retval = AMO_ABSOLUTE;
      disp = addr;
      /* This is a code address if symbol_ref is a function.  */
      /* 2 indicates func sym.  */
      code = SYMBOL_REF_FUNCTION_P (addr) ? 2 : 0;
      break;

    case REG:
      /* Register relative address. */
      /* Assume REG fits in a single register. */
      retval = AMO_REG_REL;
      base = addr;
      break;

    case PLUS:
      switch (GET_CODE (XEXP (addr, 0)))
      {
      case REG:
        /* REG + DISP20.  */
        /* All Reg relative addresses having a displacement needs 
          to fit in 20-bits.  */
        disp = XEXP (addr, 1);
        switch (GET_CODE (XEXP (addr, 1)))
          {
          case CONST_INT:
            /* Shall fit in 20-bits.  */
            if (!UNSIGNED_INT_FITS_N_BITS (INTVAL (disp), 20))
              return AMO_INVALID;
            code = 0;
            break;

          case UNSPEC:
            switch (XINT (XEXP (addr, 1), 1))
            {
              case UNSPEC_LIBRARY_OFFSET:
              default:
                gcc_unreachable ();
            }
            break;

          case LABEL_REF:
          case SYMBOL_REF:
          case CONST:
            /* This is also a valid expression for address.
              However, we cannot ascertain if the resultant
              displacement will be valid 20-bit value.  Therefore, 
              lets not allow such an expression for now.  This will 
              be updated when  we find a way to validate this 
              expression as legitimate address. 
              Till then fall through AMO_INVALID.  */
          default:
            return AMO_INVALID;
          }

        /* Now check if REG can fit into single or pair regs.  */
        retval = AMO_REG_REL;
        base = XEXP (addr, 0);
        break;

      default:
        return AMO_INVALID;
      }
      break;

    default:
      return AMO_INVALID;
    }

  /* Check if the base and index registers are valid.  */
  if (base && !(amo_addr_reg_p (base)))
    return AMO_INVALID;
  if (base && !(AMO_REG_OK_FOR_BASE_P (base)))
    return AMO_INVALID;
  if (index && !(REG_OK_FOR_INDEX_P (index)))
    return AMO_INVALID;

  /* Write the decomposition to out parameter.  */
  out->base = base;
  out->disp = disp;
  out->index = index;
  out->code = code;

  return retval;
}

/* Return non-zero value if 'x' is legitimate PIC operand
   when generating PIC code.  */
int
legitimate_pic_operand_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
      return 0;
    case LABEL_REF:
      return 0;
    case CONST:
      /* REVISIT: Use something like symbol_referenced_p.  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	      || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF)
	  && (GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT))
	return 0;
      break;
    case MEM:
      return legitimate_pic_operand_p (XEXP (x, 0));
    default:
      break;
    }
  return 1;
}

/* Convert a non-PIC address in `orig' to a PIC address in `reg'.

     Input            Output (-f pic)        Output (-f PIC)
     orig             reg
                                                                                                                             
     C1   symbol           symbol@BRO (r12)        symbol@GOT (r12)
                                                                                                                             
     C2   symbol + offset  symbol+offset@BRO (r12) symbol+offset@GOT (r12)
                                                                                                                             
     NOTE: @BRO is added using unspec:BRO
     NOTE: @GOT is added using unspec:GOT.  */
rtx
legitimize_pic_address (rtx orig, machine_mode mode ATTRIBUTE_UNUSED,
			rtx reg)
{
  /* First handle a simple SYMBOL_REF or LABEL_REF.  */
  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      if (reg == 0)
	reg = gen_reg_rtx (Pmode);

      if (flag_pic == NEAR_PIC)
	{
	  /* Unspec to handle -fpic option.  */
	  emit_insn (gen_unspec_bro_addr (reg, orig));
	  emit_insn (gen_addsi3 (reg, reg, pic_offset_table_rtx));
	}
      else if (flag_pic == FAR_PIC)
	{
	  /* Unspec to handle -fPIC option.  */
	  emit_insn (gen_unspec_got_addr (reg, orig));
	}
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      /* To handle (symbol + offset).  */
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);

      base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
      offset = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
				       base == reg ? 0 : reg);

      /* REVISIT: Optimize for const-offsets.  */
      emit_insn (gen_addsi3 (reg, base, offset));

      return reg;
    }
  return orig;
}

/* Implementation of TARGET_LEGITIMATE_ADDRESS_P.  */
static bool
amo_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
			   rtx addr, bool strict)
{
  enum amo_addrtype addrtype;
  struct amo_address address;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr,
	       "\n======\nTARGET_LEGITIMATE_ADDRESS_P, mode = %s, strict = %d",
	       GET_MODE_NAME (mode), strict);
      debug_rtx (addr);
    }
  addrtype = amo_decompose_address (addr, &address,
				     (TARGET_DEBUG_ADDR ? 1 : 0), FALSE);

  if (TARGET_DEBUG_ADDR)
    {
      const char *typestr;

      switch (addrtype)
	{
	case AMO_INVALID:
	  typestr = "invalid";
	  break;
	case AMO_ABSOLUTE:
	  typestr = "absolute";
	  break;
	case AMO_REG_REL:
	  typestr = "register relative";
	  break;
	case AMO_REGP_REL:
	  typestr = "register pair relative";
	  break;
	case AMO_INDEX_REGP_REL:
	  typestr = "index + register pair relative";
	  break;
	default:
	  gcc_unreachable ();
	}
      fprintf (stderr, "\namo address type: %s\n", typestr);
    }

  if (addrtype == AMO_INVALID)
    return FALSE;

  if (strict)
    {
      if (address.base
	  && !REGNO_MODE_OK_FOR_BASE_P (REGNO (address.base), mode))
	{
	  if (TARGET_DEBUG_ADDR)
	    fprintf (stderr, "base register not strict\n");
	  return FALSE;
	}
      if (address.index && !REGNO_OK_FOR_INDEX_P (REGNO (address.index)))
	{
	  if (TARGET_DEBUG_ADDR)
	    fprintf (stderr, "index register not strict\n");
	  return FALSE;
	}
    }

  /* Return true if addressing mode is register relative.  */
  if (flag_pic)
    {
      if (addrtype == AMO_REG_REL || addrtype == AMO_REGP_REL)
	return TRUE;
      else
	return FALSE;
    }

  return TRUE;
}

/* Routines to compute costs.  */

/* Return cost of the memory address x.  */
static int
amo_address_cost (rtx addr, machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED)
{
  enum amo_addrtype addrtype;
  struct amo_address address;
  int cost = 2;

  addrtype = amo_decompose_address (addr, &address, 0, FALSE);

  gcc_assert (addrtype != AMO_INVALID);

  /* AMO_ABSOLUTE            : 3
     AMO_REG_REL  (disp !=0) : 4
     AMO_REG_REL  (disp ==0) : 5
     AMO_REGP_REL (disp !=0) : 6
     AMO_REGP_REL (disp ==0) : 7
     AMO_INDEX_REGP_REL (disp !=0) : 8
     AMO_INDEX_REGP_REL (disp ==0) : 9.  */
  switch (addrtype)
    {
    case AMO_ABSOLUTE:
      cost += 1;
      break;
    case AMO_REGP_REL:
      cost += 2;
      /* Fall through.  */
    case AMO_REG_REL:
      cost += 3;
      if (address.disp)
	cost -= 1;
      break;
    case AMO_INDEX_REGP_REL:
      cost += 7;
      if (address.disp)
	cost -= 1;
    default:
      break;
    }

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n======\nmacro TARGET_ADDRESS_COST = %d\n", cost);
      debug_rtx (addr);
    }

  return cost;
}


/* Implement `TARGET_REGISTER_MOVE_COST'.  */
static int
amo_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			 reg_class_t from ATTRIBUTE_UNUSED, reg_class_t to)
{
  return (to != GENERAL_REGS ? 8 : 2);
}

/* Implement `TARGET_MEMORY_MOVE_COST'.  */

/* Return the cost of moving data of mode MODE between a register of class
   CLASS and memory; IN is zero if the value is to be written to memory,
   nonzero if it is to be read in. This cost is relative to those in
   REGISTER_MOVE_COST.  */
static int
amo_memory_move_cost (machine_mode mode,
		       reg_class_t rclass ATTRIBUTE_UNUSED,
		       bool in ATTRIBUTE_UNUSED)
{
  /* One LD or ST takes twice the time of a simple reg-reg move.  */
  if (reg_classes_intersect_p (rclass, GENERAL_REGS))
    return (4 * amo_hard_regno_nregs (0, mode));
  else
    return (100);
}

/* Instruction output.  */

/* Check if a const_double is ok for amo store-immediate instructions.  */
int
amo_const_double_ok (rtx op)
{
  if (GET_MODE (op) == SFmode)
    {
      long l;
      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (op), l);
      return UNSIGNED_INT_FITS_N_BITS (l, 4) ? 1 : 0;
    }

  return ((UNSIGNED_INT_FITS_N_BITS (CONST_DOUBLE_LOW (op), 4)) &&
	  (UNSIGNED_INT_FITS_N_BITS (CONST_DOUBLE_HIGH (op), 4))) ? 1 : 0;
}

/* Returns bit position of first 0 or 1 bit.
   It is safe to assume val as 16-bit wide.  */
int
amo_operand_bit_pos (int val, int bitval)
{
  int i;
  if (bitval == 0)
    val = ~val;

  for (i = 0; i < 16; i++)
    if (val & (1 << i))
      break;
  return i;
}

/* Implements the macro PRINT_OPERAND defined in amo.h.  */
static void
amo_print_operand (FILE * file, rtx x, int code)
{
  int ptr_dereference = 0;

  switch (code)
    {
    case '$':
      putc ('$', file);
      return;

    case 'b':
      /* Print the immediate address for bal 
         'b' is used instead of 'a' to avoid compiler calling
         the GO_IF_LEGITIMATE_ADDRESS which cannot
         perform checks on const_int code addresses as it
         assumes all const_int are data addresses.  */
      fprintf (file, "0x%lx", INTVAL (x));
      return;

    case 'r':
      /* Print bit position of first 0.  */
      fprintf (file, "%d", amo_operand_bit_pos (INTVAL (x), 0));
      return;

    case 's':
      /* Print bit position of first 1.  */
      fprintf (file, "%d", amo_operand_bit_pos (INTVAL (x), 1));
      return;
    case 'g':
      /* 'g' is used for implicit mem: dereference.  */
      ptr_dereference = 1;
      /* FALLTHRU */
    case 'f':
    case 0:
      /* default.  */
      switch (GET_CODE (x))
	{
	case REG:
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  return;

	case MEM:
	  output_address (GET_MODE (x), XEXP (x, 0));
	  return;

	case CONST_DOUBLE:
	  {
	    long l;

	    REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), l);

	    fprintf (file, "$0x%lx", l);
	    return;
	  }
	case CONST_INT:
	  {
	    fprintf (file, "$%ld", INTVAL (x));
	    return;
	  }
	case UNSPEC:
	  switch (XINT (x, 1))
	    {
	    default:
	      gcc_unreachable ();
	    }
	  break;

	default:
	  if (!ptr_dereference)
	    {
	      putc ('$', file);
	    }
	  amo_print_operand_address (file, VOIDmode, x);
	  return;
	}
      gcc_unreachable ();
    default:
      output_operand_lossage ("invalid %%xn code");
    }

  gcc_unreachable ();
}

/* Implements the macro PRINT_OPERAND_ADDRESS defined in amo.h.  */

static void
amo_print_operand_address (FILE * file, machine_mode /*mode*/, rtx addr)
{
  enum amo_addrtype addrtype;
  struct amo_address address;

  /* Decompose the address. Also ask it to treat address as constant.  */
  addrtype = amo_decompose_address (addr, &address, 0, TRUE);

  if (address.disp && GET_CODE (address.disp) == UNSPEC)
    {
      debug_rtx (addr);
    }

  switch (addrtype)
    {
    case AMO_REG_REL:
      if (address.disp)
	{
	  if (GET_CODE (address.disp) == UNSPEC)
	    amo_print_operand (file, address.disp, 0);
	  else
	    output_addr_const (file, address.disp);
	}
      else
	fprintf (file, "0");
      fprintf (file, "(%s)", reg_names[REGNO (address.base)]);
      break;

    case AMO_ABSOLUTE:
      if (address.disp)
	output_addr_const (file, address.disp);
      else
	fprintf (file, "0");
      break;

    case AMO_INDEX_REGP_REL:
      fprintf (file, "[%s]", reg_names[REGNO (address.index)]);
      /* Fall through.  */
    case AMO_REGP_REL:
      if (address.disp)
	{
	  if (GET_CODE (address.disp) == UNSPEC)
	    amo_print_operand (file, address.disp, 0);
	  else
	    output_addr_const (file, address.disp);
	}
      else
	fprintf (file, "0");
      fprintf (file, "(%s,%s)", reg_names[REGNO (address.base) + 1],
	       reg_names[REGNO (address.base)]);
      break;
    default:
      printf ("----------here -----------------\n");
      debug_rtx (addr);
      gcc_unreachable ();
    }
}

/* compare-and-branch */
void
amo_expand_compare_branch (rtx *operands)
{
	rtx insn, code, label;

  /* operand[0]: comparison operator  */
  /* operand[1]: first operand        */
  /* operand[2]: second operand       */
  /* operand[3]: destination          */
	rtx opn1 = operands[1];
	rtx opn2 = operands[2];

	machine_mode mode = GET_MODE (opn1);

	gcc_assert(operands[3] != NULL);

	if (!register_operand (opn1, mode))
  {
		opn1 = force_reg (mode, opn1);
  }

	if (!register_operand (opn2, mode))
  {
		opn2 = force_reg (mode, opn2);
  }

	code = gen_rtx_fmt_ee (GET_CODE (operands[0]), mode, opn1, opn2);
	label = gen_rtx_LABEL_REF (VOIDmode, operands[3]);

	insn = gen_rtx_SET (pc_rtx, gen_rtx_IF_THEN_ELSE (VOIDmode, code, label, pc_rtx));
	emit_jump_insn (insn); 
}

/* Machine description helper functions.  */

/* Called from amo.md. The return value depends on the parameter push_or_pop:
   When push_or_pop is zero -> string for push instructions of prologue.
   When push_or_pop is nonzero -> string for pop/popret/retx in epilogue.
   Relies on the assumptions:
   1. RA is the last register to be saved.
   2. The maximal value of the counter is MAX_COUNT.  */
char *
amo_prepare_push_pop_string (int push_or_pop)
{
  char insn_buf[50];
  char *buffer, *return_str;
  int ins_num;
  int i;

  buffer = (char *) xmalloc (160);
  return_str = (char *) xmalloc (160);

  memset (buffer, 0, 160);
  memset (return_str, 0, 160);

  i = 0;
  ins_num = 0;
  /* insn */
  while (i <= current_frame_info.last_reg_to_save)
  {
    if (!current_frame_info.save_regs[i])
    {
      /* Move to next reg and break.  */
      ++i;
      continue;
    }
    
    if (push_or_pop == 0)
    {
      /* push */
      if (ins_num == 0)
        sprintf (insn_buf, "\tstr\t\t[sp], %s\n", reg_names[i]);
      else
        sprintf (insn_buf, "\tstr\t\t[sp, $%d], %s\n", ins_num * 4, reg_names[i]);

      strcpy (buffer, return_str);
      strcpy (return_str, insn_buf);
      strcat (return_str, buffer);
    }
    else
    {
      /* pop */
      if (ins_num == 0)
        sprintf (insn_buf, "\tldr\t\t%s, [sp]\n", reg_names[i]);
      else
        sprintf (insn_buf, "\tldr\t\t%s, [sp, $%d]\n", reg_names[i], ins_num * 4);

      strcat (return_str, insn_buf);
    }
    ++ins_num;
    ++i;
  }
   
  if (push_or_pop == 0)
  {
    /* push */
    /*
    /*    sub   sp, sp, $8
    /*    str   [sp, 4], ra
    /*    str   [sp], fp
    /*
    */
    if (ins_num)
    {
      sprintf (insn_buf, "\tsub\t\tsp, sp, $%d\n", ins_num * 4);

      strcpy (buffer, return_str);
      strcpy (return_str, insn_buf);
      strcat (return_str, buffer);
    }
  }
  else
  {
    /* pop */
    /*
    /*    ldr   fp, [sp]
    /*    ldr   lr, [sp, 4]
    /*    add   sp, sp, $8
    /*    jmp   lr
    /*
    */
    if (ins_num)
    {
      sprintf (insn_buf, "\tadd\t\tsp, sp, $%d\n", ins_num * 4);
      strcat (return_str, insn_buf);
    }

    /* you may need fallowing conditional statement for interrupt handling */
    /* if (print_ra && !amo_interrupt_function_p () && !crtl->calls_eh_return) */
    sprintf (insn_buf, "\tjmp\t\tlr\n");
    strcat (return_str, insn_buf);
  }
  
  /* remove newline */
  return_str[strlen (return_str) - 1] = 0;
  return return_str + 1;
}

void
amo_expand_prologue (void)
{
  rtx insn;

  amo_compute_frame ();
  amo_compute_save_regs ();

  /* If there is no need in push and adjustment to sp, return.  */
  if ((current_frame_info.total_size + current_frame_info.reg_size) == 0)
    return;

  if (current_frame_info.last_reg_to_save != -1)
    {
      /* If there are registers to push.  */
      insn = emit_insn (gen_push_for_prologue (GEN_INT (current_frame_info.reg_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (current_frame_info.total_size > 0)
    {
      insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				    GEN_INT (-current_frame_info.total_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (frame_pointer_needed)
    {
      /* Initialize the frame pointer with the value of the stack pointer
         pointing now to the locals.  */
      insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
    }
}

/* Generate insn that updates the stack for local variables and padding 
   for registers we save.   - Generate the appropriate return insn.  */
void
amo_expand_epilogue (void)
{
  rtx insn;

  /* Nonzero if we need to return and pop only RA. This will generate a
     different insn. This differentiate is for the peepholes for call as 
     last statement in function.  */
  int only_popret_RA = (current_frame_info.save_regs[RETURN_ADDRESS_REGNUM] && (current_frame_info.reg_size == UNITS_PER_WORD));
  
  if (frame_pointer_needed)
    {
      /* Restore the stack pointer with the frame pointers value.  */
      insn = emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
    }

  if (current_frame_info.total_size > 0)
    {
      insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				    GEN_INT (current_frame_info.total_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (crtl->calls_eh_return)
    {
      printf ("3. calls_eh_return\n");
      /* Add this here so that (r5, r4) is actually loaded with the adjustment
         value; otherwise, the load might be optimized away...
         NOTE: remember to subtract the adjustment before popping the regs
         and add it back before returning.  */
      insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				    EH_RETURN_STACKADJ_RTX));
    }

  if (amo_interrupt_function_p ())
    {
      insn = emit_jump_insn (gen_interrupt_return ());
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (crtl->calls_eh_return)
    {
      printf ("5. calls_eh_return\n");
      /* Special case, pop what's necessary, adjust SP and jump to (RA).  */
      insn = emit_jump_insn (gen_pop_and_popret_return 
			     (GEN_INT (current_frame_info.reg_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (current_frame_info.last_reg_to_save == -1)
    {
      printf ("6. reg_to_save == -1\n");
      /* Nothing to pop.  */
      /* Don't output jump for interrupt routine, only retx.  */
      emit_jump_insn (gen_jump_return ());
    }
  else if (only_popret_RA)
    {
      printf ("7. only_popret_RA\n");
      insn = emit_jump_insn (gen_popret_RA_return ());
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    {
      insn = emit_jump_insn (gen_pop_and_popret_return 
			     (GEN_INT (current_frame_info.reg_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

/* Implements FRAME_POINTER_REQUIRED.  */
static bool
amo_frame_pointer_required (void)
{
  return (cfun->calls_alloca || crtl->calls_eh_return
	  || cfun->has_nonlocal_label || crtl->calls_eh_return);
}

static bool
amo_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == STACK_POINTER_REGNUM ? !frame_pointer_needed : true);
}


/* A C compound statement that attempts to replace X with
   a valid memory address for an operand of mode MODE. WIN
   will be a C statement label elsewhere in the code.
   X will always be the result of a call to break_out_memory_refs (),
   and OLDX will be the operand that was given to that function to
   produce X.
   The code generated by this macro should not alter the
   substructure of X.  If it transforms X into a more legitimate form, 
   it should assign X (which will always be a C variable) a new value.  */
static rtx
amo_legitimize_address (rtx x, rtx orig_x ATTRIBUTE_UNUSED,
			 machine_mode mode ATTRIBUTE_UNUSED)
{
  if (flag_pic)
    return legitimize_pic_address (orig_x, mode, NULL_RTX);
  else
    return x;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P
   Nonzero if X is a legitimate constant for an immediate
   operand on the target machine.  You can assume that X
   satisfies CONSTANT_P. In amoc treat legitimize float 
   constant as an immediate operand.  */
static bool
amo_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED,
			    rtx x ATTRIBUTE_UNUSED)
{
  return 1;
}

void
notice_update_cc (rtx exp)
{
  if (GET_CODE (exp) == SET)
    {
      /* Jumps do not alter the cc's.  */
      if (SET_DEST (exp) == pc_rtx)
	return;

      /* Moving register or memory into a register:
         it doesn't alter the cc's, but it might invalidate
         the RTX's which we remember the cc's came from.
         (Note that moving a constant 0 or 1 MAY set the cc's).  */
      if (REG_P (SET_DEST (exp))
	  && (REG_P (SET_SRC (exp)) || GET_CODE (SET_SRC (exp)) == MEM))
	{
	  return;
	}

      /* Moving register into memory doesn't alter the cc's.
         It may invalidate the RTX's which we remember the cc's came from.  */
      if (GET_CODE (SET_DEST (exp)) == MEM && REG_P (SET_SRC (exp)))
	{
	  return;
	}
    }

  CC_STATUS_INIT;
  return;
}

static scalar_int_mode
amo_unwind_word_mode (void)
{
  return SImode;
}


/* Implement PUSH_ROUNDING.  */

poly_int64
amo_push_rounding (poly_int64 bytes)
{
  return (bytes + 1) & ~1;
}

/* Initialize 'targetm' variable which contains pointers to functions 
   and data relating to the target machine.  */

struct gcc_target targetm = TARGET_INITIALIZER;
