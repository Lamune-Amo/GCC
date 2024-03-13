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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "cfgrtl.h"
#include "output.h"
#include "calls.h"
#include "alias.h"
#include "explow.h"
#include "expr.h"
#include "reload.h"
#include "langhooks.h"
#include "gimplify.h"
#include "builtins.h"
#include "dumpfile.h"
#include "hw-doloop.h"
#include "rtl-iter.h"
#include "tm-constrs.h"

/* this header must be included after the genric above */
#include "target-def.h"

/* Forward declarations of hooks we are going to assign/implement below */
static bool amo_lra_p(void);
static bool amo_legitimate_address_p(machine_mode, rtx, bool);
static bool amo_must_pass_in_stack(machine_mode, const_tree);

static rtx amo_function_arg(
  cumulative_args_t, machine_mode, const_tree, bool named);

static void amo_function_arg_advance(
  cumulative_args_t, machine_mode, const_tree, bool);

static rtx amo_function_value(const_tree, const_tree, bool);
static bool amo_function_value_regno_p (const unsigned int);

static void amo_print_operand(FILE *, rtx, int);
static void amo_print_operand_address(FILE *, machine_mode, rtx);

static void amo_init_libfuncs();
static rtx amo_libcall_value(machine_mode, const_rtx);

static bool amo_frame_pointer_required (void);

/* Additional to definitions in 'amo.h' we implement more complex hooks
   in this file. Things that need to be globally visible (e.g. referenced
   by the machine description) are exported in 'amo-protos.h' */

#undef TARGET_LRA_P
#define TARGET_LRA_P amo_lra_p

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE amo_function_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P amo_function_value_regno_p

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG amo_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE amo_function_arg_advance

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK amo_must_pass_in_stack

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P amo_legitimate_address_p

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS amo_init_libfuncs

#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE amo_libcall_value

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND amo_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS amo_print_operand_address

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED amo_frame_pointer_required 

/* NOTE, we also need to provide a struct for machine functions, the
   compilation breaks without it */
struct GTY(()) machine_function
{
  int reg_val;
};

struct gcc_target targetm = TARGET_INITIALIZER;

/******************************************************************************/
/* Return true if you prefer to use LRA instead of the original RA. For the
   time being, stick to the old reloader */

static bool amo_lra_p(void)
{
  return false;
}

/******************************************************************************/
/* Since a register can generally belong to more than just one class, we
   specify the smallest one */

enum reg_class amo_regno_to_class (int regno)
{
  if (regno >= 0 && regno < FIRST_PSEUDO_REGISTER)
  {
    return reg_class::GP_REGS;
  }

  return reg_class::NO_REGS;
}

/******************************************************************************/
/* Memory can be addressed by having an arbitrary base register, thus, no
   restrictions */

int amo_valid_regno_for_base_p(int regno)
{
  /* physical registers only, virtual registers make
     actually no sense here */
  if (regno >= 0 && regno < FIRST_PSEUDO_REGISTER)
  {
    return 1;
  }

  return 0;
}

/******************************************************************************/
/* The same as for address base registers applies to index registers, no
   restrictions */

int amo_valid_regno_for_index_p(int regno)
{
  return amo_valid_regno_for_base_p(regno);
}

/******************************************************************************/
/* Specify whether 'regno' corresponds to a function argument register */

static bool amo_valid_arg_regno(int regno)
{
  return regno >= FIRST_ARG_REGNUM && regno <= LAST_ARG_REGNUM;
}

/******************************************************************************/
/* Check whether the specified register is a function value return register */

static bool amo_function_value_regno_p(const unsigned int regno)
{
  return (RET_VALUE_REGNUM == regno);
}

/******************************************************************************/
/* See if we are dealing with integral types with length matching the return
   register width (i.e. 32 bit). If not, just create a dummy rtx for the
   return value */

static rtx amo_function_value(const_tree ret_type,
                                const_tree fn_type ATTRIBUTE_UNUSED,
                                bool outgoing ATTRIBUTE_UNUSED)
{
  //if (INTEGRAL_TYPE_P(ret_type) && TYPE_PRECISION(ret_type) < BITS_PER_WORD)
  //  return gen_rtx_REG(SImode, RET_VALUE_REGNUM);

  /* not yet sure how a result is split into two registers */
  return gen_rtx_REG(TYPE_MODE(ret_type), RET_VALUE_REGNUM);
}

/******************************************************************************/
/* Advance in the function argument list past argument given by mode/type,
   i.e. extend the cumulative list by the argument */

static void amo_function_arg_advance(cumulative_args_t cargs,
                                       machine_mode mode ATTRIBUTE_UNUSED,
                                       const_tree type ATTRIBUTE_UNUSED,
                                       bool named ATTRIBUTE_UNUSED)
{
  return;
}

/******************************************************************************/
/* Specify what kind of arguments can't be passed in registers */

static bool amo_must_pass_in_stack(machine_mode mode,
                                     const_tree type)
{
  return false;
}

/******************************************************************************/
/* Need to tell what is considered to be a 'legitimate address'. Allow just
   anything for the first build */

static bool amo_legitimate_address_p(machine_mode mode,
                                       rtx x,
                                       bool strict)
{
	int base_code, off_code;

	if (SImode == mode || HImode == mode || QImode == mode)
	{
		/* single operand addressing, register or immediate */
		if (GET_CODE(x) == REG)
		{
			return true;
		}
		else if (PLUS == GET_CODE(x))
		{
			/* base + offset addressing */
			base_code = GET_CODE (XEXP (x, 0));
			off_code = GET_CODE (XEXP (x, 1));

			if (base_code == REG && off_code == CONST_INT)
			{
				return true;
			}
		}
	}

	return false;
}

/******************************************************************************/
/* If frame register elimination is supported/desired, compute the initial
   elimination offset */

HOST_WIDE_INT amo_initial_elimination_offset (int from,
                                                int to ATTRIBUTE_UNUSED)
{
  return 0;
}

/******************************************************************************/
/* Init cumulative arguments for a function call */

void amo_init_cumulative_args(CUMULATIVE_ARGS *cum,
                                tree fntype ATTRIBUTE_UNUSED,
                                rtx libname ATTRIBUTE_UNUSED,
                                tree fndecl ATTRIBUTE_UNUSED,
                                int num_named ATTRIBUTE_UNUSED)
{
  return;
}

/******************************************************************************/
/* Determine where to put an argument to a function. Value is zero to push
   argument on the stack, or a register in which to store the argument. */

static rtx amo_function_arg(cumulative_args_t cargs,
                              machine_mode mode,
                              const_tree type,
                              bool named)
{
  return NULL_RTX;
}

/******************************************************************************/
/* When building in the beginning, we are forced to provide at least one
   condition (zero-sized array error). Thus, provide a dummy to overcome
   this.
   NOTE, it is erroneous to use this to reject particular operand combos,
   i.e. having one and the same instruction alternative being ok for
   for some operand combinations and not ok for other (if you want to
   do this, use additional instruction alternatives). Use conditions to
   allow/reject entire patterns depending on a given ISA (e.g. see Arm,
   some patterns are only allowed in A32 resp. T32 mode */

bool amo_valid_movsi_insn(machine_mode mode, rtx operands[2])
{

  return true;
}

/******************************************************************************/
/* Register/rename additional target libfuncs here */

void amo_expand_call(rtx *operands)
{
  rtx body = operands[0];
  rtx dst = XEXP(body, 0);
  rtx something = XEXP(body, 1);

  /* everything that is not a symbol references goes into a register,
     resulting in an indirect call */
  /*
  if (SYMBOL_REF != GET_CODE(dst))
  {
    dst = force_reg (FUNCTION_MODE, dst);
  }
  */

//  emit_call_insn(dst);
}

static bool
amo_frame_pointer_required (void)
{
	return true;

  //return false;
}


static void amo_init_libfuncs(void)
{
  return;
}

/******************************************************************************/
/* Generate a return value rtx for libcalls in the given mode */

static rtx amo_libcall_value(machine_mode mode,
                               const_rtx fun)
{
	return gen_rtx_REG (mode, 0);
}

/******************************************************************************/
/* Print instruction operands properly. Memory operands should trigger the
   the 'print_operand_address' hook (i.e.'amo_print_operand_address') */

static void amo_print_operand(FILE *file,
                                rtx op,
                                int letter)
{
	if (op != 0)
	{
		if (GET_CODE(op) == REG)
		{
			fprintf(file, "%s", reg_names[REGNO(op)]);
		}
		else if (GET_CODE(op) == MEM)
		{
			output_address (GET_MODE(op), XEXP(op, 0));
		}
		else
		{
			output_addr_const(file, op);
		}
	}
}

/******************************************************************************/
/* Print memory address operands properly (e.g. 'port[r62 + -4]') */

static void amo_print_operand_address(FILE *file,
                                        machine_mode mode,
                                        rtx addr)
{
  if (NULL != addr)
  {
    if (REG == GET_CODE(addr))
    {
      fprintf(file, "%s", reg_names[REGNO(addr)]);
      return;
    }
    else if (PLUS == GET_CODE(addr))
    {
      rtx base = XEXP(addr, 0);
      rtx offset = XEXP(addr, 1);

      int base_code = GET_CODE(base);
      int off_code= GET_CODE(offset);

      if (REG == base_code && CONST_INT == off_code)
      {
		  fprintf(file, "%s, $%d",
			reg_names[REGNO(base)], INTVAL(offset));
		  return;
      }
    }
  }

  fprintf (file, "<BAD ADDRESS>");
}

bool amo_dummy_insn_cond(machine_mode mdoe)
{
	return true;
}

void amo_expand_movi(machine_mode mode, rtx *operands)
{
	if (GET_CODE(operands[0]) == MEM && GET_CODE(operands[1]) == MEM)
	{
		operands[1] = force_reg(mode, operands[1]);
	}

	//emit_insn(gen_movsi_insn(operands[0], operands[1]));
}

void amo_expand_cond_branch(rtx *operands)
{
	rtx cc = operands[0];
	rtx cmp0 = operands[1];
	rtx cmp1 = operands[2];
	rtx dst = operands[3];

	rtx insn, code, label;

	machine_mode mode = GET_MODE(cmp0);
	rtx_code cc_code = GET_CODE(cc);

	gcc_assert(dst != NULL);

	if (!register_operand(cmp0, mode))
		cmp0 = force_reg(mode, cmp0);

	if (GET_CODE(cmp1) != REG && GET_CODE(cmp1) != CONST_INT)
		cmp1 = force_reg(mode, cmp1);

	code = gen_rtx_fmt_ee(cc_code, mode, cmp0, cmp1);
	label = gen_rtx_LABEL_REF(VOIDmode, dst);

	insn = gen_rtx_SET(pc_rtx, gen_rtx_IF_THEN_ELSE(VOIDmode, code, label, pc_rtx));

	emit_jump_insn(insn); 
}

void amo_expand_prologue (void)
{
  HARD_REG_SET set;

	printf ("get_frame_size: %d\n", get_frame_size());

  for (int reg = 0; reg < 32; reg++)
  {
    /* Do not push/pop __tmp_reg__, __zero_reg__, as well as
        any global register variables.  */

    if (fixed_regs[reg])
      continue;
    
    printf ("reg: %d\n", reg);
  }

}

void amo_expand_epilogue (int need_return, int eh_return, bool sibcall_p)
{
//	emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode, 31)));
}

/* this header is auto-generated in build/gcc and must be incldued at the end */
#include "gt-amo.h"
