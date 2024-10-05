/* Prototypes for exported functions defined in amo.c
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

#ifndef GCC_AMO_PROTOS_H
#define GCC_AMO_PROTOS_H

/* Register usage.  */
extern enum reg_class amo_regno_reg_class (int);

/* Passing function arguments.  */
extern int amo_function_arg_regno_p (int);

#ifdef TREE_CODE
#ifdef RTX_CODE

extern void amo_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx);

#endif /* RTX_CODE.  */
#endif /* TREE_CODE.  */

#ifdef RTX_CODE

#define ADDRESS_TYPE_DATA 0
#define ADDRESS_TYPE_CODE 1
#define ADDRESS_TYPE_LABEL 2 /* function label */

/* Addressing Modes.  */
struct amo_address
{
  rtx base;	 	/* Base register: Any register or register pair.  */
  rtx index;		/* Index register: If one is present.  */
  rtx disp;		/* Displacement or Absolute address.  */
  int type;		/* Whether the address is code address. 
			   0 - data, 1 - code label, 2 - function label.  */
};

enum amo_addrtype
{
  AMO_INVALID,
  AMO_REG_ABSOLUTE,
  AMO_REG_INDEX,
  AMO_ABSOLUTE
};

extern void notice_update_cc (rtx);
extern int amo_operand_bit_pos (int val, int bitval);
extern enum amo_addrtype amo_decompose_address (rtx addr,
						  struct amo_address *out);
extern int amo_const_double_ok (rtx op);
extern int legitimate_pic_operand_p (rtx);
extern rtx legitimize_pic_address (rtx, machine_mode, rtx);

extern void amo_expand_compare_branch (rtx *operands);

/* Prologue/Epilogue functions.  */
extern int amo_initial_elimination_offset (int, int);
extern char *amo_prepare_push_pop_string (int);
extern void amo_expand_prologue (void);
extern void amo_expand_epilogue (void);

#endif /* RTX_CODE.  */

/* Handling the "interrupt" attribute.  */
extern int amo_interrupt_function_p (void);
extern poly_int64 amo_push_rounding (poly_int64);

#endif /* Not GCC_AMO_PROTOS_H.  */ 
