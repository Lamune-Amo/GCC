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

/* Enumeration giving the various data models we support.  */
enum data_model_type
{
  DM_DEFAULT,		/* Default data model (in AMOC/C+ - up to 16M).  */
  DM_NEAR,		/* Near data model    (in AMOC/C+ - up to 1M).  */
  DM_FAR,		/* Far data model     (in AMOC+   - up to 4G)
			   (in AMOC    - up to 16M).  */
  ILLEGAL_DM		/* Illegal data model.  */
};

#ifdef RTX_CODE

/* Addressing Modes.  */
struct amo_address
{
  rtx base;	 	/* Base register: Any register or register pair.  */
  rtx index;		/* Index register: If one is present.  */
  rtx disp;		/* Displacement or Absolute address.  */
  enum data_model_type data;	/* data ref type.  */
  int code;		/* Whether the address is code address. 
			   0 - data, 1 - code label, 2 - function label.  */
};

enum amo_addrtype
{
  AMO_INVALID,
  AMO_REG_REL,
  AMO_REGP_REL,
  AMO_INDEX_REGP_REL,
  AMO_ABSOLUTE
};

extern void notice_update_cc (rtx);
extern int amo_operand_bit_pos (int val, int bitval);
extern void amo_decompose_const (rtx x, int *code,
				  enum data_model_type *data,
				  bool treat_as_const);
extern enum amo_addrtype amo_decompose_address (rtx addr,
						  struct amo_address *out,
						  bool debug_print,
						  bool treat_as_const);
extern int amo_const_double_ok (rtx op);
extern int legitimate_pic_operand_p (rtx);
extern rtx legitimize_pic_address (rtx, machine_mode, rtx);


/* Prologue/Epilogue functions.  */
extern int amo_initial_elimination_offset (int, int);
extern char *amo_prepare_push_pop_string (int);
extern void amo_expand_prologue (void);
extern void amo_expand_epilogue (void);

#endif /* RTX_CODE.  */

/* Handling the "interrupt" attribute.  */
extern int amo_interrupt_function_p (void);
extern bool amo_is_data_model (enum data_model_type);
extern poly_int64 amo_push_rounding (poly_int64);

#endif /* Not GCC_AMO_PROTOS_H.  */ 
