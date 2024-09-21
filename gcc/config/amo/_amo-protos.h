/* Exported definitions for target machine AMO.
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

#ifndef _AMO_PROTOS_H_
#define _AMO_PROTOS_H_

extern bool amo_dummy_insn_cond(machine_mode mdoe);
extern enum reg_class amo_regno_to_class(int);
extern int amo_valid_regno_for_base_p(int);
extern int amo_valid_regno_for_index_p(int);

extern void amo_init_cumulative_args(CUMULATIVE_ARGS *ca,
                                       tree fn_type,
                                       rtx libname,
                                       tree fn_decl,
                                       int num_named);

extern HOST_WIDE_INT amo_initial_elimination_offset(int, int);

extern bool amo_valid_movsi_insn(machine_mode mdoe, rtx operands[2]);

extern void amo_expand_movi(machine_mode mode, rtx *operands);
extern void amo_expand_cond_branch(rtx *operands);

extern void amo_expand_call(rtx *operands);

extern void amo_expand_prologue (void);
extern void amo_expand_epilogue (int, int, bool);

#endif
