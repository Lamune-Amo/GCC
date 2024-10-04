;; GCC machine description for AMO.
;; Copyright (C) 2012-2018 Free Software Foundation, Inc.
;; Contributed by KPIT Cummins Infosystems Limited.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>. 

;;  Register numbers
(define_constants
  [(SP_REGNUM 30); Stack pointer
   (RA_REGNUM 31); Return address
  ]
)

;; Predicates & Constraints
(include "predicates.md")
(include "constraints.md")

;; UNSPEC usage
(define_constants
  [(UNSPEC_PIC_ADDR             0)
   (UNSPEC_PIC_LOAD_ADDR        1)
   (UNSPEC_LIBRARY_OFFSET       2)
   (UNSPEC_SH_LIB_PUSH_R12      3)
   (UNSPEC_SH_LIB_POP_R12       4)
   (UNSPEC_RETURN_ADDR          5)
  ]
)

;; Attributes
(define_attr "length" "" (const_int 2))

(define_asm_attributes
  [(set_attr "length" "2")]
)

;;  Mode Macro Definitions
(define_mode_iterator AMOIM [QI HI SI])
(define_mode_iterator LONG   [SI SF])
(define_mode_iterator ALLMTD [QI HI SI SF DI DF])
(define_mode_iterator SHORT  [QI HI])
(define_mode_attr tIsa       [(QI "b") (HI "h") (SI "") (SF "")])
(define_mode_attr lImmArith  [(QI "4") (HI "4") (SI "6") (SF "6")])
(define_mode_attr lImmArithD [(QI "4") (HI "4") (SI "6") (SF "6") (DI "12") (DF "12")])
(define_mode_attr iF         [(QI "i") (HI "i") (SI "i") (SF "F")])
(define_mode_attr iFD        [(DI "i") (DF "F")])
(define_mode_attr LL         [(QI "L") (HI "L")])
(define_mode_attr shImmBits  [(QI "3") (HI "4") (SI "5")])

; In QI mode we push 2 bytes instead of 1 byte.
(define_mode_attr pushCnstr [(QI "X") (HI "<") (SI "<") (SF "<") (DI "<") (DF "<")])

; tpush will be used to generate the 'number of registers to push' in the 
; push instruction.
(define_mode_attr tpush [(QI "1") (HI "1") (SI "2") (SF "2") (DI "4") (DF "4")])

;;  Code Macro Definitions
(define_code_attr  szPat   [(sign_extend "")  (zero_extend "zero_")])
(define_code_attr  szIsa   [(sign_extend "$3") (zero_extend "$2")])
(define_code_attr  szIua   [(sign_extend "$1") (zero_extend "$0")])

(define_code_iterator sz_xtnd    [ sign_extend       zero_extend])
(define_code_iterator any_cond   [eq ne gt gtu lt ltu ge geu le leu])
(define_code_iterator plusminus  [plus minus])

(define_code_attr plusminus_insn [(plus "add") (minus "sub")])
(define_code_attr plusminus_flag [(plus "PLUS") (minus "MINUS")])
(define_code_attr comm 		 [(plus "%") (minus "")])

(define_code_iterator any_logic  [and ior xor])
(define_code_attr logic 	 [(and "and") (ior "or") (xor "xor")])
(define_code_attr any_logic_insn [(and "and") (ior "ior") (xor "xor")])

(define_mode_iterator QH 	 [QI HI])
(define_mode_attr qh 		 [(QI "qi") (HI "hi")])
(define_mode_attr QHsz 		 [(QI "2,2,2") (HI "2,2,4")])
(define_mode_attr QHsuffix 	 [(QI "b") (HI "w")])


;;  Function Prologue and Epilogue
(define_expand "prologue"
  [(const_int 0)]
  ""
  {
    amo_expand_prologue ();
    DONE;
  }
)

(define_insn "push_for_prologue"
  [(set (reg:SI SP_REGNUM)
	(minus:SI (reg:SI SP_REGNUM)
		  (match_operand:SI 0 "immediate_operand" "i")))]
  "reload_completed"
  {
    return amo_prepare_push_pop_string (0);
  }
  [(set_attr "length" "4")]
)

(define_expand "epilogue"
  [(return)]
  ""
  {
    amo_expand_epilogue ();
    DONE;
  }
)

(define_insn "pop_and_popret_return"
  [(set (reg:SI SP_REGNUM)
	(plus:SI (reg:SI SP_REGNUM)
		 (match_operand:SI 0 "immediate_operand" "i")))
   (use (reg:SI RA_REGNUM))
   (return)]
  "reload_completed"
  {
    return amo_prepare_push_pop_string (1);
  }
  [(set_attr "length" "4")]
)

(define_insn "popret_RA_return"
  [(use (reg:SI RA_REGNUM))
   (return)]
  "reload_completed"
  "popret\tra"
  [(set_attr "length" "2")]
)

;; Arithmetic Instruction  Patterns

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r")
		 (match_operand:SI 2 "reg_si_int_operand" "r,N")))]
  ""
	"add\t\t%0, %1, %2"
  [(set_attr "length" "2,2")]
)

;;  Subtract Instruction
(define_insn "subsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (minus:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,N")))]
	""
	"sub\t\t%0, %1, %2"
  [(set_attr "length" "2,2")]
)

; Logical and/ior/xor "andsi3/iorsi3/xorsi3"
(define_insn "<any_logic_insn>si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(any_logic:SI (match_operand:SI 1 "register_operand" "r,r")
		      (match_operand:SI 2 "reg_si_int_operand" "r,P")))]
  ""
  "<logic>\t\t%0, %1, %2"
  [(set_attr "length" "2,2")]
)

;;  Sign and Zero Extend Instructions
(define_insn "<szPat>extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sz_xtnd:SI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "ext\t\t%0, %1, <szIsa>"
  [(set_attr "length" "2")]
)

(define_insn "<szPat>extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sz_xtnd:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "ext\t\t%0, %1, <szIua>"
  [(set_attr "length" "2")]
)

;;  One's Complement
(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(not:SI (match_operand:SI 1 "register_operand" "r,P")))]
  ""
  "not\t\t%0, %1"
  [(set_attr "length" "2")]
)

(define_insn "ashlsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (ashift:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:QI 2 "nonmemory_operand" "r,S")))]
	""
	"lsl\t\t%0, %1, %2"
  [(set_attr "length" "2")]
)

(define_insn "lshrsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:QI 2 "nonmemory_operand" "r,S")))]
	""
	"lsr\t\t%0, %1, %2"
  [(set_attr "length" "2")]
)

(define_insn "ashrsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:QI 2 "nonmemory_operand" "r,S")))]
	""
	"asr\t\t%0, %1, %2"
  [(set_attr "length" "2")]
)

;;  Move Instructions

;; Move any non-immediate operand 0 to a general operand 1.
;; This applies only before starting the reload process
;; Operand 0 is not a register operand of type mode MODE
;; If Operand 0 is a push operand of type mode MODE
;; then, if Operand 1 is a non-SP register
;; then, Operand 1 = copy_to_mode_reg (<MODE>mode, Operand 1)
;; endif
;; else
;; if Operand 1 is either register or 4-bit immediate constant
;; then, Operand 1 = copy_to_mode_reg (<MODE>mode, Operand 1)
;; endif
;; endif
;;
;; What does copy_to_mode_reg (mode, rtx val) do?
;; Copy the value into new temp reg and return the reg where the
;; mode of the new reg is always mode MODE when value is constant
;;
;; Why should copy_to_mode_reg be called?
;; All sorts of move are nor supported by AMO. Therefore, 
;; when unsupported move is encountered, the additional instructions 
;; will be introduced for the purpose.
;;
;; A new move insn is inserted for Op 1 when one of the following
;; conditions is met.
;; Case 1:  Op 0 is push_operand
;;          Op 1 is SP register
;;
;; Case 2:  Op 0 is not push_operand
;;          Op 1 is neither register nor unsigned 4-bit immediate

(define_expand "mov<mode>"
  [(set (match_operand:ALLMTD 0 "nonimmediate_operand" "")
	(match_operand:ALLMTD 1 "general_operand" ""))]
  ""
  {
    if (!(reload_in_progress || reload_completed))
      {
	/* Only if Op0 is a register operand.  */
	if (!register_operand (operands[0], <MODE>mode))
	  {
	    if (push_operand (operands[0], <MODE>mode)) 
	      {
		/* Use copy_to_mode_reg only if the register needs 
		to be pushed is SP as AMO does not support pushing SP.  */
		if (!nosp_reg_operand (operands[1], <MODE>mode))
		  operands[1] = copy_to_mode_reg (<MODE>mode, operands[1]);
	      }
	  }

	  /* If operand-1 is a symbol, convert it into a BRO or GOT Format.  */
	  if (flag_pic && ! legitimate_pic_operand_p (operands[1]))
    {
      printf ("this can't be called\n");
      operands[1] = legitimize_pic_address (operands[1], <MODE>mode, 0);
    }
      }
  }
)

; All long (SI, SF) register move, load and store operations
; The print_operand will take care of printing the register pair 
; when mode is SI/SF and register is in REGS.
(define_insn "*mov<mode>_long"
  [(set (match_operand:LONG 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:LONG 1 "general_operand" "r,<iF>,m,r"))]
  ""
  "@
  mov\t\t%0, %1
  mov\t\t%0, %1
  ldr\t\t%0, %1
  str\t\t%0, %1"
  [(set_attr "length" "2,2,2,2")]
)

;; All short (QI, HI) register move, load and store operations
(define_insn "*mov<mode>_short"
  [(set (match_operand:SHORT 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:SHORT 1 "general_operand" "i,r,m,r"))]
  ""
  "@
  mov\t\t%0, %1
  mov\t\t%0, %1
  ldr<tIsa>\t%0, %1
  str<tIsa>\t%0, %1"
  [(set_attr "length" "2,2,2,2")]
)

;;  Compare Instructions
(define_expand "cbranchsi4"
  [(set (pc)
    (if_then_else (match_operator:VOID 0 "comparison_operator"
            [(match_operand:SI 1 "general_operand")
             (match_operand:SI 2 "general_operand")])
             (label_ref (match_operand 3 "" ""))
                        (pc)))]
  ""
  {
    amo_expand_cond_branch(operands);
    DONE;
  }
)

(define_insn "cond_branch_eq"
  [(set (pc)
    (if_then_else (
        eq:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "beq %0, %1, %2"
)

(define_insn "cond_branch_ne"
  [(set (pc)
    (if_then_else (
        ne:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bne %0, %1, %2"
)

(define_insn "cond_branch_gt"
  [(set (pc)
    (if_then_else (
        gt:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "blt %1, %0, %2"
)

(define_insn "cond_branch_ge"
  [(set (pc)
    (if_then_else (
        ge:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "ble %1, %0, %2"
)

(define_insn "cond_branch_lt"
  [(set (pc)
    (if_then_else (
        lt:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "blt %0, %1, %2"
)

(define_insn "cond_branch_le"
  [(set (pc)
    (if_then_else (
        le:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "ble %0, %1, %2"
)

(define_insn "cond_branch_gtu"
  [(set (pc)
    (if_then_else (
        gtu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bltu %1, %0, %2"
)

(define_insn "cond_branch_geu"
  [(set (pc)
    (if_then_else (
        geu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bleu %1, %0, %2"
)

(define_insn "cond_branch_ltu"
  [(set (pc)
    (if_then_else (
        ltu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bltu %0, %1, %2"
)

(define_insn "cond_branch_leu"
  [(set (pc)
    (if_then_else (
        leu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "register_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bleu %0, %1, %2"
)

;;  Jumps and Branches
(define_insn "indirect_jump_return"
  [(set (pc)
	  (reg:SI RA_REGNUM))
   (return)]
  "reload_completed"
  "jmp\t\tlr"
  [(set_attr "length" "2")]
)

(define_insn "jump_return"
  [(unspec:SI [(const_int 0)] UNSPEC_RETURN_ADDR)
   (return)]
  "reload_completed"
  "jmp\t\tlr"
  [(set_attr "length" "2")]
)

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "reg_or_sym_operand" "r,i"))]
  ""
  "@
  jmp\t\t%0
  jmp\t\t%a0"
  [(set_attr "length" "2,6")]
)

(define_insn "interrupt_return"
  [(unspec_volatile [(const_int 0)] 0)
   (return)]
  ""
  {
    return amo_prepare_push_pop_string (1);
  }
  [(set_attr "length" "14")]
)

(define_insn "jump_to_imm"
  [(set (pc)
	(match_operand 0 "jump_imm_operand" "i"))]
  ""
  "jmp_imm\t%c0"
  [(set_attr "length" "6")]
)

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jmp\t\t%l0"
  [(set_attr "length" "6")]
)

;;  Table Jump
(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref:SI (match_operand 1 "" "")))]
  "!flag_pic"
  "jmp\t\t%0"
  [(set_attr "length" "2")]
)

;;  Call Instructions
(define_expand "call"
  [(call (match_operand:SI 0 "memory_operand" "")
	 (match_operand 1 "" ""))]
  ""
  {
    if (flag_pic && ! legitimate_pic_operand_p (operands[0]))
      {
  printf ("this can't be called\n");
	operands[0] = gen_const_mem (SImode,
	legitimize_pic_address (XEXP (operands[0], 0), Pmode, 0));
	emit_call_insn (gen_amo_call (operands[0], operands[1]));
      }
    else
      emit_call_insn (gen_amo_call (operands[0], operands[1]));
      DONE;
  }
)

(define_expand "amo_call"
  [(parallel
    [(call (match_operand:SI 0 "memory_operand" "")
	   (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNUM))])]
  ""
  ""
)

(define_insn "amo_call_insn_branch_pic"
  [(call (mem:SI (match_operand:SI 0 "call_imm_operand" "i"))
	 (match_operand 1 "" ""))
   (clobber (match_operand:SI 2 "register_operand" "+r"))]
  "flag_pic == FAR_PIC"
  {
    if (GET_CODE (operands[0]) != CONST_INT)
      return "loadd\t%g0, %2 \n\tjal %2";
    else
      return "jal %2";
  }
  [(set_attr "length" "8")]
)

(define_insn "amo_call_insn_branch"
  [(call (mem:SI (match_operand:SI 0 "call_imm_operand" "i"))
	 (match_operand 1 "" ""))
   (clobber (match_operand:SI 2 "register_operand" "+r"))]
  "flag_pic == 0 || flag_pic == NEAR_PIC"
  {
    /* Print the immediate address for bal 
       'b' is used instead of 'a' to avoid compiler calling
       the GO_IF_LEGITIMATE_ADDRESS which cannot
       perform checks on const_int code addresses as it
       assumes all const_int are data addresses.
    */
    if (GET_CODE (operands[0]) != CONST_INT)
      return "bal (ra), %a0";
    else
      operands[4] = GEN_INT ((INTVAL (operands[0]))>>1);
      return "movd\t%g4,\t(r1,r0)\n\tjal\t(r1,r0)";	
  }
  [(set_attr "length" "6")]
)

(define_insn "amo_call_insn_jump"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "r"))
	 (match_operand 1 "" ""))
   (clobber (match_operand:SI 2 "register_operand" "+r"))]
  ""
  "jal\t%0"
  [(set_attr "length" "2")]
)

;;  Call Value Instructions

(define_expand "call_value"
  [(set (match_operand 0 "general_operand" "")
	(call (match_operand:SI 1 "memory_operand" "")
	      (match_operand 2 "" "")))]
  ""
  {
    if (flag_pic && !legitimate_pic_operand_p (operands[1]))
      {
  printf ("this can't be called\n");
	operands[1] = gen_const_mem (SImode,
	legitimize_pic_address (XEXP (operands[1], 0), Pmode, 0));
	emit_call_insn (gen_amo_call_value (operands[0], operands[1], operands[2]));
      }
    else
	emit_call_insn (gen_amo_call_value (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_expand "amo_call_value"
  [(parallel
    [(set (match_operand 0 "general_operand" "")
	  (call (match_operand 1 "memory_operand" "")
		(match_operand 2 "" "")))
     (clobber (reg:SI RA_REGNUM))])]
  ""
  ""
)

(define_insn "amo_call_value_insn_branch_pic"
  [(set (match_operand 0 "" "=g")
	(call (mem:SI (match_operand:SI 1 "call_imm_operand" "i"))
	      (match_operand 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" "+r"))]
  "flag_pic == FAR_PIC"
  {
    if (GET_CODE (operands[1]) != CONST_INT)
      return "loadd\t%g1, %3 \n\tjal %3";
    else
      return "jal %3";
  }
  [(set_attr "length" "8")]
)

(define_insn "amo_call_value_insn_branch"
  [(set (match_operand 0 "" "=g")
	(call (mem:SI (match_operand:SI 1 "call_imm_operand" "i"))
	      (match_operand 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" "+r"))]
  "flag_pic == 0 || flag_pic == NEAR_PIC"
  {
    /* Print the immediate address for bal 
       'b' is used instead of 'a' to avoid compiler calling
       the GO_IF_LEGITIMATE_ADDRESS which cannot
       perform checks on const_int code addresses as it
       assumes all const_int are data addresses.
    */
    if (GET_CODE (operands[1]) != CONST_INT) 
      return "bal (ra), %a1";
    else
      {
	operands[4] = GEN_INT ((INTVAL (operands[1]))>>1);
        return "movd\t%g4,\t(r1,r0)\n\tjal\t(r1,r0)";	
      }
  }
  [(set_attr "length" "6")]
)


(define_insn "amo_call_value_insn_jump"
  [(set (match_operand 0 "" "=g")
	(call (mem:SI (match_operand:SI 1 "register_operand" "r"))
	      (match_operand 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" "+r"))]
  ""
  "jal\t%1"
  [(set_attr "length" "2")]
)


;;  Nop
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop\t"
)

;; PIC
/* When generating pic, we need to load the symbol offset into a register.
   So that the optimizer does not confuse this with a normal symbol load
   we use an unspec.  The offset will be loaded from a constant pool entry,
   since that is the only type of relocation we can use.  */
                                                                                                                            
(define_insn "unspec_bro_addr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand 1 "" "")] UNSPEC_PIC_ADDR))]
  ""
  "movd \t%f1, %0"
  [(set_attr "length" "4")]
)

(define_insn "unspec_got_addr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand 1 "" "")] UNSPEC_PIC_LOAD_ADDR))]
  ""
  "loadd \t%g1, %0"
  [(set_attr "length" "6")]
)
