;; GCC machine description for AMO. 
;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(include "constraints.md")

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  "amo_dummy_insn_cond(QImode)"
  {
     amo_expand_movi(SImode, operands);
  }
)

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
  {
     amo_expand_movi(HImode, operands);
  }
)

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
  {
     amo_expand_movi(QImode, operands);
  }
)

(define_insn "movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,m")
        (match_operand:SI 1 "general_operand" "i,r,m,r"))]
  ""
  "@
   mov %0, $%1
   mov %0, %1
   ldr %0, [%1]
   str [%0], %1"
)

(define_insn "movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,m")
        (match_operand:HI 1 "general_operand" "i,r,m,r"))]
  ""
  "@
   mov %0, $%1
   mov %0, %1
   ldrh %0, [%1]
   strh [%0], %1"
)

(define_insn "movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,m")
        (match_operand:QI 1 "general_operand" "i,r,m,r"))]
  ""
  "@
   mov %0, $%1
   mov %0, %1
   ldrb %0, [%1]
   strb [%0], %1"
)

(define_insn "pushsi1"
  [(set (mem:SI (pre_dec:SI (reg:SI 29)))
        (match_operand:SI 0 "register_operand" "r"))]
  ""
  "sub sp, sp, $4\nstr [sp], %0"
)

(define_insn "popsi_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mem:SI (post_inc:SI (reg:SI 29))))]
  ""
  "ldr %0, [sp]\nadd sp, sp, $4"
)

;; direct unconditional
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0)))]
  ""
  "jmp %0"
)

;; indirect unconditional
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jmp %0"
)

;; later for call instructions
(define_insn "return"
  [(return)]
  ""
  "jmp r31"
)



(define_insn "addsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (plus:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Ia")))]
	""
	"@
	 add %0, %1, %2
	 add %0, %1, $%2"
)

(define_insn "subsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (minus:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Ia")))]
	""
	"@
	 sub %0, %1, %2
	 sub %0, %1, $%2"
)

(define_insn "ashlsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (ashift:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Il")))]
	""
	"@
	 lsl %0, %1, %2
	 lsl %0, %1, $%2"
)

(define_insn "lshrsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Il")))]
	""
	"@
	 lsr %0, %1, %2
	 lsr %0, %1, $%2"
)

(define_insn "ashrsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Il")))]
	""
	"@
	 asr %0, %1, %2
	 asr %0, %1, $%2"
)

(define_insn "andsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (and:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Il")))]
	""
	"@
	 and %0, %1, %2
	 and %0, %1, $%2"
)

(define_insn "iorsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (ior:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Il")))]
	""
	"@
	 or %0, %1, %2
	 or %0, %1, $%2"
)

(define_insn "xorsi3"
	[(set (match_operand:SI 0 "register_operand" "=r,r")
		  (xor:SI (match_operand:SI 1 "register_operand" "r,r")
				   (match_operand:SI 2 "nonmemory_operand" "r,Il")))]
	""
	"@
	 xor %0, %1, %2
	 xor %0, %1, $%2"
)

;;------------------------------------------------------------------------------
;; Sign/zero extension
;;
;; 'port8/16' load zero-extended values (bytes/half-words) into registers
;; 'sport8/16' load sign-extended values (bytes/half-words) into registers
;;
;; There is no dedicated zero-extension instruction for registers on our
;; target, thus, leave it to gcc to find alternatives if/when required
;;------------------------------------------------------------------------------

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r")))]
  ""
  "ext %0, %1, $3"
)

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r")))]
  ""
  "ext %0, %1, $1"
)

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r")))]
  ""
  "ext %0, %1, $2"
)

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r")))]
  ""
  "ext %0, %1, $0"
)

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
     /* see auto-generated 'insn-emit.c' in build/gcc on when and
        when not to use 'DONE' */
     DONE;
  }
)

(define_insn "cond_branch_eq"
  [(set (pc)
    (if_then_else (
        eq:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "beq %0, %1, %2"
)

(define_insn "cond_branch_ne"
  [(set (pc)
    (if_then_else (
        ne:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bne %0, %1, %2"
)

(define_insn "cond_branch_gt"
  [(set (pc)
    (if_then_else (
        gt:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "blt %1, %0, %2"
)

(define_insn "cond_branch_ge"
  [(set (pc)
    (if_then_else (
        ge:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "ble %1, %0, %2"
)

(define_insn "cond_branch_lt"
  [(set (pc)
    (if_then_else (
        lt:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "blt %0, %1, %2"
)

(define_insn "cond_branch_le"
  [(set (pc)
    (if_then_else (
        le:SI (match_operand:SI 0 "register_operand" "r")
              (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "ble %0, %1, %2"
)

;; unsigned condition branches, if you fail to see them in your code,
;; you most probably messed up machine modes (and/or more)

(define_insn "cond_branch_gtu"
  [(set (pc)
    (if_then_else (
        gtu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bltu %1, %0, %2"
)

(define_insn "cond_branch_geu"
  [(set (pc)
    (if_then_else (
        geu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bleu %1, %0, %2"
)

(define_insn "cond_branch_ltu"
  [(set (pc)
    (if_then_else (
        ltu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bltu %0, %1, %2"
)

(define_insn "cond_branch_leu"
  [(set (pc)
    (if_then_else (
        leu:SI (match_operand:SI 0 "register_operand" "r")
               (match_operand:SI 1 "nonmemory_operand" "r"))
        (label_ref (match_operand 2))
        (pc)))]
  ""
  "bleu %0, %1, %2"
)

(define_expand "call"
    [(call (match_operand:SI 0 "memory_operand" "")
       (match_operand 1 "general_operand" ""))]
           ""
           {
			gcc_assert (MEM_P (operands[0]));
           })

(define_insn "*call"
    [(call (mem:SI (match_operand:SI 0 "nonmemory_operand" "ir")) (match_operand 1 "" ""))]
           ""
           "jal %0"
)

(define_insn "nop"
	[(const_int 0)]
	""
	"nop"
)

;; Prologue and epilogue.

(define_expand "prologue"
  [(const_int 0)]
  ""
  {
    amo_expand_prologue ();
    DONE;
  }
)

(define_expand "epilogue"
  [(const_int 1)]
  ""
  "amo_expand_epilogue (1, 0, 0); DONE;"
)

(define_expand "sibcall_epilogue"
  [(const_int 1)]
  ""
  "amo_expand_epilogue (0, 0, 1); DONE;"
)


