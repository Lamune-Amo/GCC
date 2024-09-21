(define_register_constraint "D" "GP_REGS"
  "@internal
   General-purpose machine registers")

;; Integer
(define_constraint "Is"
  "32-bit signed integer constraint"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -2147483648, 2147483647)")))

;; Integer Arithmetic
(define_constraint "Ia"
  "16-bit signed integer constraint"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

;; Integer Logical
(define_constraint "Il"
  "16-bit unsigned integer constraint"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))



(define_constraint "K"
  "32-bit unsigned constraint."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 4294967295)")))

(define_constraint "J"
  "12-bit signed integer constraint."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -2048, 2047)")))

(define_predicate "call_target_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "symbol_ref")))
