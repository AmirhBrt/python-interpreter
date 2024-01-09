#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens LITERALS (ID NUMBER))

(define-empty-tokens KWS (DEF GLOBAL PASS BREAK CONTINUE RETURN NONE PRINT))
(define-empty-tokens OPS (ASSIGN LPAR RPAR PAR COLON PAR_COLON COMMA SEMICOLON))
(define-empty-tokens LOOP_KWS (FOR IN))
(define-empty-tokens BOOL_KWS (TRUE FALSE))
(define-empty-tokens BOOL_OPS (OR AND NOT))
(define-empty-tokens COND_KWS (IF ELSE))
(define-empty-tokens COND_OPS (ISEQ LT BT))
(define-empty-tokens ARITH_OPS (PLUS MINUS MULTI DIV POW))
(define-empty-tokens INDEX_OPS (LBRACK RBRACK BRACK))
(define-empty-tokens END (EOF))

(define python-lexer
  (lexer
   ;KWS
   ("def" (token-DEF))
   ("global" (token-GLOBAL))
   ("pass" (token-PASS))
   ("break" (token-BREAK))
   ("continue" (token-CONTINUE))
   ("return" (token-RETURN))
   ("None" (token-NONE))
   ("print" (token-PRINT))
   ;OPS
   ("=" (token-ASSIGN))
   ;  ("():" (token-PAR_COLON))
   ("()" (token-PAR))
   ("(" (token-LPAR))
   (")" (token-RPAR))
   (":" (token-COLON))
   ("," (token-COMMA))
   (";" (token-SEMICOLON))
   ;LOOP_KWS
   ("for" (token-FOR))
   ("in" (token-IN))
   ;BOOL_KWS
   ("True" (token-TRUE))
   ("False" (token-FALSE))
   ;BOOL_OPS (OR AND NOT)
   ("or" (token-OR))
   ("and" (token-AND))
   ("not" (token-NOT))
   ;COND_KWS (IF ELSE)
   ("if" (token-IF))
   ("else" (token-ELSE))
   ;COND_OPS (ISEQ LT BT)
   ("==" (token-ISEQ))
   ("<" (token-LT))
   (">" (token-BT))
   ;ARITH_OPS
   ("+" (token-PLUS))
   ("-" (token-MINUS))
   ("**" (token-POW))
   ("*" (token-MULTI))
   ("/" (token-DIV))
   ;INDEX_OPS (LBRACK RBRACK BRACK)
   ("[]" (token-BRACK))
   ("[" (token-LBRACK))
   ("]" (token-RBRACK))
   (
    (:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUMBER (string->number lexeme))
    )
   (
    (:: (:or "_" (char-range "a" "z") (char-range "A" "Z"))
        (:* (:or "_" (char-range "a" "z") (char-range "A" "Z") (char-range #\0 #\9)))
        ;[_a-zA-Z][_a-zA-Z0-9]*
        )
    (token-ID lexeme)
    )
   ;LITERALS
   (whitespace (python-lexer input-port))
   ((eof) (token-EOF))))


;test
(define (lex-this prog-string)
  (let ([l (open-input-string prog-string)])
    (begin
      (lambda ()
        (python-lexer l)
        ))))

(provide (all-defined-out))
