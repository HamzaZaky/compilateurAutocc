type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | IF
  | THEN
  | ELSE
  | ELSEIF
  | WHEN
  | OTHERWISE
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT_DOT
  | DOT
  | EQ
  | NOTEQ
  | SM
  | GR
  | SMEQ
  | GREQ
  | ID of (string)
  | INT of (int)
  | ADD
  | SUB
  | MULT
  | DIV
  | MOD
  | PO
  | PF
  | NEG

open Parsing;;
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param v	Initial values.
	@param ws	Sequence of (condition, expression).
	@return		Built statement. *)
let rec make_when f v ws =
	match ws with
	| [] ->	f v
	| (c, nv)::t ->
		IF_THEN(c, f v, make_when f nv t)

# 62 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* ELSEIF *);
  264 (* WHEN *);
  265 (* OTHERWISE *);
  266 (* ASSIGN *);
  267 (* COMMA *);
  268 (* LBRACKET *);
  269 (* RBRACKET *);
  270 (* DOT_DOT *);
  271 (* DOT *);
  272 (* EQ *);
  273 (* NOTEQ *);
  274 (* SM *);
  275 (* GR *);
  276 (* SMEQ *);
  277 (* GREQ *);
  280 (* ADD *);
  281 (* SUB *);
  282 (* MULT *);
  283 (* DIV *);
  284 (* MOD *);
  285 (* PO *);
  286 (* PF *);
  287 (* NEG *);
    0|]

let yytransl_block = [|
  278 (* ID *);
  279 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\006\000\006\000\006\000\006\000\009\000\009\000\
\009\000\009\000\009\000\009\000\007\000\008\000\008\000\008\000\
\010\000\010\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\011\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\003\000\003\000\005\000\007\000\009\000\011\000\003\000\003\000\
\003\000\003\000\003\000\003\000\005\000\003\000\003\000\001\000\
\003\000\003\000\003\000\001\000\003\000\002\000\002\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\007\000\000\000\000\000\
\002\000\000\000\005\000\000\000\001\000\000\000\000\000\000\000\
\008\000\000\000\006\000\034\000\033\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\030\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\026\000\027\000\000\000\
\011\000\000\000\000\000\021\000\000\000\000\000\012\000\007\000\
\000\000\013\000\007\000\000\000\014\000"

let yydgoto = "\002\000\
\004\000\009\000\018\000\010\000\011\000\025\000\033\000\034\000\
\035\000\036\000\037\000"

let yysindex = "\255\255\
\253\254\000\000\032\255\000\000\035\255\251\254\038\255\034\255\
\048\255\040\255\000\000\030\255\031\255\000\000\041\255\042\255\
\000\000\066\000\000\000\044\255\000\000\020\255\049\255\047\255\
\000\000\054\255\000\000\000\000\000\000\020\255\020\255\020\255\
\000\000\065\255\066\255\033\255\000\000\062\255\020\255\020\255\
\000\000\000\000\242\254\020\255\020\255\020\255\020\255\020\255\
\020\255\020\255\020\255\000\000\020\255\020\255\020\255\068\255\
\022\255\022\255\000\000\022\255\022\255\022\255\022\255\022\255\
\022\255\033\255\033\255\002\255\000\000\000\000\000\000\075\255\
\000\000\018\255\020\255\000\000\090\255\088\255\000\000\000\000\
\017\255\000\000\000\000\003\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\092\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\080\000\087\000\000\000\091\255\093\255\094\255\095\255\096\255\
\097\255\028\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\207\255\000\000\082\000\021\000\240\255\030\000\
\029\000\232\255\238\255"

let yytablesize = 365
let yytable = "\001\000\
\024\000\026\000\068\000\073\000\085\000\022\000\022\000\074\000\
\075\000\050\000\051\000\041\000\042\000\023\000\023\000\059\000\
\007\000\008\000\082\000\003\000\022\000\022\000\083\000\024\000\
\024\000\066\000\067\000\022\000\023\000\023\000\081\000\023\000\
\005\000\084\000\069\000\070\000\071\000\006\000\024\000\024\000\
\012\000\028\000\029\000\030\000\031\000\050\000\051\000\013\000\
\032\000\014\000\015\000\026\000\016\000\017\000\023\000\020\000\
\039\000\026\000\053\000\054\000\055\000\043\000\007\000\040\000\
\026\000\021\000\027\000\026\000\057\000\058\000\052\000\038\000\
\056\000\060\000\061\000\062\000\063\000\064\000\065\000\010\000\
\044\000\045\000\046\000\047\000\048\000\049\000\009\000\076\000\
\050\000\051\000\072\000\079\000\080\000\003\000\077\000\015\000\
\019\000\016\000\018\000\017\000\020\000\019\000\000\000\078\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\000\000\024\000\024\000\024\000\024\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\000\000\
\024\000\024\000\000\000\000\000\000\000\022\000\024\000\022\000\
\022\000\022\000\022\000\000\000\000\000\000\000\000\000\022\000\
\000\000\000\000\000\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\000\000\022\000\022\000\000\000\000\000\000\000\
\023\000\022\000\023\000\023\000\023\000\023\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\022\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\000\000\010\000\000\000\010\000\023\000\010\000\010\000\024\000\
\009\000\000\000\009\000\010\000\009\000\009\000\000\000\000\000\
\000\000\000\000\009\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000"

let yycheck = "\001\000\
\000\000\018\000\052\000\002\001\002\001\004\001\004\001\006\001\
\007\001\024\001\025\001\030\000\031\000\012\001\012\001\030\001\
\022\001\023\001\002\001\023\001\004\001\004\001\006\001\022\001\
\022\001\050\000\051\000\000\000\012\001\012\001\080\000\012\001\
\001\001\083\000\053\000\054\000\055\000\003\001\022\001\022\001\
\003\001\022\001\023\001\024\001\025\001\024\001\025\001\014\001\
\029\001\002\001\011\001\068\000\023\001\023\001\000\000\014\001\
\010\001\074\000\026\001\027\001\028\001\032\000\022\001\010\001\
\081\000\000\000\023\001\084\000\039\000\040\000\005\001\023\001\
\011\001\044\000\045\000\046\000\047\000\048\000\049\000\000\000\
\016\001\017\001\018\001\019\001\020\001\021\001\000\000\013\001\
\024\001\025\001\023\001\002\001\005\001\002\001\074\000\005\001\
\015\000\005\001\005\001\005\001\005\001\005\001\255\255\075\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\004\001\005\001\006\001\007\001\
\255\255\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\255\255\255\255\255\255\002\001\030\001\004\001\
\005\001\006\001\007\001\255\255\255\255\255\255\255\255\012\001\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\255\255\255\255\255\255\
\002\001\030\001\004\001\005\001\006\001\007\001\255\255\255\255\
\255\255\255\255\012\001\255\255\255\255\004\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\012\001\024\001\025\001\
\255\255\002\001\255\255\004\001\030\001\006\001\007\001\022\001\
\002\001\255\255\004\001\012\001\006\001\007\001\255\255\255\255\
\255\255\255\255\012\001\255\255\255\255\022\001\255\255\255\255\
\255\255\255\255\255\255\255\255\022\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSEIF\000\
  WHEN\000\
  OTHERWISE\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT_DOT\000\
  DOT\000\
  EQ\000\
  NOTEQ\000\
  SM\000\
  GR\000\
  SMEQ\000\
  GREQ\000\
  ADD\000\
  SUB\000\
  MULT\000\
  DIV\000\
  MOD\000\
  PO\000\
  PF\000\
  NEG\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 89 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 309 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 320 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 102 "parser.mly"
  ( set_fields _1 )
# 327 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 107 "parser.mly"
  ( [_1] )
# 334 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 109 "parser.mly"
  (_3 :: _1 )
# 342 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 114 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 354 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
  ( NOP )
# 360 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 124 "parser.mly"
  ( SEQ(_1, _2) )
# 368 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 130 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 380 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 136 "parser.mly"
  (
		    let num_reg = declare_var _1 in
		    SET_VAR (num_reg, _3)
		)
# 391 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 142 "parser.mly"
    (
      IF_THEN(_2, _4,NOP)
    )
# 401 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 146 "parser.mly"
    (
      IF_THEN(_2, _4, _6)
    )
# 412 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 150 "parser.mly"
    (
      IF_THEN(_2, _4, IF_THEN(_6, _8, NOP))
    )
# 424 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'condition) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'opt_statements) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 154 "parser.mly"
    (
      IF_THEN(_2, _4, IF_THEN(_6, _8, _10))
    )
# 437 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 162 "parser.mly"
  (COMP(COMP_EQ, _1, _3))
# 445 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 164 "parser.mly"
  (COMP(COMP_NE, _1, _3))
# 453 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 166 "parser.mly"
  (COMP(COMP_GT, _1, _3))
# 461 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 168 "parser.mly"
  (COMP(COMP_LT, _1, _3))
# 469 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 170 "parser.mly"
  (COMP(COMP_GE, _1, _3))
# 477 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 172 "parser.mly"
  (COMP(COMP_LE, _1, _3))
# 485 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 177 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 497 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'terme) in
    Obj.repr(
# 186 "parser.mly"
    ( BINOP(OP_ADD, _1, _3) )
# 505 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'terme) in
    Obj.repr(
# 188 "parser.mly"
    ( BINOP(OP_SUB, _1, _3) )
# 513 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'terme) in
    Obj.repr(
# 190 "parser.mly"
    ( _1 )
# 520 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'terme) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'facteur) in
    Obj.repr(
# 195 "parser.mly"
    ( BINOP(OP_MUL, _1, _3) )
# 528 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'terme) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'facteur) in
    Obj.repr(
# 197 "parser.mly"
    ( BINOP(OP_DIV, _1, _3) )
# 536 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'terme) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'facteur) in
    Obj.repr(
# 199 "parser.mly"
    ( BINOP(OP_MOD, _1, _3) )
# 544 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'facteur) in
    Obj.repr(
# 201 "parser.mly"
    ( _1 )
# 551 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 206 "parser.mly"
    ( _2 )
# 558 "parser.ml"
               : 'facteur))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'facteur) in
    Obj.repr(
# 208 "parser.mly"
    ( _2 )
# 565 "parser.ml"
               : 'facteur))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'facteur) in
    Obj.repr(
# 210 "parser.mly"
    ( NEG _2 )
# 572 "parser.ml"
               : 'facteur))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 212 "parser.mly"
    ( CELL (0, fst _1, snd _1) )
# 579 "parser.ml"
               : 'facteur))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 214 "parser.mly"
    ( CST _1 )
# 586 "parser.ml"
               : 'facteur))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 216 "parser.mly"
    ( 
    
      let reg = get_var _1 in
      if reg = -1 then
        error (sprintf "Variable %s non declaree" _1)
      else
        VAR reg 
    )
# 600 "parser.ml"
               : 'facteur))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
