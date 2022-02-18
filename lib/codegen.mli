type generator
module type CodeGenerator =
  sig
    val new_generator : string -> generator
    val close_generator : generator -> unit
    val generate_begin : string
    val generate_end : string
    val generate_entrypoint : string
    val generate_exit : string
    val var : generator -> string -> string
    val gen_plus_op : string -> string -> generator -> string * int
    val gen_sub_op : string -> string -> generator -> string * int
    val gen_mult_op : string -> string -> generator -> string * int
    val gen_and_op : string -> string -> generator -> string * int
    val gen_or_op : string -> string -> generator -> string * int
    val gen_not_op : string -> generator -> string * int
    val gen_print : string -> generator -> generator
    val gen_copy_ident : string -> string -> generator -> generator
    val gen_assign : string -> string -> generator -> generator
    val gen_def_function :
      string -> Lexer.token list -> string -> generator -> generator
  end