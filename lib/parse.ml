
module T = Parse_utils.Make(
  struct
    type token = Parser.token
    type result = Syntax.program
    let rule = Lexer.token
    let input = Parser.program
    let tail = Lexer.tail
  end)

include T
