signature GoBot_TOKENS =
sig
type ('a,'b) token
type svalue
val OPERB: (string) *  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val ATR:  'a * 'a -> (svalue,'a) token
val TYPE: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val NOOP:  'a * 'a -> (svalue,'a) token
val GET_TEMP:  'a * 'a -> (svalue,'a) token
val GET_LIGHT:  'a * 'a -> (svalue,'a) token
val GET_ENERGY:  'a * 'a -> (svalue,'a) token
val SHOW:  'a * 'a -> (svalue,'a) token
val MOVED:  'a * 'a -> (svalue,'a) token
val MOVEE:  'a * 'a -> (svalue,'a) token
val MOVEB:  'a * 'a -> (svalue,'a) token
val MOVEA:  'a * 'a -> (svalue,'a) token
val MAPA:  'a * 'a -> (svalue,'a) token
val HOWAMI:  'a * 'a -> (svalue,'a) token
val TEXTO: (string) *  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
end
signature GoBot_LRVALS=
sig
structure Tokens : GoBot_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
