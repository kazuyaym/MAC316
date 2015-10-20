functor GoBotLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : GoBot_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* goBot definitions for ML-Yacc *)
type Mem = (string*int) list
type Trans = (Mem -> Mem)

fun printMem (sigma:Mem, s:string, i :int) = (case sigma of nil => print(s^"FIM\n")
      | (x::xs) => let val z = Int.toString(i)^" "^ (#1 x) ^"="^Int.toString(#2 x) ^ "\n"
                                 in printMem(xs,s^z,i+1) end)

fun printLS (lista:string list, s:string) = (case lista of nil => print(s^"FIM\n")
                                          | (x::xs) => printLS(xs,s^x) )
                           
fun defVarSig (s: string, t :string, sigma:Mem) = (*let val z = printMem sigma in*) ( 
    case sigma of nil => [(s,~999)]
                | (x::xs) => ( if ((#1 x) = s) then let val z = print("Variável "^s^" ja declarada!\n") in sigma end
                else x::defVarSig (s, t, xs) )
    )
                
fun lookupSig (s: string, sigma:Mem) = (*let val z = printMem sigma in *)(
        case sigma of nil => GoBotConfig.printErro("Warning: Variável "^(s)^" não declarada!\n") 
                | (x::xs) => ( if ((#1 x) = (s)) then  (#2 x) 
                               else  lookupSig(s,xs) ) 
        ) (*end*)
                               
fun fazAtribSig (y: (string * int), sigma:Mem) = (*let val z = printMem sigma in *)
	(case sigma of nil => let val x = print("Variável "^(#1 y)^" não declarada!\n") in sigma end
    | (x::xs) =>	 (if (#1 x) = (#1 y) then y::xs else x::fazAtribSig(y,xs) )
    ) (* end *)


fun getLin (sigma:Mem) = lookupSig("GBlinha",sigma);
fun getCol (sigma:Mem) = lookupSig("GBcoluna",sigma);
fun getLinCol (sigma:Mem) = (lookupSig("GBlinha",sigma),lookupSig("GBcoluna",sigma));
fun getEnergia (sigma:Mem) = (lookupSig("GBenergia",sigma));

fun printHowAmI (sigma:Mem) = let
    val lin = Int.toString(getLin( sigma))
    val col = Int.toString(getCol( sigma))
    val en = Int.toString(getEnergia( sigma))
    in
    print(lin^" "^col^" "^en^"\n")
    end


fun temEnergia(sigma:Mem) = (
    let val energia = getEnergia(sigma)
    in if (energia > 0) then true else false
    end
    )

fun reduzEnergia(sigma:Mem, custo:int) = (
    let val energia = getEnergia(sigma);
    in fazAtribSig ( ("GBenergia",energia - custo), sigma)
    end
    )

fun move(sigma:Mem, movedir:Trans) = (
    if (temEnergia(sigma)) then reduzEnergia(movedir(sigma),GoBotConfig.moveCost)
    else sigma
    )


fun moveA(sigma:Mem) = (
    let
		val (lin,col) = (getLinCol (sigma))
	in
		if(lin+1 > (GoBotConfig.altura) orelse GoBotConfig.getMuroPos( (lin+1,col) ) = 1)
          then sigma 
          else fazAtribSig( ("GBlinha",lin+1), sigma)
	end)
	
fun moveB(sigma:Mem) = (
    let
		val (lin,col) = (getLinCol (sigma))
	in
		if(lin-1 <= 0 orelse GoBotConfig.getMuroPos( (lin-1,col) ) = 1)
          then sigma 
          else fazAtribSig( ("GBlinha",lin-1),sigma)
	end)

fun moveE(sigma:Mem) = (
    let
		val (lin,col) = (getLinCol (sigma))
	in
		if(col-1 <= 0  orelse GoBotConfig.getMuroPos( (lin,col-1) ) = 1)
          then sigma 
          else fazAtribSig( ("GBcoluna",col-1),sigma)
	end)

fun moveD(sigma:Mem) = (
    let
        val (lin,col) = (getLinCol (sigma))
	in
		if(col+1 > GoBotConfig.largura orelse GoBotConfig.getMuroPos( (lin,col+1) ) = 1)
          then sigma 
          else fazAtribSig( ("GBcoluna",col+1),sigma)
	end)

fun getLuz (sigma:Mem) = GoBotConfig.getLuzPos(getLinCol(sigma));
fun getTemp (sigma:Mem) = GoBotConfig.getTempPos(getLinCol(sigma));
fun getMuro (sigma:Mem) = GoBotConfig.getMuroPos(getLinCol(sigma));

fun clear(s:string) = let val n1 = size(s) val z = print(s) in substring(s,2,n1-4) end 

fun app_operb(x:int,y:int,z:string) = ( case z of "+" => (x+y)
		                                      | "-" => (x-y)
		                                      | "*" => (x*y)
		                                      | "/" => (x div y)
		                                      | "%" => (x mod y)
		                                      | "<" => ( (if (x < y) then 1 else 0) )
		                                      | ">" => ( (if (x > y) then 1 else 0) )
		                                      | "=" => (if (x = y) then 1 else 0)
														  | "!=" => (if (x = y) then 0 else 1)
                                            | ">=" => (if (x >= y) then 1 else 0)
		                                      | "<=" => (if (x <= y) then 1 else 0)
		                                      | "and" => (if (x*y>0) then 1 else 0)
		                                      | "or" => (if (x+y>0) then 1 else 0)
                                              | "^" => (let fun e (m,0) = 1
                                                              | e (m,l) = m*e(m,l-1)
                                                              in e (x,y)
                                                        end)
                         		              |   _  => (1) )


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\006\000\018\000\007\000\017\000\008\000\016\000\009\000\015\000\
\\010\000\014\000\014\000\013\000\016\000\012\000\019\000\011\000\000\000\
\\001\000\001\000\024\000\000\000\
\\001\000\001\000\036\000\002\000\035\000\003\000\034\000\011\000\033\000\
\\012\000\032\000\013\000\031\000\021\000\030\000\023\000\029\000\
\\024\000\028\000\025\000\027\000\000\000\
\\001\000\001\000\036\000\002\000\035\000\011\000\033\000\012\000\032\000\
\\013\000\031\000\021\000\030\000\023\000\029\000\024\000\028\000\
\\025\000\027\000\000\000\
\\001\000\001\000\036\000\002\000\035\000\011\000\033\000\012\000\032\000\
\\013\000\031\000\021\000\044\000\024\000\028\000\025\000\027\000\000\000\
\\001\000\001\000\036\000\002\000\035\000\011\000\033\000\012\000\032\000\
\\013\000\031\000\021\000\052\000\024\000\028\000\025\000\027\000\000\000\
\\001\000\018\000\000\000\000\000\
\\001\000\020\000\037\000\000\000\
\\001\000\021\000\040\000\000\000\
\\001\000\022\000\046\000\000\000\
\\001\000\022\000\048\000\000\000\
\\001\000\022\000\050\000\000\000\
\\001\000\022\000\054\000\000\000\
\\056\000\000\000\
\\057\000\017\000\023\000\000\000\
\\058\000\001\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\006\000\018\000\007\000\017\000\008\000\016\000\009\000\015\000\
\\010\000\014\000\014\000\013\000\016\000\012\000\019\000\011\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\026\000\039\000\000\000\
\\080\000\026\000\049\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\"
val actionRowNumbers =
"\000\000\013\000\014\000\024\000\
\\023\000\022\000\021\000\020\000\
\\019\000\001\000\002\000\029\000\
\\027\000\035\000\034\000\033\000\
\\032\000\028\000\026\000\025\000\
\\007\000\015\000\030\000\036\000\
\\017\000\045\000\044\000\008\000\
\\003\000\049\000\048\000\047\000\
\\018\000\043\000\046\000\003\000\
\\016\000\004\000\003\000\009\000\
\\031\000\041\000\003\000\010\000\
\\037\000\011\000\042\000\005\000\
\\039\000\040\000\003\000\012\000\
\\038\000\006\000"
val gotoT =
"\
\\003\000\008\000\004\000\007\000\005\000\006\000\006\000\005\000\
\\007\000\004\000\008\000\003\000\009\000\053\000\010\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\024\000\002\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\008\000\004\000\007\000\005\000\006\000\006\000\005\000\
\\007\000\004\000\008\000\003\000\010\000\002\000\011\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\039\000\002\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\040\000\002\000\023\000\000\000\
\\000\000\
\\002\000\041\000\000\000\
\\001\000\043\000\002\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\045\000\002\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\049\000\000\000\
\\000\000\
\\000\000\
\\001\000\051\000\002\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 54
val numrules = 37
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | OPERB of unit ->  (string) | TYPE of unit ->  (string)
 | TEXTO of unit ->  (string) | NUM of unit ->  (int)
 | ID of unit ->  (string) | BLOCO of unit ->  (Trans)
 | CMD of unit ->  (Trans) | START of unit ->  (Mem)
 | C_MOVED of unit ->  (Trans) | C_MOVEE of unit ->  (Trans)
 | C_MOVEB of unit ->  (Trans) | C_MOVEA of unit ->  (Trans)
 | ATRIB of unit ->  (Trans) | DECL of unit ->  (Trans)
 | TERMO of unit ->  ( ( Mem -> int ) )
 | EXP of unit ->  ( ( Mem -> int ) )
end
type svalue = MlyValue.svalue
type result = Mem
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 25))::
(nil
,nil
 $$ (T 0))::
nil
val noShift = 
fn (T 17) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "TEXTO"
  | (T 3) => "HOWAMI"
  | (T 4) => "MAPA"
  | (T 5) => "MOVEA"
  | (T 6) => "MOVEB"
  | (T 7) => "MOVEE"
  | (T 8) => "MOVED"
  | (T 9) => "SHOW"
  | (T 10) => "GET_ENERGY"
  | (T 11) => "GET_LIGHT"
  | (T 12) => "GET_TEMP"
  | (T 13) => "NOOP"
  | (T 14) => "TIMES"
  | (T 15) => "PRINT"
  | (T 16) => "SEMI"
  | (T 17) => "EOF"
  | (T 18) => "TYPE"
  | (T 19) => "ATR"
  | (T 20) => "LPAR"
  | (T 21) => "RPAR"
  | (T 22) => "NOT"
  | (T 23) => "TRUE"
  | (T 24) => "FALSE"
  | (T 25) => "OPERB"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 0) => MlyValue.ID(fn () => ("bogus")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.BLOCO BLOCO1, BLOCO1left, BLOCO1right)) :: 
rest671)) => let val  result = MlyValue.START (fn _ => let val  (BLOCO
 as BLOCO1) = BLOCO1 ()
 in ( BLOCO(GoBotConfig.MemoriaInicial) )
end)
 in ( LrTable.NT 8, ( result, BLOCO1left, BLOCO1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CMD CMD1, CMD1left, CMD1right)) :: rest671))
 => let val  result = MlyValue.BLOCO (fn _ => let val  (CMD as CMD1) =
 CMD1 ()
 in (CMD)
end)
 in ( LrTable.NT 10, ( result, CMD1left, CMD1right), rest671)
end
|  ( 2, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.CMD CMD1, 
CMD1left, _)) :: rest671)) => let val  result = MlyValue.BLOCO (fn _
 => let val  (CMD as CMD1) = CMD1 ()
 in (CMD)
end)
 in ( LrTable.NT 10, ( result, CMD1left, SEMI1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.BLOCO BLOCO1, _, BLOCO1right)) :: _ :: ( _, 
( MlyValue.CMD CMD1, CMD1left, _)) :: rest671)) => let val  result = 
MlyValue.BLOCO (fn _ => let val  (CMD as CMD1) = CMD1 ()
 val  (BLOCO as BLOCO1) = BLOCO1 ()
 in (fn x => BLOCO(CMD x) )
end)
 in ( LrTable.NT 10, ( result, CMD1left, BLOCO1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (
fn sigma => let val z = print ((Int.toString (EXP sigma)) ^ "\n") in sigma end
)
end)
 in ( LrTable.NT 9, ( result, PRINT1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.TEXTO TEXTO1, _, TEXTO1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (TEXTO as TEXTO1) = TEXTO1 ()
 in (fn x => let val z = print ( (TEXTO)^"\n") in x end)
end)
 in ( LrTable.NT 9, ( result, PRINT1left, TEXTO1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.DECL DECL1, DECL1left, DECL1right)) :: 
rest671)) => let val  result = MlyValue.CMD (fn _ => let val  (DECL
 as DECL1) = DECL1 ()
 in ( DECL)
end)
 in ( LrTable.NT 9, ( result, DECL1left, DECL1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ATRIB ATRIB1, ATRIB1left, ATRIB1right)) :: 
rest671)) => let val  result = MlyValue.CMD (fn _ => let val  (ATRIB
 as ATRIB1) = ATRIB1 ()
 in ( ATRIB)
end)
 in ( LrTable.NT 9, ( result, ATRIB1left, ATRIB1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.C_MOVEA C_MOVEA1, C_MOVEA1left, 
C_MOVEA1right)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (C_MOVEA as C_MOVEA1) = C_MOVEA1 ()
 in (C_MOVEA)
end)
 in ( LrTable.NT 9, ( result, C_MOVEA1left, C_MOVEA1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.C_MOVEB C_MOVEB1, C_MOVEB1left, 
C_MOVEB1right)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (C_MOVEB as C_MOVEB1) = C_MOVEB1 ()
 in (C_MOVEB)
end)
 in ( LrTable.NT 9, ( result, C_MOVEB1left, C_MOVEB1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.C_MOVEE C_MOVEE1, C_MOVEE1left, 
C_MOVEE1right)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (C_MOVEE as C_MOVEE1) = C_MOVEE1 ()
 in (C_MOVEE)
end)
 in ( LrTable.NT 9, ( result, C_MOVEE1left, C_MOVEE1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.C_MOVED C_MOVED1, C_MOVED1left, 
C_MOVED1right)) :: rest671)) => let val  result = MlyValue.CMD (fn _
 => let val  (C_MOVED as C_MOVED1) = C_MOVED1 ()
 in (C_MOVED)
end)
 in ( LrTable.NT 9, ( result, C_MOVED1left, C_MOVED1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.TEXTO TEXTO1, TEXTO1left, TEXTO1right)) :: 
rest671)) => let val  result = MlyValue.CMD (fn _ => let val  TEXTO1 =
 TEXTO1 ()
 in ( fn x => x)
end)
 in ( LrTable.NT 9, ( result, TEXTO1left, TEXTO1right), rest671)
end
|  ( 13, ( ( _, ( _, HOWAMI1left, HOWAMI1right)) :: rest671)) => let
 val  result = MlyValue.CMD (fn _ => (
fn x => let val z = printHowAmI(x) in x end))
 in ( LrTable.NT 9, ( result, HOWAMI1left, HOWAMI1right), rest671)
end
|  ( 14, ( ( _, ( _, SHOW1left, SHOW1right)) :: rest671)) => let val  
result = MlyValue.CMD (fn _ => (
fn x => let val z = printMem(x,"",0) in x end))
 in ( LrTable.NT 9, ( result, SHOW1left, SHOW1right), rest671)
end
|  ( 15, ( ( _, ( _, MAPA1left, MAPA1right)) :: rest671)) => let val  
result = MlyValue.CMD (fn _ => (
fn x => let val z = printLS(GoBotConfig.listaMapa,"") in x end))
 in ( LrTable.NT 9, ( result, MAPA1left, MAPA1right), rest671)
end
|  ( 16, ( ( _, ( _, NOOP1left, NOOP1right)) :: rest671)) => let val  
result = MlyValue.CMD (fn _ => ( fn x => x))
 in ( LrTable.NT 9, ( result, NOOP1left, NOOP1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( 
MlyValue.TYPE TYPE1, TYPE1left, _)) :: rest671)) => let val  result = 
MlyValue.DECL (fn _ => let val  (TYPE as TYPE1) = TYPE1 ()
 val  (ID as ID1) = ID1 ()
 in (fn sigma => defVarSig (ID,TYPE,sigma) )
end)
 in ( LrTable.NT 2, ( result, TYPE1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ATRIB (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (fn sigma => fazAtribSig( (ID,EXP(sigma)),sigma) )
end)
 in ( LrTable.NT 3, ( result, ID1left, EXP1right), rest671)
end
|  ( 19, ( ( _, ( _, MOVEA1left, MOVEA1right)) :: rest671)) => let
 val  result = MlyValue.C_MOVEA (fn _ => (
fn sigma => move(sigma,moveA)))
 in ( LrTable.NT 4, ( result, MOVEA1left, MOVEA1right), rest671)
end
|  ( 20, ( ( _, ( _, MOVEB1left, MOVEB1right)) :: rest671)) => let
 val  result = MlyValue.C_MOVEB (fn _ => (
fn sigma => move(sigma,moveB)))
 in ( LrTable.NT 5, ( result, MOVEB1left, MOVEB1right), rest671)
end
|  ( 21, ( ( _, ( _, MOVEE1left, MOVEE1right)) :: rest671)) => let
 val  result = MlyValue.C_MOVEE (fn _ => (
fn sigma => move(sigma,moveE)))
 in ( LrTable.NT 6, ( result, MOVEE1left, MOVEE1right), rest671)
end
|  ( 22, ( ( _, ( _, MOVED1left, MOVED1right)) :: rest671)) => let
 val  result = MlyValue.C_MOVED (fn _ => (
fn sigma => move(sigma,moveD)))
 in ( LrTable.NT 7, ( result, MOVED1left, MOVED1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.TERMO TERMO1, TERMO1left, TERMO1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (TERMO
 as TERMO1) = TERMO1 ()
 in (TERMO)
end)
 in ( LrTable.NT 0, ( result, TERMO1left, TERMO1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.EXP EXP1, _, _
)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 0, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAR2right)) :: ( _, ( MlyValue.EXP EXP2, _, _
)) :: _ :: ( _, ( MlyValue.OPERB OPERB1, _, _)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  (OPERB as OPERB1) = OPERB1 ()
 val  EXP2 = EXP2 ()
 in (fn sigma => app_operb(EXP1(sigma),EXP2(sigma),OPERB) )
end)
 in ( LrTable.NT 0, ( result, LPAR1left, RPAR2right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.EXP EXP1, _, _
)) :: _ :: ( _, ( MlyValue.OPERB OPERB1, _, _)) :: ( _, ( 
MlyValue.TERMO TERMO1, TERMO1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  (TERMO as TERMO1) = TERMO1 ()
 val  (OPERB as OPERB1) = OPERB1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (fn sigma => app_operb(TERMO(sigma),EXP(sigma),OPERB) )
end)
 in ( LrTable.NT 0, ( result, TERMO1left, RPAR1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.TERMO TERMO1, _, TERMO1right)) :: ( _, ( 
MlyValue.OPERB OPERB1, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _))
 :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (OPERB as OPERB1) = OPERB1 ()
 val  (TERMO as TERMO1) = TERMO1 ()
 in (fn sigma => app_operb(EXP(sigma),TERMO(sigma),OPERB) )
end)
 in ( LrTable.NT 0, ( result, LPAR1left, TERMO1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.TERMO TERMO2, _, TERMO2right)) :: ( _, ( 
MlyValue.OPERB OPERB1, _, _)) :: ( _, ( MlyValue.TERMO TERMO1, 
TERMO1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  TERMO1 = TERMO1 ()
 val  (OPERB as OPERB1) = OPERB1 ()
 val  TERMO2 = TERMO2 ()
 in (fn sigma => app_operb(TERMO1(sigma),TERMO2(sigma),OPERB) )
end)
 in ( LrTable.NT 0, ( result, TERMO1left, TERMO2right), rest671)
end
|  ( 29, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.EXP EXP1, _, _
)) :: _ :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (fn sigma => if(EXP(sigma)=0) then 1 else 0)
end)
 in ( LrTable.NT 0, ( result, NOT1left, RPAR1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.TERMO (fn _ => let val  (NUM as NUM1)
 = NUM1 ()
 in (fn sigma => NUM)
end)
 in ( LrTable.NT 1, ( result, NUM1left, NUM1right), rest671)
end
|  ( 31, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.TERMO (fn _ => (fn sigma => 1))
 in ( LrTable.NT 1, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 32, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.TERMO (fn _ => (fn sigma => 0))
 in ( LrTable.NT 1, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.TERMO (fn _ => let val  (ID as ID1) = ID1
 ()
 in (fn sigma => lookupSig(ID,sigma) )
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 34, ( ( _, ( _, GET_ENERGY1left, GET_ENERGY1right)) :: rest671))
 => let val  result = MlyValue.TERMO (fn _ => (
fn sigma => getEnergia(sigma)))
 in ( LrTable.NT 1, ( result, GET_ENERGY1left, GET_ENERGY1right), 
rest671)
end
|  ( 35, ( ( _, ( _, GET_LIGHT1left, GET_LIGHT1right)) :: rest671)) =>
 let val  result = MlyValue.TERMO (fn _ => (fn sigma => getLuz(sigma))
)
 in ( LrTable.NT 1, ( result, GET_LIGHT1left, GET_LIGHT1right), 
rest671)
end
|  ( 36, ( ( _, ( _, GET_TEMP1left, GET_TEMP1right)) :: rest671)) =>
 let val  result = MlyValue.TERMO (fn _ => (fn sigma => getTemp(sigma)
))
 in ( LrTable.NT 1, ( result, GET_TEMP1left, GET_TEMP1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : GoBot_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun TEXTO (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.TEXTO (fn () => i),p1,p2))
fun HOWAMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MAPA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun MOVEA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun MOVEB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun MOVEE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun MOVED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun GET_ENERGY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun GET_LIGHT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun GET_TEMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NOOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.TYPE (fn () => i),p1,p2))
fun ATR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OPERB (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.OPERB (fn () => i),p1,p2))
end
end
