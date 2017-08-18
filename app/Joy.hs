module Joy where

import Mascarpone as M hiding (pop')
import Control.Monad
import Data.Char
import Control.Monad.State
import Data.Fixed
import Data.List
import System.Exit
import qualified Data.Map as Ma
import qualified Prefix as P
import Control.Arrow

num = do
  Symb (Chr c) <- peek
  if not (isDigit c)
     then return Nothing
     else do joy_pop
             x <- num
             return $ case x of
               Nothing -> Just . Symb . Num $ read [c]
               (Just(Symb(Num x))) -> Just . Symb . Num $ read [c] * 10 + x

num' = do
  x <- num
  case x of Just n -> push n
            _   -> nop

popOp = do
  x <- pop
  case x of
       Op _ -> return x
       Symb (Chr ']') -> do
          str <- push x >> popString
          i   <- gets interpreter
          return . Op $ interpretWith i str

popBool = do
  x <- pop
  case x of
       Symb (Num 0) -> return False
       _            -> return True

pushN = mapM_ push . reverse

wordSyn = P.toInterp defaultInterpreter
        . P.fromList . map (first $ map Chr)

joyInterp :: Interpreter
joyInterp = wordSyn
  [ ("!=",            joy_neq)
  , ("*",             joy_mul)
  , ("+",             joy_add)
  , ("-",             joy_sub)
  , ("/",             joy_div)
  , ("<",             joy_gt)
  , ("<=",            joy_geq)
  , (">",             joy_lt)
  , (">=",            joy_leq)
  , ("abort",         joy_abort)
  , ("abs",           joy_abs)
  , ("all",           joy_all)
  , ("and",           joy_and)
  , ("app1",          joy_app1)
  , ("app11",         joy_app11)
  , ("app12",         joy_app12)
  , ("app2",          joy_app2)
  , ("app3",          joy_app3)
  , ("app4",          joy_app4)
  , ("argc",          joy_argc)
  , ("argv",          joy_argv)
  , ("asin",          joy_asin)
  , ("at",            joy_at)
  , ("atan",          joy_atan)
  , ("atan2",         joy_atan2)
  , ("autoput",       joy_autoput)
  , ("binary",        joy_binary)
  , ("binrec",        joy_binrec)
  , ("body",          joy_body)
  , ("branch",        joy_branch)
  , ("case",          joy_case)
  , ("ceil",          joy_ceil)
  , ("char",          joy_char)
  , ("choice",        joy_choice)
  , ("chr",           joy_chr)
  , ("cleave",        joy_cleave)
  , ("clock",         joy_clock)
  , ("compare",       joy_compare)
  , ("concat",        joy_concat)
  , ("cond",          joy_cond)
  , ("condlinrec",    joy_condlinrec)
  , ("cons",          joy_cons)
  , ("construct",     joy_construct)
  , ("conts",         joy_conts)
  , ("cos",           joy_cos)
  , ("cosh",          joy_cosh)
  , ("dip",           joy_dip)
  , ("div",           joy_div')
  , ("drop",          joy_drop)
  , ("dup",           joy_dup)
  , ("dupd",          joy_dupd)
  , ("echo",          joy_echo)
  , ("enconcat",      joy_enconcat)
  , ("eq",            joy_eq)
  , ("equal",         joy_equal)
  , ("exp",           joy_exp)
  , ("false",         joy_false)
  , ("fclose",        joy_fclose)
  , ("feof",          joy_feof)
  , ("ferror",        joy_ferror)
  , ("fflush",        joy_fflush)
  , ("fgetch",        joy_fgetch)
  , ("fgets",         joy_fgets)
  , ("file",          joy_file)
  , ("filter",        joy_filter)
  , ("first",         joy_first)
  , ("float",         joy_float)
  , ("floor",         joy_floor)
  , ("fold",          joy_fold)
  , ("fopen",         joy_fopen)
  , ("format",        joy_format)
  , ("formatf",       joy_formatf)
  , ("fput",          joy_fput)
  , ("fputch",        joy_fputch)
  , ("fputchars",     joy_fputchars)
  , ("fputstring",    joy_fputstring)
  , ("fread",         joy_fread)
  , ("fremove",       joy_fremove)
  , ("frename",       joy_frename)
  , ("frexp",         joy_frexp)
  , ("fseek",         joy_fseek)
  , ("ftell",         joy_ftell)
  , ("fwrite",        joy_fwrite)
  , ("gc",            joy_gc)
  , ("genrec",        joy_genrec)
  , ("get",           joy_get)
  , ("getenv",        joy_getenv)
  , ("gmtime",        joy_gmtime)
  , ("has",           joy_has)
  , ("help",          joy_help)
  , ("helpdetail",    joy_helpdetail)
  , ("i",             joy_i)
  , ("id",            joy_id)
  , ("ifchar",        joy_ifchar)
  , ("iffile",        joy_iffile)
  , ("iffloat",       joy_iffloat)
  , ("ifinteger",     joy_ifinteger)
  , ("iflist",        joy_iflist)
  , ("iflogical",     joy_iflogical)
  , ("ifset",         joy_ifset)
  , ("ifstring",      joy_ifstring)
  , ("ifte",          joy_ifte)
  , ("in",            joy_in)
  , ("include",       joy_include)
  , ("infra",         joy_infra)
  , ("integer",       joy_integer)
  , ("intern",        joy_intern)
  , ("ldexp",         joy_ldexp)
  , ("leaf",          joy_leaf)
  , ("linrec",        joy_linrec)
  , ("list",          joy_list)
  , ("localtime",     joy_localtime)
  , ("log",           joy_log)
  , ("log10",         joy_log10)
  , ("logical",       joy_logical)
  , ("manual",        joy_manual)
  , ("map",           joy_map)
  , ("max",           joy_max)
  , ("maxint",        joy_maxint)
  , ("min",           joy_min)
  , ("mktime",        joy_mktime)
  , ("modf",          joy_modf)
  , ("name",          joy_name)
  , ("neg",           joy_neg)
  , ("not",           joy_not)
  , ("null",          joy_null)
  , ("nullary",       joy_nullary)
  , ("of",            joy_of)
  , ("opcase",        joy_opcase)
  , ("or",            joy_or)
  , ("ord",           joy_ord)
  , ("pop",           joy_pop)
  , ("popd",          joy_popd)
  , ("pow",           joy_pow)
  , ("pred",          joy_pred)
  , ("primrec",       joy_primrec)
  , ("put",           joy_put)
  , ("putch",         joy_putch)
  , ("putchars",      joy_putchars)
  , ("quit",          joy_quit)
  , ("rand",          joy_rand)
  , ("rem",           joy_rem)
  , ("rest",          joy_rest)
  , ("rolldown",      joy_rolldown)
  , ("rolldownd",     joy_rolldownd)
  , ("rollup",        joy_rollup)
  , ("rollupd",       joy_rollupd)
  , ("rotate",        joy_rotate)
  , ("rotated",       joy_rotated)
  , ("set",           joy_set)
  , ("setautoput",    joy_setautoput)
  , ("setecho",       joy_setecho)
  , ("setsize",       joy_setsize)
  , ("setundeferror", joy_setundeferror)
  , ("sign",          joy_sign)
  , ("sin",           joy_sin)
  , ("sinh",          joy_sinh)
  , ("size",          joy_size)
  , ("small",         joy_small)
  , ("some",          joy_some)
  , ("split",         joy_split)
  , ("sqrt",          joy_sqrt)
  , ("srand",         joy_srand)
  , ("stack",         joy_stack)
  , ("stderr",        joy_stderr)
  , ("stdin",         joy_stdin)
  , ("stdout",        joy_stdout)
  , ("step",          joy_step)
  , ("strftime",      joy_strftime)
  , ("string",        joy_string)
  , ("strtod",        joy_strtod)
  , ("strtol",        joy_strtol)
  , ("succ",          joy_succ)
  , ("swap",          joy_swap)
  , ("swapd",         joy_swapd)
  , ("swons",         joy_swons)
  , ("system",        joy_system)
  , ("tailrec",       joy_tailrec)
  , ("take",          joy_take)
  , ("tan",           joy_tan)
  , ("tanh",          joy_tanh)
  , ("ternary",       joy_ternary)
  , ("time",          joy_time)
  , ("times",         joy_times)
  , ("treegenrec",    joy_treegenrec)
  , ("treerec",       joy_treerec)
  , ("treestep",      joy_treestep)
  , ("true",          joy_true)
  , ("trunc",         joy_trunc)
  , ("unary",         joy_unary)
  , ("unary2",        joy_unary2)
  , ("unary3",        joy_unary3)
  , ("unary4",        joy_unary4)
  , ("uncons",        joy_uncons)
  , ("undeferror",    joy_undeferror)
  , ("undefs",        joy_undefs)
  , ("unstack",       joy_unstack)
  , ("unswons",       joy_unswons)
  , ("user",          joy_user)
  , ("while",         joy_while)
  , ("x",             joy_x)
  , ("xor",           joy_xor)
  ]

-- false : -> false
-- Pushes the value true.
joy_false  = push $ Symb $ Num 0

-- true : -> true
-- Pushes the value true.
joy_true  = push $ Symb $ Num 1

-- maxint : -> maxint
-- Pushes largest integer (platform dependent). Typically it is 32 bits.
joy_maxint = undefined

-- setsize : -> setsize
-- Pushes the maximum number of elements in a set (platform dependent). Typically it is 32, and set members are in the range 0..31.
joy_setsize = undefined

-- stack : .. X Y Z -> .. X Y Z [Z Y X ..]
-- Pushes the stack as a list.
joy_stack = undefined

-- conts : -> [[P] [Q] ..]
-- Pushes current continuations. Buggy, do not use.
joy_conts = undefined

-- autoput : -> I
-- Pushes current value of flag for automatic output, I = 0..2.
joy_autoput = undefined

-- undeferror : -> I
-- Pushes current value of undefined-is-error flag.
joy_undeferror = undefined

-- undefs : ->
-- Push a list of all undefined symbols in the current symbol table.
joy_undefs = undefined

-- echo : -> I
-- Pushes value of echo flag, I = 0..3.
joy_echo = undefined

-- clock : -> I
-- Pushes the integer value of current CPU usage in hundreds of a second.
joy_clock = undefined

-- time : -> I
-- Pushes the current time (in seconds since the Epoch).
joy_time = undefined

-- rand : -> I
-- I is a random integer.
joy_rand = undefined

-- stdin : -> S
-- Pushes the standard input stream.
joy_stdin = undefined

-- stdout : -> S
-- Pushes the standard output stream.
joy_stdout = undefined

-- stderr : -> S
-- Pushes the standard error stream. operator
joy_stderr = undefined

-- id : ->
-- Identity function, does nothing. Any program of the form P id Q is equivalent to just P Q.
joy_id = return ()

-- dup : X -> X X
-- Pushes an extra copy of X onto stack.
joy_dup = dup

-- swap : X Y -> Y X
-- Interchanges X and Y on top of the stack.
joy_swap = swap

-- rollup : X Y Z -> Z X Y
-- Moves X and Y up, moves Z down
joy_rollup = pop3 >>= \(x,y,z) -> pushN [z,x,y]

-- rolldown : X Y Z -> Y Z X
-- Moves Y and Z down, moves X up
joy_rolldown = pop3 >>= \(x,y,z) -> pushN [y,z,x]

-- rotate : X Y Z -> Z Y X
-- Interchanges X and Z
joy_rotate = pop3 >>= \(x,y,z) -> pushN [z,y,x]

-- popd : Y Z -> Z
-- As if defined by: popd == [pop] dip
joy_popd = pop2 >>= \(y,z) -> push z

-- dupd : Y Z -> Y Y Z
-- As if defined by: dupd == [dup] dip
joy_dupd = pop2 >>= \(y,z) -> pushN [y,y,z]

-- swapd : X Y Z -> Y X Z
-- As if defined by: swapd == [swap] dip
joy_swapd = pop3 >>= \(x,y,z) -> pushN [y,x,z]

-- rollupd : X Y Z W -> Z X Y W
-- As if defined by: rollupd == [rollup] dip
joy_rollupd = pop4 >>= \(x,y,z,w) -> pushN [z,x,y,w]

-- rolldownd : X Y Z W -> Y Z X W
-- As if defined by: rolldownd == [rolldown] dip
joy_rolldownd = pop4 >>= \(x,y,z,w) -> pushN [y,z,x,w]

-- rotated : X Y Z W -> Z Y X W
-- As if defined by: rotated == [rotate] dip
joy_rotated = pop4 >>= \(x,y,z,w) -> pushN [z,y,x,w]

-- pop : X ->
-- Removes X from top of the stack.
joy_pop = void pop

-- choice : B T F -> X
-- If B is true, then X = T else X = F.
joy_choice = do
  b     <- popBool
  (t,f) <- pop2
  push $ if b then t else f

-- or : X Y -> Z
-- Z is the union of sets X and Y, logical disjunction for truth values.
joy_or = undefined

-- xor : X Y -> Z
-- Z is the symmetric difference of sets X and Y, logical exclusive disjunction for truth values.
joy_xor = undefined

-- and : X Y -> Z
-- Z is the intersection of sets X and Y, logical conjunction for truth values.
joy_and = undefined

-- not : X -> Y
-- Y is the complement of set X, logical negation for truth values.
joy_not = undefined

-- + : M I -> N
-- Numeric N is the result of adding integer I to numeric M. Also supports float.
joy_add = pop2 >>= \(Symb (Num a),Symb (Num b)) -> push $ Symb $ Num $ a+b


-- - : M I -> N
-- Numeric N is the result of subtracting integer I from numeric M. Also supports float.
joy_sub = pop2 >>= \(Symb (Num a),Symb (Num b)) -> push $ Symb $ Num $ a-b

-- * : I J -> K
-- Integer K is the product of integers I and J. Also supports float.
joy_mul = pop2 >>= \(Symb (Num a),Symb (Num b)) -> push $ Symb $ Num $ a*b

-- / : I J -> K
-- Integer K is the (rounded) ratio of integers I and J. Also supports float.
joy_div = pop2 >>= \(Symb (Num a),Symb (Num b)) -> push $ Symb $ Num $ a/b

-- rem : I J -> K
-- Integer K is the remainder of dividing I by J. Also supports float.
joy_rem = pop2 >>= \(Symb (Num a),Symb (Num b)) -> push $ Symb $ Num $ a `mod'` b

-- div : I J -> K L
-- Integers K and L are the quotient and remainder of dividing I by J.
joy_div' = undefined

-- sign : N1 -> N2
-- Integer N2 is the sign (-1 or 0 or +1) of integer N1, or float N2 is the sign (-1.0 or 0.0 or 1.0) of float N1.
joy_sign = undefined

-- neg : I -> J
-- Integer J is the negative of integer I. Also supports float.
joy_neg = pop >>= \(Symb (Num a)) -> push $ Symb $ Num $ negate a

-- ord : C -> I
-- Integer I is the Ascii value of character C (or logical or integer).
joy_ord = pop >>= \(Symb (Chr a)) -> push $ Symb $ Num $ fromIntegral $ ord a

-- chr : I -> C
-- C is the character whose Ascii value is integer I (or logical or character).
joy_chr = pop >>= \(Symb (Num a)) -> push $ Symb $ Chr $ chr $ floor a

-- abs : N1 -> N2
-- Integer N2 is the absolute value (0,1,2..) of integer N1, or float N2 is the absolute value (0.0 ..) of float N1
joy_abs = pop >>= \(Symb (Num a)) -> push $ Symb $ Num $ abs a

-- asin : F -> G
-- G is the arc sine of F.
joy_asin = undefined

-- atan : F -> G
-- G is the arc tangent of F.
joy_atan = undefined

-- atan2 : F G -> H
-- H is the arc tangent of F / G.
joy_atan2 = undefined

-- ceil : F -> G
-- G is the float ceiling of F.
joy_ceil = undefined

-- cos : F -> G
-- G is the cosine of F.
joy_cos = undefined

-- cosh : F -> G
-- G is the hyperbolic cosine of F.
joy_cosh = undefined

-- exp : F -> G
-- G is e (2.718281828...) raised to the Fth power.
joy_exp = undefined

-- floor : F -> G
-- G is the floor of F.
joy_floor = undefined

-- frexp : F -> G I
-- G is the mantissa and I is the exponent of F. Unless F = 0, 0.5 <= abs(G) < 1.0.
joy_frexp = undefined

-- ldexp : F I -> G
-- G is F times 2 to the Ith power.
joy_ldexp = undefined

-- log : F -> G
-- G is the natural logarithm of F.
joy_log = undefined

-- log10 : F -> G
-- G is the common logarithm of F.
joy_log10 = undefined

-- modf : F -> G H
-- G is the fractional part and H is the integer part (but expressed as a float) of F.
joy_modf = undefined

-- pow : F G -> H
-- H is F raised to the Gth power.
joy_pow = undefined

-- sin : F -> G
-- G is the sine of F.
joy_sin = undefined

-- sinh : F -> G
-- G is the hyperbolic sine of F.
joy_sinh = undefined

-- sqrt : F -> G
-- G is the square root of F.
joy_sqrt = undefined

-- tan : F -> G
-- G is the tangent of F.
joy_tan = undefined

-- tanh : F -> G
-- G is the hyperbolic tangent of F.
joy_tanh = undefined

-- trunc : F -> I
-- I is an integer equal to the float F truncated toward zero.
joy_trunc = undefined

-- localtime : I -> T
-- Converts a time I into a list T representing local time: [year month day hour minute second isdst yearday weekday]. Month is 1 = January ... 12 = December; isdst is a Boolean flagging daylight savings/summer time; weekday is 0 = Monday ... 7 = Sunday.
joy_localtime = undefined

-- gmtime : I -> T
-- Converts a time I into a list T representing universal time: [year month day hour minute second isdst yearday weekday]. Month is 1 = January ... 12 = December; isdst is false; weekday is 0 = Monday ... 7 = Sunday.
joy_gmtime = undefined

-- mktime : T -> I
-- Converts a list T representing local time into a time I. T is in the format generated by localtime.
joy_mktime = undefined

-- strftime : T S1 -> S2
-- Formats a list T in the format of localtime or gmtime using string S1 and pushes the result S2.
joy_strftime = undefined

-- strtol : S I -> J
-- String S is converted to the integer J using base I. If I = 0, assumes base 10, but leading "0" means base 8 and leading "0x" means base 16.
joy_strtol = undefined

-- strtod : S -> R
-- String S is converted to the float R.
joy_strtod = undefined




-- format : N C I J -> S
-- S is the formatted version of N in mode C ('d or 'i = decimal, 'o = octal, 'x or 'X = hex with lower or upper case letters) with maximum width I and minimum width J.
joy_format = undefined

-- formatf : F C I J -> S
-- S is the formatted version of F in mode C ('e or 'E = exponential, 'f = fractional, 'g or G = general with lower or upper case letters) with maximum width I and precision J.
joy_formatf = undefined

-- srand : I ->
-- Sets the random integer seed to integer I.
joy_srand = undefined

-- pred : M -> N
-- Numeric N is the predecessor of numeric M.
joy_pred = undefined

-- succ : M -> N
-- Numeric N is the successor of numeric M.
joy_succ = undefined

-- max : N1 N2 -> N
-- N is the maximum of numeric values N1 and N2. Also supports float.
joy_max = undefined

-- min : N1 N2 -> N
-- N is the minimum of numeric values N1 and N2. Also supports float.
joy_min = undefined

-- fclose : S ->
-- Stream S is closed and removed from the stack.
joy_fclose = undefined

-- feof : S -> S B
-- B is the end-of-file status of stream S.
joy_feof = undefined

-- ferror : S -> S B
-- B is the error status of stream S.
joy_ferror = undefined

-- fflush : S -> S
-- Flush stream S, forcing all buffered output to be written.
joy_fflush = undefined

-- fgetch : S -> S C
-- C is the next available character from stream S.
joy_fgetch = undefined

-- fgets : S -> S L
-- L is the next available line (as a string) from stream S.
joy_fgets = undefined

-- fopen : P M -> S
-- The file system object with pathname P is opened with mode M (r, w, a, etc.) and stream object S is pushed; if the open fails, file:NULL is pushed.
joy_fopen = undefined

-- fread : S I -> S L
-- I bytes are read from the current position of stream S and returned as a list of I integers.
joy_fread = undefined

-- fwrite : S L -> S
-- A list of integers are written as bytes to the current position of stream S.
joy_fwrite = undefined

-- fremove : P -> B
-- The file system object with pathname P is removed from the file system. is a boolean indicating success or failure.
joy_fremove = undefined

-- frename : P1 P2 -> B
-- The file system object with pathname P1 is renamed to P2. B is a boolean indicating success or failure.
joy_frename = undefined

-- fput : S X -> S
-- Writes X to stream S, pops X off stack.
joy_fput = undefined

-- fputch : S C -> S
-- The character C is written to the current position of stream S.
joy_fputch = undefined

-- fputchars : S "abc.." -> S
-- The string abc.. (no quotes) is written to the current position of stream S.
joy_fputchars = undefined

-- fputstring : S "abc.." -> S
-- == fputchars, as a temporary alternative.
joy_fputstring = undefined

-- fseek : S P W -> S
-- Stream S is repositioned to position P relative to whence-point W, where W = 0, 1, 2 for beginning, current position, end respectively.
joy_fseek = undefined

-- ftell : S -> S I
-- I is the current position of stream S.
joy_ftell = undefined

-- unstack : [X Y ..] -> ..Y X
-- The list [X Y ..] becomes the new stack.
joy_unstack = undefined

-- cons : X A -> B
-- Aggregate B is A with a new member X (first member for sequences).
joy_cons = undefined

-- swons : A X -> B
-- Aggregate B is A with a new member X (first member for sequences).
joy_swons = undefined

-- first : A -> F
-- F is the first member of the non-empty aggregate A.
joy_first = undefined

-- rest : A -> R
-- R is the non-empty aggregate A with its first member removed.
joy_rest = undefined

-- compare : A B -> I
-- I (=-1,0,+1) is the comparison of aggregates A and B. The values correspond to the predicates <=, =, >=.
joy_compare = undefined

-- at : A I -> X
-- X (= A[I]) is the member of A at position I.
joy_at = undefined

-- of : I A -> X
-- X (= A[I]) is the I-th member of aggregate A.
joy_of = undefined

-- size : A -> I
-- Integer I is the number of elements of aggregate A.
joy_size = undefined

-- opcase : X [..[X Xs]..] -> [Xs]
-- Indexing on type of X, returns the list [Xs].
joy_opcase = undefined

-- case : X [..[X Y]..] -> Y i
-- Indexing on the value of X, execute the matching Y.
joy_case = undefined

-- uncons : A -> F R
-- F and R are the first and the rest of non-empty aggregate A.
joy_uncons = undefined

-- unswons : A -> R F
-- R and F are the rest and the first of non-empty aggregate A.
joy_unswons = undefined

-- drop : A N -> B
-- Aggregate B is the result of deleting the first N elements of A.
joy_drop = undefined

-- take : A N -> B
-- Aggregate B is the result of retaining just the first N elements of A.
joy_take = undefined

-- concat : S T -> U
-- Sequence U is the concatenation of sequences S and T.
joy_concat = undefined

-- enconcat : X S T -> U
-- Sequence U is the concatenation of sequences S and T with X inserted between S and T (== swapd cons concat)
joy_enconcat = undefined

-- name : sym -> "sym"
-- For operators and combinators, the string "sym" is the name of item sym, for literals sym the result string is its type.
joy_name = undefined

-- intern : "sym" -> sym
-- Pushes the item whose name is "sym".
joy_intern = undefined

-- body : U -> [P]
-- Quotation [P] is the body of user-defined symbol U. predicate
joy_body = undefined

-- null : X -> B
-- Tests for empty aggregate X or zero numeric.
joy_null = undefined

-- small : X -> B
-- Tests whether aggregate X has 0 or 1 members, or numeric 0 or 1.
joy_small = undefined

-- >= : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X greater than or equal to Y. Also supports float.
joy_leq = undefined

-- > : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X greater than Y. Also supports float.
joy_lt = undefined

-- <= : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X less than or equal to Y. Also supports float.
joy_geq = undefined

-- < : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X less than Y. Also supports float.
joy_gt = undefined

-- != : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X not equal to Y. Also supports float.
joy_neq = undefined

-- = : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X equal to Y. Also supports float.
joy_eq = undefined

-- equal : T U -> B
-- (Recursively) tests whether trees T and U are identical.
joy_equal = undefined

-- has : A X -> B
-- Tests whether aggregate A has X as a member.
joy_has = undefined

-- in : X A -> B
-- Tests whether X is a member of aggregate A.
joy_in = undefined

-- integer : X -> B
-- Tests whether X is an integer.
joy_integer = undefined

-- char : X -> B
-- Tests whether X is a character.
joy_char = undefined

-- logical : X -> B
-- Tests whether X is a logical.
joy_logical = undefined

-- set : X -> B
-- Tests whether X is a set.
joy_set = undefined

-- string : X -> B
-- Tests whether X is a string.
joy_string = undefined

-- list : X -> B
-- Tests whether X is a list.
joy_list = undefined

-- leaf : X -> B
-- Tests whether X is not a list.
joy_leaf = undefined

-- user : X -> B
-- Tests whether X is a user-defined symbol.
joy_user = undefined

-- float : R -> B
-- Tests whether R is a float.
joy_float = undefined

-- file : F -> B
-- Tests whether F is a file. combinator
joy_file = undefined

-- i : [P] -> ...
-- Executes P. So, [P] i == P.
joy_i = undefined

-- x : [P]i -> ...
-- Executes P without popping [P]. So, [P] x == [P] P.
joy_x = undefined

-- dip : X [P] -> ... X
-- Saves X, executes P, pushes X back.
joy_dip = undefined

-- app1 : X [P] -> R
-- Executes P, pushes result R on stack without X.
joy_app1 = undefined

-- app11 : X Y [P] -> R
-- Executes P, pushes result R on stack.
joy_app11 = undefined

-- app12 : X Y1 Y2 [P] -> R1 R2
-- Executes P twice, with Y1 and Y2, returns R1 and R2.
joy_app12 = undefined

-- construct : [P] [[P1] [P2] ..] -> R1 R2 ..
-- Saves state of stack and then executes [P]. Then executes each [Pi] to give Ri pushed onto saved stack.
joy_construct = undefined

-- nullary : [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, none are removed from the stack.
joy_nullary = undefined

-- unary : X [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, exactly one is removed from the stack.
joy_unary = undefined

-- unary2 : X1 X2 [P] -> R1 R2
-- Executes P twice, with X1 and X2 on top of the stack. Returns the two values R1 and R2.
joy_unary2 = undefined

-- unary3 : X1 X2 X3 [P] -> R1 R2 R3
-- Executes P three times, with Xi, returns Ri (i = 1..3).
joy_unary3 = undefined

-- unary4 : X1 X2 X3 X4 [P] -> R1 R2 R3 R4
-- Executes P four times, with Xi, returns Ri (i = 1..4).
joy_unary4 = undefined

-- app2 : X1 X2 [P] -> R1 R2
-- Obsolescent. == unary2
joy_app2 = undefined

-- app3 : X1 X2 X3 [P] -> R1 R2 R3
-- Obsolescent. == unary3
joy_app3 = undefined

-- app4 : X1 X2 X3 X4 [P] -> R1 R2 R3 R4
-- Obsolescent. == unary4
joy_app4 = undefined

-- binary : X Y [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, exactly two are removed from the stack.
joy_binary = undefined

-- ternary : X Y Z [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, exactly three are removed from the stack.
joy_ternary = undefined

-- cleave : X [P1] [P2] -> R1 R2
-- Executes P1 and P2, each with X on top, producing two results.
joy_cleave = undefined

-- branch : B [T] [F] -> ...
-- If B is true, then executes T else executes F.
joy_branch = undefined

-- ifte : [B] [T] [F] -> ...
-- Executes B. If that yields true, then executes T else executes F.
joy_ifte = undefined

-- ifinteger : X [T] [E] -> ...
-- If X is an integer, executes T else executes E.
joy_ifinteger = undefined

-- ifchar : X [T] [E] -> ...
-- If X is a character, executes T else executes E.
joy_ifchar = undefined

-- iflogical : X [T] [E] -> ...
-- If X is a logical or truth value, executes T else executes E.
joy_iflogical = undefined

-- ifset : X [T] [E] -> ...
-- If X is a set, executes T else executes E.
joy_ifset = undefined

-- ifstring : X [T] [E] -> ...
-- If X is a string, executes T else executes E.
joy_ifstring = undefined

-- iflist : X [T] [E] -> ...
-- If X is a list, executes T else executes E.
joy_iflist = undefined

-- iffloat : X [T] [E] -> ...
-- If X is a float, executes T else executes E.
joy_iffloat = undefined

-- iffile : X [T] [E] -> ...
-- If X is a file, executes T else executes E.
joy_iffile = undefined

-- cond : [..[[Bi] Ti]..[D]] -> ...
-- Tries each Bi. If that yields true, then executes Ti and exits. If no Bi yields true, executes default D.
joy_cond = undefined

-- while : [B] [D] -> ...
-- While executing B yields true executes D.
joy_while = undefined

-- linrec : [P] [T] [R1] [R2] -> ...
-- Executes P. If that yields true, executes T. Else executes R1, recurses, executes R2.
joy_linrec = undefined

-- tailrec : [P] [T] [R1] -> ...
-- Executes P. If that yields true, executes T. Else executes R1, recurses.
joy_tailrec = undefined

-- binrec : [B] [T] [R1] [R2] -> ...
-- Executes P. If that yields true, executes T. Else uses R1 to produce two intermediates, recurses on both, then executes R2 to combines their results.
joy_binrec = undefined

-- genrec : [B] [T] [R1] [R2] -> ...
-- Executes B, if that yields true executes T. Else executes R1 and then [[B] [T] [R1] [R2] genrec] R2.
joy_genrec = undefined

-- condlinrec : [ [C1] [C2] .. [D] ] -> ...
-- Each [Ci] is of the forms [[B] [T]] or [[B] [R1] [R2]]. Tries each B. If that yields true and there is just a [T], executes T and exit. If there are [R1] and [R2], executes R1, recurses, executes R2. Subsequent case are ignored. If no B yields true, then [D] is used. It is then of the forms [[T]] or [[R1] [R2]]. For the former, executes T. For the latter executes R1, recurses, executes R2.
joy_condlinrec = undefined

-- step : A [P] -> ...
-- Sequentially putting members of aggregate A onto stack, executes P for each member of A.
joy_step = undefined

-- fold : A V0 [P] -> V
-- Starting with value V0, sequentially pushes members of aggregate A and combines with binary operator P to produce value V.
joy_fold = undefined

-- map : A [P] -> B
-- Executes P on each member of aggregate A, collects results in sametype aggregate B.
joy_map = undefined

-- times : N [P] -> ...
-- N times executes P.
joy_times = undefined

-- infra : L1 [P] -> L2
-- Using list L1 as stack, executes P and returns a new list L2. The first element of L1 is used as the top of stack, and after execution of P the top of stack becomes the first element of L2.
joy_infra = undefined

-- primrec : X [I] [C] -> R
-- Executes I to obtain an initial value R0. For integer X uses increasing positive integers to X, combines by C for new R. For aggregate X uses successive members and combines by C for new R.
joy_primrec = undefined

-- filter : A [B] -> A1
-- Uses test B to filter aggregate A producing sametype aggregate A1.
joy_filter = undefined

-- split : A [B] -> A1 A2
-- Uses test B to split aggregate A into sametype aggregates A1 and A2 .
joy_split = undefined

-- some : A [B] -> X
-- Applies test B to members of aggregate A, X = true if some pass.
joy_some = undefined

-- all : A [B] -> X
-- Applies test B to members of aggregate A, X = true if all pass.
joy_all = undefined

-- treestep : T [P] -> ...
-- Recursively traverses leaves of tree T, executes P for each leaf.
joy_treestep = undefined

-- treerec : T [O] [C] -> ...
-- T is a tree. If T is a leaf, executes O. Else executes [[O] [C] treerec] C.
joy_treerec = undefined

-- treegenrec : T [O1] [O2] [C] -> ...
-- T is a tree. If T is a leaf, executes O1. Else executes O2 and then [[O1] [O2] [C] treegenrec] C. miscellaneous commands
joy_treegenrec = undefined

-- help : ->
-- Lists all defined symbols, including those from library files. Then lists all primitives of raw Joy (There is a variant: "_help" which lists hidden symbols).
joy_help = undefined

-- helpdetail : [ S1 S2 .. ]
-- Gives brief help on each symbol S in the list.
joy_helpdetail = undefined

-- manual : ->
-- Writes this manual of all Joy primitives to output file.
joy_manual = undefined

-- setautoput : I ->
-- Sets value of flag for automatic put to I (if I = 0, none; if I = 1, put; if I = 2, stack.
joy_setautoput = undefined

-- setundeferror : I ->
-- Sets flag that controls behavior of undefined functions (0 = no error, 1 = error).
joy_setundeferror = undefined

-- setecho : I ->
-- Sets value of echo flag for listing. I = 0: no echo, 1: echo, 2: with tab, 3: and linenumber.
joy_setecho = undefined

-- gc : ->
-- Initiates garbage collection.
joy_gc = undefined

-- system : "command" ->
-- Escapes to shell, executes string "command". The string may cause execution of another program. When that has finished, the process returns to Joy.
joy_system = undefined

-- getenv : "variable" -> "value"
-- Retrieves the value of the environment variable "variable".
joy_getenv = undefined

-- argv : -> A
-- Creates an aggregate A containing the interpreter's command line arguments.
joy_argv = undefined

-- argc : -> I
-- Pushes the number of command line arguments. This is quivalent to 'argv size'.
joy_argc = undefined

-- get : -> F
-- Reads a factor from input and pushes it onto stack.
joy_get = undefined

-- put : X ->
-- Writes X to output, pops X off stack.
joy_put = undefined

-- putch : N ->
-- N : numeric, writes character whose ASCII is N.
joy_putch = undefined

-- putchars : "abc.." ->
-- Writes abc.. (without quotes)
joy_putchars = undefined

-- include : "filnam.ext" ->
-- Transfers input to file whose name is "filnam.ext". On end-of-file returns to previous input file.
joy_include = undefined

-- abort : ->
-- Aborts execution of current Joy program, returns to Joy main cycle.
joy_abort = undefined

-- quit : ->
-- Exit from Joy.
joy_quit = liftIO exitSuccess
