module Joy where

import Mascarpone as M hiding (pop')
import Control.Monad
import Data.Char
import Control.Monad.State
import Data.Fixed
import System.Exit

num = do
  Symb (Chr c) <- peek
  if not (isDigit c)
     then return Nothing
     else do pop'
             x <- num
             return $ case x of
               Nothing -> Just . Symb . Num $ read [c]
               (Just(Symb(Num x))) -> Just . Symb . Num $ read [c] * 10 + x

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

---------

-- false : -> false
-- Pushes the value false.
false = push $ Symb $ Num 0

-- true : -> true
-- Pushes the value true.
true  = push $ Symb $ Num 1

-- maxint : -> maxint
-- Pushes largest integer (platform dependent). Typically it is 32 bits.
maxint = undefined

-- setsize : -> setsize
-- Pushes the maximum number of elements in a set (platform dependent). Typically it is 32, and set members are in the range 0..31.
setsize = undefined

-- stack : .. X Y Z -> .. X Y Z [Z Y X ..]
-- Pushes the stack as a list.
stack = undefined

-- conts : -> [[P] [Q] ..]
-- Pushes current continuations. Buggy, do not use.
conts = undefined

-- autoput : -> I
-- Pushes current value of flag for automatic output, I = 0..2.
autoput = undefined

-- undeferror : -> I
-- Pushes current value of undefined-is-error flag.
undeferror = undefined

-- undefs : ->
-- Push a list of all undefined symbols in the current symbol table.
undefs = undefined

-- echo : -> I
-- Pushes value of echo flag, I = 0..3.
echo = undefined

-- clock : -> I
-- Pushes the integer value of current CPU usage in hundreds of a second.
clock = undefined

-- time : -> I
-- Pushes the current time (in seconds since the Epoch).
time = undefined

-- rand : -> I
-- I is a random integer.
rand = undefined

-- stdin : -> S
-- Pushes the standard input stream.
stdin = undefined

-- stdout : -> S
-- Pushes the standard output stream.
stdout = undefined

-- stderr : -> S
-- Pushes the standard error stream. operator
stderr = undefined

-- id : ->
-- Identity function, does nothing. Any program of the form P id Q is equivalent to just P Q.
id' = id

-- dup : X -> X X
-- Pushes an extra copy of X onto stack.
dup' = dup

-- swap : X Y -> Y X
-- Interchanges X and Y on top of the stack.
swap' = swap

-- rollup : X Y Z -> Z X Y
-- Moves X and Y up, moves Z down
rollup = pop3 >>= \(x,y,z) -> pushN [z,x,y]

-- rolldown : X Y Z -> Y Z X
-- Moves Y and Z down, moves X up
rolldown = pop3 >>= \(x,y,z) -> pushN [y,z,x]

-- rotate : X Y Z -> Z Y X
-- Interchanges X and Z
rotate = pop3 >>= \(x,y,z) -> pushN [z,y,x]

-- popd : Y Z -> Z
-- As if defined by: popd == [pop] dip
popd = pop2 >>= \(y,z) -> push z

-- dupd : Y Z -> Y Y Z
-- As if defined by: dupd == [dup] dip
dupd = pop2 >>= \(y,z) -> pushN [y,y,z]

-- swapd : X Y Z -> Y X Z
-- As if defined by: swapd == [swap] dip
swapd = pop3 >>= \(x,y,z) -> pushN [y,x,z]

-- rollupd : X Y Z W -> Z X Y W
-- As if defined by: rollupd == [rollup] dip
rollupd = pop4 >>= \(x,y,z,w) -> pushN [z,x,y,w]

-- rolldownd : X Y Z W -> Y Z X W
-- As if defined by: rolldownd == [rolldown] dip
rolldownd = pop4 >>= \(x,y,z,w) -> pushN [y,z,x,w]

-- rotated : X Y Z W -> Z Y X W
-- As if defined by: rotated == [rotate] dip
rotated = pop4 >>= \(x,y,z,w) -> pushN [z,y,x,w]

-- pop : X ->
-- Removes X from top of the stack.
pop' = void pop

-- choice : B T F -> X
-- If B is true, then X = T else X = F.
choice = do
  b     <- popBool
  (t,f) <- pop2
  return $ if b then t else f

-- or : X Y -> Z
-- Z is the union of sets X and Y, logical disjunction for truth values.
or = undefined

-- xor : X Y -> Z
-- Z is the symmetric difference of sets X and Y, logical exclusive disjunction for truth values.
xor = undefined

-- and : X Y -> Z
-- Z is the intersection of sets X and Y, logical conjunction for truth values.
and = undefined

-- not : X -> Y
-- Y is the complement of set X, logical negation for truth values.
not' = undefined

-- + : M I -> N
-- Numeric N is the result of adding integer I to numeric M. Also supports float.
add = pop2 >>= \(Symb (Num a),Symb (Num b)) -> return $ Symb $ Num $ a+b


-- - : M I -> N
-- Numeric N is the result of subtracting integer I from numeric M. Also supports float.
sub = pop2 >>= \(Symb (Num a),Symb (Num b)) -> return $ Symb $ Num $ a-b

-- * : I J -> K
-- Integer K is the product of integers I and J. Also supports float.
mul = pop2 >>= \(Symb (Num a),Symb (Num b)) -> return $ Symb $ Num $ a*b

-- / : I J -> K
-- Integer K is the (rounded) ratio of integers I and J. Also supports float.
div' = pop2 >>= \(Symb (Num a),Symb (Num b)) -> return $ Symb $ Num $ a/b

-- rem : I J -> K
-- Integer K is the remainder of dividing I by J. Also supports float.
rem' = pop2 >>= \(Symb (Num a),Symb (Num b)) -> return $ Symb $ Num $ a `mod'` b

-- div : I J -> K L
-- Integers K and L are the quotient and remainder of dividing I by J.
div = undefined

-- sign : N1 -> N2
-- Integer N2 is the sign (-1 or 0 or +1) of integer N1, or float N2 is the sign (-1.0 or 0.0 or 1.0) of float N1.
sign = undefined

-- neg : I -> J
-- Integer J is the negative of integer I. Also supports float.
neg = pop >>= \(Symb (Num a)) -> return $ Symb $ Num $ negate a

-- ord : C -> I
-- Integer I is the Ascii value of character C (or logical or integer).
ord' = pop >>= \(Symb (Chr a)) -> return $ Symb $ Num $ fromIntegral $ ord a

-- chr : I -> C
-- C is the character whose Ascii value is integer I (or logical or character).
chr' = pop >>= \(Symb (Num a)) -> return $ Symb $ Chr $ chr $ floor a

-- abs : N1 -> N2
-- Integer N2 is the absolute value (0,1,2..) of integer N1, or float N2 is the absolute value (0.0 ..) of float N1
abs' = pop >>= \(Symb (Num a)) -> return $ Symb $ Num $ abs a

-- asin : F -> G
-- G is the arc sine of F.
asin = undefined

-- atan : F -> G
-- G is the arc tangent of F.
atan = undefined

-- atan2 : F G -> H
-- H is the arc tangent of F / G.
atan2 = undefined

-- ceil : F -> G
-- G is the float ceiling of F.
ceil = undefined

-- cos : F -> G
-- G is the cosine of F.
cos = undefined

-- cosh : F -> G
-- G is the hyperbolic cosine of F.
cosh = undefined

-- exp : F -> G
-- G is e (2.718281828...) raised to the Fth power.
exp = undefined

-- floor : F -> G
-- G is the floor of F.
floor' = undefined

-- frexp : F -> G I
-- G is the mantissa and I is the exponent of F. Unless F = 0, 0.5 <= abs(G) < 1.0.
frexp = undefined

-- ldexp : F I -> G
-- G is F times 2 to the Ith power.
ldexp = undefined

-- log : F -> G
-- G is the natural logarithm of F.
log = undefined

-- log10 : F -> G
-- G is the common logarithm of F.
log10 = undefined

-- modf : F -> G H
-- G is the fractional part and H is the integer part (but expressed as a float) of F.
modf = undefined

-- pow : F G -> H
-- H is F raised to the Gth power.
pow = undefined

-- sin : F -> G
-- G is the sine of F.
sin = undefined

-- sinh : F -> G
-- G is the hyperbolic sine of F.
sinh = undefined

-- sqrt : F -> G
-- G is the square root of F.
sqrt = undefined

-- tan : F -> G
-- G is the tangent of F.
tan = undefined

-- tanh : F -> G
-- G is the hyperbolic tangent of F.
tanh = undefined

-- trunc : F -> I
-- I is an integer equal to the float F truncated toward zero.
trunc = undefined

-- localtime : I -> T
-- Converts a time I into a list T representing local time: [year month day hour minute second isdst yearday weekday]. Month is 1 = January ... 12 = December; isdst is a Boolean flagging daylight savings/summer time; weekday is 0 = Monday ... 7 = Sunday.
localtime = undefined

-- gmtime : I -> T
-- Converts a time I into a list T representing universal time: [year month day hour minute second isdst yearday weekday]. Month is 1 = January ... 12 = December; isdst is false; weekday is 0 = Monday ... 7 = Sunday.
gmtime = undefined

-- mktime : T -> I
-- Converts a list T representing local time into a time I. T is in the format generated by localtime.
mktime = undefined

-- strftime : T S1 -> S2
-- Formats a list T in the format of localtime or gmtime using string S1 and pushes the result S2.
strftime = undefined

-- strtol : S I -> J
-- String S is converted to the integer J using base I. If I = 0, assumes base 10, but leading "0" means base 8 and leading "0x" means base 16.
strtol = undefined

-- strtod : S -> R
-- String S is converted to the float R.
strtod = undefined




-- format : N C I J -> S
-- S is the formatted version of N in mode C ('d or 'i = decimal, 'o = octal, 'x or 'X = hex with lower or upper case letters) with maximum width I and minimum width J.
format = undefined

-- formatf : F C I J -> S
-- S is the formatted version of F in mode C ('e or 'E = exponential, 'f = fractional, 'g or G = general with lower or upper case letters) with maximum width I and precision J.
formatf = undefined

-- srand : I ->
-- Sets the random integer seed to integer I.
srand = undefined

-- pred : M -> N
-- Numeric N is the predecessor of numeric M.
pred = undefined

-- succ : M -> N
-- Numeric N is the successor of numeric M.
succ = undefined

-- max : N1 N2 -> N
-- N is the maximum of numeric values N1 and N2. Also supports float.
max = undefined

-- min : N1 N2 -> N
-- N is the minimum of numeric values N1 and N2. Also supports float.
min = undefined

-- fclose : S ->
-- Stream S is closed and removed from the stack.
fclose = undefined

-- feof : S -> S B
-- B is the end-of-file status of stream S.
feof = undefined

-- ferror : S -> S B
-- B is the error status of stream S.
ferror = undefined

-- fflush : S -> S
-- Flush stream S, forcing all buffered output to be written.
fflush = undefined

-- fgetch : S -> S C
-- C is the next available character from stream S.
fgetch = undefined

-- fgets : S -> S L
-- L is the next available line (as a string) from stream S.
fgets = undefined

-- fopen : P M -> S
-- The file system object with pathname P is opened with mode M (r, w, a, etc.) and stream object S is pushed; if the open fails, file:NULL is pushed.
fopen = undefined

-- fread : S I -> S L
-- I bytes are read from the current position of stream S and returned as a list of I integers.
fread = undefined

-- fwrite : S L -> S
-- A list of integers are written as bytes to the current position of stream S.
fwrite = undefined

-- fremove : P -> B
-- The file system object with pathname P is removed from the file system. is a boolean indicating success or failure.
fremove = undefined

-- frename : P1 P2 -> B
-- The file system object with pathname P1 is renamed to P2. B is a boolean indicating success or failure.
frename = undefined

-- fput : S X -> S
-- Writes X to stream S, pops X off stack.
fput = undefined

-- fputch : S C -> S
-- The character C is written to the current position of stream S.
fputch = undefined

-- fputchars : S "abc.." -> S
-- The string abc.. (no quotes) is written to the current position of stream S.
fputchars = undefined

-- fputstring : S "abc.." -> S
-- == fputchars, as a temporary alternative.
fputstring = undefined

-- fseek : S P W -> S
-- Stream S is repositioned to position P relative to whence-point W, where W = 0, 1, 2 for beginning, current position, end respectively.
fseek = undefined

-- ftell : S -> S I
-- I is the current position of stream S.
ftell = undefined

-- unstack : [X Y ..] -> ..Y X
-- The list [X Y ..] becomes the new stack.
unstack = undefined

-- cons : X A -> B
-- Aggregate B is A with a new member X (first member for sequences).
cons = undefined

-- swons : A X -> B
-- Aggregate B is A with a new member X (first member for sequences).
swons = undefined

-- first : A -> F
-- F is the first member of the non-empty aggregate A.
first = undefined

-- rest : A -> R
-- R is the non-empty aggregate A with its first member removed.
rest = undefined

-- compare : A B -> I
-- I (=-1,0,+1) is the comparison of aggregates A and B. The values correspond to the predicates <=, =, >=.
compare = undefined

-- at : A I -> X
-- X (= A[I]) is the member of A at position I.
at = undefined

-- of : I A -> X
-- X (= A[I]) is the I-th member of aggregate A.
of' = undefined

-- size : A -> I
-- Integer I is the number of elements of aggregate A.
size = undefined

-- opcase : X [..[X Xs]..] -> [Xs]
-- Indexing on type of X, returns the list [Xs].
opcase = undefined

-- case : X [..[X Y]..] -> Y i
-- Indexing on the value of X, execute the matching Y.
case' = undefined

-- uncons : A -> F R
-- F and R are the first and the rest of non-empty aggregate A.
uncons = undefined

-- unswons : A -> R F
-- R and F are the rest and the first of non-empty aggregate A.
unswons = undefined

-- drop : A N -> B
-- Aggregate B is the result of deleting the first N elements of A.
drop = undefined

-- take : A N -> B
-- Aggregate B is the result of retaining just the first N elements of A.
take = undefined

-- concat : S T -> U
-- Sequence U is the concatenation of sequences S and T.
concat = undefined

-- enconcat : X S T -> U
-- Sequence U is the concatenation of sequences S and T with X inserted between S and T (== swapd cons concat)
enconcat = undefined

-- name : sym -> "sym"
-- For operators and combinators, the string "sym" is the name of item sym, for literals sym the result string is its type.
name = undefined

-- intern : "sym" -> sym
-- Pushes the item whose name is "sym".
intern = undefined

-- body : U -> [P]
-- Quotation [P] is the body of user-defined symbol U. predicate
body = undefined

-- null : X -> B
-- Tests for empty aggregate X or zero numeric.
null = undefined

-- small : X -> B
-- Tests whether aggregate X has 0 or 1 members, or numeric 0 or 1.
small = undefined

-- >= : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X greater than or equal to Y. Also supports float.
(>=) = undefined

-- > : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X greater than Y. Also supports float.
(>) = undefined

-- <= : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X less than or equal to Y. Also supports float.
(<=) = undefined

-- < : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X less than Y. Also supports float.
(<) = undefined

-- != : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X not equal to Y. Also supports float.
(!=) = undefined

-- = : X Y -> B
-- Either both X and Y are numeric or both are strings or symbols. Tests whether X equal to Y. Also supports float.
eq' = undefined

-- equal : T U -> B
-- (Recursively) tests whether trees T and U are identical.
equal = undefined

-- has : A X -> B
-- Tests whether aggregate A has X as a member.
has = undefined

-- in : X A -> B
-- Tests whether X is a member of aggregate A.
in' = undefined

-- integer : X -> B
-- Tests whether X is an integer.
integer = undefined

-- char : X -> B
-- Tests whether X is a character.
char = undefined

-- logical : X -> B
-- Tests whether X is a logical.
logical = undefined

-- set : X -> B
-- Tests whether X is a set.
set = undefined

-- string : X -> B
-- Tests whether X is a string.
string = undefined

-- list : X -> B
-- Tests whether X is a list.
list = undefined

-- leaf : X -> B
-- Tests whether X is not a list.
leaf = undefined

-- user : X -> B
-- Tests whether X is a user-defined symbol.
user = undefined

-- float : R -> B
-- Tests whether R is a float.
float = undefined

-- file : F -> B
-- Tests whether F is a file. combinator
file = undefined

-- i : [P] -> ...
-- Executes P. So, [P] i == P.
i = undefined

-- x : [P]i -> ...
-- Executes P without popping [P]. So, [P] x == [P] P.
x = undefined

-- dip : X [P] -> ... X
-- Saves X, executes P, pushes X back.
dip = undefined

-- app1 : X [P] -> R
-- Executes P, pushes result R on stack without X.
app1 = undefined

-- app11 : X Y [P] -> R
-- Executes P, pushes result R on stack.
app11 = undefined

-- app12 : X Y1 Y2 [P] -> R1 R2
-- Executes P twice, with Y1 and Y2, returns R1 and R2.
app12 = undefined

-- construct : [P] [[P1] [P2] ..] -> R1 R2 ..
-- Saves state of stack and then executes [P]. Then executes each [Pi] to give Ri pushed onto saved stack.
construct = undefined

-- nullary : [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, none are removed from the stack.
nullary = undefined

-- unary : X [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, exactly one is removed from the stack.
unary = undefined

-- unary2 : X1 X2 [P] -> R1 R2
-- Executes P twice, with X1 and X2 on top of the stack. Returns the two values R1 and R2.
unary2 = undefined

-- unary3 : X1 X2 X3 [P] -> R1 R2 R3
-- Executes P three times, with Xi, returns Ri (i = 1..3).
unary3 = undefined

-- unary4 : X1 X2 X3 X4 [P] -> R1 R2 R3 R4
-- Executes P four times, with Xi, returns Ri (i = 1..4).
unary4 = undefined

-- app2 : X1 X2 [P] -> R1 R2
-- Obsolescent. == unary2
app2 = undefined

-- app3 : X1 X2 X3 [P] -> R1 R2 R3
-- Obsolescent. == unary3
app3 = undefined

-- app4 : X1 X2 X3 X4 [P] -> R1 R2 R3 R4
-- Obsolescent. == unary4
app4 = undefined

-- binary : X Y [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, exactly two are removed from the stack.
binary = undefined

-- ternary : X Y Z [P] -> R
-- Executes P, which leaves R on top of the stack. No matter how many parameters this consumes, exactly three are removed from the stack.
ternary = undefined

-- cleave : X [P1] [P2] -> R1 R2
-- Executes P1 and P2, each with X on top, producing two results.
cleave = undefined

-- branch : B [T] [F] -> ...
-- If B is true, then executes T else executes F.
branch = undefined

-- ifte : [B] [T] [F] -> ...
-- Executes B. If that yields true, then executes T else executes F.
ifte = undefined

-- ifinteger : X [T] [E] -> ...
-- If X is an integer, executes T else executes E.
ifinteger = undefined

-- ifchar : X [T] [E] -> ...
-- If X is a character, executes T else executes E.
ifchar = undefined

-- iflogical : X [T] [E] -> ...
-- If X is a logical or truth value, executes T else executes E.
iflogical = undefined

-- ifset : X [T] [E] -> ...
-- If X is a set, executes T else executes E.
ifset = undefined

-- ifstring : X [T] [E] -> ...
-- If X is a string, executes T else executes E.
ifstring = undefined

-- iflist : X [T] [E] -> ...
-- If X is a list, executes T else executes E.
iflist = undefined

-- iffloat : X [T] [E] -> ...
-- If X is a float, executes T else executes E.
iffloat = undefined

-- iffile : X [T] [E] -> ...
-- If X is a file, executes T else executes E.
iffile = undefined

-- cond : [..[[Bi] Ti]..[D]] -> ...
-- Tries each Bi. If that yields true, then executes Ti and exits. If no Bi yields true, executes default D.
cond = undefined

-- while : [B] [D] -> ...
-- While executing B yields true executes D.
while = undefined

-- linrec : [P] [T] [R1] [R2] -> ...
-- Executes P. If that yields true, executes T. Else executes R1, recurses, executes R2.
linrec = undefined

-- tailrec : [P] [T] [R1] -> ...
-- Executes P. If that yields true, executes T. Else executes R1, recurses.
tailrec = undefined

-- binrec : [B] [T] [R1] [R2] -> ...
-- Executes P. If that yields true, executes T. Else uses R1 to produce two intermediates, recurses on both, then executes R2 to combines their results.
binrec = undefined

-- genrec : [B] [T] [R1] [R2] -> ...
-- Executes B, if that yields true executes T. Else executes R1 and then [[B] [T] [R1] [R2] genrec] R2.
genrec = undefined

-- condlinrec : [ [C1] [C2] .. [D] ] -> ...
-- Each [Ci] is of the forms [[B] [T]] or [[B] [R1] [R2]]. Tries each B. If that yields true and there is just a [T], executes T and exit. If there are [R1] and [R2], executes R1, recurses, executes R2. Subsequent case are ignored. If no B yields true, then [D] is used. It is then of the forms [[T]] or [[R1] [R2]]. For the former, executes T. For the latter executes R1, recurses, executes R2.
condlinrec = undefined

-- step : A [P] -> ...
-- Sequentially putting members of aggregate A onto stack, executes P for each member of A.
step = undefined

-- fold : A V0 [P] -> V
-- Starting with value V0, sequentially pushes members of aggregate A and combines with binary operator P to produce value V.
fold = undefined

-- map : A [P] -> B
-- Executes P on each member of aggregate A, collects results in sametype aggregate B.
map = undefined

-- times : N [P] -> ...
-- N times executes P.
times = undefined

-- infra : L1 [P] -> L2
-- Using list L1 as stack, executes P and returns a new list L2. The first element of L1 is used as the top of stack, and after execution of P the top of stack becomes the first element of L2.
infra = undefined

-- primrec : X [I] [C] -> R
-- Executes I to obtain an initial value R0. For integer X uses increasing positive integers to X, combines by C for new R. For aggregate X uses successive members and combines by C for new R.
primrec = undefined

-- filter : A [B] -> A1
-- Uses test B to filter aggregate A producing sametype aggregate A1.
filter = undefined

-- split : A [B] -> A1 A2
-- Uses test B to split aggregate A into sametype aggregates A1 and A2 .
split = undefined

-- some : A [B] -> X
-- Applies test B to members of aggregate A, X = true if some pass.
some = undefined

-- all : A [B] -> X
-- Applies test B to members of aggregate A, X = true if all pass.
all = undefined

-- treestep : T [P] -> ...
-- Recursively traverses leaves of tree T, executes P for each leaf.
treestep = undefined

-- treerec : T [O] [C] -> ...
-- T is a tree. If T is a leaf, executes O. Else executes [[O] [C] treerec] C.
treerec = undefined

-- treegenrec : T [O1] [O2] [C] -> ...
-- T is a tree. If T is a leaf, executes O1. Else executes O2 and then [[O1] [O2] [C] treegenrec] C. miscellaneous commands
treegenrec = undefined

-- help : ->
-- Lists all defined symbols, including those from library files. Then lists all primitives of raw Joy (There is a variant: "_help" which lists hidden symbols).
help = undefined

-- helpdetail : [ S1 S2 .. ]
-- Gives brief help on each symbol S in the list.
helpdetail = undefined

-- manual : ->
-- Writes this manual of all Joy primitives to output file.
manual = undefined

-- setautoput : I ->
-- Sets value of flag for automatic put to I (if I = 0, none; if I = 1, put; if I = 2, stack.
setautoput = undefined

-- setundeferror : I ->
-- Sets flag that controls behavior of undefined functions (0 = no error, 1 = error).
setundeferror = undefined

-- setecho : I ->
-- Sets value of echo flag for listing. I = 0: no echo, 1: echo, 2: with tab, 3: and linenumber.
setecho = undefined

-- gc : ->
-- Initiates garbage collection.
gc = undefined

-- system : "command" ->
-- Escapes to shell, executes string "command". The string may cause execution of another program. When that has finished, the process returns to Joy.
system = undefined

-- getenv : "variable" -> "value"
-- Retrieves the value of the environment variable "variable".
getenv = undefined

-- argv : -> A
-- Creates an aggregate A containing the interpreter's command line arguments.
argv = undefined

-- argc : -> I
-- Pushes the number of command line arguments. This is quivalent to 'argv size'.
argc = undefined

-- get : -> F
-- Reads a factor from input and pushes it onto stack.
get = undefined

-- put : X ->
-- Writes X to output, pops X off stack.
put = undefined

-- putch : N ->
-- N : numeric, writes character whose ASCII is N.
putch = undefined

-- putchars : "abc.." ->
-- Writes abc.. (without quotes)
putchars = undefined

-- include : "filnam.ext" ->
-- Transfers input to file whose name is "filnam.ext". On end-of-file returns to previous input file.
include = undefined

-- abort : ->
-- Aborts execution of current Joy program, returns to Joy main cycle.
abort = undefined

-- quit : ->
-- Exit from Joy.
quit = exitSuccess
