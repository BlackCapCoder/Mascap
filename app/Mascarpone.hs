module Mascarpone where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M

-- Note: Numbers are not standard mascarpone, I chose to
--       include them for efficiency
data Symbol = Chr Char | Num Double
            deriving (Ord, Eq, Show)

type Effect = StateT ProgState IO ()

data Interpreter
  = Interpreter
    { codepage :: M.Map Symbol Effect
    , parent   :: Interpreter
    , fallback :: Symbol -> Effect
    }

data StackElem = Symb Symbol
               | Op   Effect
               | Intr Interpreter

data ProgState = State
  { interpreter :: Interpreter
  , stack       :: [StackElem]
  }

instance Monoid Interpreter where
  mempty = Interpreter M.empty mempty $ const nop
  mappend (Interpreter ca pa fa)
          (Interpreter cb pb fb)
            = Interpreter (ca `mappend` cb) pa (fa >> fb)


defaultInterpreter, emptyInterpreter, nullInterpreter :: Interpreter

defaultInterpreter
  = Interpreter defCodepage nullInterpreter $ const nop

emptyInterpreter
  = Interpreter M.empty emptyInterpreter $ const nop

nullInterpreter
  = Interpreter M.empty nullInterpreter
  $ error "Tried to interpret with the null interpreter"



initialState
  = State defaultInterpreter []


interpret :: String -> Effect
interpret code = gets interpreter
             >>= flip interpretWith code

interpretWith :: Interpreter -> String -> Effect
interpretWith i code = forM_ code $ \op -> do
  (Interpreter c _ f) <- gets interpreter
  let o = Chr op
  fromMaybe (f o)
    $ M.lookup o c

run :: String -> IO ()
run code = void $ runStateT (interpret code) initialState

--------------


push :: StackElem -> Effect
push x = modify $ \s -> s { stack = x : stack s }

pop :: StateT ProgState IO StackElem
pop = do
  s <- gets stack
  case s of (x:xs) -> do
              modify $ \s -> s { stack = xs }
              return x
            _ -> error "Not enough elements on the stack"

pop2 = (, ) <$> pop <*> pop
pop3 = (,,) <$> pop <*> pop <*> pop
pop4 = (,,,) <$> pop <*> pop <*> pop <*> pop

peek = do
  a <- pop
  push a
  return a

popString :: StateT ProgState IO String
popString = do
  Symb (Chr ']') <- pop
  popString'
 where popString' = do
         Symb (Chr s) <- pop
         case s of
             '[' -> return ""
             ']' -> popString' >>= \str -> return $ '[' : str ++ "]"
             _   -> (++[s]) <$> popString'

nop = return ()


defCodepage
  = M.fromList
    [ (Chr 'v', reify)
    , (Chr '^', deify)
    , (Chr '>', extract)
    , (Chr '<', install)
    , (Chr '{', getParent)
    , (Chr '}', setParent)
    , (Chr '*', create)
    , (Chr '@', expand)
    , (Chr '!', perform)
    , (Chr '0', nulli)
    , (Chr '1', uniform)
    , (Chr '[', deepquote)
    , (Chr '\'', quotesym)
    , (Chr '.', outp)
    , (Chr ',', readInp)
    , (Chr ':', dup)
    , (Chr '$', pop')
    , (Chr '/', swap)
    ]

reify = gets interpreter >>= push . Intr
deify = pop >>= \(Intr i) -> modify $ \s -> s { interpreter = i }

extract = do
  (Symb (Chr s), Intr i) <- pop2
  push . Op $ interpretWith i [s]

install   = do
  (Symb s, Op o, Intr i) <- pop3
  push . Intr $ i { codepage = M.insert s o $ codepage i}

getParent = do
  Intr i <- pop
  push . Intr $ parent i

setParent = do
  (Intr i, Intr j) <- pop2
  push $ Intr i { parent = j }

create = do
  Intr i <- pop
  str    <- popString
  push . Op $ interpretWith i str

expand = error "Expand not implemented"

perform = pop >>= \(Op e) -> e
nulli   = push $ Intr nullInterpreter

uniform = do
  i <- gets interpreter
  Op o <- pop
  push $ Intr $ Interpreter M.empty i $ const o

deepquote = do
  push . Symb $ Chr '['
  i <- gets interpreter
  push . Intr $ Interpreter M.empty i $ \c ->
    case c of
      (Chr '[') -> deepquote
      (Chr ']') -> push (Symb $ Chr ']') >> push (Intr i) >> deify
      _   -> push $ Symb c
  deify

quotesym = do
  i <- gets interpreter
  push . Intr . Interpreter M.empty i $ \c -> do
    push $ Symb c
    push $ Intr i
    deify
  deify

outp = do
  Symb x <- pop
  case x of
    Chr s -> liftIO $ putChar s
    Num n -> liftIO $ putStr $ show n


readInp = liftIO getChar >>= push . Symb . Chr

dup = peek >>= push

pop' = void pop

swap = gets stack >>= \s -> case s of
    (x:y:ys) -> pop' >> pop' >> push x >> push y
    _        -> nop

