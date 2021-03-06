module Mascap where

import Mascarpone hiding (run, initialState)
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Joy (joyInterp)


newInterp
  = Interpreter (defCodepage `mappend` newCodepage) nullInterpreter $ const nop

initialState
  = State newInterp []

run :: String -> IO ()
run code = void $ runStateT (interpret code) initialState


-----

newCodepage = M.fromList
  [ (Chr '+', combine)
  , (Chr 'C', pushCap)
  , (Chr 'D', pushDef)
  , (Chr 'J', pushJoy)
  ]

combine = pop2 >>= \(Intr i, Intr j) -> push . Intr $ i `mappend` j
pushDef = push $ Intr defaultInterpreter
pushCap = push $ Intr newInterp
pushJoy = push $ Intr joyInterp
