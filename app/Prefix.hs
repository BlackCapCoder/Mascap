module Prefix where

import qualified Data.Map as M
import Data.List hiding (find)
import Control.Arrow
import Mascarpone hiding (install)
import Control.Monad.State
import Debug.Trace

data Prefix a b = Prefix (M.Map a (Prefix a b)) (Maybe b)
  deriving Show


fromList :: (Eq a, Ord a) => [([a], b)] -> Prefix a b
fromList ws
  = Prefix rst def
  where ss  = sortOn fst $ filter (not.null.fst) ws
        gs  = groupBy (\a b -> head (fst a) == head (fst b)) ss
        def | [x] <- filter (null.fst) ws = Just $ snd x | otherwise = Nothing
        rst = M.fromList . flip map gs $ \g ->
          (head . fst $ head g, fromList $ first tail<$>g)

find :: (Ord a) => Prefix a b -> b -> [a] -> (b, [a])
find (Prefix m d) f s
  | null s = (f', s)
  | Just x <- M.lookup (head s) m = find x f' $ tail s
  | otherwise = (f', s)
  where f' | Just x <- d = x | otherwise = f

toInterp :: Interpreter -> Prefix Symbol Effect -> Interpreter
toInterp p (Prefix m d) =
    Interpreter
      { parent   = p
      , codepage = M.map install m
      , fallback = \s -> do
          case d of Just x -> x;_ -> nop
          push $ Intr p
          deify
          push $ Intr p
          push $ Symb s
          extract
          perform
      }

install :: Prefix Symbol Effect -> Effect
install p@(Prefix m b)
  | M.null m, Just x <- b = x
  | otherwise = do
      -- liftIO $ print $ M.keys m
      i <- gets interpreter
      modify $ \s -> s { interpreter = toInterp i p }
