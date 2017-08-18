module Prefix where

import qualified Data.Map as M
import Data.List hiding (find)
import Control.Arrow
import Mascarpone
import Control.Monad.State

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

toInterp (Prefix m d) f = Interpreter
  { codepage = M.empty
  , parent   = f
  , fallback = \s -> do
      case s of
           Chr x | Just p' <- M.lookup x m ->
             modify $ \st -> st { interpreter = toInterp p' f }
                 | Just x <- d  -> x
                 | otherwise -> nop
           _     -> nop
  }
