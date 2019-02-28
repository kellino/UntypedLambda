module Eval where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import Data.Typeable
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (pack)

import Syntax
import Env

fvSet :: (Alpha a, Typeable b) => a -> S.Set (Name b)
fvSet = S.fromList . toListOf fv

step :: String -> Term -> Eval ()
step s t = do
    depth <- get
    tell [(s, depth, t)]
    modify $ \(Depth d) -> Depth (d + 1)
    return ()

eval :: Term -> Eval Term
eval t@(Var x) = do 
    env <- ask
    case M.lookup x env of
      Nothing -> return t
      Just v  -> return v

eval t@(Lam bnd) = do
    (x, e) <- unbind bnd
    e' <- eval e
    case e of
      App e1 (Var y) | y == x && x `S.notMember` fvSet e1 -> return e1
      _ -> return (Lam (bind x e'))

eval (App t1 t2) = do
    v1 <- eval t1
    v2 <- eval t2
    case v1 of
      (Lam bnd) -> do
          (x, body) <- unbind bnd
          let body' = subst x v2 body
          step "Application" body'
          eval body'

      _ -> return (App v1 v2)
