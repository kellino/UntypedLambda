{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Env where


import Unbound.Generics.LocallyNameless
import Data.Typeable
import GHC.Generics
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as M
import qualified Data.Text as T

import Syntax
import Parser

-------------
-- Context --
-------------

type Err = T.Text

type Delta = [TmName]

type Env = M.Map TmName Term

data Gamma = Gamma { env :: Env, delta :: Delta }

initEnv :: Env
initEnv = M.empty

toEnv :: [Decl] -> Env -> Env
toEnv [] env = env
toEnv (Def x trm : ds) env = M.insert x trm (toEnv ds env)
toEnv (_ : ds) env = toEnv ds env

----------------
-- Evaluation --
----------------

newtype Depth = Depth { depth :: Int }
    deriving (Typeable, Generic)

instance Show Depth where
    show = show . depth

instance Alpha Depth

initState :: Depth
initState = Depth 0

type Step = (String, Depth , Term)

type Eval = FreshMT (RWST Env [Step] Depth (ExceptT Err IO))

runEval :: Env -> Eval a -> IO (Either Err (a, [Step]))
runEval env m = runExceptT $ evalRWST (runFreshMT m) env initState
    
