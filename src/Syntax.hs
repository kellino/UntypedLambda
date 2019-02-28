{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import Data.Typeable
import GHC.Generics (Generic)
import Data.Text

type TmName = Name Term

data Term =
        Var TmName
      | Lam (Bind TmName Term)
      | App Term Term
      deriving (Show, Generic, Typeable)


instance Alpha Term where

instance Subst Term Term where
    isvar (Var x) = Just (SubstName x)
    isvar _ = Nothing
