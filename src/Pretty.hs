{-# LANGUAGE DefaultSignatures, GeneralisedNewtypeDeriving, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Pretty where

import           Prelude                 hiding ( (<>) )
import           Text.PrettyPrint.HughesPJ
import           Data.Typeable
import           Unbound.Generics.LocallyNameless
import           Control.Monad.Reader
import           Control.Monad.Identity
import qualified Data.Text                     as T
import qualified Data.Set                      as S
import Data.List

import           Syntax
import           Env

class Disp d where
    disp :: d -> Doc

    default disp :: (Display d) => d -> Doc
    disp = cleverDisp

instance Disp Term
instance Disp Step
instance Typeable a => Disp (Name a)

newtype DispInfo = DI { dispAvoid :: S.Set AnyName }

initDI :: DispInfo
initDI = DI S.empty

cleverDisp :: (Display d) => d -> Doc
cleverDisp d = runReaderDI (display d) initDI

newtype ReaderDI a = ReaderDI (ReaderT DispInfo Identity a)
    deriving (Functor, Applicative, Monad, MonadReader DispInfo)

runReaderDI :: ReaderDI a -> DispInfo -> a
runReaderDI (ReaderDI d) = runReader d

instance LFresh ReaderDI where
    lfresh nm = do
        let s = name2String nm
        di <- ask
        return $ head
            (filter (\x -> AnyName x `S.notMember` dispAvoid di)
                    (map (makeName s) [0 ..])
            )
    getAvoids = asks dispAvoid
    avoid names = local upd      where
        upd di = di { dispAvoid = S.fromList names `S.union` dispAvoid di }

type M a = ReaderDI a

class (Alpha t) => Display t where
    display :: t -> M Doc

gatherBinds :: Term -> M ([Doc], Doc)
gatherBinds (Lam b) = lunbind b $ \(n, body) -> do
    dn           <- display n
    (rest, body) <- gatherBinds body
    return (dn : rest, body)
gatherBinds t = do
    dt <- display t
    return ([], dt)

instance Display Term where
    display (    Var x) = display x

    display lam@(Lam t) = do
        (binds, body) <- gatherBinds lam
        let lams = text "λ" : intersperse (text "λ") binds
        return $ hang (sep lams <+> text ".") 2 body

    display (App f x) = do
        df <- display f
        dx <- display x
        return $ parens $ df <+> dx

instance Typeable a => Display (Name a) where
    display n = return $ (text . name2String) n

instance Display Depth where
    display (Depth d) = return . text . show $ d

instance Display Step where
    display (s, d, t) = do
        dd <- display d
        dt <- display t
        return $ text "(" <> text s <+> dd <> comma <+> dt <> text ")"
