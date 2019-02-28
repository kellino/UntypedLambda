{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric #-}

module Parser where

import           Unbound.Generics.LocallyNameless
import           Data.Typeable
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Char                      ( isUpper )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Control.Monad
import qualified Data.Text                     as T
import           Data.List
import           Data.Function                  ( on )
import           Control.Monad.State

import           Syntax


------------
-- Parser --
------------

type Parser = StateT ParseState (Parsec Void T.Text)

type Error = ParseErrorBundle T.Text Void

type RawData t e = [Either (ParseError t e) Decl]

data Decl =
        Def TmName Term
      | FixityDecl FixitySpec
    deriving (Show, Generic, Typeable)
instance Alpha Decl where

pTest :: Parser a -> T.Text -> Either Error a
pTest p = runParser (evalStateT p initParseState) "<stdin>"

parseProgram :: T.Text -> Either Error [Either (ParseError T.Text Void) Decl]
parseProgram = runParser (evalStateT rawData initParseState) "<file>"

rawData :: Parser [Either (ParseError T.Text Void) Decl]
rawData = between scn eof (e `sepEndBy` scn)
  where
    e = withRecovery recover (Right <$> decl)
    recover err = Left err <$ manyTill anySingle eol


---------------
-- Operators --
---------------

data FixitySpec = FixitySpec
    { fixity :: Fixity
    , fixityName :: String
    , definition :: Term
    } deriving (Show, Generic)

instance Alpha FixitySpec where

data Assoc = L | R | N deriving (Show, Eq, Ord, Generic)

data Fixity  =
    Infix Assoc Int
      | Prefix Int
      | Postfix Int
      deriving (Show, Eq, Ord, Generic)

instance Alpha Assoc
instance Alpha Fixity

newtype ParseState = ParseState { fixities :: [FixitySpec] }
    deriving Show

initParseState :: ParseState
initParseState = ParseState defaultOps

defaultOps :: [FixitySpec]
defaultOps = []

opChars :: String
opChars = ":!#$%&*+./<=>?@\\^|-~"

operator :: Parser String
operator = some $ oneOf opChars

fixityPrec :: FixitySpec -> Int
fixityPrec (FixitySpec (Infix _ n) _ _) = n
fixityPrec FixitySpec{}                 = 0

mkTable :: [FixitySpec] -> [[Operator Parser Term]]
mkTable ops = map (map toParser) $ groupBy ((==) `on` fixityPrec) $ sortBy
    (flip compare `on` fixityPrec)
    ops

toParser :: FixitySpec -> Operator Parser Term
toParser (FixitySpec ass tok fc) = case ass of
    Infix L _ -> InfixL ((\x y -> App (App fc x) y) <$ symbol (T.pack tok))
    Infix R _ -> undefined
    Infix N _ -> undefined

--lambda :: Parser Term
--lambda = do
    --symbol "\\" <|> symbol "λ"
    --var <- identifier
    --symbol "."
    --Lam . bind (string2Name var) <$> expr
    --lam :: String -> Term -> Term
    --lam x e = Lam (bind (string2Name x) e)
  
fixDef :: Parser Term
fixDef = do
    v1 <- identifier
    op <- lexeme operator
    v2 <- identifier
    symbol "="
    body <- expr
    return $ foldr lam body [v1,v2] 

fixitySpec :: Parser FixitySpec
fixitySpec = do
    assoc <- fixity
    prec  <- precedence
    op    <- operator <* scn
    def   <- fixDef
    let spec = FixitySpec (assoc prec) op def
    addOperator spec
    return spec
  where
    fixity =
        Infix L
            <$  symbol "infixl"
            <|> Infix R
            <$  symbol "infixr"
            <|> Infix N
            <$  symbol "infix"

addOperator :: FixitySpec -> Parser ()
addOperator f@(FixitySpec fixity op _) =
    modify $ \st -> st { fixities = f : fixities st }

precedence :: Parser Int
precedence = do
    n <- lexeme L.decimal
    if n <= 10
        then return (fromInteger n)
        else empty <?> "Invalid Operator precedence"


-----------------
-- Combinators --
-----------------

lineCmnt, blockCmnt :: Parser ()
lineCmnt = L.skipLineComment "--"
blockCmnt = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void $ oneOf (" \t" :: String)) lineCmnt empty

scn :: Parser ()
scn = L.space (void spaceChar) lineCmnt blockCmnt

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = (lexeme . try) p
  where
    p = do
        x <- letterChar
        when (isUpper x) (fail "variable must start with a lower case letter")
        xs <- many alphaNumChar
        return $ x : xs

variable :: Parser Term
variable = Var . string2Name <$> identifier

lambda :: Parser Term
lambda = do
    symbol "\\" <|> symbol "λ"
    var <- identifier
    symbol "."
    Lam . bind (string2Name var) <$> expr

term :: Parser Term
term = do
    tms <- some factor
    return $ foldl1 App tms

factor :: Parser Term
factor = choice [parens expr, lambda, variable]

def :: Parser Decl
def = do
    name <- identifier
    bnds <- many identifier
    symbol "="
    body <- expr <* scn
    let fun = foldr lam body bnds
    return $ Def (string2Name name) fun

lam :: String -> Term -> Term
lam x e = Lam (bind (string2Name x) e)

fixityDecl :: Parser Decl
fixityDecl = do
    fs <- fixitySpec <* scn
    return $ FixityDecl fs

expr :: Parser Term
expr = do
    st <- get
    let ops = mkTable (fixities st)
    makeExprParser term ops <?> "expression"

decl :: Parser Decl
decl = choice [fixityDecl, def]
