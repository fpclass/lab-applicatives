--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Applicative functors                                                  --
--------------------------------------------------------------------------------

module Lab where

import Control.Monad

import Data.Bifunctor
import Data.Char
import Data.Maybe

--------------------------------------------------------------------------------
-- Parsers

data Parser a = MkParser (String -> Maybe (a, String))

-- | Runs a parser on some input. If successful, the result of the parser
-- is returned along with any remaining input.
parse :: Parser a -> String -> Maybe (a, String)
parse (MkParser f) xs = f xs

-- | A function which, given a predicate, constructs a parser that succeeds
-- if the first character in the input satisfies the predicate.
ch :: (Char -> Bool) -> Parser Char
ch p = MkParser $ \xs -> case xs of
    (y:ys) | p y -> Just (y,ys)
    _            -> Nothing

--------------------------------------------------------------------------------
-- Parsers are functors

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    -- fmap f (MkParser g) =
    --     MkParser $ fmap (first f) . g 
    fmap f (MkParser g) =
        MkParser $ \xs -> fmap (\(x,inp) -> (f x,inp)) (g xs)
    -- fmap f (MkParser g) =
    --     MkParser $ \xs -> case g xs of 
    --         Nothing -> Nothing
    --         Just (x,inp) -> Just (f x,inp)

--------------------------------------------------------------------------------
-- Parsers are applicative functors

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = MkParser $ \xs -> Just (x,xs)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (MkParser a) <*> p = MkParser (\xs -> case a xs of
        Nothing      -> Nothing
        Just (f, ys) -> let (MkParser b) = p in case b ys of
            Nothing      -> Nothing
            Just (x, zs) -> Just (f x, zs))

-- validModuleCode :: Parser Int
-- validModuleCode = 
--     (\x y a b c -> digitToInt a * 100 + 
--                    digitToInt b * 10 + 
--                    digitToInt c) <$>
--     ch (=='c') <*> 
--     ch (=='s') <*> 
--     ch isDigit <*> 
--     ch isDigit <*> 
--     ch isDigit

instance Monad Parser where 
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (MkParser m) >>= f = MkParser $ \xs -> case m xs of 
        Nothing -> Nothing
        Just (x,xs') -> let (MkParser n) = f x
                        in n xs'

instance MonadFail Parser where 
    -- fail :: String -> Parser a
    fail _ = empty

string :: String -> Parser String
string = mapM (ch . (==))

validModuleCode :: Parser Int
validModuleCode = token $ do 
    string "cs"
    [a,b,c] <- replicateM 3 (ch isDigit)
    pure $ digitToInt a * 100 
         + digitToInt b * 10 
         + digitToInt c

-- validModuleCode :: Parser Int
-- validModuleCode =  
--     ch (=='c') >>
--     ch (=='s') >>
--     ch isDigit >>= \a ->
--     ch isDigit >>= \b ->
--     ch isDigit >>= \c ->
--     pure $ digitToInt a * 100 
--          + digitToInt b * 10 
--          + digitToInt c

--------------------------------------------------------------------------------
-- Alternative

infixl 3 <|>
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    some  :: f a -> f [a]
    some p = (:) <$> p <*> many p

    many  :: f a -> f [a]
    many p = some p <|> pure []

instance Alternative Parser where
    -- empty :: Parser a
    empty = MkParser $ \xs -> Nothing

    -- (<|>) :: Parser a -> Parser a -> Parser a
    (MkParser a) <|> (MkParser b) = MkParser $ \xs ->
        case a xs of 
            Just r -> Just r 
            Nothing -> b xs

--------------------------------------------------------------------------------

nat :: Parser Integer
nat = read <$> some (ch isDigit)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

oneOf :: [Char] -> Parser Char
oneOf = choice . map (ch . (==))

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t', '\n', '\r'])

token :: Parser a -> Parser a
token p = whitespace *> p

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

--------------------------------------------------------------------------------

data Expr = Val Integer | Add Expr Expr
    deriving (Show, Eq)

-- instance Show Expr where
--     show (Val n)   = show n
--     show (Add l r) = concat ["( ", show l, " + ", show r, " )"]

eval :: Expr -> Integer
eval (Val n)   = n
eval (Add l r) = eval l + eval r

--------------------------------------------------------------------------------

lexme :: Char -> Parser Char
lexme c = token (ch (==c))

lparen :: Parser Char
lparen = lexme '('

rparen :: Parser Char
rparen = lexme ')'

plus :: Parser Char
plus = lexme '+'

val :: Parser Expr
val = Val <$> token nat

add :: Parser Expr
add = between lparen rparen $
        Add <$> expr
            <*> (plus *> token expr)

expr :: Parser Expr
expr = val <|> add

parseAndEval :: String -> Maybe Integer
parseAndEval xs = case parse (expr <* whitespace) xs of
    Just (e,[]) -> Just (eval e)
    _ -> Nothing

--------------------------------------------------------------------------------
