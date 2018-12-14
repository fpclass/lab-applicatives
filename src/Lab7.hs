--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 7: Applicative functors                                                --
--------------------------------------------------------------------------------

module Lab7 where

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
    (y:ys) | p y -> undefined
    _            -> undefined

--------------------------------------------------------------------------------
-- Parsers are functors

instance Functor Parser where
    fmap f (MkParser g) =
        MkParser $ \xs -> undefined

--------------------------------------------------------------------------------
-- Parsers are applicative functors

instance Applicative Parser where
    pure x = undefined

    (MkParser a) <*> p = MkParser (\xs -> case a xs of
        Nothing      -> undefined
        Just (f, ys) -> let (MkParser b) = p in case b ys of
            Nothing      -> undefined
            Just (x, zs) -> undefined)

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
    empty = undefined

    (MkParser a) <|> (MkParser b) =
        undefined

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
token p = undefined

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = undefined

--------------------------------------------------------------------------------

data Expr = Val Integer | Add Expr Expr
    deriving Eq

instance Show Expr where
    show (Val n)   = show n
    show (Add l r) = concat ["( ", show l, " + ", show r, " )"]

eval :: Expr -> Integer
eval (Val n)   = n
eval (Add l r) = eval l + eval r

--------------------------------------------------------------------------------

lparen :: Parser Char
lparen = undefined

rparen :: Parser Char
rparen = undefined

plus :: Parser Char
plus = undefined

val :: Parser Expr
val = Val <$> token nat

add :: Parser Expr
add = between lparen rparen $
        Add <$> expr
            <*> (plus *> token expr)

expr :: Parser Expr
expr = undefined

parseAndEval :: String -> Maybe Integer
parseAndEval xs = eval . fst <$> parse expr xs

--------------------------------------------------------------------------------
