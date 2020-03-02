--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Applicative functors                                                  --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Char

import qualified Lab as L

--------------------------------------------------------------------------------

examples :: [(String, Maybe Integer)]
examples =
    [ ("4", Just 4)
    , ("(15 + 16)", Just 31)
    , ("((2 + 4) + (9 + 2))", Just 17)
    , ("(2", Nothing)
    ]

ws :: [Char]
ws = [' ', '\t', '\n', '\r']

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "ch" $ do
        it "fails when given the empty list" $
            L.parse (L.ch undefined) "" `shouldBe` Nothing
        it "fails when given an input which does not satisfy the predicate" $
            L.parse (L.ch (=='x')) "zyx" `shouldBe` Nothing
        prop "succeeds when given an input which does satisfy the predicate" $
            \x xs -> L.parse (L.ch (==x)) (x:xs) == Just (x,xs)
    describe "Parser is a Functor" $ do
        it "applies isUpper to the result of a successful parser" $
            L.parse (fmap isUpper (L.ch (=='x'))) "xyz"
            `shouldBe` Just (False,"yz")
        it "fails when applying isUpper to the result of a failed parser" $
            L.parse (fmap isUpper (L.ch (=='y'))) "xyz"
            `shouldBe` Nothing
        it "applies digitToInt to the result of a successful parser" $
            L.parse (fmap digitToInt (L.ch isDigit)) "1xy"
            `shouldBe` Just (1,"xy")
        it "fails when applying digitToInt to the result of a failed parser" $
            L.parse (fmap digitToInt (L.ch isDigit)) "xy"
            `shouldBe` Nothing
    describe "Parser is an Applicative" $ do
        prop "pure always returns the value it is given and ignores the input" $
            \(xs :: String) (n :: Int) -> L.parse (pure n) xs == Just (n, xs)
        it "<*> applies a function returned by one parser to the result of another, successful parser" $
            L.parse (pure digitToInt <*> L.ch isDigit) "1yz"
            `shouldBe` Just (1, "yz")
        it "<*> fails if the first parser fails" $
            L.parse (L.MkParser (\xs -> Nothing) <*> L.ch isDigit) "1yz"
            `shouldBe` (Nothing :: Maybe (Bool, String))
        it "<*> fails if the second parser fails" $
            L.parse (pure digitToInt <*> L.ch isDigit) "xyz"
            `shouldBe` Nothing
        prop "<$> and <*> can be used to combine the results of multiple parsers" $
            \(a :: Char) (b :: Char) (zs :: String) ->
                L.parse ((\x y -> [x,y]) <$> L.ch (==a) <*> L.ch (==b)) (a:b:zs)
                == Just ([a,b],zs)
    describe "Parser is an Alternative" $ do
        prop "empty always fails" $ \(xs :: String) ->
            L.parse L.empty xs == (Nothing :: Maybe (Bool, String))
        prop "<|> succeeds if one parser succeeds" $
            \(a :: Char) (b :: Char) (zs :: String) ->
                let r = L.parse (L.ch (==a) L.<|> L.ch (==b)) zs in case zs of
                    (z:ds) | z==a || z==b ->
                        let (Just (c,cs)) = r in (c==a || c==b) && ds==cs
                    _ -> r == Nothing
        it "some fails if the parser never succeeds" $
            L.parse (L.some (L.ch isDigit)) "xyz" `shouldBe` Nothing
        it "many returns the empty list if the parser never succeeds" $
            L.parse (L.many (L.ch isDigit)) "xyz" `shouldBe` Just ([],"xyz")
        it "some succeeds if the parser succeeds once" $
            L.parse (L.some (L.ch isDigit)) "1xyz" `shouldBe` Just ("1","xyz")
        it "many succeeds if the parser succeeds at least once" $
            L.parse (L.many (L.ch isDigit)) "1xyz" `shouldBe` Just ("1","xyz")
        it "some succeeds if the parser succeeds more than once" $
            L.parse (L.some (L.ch isDigit)) "123xyz" `shouldBe` Just ("123","xyz")
        it "many succeeds if the parser succeeds more than once" $
            L.parse (L.many (L.ch isDigit)) "123xyz" `shouldBe` Just ("123","xyz")
    describe "token" $ do
        prop "succeeds if there is no whitespace" $
            \xs -> forAll (arbitrary `suchThat` (`notElem` ws)) $ \x ->
            L.parse (L.token (L.ch (==x))) (x:xs) == Just (x, xs)
        prop "succeeds if there is whitespace" $
            \n xs -> forAll (arbitrary `suchThat` (`notElem` ws)) $ \x ->
            L.parse (L.token (L.ch (==x))) (replicate n ' ' ++ x:xs)
            == Just (x, xs)
    describe "between" $ do
        prop "succeeds if all three parsers succeed in sequence" $
            \(Positive (n :: Int)) ->
            forAll (arbitrary `suchThat` (not . isDigit)) $ \o ->
            forAll (arbitrary `suchThat` (not . isDigit)) $ \c ->
            \xs ->
            L.parse (L.between
                    (L.ch (==o))
                    (L.ch (==c))
                    (L.some (L.ch isDigit)))
                (o:(show n)++(c:xs))
            == Just (show n,xs)
    describe "nat" $ do
        prop "parses natural numbers" $ \(Positive n) xs ->
            let suffix = ' ' : xs
            in L.parse L.nat (show n ++ suffix) `shouldBe` Just (n, suffix)
    describe "primitives (lparen, rparen, plus)" $ do
        prop "lparen parses left parentheses" $ \xs n ->
            L.parse L.lparen (replicate n ' ' ++ ('(' : xs)) `shouldBe` Just ('(', xs)
        prop "rparen parses right parentheses" $ \xs n ->
            L.parse L.rparen (replicate n ' ' ++ (')' : xs)) `shouldBe` Just (')', xs)
        prop "plus parses +" $ \xs n ->
            L.parse L.plus (replicate n ' ' ++ ('+' : xs)) `shouldBe` Just ('+', xs)
        prop "lparen fails on other input" $ forAll (except '(') $ \x n ->
            L.parse L.lparen (replicate n ' ' ++ [x]) `shouldBe` Nothing
        prop "rparen fails on other input" $ forAll (except ')') $ \x n ->
            L.parse L.rparen (replicate n ' ' ++ [x]) `shouldBe` Nothing
        prop "plus fails on other input" $ forAll (except '+') $ \x n ->
            L.parse L.plus (replicate n ' ' ++ [x]) `shouldBe` Nothing
    describe "val" $ do
        prop "parses natural numbers with arbitrary amounts of preceding whitespace" $
            \(Positive n) (Positive m) -> L.parse L.val (replicate n ' ' ++ show m) `shouldBe`
                Just (L.Val m, "")
    describe "parseAndEval" $ do
        it "successfully parses the examples from the specification" $
            examples `shouldSatisfy` all (\(xs,r) -> L.parseAndEval xs == r)


except :: Char -> Gen Char
except c = suchThat arbitrary (\x -> x /= c && not (elem x ws))

--------------------------------------------------------------------------------
