module SKI (SKI,
    evaluate,
    skiPrint,
    treePrint,
    readNonAssociativeNodesOnlyStr,
    readSKI,readSKI2,
    BoolExpr,
    boolEncode,
    boolTest,
    readMul,
) where

import Data.List.NonEmpty ( NonEmpty(..), fromList )
import Data.List.Extra (drop1, dropEnd1)
import Data.Bifunctor (second)
import Data.Either (lefts,rights)
import Data.Bool (bool)
import Data.Maybe (fromJust)

data SKI = S | K | I | SKIVar String deriving (Eq)

instance Show SKI where
    show S = "S"
    show K = "K"
    show I = "I"
    show (SKIVar y) = "{"++y++"}"

newtype ParseError = ParseError String deriving (Show)

readSKISym :: String -> Either ParseError SKI
readSKISym "S" = Right S
readSKISym "K" = Right K
readSKISym "I" = Right I
readSKISym x = let (inCurly,contents) = isInCurly x in if inCurly then Right $ SKIVar contents else Left $ ParseError "Variable names must be enclosed in {}"

isInCurly :: String -> (Bool,String)
isInCurly x = if length x>2 && (head x=='{' && last x == '}') then (True,(drop1 . dropEnd1) x) else (False,x)

data Tree a = Node a | Tree (Tree a) (Tree a) deriving (Eq)

-- only terminates on normalizable expressions, the boolean is whether or not it was normal already
evaluate :: Tree SKI -> (Bool,Tree SKI)
evaluate (Node x) = (True,Node x)
evaluate (Tree (Node I) y) = (False,snd $ evaluate y)
evaluate (Tree (Tree (Node K) x) y) = (False,snd $ evaluate x)
evaluate (Tree (Tree (Tree (Node S) x) y) z) = let
    x2 = snd $ evaluate x
    y2 = snd $ evaluate y
    z2 = snd $ evaluate z
    in (False,snd . evaluate $ Tree (Tree x2 z2) (Tree y2 z2))
evaluate (Tree x y) = if irreducible then (True,Tree x y) else (False,snd $ evaluate $ Tree x2 y2) where
    (irr1,x2) = evaluate x
    (irr2,y2) = evaluate y
    irreducible = irr1 && irr2

skiPrint :: Tree SKI -> String
skiPrint (Node x) = show x
skiPrint (Tree x y) = let parenthesize = (\t p1 -> if parensUnnecessary t then p1 else "("++p1++")") in skiPrint x ++ parenthesize y (skiPrint y) where
    parensUnnecessary (Node _) = True
    parensUnnecessary _ = False

treePrint :: Show a => (Bool,Bool) -> Tree a -> String
treePrint _ (Node x) = show x
-- b means must parenthesize trailing nodes or can ....(b) be rewritten as ....b with the parentheses assumed
treePrint (all_parens,b) (Tree x y) = let parenthesize = (\p1 do_paren -> if not do_paren then p1 else "("++p1++")") in
    parenthesize (treePrint (all_parens,b) x) all_parens ++ parenthesize (treePrint (all_parens,b) y) ((all_parens || b) || not (parensUnnecessary y)) where
    parensUnnecessary (Node _) = True
    parensUnnecessary _ = False

leftAssociative :: NonEmpty SKI -> Tree SKI
leftAssociative (x:|[]) = Node x
leftAssociative (x:|ys) = foldl (\t n -> Tree t (Node n)) (Node x) ys

readSKINoParensUnsafe :: String -> Either ParseError (Tree SKI)
readSKINoParensUnsafe x = if alright then Right $ (leftAssociative . fromList) goodPieces else Left (ParseError "One of the pieces was not a valid SKI node w/ {} variables") where
    piecesList = readSKISym <$> tokensList x
    alright = null (lefts piecesList)
    goodPieces = rights piecesList

-- every character is it's own element of the list except those in {} which are all put together as one string
-- assuming no nesting of curly braces, that will give a result that will produce ParseError for readSKINoParensUnsafe
tokensList :: String -> [String]
tokensList str = let pieces = splitOn (`elem` "{}") str in combine pieces where
    combine :: [String] -> [String]
    combine [] = []
    combine ("{":(a:("}":rest))) = ("{"++a++"}"):combine rest
    combine (other:rest) = ((: []) <$> other) ++ combine rest

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f [] = []
splitOn f [x] = [[x]]
splitOn f (x:y:ys) = let rest = splitOn f (y:ys) in if f x || f y then [x]:rest else concatenateFirst x rest where
    concatenateFirst :: a -> [[a]] -> [[a]]
    concatenateFirst x [] = [[x]]
    concatenateFirst x (y:ys) = (x:y):ys

reduceStack :: NonEmpty (Tree a) -> [Tree a]
reduceStack (x:|[]) = [x]
reduceStack (x:|(y:zs)) = reduceStack (Tree y x :| zs)

data Parenthesizer = OpenParen | CloseParen
readNonAssociative :: [a] -> (a -> Either ParseError (Either Parenthesizer (Tree b))) -> [Tree b] -> [Tree b]
readNonAssociative [] f [] = []
readNonAssociative [] f (x:xs) = reduceStack (x:|xs)
readNonAssociative (x:xs) f st = go (f x) x xs f st where
    go (Left msg) _ _ _ _ = []
    go (Right (Right xprime)) x xs f st = readNonAssociative xs f (xprime:st)
    go (Right (Left OpenParen)) x xs f st = readNonAssociative xs f st
    go (Right (Left CloseParen)) x xs f st = if length st>1 then readNonAssociative xs f ((Tree y x):z) else readNonAssociative xs f st where
        x=head st
        y=head . tail $ st
        z=tail . tail $ st

readNonAssociativeNodesOnly :: [Either Parenthesizer a] -> [Tree a]
readNonAssociativeNodesOnly input = readNonAssociative input (Right . second Node) []
readNonAssociativeNodesOnlyStr :: (Read a) => String -> [Tree a]
readNonAssociativeNodesOnlyStr str = readNonAssociativeNodesOnly $ g <$> splitOn (`elem` "()") str where
    g :: (Read a) => String -> Either Parenthesizer a
    g "(" = Left OpenParen
    g ")" = Left CloseParen
    g x = Right $ read x

maybeFromSingleton :: [a] -> Maybe a
maybeFromSingleton [] = Nothing
maybeFromSingleton [x] = Just x
maybeFromSingleton (x:xs) = Nothing

eitherShuffle :: Either a b -> Either a (Either c b)
eitherShuffle (Left msg) = Left msg
eitherShuffle (Right dat) = Right $ Right dat

readSKI :: String -> Maybe (Tree SKI)
readSKI = maybeFromSingleton . readSKI2
readSKI2 :: String -> [Tree SKI]
readSKI2 str = readNonAssociative (fixTrailing 0 True $ splitOn (`elem` "()") $ str) f [] where
    f :: String -> Either ParseError (Either Parenthesizer (Tree SKI))
    f "(" = Right $ Left OpenParen
    f ")" = Right $ Left CloseParen
    f str = eitherShuffle $ readSKINoParensUnsafe str
    fixTrailing :: Int -> Bool -> [String] -> [String]
    fixTrailing _ _ [] = []
    fixTrailing n b (x:xs)
        | n<0 = [] -- Too many closing parentheses
        | x=="(" = x:fixTrailing (n+1) False xs
        | x==")" = x:fixTrailing (n-1) False xs
        | n==0 && not b = (splitOn (`elem` "()") (x>>=(\c -> ['(', c, ')'] ))) ++ fixTrailing n False xs
        | otherwise = x:fixTrailing n False xs

data BoolExpr = AND BoolExpr BoolExpr | OR BoolExpr BoolExpr | NOT BoolExpr | TrueLit | FalseLit | Var String

boolEncode :: BoolExpr -> Tree SKI
boolEncode TrueLit = fromJust $ readSKI "K"
boolEncode FalseLit = fromJust $ readSKI "(SK)"
boolEncode (NOT x) = Tree (boolEncode x) (fromJust $ readSKI "(SK)(K)")
boolEncode (OR x y) = Tree (Tree (boolEncode x) (fromJust $ readSKI "K")) (boolEncode y)
boolEncode (AND x y) = Tree (Tree (boolEncode x) (boolEncode y)) (fromJust $ readSKI "(SK)")
boolEncode (Var x) = Node (SKIVar x)

readMul :: String -> Maybe (Tree Integer)
readMul str = if all (`elem` "0123456789()") str then maybeFromSingleton $ readNonAssociative (splitOn (`elem` "()") str) f [] else Nothing where
    f :: String -> Either ParseError (Either Parenthesizer (Tree Integer))
    f "(" = Right $ Left OpenParen
    f ")" = Right $ Left CloseParen
    f str = if all (`elem` "0123456789") str then Right $ Right . Node $ read str else Left $ ParseError "All letters must be 0123456789"

boolTest :: IO ()
boolTest = do
    let hello = Var "Hello"
    print $ "hello = " ++ skiPrint (boolEncode hello)
    let trueExp = TrueLit
    print $ "TRUE = " ++ skiPrint (boolEncode trueExp)
    let alpha = Node $ SKIVar "alpha"
    let beta = Node $ SKIVar "beta"
    let temp = Tree (Tree (boolEncode trueExp) alpha) beta in print $ "TRUE alpha beta = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp) ++ " should be alpha"
    let falseExp = FalseLit
    print $ "FALSE = " ++ skiPrint (boolEncode falseExp)
    let temp = Tree (Tree (boolEncode falseExp) alpha) beta in print $ "FALSE alpha beta = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp) ++ " should be beta"
    let temp = boolEncode (NOT trueExp) in print $ "NOT(TRUE) = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = Tree (Tree (boolEncode (NOT trueExp)) alpha) beta in print $ "NOT(TRUE) alpha beta = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp) ++ " should be beta"
    let temp = boolEncode (NOT falseExp) in print $ "NOT(FALSE) = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (NOT hello) in print $ "NOT({hello}) = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)

    let temp = boolEncode (AND trueExp trueExp) in print $ "AND TRUE TRUE =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (AND trueExp falseExp) in print $ "AND TRUE FALSE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (AND falseExp trueExp) in print $ "AND FALSE TRUE =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (AND falseExp falseExp) in print $ "AND FALSE FALSE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)

    let temp = boolEncode (AND trueExp hello) in print $ "AND TRUE {hello} =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (AND hello trueExp) in print $ "AND {hello} TRUE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (AND falseExp hello) in print $ "AND FALSE {hello} =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (AND hello falseExp) in print $ "AND {hello} FALSE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)

    let temp = boolEncode (OR trueExp trueExp) in print $ "OR TRUE TRUE =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (OR trueExp falseExp) in print $ "OR TRUE FALSE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (OR falseExp trueExp) in print $ "OR FALSE TRUE =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (OR falseExp falseExp) in print $ "OR FALSE FALSE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)

    let temp = boolEncode (OR trueExp hello) in print $ "OR TRUE {hello} =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (OR hello trueExp) in print $ "OR {hello} TRUE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (OR falseExp hello) in print $ "OR FALSE {hello} =  " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)
    let temp = boolEncode (OR hello falseExp) in print $ "OR {hello} FALSE = " ++ skiPrint temp ++ " -> " ++ (skiPrint . snd . evaluate $ temp)