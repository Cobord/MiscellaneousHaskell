{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Monad (forM_, replicateM, join)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Bifunctor (first, second)
import Data.Either (isLeft, isRight)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (intersperse, sort)
import Data.Ord (Ordering (..))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M (Map, empty, lookup, insert)
import qualified Data.Set as S (Set (..))
import Data.Text (Text, pack, replace)
import Data.Tuple.Extra (swap, (&&&))
import Data.Void (Void)
import System.IO
import Text.Megaparsec (Parsec, between, choice, eof, many, parseTest, runParser, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

maybeToLeft _ (Just x) = Left x
maybeToLeft def Nothing = Right def

maybeToRight _ (Just x) = Right x
maybeToRight def Nothing = Left def

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

data AOrAList a = AOrAList {myelem :: Either a (Bool, [AOrAList a])}

instance Functor AOrAList where
  fmap f AOrAList {myelem = Left x} = AOrAList {myelem = Left $ f x}
  fmap f AOrAList {myelem = Right (y, x)} = AOrAList {myelem = Right (y, (f <$>) <$> x)}

instance Show a => Show (AOrAList a) where
  show AOrAList {myelem = Left x} = show x
  show AOrAList {myelem = Right (False, xs)} = "(" ++ mconcat (intersperse "," (show <$> xs)) ++ ")"
  show AOrAList {myelem = Right (True, xs)} = "'(" ++ mconcat (intersperse "," (show <$> xs)) ++ ")"

instance Eq a => Eq (AOrAList a) where
  AOrAList {myelem = Left x} == AOrAList {myelem = Left y} = x == y
  AOrAList {myelem = Right x} == AOrAList {myelem = Right y} = x == y
  _ == _ = False

fromA :: a -> AOrAList a
fromA x = AOrAList {myelem = Left x}

fromAs :: [a] -> AOrAList a
fromAs xs = AOrAList {myelem = second (False,) . Right $ map fromA xs}

collapseList :: Bool -> [AOrAList a] -> AOrAList a
collapseList quote z = AOrAList {myelem = Right (quote, z)}

wrapList :: AOrAList a -> AOrAList a
wrapList z = AOrAList {myelem = Right (False, [z])}

compareHelper :: (Ord a) => [a] -> [a] -> Ordering
compareHelper [] [] = EQ
compareHelper _ [] = GT
compareHelper [] _ = LT
compareHelper (x : xs) (y : ys) = if x == y then compareHelper xs ys else compare x y

instance (Eq a, Ord a) => (Ord (AOrAList a)) where
  compare AOrAList {myelem = Left x} AOrAList {myelem = Left y} = compare x y
  compare AOrAList {myelem = Right (_, x)} AOrAList {myelem = Right (_, y)} = compareHelper x y
  compare AOrAList {myelem = Left x} AOrAList {myelem = Right (z, y)} = compare (fromAs [x]) AOrAList {myelem = Right (z, y)}
  compare AOrAList {myelem = Right (z, x)} AOrAList {myelem = Left y} = compare AOrAList {myelem = Right (z, x)} (fromAs [y])

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")
    <?> "space consumer"

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pNum :: (Integral a) => Parser a
pNum = lexeme L.decimal <?> "number"

pString :: Parser String
pString = lexeme ((:) <$> letterChar <*> many (choice [alphaNumChar, char '_']) <?> "variable")

pOp :: Parser Text
pOp = choice [string $ pack $ show z | z <- [minBound :: ArithmeticOperations .. maxBound :: ArithmeticOperations]] <?> "arithmetic operation"

brackets :: Parser a -> Parser a
brackets = between (symbol "(") (symbol ")")

quotedBrackets :: Parser a -> Parser a
quotedBrackets = between (symbol "'(") (symbol ")")

data ArithmeticOperations = PLUS | TIMES | MINUS | DIV deriving (Enum, Bounded, Eq, Ord)

instance Show ArithmeticOperations where
  show PLUS = "+"
  show MINUS = "-"
  show DIV = "/"
  show TIMES = "*"

toSnd f x = (x, f x)

parseShown :: (Bounded a, Enum a, Show a) => Text -> Either a Text
parseShown z =
  let asShown = lookup z (swap . toSnd (pack . show) <$> [minBound ..])
   in maybeToLeft z asShown

parseArith :: Text -> Either ArithmeticOperations Text
parseArith = parseShown


data LISPyToken = IntegerToken Integer | ArithmeticToken ArithmeticOperations | TextToken Text deriving (Eq)

instance Show LISPyToken where
  show (IntegerToken i) = show i
  show (ArithmeticToken i) = show i
  show (TextToken t) = show t

fromNum :: (Integral a) => a -> LISPyToken
fromNum z = IntegerToken (toInteger z)

toNumH :: LISPyToken -> Maybe Integer
toNumH (IntegerToken z) = Just z
toNumH _ = Nothing

toNum :: AOrAList LISPyToken -> Maybe Integer
toNum AOrAList {myelem = Left z} = toNumH z
toNum _ = Nothing

fromOp :: ArithmeticOperations -> LISPyToken
fromOp = ArithmeticToken

fromString :: String -> LISPyToken
fromString z = either ArithmeticToken TextToken partial
  where partial = parseArith $ pack z :: Either ArithmeticOperations Text


fromText :: Text -> LISPyToken
fromText z = either ArithmeticToken TextToken partial
  where partial = parseArith z :: Either ArithmeticOperations Text

instance Ord LISPyToken where
  compare (IntegerToken x) (IntegerToken y) = compare x y
  compare (TextToken x) (TextToken y) = compare x y
  compare (TextToken _) (ArithmeticToken _) = GT
  compare (ArithmeticToken _) (TextToken _) = LT
  compare (ArithmeticToken x) (ArithmeticToken y) = compare x y
  compare (IntegerToken _) (ArithmeticToken _) = LT
  compare (IntegerToken _) (TextToken _) = LT
  compare (ArithmeticToken _) (IntegerToken _) = GT
  compare (TextToken _) (IntegerToken _) = GT

tryOp :: ([Integer] -> Maybe Integer) -> [AOrAList LISPyToken] -> Maybe (AOrAList LISPyToken)
tryOp f args = primitiveArgs >>= fmap (fromA . fromNum) . f
  where
    primitiveArgs = mapM toNum args

data Context = Context (M.Map Text ([AOrAList LISPyToken] -> Maybe (AOrAList LISPyToken)))

emptyContext :: Context
emptyContext = Context M.empty

testingContext :: Context
testingContext = Context $ M.insert (pack "plus") (tryOp (Just .sum)) M.empty

evaluate :: Context -> AOrAList LISPyToken -> AOrAList LISPyToken
evaluate _ AOrAList {myelem = Left y} = AOrAList {myelem = Left y}
evaluate _ AOrAList {myelem = Right (True, y)} = AOrAList {myelem = Right (True, y)}
evaluate (Context ctx) AOrAList {myelem = Right (False, xs)} = case xs of
  [] -> AOrAList {myelem = Right (False, [])}
  AOrAList {myelem = Left z} : rest -> go z
    where
      go z
        | z == fromOp PLUS = orElse (tryOp (Just . sum) rest) unchanged
        | z == fromOp TIMES = orElse (tryOp (Just . product) rest) unchanged
        | z == fromOp MINUS = orElse (tryOp (binaryTry (-)) rest) (orElse (tryOp (unaryTry (\z -> -z)) rest) unchanged)
        | z == fromOp DIV = orElse (tryOp dumbDiv rest) unchanged
        | otherwise = helper rest z
      helper rest (TextToken w) = let func = M.lookup w ctx in orElse (join (func <*> Just rest)) unchanged
      helper _ (IntegerToken _) = unchanged
      helper _ (ArithmeticToken _) = unchanged
      binaryTry op [a, b] = Just $ op a b
      binaryTry op _ = Nothing
      unaryTry op [a] = Just $ op a
      unaryTry op _ = Nothing
      dumbDiv [a, b] = if rem a b == 0 then Just $ quot a b else Nothing
      dumbDiv _ = Nothing
      unchanged = AOrAList {myelem = Right (False, xs)}
  _ -> AOrAList {myelem = Right (False, xs)}

pTerm :: Parser (AOrAList LISPyToken)
pTerm =
  choice
    [ (\_ -> fromAs [] :: AOrAList LISPyToken) <$> string "()" <?> "empty bracket",
      fmap IntegerToken . fromA <$> pNum <?> "number",
      fromA . fromString <$> pString <?> "variable",
      fromA . fromText <$> pOp <?> "arithmetic operation",
      (evaluate emptyContext . collapseList False <$> brackets pExpr) <?> "multiple pieces in brackets together",
      (collapseList True <$> quotedBrackets pExpr) <?> "multiple pieces in brackets together"
    ]

pExpr :: Parser [AOrAList LISPyToken]
pExpr = makeExprParser (return <$> pTerm) operatorTable

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

operatorTable :: [[Operator Parser [AOrAList a]]]
operatorTable =
  [ [ binary "," (++)
    ]
  ]

type ErrorMessage = String

parseToAOrAList :: Text -> Either ErrorMessage (AOrAList LISPyToken)
parseToAOrAList = first errorBundlePretty . runParser (pTerm <* eof) mempty

preProcessor :: Text -> Text
preProcessor = replace "(," "(" . replace ",)" ")" . repeated (replace ",," ",") . replace " " ","

repeated :: (Eq a) => (a -> a) -> a -> a
repeated replacer item = let oneRound = replacer item in if oneRound == item then item else repeated replacer oneRound

main :: IO ()
main = do
  let my_data_filename = "lispy_test.txt"
  dataFile <- openFile my_data_filename ReadMode
  error_counter <- newIORef (0 :: Integer)
  num_lines <- length . lines <$> readFile my_data_filename
  all_seen <- newIORef ([] :: [Either ErrorMessage (AOrAList LISPyToken)])
  forM_
    [0 .. (num_lines -1)]
    ( \i -> do
        left_signal <- hGetLine dataFile
        let left_signal_parsed = parseToAOrAList (preProcessor $ pack left_signal)
        let errored = if isLeft left_signal_parsed then 1 else 0
        modifyIORef error_counter (+ errored)
        modifyIORef all_seen (left_signal_parsed :)
        return ()
    )
  print "Number of errored s-expressions"
  error_count_final <- readIORef error_counter
  print error_count_final
  all_seen_final <- reverse <$> readIORef all_seen
  forM_
    all_seen_final
    ( \signal -> do
        print signal
        return ()
    )
  hClose dataFile
  putStrLn "done!"