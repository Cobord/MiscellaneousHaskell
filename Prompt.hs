import Control.Monad (MonadPlus, liftM, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import Data.Bifunctor (first)

data Prompter a b = Prompter{
                    -- | initial prompt for input
                    startingPrompt::String,
                    -- | is the input valid
                    validator :: a -> Bool,
                    -- | if the input is invalid, this message is output to show why the input was invalid
                    retryMessage :: String,
                    -- | apply this function to the input
                    postProcessor :: a -> b}

prompt :: (Read a) => Prompter a b -> IO b
prompt Prompter{startingPrompt=x, validator=f, retryMessage=y, postProcessor=g} = do
    putStr x
    res <- runExceptT $ readValidateAndTryTransform readLn f (return . g) ("validator error","postProcessor error")
    case res of
        Left someError -> prompt Prompter{startingPrompt=y,validator=f,retryMessage=y,postProcessor=g}
        Right goodValue -> return goodValue

readValidateAndTryTransform :: (Read b) => IO String -> (b -> Bool) -> (b -> Maybe a) -> ([e], [e]) -> ExceptT [e] IO a
readValidateAndTryTransform strReader validator transformer (validatorFailed, transformerFailed) = do
  x <- read <$> liftIO strReader
  if validator x
    then case transformer x of
      Nothing -> withExceptT (++ transformerFailed) mzero
      Just z -> return z
    else withExceptT (++ validatorFailed) mzero

main :: IO ()
main = do
  res <- runExceptT (readValidateAndTryTransform getLine (== "5") (Just . const 5) ("wasn't '5'", "didn't transform correctly")) :: IO (Either [Char] Int)
  print res
  putStrLn "done"