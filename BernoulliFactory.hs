import qualified Numeric.Probability.Distribution as Dist (fromFreqs,T,size)
import Control.Monad.State (State,runState,liftIO)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State (StateT,get,evalStateT,runStateT,state) -- from the "transformers" library
import System.Random (random, uniformR, newStdGen)

class BernoulliFactory a where
    makeBernoulli :: a -> IO Bool

data Constants = ZERO | ONE

instance BernoulliFactory Constants where
    makeBernoulli ZERO = return False
    makeBernoulli ONE = return True

newtype BernoulliRational = BernoulliRational Rational
rat :: BernoulliRational -> Rational
rat (BernoulliRational x) = if x>=0 && x<=1 then x else error "Invalid probabiilty"

instance BernoulliFactory BernoulliRational where
    makeBernoulli (BernoulliRational x) = do
        pureGen <- newStdGen
        let myRange = uniformR (0 :: Float, 1 :: Float)
        return $ ((\z -> z<fromRational x) . fst) (myRange pureGen)

instance (BernoulliFactory a,BernoulliFactory b) => BernoulliFactory (Either a b) where
    makeBernoulli (Left x) = makeBernoulli x
    makeBernoulli (Right x) = makeBernoulli x

data Nat = Z | S Nat deriving (Show)

data Expression a = Leaf a | -- lambda
                    Negate (Expression a) | -- 1-lambda
                    Average (Expression a) (Expression a) | -- (lambda+mu)/2
                    IndependentOr (Expression a) (Expression a) | --lambda+mu-lambda*mu
                    IndependentAnd (Expression a) (Expression a) | -- lambda*mu
                    Power (Expression a) Nat | -- lambda^n
                    FracPower (Expression a) Rational | -- lambda^(x/y)
                    GenPower (Expression a) (Expression a) | -- lambda^mu
                    Sqrt (Expression a) -- lambda^(1/2)


runRepeated :: Int -> State s a -> s -> [a]
runRepeated n stater init_state
    | n<=0 = []
    | otherwise = let (res,new_init_state) = runState stater init_state in
        res:runRepeated (n-1) stater new_init_state

firstJust :: [Maybe b] -> Maybe b
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x:_) = Just x

firstSuccess :: Int -> b -> State s (Maybe b) -> s -> b
firstSuccess upperBound ifFail stater init_state = fromMaybe ifFail $ firstJust $ runRepeated upperBound stater init_state

ensure :: (a -> Bool) -> a -> Maybe a
ensure pred val = if pred val then Just val else Nothing

instance BernoulliFactory a => BernoulliFactory (Expression a) where
    makeBernoulli (Leaf x) = makeBernoulli x
    makeBernoulli (Negate x) = not <$> makeBernoulli x
    makeBernoulli (Average x y) = do
                                    f <- makeBernoulli (BernoulliRational $ 1/2)
                                    if f then makeBernoulli x else makeBernoulli y
    makeBernoulli (IndependentOr x y) = do
                                    xr <- makeBernoulli x
                                    if xr then return True else makeBernoulli y
    makeBernoulli (IndependentAnd x y) = do
                                    xr <- makeBernoulli x
                                    if not xr then return False else makeBernoulli y
    makeBernoulli (Power x Z) = makeBernoulli ONE
    makeBernoulli (Power x (S Z)) = makeBernoulli x
    makeBernoulli (Power x (S y)) = makeBernoulli (IndependentAnd x (Power x y))
    makeBernoulli (FracPower x z)
        | z==0 = makeBernoulli ONE
        | z==1 = makeBernoulli x
        | z>1 = undefined
        | z<0 = undefined
        | z<1 && z>0 = undefined
    makeBernoulli (GenPower x y) = undefined
    makeBernoulli (Sqrt x) = makeBernoulli (FracPower x (1/2))

loggingStater :: Show s => (s -> (a,s)) -> StateT s IO a

loggingStater runner = do
    --occuring in (StateT s IO) monad
  cur_state <- get
    --cur_state treated as in s because get had type Monad m => StateT s m s where m has specialized to IO
  liftIO $ putStrLn $ "The state was "++show cur_state
    --liftIO :: MonadIO m => IO a -> m a
    -- the putStrLn ... is in IO () then the liftIO takes it to StateT s IO () so that it works within this do
  state runner
    -- the ordinary state with any monad where in particular it can be either Identity for State and IO for here

runRepeatedT :: (Monad m) => Int -> StateT s m a -> s -> m [a]
runRepeatedT n stater init_state
    | n<=0 = return []
    | otherwise = do
                    (res,new_init_state) <- runStateT stater init_state
                    remaining <- runRepeatedT (n-1) stater new_init_state
                    return $ res:remaining

x1 n = runRepeatedT n (loggingStater (\z -> (reverse z,z++"1"))) "hello"
x2 n = runRepeatedT n (loggingStater (\z -> (z+1,z*5))) 3