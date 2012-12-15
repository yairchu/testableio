{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}

import Control.Arrow (first)
import Control.Monad (guard, liftM)
import Data.Char (toLower)
import Data.DList (DList)
import qualified Data.DList as DList

data Process u a where
    Do :: u a -> Process u a
    Return :: a -> Process u a
    Bind :: Process u b -> (b -> Process u a) -> Process u a

instance Monad (Process u) where
    return = Return
    (>>=) = Bind


data Action a where
    PutStrLn :: String -> Action ()
    GetLine :: Action String

program :: Process Action ()
program = do
    Do $ PutStrLn "Hello there!"
    Do $ PutStrLn "Please tell me your name:"
    name <- Do GetLine
    let lowered = map toLower name
    if lowered == reverse lowered
        then Do $ PutStrLn "What a nice name!"
        else Do . PutStrLn $ "Ok, " ++ name ++ " is an ok name."


ioFromAction :: Action a -> IO a
ioFromAction (PutStrLn line) = putStrLn line
ioFromAction GetLine = getLine

runProcess :: Monad m => (forall r. u r -> m r) -> Process u a -> m a
runProcess f (Do x) = f x
runProcess _ (Return r) = return r
runProcess f (Bind x y) = runProcess f x >>= runProcess f . y

-- > runProcess ioFromAction program
-- Hello there!
-- Please tell me your name:
-- Yoda
-- Ok, Yoda is an ok name.


data Trace u where
    Trace :: u a -> a -> Trace u

traceRunProcess :: Monad m => (forall r. u r -> m r) -> Process u a -> m ([Trace u], a)
traceRunProcess f =
    (liftM . first) DList.toList . traceRunProcessD f

traceRunProcessD :: Monad m => (forall r. u r -> m r) -> Process u a -> m (DList (Trace u), a)
traceRunProcessD _ (Return r) = return (DList.empty, r)
traceRunProcessD f (Do x) = do
    r <- f x
    return (DList.singleton (Trace x r), r)
traceRunProcessD f (Bind x y) = do
    (tx, rx) <- traceRunProcessD f x
    (ty, ry) <- traceRunProcessD f $ y rx
    return (DList.append tx ty, ry)

instance Show (Trace Action) where
    show (Trace (PutStrLn l) ()) = "?: " ++ l
    show (Trace GetLine l) = "!: " ++ l

-- > t <- traceRunProcess ioFromAction program
-- Hello there!
-- Please tell me your name:
-- Yoda
-- Ok, Yoda is an ok name.
-- > t
-- ([?: Hello there!,?: Please tell me your name:,!: Yoda,?: Ok, Yoda is an ok name.],())
-- > putStr . unlines . map show $ fst t
-- ?: Hello there!
-- ?: Please tell me your name:
-- !: Yoda
-- ?: Ok, Yoda is an ok name.


class GadtMatch f where
    gadtMatch :: g a -> f a -> f b -> Maybe (g b)

instance GadtMatch Action where
    gadtMatch r (PutStrLn _) (PutStrLn _) = Just r
    gadtMatch r GetLine GetLine = Just r
    gadtMatch _ _ _ = Nothing

instance Eq (Action a) where
    PutStrLn x == PutStrLn y = x == y
    GetLine == GetLine = True
    _ == _ = False

class Eq1 f where
    eq1 :: f a -> f a -> Bool

instance Eq1 Action where
    eq1 = (==)

data TraceAction u where
    TraceAction :: u a -> TraceAction u

instance (GadtMatch u, Eq1 u) => Eq (TraceAction u) where
    TraceAction x == TraceAction y =
        case gadtMatch x x y of
        Just nx -> eq1 nx y
        _ -> False

getAction :: Trace u -> TraceAction u
getAction (Trace x _) = TraceAction x

data TraceOf u a = TraceOf (u a) a

testVsTrace :: (Eq a, Eq1 u, GadtMatch u) => ([Trace u], a) -> Process u a -> Bool
testVsTrace (trace, result) process =
    case testVsTraceD trace process of
    (_, Nothing) -> False
    (newTrace, Just newResult) ->
        newResult == result && newTrace == map getAction trace

testVsTraceD :: (Eq1 u, GadtMatch u) => [Trace u] -> Process u a -> ([TraceAction u], Maybe a)
testVsTraceD _ (Return r) = ([], Just r)
testVsTraceD ts (Bind x y) =
    case mx of
    Nothing -> (tx, Nothing)
    Just rx -> first (tx ++) . testVsTraceD (drop (length tx) ts) $ y rx
    where
        (tx, mx) = testVsTraceD ts x
testVsTraceD [] (Do x) = ([TraceAction x], Nothing)
testVsTraceD (Trace action r:_) (Do x) =
    (,) [TraceAction x] $
    case gadtMatch (TraceOf action r) action x of
    Just (TraceOf ax rx) | eq1 ax x -> Just rx
    _ -> Nothing

badProgram :: Process Action ()
badProgram = do
    Do $ PutStrLn "Hello there!"
    Do $ PutStrLn "Please tell me your name:"
    _ <- Do GetLine
    Do $ PutStrLn "What a nice name!"

-- > testVsTrace t program
-- True
-- > testVsTrace t badProgram
-- False

instance Show (TraceAction Action) where
    show (TraceAction (PutStrLn l)) = "?: " ++ l
    show (TraceAction GetLine) = "!"

-- > testVsTraceD t badProgram
-- ([?: Hello there!,?: Please tell me your name:,!,?: What a nice name!],Nothing)
