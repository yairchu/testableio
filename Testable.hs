{-# LANGUAGE GADTs, RankNTypes #-}

module Testable
    ( Eq1(..), GadtMatch(..)
    , Process(..), Trace(..), OfA(..)
    , runProcess
    , traceRunProcess
    , testVsTrace, testVsTraceD
    ) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.DList (DList)
import qualified Data.DList as DList

class GadtMatch f where
    gadtMatch :: g a -> f a -> f b -> Maybe (g b)

class Eq1 f where
    eq1 :: f a -> f a -> Bool

data Process u a where
    Do :: u a -> Process u a
    Return :: a -> Process u a
    Bind :: Process u b -> (b -> Process u a) -> Process u a

instance Monad (Process u) where
    return = Return
    (>>=) = Bind

data OfA f where
    OfA :: f a -> OfA f

instance (GadtMatch f, Eq1 f) => Eq (OfA f) where
    OfA x == OfA y =
        case gadtMatch x x y of
        Just nx -> eq1 nx y
        _ -> False

data Trace u where
    Trace :: u a -> a -> Trace u

runProcess :: Monad m => (forall r. u r -> m r) -> Process u a -> m a
runProcess f (Do x) = f x
runProcess _ (Return r) = return r
runProcess f (Bind x y) = runProcess f x >>= runProcess f . y

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

getAction :: Trace u -> OfA u
getAction (Trace x _) = OfA x

testVsTrace :: (Eq a, Eq1 u, GadtMatch u) => ([Trace u], a) -> Process u a -> Bool
testVsTrace (trace, result) process =
    case testVsTraceD trace process of
    (_, Nothing) -> False
    (newTrace, Just newResult) ->
        newResult == result && newTrace == map getAction trace

data TraceOf u a = TraceOf (u a) a

testVsTraceD :: (Eq1 u, GadtMatch u) => [Trace u] -> Process u a -> ([OfA u], Maybe a)
testVsTraceD _ (Return r) = ([], Just r)
testVsTraceD ts (Bind x y) =
    case mx of
    Nothing -> (tx, Nothing)
    Just rx -> first (tx ++) . testVsTraceD (drop (length tx) ts) $ y rx
    where
        (tx, mx) = testVsTraceD ts x
testVsTraceD [] (Do x) = ([OfA x], Nothing)
testVsTraceD (Trace action r:_) (Do x) =
    (,) [OfA x] $
    case gadtMatch (TraceOf action r) action x of
    Just (TraceOf ax rx) | eq1 ax x -> Just rx
    _ -> Nothing
