{-# LANGUAGE FlexibleInstances, GADTs #-}

import Data.Char (toLower)
import Testable

data Action a where
    PutStrLn :: String -> Action ()
    GetLine :: Action String

ioFromAction :: Action a -> IO a
ioFromAction (PutStrLn line) = putStrLn line
ioFromAction GetLine = getLine

program :: Process Action ()
program = do
    Do $ PutStrLn "Hello there!"
    Do $ PutStrLn "Please tell me your name:"
    name <- Do GetLine
    let lowered = map toLower name
    if lowered == reverse lowered
        then Do $ PutStrLn "What a nice name!"
        else Do . PutStrLn $ "Ok, " ++ name ++ " is an ok name."

-- > runProcess ioFromAction program
-- Hello there!
-- Please tell me your name:
-- Bob
-- What a nice name!

instance Show (Trace Action) where
    show (Trace (PutStrLn l) ()) = "PutStrLn " ++ show l
    show (Trace GetLine l) = show l ++ " <- GetLine"

-- > t <- traceRunProcess ioFromAction program
-- Hello there!
-- Please tell me your name:
-- Yoda
-- Ok, Yoda is an ok name.
-- > t
-- ([PutStrLn "Hello there!"
--  ,PutStrLn "Please tell me your name:"
--  ,"Yoda" <- GetLine
--  ,PutStrLn "Ok, Yoda is an ok name."
--  ],())

instance GadtMatch Action where
    gadtMatch r (PutStrLn _) (PutStrLn _) = Just r
    gadtMatch r GetLine GetLine = Just r
    gadtMatch _ _ _ = Nothing

instance Eq (Action a) where
    PutStrLn x == PutStrLn y = x == y
    GetLine == GetLine = True
    _ == _ = False

instance Eq1 Action where
    eq1 = (==)

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

instance Show (OfA Action) where
    show (OfA (PutStrLn l)) = "PutStrLn " ++ show l
    show (OfA GetLine) = "GetLine"

-- > testVsTraceD (fst t) badProgram
-- ([PutStrLn "Hello there!"
--  ,PutStrLn "Please tell me your name:"
--  ,GetLine
--  ,PutStrLn "What a nice name!"
--  ],Nothing)
