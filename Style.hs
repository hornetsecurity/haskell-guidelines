{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{- |
Module: Style.Haskell.Example

Haskell Code Style Example.
-}
module Style.Haskell.Example
    ( -- * Types
      Enum(..)
    , Either(..)
    , Point(..)
      -- * Functions
    , hello
    ) where

-- Module imports
import           Prelude                   hiding ( map )

import qualified Control.Monad.Trans.State ( State, evalState, execState, get
                                           , modify, put, runState )

import qualified Data.Map                  as Map
import qualified Data.Text                 as Text

-- Data declarations
data Enum = CaseA | CaseB | CaseC
    deriving (Eq, Enum, Show)

data Either a b = Left a
                | Right b
    deriving (Eq, Show)

data Point = Point { pointX     :: Float
                   , pointY     :: Float
                   , pointLabel :: String
                   }
    deriving (Eq, Show)

-- Type classes
class Functor f => Applicative a where
    pure :: b -> a b
    ap :: a (b -> c) -> a b -> a c

class Fundep a b | a -> b where
    convert :: a -> b

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap x) = Wrap $ fmap f x

-- Values
origin :: Point
origin = Point { pointX = 0, pointY = 0, pointLabel = "Origin" }

lorem :: [String]
lorem = [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        , "Curabitur nec ante nec mauris ornare suscipit."
        , "In ac vulputate libero."
        , "Duis eget magna non purus imperdiet molestie nec quis mauris."
        , "Praesent blandit quam vel arcu pellentesque, id aliquet turpis faucibus."
        ]

-- Functions
facs :: [Int]
facs = [ 1, 1 ] ++ zipWith (+) (tail facs)

hello :: MonadIO m => m ()
hello = do
    name <- liftIO getLine
    liftIO . putStrLn $ greetings name
  where
    greetings n = "Hello " ++ n ++ "!"

letExpr :: Point -> String
letExp x = let y = 1
               z = 2
           in
               if x > 0 then y else z

ifExpr :: Bool -> Bool
ifExpr b = if b == True  -- useless
           then False
           else True

caseExpr :: [a] -> Maybe a
caseExpr xs = case xs of
    [] -> Nothing
    (x : _) -> Just x

guarded :: Int -> Int
guarded x
    | x == 0 = 1
    | x == 1 = 1
    | otherwise = guarded (x - 2) + guarded (x - 1)

someLongFunctionNameWithALotOfParameters :: (MonadIO m, MonadRandom m)
                                         => String
                                         -> (String -> String)
                                         -> m ()
someLongFunctionNameWithALotOfParameters = undefined
