module Main where

import Control.Applicative
import Data.ByteString as B (readFile)
import System.Environment

import Data.Attoparsec.ByteString

main :: IO ()
main = do
  args <- getArgs
  case args of
      (x:xs) -> B.readFile x >>= print . parseOnly xbase
      _      -> error "Please supply a .dbf file as the first arg."

data DBF = DBF { version :: Integer } deriving (Show)

xbase :: Parser DBF
xbase = DBF . toInteger <$> anyWord8
