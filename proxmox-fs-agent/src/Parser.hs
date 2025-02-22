{-# LANGUAGE OverloadedStrings #-}
module Parser (VMArgs(..), parseVNCArgs) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text

data VMArgs = VNCArgs String | OtherArgs String deriving Show

parseVNCArgs :: String -> Either String [VMArgs]
parseVNCArgs = parseOnly vncArgsParser . pack

vncArgsParser :: Parser [VMArgs]
vncArgsParser = optional (string "args:") *> many space *> manyTill argsParser endOfInput

argsParser :: Parser VMArgs
argsParser = choice [try f, f'] where
  f = do
    address <- many space *> string "-vnc" *> many1 space *> many1 (notChar ' ')
    return $ VNCArgs address

  f' :: Parser VMArgs
  f' = do
    opt <- many space *> many1 (notChar ' ')
    (return . OtherArgs) opt
