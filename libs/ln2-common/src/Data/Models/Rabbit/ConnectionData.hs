module Data.Models.Rabbit.ConnectionData
  ( RabbitConnectionData(..)
  ) where

data RabbitConnectionData = RConData
  { getRConUser :: !String
  , getRConPass :: !String
  , getRConHost :: !String
  , getRConPort :: !Int
  } deriving (Show, Eq)
