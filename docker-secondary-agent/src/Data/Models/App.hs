module Data.Models.App (App(..)) where

import           Network.AMQP

data App = App
  { rabbitConnection :: Connection
  }
