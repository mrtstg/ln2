{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( unsafeRandomString
  , taskStatusToText
  ) where

import           Data.Text          (Text)
import           System.Environment
import           System.Random

taskStatusToText :: String -> Text
taskStatusToText "queued"     = "В очереди на выполнение"
taskStatusToText "taken"      = "Взята на выполнение"
taskStatusToText "error"      = "Завершена с ошибкой"
taskStatusToText "processing" = "В обработке"
taskStatusToText "finished"   = "Завершена"
taskStatusToText "accepted"   = "Зачтена"
taskStatusToText "cancelled"  = "Проверка прервана"
taskStatusToText "timeout"    = "Тайм-аут проверки"
taskStatusToText _            = "Неизвестный статус"

unsafeRandomString :: StdGen -> Int -> String
unsafeRandomString rnd n = take n $ randomRs ('a', 'z') rnd
