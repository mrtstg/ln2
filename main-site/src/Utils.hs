{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( taskStatusToText
  ) where

import           Data.Text (Text)

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
