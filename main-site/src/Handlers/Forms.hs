{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Handlers.Forms (taskResponseForm) where

import           Data.Text
import           Foundation
import           Yesod.Core
import           Yesod.Form

taskResponseForm :: Html -> MForm Handler (FormResult Textarea, Widget)
taskResponseForm = renderDivs $ areq textareaField "Ответ" Nothing
