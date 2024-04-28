{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Index (getIndexR) where

import           Crud.Course
import           Crud.User
import qualified Data.Map         as M
import           Data.Models.User
import           Foundation
import           Handlers.Utils
import           Yesod.Core
import           Yesod.Persist

-- TODO: public courses page!
-- random courses?
getIndexR :: Handler Html
getIndexR = do
  UserDetails { .. } <- requireAuth
  (courses, _) <- getUserMembershipCourses getUserRoles 1
  let limitedCourses = take 4 courses
  courseAuthors <- liftIO $ retrieveCourseUsers courses
  let linkedCourses = linkUserAndCourses limitedCourses courseAuthors
  defaultLayout $ do
    (setTitle . toHtml) ("Профиль: " <> getUserDetailsName)
    [whamlet|
<div .container>
  <h1 .title.is-3> Привет, #{getUserDetailsName}!
  $if null courses
    <p .is-size-3>
      У вас пока нет курсов для прохождения.
      <a href=@{CoursesR}> Хотите поискать?
  $else
    <p .is-size-3> Ваши последние курсы:
    <div .columns.is-multiline>
      $forall ((Entity _ (Course { .. })), d) <- linkedCourses
        <div .column>
          <div .card>
            <header .card-header>
              <p .card-header-title> #{ courseName }
            $case d
              $of (Just (UserDetails { getUserDetailsName = authorName }))
                <div .card-content>
                  <div .content>
                    <p> Автор: #{ authorName }
              $of Nothing
                <div .card-content>
    <a .button.is-fullwidth href=@{CoursesR}> Все курсы
|]
