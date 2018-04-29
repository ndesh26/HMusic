{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.UserSearch where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data UserQuery = UserQuery
    { userName :: Text
    }

postUserSearchR :: Handler Html
postUserSearchR = do 
    ((userSearch, userWidget), userEnctype) <- runFormPost userForm

    (userId, user) <- requireAuthPair
    let handlerName = "postProfileR" :: Text
        userSearchStr = case userSearch of
            FormSuccess userQ -> Just userQ
            _ -> Nothing

    userSearchRes <- getUserMatches userSearchStr
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "userSearch")

getUserMatches userQuery = do
        let Just (UserQuery username) = userQuery
        runDB $ getUserSearch username

userForm :: Form UserQuery
userForm = renderBootstrap3 BootstrapBasicForm $ UserQuery
    <$> areq textField "User Name: " Nothing


