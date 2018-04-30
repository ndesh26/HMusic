{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import System.Directory

data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

data Song = Song
    { songName :: Text
    }

data UserQuery = UserQuery
    { userName :: Text
    }

getProfileR :: Handler Html
getProfileR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    (songWidget, enctype) <- generateFormPost songForm
    (userWidget, userEnctype) <- generateFormPost userForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getProfileR" :: Text

    (userId, user) <- requireAuthPair
    storedFiles <- runDB $ getUserUploads userId
    songSearchRes <- runDB $ getUserUploads userId
    users <- runDB $ getUserFollowing userId
    {-users <- runDB $ getAllUsers-}
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (songWidget, enctype) <- generateFormPost songForm
    (userWidget, userEnctype) <- generateFormPost userForm
    (userId, user) <- requireAuthPair
    let handlerName = "postProfileR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    storedFiles <- runDB $ getUserUploads userId
    users <- runDB $ getUserFollowing userId
    {-users <- runDB $ getAllUsers-}
    _ <- case result of
            FormSuccess res -> writeF submission userId user
            _ -> liftIO $ createDirectoryIfMissing False "static/"
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")

writeF subm userId user = do
        let Just (FileForm info descrip) = subm
            thisFileName = fileName info
            thisPathName = concat ["static/Users/",(userIdent user),"/",thisFileName]
            thisContentType = fileContentType info
        let storedFile' = StoredFile { storedFileName = thisFileName,
        storedFileContentType = thisContentType, storedFilePath =  thisPathName, storedFileUserId = userId, storedFileDescription = descrip }

        _ <- liftIO $ createDirectoryIfMissing True (unpack (concat ["static/Users/", (userIdent user)]))
        _ <- runDB $ insertEntity storedFile'
        liftIO $ fileMove info (unpack thisPathName)

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

songForm :: Form Song 
songForm = renderBootstrap3 BootstrapBasicForm $ Song
    <$> areq textField "Song Name: " Nothing

userForm :: Form UserQuery
userForm = renderBootstrap3 BootstrapBasicForm $ UserQuery
    <$> areq textField "User Name: " Nothing


