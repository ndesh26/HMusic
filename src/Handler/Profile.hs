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

getProfileR :: Handler Html
getProfileR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getProfileR" :: Text

    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postProfileR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    (userId, user) <- requireAuthPair
    _ <- writeF submission userId user
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")

writeF subm userId user = do
        let Just (FileForm info _) = subm
            thisFileName = fileName info
            thisPathName = concat ["Users/",(userIdent user),"/",thisFileName]
            thisContentType = fileContentType info
        let storedFile' = StoredFile { storedFileName = thisFileName,
        storedFileContentType = thisContentType, storedFilePath =  thisPathName, storedFileUserId = userId }

        _ <- liftIO $ createDirectoryIfMissing True (unpack (concat ["Users/", (userIdent user)]))
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
