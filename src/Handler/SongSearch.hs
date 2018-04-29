{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.SongSearch where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data Song = Song
    { songName :: Text
    }

postSongSearchR :: Handler Html
postSongSearchR = do
    ((songSearch, songWidget), enctype) <- runFormPost songForm

    (userId, user) <- requireAuthPair
    let handlerName = "postProfileR" :: Text
        songSearchStr = case songSearch of
            FormSuccess song -> Just song
            _ -> Nothing

    songSearchRes <- getMatches songSearchStr
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "songSearch")

getMatches songQuery = do
        let Just (Song songname) = songQuery
        runDB $ getAllUploads songname

songForm :: Form Song 
songForm = renderBootstrap3 BootstrapBasicForm $ Song
    <$> areq textField "Song Name: " Nothing
