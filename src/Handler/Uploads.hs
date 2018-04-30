{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Uploads where

import Import

getUploadsR :: UserId -> Handler Html
getUploadsR userId = do 
                        storedFiles <- runDB $ getUserUploads userId
                        (userId, user) <- requireAuthPair
                        defaultLayout $ do
                            setTitle "Welcome To Yesod!"
                            $(widgetFile "uploads")
