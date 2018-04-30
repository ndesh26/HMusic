{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Uploads where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import System.Directory

data Dummy = Dummy
    { dummy :: Text
    }

getUploadsR :: UserId -> Handler Html
getUploadsR userId = do 
                        (widget, enctype) <- generateFormPost entryForm 
                        (thisId, _) <- requireAuthPair
                        storedFiles <- runDB $ getUserUploads userId
                        user <- getUser userId
                        defaultLayout $ do
                            setTitle "HMusic"
                            $(widgetFile "uploads")

postUploadsR :: UserId -> Handler Html
postUploadsR userId = do 
    ((result, widget), enctype) <- runFormPost entryForm
    (thisId, thisUser) <- requireAuthPair
    user <- getUser userId
    submission <- case result of
        {-FormSuccess res -> makeNewEntry thisId userId-}
        FormSuccess res -> runDB $ insertEntity followRelation where followRelation = Followers {followersFollowBy = thisId, followersFollowed = userId}
        {-_ -> runDB $ getAllRelations -}

    storedFiles <- runDB $ getUserUploads userId
    defaultLayout $ do
                    setTitle "HMusic"
                    $(widgetFile "followed")

{-makeNewEntry followA followB = do-}
    {-let followRelation = Followers { followersFollowBy = followA, followersFollowed = followB}-}
    {-runDB $ insertEntity followRelation-}

getUser userId = runDB $ getThisUser userId

entryForm :: Form Dummy
entryForm = renderBootstrap3 BootstrapBasicForm $ Dummy
    <$> areq textField "dummy " Nothing 
