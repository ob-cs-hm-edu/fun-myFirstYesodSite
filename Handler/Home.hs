{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost simplestForm
    let submission = Nothing :: Maybe (Text, Text)
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To SimplestNotes!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost simplestForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To SimplestNotes!"
        case submission of
            Just (user,_) -> [whamlet|<h1>Hallo #{user}|]
            _ -> $(widgetFile "homepage")

simplestForm :: Form (Text, Text)
simplestForm = renderDivs $ (,)
    <$> areq textField "Username:" Nothing
    <*> areq textField "Password" Nothing
