{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module YesodApp (yesodApp) where

import Yesod

data App = App

mkYesod "App" [parseRoutes|
/yesod YesodR GET
|]

instance Yesod App

getYesodR :: HandlerFor App Html
getYesodR = defaultLayout [whamlet|Hello from Yesod!|]

yesodApp :: IO ()
yesodApp = warp 3002 App