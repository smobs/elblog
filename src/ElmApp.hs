{-# LANGUAGE OverloadedStrings #-}
module ElmApp where

import           Prelude                     hiding (head)
import           Servant.HTML.Blaze
import           Text.Blaze
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

data ElmApp = ElmApp FilePath

instance ToMarkup ElmApp where
    toMarkup  = elmPage



elmPage :: ElmApp -> Html
elmPage e = do
        head $ do
             elmImport e
        body $ do
             h1 "Unable to mount elm module. Please check console output"
             elmMount

elmImport :: ElmApp -> Html
elmImport  (ElmApp l) = script "" ! src  (stringValue $ l ++ "/elm.js" ) ! type_ "text/javascript"


elmMount :: Html
elmMount = script "Elm.fullscreen(Elm.Main)" ! type_ "text/javascript"
