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
             elmCss
             elmImport e
        body $ do
             elmMount

elmImport :: ElmApp -> Html
elmImport  (ElmApp l) = script "" ! src  (stringValue $ l ++ "/Main.js" ) ! type_ "text/javascript"

elmCss :: Html
elmCss = link ! rel "stylesheet" ! href "static/css/pure.css"

elmMount :: Html
elmMount = script "Elm.fullscreen(Elm.Main)" ! type_ "text/javascript"
