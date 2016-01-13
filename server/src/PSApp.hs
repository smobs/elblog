{-# LANGUAGE OverloadedStrings #-}
module PSApp where

import           Prelude                     hiding (head)
import           Servant.HTML.Blaze
import           Text.Blaze
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

data PSApp = PSApp FilePath

instance ToMarkup PSApp where
    toMarkup  = psPage



psPage :: PSApp -> Html
psPage e = do
         head $ do
              elmCss
              elmImport e
         body $ do
              elmMount

elmImport :: PSApp -> Html
elmImport  (PSApp l) = script "" ! src  (stringValue $ l ++ "/bundle.js" ) ! type_ "text/javascript"

elmCss :: Html
elmCss = link ! rel "stylesheet" ! href "static/css/pure.css"

elmMount :: Html
elmMount = script "PS.Main.main()" ! type_ "text/javascript"