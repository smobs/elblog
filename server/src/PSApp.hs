{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric         #-}
module PSApp where

import           Prelude                     hiding (head)
import           Servant.HTML.Blaze
import           Text.Blaze
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import           GHC.Generics                       (Generic)

data PSApp = PSApp FilePath deriving (Generic, Show, Eq, Ord)

instance ToMarkup PSApp where
    toMarkup  = psPage

psPage :: PSApp -> Html
psPage e = do
  docType
  head $ do
    psCss
  body $ do
      psImport e

psImport :: PSApp -> Html
psImport  (PSApp l) = script "" ! src  (stringValue $ l ++ "/bundle.js" ) ! type_ "text/javascript"

psCss :: Html
psCss = link ! rel "stylesheet" ! href "static/css/pure.css"
