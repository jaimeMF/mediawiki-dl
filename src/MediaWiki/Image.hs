module MediaWiki.Image
( Image(..)
, getImagesFrom
) where

import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Network.HTTP (urlEncodeVars)
import Text.JSON

import MediaWiki.Utils
import MediaWiki.Wiki

data Image = Image {name :: String, url :: Url} deriving (Show, Read)

imagesApiQuery imageNames = urlEncodeVars queryList
    where titles = intercalate "|" imageNames
          queryList = [("format", "json"),
                       ("action", "query"),
                       ("prop", "imageinfo"),
                       ("iiprop", "url"),
                       ("titles", titles)]

imagesApiUrl imageNames wiki = (baseWikiApiURL wiki) ++ (imagesApiQuery imageNames)

extractJSString (JSString jsstr) = fromJSString jsstr

imagesFromJson (Ok json) = [Image title $ extractJSString $ fromJust $ lookup "url" info | (title, info) <- final_zip]
    where (Ok pages) = valFromObj "query" json >>= valFromObj "pages" :: Result (JSObject JSValue)
          images = [fromJSObject obj | (_, JSObject  obj) <- fromJSObject pages]
          titles = [extractJSString $ fromJust $ lookup "title" im| im <- images]
          maybe_imageinfos = [lookup "imageinfo" obj | obj <- images]
          final_zip = [let (JSArray a) = fromJust iminfo
                           (JSObject b) = head a in 
                       (title, fromJSObject b) | (title, iminfo) <- zip titles maybe_imageinfos, isJust iminfo]

getImagesFrom :: [String] -> Wiki -> IO [Image]
getImagesFrom [] _ = return []
getImagesFrom imagesNames wiki = fmap (imagesFromJson . decode) $ getPageContent $ imagesApiUrl imagesNames wiki
