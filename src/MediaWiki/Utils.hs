module MediaWiki.Utils 
( Url
, getPageContent
, getBinaryContent
) where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Base (defaultGETRequest_)
import Network.URI (parseAbsoluteURI)
import qualified Data.ByteString as B

type Url = String

getPageContent :: Url -> IO String
getPageContent url = simpleHTTP (getRequest url) >>= getResponseBody

getBinaryContent :: Url -> IO B.ByteString 
getBinaryContent url = do
    let uri = case parseAbsoluteURI url of
            Just uri' -> uri'
            Nothing -> error ("Wrong url " ++ show url)
    simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody


