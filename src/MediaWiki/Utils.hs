module MediaWiki.Utils 
( Url
, getPageContent
, getBinaryContent
) where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Base (defaultGETRequest_, Request)
import Network.HTTP.Headers (HeaderName(..), findHeader, replaceHeader)
import Network.URI (parseAbsoluteURI)
import qualified Data.ByteString as B

type Url = String

customGETRequest url = replaceHeader HdrUserAgent custom_user_agent default_request
    where uri = case parseAbsoluteURI url of
            Just uri' -> uri'
            Nothing -> error ("Wrong url " ++ show url)
          default_request = defaultGETRequest_ uri
          Just default_user_agent = findHeader HdrUserAgent default_request
          custom_user_agent = "mediawiki-dl/0.0 (https://github.com/jaimeMF/mediawiki-dl) " ++ default_user_agent 

getPageContent :: Url -> IO String
getPageContent url = simpleHTTP (customGETRequest url) >>= getResponseBody

getBinaryContent :: Url -> IO B.ByteString 
getBinaryContent url = do
    simpleHTTP (customGETRequest url) >>= getResponseBody
