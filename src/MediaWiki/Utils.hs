module MediaWiki.Utils 
( Url
, getPageContent
) where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

type Url = String

getPageContent :: Url -> IO String
getPageContent url = simpleHTTP (getRequest url) >>= getResponseBody
