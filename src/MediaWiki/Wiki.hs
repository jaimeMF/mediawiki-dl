module MediaWiki.Wiki
( Wiki (..)
, wikipedia
, englishWikipedia
, baseWikiURL
, baseWikiApiURL
, wikimediaFromList
) where

data Wiki = Wikimedia { project :: String, lang :: String} | WikimediaCommons | WikiNamed String deriving (Show, Read)

wikipedia = Wikimedia "wikipedia"
wikibooks = Wikimedia "wikibooks"
wikisource = Wikimedia "wikisource"
englishWikipedia =  Wikimedia "wikipedia" "en"

baseWikiURL WikimediaCommons = "http://commons.wikimedia.org"
baseWikiURL (WikiNamed name) = "http://" ++ name
baseWikiURL wiki = "http://" ++ (lang wiki) ++ "." ++ (project wiki) ++ ".org"

baseWikiApiURL wiki = (baseWikiURL wiki) ++ "/w/api.php?"

wikimediaFromList [] = englishWikipedia
wikimediaFromList (lang:[]) = wikipedia lang
wikimediaFromList (lang:name:[]) = Wikimedia name lang
