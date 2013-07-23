module MediaWiki.Article
( ArticleReference (..)
, Article(..)
, articleReferenceFromString
, getArticle
) where

import Network.HTTP (urlEncodeVars)
import Text.JSON
import Text.Pandoc
import Data.Text (splitOn, pack, unpack)

import MediaWiki.Wiki
import MediaWiki.Utils

data ArticleReference = ArticleReference { wiki :: Wiki, title :: String }
    deriving (Show, Read)

data Article = Article {reference :: ArticleReference, source :: Pandoc}

articleReferenceFromString :: String -> ArticleReference
articleReferenceFromString art_spec = ArticleReference wiki title
    where parts = reverse $ map unpack $ splitOn (pack ":") $ pack art_spec
          title:wiki_spec = parts
          wiki = wikimediaFromList wiki_spec


apiQuery title = urlEncodeVars [("format", "json"),
                                ("action", "query"),
                                ("prop", "revisions"),
                                ("rvprop", "content"),
                                ("titles", title)]

apiURL :: ArticleReference -> Url
apiURL article = (baseWikiURL $ wiki article) ++ "/w/api.php?" ++ (apiQuery $ title article)

articleSourceFromJSON :: Result (JSObject JSValue) -> String
articleSourceFromJSON (Ok json) = fromJSString content
    where (Ok pages) = valFromObj "query" json >>= valFromObj "pages" :: Result (JSObject JSValue)
          (revision, page) = head $ fromJSObject pages
          (JSObject page_obj) = page
          (Ok content) = fmap (head) (valFromObj "revisions" page_obj) >>= valFromObj "*"

getArticle article_ref = fmap ((\source -> Article article_ref source) .build_article) json_string
    where json_string = getPageContent $ apiURL article_ref
          build_article = (readMediaWiki def) . articleSourceFromJSON . decode
