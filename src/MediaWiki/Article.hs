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
import MediaWiki.Image

data ArticleReference = ArticleReference { wiki :: Wiki, title :: String }
    deriving (Show, Read)

data Article = Article {reference :: ArticleReference, source :: Pandoc, images :: [Image]} deriving Show

articleReferenceFromString :: String -> ArticleReference
articleReferenceFromString art_spec = ArticleReference wiki title
    where parts = reverse $ map unpack $ splitOn (pack ":") $ pack art_spec
          title:wiki_spec = parts
          wiki = wikimediaFromList wiki_spec


articleApiQuery title = urlEncodeVars [("format", "json"),
                                       ("action", "query"),
                                       ("titles", title),
                                       ("prop", "revisions|images"),
                                       ("rvprop", "content"),
                                       ("imlimit", "500")]

articleApiURL :: ArticleReference -> Url
articleApiURL article = (baseWikiURL $ wiki article) ++ "/w/api.php?" ++ (articleApiQuery $ title article)

articleComponentsFromJSON :: Result (JSObject JSValue) -> (String, Pandoc, [String])
articleComponentsFromJSON (Ok json) = (real_title, readMediaWiki def $ content , images_names)
    where (Ok pages) = valFromObj "query" json >>= valFromObj "pages"
          (revision, (JSObject page)) = head $ fromJSObject pages
          (Ok real_title) = valFromObj "title" page
          (Ok content) = fmap head (valFromObj "revisions" page) >>= valFromObj "*"
          imagesNames images_array = [let (Ok image_name) = fmap fromJSString $ valFromObj "title" a in image_name | a <- images_array]
          (Ok images_names) = fmap imagesNames (valFromObj "images" page )

getArticle :: ArticleReference -> IO Article
getArticle article_ref@(ArticleReference wiki' _) = do
    (real_title ,source, images_names) <- fmap (articleComponentsFromJSON . decode) $ getPageContent $ articleApiURL article_ref
    let real_article_ref = ArticleReference wiki' real_title
    fmap (Article real_article_ref source) $ images_names `getImagesFrom` (wiki article_ref)
