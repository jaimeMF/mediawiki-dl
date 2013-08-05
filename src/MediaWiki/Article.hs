module MediaWiki.Article
( ArticleReference (..)
, Article(..)
, articleReferenceFromString
, getArticle
, getArticleWithRenderedTemplates
) where

import Network.HTTP (urlEncodeVars, urlDecode)
import Network.URI
import Text.JSON
import Text.Pandoc
import Data.Text (splitOn, pack, unpack)

import MediaWiki.Wiki
import MediaWiki.Utils
import MediaWiki.Image

data ArticleReference = ArticleReference { wiki :: Wiki, title :: String }
    deriving (Show, Read)

data Article = Article {reference :: ArticleReference, source :: Pandoc, images :: [Image]} deriving Show

articleReferenceFromSpec art_spec = ArticleReference wiki title
    where parts = reverse $ map unpack $ splitOn (pack ":") $ pack art_spec
          title:wiki_spec = parts
          wiki = wikimediaFromList wiki_spec

articleReferenceFromURI uri = ArticleReference wiki article_title
    where Just domain = fmap uriRegName $ uriAuthority uri
          wiki = WikiNamed domain
          -- TODO: Check if the path really starts with "/wiki/"
          article_title = urlDecode $ drop 6 $ uriPath uri

articleReferenceFromString :: String -> ArticleReference
articleReferenceFromString string = case (parseAbsoluteURI string) of
    Nothing -> articleReferenceFromSpec string
    (Just uri) -> case uriScheme uri of
            "http:" -> articleReferenceFromURI uri
            "https:" -> articleReferenceFromURI uri
            _ -> articleReferenceFromSpec string


articleApiQuery title = urlEncodeVars [("format", "json"),
                                       ("action", "query"),
                                       ("titles", title),
                                       ("prop", "revisions|images"),
                                       ("rvprop", "content"),
                                       ("imlimit", "500")]

templateApiQuery title template = urlEncodeVars [("format", "json"),
                                                 ("action", "parse"),
                                                 ("title", title),
                                                 ("text", template)]

articleApiURL :: ArticleReference -> Url
articleApiURL article = (baseWikiURL $ wiki article) ++ "/w/api.php?" ++ (articleApiQuery $ title article)

templateApiURL article template = (baseWikiApiURL $ wiki article) ++ (templateApiQuery (title article) template)

articleComponentsFromJSON :: Result (JSObject JSValue) -> (String, Pandoc, [String])
articleComponentsFromJSON (Ok json) = (real_title, readMediaWiki def content , images_names)
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


templateFromJSON json = template
    where (Ok (Pandoc _ template)) = fmap (readHtml def . fromJSString) $ valFromObj "*" =<< valFromObj "text" =<< valFromObj "parse" json

getTemplate article template_string = do
    response <- getPageContent $ templateApiURL article template_string
    let (Ok temp) =  fmap templateFromJSON $ decode response
    return temp

getRenderedTemplate :: Article -> [Block] -> IO [Block]
getRenderedTemplate art ((RawBlock t temp):bs) = getTemplate (reference art) temp >>= \x -> fmap (x++) (getRenderedTemplate art bs)
getRenderedTemplate art (b:bs) = fmap ([b]++) (getRenderedTemplate art bs)
getRenderedTemplate art [] = return []

getArticleWithRenderedTemplates :: Article -> IO Article
getArticleWithRenderedTemplates art  = fmap (\x -> art {source=x}) $ bottomUpM (getRenderedTemplate art) $ source art
