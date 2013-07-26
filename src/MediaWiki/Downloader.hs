module MediaWiki.Downloader
( downloadArticle
, downloadArticleReference
) where

import System.FilePath (addExtension)
import Data.List (stripPrefix)
import qualified Data.ByteString as B
import Text.Pandoc (writeMarkdown, def)

import MediaWiki.Article
import MediaWiki.Image
import MediaWiki.Utils

downloadImage :: Image -> IO ()
downloadImage img = do
    let i_url = url img
        final_name = case ("File:" `stripPrefix` name img) of
            Just name -> name
            Nothing -> name img
    putStrLn $ "Downloading " ++ show final_name
    getBinaryContent i_url >>= B.writeFile final_name
    return ()

downloadArticle :: Article -> IO ()
downloadArticle article = do
    let file_name = (title $ reference article) `addExtension` ".md"
        markdown = writeMarkdown def $ source article
    putStrLn $ "Saving article to " ++ show file_name
    file_name `writeFile` markdown
    putStrLn $ "Downloading " ++ (show $ length $ images article) ++ " additional files"
    sequence $ map downloadImage $ images article
    return ()

downloadArticleReference :: ArticleReference -> IO ()
downloadArticleReference art_ref = do
    putStrLn $ "Getting information for " ++ (show $ title art_ref)
    getArticle art_ref >>= downloadArticle
