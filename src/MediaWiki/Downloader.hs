module MediaWiki.Downloader
( downloadArticle
, downloadArticleReference
, OutputFileFormat(..)
, DownloadOptions(..)
, def
) where

import System.FilePath (addExtension)
import System.Directory (doesFileExist, renameFile)
import Data.List (stripPrefix)
import Data.Default
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Text.Pandoc (writeMarkdown, writeHtmlString, writeEPUB, def, WriterOptions(..), EPUBVersion(..), getDefaultTemplate)

import MediaWiki.Article
import MediaWiki.Image
import MediaWiki.Utils

data OutputFileFormat = Markdown | HTML | EPUB  deriving (Show, Read, Eq)

saveMarkdown md_name article = do
    Right template <- getDefaultTemplate Nothing "markdown"
    md_name `writeFile` (writeMarkdown def {writerStandalone=True, writerTemplate=template} $ source article)
    return ()

saveHTML html_name article = do
    Right template <- getDefaultTemplate Nothing "html"
    html_name `writeFile` (writeHtmlString def {writerStandalone=True, writerTemplate=template} $ source article)
    return ()

saveEPUB epub_name article = do
    Right epub_template <- getDefaultTemplate Nothing "epub"
    writeEPUB def {writerEpubVersion= Just EPUB2, writerStandalone=True, writerTemplate=epub_template} (source article) >>= Bl.writeFile epub_name
    return ()

savers = [
     (Markdown, (saveMarkdown, "md"))
    ,(HTML, (saveHTML, "html"))
    ,(EPUB, (saveEPUB, "epub"))
    ]

data DownloadOptions = DownloadOptions
    { outputFormat :: OutputFileFormat
    , downloadResources :: Bool
    } deriving Show

instance Default DownloadOptions where
    def = DownloadOptions { outputFormat = EPUB
                          , downloadResources = True
                          }

-- |Download using an intermediate temporary file if the file doesn't exist
downloadUsingTemp final_name url = do
    let temp_file = addExtension final_name "temp"
    f_exists <- doesFileExist final_name
    if f_exists
        then putStrLn $ (show final_name) ++ " has already been downloaded"
        else do
            putStrLn $ "Downloading " ++ show final_name
            getBinaryContent url >>= B.writeFile temp_file
            renameFile temp_file final_name

downloadImage :: Image -> IO ()
downloadImage img = do
    let i_url = url img
        final_name = case ("File:" `stripPrefix` name img) of
            Just name -> name
            Nothing -> name img
    downloadUsingTemp final_name i_url
    return ()

downloadArticleResources article = do
    putStrLn $ "Downloading " ++ (show $ length $ images article) ++ " additional files"
    sequence $ map downloadImage $ images article
    return ()

downloadArticle :: Article -> DownloadOptions -> IO ()
downloadArticle article options = do
    let Just (saver, ext) = (outputFormat options) `lookup` savers
        file_name = (title $ reference article) `addExtension` ext
    when (downloadResources options) $ downloadArticleResources article 
    putStrLn $ "Saving article to " ++ (show file_name)
    saver file_name article
    return ()

downloadArticleReference :: ArticleReference -> DownloadOptions -> IO ()
downloadArticleReference art_ref downloadOptions = do
    putStrLn $ "Getting information for " ++ (show $ title art_ref)
    getArticle art_ref >>= flip downloadArticle downloadOptions
