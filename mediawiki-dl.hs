module Main where

import System.Environment

import Text.Pandoc

import MediaWiki (getArticle, articleReferenceFromString, Article(..))

getArtContentUsing :: (Pandoc -> String) -> String -> IO String
getArtContentUsing convert_f article_string = fmap (convert_f . source) $ getArticle $ articleReferenceFromString article_string

getArtInMarkdown = getArtContentUsing (writeMarkdown def)
getArtInHTML = getArtContentUsing (writeHtmlString def)

main = do
    article_string:_ <- getArgs
    article_content <- getArtInMarkdown article_string
    putStrLn article_content
    return ()
