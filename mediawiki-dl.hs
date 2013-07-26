module Main where

import System.Environment

import MediaWiki (downloadArticleReference, articleReferenceFromString, Article(..))

main = do
    article_string:_ <- getArgs
    downloadArticleReference $ articleReferenceFromString article_string
    return ()
