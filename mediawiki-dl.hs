module Main where

import System.Environment

import MediaWiki (downloadArticleReference, articleReferenceFromString, OutputFileFormat (..), DownloadOptions(..), def)

main = do
    article_string:rest <- getArgs
    let writer = case rest of
            [] -> Markdown
            [writer_string] -> case writer_string of
                "md" -> Markdown
                "html" -> HTML
                "epub" -> EPUB
                _ -> error "Unsoported output format"
        options = def {outputFormat = writer}
    downloadArticleReference (articleReferenceFromString article_string) options
    return ()
