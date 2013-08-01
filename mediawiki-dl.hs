module Main where

import System.Environment
import System.Exit
import System.Console.GetOpt

import Control.Monad

import MediaWiki (downloadArticleReference, articleReferenceFromString, OutputFileFormat (..), DownloadOptions(..), def)

data Opt = Opt
    { optOutputFormat :: String
    }
    deriving (Show)

defaultOpts = Opt
    { optOutputFormat = "md"
    }

usageHeader = "Usage: mediawiki-dl [OPTION...] article"

printUsageInfo = putStrLn $ usageInfo usageHeader options

options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "f" ["format"]
             (ReqArg
                (\arg opt -> return opt {optOutputFormat = arg})
                "FORMAT"
             )
             "output format"
      
    , Option "h" ["help"]
             (NoArg
                (\_ -> do
                    printUsageInfo
                    exitSuccess
                )
            )
            "help"
    ]

main = do
    (actions, args, errors) <- fmap (getOpt Permute options) getArgs

    opts <- foldl (>>=) (return defaultOpts) actions
    let Opt {optOutputFormat = outputFormat} = opts

    unless (null errors) $ do
        putStrLn "Error on arguments parsing:"
        putStrLn $ concat errors
        exitFailure

    when (null args) $ do
        putStrLn usageHeader
        putStrLn "no article given"
        exitFailure

    let writer = case outputFormat of
                "md" -> Markdown
                "html" -> HTML
                "epub" -> EPUB
                _ -> error "Unsoported output format"
        dlOptions = def {outputFormat = writer}
    downloadArticleReference (articleReferenceFromString $ head args) dlOptions
    return ()
