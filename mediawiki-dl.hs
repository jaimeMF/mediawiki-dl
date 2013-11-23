module Main where

import System.Environment
import System.Exit
import System.Console.GetOpt

import Control.Monad

import MediaWiki (downloadArticleReference, articleReferenceFromString, OutputFileFormat (..), DownloadOptions(..), def)

data Opt = Opt
    { optOutputFormat :: String
    , debug :: Bool
    }
    deriving (Show)

defaultOpts = Opt
    { optOutputFormat = "md"
    , debug = False
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
    , Option "" ["debug"]
             (NoArg
                (\opt -> return opt {debug = True})
             )
             "turn on debug mode"
    , Option "h" ["help"]
             (NoArg
                (\_ -> do
                    printUsageInfo
                    exitSuccess
                )
            )
            "help"
    ]

optsToDlOpts opts = dlOptions
    where writer = case (optOutputFormat opts) of
                "md" -> Markdown
                "html" -> HTML
                "epub" -> EPUB
                _ -> error "Unsoported output format"
          dlOptions = def {outputFormat = writer, debugMode = debug opts}

main = do
    (actions, args, errors) <- fmap (getOpt Permute options) getArgs

    opts <- foldl (>>=) (return defaultOpts) actions

    unless (null errors) $ do
        putStrLn "Error on arguments parsing:"
        putStrLn $ concat errors
        exitFailure

    when (null args) $ do
        putStrLn usageHeader
        putStrLn "no article given"
        exitFailure

    downloadArticleReference (articleReferenceFromString $ head args) $ optsToDlOpts opts
    return ()
