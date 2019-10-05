{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Language.Haskell.Exts       as H
import           Language.Haskell.TH         (runIO)
import qualified Language.Preprocessor.Cpphs as CPP
import           System.Directory            (getCurrentDirectory)
import           System.Environment          (getArgs)

main :: IO ()
main = do
    (name:opts) <- getArgs
    code <- readFile $ modulePath name
    H.ParseOk ast <- H.parseModuleWithMode parseMode
        <$> CPP.runCpphs cppOpt "/dev/null" code
    case opts of
        [] -> putStrLn $ pretty ast
        [dst] -> do
            appendFile dst $ header name
            appendFile dst $ pretty ast

installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory;[e|dir|])

modulePath :: String -> FilePath
modulePath name = mconcat
    [ installPath
    , "/src/"
    , map convert name
    , ".hs"
    ]
  where
    convert '.' = '/'
    convert c   = c

lineWidth :: Int
lineWidth = 80

header :: String -> String
header name = unlines
    [ replicate (lineWidth - 1) '-'
    , "-- " ++ name
    , replicate (lineWidth - 1) '-'
    ]

pretty :: H.Module l -> String
pretty (H.Module _ _ _ _ decls) = unlines
    $ map (H.prettyPrintWithMode pphsMode) decls

cppOpt :: CPP.CpphsOptions
cppOpt = CPP.defaultCpphsOptions
    { CPP.boolopts = CPP.defaultBoolOptions
        { CPP.macros = False
        , CPP.locations = False
        }
    }

parseMode :: H.ParseMode
parseMode = H.defaultParseMode {
    H.extensions =
        [ H.EnableExtension H.BangPatterns
        , H.EnableExtension H.CPP
        , H.EnableExtension H.LambdaCase
        , H.EnableExtension H.MagicHash
        , H.EnableExtension H.MultiParamTypeClasses
        , H.EnableExtension H.RecordWildCards
        , H.EnableExtension H.TypeFamilies
        ]
    }

pphsMode :: H.PPHsMode
pphsMode = H.defaultMode{ H.layout = H.PPNoLayout }
