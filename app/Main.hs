{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.List as L
import qualified Language.Haskell.Exts as H
import Language.Haskell.TH (runIO)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.IO.Temp (withTempDirectory)
import System.Process (rawSystem)

main :: IO ()
main = do
  (name : opts) <- getArgs
  let path = modulePath name
  code <- readFile path
  processed <- withTempDirectory "." "iota-cpp" $ \tmpDir -> do
    let originalPath = tmpDir ++ "/define-removed.hs"
    let processedPath = tmpDir ++ "/cpp-processed.hs"
    writeFile originalPath $ removeDefineMacros code
    _exitCode <-
      rawSystem
        "stack"
        ["ghc", "--", "-E", originalPath, "-o", processedPath]
    removeMacros <$> readFile processedPath
  let Just (_, exts) = H.readExtensions processed
  let parseOption =
        H.defaultParseMode
          { H.parseFilename = path
          , H.extensions = exts
          }
  case H.parseModuleWithMode parseOption processed of
    H.ParseOk ast -> case opts of
      [] -> putStrLn $ pretty ast
      [dst] -> do
        appendFile dst $ header name
        appendFile dst $ pretty ast
      _ -> pure ()
    failed -> print failed

installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

modulePath :: String -> FilePath
modulePath name =
  mconcat
    [ installPath
    , "/src/"
    , map convert name
    , ".hs"
    ]
  where
    convert '.' = '/'
    convert c = c

lineWidth :: Int
lineWidth = 80

header :: String -> String
header name =
  unlines
    [ replicate (lineWidth - 1) '-'
    , "-- " ++ name
    , replicate (lineWidth - 1) '-'
    ]
#if MIN_VERSION_haskell_src_exts(1,18,0)
pretty :: H.Module l -> String
pretty (H.Module _ _ _ _ decls) = unlines
#else
pretty :: H.Module -> String
pretty (H.Module _ _ _ _ _ _ decls) = unlines
#endif
    $ map (H.prettyPrintWithMode pphsMode) decls
pretty _ = ""

removeDefineMacros :: String -> String
removeDefineMacros = unlines . filter (not . L.isPrefixOf "#define") . lines

removeMacros :: String -> String
removeMacros = unlines . filter (not . L.isPrefixOf "#") . lines

pphsMode :: H.PPHsMode
pphsMode = H.defaultMode{H.layout = H.PPNoLayout}
