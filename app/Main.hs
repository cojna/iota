{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified GHC.Driver.Session
import qualified GHC.Hs
import qualified GHC.LanguageExtensions.Type
import qualified GHC.LanguageExtensions.Type as X
import qualified GHC.Parser.Lexer
import qualified GHC.Types.SrcLoc
import qualified GHC.Utils.Error
import qualified GHC.Utils.Outputable
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as GHC.Parser.Ex
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config as GHC.Settings.Config.Ex
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (
  ExitCode (ExitFailure, ExitSuccess),
  exitFailure,
 )
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (rawSystem)
import "template-haskell" Language.Haskell.TH (runIO)

main :: IO ()
main = do
  (name : opts) <- getArgs
  let path = moduleAbsPath name
  withCPPProcessed path $ \processedPath -> do
    code <- readFile processedPath
    case parseFile path code of
      GHC.Parser.Lexer.POk _ ast -> do
        case opts of
          [] -> putStrLn $ renderDecls ast
          [dst] -> do
            appendFile dst
              $ mconcat
                [ header name
                , renderDecls ast
                , "\n"
                ]
          _ -> pure ()
      GHC.Parser.Lexer.PFailed ps -> do
        hPutStrLn stderr $ renderParseErrors ps
        exitFailure

installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

moduleAbsPath :: String -> FilePath
moduleAbsPath name =
  mconcat
    [ installPath
    , "/src/"
    , moduleRelPath name
    ]

moduleRelPath :: String -> FilePath
moduleRelPath name = map convert name <> ".hs"
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

extensions :: [GHC.LanguageExtensions.Type.Extension]
extensions =
  [ X.DerivingVia
  , X.LambdaCase
  , X.MagicHash
  , X.MultiWayIf
  , X.OverloadedStrings
  , X.PatternSynonyms
  , X.RecordWildCards
  , X.TypeFamilies
  , X.UnboxedTuples
  , X.UnboxedSums
  , X.ViewPatterns
  , X.ImplicitParams
  ]

dynFlags :: GHC.Driver.Session.DynFlags
dynFlags =
  foldl
    GHC.Driver.Session.xopt_set
    ( GHC.Driver.Session.defaultDynFlags
        GHC.Settings.Config.Ex.fakeSettings
        GHC.Settings.Config.Ex.fakeLlvmConfig
    )
    extensions

withCPPProcessed :: FilePath -> (FilePath -> IO ()) -> IO ()
withCPPProcessed path f = do
  withSystemTempDirectory "iota" $ \tmpDir -> do
    let processedPath = tmpDir ++ "/cpp-processed.hs"
    exitCode <-
      rawSystem
        "stack"
        ["ghc", "--", "-E", path, "-o", processedPath]
    case exitCode of
      ExitSuccess -> f processedPath
      ExitFailure _ -> exitFailure

parseFile ::
  FilePath ->
  String ->
  GHC.Parser.Lexer.ParseResult (GHC.Types.SrcLoc.Located GHC.Hs.HsModule)
parseFile = flip GHC.Parser.Ex.parseFile dynFlags

renderDecls :: GHC.Types.SrcLoc.Located GHC.Hs.HsModule -> String
renderDecls =
  GHC.Utils.Outputable.renderWithContext
    (GHC.Driver.Session.initDefaultSDocContext dynFlags)
    . GHC.Utils.Outputable.vcat
    . map GHC.Utils.Outputable.ppr
    . GHC.Hs.hsmodDecls
    . GHC.Types.SrcLoc.unLoc

renderParseErrors :: GHC.Parser.Lexer.PState -> String
renderParseErrors =
  GHC.Utils.Outputable.renderWithContext
    (GHC.Driver.Session.initDefaultSDocContext dynFlags)
    . GHC.Utils.Error.pprMessages
    . snd
    . GHC.Parser.Lexer.getPsMessages
