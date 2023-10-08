{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  (
    pLocMode, 
    pRenderConfig,
    RenderConfig (..),
    ClusterModules (..),
    LocMode (..),
    pGraphVizConfig,
    GraphVizConfig (..),
    EdgeCleanupConfig(..),
    pEdgeCleanupConfig,
    pDependencyFilterConfig,
  )
 where

import Options.Applicative hiding (style)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Text.Read.Lex as Optparse

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

import Calligraphy.Compat.Debug (ppHieFile)
import qualified Calligraphy.Compat.GHC as GHC
import Calligraphy.Phases.DependencyFilter
import Calligraphy.Phases.EdgeCleanup
import Calligraphy.Phases.NodeFilter
import Calligraphy.Phases.Parse
-- import Calligraphy.Phases.Render
import Calligraphy.Phases.Render.Common
import Calligraphy.Phases.Render.GraphViz
import Calligraphy.Phases.Render.Mermaid
import Calligraphy.Phases.Search
import Calligraphy.Util.Printer
import Calligraphy.Util.Types (ppCallGraph)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Options.Applicative
-- import Paths_calligraphy (version)
import System.Directory (findExecutable)
import System.Exit
import System.IO (stderr)
import System.Process

main :: IO ()
main = do
  config <- execParser $ info (pConfig <**> helper <**> versionP) mempty
  mainWithConfig config
  where
    versionP =
      infoOption
        ( "calligraphy version "
            <> showVersion version
            <> "\nhie version "
            <> show GHC.hieVersion
        )
        (long "version" <> help "Show version")

mainWithConfig :: AppConfig -> IO ()
mainWithConfig AppConfig {..} = do
  let debug :: (DebugConfig -> Bool) -> Printer () -> IO ()
      debug fp printer = when (fp debugConfig) (printStderr printer)

  hieFiles <- searchFiles searchConfig
  when (null hieFiles) $ die "No files matched your search criteria.."
  debug dumpHieFile $ mapM_ ppHieFile hieFiles

  (parsePhaseDebug, cgParsed) <- either (printDie . ppParseError) pure (parseHieFiles hieFiles)
  debug dumpLexicalTree $ ppParsePhaseDebugInfo parsePhaseDebug
  let cgCollapsed = filterNodes nodeFilterConfig cgParsed
  cgDependencyFiltered <- either (printDie . ppFilterError) pure $ dependencyFilter dependencyFilterConfig cgCollapsed
  let cgCleaned = cleanupEdges edgeFilterConfig cgDependencyFiltered
  debug dumpFinal $ ppCallGraph cgCleaned

  let renderConfig'
        | collapseModules nodeFilterConfig = renderConfig {clusterModules = ClusterNever}
        | otherwise = renderConfig
  renderable <- either (printDie . ppRenderError) pure (renderGraph renderConfig' cgCleaned)

  output
    outputConfig
    (runPrinter $ renderGraphViz graphVizConfig renderable)
    (runPrinter $ renderMermaid renderable)

data AppConfig = AppConfig
  { searchConfig :: SearchConfig,
    nodeFilterConfig :: NodeFilterConfig,
    dependencyFilterConfig :: DependencyFilterConfig,
    edgeFilterConfig :: EdgeCleanupConfig,
    renderConfig :: RenderConfig,
    graphVizConfig :: GraphVizConfig,
    outputConfig :: OutputConfig,
    debugConfig :: DebugConfig
  }

printStderr :: Printer () -> IO ()
printStderr = Text.hPutStrLn stderr . runPrinter

printDie :: Printer () -> IO a
printDie txt = printStderr txt >> exitFailure

pConfig :: Parser AppConfig
pConfig =
  AppConfig
    <$> pSearchConfig
    <*> pNodeFilterConfig
    <*> pDependencyFilterConfig
    <*> pEdgeCleanupConfig
    <*> pRenderConfig
    <*> pGraphVizConfig
    <*> pOutputConfig
    <*> pDebugConfig

output :: OutputConfig -> Text -> Text -> IO ()
output cfg@OutputConfig {..} dotTxt mermaidTxt = do
  unless (hasOutput cfg) $ Text.hPutStrLn stderr "Warning: no output options specified, run with --help to see options"
  getSvg <- once $ runDot ["-Tsvg"]
  forM_ outputDotPath $ \fp -> Text.writeFile fp dotTxt
  forM_ outputPngPath $ \fp -> runDot ["-Tpng", "-o", fp]
  forM_ outputSvgPath $ \fp -> getSvg >>= writeFile fp
  forM_ outputMermaidPath $ \fp -> Text.writeFile fp mermaidTxt
  case outputStdout of
    StdoutDot -> Text.putStrLn dotTxt
    StdoutMermaid -> Text.putStrLn mermaidTxt
    StdoutSVG -> getSvg >>= putStrLn
    StdoutNone -> pure ()
  where
    hasOutput (OutputConfig Nothing Nothing Nothing Nothing _ StdoutNone) = False
    hasOutput _ = True

    once :: IO a -> IO (IO a)
    once act = do
      ref <- newIORef Nothing
      pure $
        readIORef ref >>= \case
          Just a -> pure a
          Nothing -> do
            a <- act
            writeIORef ref (Just a)
            pure a

    runDot :: [String] -> IO String
    runDot flags = do
      mexe <- findExecutable outputEngine
      case mexe of
        Nothing -> die $ "Unable to find '" <> outputEngine <> "' executable! Make sure it is installed, or use another output method/engine."
        Just exe -> do
          (code, out, err) <- readProcessWithExitCode exe flags (T.unpack dotTxt)
          case code of
            ExitSuccess -> pure out
            _ -> printDie $ do
              strLn $ outputEngine <> " crashed with " <> show code
              strLn "Stdout:"
              indent $ strLn out
              strLn "Stderr:"
              indent $ strLn err

data StdoutFormat = StdoutNone | StdoutDot | StdoutMermaid | StdoutSVG

data OutputConfig = OutputConfig
  { outputDotPath :: Maybe FilePath,
    outputPngPath :: Maybe FilePath,
    outputSvgPath :: Maybe FilePath,
    outputMermaidPath :: Maybe FilePath,
    outputEngine :: String,
    outputStdout :: StdoutFormat
  }

pOutputConfig :: Parser OutputConfig
pOutputConfig =
  OutputConfig
    <$> optional (strOption (long "output-dot" <> short 'd' <> metavar "FILE" <> help ".dot output path"))
    <*> optional (strOption (long "output-png" <> short 'p' <> metavar "FILE" <> help ".png output path (requires `dot` or other engine in PATH)"))
    <*> optional (strOption (long "output-svg" <> short 's' <> metavar "FILE" <> help ".svg output path (requires `dot` or other engine in PATH)"))
    <*> optional (strOption (long "output-mermaid" <> short 'm' <> metavar "FILE" <> help "Mermaid output path"))
    <*> strOption (long "render-engine" <> metavar "CMD" <> help "Render engine to use with --output-png and --output-svg" <> value "dot" <> showDefault)
    <*> pStdoutFormat

pStdoutFormat :: Parser StdoutFormat
pStdoutFormat =
  flag' StdoutDot (long "stdout-dot" <> help "Output graphviz dot to stdout")
    <|> flag' StdoutMermaid (long "stdout-mermaid" <> help "Output Mermaid to stdout")
    <|> flag' StdoutSVG (long "stdout-svg" <> help "Output SVG to stdout")
    <|> pure StdoutNone

data DebugConfig = DebugConfig
  { dumpHieFile :: Bool,
    dumpLexicalTree :: Bool,
    dumpFinal :: Bool
  }

pDebugConfig :: Parser DebugConfig
pDebugConfig =
  DebugConfig
    <$> switch (long "ddump-hie-file" <> help "Debug dump raw HIE files.")
    <*> switch (long "ddump-lexical-tree" <> help "Debug dump the reconstructed lexical structure of HIE files, the intermediate output in the parsing phase.")
    <*> switch (long "ddump-final" <> help "Debug dump the final tree after processing, i.e. as it will be rendered.")



-- Used in GraphViz.hs
data GraphVizConfig = GraphVizConfig
  { 
    showChildArrowhead :: Bool,
    clusterGroups :: Bool,
    splines :: Bool,
    reverseDependencyRank :: Bool
  }

pGraphVizConfig :: Parser GraphVizConfig
pGraphVizConfig =
  GraphVizConfig
    <$> flag False True (long "show-child-arrowhead" <> help "Put an arrowhead at the end of a parent-child edge")
    <*> flag True False (long "no-cluster-trees" <> help "Don't draw definition trees as a cluster.")
    <*> flag True False (long "no-splines" <> help "Render arrows as straight lines instead of splines")
    <*> flag False True (long "reverse-dependency-rank" <> help "Make dependencies have lower rank than the dependee, i.e. show dependencies above their parent.")

-- Used in Common.hs

data LocMode = Hide | Line | LineCol

pLocMode :: Parser LocMode
pLocMode =
  flag' Line (long "show-line" <> help "Show line numbers")
    <|> flag' LineCol (long "show-line-col" <> help "Show line and column numbers")
    <|> pure Hide

data RenderConfig = RenderConfig
  { 
    showCalls :: Bool,
    showTypes :: Bool,
    showKey :: Bool,
    showGHCKeys :: Bool,
    showModulePath :: Bool,
    locMode :: LocMode,
    clusterModules :: ClusterModules
  }

data ClusterModules
  = ClusterNever
  | ClusterWhenMultiple
  | ClusterAlways

pRenderConfig :: Parser RenderConfig
pRenderConfig =
  RenderConfig
    <$> flag True False (long "hide-calls" <> help "Don't show call arrows")
    <*> flag True False (long "hide-types" <> help "Don't show type arrows")
    <*> flag False True (long "show-key" <> help "Show internal keys with identifiers. Useful for debugging.")
    <*> flag False True (long "show-ghc-key" <> help "Show GHC keys with identifiers. Useful for debugging.")
    <*> flag False True (long "show-module-path" <> help "Show a module's filepath instead of its name")
    <*> pLocMode
    <*> pClusterModules

pClusterModules :: Parser ClusterModules
pClusterModules =
  flag' ClusterNever (long "no-cluster-modules" <> help "Don't draw modules as a cluster.")
    <|> flag' ClusterAlways (long "force-cluster-modules" <> help "Draw modules as a cluster, even if there is only one.")
    <|> pure ClusterWhenMultiple

-- Used in EdgeCleanup.hs
data EdgeCleanupConfig = EdgeCleanupConfig
  { 
    cleanDoubles :: Bool,
    cleanLoops :: Bool,
    cleanData :: Bool,
    cleanClass :: Bool
  }

pEdgeCleanupConfig :: Parser EdgeCleanupConfig
pEdgeCleanupConfig =
  EdgeCleanupConfig
    <$> flag True False (long "no-clean-double-edges" <> help "Don't remove type edges when value edges already exist.")
    <*> flag True False (long "no-clean-loops" <> help "Don't remove edges that start and stop at the same node, i.e. simple recursion.")
    <*> flag True False (long "no-clean-data" <> help "Don't remove type edges constructors/records back to the parent data type. These are removed by default because their behavior is unreliable, and they're generally redundant.")
    <*> flag True False (long "no-clean-classes" <> help "Don't remove type edges from class members back to the parent class. These are removed by default because their behavior is unreliable, and they're generally redundant.")

-- Used in Render.hs

-- Used in DependencyFilter.hs

data DependencyFilterConfig = DependencyFilterConfig
  { 
    _depRoot :: Maybe (NonEmpty String),
    _revDepRoot :: Maybe (NonEmpty String),
    _depDepth :: Maybe Int,
    _followParent :: Bool,
    _followChildren :: Bool,
    _followCalls :: Bool,
    _followTypes :: Bool
  }

pDependencyFilterConfig :: Parser DependencyFilterConfig
pDependencyFilterConfig =
  DependencyFilterConfig
    <$> (fmap nonEmpty . many)
      ( strOption
          ( long "forward-root"
              <> short 'f'
              <> metavar "NAME"
              <> help "Name of a dependency filter root. Specifying a dependency filter root hides everything that's not a (transitive) dependency of a root. The name can be qualified. This argument can be repeated."
          )
      )
    <*> (fmap nonEmpty . many)
      ( strOption
          ( long "reverse-root"
              <> short 'r'
              <> metavar "NAME"
              <> help "Name of a reverse dependency filter root. Specifying a dependency filter root hides everything that's not a reverse (transitive) dependency of a root. The name can be qualified. This argument can be repeated."
          )
      )
    <*> optional (option auto (long "max-depth" <> help "Maximum search depth for transitive dependencies."))
    <*> boolFlags True "follow-parent" "In calculating (transitive) dependencies, follow edges to from a child to its parent." mempty
    <*> boolFlags True "follow-child" "In calculating (transitive) dependencies, follow edges from a parent to its children." mempty
    <*> boolFlags True "follow-value" "In calculating (transitive) dependencies, follow normal edges." mempty
    <*> boolFlags False "follow-type" "In calculating (transitive) dependencies, follow type edges." mempty