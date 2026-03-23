{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON (..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Text.Mustache (compileMustacheFile, renderMustache)
import Text.Pandoc (def, readMarkdown, runIOorExplode, writeDocx)

data TemplateContext = TemplateContext
  { tenant :: Tenant
  }
  deriving (Show, Generic)

instance FromJSON TemplateContext

instance ToJSON TemplateContext

data Tenant = Tenant
  { name :: String,
    cid :: Integer
  }
  deriving (Show, Generic)

instance FromJSON Tenant

instance ToJSON Tenant

main :: IO ()
main = do
  args <- getArgs
  case args of
    [templatePath, contextPath] -> convert templatePath contextPath (templatePath <> ".docx")
    [templatePath, contextPath, outputPath] -> convert templatePath contextPath outputPath
    _ -> error "Usage: contratos <input-markdown-path> <input-context-yaml-path> [output-docx-path]"

convert :: FilePath -> FilePath -> FilePath -> IO ()
convert templatePath contextPath outputPath = do
  template <- compileMustacheFile templatePath
  context <- decodeContext contextPath
  let renderedMarkdown = LT.toStrict (renderMustache template (toJSON context))
  docxBytes <- runIOorExplode $ do
    doc <- readMarkdown def renderedMarkdown
    writeDocx def doc
  BL.writeFile outputPath docxBytes
  putStrLn ("Generated DOCX: " <> outputPath)

decodeContext :: FilePath -> IO TemplateContext
decodeContext path = do
  result <- Yaml.decodeFileEither path
  case result of
    Left err -> error ("Failed to parse YAML context file '" <> path <> "': " <> Yaml.prettyPrintParseException err)
    Right context -> pure context
