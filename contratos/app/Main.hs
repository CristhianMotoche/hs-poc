module Main (main) where

import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import System.Environment (getArgs, getEnvironment)
import Text.Mustache (compileMustacheFile, renderMustache)
import Text.Pandoc (def, readMarkdown, runIOorExplode, writeDocx)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> convert inputPath (inputPath <> ".docx")
    [inputPath, outputPath] -> convert inputPath outputPath
    _ -> error "Usage: contratos <input-markdown-path> [output-docx-path]"

convert :: FilePath -> FilePath -> IO ()
convert inputPath outputPath = do
  template <- compileMustacheFile inputPath
  envVars <- getEnvironment
  let context = object [fromString key .= value | (key, value) <- envVars]
      renderedMarkdown = LT.toStrict (renderMustache template context)
  docxBytes <- runIOorExplode $ do
    doc <- readMarkdown def renderedMarkdown
    writeDocx def doc
  BL.writeFile outputPath docxBytes
  putStrLn ("Generated DOCX: " <> outputPath)
