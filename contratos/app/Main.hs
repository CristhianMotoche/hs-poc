{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Text.Mustache (compileMustacheFile, renderMustache)
import Text.Pandoc (def, readMarkdown, runIOorExplode, writeDocx)

data TemplateContext = TemplateContext
  { tenant :: Tenant,
    street_one :: String,
    street_two :: String,
    canon_price :: Double,
    canon_price_words :: String,
    landlords :: [Landlord],
    banking :: Banking,
    payment_day :: Integer,
    dates :: Dates
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

data Landlord = Landlord
  { name :: String,
    cid :: String
  }
  deriving (Show, Generic)

instance FromJSON Landlord

instance ToJSON Landlord

data Banking = Banking
  { name :: String,
    cid :: String,
    number :: String,
    account :: String
  }
  deriving (Show, Generic)

instance FromJSON Banking

instance ToJSON Banking

data Date = Date
  { d :: Integer,
    m :: Integer,
    y :: Integer
  }
  deriving (Show, Generic)

instance FromJSON Date

instance ToJSON Date where
  toJSON date =
    object
      [ "d" .= d date,
        "m" .= m date,
        "y" .= y date,
        "m_word" .= monthToSpanish (m date)
      ]

monthToSpanish :: Integer -> String
monthToSpanish 1 = "Enero"
monthToSpanish 2 = "Febrero"
monthToSpanish 3 = "Marzo"
monthToSpanish 4 = "Abril"
monthToSpanish 5 = "Mayo"
monthToSpanish 6 = "Junio"
monthToSpanish 7 = "Julio"
monthToSpanish 8 = "Agosto"
monthToSpanish 9 = "Septiembre"
monthToSpanish 10 = "Octubre"
monthToSpanish 11 = "Noviembre"
monthToSpanish 12 = "Diciembre"
monthToSpanish _ = "Mes_desconocido"

data Dates = Dates
  { from :: Date,
    to :: Date,
    sign :: Date
  }
  deriving (Show, Generic)

instance FromJSON Dates

instance ToJSON Dates

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
