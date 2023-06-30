module Neo.Config (Config(..), readConfig) where


import           System.Environment (getEnv)


type Token = String

newtype Config = Config Token

readConfig :: IO Config
readConfig =
  Config <$> getEnv "API_TOKEN"
