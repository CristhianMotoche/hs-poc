{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Aeson           (encode)

import           Neo.Slack            (Status (..))

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Status"
  [ testCase "encode" $
    let fooStatus =
          Status { text = "Foo"
                 , emoji = ":bar:"
                 , expiration = 0
                 }
        codification = toStrict1 $ encode fooStatus
     in do
        "\"profile\":" `B.isInfixOf` codification @? "Contains profile"
        "\"status_text\":\"Foo\"" `B.isInfixOf` codification @? "Contains text key"
        "\"status_emoji\":\":bar:\"" `B.isInfixOf` codification @? "Contains emoji key"
        "\"status_expiration\":0" `B.isInfixOf` codification @? "Contains expiration key"
  ]
