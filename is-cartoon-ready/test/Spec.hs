module Spec (main) where

import Cartoon
import Scraper

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "scrappers"
    [ testCase "scrapLatestToons" $ do
        text <- readFile "./test/fixtures/example.html"
        let scrapedList = scrapLatestToons text
            expectedCartoons = Just
              [ Cartoon "Unikitty! Season 2" "Episode 28" "/Cartoon/Unikitty-Season-2/Episode-28"
              , Cartoon "DC Super Hero Girls 2019" "Episode 1-2-3-4" "/Cartoon/DC-Super-Hero-Girls-2019/Episode-1-2-3-4"
              , Cartoon "Tangled: The Series Season 2" "Episode 12" "/Cartoon/Tangled-The-Series-Season-2/Episode-12"
              ]
        print scrapedList
        assertBool "Extracts the cartoons" (scrapedList == expectedCartoons)
    ]
