{-# LANGUAGE RecordWildCards #-}

module Main where

import Scraper

import Cartoon

import Text.HTML.Scalpel

main :: IO ()
main =
  let url = domain ++ "/Status/Ongoing/LatestUpdate"
      domain = "https://kimcartoon.to"
   in scrapeURL url cartoonsScrapper >>= \result ->
     case result of
       Nothing -> print "Hey! I think the HTML has changed or our scrapper is failing!"
       Just cartoonList ->
         mapM_ (putStrLn . format) cartoonList
           where
             format Cartoon {..} =
               "Cartoon: " ++ name ++ " " ++ latestEpisode ++ "\n" ++
                 "\t Go to: " ++ domain ++ episodeURL
