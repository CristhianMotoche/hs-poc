{-# LANGUAGE OverloadedStrings #-}

module Scraper (scrapLatestToons, cartoonsScrapper) where

import Data.Char

import Text.HTML.Scalpel

import Cartoon (Cartoon(..))

scrapLatestToons :: String -> Maybe [Cartoon]
scrapLatestToons = flip scrapeStringLike cartoonsScrapper

cartoonsScrapper :: Scraper String [Cartoon]
cartoonsScrapper =
  chroot ("div" @: [hasClass "item-list"]) $
    chroots ("div" @: [hasClass "item"])
      cartoonScrapper

cartoonScrapper :: Scraper String Cartoon
cartoonScrapper = do
  cartoonName <- attr "title" "img"
  newEpisode  <- text $ "span" @: [hasClass "textWhite"]
  urlEpisode  <- chroot ("div" @: [hasClass "info"]) $
                    attr "href" "a"
  return $ Cartoon cartoonName (trim newEpisode) urlEpisode

-- | Helpers

trim :: String -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
