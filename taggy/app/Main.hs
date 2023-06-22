module Main (main) where

import Data.Monoid
import Sound.HTagLib
import System.Environment (getArgs)

data AudioTrack = AudioTrack
  { atTitle :: Title,
    atArtist :: Artist,
    atAlbum :: Album,
    atComment :: Comment,
    atGenre :: Genre,
    atYear :: Maybe Year,
    atTrack :: Maybe TrackNumber
  }
  deriving (Show)

audioTrackGetter :: TagGetter AudioTrack
audioTrackGetter =
  AudioTrack
    <$> titleGetter
    <*> artistGetter
    <*> albumGetter
    <*> commentGetter
    <*> genreGetter
    <*> yearGetter
    <*> trackNumberGetter

main :: IO ()
main = do
  path <- head <$> getArgs
  track <- getTags path audioTrackGetter
  print track
