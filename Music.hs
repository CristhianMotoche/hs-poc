

    Type: Guitar
    Description: A type representing a guitar.
    Fields:
    - model :: String
    - brand :: String
    - color :: String
    - electric :: Bool
    - acoustic :: Bool
    - numStrings :: Int

    Type: Bass
    Description: A type representing a bass guitar.
    Fields:
    - model :: String
    - brand :: String
    - color :: String
    - electric :: Bool
    - numStrings :: Int

    Type: Drum Set
    Description: A type representing a full drum set.
    Fields:
    - brand :: String
    - acoustic :: Bool
    - numDrums :: Int
    - cymbalsIncluded :: Bool

    Type: Snare Drum
    Description: A type representing a snare drum.
    Fields:
    - brand :: String
    - diameter :: Double
    - depth :: Double
    - material :: String

    Type: Keyboard
    Description: A type representing a musical keyboard.
    Fields:
    - model :: String
    - brand :: String
    - color :: String
    - numOctaves :: Int
    - weightedKeys :: Bool

    Type: Amplifier
    Description: A type representing an audio amplifier.
    Fields:
    - brand :: String
    - wattage :: Int
    - numChannels :: Int
    - isHead :: Bool

    Type: Microphone
    Description: A type representing a microphone.
    Fields:
    - brand :: String
    - polarPattern :: String
    - frequencyResponse :: String
    - connectorType :: String

    Type: PA System
    Description: A type representing a public address (PA) system.
    Fields:
    - brand :: String
    - powerOutput :: Int
    - numChannels :: Int
    - weight :: Double

    Type: DJ Mixer
    Description: A type representing a DJ mixer.
    Fields:
    - brand :: String
    - numChannels :: Int
    - numInputs :: Int
    - numOutputs :: Int

    Type: Turntable
    Description: A type representing a turntable.
    Fields:
    - brand :: String
    - model :: String
    - color :: String
    - numSpeeds :: Int

    Type: Speaker
    Description: A type representing a speaker.
    Fields:
    - brand :: String
    - model :: String
    - speakerType :: String
    - powerHandling :: Int

    Type: Headphones
    Description: A type representing headphones.
    Fields:
    - brand :: String
    - model :: String
    - color :: String
    - noiseCancelling :: Bool

    Type: Guitar Pedal
    Description: A type representing a guitar effects pedal.
    Fields:
    - brand :: String
    - model :: String
    - color :: String
    - effectType :: String

    Type: Guitar Strap
    Description: A type representing a guitar strap.
    Fields:
    - brand :: String
    - material :: String
    - color :: String
    - adjustable :: Bool

    Type: Strings
    Description: A type representing guitar strings.
    Fields:
    - brand :: String
    - gauge :: Double
    - material :: String
    - numStrings :: Int

    Type: Guitar Pick
    Description: A type representing a guitar pick.
    Fields:
    - brand :: String
    - material :: String
    - thickness :: Double

    Type: Guitar Stand
    Description: A type representing a guitar stand.
    Fields:
    - brand :: String
    - material :: String
    - color :: String
    - collapsible :: Bool

    Type: Sheet Music
    Description: A type representing sheet music.
    Fields:
    - title :: String
    - artist :: String
    - composer :: String
    - instrument :: String

    Type: Metronome
    Description: A type representing a metronome.
    Fields:
    - brand :: String
    - tempoRange :: (Int, Int)
    - beatAccent :: Bool

    Type: Guitar Tuner
    Description: A type representing a guitar tuner.
    Fields:
    - brand :: String
    - numStrings :: Int
    - displayType :: String
    - tuningMode :: String
