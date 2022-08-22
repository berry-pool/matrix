module Main where

import Onchain (mintSerializedMain, spendSerializedControl, mintSerializedControl, spendSerializedReference)
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as I
import GHC.Generics (Generic)
import Prelude

data Scripts = Scripts {mintMain :: String, spendControl :: String, mintControl :: String, spendReference :: String} deriving (Show, Generic, ToJSON)

scripts :: Scripts
scripts = Scripts {mintMain = mintSerializedMain, spendControl = spendSerializedControl, mintControl = mintSerializedControl, spendReference = spendSerializedReference}

main :: IO ()
main = do
  I.writeFile "scripts.json" (encodeToLazyText scripts)
  putStrLn "Scripts compiled"