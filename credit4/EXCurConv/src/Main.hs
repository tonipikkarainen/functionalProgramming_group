{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad
import Data.Text.Read
import Data.Text.Lazy hiding (length)
import Text.Read (readMaybe)
import Data.Maybe

-- Tässä tehdään muunnos kurssien välillä. 
-- Tällä hetkellä vain eur -> usd ja usd -> eur
-- logiikka kuitenkin toteutettu
-- rahasumman lukeminen toteutettu readMaybellä, jotta
-- vältytään poikkeuksilta.
muunna :: Text -> Text -> Text -> Text
muunna summa to from 
    | isNothing(rahaSumma) = pack "ei kelvollinen rahasumma"
    | to_string == "usd" && from_string == "eur" = pack (show (1.11*fromJust(rahaSumma)))
    | to_string == "eur" && from_string == "usd" = pack (show (0.9*fromJust(rahaSumma)))
    | otherwise = pack "ei osata muuntaa"
  where
      rahaSumma = readMaybe (unpack summa) :: Maybe Float
      to_string = unpack to
      from_string = unpack from 

main = scotty 3000 $ do
  get "/convert/:x" $ do
    pars  <- params 
    unless ((length pars) == 3) next

    let (arvo,numero) = pars !! 0 
    let (to,to_kurssi) = pars !! 1
    let (from,from_kurssi) = pars !! 2  
    
    html $ mconcat ["<h1>Muunnos from: ",from_kurssi," to: ",
            to_kurssi," = " , (muunna numero to_kurssi from_kurssi),"</h1>"]

  get "/convert/:x" $ do
    text "Anna kolme parametria muodossa /15?to=usd&from=eur"
  


