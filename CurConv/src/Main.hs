module Main where

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text
import Data.Monoid (mconcat)


-- Conversion function (only eur <-> usd at the moment) -- 
convert :: Text -> Text -> Text -> Maybe Text 
convert sum cur1 cur2 =   -- "convert sum in cur1 to sum in cur2"
    case (unpack cur1, unpack cur2) of
        ("eur", "usd") -> Just (numToText (textToNum sum * eurToUsd))
        ("usd", "eur") -> Just (numToText (textToNum sum * usdToEur))
        (_, _) -> Nothing
        
        where 
            eurToUsd = 1.1128
            usdToEur = 0.8986
            textToNum :: Text -> Double
            textToNum = \x -> read (unpack x)
            numToText :: Double -> Text
            numToText = \x -> pack (show x)
            
    

main = scotty 3000 $
  get (capture "/convert/") $ do
  pure ()
    

