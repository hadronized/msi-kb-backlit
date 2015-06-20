-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

import Data.ByteString ( pack )
import Data.Foldable
import Data.List.Split ( splitOn )
import Data.Word ( Word16 )
import Prelude hiding ( Left, Right )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.HID
import System.IO

vendorID,productID :: Word16
vendorID  = 0x1770 
productID = 0xff00

data Color
  = Black
  | Red
  | Orange
  | Yellow
  | Green
  | Sky
  | Blue
  | Purple
  | White
    deriving (Enum,Eq,Show)

data Intensity
  = High
  | Medium
  | Low
  | Light
    deriving (Enum,Eq,Show)

data Region
  = Left
  | Middle
  | Right
    deriving (Enum,Eq,Show)

data Mode
  = Normal
  | Gaming
  | Breathe
  | Demo
  | Wave
    deriving (Enum,Eq,Show)

data Conf 
  = Mode Mode
  | Region (Region,Color,Intensity)
  | Wrong String
    deriving (Eq,Show)

options :: [OptDescr Conf]
options =
  [
    Option ['n'] ["normal"] (NoArg $ Mode Normal) "normal mode"
  , Option ['g'] ["gaming"] (NoArg $ Mode Gaming) "gaming mode"
  , Option ['b'] ["breathe"] (NoArg $ Mode Breathe) "breathe mode"
  , Option ['d'] ["demo"] (NoArg $ Mode Demo) "demo mode"
  , Option ['w'] ["wave"] (NoArg $ Mode Wave) "wave mode"
  , Option ['l'] ["left"] (ReqArg (parseRegion Left) "color") "color to assign to the left region"
  , Option ['m'] ["middle"] (ReqArg (parseRegion Middle) "color") "color to assign to middle region"
  , Option ['r'] ["right"] (ReqArg (parseRegion Right) "color") "color to assign to right region"
  ]

parseRegion :: Region -> String -> Conf
parseRegion r s = maybe (Wrong "region") Region intensityColor
  where
    splitUp = splitOn "-" s
    [intensity,color] = if length splitUp == 2 then splitUp else ["high",s]
    parsedIntensity
      | intensity == "high" = Just High
      | intensity == "medium" = Just Medium
      | intensity == "low" = Just Low
      | intensity == "light" = Just Light
      | otherwise = Nothing
    parsedColor
      | color == "black" = Just Black
      | color == "red" = Just Red
      | color == "orange" = Just Orange
      | color == "yellow" = Just Yellow
      | color == "green" = Just Green
      | color == "sky" = Just Sky
      | color == "blue" = Just Blue
      | color == "purple" = Just Purple
      | color == "white" = Just White
      | otherwise = Nothing
    intensityColor = (r,,) <$> parsedColor <*> parsedIntensity
  
confMode :: [Conf] -> Mode
confMode [] = Normal
confMode (conf:cs) = case conf of
  Mode m -> m
  Wrong _ -> Normal
  _ -> confMode cs

confRegions :: [Conf] -> [(Region,Color,Intensity)]
confRegions = foldl extractRegion []
  where
    extractRegion regions (Mode _) = regions
    extractRegion regions (Region r) = r : regions
    extractRegion regions (Wrong _) = regions

confWrong :: [Conf] -> String
confWrong = foldl extractWrong ""
  where
    extractWrong _ (Wrong s) = s
    extractWrong s _ = s

err :: String -> IO ()
err = hPutStrLn stderr

usage :: String
usage = flip usageInfo options $ unlines
  [
    "Usage: msi-kb-backlit [options...]"
  , ""
  , "Colors follow the following syntax:"
  , "  intensity-color"
  , ""
  , "If you don’t pass any intensity, it will default to a high intensity."
  , ""
  , "Possible intensity:"
  , "  light      very low intensity"
  , "  low        low intensity"
  , "  medium     medium intensity"
  , "  high       high intensity"
  , ""
  , "Possible colors:"
  , "  black"
  , "  red"
  , "  orange"
  , "  yellow"
  , "  green"
  , "  sky"
  , "  blue"
  , "  purple"
  , "  white"
  , ""
  , "For instance, high-red is a very bright red. green is a very bright green."
  ]

setColors :: Mode -> [(Region,Color,Intensity)] -> IO ()
setColors mode regions = do
  hidDev <- vendorProductSerialDevice vendorID productID Nothing
  case hidDev of
    Just dev -> do
      _ <- sendFeatureReport dev . pack $ map fromIntegral [1,2,65,fromEnum mode + 1,0,0,0,236] -- mode
      for_ regions $ \(r,c,i) ->
        sendFeatureReport dev . pack $ map fromIntegral [1,2,66,fromEnum r + 1,fromEnum c,fromEnum i,0,236]
    Nothing -> err "unable to access hardware"

main :: IO ()
main = do
  _ <- System.HID.init

  args <- getArgs
  if null args then
    putStrLn usage
    else do
      let (matched,_,errors) = getOpt Permute options args
          mode = confMode matched
          regions = confRegions matched
          wrong = confWrong matched
      if (not $ null errors)
      then do
        traverse_ err errors
        putStrLn usage
      else do
        if (not $ null wrong)
        then do
          err wrong
        else
          setColors mode regions

      _ <- exit
      pure ()
