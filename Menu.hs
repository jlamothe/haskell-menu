module Menu ( Menu (..)
            , Item (..)
            , new
            , addItem
            , doMenu
            ) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

data Menu =
  Menu { title :: String
       , items :: Map Char Item
       }

instance Show Menu where
  show menu =
    "\n*** " ++ title menu ++ " ***\n" ++ showItems (items menu)

showItems :: Map Char Item -> String
showItems =
  Map.foldrWithKey
  (\k x r ->
    k : ") " ++ show x ++ '\n' : r
  ) ""

data Item =
  Item { description :: String
       , action      :: IO ()
       }

instance Show Item where
  show = description

new :: String -> Menu
new title = Menu title Map.empty

addItem :: Menu -> Char -> String -> IO () -> Menu
addItem menu ch desc act =
  menu { items = Map.insert ch (Item desc act) (items menu) }

doMenu :: Menu -> IO ()
doMenu menu = do
  hSetBuffering stdout NoBuffering
  putStr $ show menu
  menuPrompt menu

menuPrompt :: Menu -> IO ()
menuPrompt menu = do
  putStr "Please make a selection: "
  sel <- getLine
  case sel of
    (x : _) ->
      case Map.lookup (toUpper x) (items menu) of
        Just item -> action item
        _         -> menuPrompt menu
    _ -> do
      putStrLn "Invalid Selection"
      menuPrompt menu
