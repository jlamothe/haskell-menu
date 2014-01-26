-- Haskell Menu

-- Copyright (C) 2014 Jonathan Lamothe <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public
-- License along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

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
