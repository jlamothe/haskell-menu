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
            , build
            , execute
            ) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

data Menu a =
  Menu { title :: String
       , items :: Map Char (Item a)
       }

instance Show (Menu a) where
  show menu =
    "\n*** " ++ title menu ++ " ***\n" ++ showItems (items menu) ++ "\n"

showItems :: Map Char (Item a) -> String
showItems =
  Map.foldrWithKey
  (\k x r ->
    k : ") " ++ show x ++ '\n' : r
  ) ""

data Item a =
  Item { description :: String
       , action      :: IO a
       }

instance Show (Item a) where
  show = description

build :: String -> [(Char, Item a)] -> Menu a
build title items =
  Menu { title = title
       , items = Map.fromList items
       }

execute :: Menu a -> IO a
execute menu = do
  hSetBuffering stdout NoBuffering
  putStr $ show menu
  prompt menu

prompt :: Menu a -> IO a
prompt menu = do
  putStr "Please make a selection: "
  sel <- getLine
  case sel of
    (x : _) ->
      case Map.lookup (toUpper x) (items menu) of
        Just item -> action item
        _         -> prompt menu
    _ -> do
      putStrLn "Invalid Selection"
      prompt menu
