-- Haskell Menu

-- Copyright (C) 2014, 2016 Jonathan Lamothe <jlamothe1980@gmail.com>

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

{-|
Module      : System.IO.Menu
Description : A simple menu system
License     : LGPL-3
Maintainer  : jlamothe1980@gmail.com
-}

module System.IO.Menu ( Menu (..)
                      , Item (..)
                      , build
                      , execute
                      , addItem
                      ) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

-- | Defines a 'Menu'
data Menu a =
  Menu { title :: String
       -- ^ The menu title
       , items :: Map Char (Item a)
       -- ^ The items in the menu
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

-- | Defines a 'Menu' item
data Item a =
  Item { description :: String
       -- ^ The description
       , action      :: IO a
       }

instance Show (Item a) where
  show = description

-- | Builds a 'Menu' from a list.
build
  :: String
  -- ^ The menu title
  -> [(Char, Item a)]
  -- ^ A list of tuples containing a character (must be upper case)
  -- and its corresponding 'Item'
  -> Menu a
build title items =
  Menu { title = title
       , items = Map.fromList items
       }

-- | Prompts the user to select an option from a 'Menu' and returns
-- the resulting value
execute
  :: Menu a
  -- ^ The 'Menu'
  -> IO a
execute menu = do
  putStr $ show menu
  prompt menu

-- | Adds an 'Item' to a 'Menu', returning the resulting 'Menu'
addItem
  :: Char
  -- ^ The character (must be upper case) of the 'Item' to be added to
  -- the menu
  -> Item a
  -- ^ The 'Item' to be added
  -> Menu a
  -- ^ The 'Menu' being modified
  -> Menu a
addItem ch item menu =
  menu { items = Map.insert ch item $ items menu }

prompt :: Menu a -> IO a
prompt menu = do
  inBufMode <- hGetBuffering stdin
  outBufMode <- hGetBuffering stdout
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr "Please make a selection: "
  sel <- getChar
  putChar '\n'
  hSetBuffering stdin inBufMode
  hSetBuffering stdout outBufMode
  case Map.lookup (toUpper sel) (items menu) of
    Just item -> action item
    _         -> do
      putStrLn "Invalid selection"
      prompt menu
