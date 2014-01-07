module Menu ( Menu (..)
            , Item (..)
            , doMenu
            ) where

import Data.Map (Map)
import qualified Data.Map as Map

data Menu =
  Menu { title :: String
       , menuItems :: Map Char Item
       }
  
instance Show Menu where
  show menu =
    "\n*** " ++ title menu ++ " ***\n" ++ showItems (menuItems menu)

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

doMenu :: Menu -> IO ()
doMenu = undefined
