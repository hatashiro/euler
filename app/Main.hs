module Main where

import Data.HashMap
import System.Environment (getArgs)

import qualified Problem17
import qualified Problem18
import qualified Problem19
import qualified Problem20
import qualified Problem21
import qualified Problem22
import qualified Problem23
import qualified Problem24
import qualified Problem25
import qualified Problem26
import qualified Problem27
import qualified Problem28
import qualified Problem29
import qualified Problem30
import qualified Problem31
import qualified Problem32
import qualified Problem33
import qualified Problem34
import qualified Problem35
import qualified Problem36
import qualified Problem37
import qualified Problem38
import qualified Problem39
import qualified Problem40
import qualified Problem41
import qualified Problem42
import qualified Problem43
import qualified Problem44
import qualified Problem45
import qualified Problem46
import qualified Problem47
import qualified Problem48
import qualified Problem49
import qualified Problem50
import qualified Problem51
import qualified Problem52
import qualified Problem53
import qualified Problem54
import qualified Problem55
import qualified Problem56
import qualified Problem57
import qualified Problem58
import qualified Problem59
import qualified Problem60
import qualified Problem61
import qualified Problem62
import qualified Problem63
import qualified Problem64
import qualified Problem65
-- IMPORT

problems :: Map Integer (IO ())
problems = fromList
  [ (17, Problem17.main)
  , (18, Problem18.main)
  , (19, Problem19.main)
  , (20, Problem20.main)
  , (21, Problem21.main)
  , (22, Problem22.main)
  , (23, Problem23.main)
  , (24, Problem24.main)
  , (25, Problem25.main)
  , (26, Problem26.main)
  , (27, Problem27.main)
  , (28, Problem28.main)
  , (29, Problem29.main)
  , (30, Problem30.main)
  , (31, Problem31.main)
  , (32, Problem32.main)
  , (33, Problem33.main)
  , (34, Problem34.main)
  , (35, Problem35.main)
  , (36, Problem36.main)
  , (37, Problem37.main)
  , (38, Problem38.main)
  , (39, Problem39.main)
  , (40, Problem40.main)
  , (41, Problem41.main)
  , (42, Problem42.main)
  , (43, Problem43.main)
  , (44, Problem44.main)
  , (45, Problem45.main)
  , (46, Problem46.main)
  , (47, Problem47.main)
  , (48, Problem48.main)
  , (49, Problem49.main)
  , (50, Problem50.main)
  , (51, Problem51.main)
  , (52, Problem52.main)
  , (53, Problem53.main)
  , (54, Problem54.main)
  , (55, Problem55.main)
  , (56, Problem56.main)
  , (57, Problem57.main)
  , (58, Problem58.main)
  , (59, Problem59.main)
  , (60, Problem60.main)
  , (61, Problem61.main)
  , (62, Problem62.main)
  , (63, Problem63.main)
  , (64, Problem64.main)
  , (65, Problem65.main)
  -- MAIN
  ]

main :: IO ()
main = do
  number <- read . (!! 0) <$> getArgs
  problems ! number
