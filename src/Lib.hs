-- Noise texture generator
--
-- To the extent possible under law, the author(s) have dedicated all
-- copyright and related and neighboring rights to this software to
-- the public domain worldwide. This software is distributed without
-- any warranty.
--
-- You should have received a copy of the CC0 Public Domain Dedication
-- along with this software. If not, see
-- <http://creativecommons.org/publicdomain/zero/1.0/>.

module Lib where

import Codec.Picture.Tiff
import Codec.Picture.Types
import System.Random.MWC

makeNoise :: FilePath -> Int -> Int -> IO ()
makeNoise path width height = do
  img <- genImage width height
  writeTiff path img

genImage :: Int -> Int -> IO (Image PixelF)
genImage width height = do
  vs <- withSystemRandom . asGenST $ \gen -> uniformVector @_ @Float gen (width*height)
  pure (Image width height vs)
