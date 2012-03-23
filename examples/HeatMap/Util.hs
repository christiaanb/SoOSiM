module HeatMap.Util where

dimTrans w h = f
  where
    f x (xOffset,yOffset) | x' >= w         = -1
                          | x' < 0          = -1
                          | x + y' >= w * h = -1
                          | otherwise       = x + xOffset + y'
      where
        y' = yOffset * w
        x' = (x `mod` w) + xOffset
