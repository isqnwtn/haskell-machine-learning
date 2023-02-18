module Graphics
  ( asciiPlot
  ) where

asciiPlot :: (RealFloat a,Show a,Integral b)
          => (a -> a) -- ^ Function
          -> (a,a)    -- ^ Range
          -> b        -- ^ total steps
          -> IO ()
asciiPlot fn (beg,end) steps = do
  putStrLn $ show beg <> "|" <> show minY <> replicate 20 '-' <> show maxY
  mapM_ putStrLn $ map toAsciiPoints asciiLines
  print end
    where
      inc = (end - beg) / fromIntegral steps
      range = [beg + fromIntegral x * inc | x <- [0..steps]]
      asciiLines = [(x,fn x) | x <- range]
      maxY = maximum $ map snd asciiLines
      minY = minimum $ map snd asciiLines
      toAsciiPoints (x,y) = "|"<> replicate (round $ (y-minY)*20/(maxY-minY)) ' '<>"*"
