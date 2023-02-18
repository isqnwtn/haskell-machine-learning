module Main (main) where

import Options.Applicative
import Graphics (asciiPlot)

data ArgOptions = ArgOptions
  { quiet      :: Bool
  , debugLevel :: Int
  , programType :: ProgramType
  }

data ProgramType = RandomGen
  | LinearAlgebra
  | Plots
  deriving (Show)

optargs :: Parser ArgOptions
optargs = ArgOptions
      <$> switch
          ( long "interactive"
         <> short 'i'
         <> help "Interactive mode" )
      <*> option auto
          ( long "debug"
         <> help "How extensively to debug"
         <> showDefault
         <> value 0
         <> metavar "INT" )
      <*> (   flag' RandomGen (long "random-gen" <> help "Generate Random")
          <|> flag' LinearAlgebra (long "linear-algebra" <> help "Linear algebra stuff")
          <|> flag' Plots (long "plots" <> help "graphical analysis")
          )

main :: IO ()
main = work =<< execParser opts
  where
    opts = info (optargs <**> helper)
      ( fullDesc
     <> progDesc "HXX - stuff related to haskell and ml and everything that comes with it (hopefully)"
     <> header "some header bruh" )

work :: ArgOptions -> IO ()
work opt = case programType opt of
  Plots -> asciiPlot sin (0,6.28) 30
  _ -> print "not implemented yet"
