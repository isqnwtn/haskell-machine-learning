module Main (main) where

import Options.Applicative

data ArgOptions = ArgOptions
  { quiet      :: Bool
  , debugLevel :: Int
  , programType :: ProgramType
  }

data ProgramType = RandomGen | LinearAlgebra deriving (Show)

optargs :: Parser ArgOptions
optargs = ArgOptions
      <$> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "debug"
         <> help "How extensively to debug"
         <> showDefault
         <> value 0
         <> metavar "INT" )
      <*> (   flag' RandomGen (long "random-gen" <> help "Generate Random")
          <|> flag' LinearAlgebra (long "linear-algebra" <> help "Linear algebra stuff")
          )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (optargs <**> helper)
      ( fullDesc
     <> progDesc "HXX - stuff related to haskell and ml and everything that comes with it (hopefully)"
     <> header "some header bruh" )

greet :: ArgOptions -> IO ()
greet opt = do
  putStrLn $ "Requested Method:" <> show (programType opt)
