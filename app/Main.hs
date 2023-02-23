module Main (main) where

import Options.Applicative
import GraphicsLib.Plot (plotTest)
import GraphicsLib.OpenGL

data ArgOptions = RandomGen
  | LinearAlgebra
  | Plots String
  | MonomerBased
  | Net

argParser :: Parser ArgOptions
argParser =  flag' RandomGen (long "random-gen" <> help "Generate Random")
           <|> flag' LinearAlgebra (long "linear-algebra" <> help "Linear algebra stuff")
           <|> Plots <$> strOption (long "plots" <> help "output path")
           <|> flag' MonomerBased (long "gui" <> help "graphical user interface")
           <|> flag' Net (long "net" <> help "run neural net related functions")


main :: IO ()
main = work =<< execParser opts
  where
    opts = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "HXX - stuff related to haskell and ml and everything that comes with it (hopefully)"
     <> header "some header bruh" )

work :: ArgOptions -> IO ()
work opt = case opt of
  Plots _  -> plotTest
  MonomerBased  -> openGLmain
  _ -> print "not implemented yet"
