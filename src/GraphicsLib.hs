{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GraphicsLib
  ( asciiPlot
  , monomerGUI
  ) where
import           Control.Lens
import           Data.Text (Text, pack)
import           Monomer

data AppModel = AppModel {
  _clickCount :: Int
  , _someValue :: Action
} deriving (Eq, Show)

data Action = Init | Inc | Dec deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppDecrease
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello world"
    , spacer
    , hstack
      [   vstack
            [ label . pack $ "Click count: " <> show (model ^. clickCount)
            , spacer
            , label . pack $ "Some value: " <> show (model ^. someValue)
            ]
        , spacer
        , button "Increase count" AppIncrease
        , spacer
        , button "Decrease count" AppDecrease
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1 & someValue .~ Inc)]
  AppDecrease -> [Model (model & clickCount -~ 1 & someValue .~ Dec)]

monomerGUI :: IO ()
monomerGUI = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Dev test app",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/font.ttf",
      appInitEvent AppInit
      ]
    model = AppModel 0 Init


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
