{-# LANGUAGE OverloadedStrings #-}
module SVG (module SVG, Svg) where

import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Data.String (fromString)
import Types

-- overloads for different stroke widths
renderLine :: String -> String -> Line -> Svg
renderLine colour "0.1" ((x1,y1),(x2,y2)) =
	S.path ! stroke (fromString colour) ! strokeWidth "0.1" ! fill "none" ! (d $ mkPath $ m x1 y1 >> l x2 y2)
renderLine colour "0.05" ((x1,y1),(x2,y2)) =
	S.path ! stroke (fromString colour) ! strokeWidth "0.05" ! fill "none" ! (d $ mkPath $ m x1 y1 >> l x2 y2)
renderLine colour "0.4" ((x1,y1),(x2,y2)) =
	S.path ! stroke (fromString colour) ! strokeWidth "0.4" ! fill "none" ! (d $ mkPath $ m x1 y1 >> l x2 y2)

type Dimensions = (Int,Int,Int,Int) -- xmin ymin xmax ymax

linesToSvg :: FilePath -> Dimensions -> [Svg] -> IO ()
linesToSvg file (xmin,ymin,xmax,ymax) lines = writeFile file $ renderSvg $ docTypeSvg
	! version "1.1"
	! width "1000"
	! height "800"
	! preserveaspectratio "xMinYMin meet"
	! viewbox (fromString $ show xmin ++ " " ++ show ymin ++ " " ++ show xmax ++ " " ++ show ymax) $ do
		sequence_ lines
