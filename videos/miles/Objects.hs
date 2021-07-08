{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Objects where

import           Codec.Picture.Types
import           Control.Lens
import           Graphics.SvgTree
import           Linear.V2
import           Reanimate
import           Reanimate.Builtin.CirclePlot
import           Reanimate.Builtin.Documentation
import           Reanimate.Scene


data RoundFunction = RoundFunction
  { _roundHeight :: Double
  , _roundDepth  :: Double
  }

data SquareFunction = SquareFunction
  { _squareHeight    :: Double
  , _squareDepth     :: Double
  , _squareNubHeight :: Double
  , _squareNubDepth  :: Double
  }

data ArcPlot = ArcPlot
  { _arcPlotPartial :: Double
  , _arcPlotFn      :: Double -> PixelRGBA8
  , _arcPlotAngle   :: Double
  , _arcPlotCenter  :: Double
  , _arcPlotQuality :: Int
  }

{-
operations:
  Fade in new board
  Add go pieces
  Highlight piece
  Move piece
-}
data GoBoard = GoBoard
  { _goBoardSize :: Int
  , _goBoardWhite :: [Int]
  , _goBoardBlack :: [Int]
  , _goBoardSelected :: [Int]
  }

data GoPiece = GoPiece
  { _goPieceBoardSize :: Int
  , _goPieceLocation :: Int
  , _goPieceColor :: String
  }

-- circlePlot :: Int -- ^ Number of diagonal pixels. Only affects quality, not size.
--            -> (Double -> Double -> PixelRGBA8)
--               -- ^ Angle and radius in radians and percent respectively.
--            -> Tree


makeLenses ''RoundFunction
makeLenses ''SquareFunction
makeLenses ''ArcPlot

makeLenses ''GoBoard
makeLenses ''GoPiece

instance Renderable GoBoard where
  toSVG (GoBoard size white black selected) = withFillOpacity 0 $ mkGroup
    [ mkRect 1 1 ]

instance Renderable RoundFunction where
  toSVG (RoundFunction mouthHeight mouthDepth) =
    mkGroup
    [ withFillOpacity 0 $ mkPath
      [ MoveTo OriginAbsolute [V2 (mouthDepth-1) 0]
      , VerticalTo OriginRelative [-mouthHeight/2]
      , HorizontalTo OriginAbsolute [-mouthX]
      , EllipticalArc OriginRelative
          [(1,1,0,True,True,V2 0 mouthHeight)]
      , HorizontalTo OriginAbsolute [mouthDepth-1]
      , VerticalTo OriginRelative [-mouthHeight/2]
      , EndPath
      ]
    ]
    where
      mouthX = cos (asin (mouthHeight/2))

instance Renderable SquareFunction where
  toSVG (SquareFunction mouthHeight mouthDepth nubHeight nubDepth) =
    mkGroup
    [ withFillOpacity 0 $ mkPath
      [ MoveTo OriginAbsolute [V2 (mouthDepth-1) 0]
      , VerticalTo OriginRelative [-mouthHeight/2]
      , HorizontalTo OriginAbsolute [-1]
      , VerticalTo OriginAbsolute [-1]
      , HorizontalTo OriginAbsolute [1]

      , VerticalTo OriginAbsolute [-nubHeight/2]
      , HorizontalTo OriginAbsolute [1-nubDepth]
      , VerticalTo OriginAbsolute [nubHeight/2]
      , HorizontalTo OriginAbsolute [1]

      , VerticalTo OriginAbsolute [1]
      , HorizontalTo OriginAbsolute [-1]
      , VerticalTo OriginAbsolute [mouthHeight/2]
      , HorizontalTo OriginAbsolute [mouthDepth-1]
      , VerticalTo OriginRelative [-mouthHeight/2]
      , EndPath
      ]
    ]

instance Renderable ArcPlot where
  toSVG plot | plot^.arcPlotPartial == 0 = None
  toSVG plot = mkGroup
      [ withId "arcPlotMask" $
        MaskTree $ defaultSvg
        & maskWidth .~ Num screenWidth
        & maskHeight .~ Num screenHeight
        & maskContent .~
          [ withFillOpacity 1 $
            withStrokeWidth 0 $
            mkGroup
            [ -- Show everything by default
              withFillColor "white" $
              mkCircle 1
              -- Hide center
            , withFillColor "black" $
              mkCircle (plot^.arcPlotCenter)
              -- Hide everything outside of arcPlotAngle
            , withFillColor "black" $ scale 2 $ mkPath
              [ MoveTo OriginAbsolute [V2 0 0]
              , LineTo OriginAbsolute [V2 (cos pAng) (sin pAng)]
              , EllipticalArc OriginAbsolute
                [(1,1,0,True,True,V2 (cos pAng) (-sin pAng))]
              , EndPath ]
              -- Partially hide arc plot
            , withFillColor "black" $ mkPath
              [ MoveTo OriginAbsolute [V2 0 0]
              , LineTo OriginAbsolute [V2 (-1) (0)]
              , EllipticalArc OriginAbsolute
                [(1,1,0,lAng>0,True,V2 (cos lAng) (sin lAng))]
              , EndPath ]
            ]
          ]
          
      , (lowerTransformations $
        circlePlot (plot^.arcPlotQuality) gen)
        & maskRef .~ pure (Ref "arcPlotMask")
      ]
    where
      pAng = (plot^.arcPlotAngle)/2
      lAng = fromToS pAng (-pAng) (plot^.arcPlotPartial)
      gen ang _r =
        (plot^.arcPlotFn) ((ang/pAng+1)/2)
