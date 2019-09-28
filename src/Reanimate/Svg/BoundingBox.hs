{-# LANGUAGE LambdaCase #-}
module Reanimate.Svg.BoundingBox where

import           Control.Arrow
import           Control.Lens                 ((^.))
import           Data.List
import           Graphics.SvgTree             hiding (height, line, path, use,
                                               width)
import           Linear.V2                    hiding (angle)
import           Linear.Vector
import           Reanimate.Constants
import           Reanimate.Svg.LineCommand
import qualified Reanimate.Transform          as Transform
-- import qualified Geom2D.CubicBezier           as Bezier

-- (x,y,w,h)
boundingBox :: Tree -> (Double, Double, Double, Double)
boundingBox t =
    case svgBoundingPoints t of
      [] -> (0,0,0,0)
      (V2 x y:rest) ->
        let (minx, miny, maxx, maxy) = foldl' worker (x, y, x, y) rest
        in (minx, miny, maxx-minx, maxy-miny)
  where
    worker (minx, miny, maxx, maxy) (V2 x y) =
      (min minx x, min miny y, max maxx x, max maxy y)

linePoints :: [LineCommand] -> [RPoint]
linePoints = worker zero
  where
    worker _from [] = []
    worker from (x:xs) =
      case x of
        LineMove to     -> worker to xs
        -- LineDraw to     -> from:to:worker to xs
        -- FIXME: Use approximation from Geom2D.Bezier
        LineBezier ctrl -> -- approximation
          [ last (partial_bezier_points (from:ctrl) 0 (recip chunks*i)) | i <- [0..chunks]] ++
          worker (last ctrl) xs
        LineEnd p -> p : worker p xs
    chunks = 10

svgBoundingPoints :: Tree -> [RPoint]
svgBoundingPoints t = map (Transform.transformPoint m) $
    case t of
      None            -> []
      UseTree{}       -> []
      GroupTree g     -> concatMap svgBoundingPoints (g^.groupChildren)
      SymbolTree (Symbol g) -> concatMap svgBoundingPoints (g^.groupChildren)
      FilterTree{}    -> []
      DefinitionTree{} -> []
      PathTree p      -> linePoints $ toLineCommands (p^.pathDefinition)
      CircleTree{}    -> error "Bounding box: CircleTree"
      PolyLineTree{}  -> error "Bounding box: PolyLineTree"
      EllipseTree{}   -> error "Bounding box: EllipseTree"
      LineTree line   -> map pointToRPoint [line^.linePoint1, line^.linePoint2]
      RectangleTree rect ->
        case pointToRPoint (rect^.rectUpperLeftCorner) of
          V2 x y -> [V2 x y] ++
            case mapTuple (fmap $ toUserUnit defaultDPI) (rect^.rectWidth, rect^.rectHeight) of
              (Just (Num w), Just (Num h)) -> [V2 (x+w) (y+h)]
              _                            -> []
      TextTree{}      -> []
      ImageTree img   ->
        case (img^.imageCornerUpperLeft, img^.imageWidth, img^.imageHeight) of
          ((Num x, Num y), Num w, Num h) ->
            [V2 x y, V2 (x+w) (y+h)]
          _ -> []
      MeshGradientTree{} -> []
      _ -> []
  where
    m = Transform.mkMatrix (t^.transform)
    mapTuple f = f *** f
    pointToRPoint p =
      case mapTuple (toUserUnit defaultDPI) p of
        (Num x, Num y) -> (V2 x y)
        _ -> error "Reanimate.Svg.svgBoundingPoints: Unrecognized number format."
