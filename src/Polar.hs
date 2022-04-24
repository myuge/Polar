{- |
Copyright: (c) 2021 myuge
SPDX-License-Identifier: GPL-3.0-only
Maintainer: myuge <myuge@acm.org>

Exprerimentation on polar transformantions
-}

module Polar
    ( projectName,
      getContext,
      Lines,
      Plane,
      Point2D
    ) where

projectName :: String
projectName = "polar"

data PolarEnv = PolarEnv {
      -- central point: (x0, y0, z0)
      x0 :: Double,
      y0 :: Double,
      z0 :: Double,
      -- radius of sphere: r
      r :: Double,
      -- grid points in screen (m*a, n*a, 0), on x-y plane.
      -- edge size of grid: a
      a :: Double,
      -- number of lines per plane
      times :: Int
    }

type Point3D = (Double,Double,Double)

type Grid = (Int,Int)
    
-- a point in space: (p1, p2, p3)
-- Grid points.
x :: Point3D->Grid->PolarEnv->Double
x (p1,p2,p3) (m,n) env = ((x0 env)*(r env)^2 - ((x0 env)*p1 + (z0 env)*p3)*(a env)*(fromIntegral m)
                                      - (x0 env)*p2*(a env)*(fromIntegral n))
                         /((r env)^2 - p1*(a env)*(fromIntegral m) - p2*(a env)*(fromIntegral n) - p3*(z0 env))
y :: Point3D->Grid->PolarEnv->Double
y (p1,p2,p3) (m,n) env = ((y0 env)*(r env)^2 - (y0 env)*p1*(a env)*(fromIntegral m)
                                      - ((y0 env)*p2 + (z0 env)*p3)*(a env)*(fromIntegral n))
                         /((r env)^2 - p1*(a env)*(fromIntegral m) - p2*(a env)*(fromIntegral n) - p3*(z0 env))

-- x p1 p2 p3 m n =  ((-2.0)*p3*m)/(2 - p1*m - p2*n - p3*(-2.0))
-- y p1 p2 p3 m n = -((-2.0)*p3*n)/(2 - p1*m - p2*n - p3*(-2.0))
        
type Point2D = (Double,Double)

type Range = (Double,Double)

{-
 get intersection of a line (determined by two points p1, p2)
 and a view port (w, h) of center (0,0).
 if p1==p2 (so don't determine a line) or
 the line doesn't intersect with the view port, Nothing is returned.
-}
intersection :: Point2D->Point2D->Double->Double->Maybe(Point2D,Point2D)
intersection p1 p2 w h =
  let
    x1 = fst p1
    y1 = snd p1
    x2 = fst p2
    y2 = snd p2
    wmax = w/2.0
    wmin = -wmax
    hmax = h/2.0
    hmin = -hmax
    wrange = (wmin,wmax)
    hrange = (hmin,hmax)
  in
  if x1 == x2 then
    if y1 == y2 then Nothing
    else if inRange x1 wrange then Just ((x1,hmin),(x1,hmax))
    else Nothing
  else if y1 == y2 then 
    if inRange y1 hrange then Just ((wmin,y1),(wmax,y1))
    else Nothing
  else
    let
      -- equation of line: (y1-y2)x-(x1-x2)y+(x1y2-x2y1)=0
      dx = x1 - x2
      dy = y1 - y2
      det = x1*y2 - x2*y1
      xu = (dx*hmax - det)/dy 
      xl = (dx*hmin - det)/dy 
      yu = (dy*wmax + det)/dx 
      yl = (dy*wmin + det)/dx
    in
    if inRange xu wrange then
      if inRange yl hrange then Just ((xu,hmax),(wmin,yl))
      else if inRange xl wrange then Just ((xu,hmax),(xl,hmin))
      else if inRange yu hrange then Just ((xu,hmax),(wmax,yu))
      else Nothing
    else if inRange yl hrange then
      if inRange xl wrange then Just ((wmin,yl),(xl,hmin))
      else if inRange yu hrange then Just ((wmin,yl),(wmax,yu))
      else Nothing
    else if inRange xl wrange then
      if inRange yu hrange then Just ((xl,hmin),(wmax,yu))
      else Nothing
    else Nothing

inRange :: Double->Range->Bool
inRange x range = x >= (fst range) && x <= (snd range)

type Lines = [(Point2D, Point2D)]

type Plane = (Lines,Lines)

getContext :: Double->Double->Double->Double->Double->Int->[Plane]
getContext x0 y0 z0 r a times = getPolarPlanes aShape (PolarEnv x0 y0 z0 r a times)

type Shape = [Point3D]

aShape :: Shape    
{-
aShape = [(-0.6000000000000014,9.932256839900757e-15,0.4999999999999975),
          (0.3119054277195731,-0.2718093557869878,0.014774031693273055),
          (0.0381948246312592,0.03328480287541778,0.9852259683067266),
          (0.4242640687119366,0.4242640687119225,0.4999999999999975),
          (-0.02835220436247464,0.4127486816963267,0.014774031693273055),
          (-0.27180935578698784,-0.3119054277195731,0.014774031693273055),
          (0.4415639839783377,-0.39947686317327447,0.5074994375506179)
         ]
-}
aShape = [(-0.6000000000000014,0.4999999999999975,9.932256839900757e-15),
          (0.3119054277195731,0.014774031693273055,-0.2718093557869878),
          (0.0381948246312592,0.9852259683067266,0.03328480287541778),
          (0.4242640687119366,0.4999999999999975,0.4242640687119225),
          (-0.02835220436247464,0.014774031693273055,0.4127486816963267),
          (-0.27180935578698784,0.014774031693273055,-0.3119054277195731),
          (0.4415639839783377,0.5074994375506179,-0.39947686317327447)
         ]

getPolarPlanes :: Shape -> PolarEnv -> [Plane]
getPolarPlanes ps env = map (getPolarPlane env) ps

getPolarPlane :: PolarEnv -> Point3D -> Plane
getPolarPlane env p = ((getVLines p 0 1 env) ++ (getVLines p (-1) (-1) env),
                       (getHLines p 0 1 env) ++ (getHLines p (-1) (-1) env))

getVLines :: Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getVLines p n delta env =
    getVLines2 (intersection (x1,y1) (x2,y2) 1000 1000) p n delta env
        where
          x1 = x p (n,0) env
          y1 = y p (n,0) env
          x2 = x p (n,1) env
          y2 = y p (n,1) env

-- TODO: the case that the distance between ajacent lines are under some limit (effectively zero)
-- computation must terminate.

getVLines2 :: Maybe(Point2D,Point2D)->Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getVLines2 (Just vline) p n delta env
--    = vline : (getVLines2 (intersection (x1,y1) (x2,y2) 1000 1000) p nn delta env)
    | (n >= -(times env) && n <= (times env)) = vline : (getVLines2 (intersection (x1,y1) (x2,y2) 1000 1000) p nn delta env)
    | otherwise = []
    where
      nn = n + delta
      x1 = x p (nn,0) env
      y1 = y p (nn,0) env
      x2 = x p (nn,1) env
      y2 = y p (nn,1) env
getVLines2 Nothing _ _ _ _ = []

getHLines :: Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getHLines p n delta env =
    getHLines2 (intersection (x1,y1) (x2,y2) 1000 1000) p n delta env
        where
          x1 = x p (0,n) env
          y1 = y p (0,n) env
          x2 = x p (1,n) env
          y2 = y p (1,n) env

getHLines2 :: Maybe(Point2D,Point2D)->Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getHLines2 (Just vline) p n delta env
--    = vline : (getHLines2 (intersection (x1,y1) (x2,y2) 1000 1000) p nn delta env)
    | (n >= -(times env) && n <= (times env)) = vline : (getHLines2 (intersection (x1,y1) (x2,y2) 1000 1000) p nn delta env)
    | otherwise = []
    where
      nn = n + delta
      x1 = x p (0,nn) env
      y1 = y p (0,nn) env
      x2 = x p (1,nn) env
      y2 = y p (1,nn) env
getHLines2 Nothing _ _ _ _ = []
