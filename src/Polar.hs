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
      Point2D,
      Point3D,
      Shape
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
pointToGridPointOn3DPlane :: Point3D -> Grid -> PolarEnv -> Maybe(Point3D)
pointToGridPointOn3DPlane (p1,p2,p3) (m,n) env
    | p3 == 0   = Nothing
    | otherwise = Just (x, y, z)
                where
                  gx = (a env)*(fromIntegral m)
                  gy = (a env)*(fromIntegral n)
                  x = gx
                  y = gy
                  z = ((r env)^2 - p1*gx  - p2*gy)/p3
        
type Point2D = (Double,Double)

point3DToScreen2D :: Point3D->Grid->PolarEnv->Maybe(Point2D)
point3DToScreen2D p g env = point3DToScreen2D_sub (pointToGridPointOn3DPlane p g env) env

point3DToScreen2D_sub :: Maybe(Point3D) -> PolarEnv -> Maybe(Point2D)
point3DToScreen2D_sub Nothing _       = Nothing
point3DToScreen2D_sub (Just(x,y,z)) env = point3DToScreen (x,y,z) env

-- a point in space: (p1, p2, p3)
-- Grid points.
point3DToScreen :: Point3D -> PolarEnv -> Maybe(Point2D)
point3DToScreen (x,y,z) env
    | z == (z0 env) = Nothing
    | z < 0         = Nothing
    | otherwise     = Just (((x0 env)*z - (z0 env)*x)/(z - (z0 env)), ((y0 env)*z - (z0 env)*y)/(z - (z0 env)))
            
-- x p1 p2 p3 m n =  ((-2.0)*p3*m)/(2 - p1*m - p2*n - p3*(-2.0))
-- y p1 p2 p3 m n = -((-2.0)*p3*n)/(2 - p1*m - p2*n - p3*(-2.0))

type Range = (Double,Double)

{-
 get intersection of a line (determined by two points p1, p2)
 and a view port (w, h) of center (0,0).
 if p1==p2 (so don't determine a line) or
 the line doesn't intersect with the view port, Nothing is returned.
-}
intersection :: Maybe(Point2D)->Maybe(Point2D)->Double->Double->Maybe(Point2D,Point2D)
intersection (Just p1) (Just p2) w h =
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
intersection Nothing _ _ _ = Nothing
intersection _ Nothing _ _ = Nothing

inRange :: Double->Range->Bool
inRange x range = x >= (fst range) && x <= (snd range)

type Lines = [(Point2D, Point2D)]

type Plane = (Lines,Lines)

getContext :: Double->Double->Double->Double->Double->Int->Shape->[Plane]
getContext x0 y0 z0 r a times shape = getPolarPlanes shape (PolarEnv x0 y0 z0 r a times)

type Shape = [Point3D]

-- for debug
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
aShape = [
-- (0,0,0.5)
   (-0.6000000000000014, 0.4999999999999975,   9.932256839900757e-15), -- indigo
   ( 0.3119054277195731, 0.014774031693273055,-0.2718093557869878),    -- blue
   ( 0.0381948246312592, 0.9852259683067266,   0.03328480287541778),   -- green
   ( 0.4242640687119366, 0.4999999999999975,   0.4242640687119225),    -- yellow
   (-0.02835220436247464,0.014774031693273055, 0.4127486816963267),    -- orange
   (-0.27180935578698784,0.014774031693273055,-0.3119054277195731),    -- red
   ( 0.4415639839783377, 0.5074994375506179,  -0.39947686317327447)    -- violet
  ]

getPolarPlanes :: Shape -> PolarEnv -> [Plane]
getPolarPlanes ps env = map (getPolarPlane env) ps

getPolarPlane :: PolarEnv -> Point3D -> Plane
getPolarPlane env p = ((getVLines p 0 1 env) ++ (getVLines p (-1) (-1) env),
                       (getHLines p 0 1 env) ++ (getHLines p (-1) (-1) env))

getVLines :: Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getVLines p n delta env =
    getVLines2 (intersection (point3DToScreen2D p (n,0) env) (point3DToScreen2D p (n,1) env) 1000 1000) p n delta env

-- TODO: the case that the distance between ajacent lines are under some limit (effectively zero)
-- computation must terminate.

getVLines2 :: Maybe(Point2D,Point2D)->Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getVLines2 (Just vline) p n delta env
    | (n >= -(times env) && n <= (times env)) = vline : (getVLines2 (intersection (point3DToScreen2D p (nn,0) env) (point3DToScreen2D p (nn,1) env) 1000 1000) p nn delta env)
    | otherwise = []
    where
      nn = n + delta
getVLines2 Nothing p n delta env
    | (n >= -(times env) && n <= (times env)) = (getVLines2 (intersection (point3DToScreen2D p (nn,0) env) (point3DToScreen2D p (nn,1) env) 1000 1000) p nn delta env)
    | otherwise = []
    where
      nn = n + delta

getHLines :: Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getHLines p n delta env =
    getHLines2 (intersection (point3DToScreen2D p (0,n) env) (point3DToScreen2D p (1,n) env) 1000 1000) p n delta env

getHLines2 :: Maybe(Point2D,Point2D)->Point3D->Int->Int->PolarEnv->[(Point2D, Point2D)]
getHLines2 (Just vline) p n delta env
    | (n >= -(times env) && n <= (times env)) = vline : (getHLines2 (intersection (point3DToScreen2D p (0,nn) env) (point3DToScreen2D p (1,nn) env) 1000 1000) p nn delta env)
    | otherwise = []
    where
      nn = n + delta
getHLines2 Nothing p n delta env
    | (n >= -(times env) && n <= (times env)) = (getHLines2 (intersection (point3DToScreen2D p (0,nn) env) (point3DToScreen2D p (1,nn) env) 1000 1000) p nn delta env)
    | otherwise = []
    where
      nn = n + delta
