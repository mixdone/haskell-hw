{-# UnicodeSyntax #-}

\section{Прямые и плоскости в 3-мерном пространстве}

\begin{code}

module LinesPlanes where

import AnGeo
import Lines
import Data.Semigroup
import Data.Monoid

\end{code}


Зададим тип данных "плоскость", задаваемый скалярным произведением:
$$
(\vec{r}_0 - \vec{r}) \cdot \vec{n} = 0,
$$
т.е. описываем точки плоскости в которые приходит радиус-вектор $\vec{r}$ с помощью радиус-вектора начальной точки $\vec{r}_0$ и нормали $\vec{n}$.

\begin{code}
data Plane = Pl {mo, normal :: Vec} deriving (Read)
\end{code}

Зададим тип данных "каноническое уравнение плоскости" в соответствии с каноническим уравнением плоскости:
$$
Ax + By + Cz + D = 0.
$$

\begin{code}
data CPlane = CPl {aa,bb,cc,dd :: Double} deriving (Read)
\end{code}

Зададим функцию нахождения нормали для плоскости, заданной в канонической форме

\begin{code}
normalForCPlane :: CPlane -> Vec
normalForCPlane (CPl a b c _) = Vc a b c
-- normalForCPlane (CPl a b c d) = Vc a b c
\end{code}

Зададим функции-конструкторы плоскости:

\begin{code}
planeFromPointAndVec :: Point -> Vec -> Plane
planeFromPointAndVec p u = Pl (fromPoint p) u
\end{code}

(неплохо бы обдумать вырожденные случаи)

\begin{code}
planeFrom3Points :: Point -> Point -> Point -> CPlane
planeFrom3Points p1 p2 p3 | p1==p2 && p2==p3 = planeFrom3Points p1 (Pt ((px p2)+1) (py p2) (pz p2)) (Pt (px p3) ((py p3)+2) (pz p3)) --make plane with a single dot
                          | p1==p2 || p2==p3 = planeFrom3Points p1 (Pt ((px p2)+1) (py p2) (pz p2)) p3                               --make plane with a line
                          | p1==p3           = planeFrom3Points p1 p2 (Pt (px p3) ((py p3)+2) (pz p3))                               --make plane with a line
                          | otherwise        = CPl a b c d where                                                                     --make plane with 3 dots
                            a = (py p2 - py p1)*(pz p3 - pz p1) - (py p3 - py p1)*(pz p2 - pz p1)
                            b = (px p3 - px p1)*(pz p2 - pz p1) - (px p2 - px p1)*(pz p3 - pz p1)
                            c = (px p2 - px p1)*(py p3 - py p1) - (py p2 - py p1)*(px p3 - px p1)
                            d = (-a) * (px p1) - b * (py p1) - c * (pz p1)
\end{code}
-> planeFrom3Points (Pt (-1) 3 (-5)) (Pt 2 (-1) 0) (Pt 0 (-4) 7)
->> -13.0x + -31.0y + -17.0z + -5.0
-> planeFrom3Points (Pt (-1) 3 (-5)) (Pt 0 (-4) 7) (Pt (-1) 3 (-5))
->> -24.0x + 0.0y + 2.0z + -14.0


\begin{code}
planeFrom2Lines :: Line -> Line -> CPlane
planeFrom2Lines l1 l2 | l1==l2       = planeFrom3Points p1 p2 p3
                      | otherwise    = planeFrom3Points p1 p2 p4 where
                        p1 = toPoint (dir l1)
                        p2 = toPoint (pls (ro l1) (dir l1))
                        p3 = toPoint (Vc ((vx (dir l2)) + 1)  (vy (dir l2)) (vz (dir l2)))
                        p4 = toPoint (dir l2)
\end{code}

Преобразование типов плоскостей:

\begin{code}
planeToCPlane :: Plane -> CPlane
planeToCPlane pl = CPl (vx (normal pl)) (vy (normal pl)) (vz (normal pl)) ((-1)*(sprod (mo pl) (normal pl)))
\end{code}

\begin{code}
cplaneToPlane :: CPlane -> Plane
cplaneToPlane (CPl a b c 0) = (Pl (Vc 0 0 0) (Vc a b c))
cplaneToPlane (CPl a b c d) | a /= 0 = (Pl (Vc ((-d) / a) 0 0) (Vc a b c))
                            | b /= 0 = (Pl (Vc 0 ((-d) / b) 0) (Vc a b c))
                            | c /= 0 = (Pl (Vc 0 0 ((-d) / c)) (Vc a b c))
                            | otherwise = error "no equation"


\end{code}

Красивое отображение канонической плоскости в виде уравнения:

\begin{code}
instance Show CPlane where
  show cplane = "("++(show (aa cplane))++")x + ("++(show (bb cplane))++")y + ("++(show (cc cplane))++")z + ("++(show (dd cplane))++")"
\end{code}

Проверка принадлежености точки плоскости (в обеих формах)

\begin{code}
pointOnPlane :: Point -> Plane -> Bool
pointOnPlane point plane = pointOnCPlane point (planeToCPlane plane)

pointOnCPlane :: Point -> CPlane -> Bool
pointOnCPlane (Pt x y z) (CPl a b c d)
              | a*x + b*y + c*z + d == 0 = True
              | otherwise                = False
\end{code}

Проверка принадлежености прямой плоскости

\begin{code}
lineOnPlane:: Line -> Plane -> Bool
lineOnPlane (Ln t1 t2) plane | (pointOnPlane (toPoint t1) plane) &&
                               (pointOnPlane (toPoint t2) plane) = True
                             | otherwise                         = False
lineOnCPlane :: Line -> CPlane -> Bool
lineOnCPlane (Ln t1 t2) plane | (pointOnCPlane (toPoint t1) plane) &&
                                (pointOnCPlane (toPoint t2) plane) = True
                              | otherwise                          = False
\end{code}

Проверка совпадения двух плоскостей

\begin{code}
instance Eq Plane where
  first == second =
    (planeToCPlane first) == (planeToCPlane second)
instance Eq CPlane where
  (CPl a1 b1 c1 d1) == (CPl a2 b2 c2 d2) =
    (
    a1 == a2
    ) &&
    (
    b1 == b2
    ) &&
    (
    c1 == c2
    ) &&
    (
    d1 == d2
    )

\end{code}

Проверка параллельности двух плоскостей

\begin{code}
planeParall :: Plane -> Plane -> Bool
planeParall plane1 plane2 = (normal plane1) ¦¦ (normal plane2)

cplaneParall :: CPlane -> CPlane -> Bool
cplaneParall (CPl a1 b1 c1 _) (CPl a2 b2 c2 _)
             | ((a1 / a2) == (b1 / b2)) &&
               ((b1 / b2) == (c1 / c2)) = True
             | otherwise                = False
\end{code}

Проверка перпедикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp p1 p2 = (normal p1) `perp` (normal p2)

cplanePerp :: CPlane -> CPlane -> Bool
cplanePerp (CPl a1 b1 c1 _) (CPl a2 b2 c2 _)
           | a1*a2 + b1*b2 + c1*c2 == 0 = True
           | otherwise                  = False
\end{code}


Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall line plane = (dir line) ┴ (normal plane)

lineAndCPlaneParall :: Line -> CPlane -> Bool
lineAndCPlaneParall line (CPl a b c _) = (dir line) ┴ (Vc a b c)
\end{code}

Проверка перпедикулярности прямой и плоскости

\begin{code}
lineAndPlanePerp :: Line -> Plane -> Bool
lineAndPlanePerp line plane = (dir line) ¦¦ (normal plane)

lineCPlanePerp :: Line -> CPlane -> Bool
lineCPlanePerp line (CPl a b c _) =  (dir line) ¦¦ (Vc a b c)
\end{code}

Нахождение угла между плоскостями (в градусах бы)...

\begin{code}

radToDeg:: Double -> Double
radToDeg angle = (angle * 180) / pi

planeAngle :: Plane -> Plane -> Double
planeAngle (Pl _ (Vc a1 b1 c1)) (Pl _ (Vc a2 b2 c2)) =  radToDeg $ acos (up / down) where
  up   = a1*a2 + b1*b2 + c1*c2
  down = (sqrt (a1^2 + b1^2 + c1^2)) * (sqrt (a2^2 + b2^2 + c2^2))

cplaneAngle :: CPlane -> CPlane  -> Double
cplaneAngle (CPl a1 b1 c1 _) (CPl a2 b2 c2 _) = radToDeg $ acos (up / down) where
  up   = a1*a2 + b1*b2 + c1*c2
  down = (sqrt (a1^2 + b1^2 + c1^2)) * (sqrt (a2^2 + b2^2 + c2^2))
\end{code}

Нахождение угла между прямой и плоскостью (в градусах бы)...

\begin{code}
lineAndPlaneAngle :: Line -> Plane -> Double
lineAndPlaneAngle (Ln (Vc x1 y1 z1) (Vc x2 y2 z2)) (Pl _ (Vc a b c)) = asin (up / down) where
  up = a * (x2 - x1) + b * (y2 - y1) + c * (z2 - z1)
  down = pl * ln
  pl = sqrt (a^2 + b^2 + c^2)
  ln = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

lineAndCPlaneAngle :: Line -> CPlane -> Double
lineAndCPlaneAngle (Ln (Vc x1 y1 z1) (Vc x2 y2 z2)) (CPl a b c _) = asin (up / down) where
  up = a * (x2 - x1) + b * (y2 - y1) + c * (z2 - z1)
  down = pl * ln
  pl = sqrt (a^2 + b^2 + c^2)
  ln = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
\end{code}

Нахождение расстояния между точкой и плоскостью

\begin{code}
pointToPLaneDistance :: Point -> Plane -> Double
pointToPLaneDistance point plane = pointToCPLaneDistance point (planeToCPlane plane)

pointToCPLaneDistance :: Point -> CPlane -> Double
pointToCPLaneDistance (Pt x y z) (CPl a b c d) = up / bottom where
  up      = x*a + y*b + z*c + d
  bottom  = sqrt (a^2 + b^2 + c^2)
\end{code}

Нахождение линии пересечения двух плоскостей, заданных уравнением...

\begin{code}

solve :: CPlane -> CPlane -> Vec
solve (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = (Vc 0 y z) where
  y = (c1*d2 - d1*c2) / det
  z = (d1*b2 - b1*d2) / det
  det = b1*c2 - c1*b2

sl :: CPlane -> CPlane -> Vec
sl (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = (Vc x 0 z) where
  x = (d2*c1 - d1*c2) / det
  z = (d1*a2 - d2*a1) / det
  det = a1*c2 - c1*a2

lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes p1 p2 = lineIntersectionOf2CPlanes (planeToCPlane p1) (planeToCPlane p2)

lineIntersectionOf2CPlanes :: CPlane -> CPlane -> Line
lineIntersectionOf2CPlanes (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = (Ln v1 v2) where
  v1 = (Vc a1 b1 c1) × (Vc a2 b2 c2)
  v2 | (v1 ┴ (Vc 0 1 0)) &&
       (v1 ┴ (Vc 0 0 1)) = sl (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2)
     | otherwise = solve (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2)


\end{code}
