{-# UnicodeSyntax #-}

\section{Прямые в 3-мерном пространстве}

\begin{code}

module Lines where

import AnGeo
import Data.Semigroup
import Data.Monoid

\end{code}


Зададим тип данных "прямая" в соответствии с параметрическим векторным уравнением прямой:
$\vec{r} = \vec{r}_0 + \vec{a}t$.

\begin{code}
data Line = Ln {ro, dir :: Vec} deriving (Read)
\end{code}

Зададим функции-конструкторы прямой линии:

\begin{code}
lineFromPointAndVec :: Point -> Vec -> Line
lineFromPointAndVec p a = Ln (fromPoint p) a

lineFrom2Points :: Point -> Point -> Line
lineFrom2Points p q = lineFromPointAndVec p (fromOrSeg (OrS p q))
\end{code}

(неплохо бы обдумать вырожденные случаи)

Проверка принадлежености точки прямой линии

\begin{code}
pointOnLine :: Point -> Line -> Bool
pointOnLine p l = (fromOrSeg (OrS p q)) ¦¦
  (dir l) where q = toPoint $ ro l
\end{code}

Проверка совпадения двух прямых линий

\begin{code}
instance Eq Line where
  l1 == l2 = ((dir l1) ¦¦ (dir l2)) && 
               ((toPoint $ ro l1) `pointOnLine` l2) && 
               ((toPoint $ ro l2) `pointOnLine` l1)
-- Наверно, проверка ((toPoint $ ro l2) `pointOnLine` l1) уже лишняя
\end{code}

Проверка параллельности двух прямых линий

\begin{code}
lineParall :: Line -> Line -> Bool
lineParall l1 l2 = (dir l1) `coll` (dir l2)
\end{code}

Проверка перпендикулярности двух прямых линий

\begin{code}
linePerp :: Line -> Line -> Bool
linePerp l1 l2 = (dir l1) `perp` (dir l2)
\end{code}

Нахождение угла между прямыми (в градусах бы)...

\begin{code}
lineAngle :: Line -> Line -> Double
lineAngle a b = (acos (abs(dir a · dir b))/(sqrt(x1**2+y1**2+z1**2)*sqrt(x2**2+y2**2+z2**2)))*180/pi where
  x1 = vx $ dir a
  x2 = vx $ dir b
  y1 = vy $ dir a
  y2 = vy $ dir b
  z1 = vz $ dir a
  z2 = vz $ dir b  
\end{code}
lineAngle (Ln (Vc 1 2 3) (Vc 7 9 3)) (Ln (Vc 3 4 5) (Vc 4 2 9))
51.96774039237247

Нахождение расстояния между точкой и прямой в пространстве

\begin{code}
pointToLineDistance :: Point -> Line -> Double
pointToLineDistance pt ln = (abs(vecLength s)) / vecLength (dir ln) where 
  s = (vec `vprod` (dir ln)) where
    vec = fromList [px pt - vx (ro ln), py pt - vy (ro ln), pz pt - vz (ro ln)]
\end{code}
tests:
pointToLineDistance (Pt 1 2 3) (Ln (Vc 3 4 5) (Vc 8 4 5))
0.9952267030562385
pointToLineDistance (Pt 1 2 3) (Ln (Vc 3 4 5) (Vc 23 4 5))
2.1940909479142605

Нахождение расстояния между двумя скрещивающимися прямыми

\begin{code}
skewLinesDistance :: Line -> Line -> Double
skewLinesDistance l1 l2 = abs (mixprod vec1 vec2 vec3) / vecLength (vprod vec2 vec3) where
  vec1 = fromList [vx (ro l2) - vx (ro l1), vy (ro l2) - vy (ro l1), vz (ro l2) - vz (ro l1)]
  vec2 = dir l1
  vec3 = dir l2
\end{code}
skewLinesDistance (Ln (Vc 1 2 3) (Vc 8 3 0)) (Ln (Vc 8 4 6) (Vc 2 10 2))
3.0547477882683953


Красивое отображение прямой линии в виде уравнения
instance Show Line where
  show (Ln ro dir) = "{"++(show ro)++"; "++(show dir)++"}"

\begin{code}
instance Show Line where
  show line = "x = "++(show (vx (ro line)))++" + t * "++(show (vx (dir line)))++"\n"++
              "y = "++(show (vy (ro line)))++" + t * "++(show (vy (dir line)))++"\n"++
              "z = "++(show (vz (ro line)))++" + t * "++(show (vz (dir line)))++"\n"
\end{code}
