{-# UnicodeSyntax #-}

Опишем класс и тип данных для работы с аналитической геометрией.

\begin{code}

module AnGeo where

import Data.Semigroup
import Data.Monoid

\end{code}

Тип данных "точка" в трехмерном пространстве

\begin{code}

data Point = Pt {px,py,pz :: Double} deriving (Eq,Read)

instance Show Point where
  show (Pt x y z) = "(" ++ (show x) ++ "; "
        ++ (show y) ++ "; " ++ (show z) ++ ")"
\end{code}

Тип данных "направленный отрезок", заданный двумя точками в трехмерном пространстве

\begin{code}

data OrSeg = OrS {beg,end :: Point} deriving (Read)

instance Show OrSeg where
  show (OrS a b) = "(" ++ (show a) ++ "; "
    ++ (show b) ++ ")"

instance Eq OrSeg where
   u == v =
      ( (px $ end u) - (px $ beg u) )
        ==
      (
        (px $ end v) - (px $ beg v)
      ) &&
      (
        (py $ end u) - (py $ beg u)
      ) ==
      (
        (py $ end v) - (py $ beg v)
      ) &&
      (
        (pz $ end u) - (pz $ beg u)
      ) ==
      (
        (pz $ end v) - (pz $ beg v)
      )

\end{code}

Тип данных "вектор" в 3-мерном пр-ве. Фактически, это направленный отрезок, у которого первая точка совпадает с началом координат.
Но в силу особой роли таких направленных отрезков будем их считать особым типом данных, "векторами".

\begin{code}

data Vec = Vc {vx,vy,vz :: Double} deriving (Eq,Read)

instance Show Vec where
  show (Vc x y z) = "(" ++ (show x) ++ "; "
    ++ (show y) ++ "; " ++ (show z) ++ ")"

fromOrSeg :: OrSeg -> Vec
-- задаем вектор из направленного отрезка вычитанием координат
fromOrSeg (OrS a b)  = Vc
        ((px b) - (px a))
        ((py b) - (py a))
        ((pz b) - (pz a))

toOrSeg :: Vec -> OrSeg
-- задаём направленный отрезок по данному вектору
-- направленный отрезок начинается в точке (0;0;0)
toOrSeg (Vc x y z) = OrS (Pt 0 0 0) (Pt x y z)

fromList :: [Double] -> Vec
-- для удобства: задаём вектор по списку трёх действительных чисел
-- если чисел больше трёх, то выкидываем ошибку!
fromList [x,y,z] = Vc x y z
fromList _ = error "Somth. wrong with arguments!"

fromPoint :: Point -> Vec
-- задаём вектор (радиус-вектор) из начала координат к заданной точке
fromPoint (Pt x y z) = Vc x y z

toPoint :: Vec -> Point
-- по заданному вектору определим точку, куда он "упрётся"
-- т.е. радиус-вектор определяет точку с теми же самыми координатами
toPoint (Vc x y z) = Pt x y z

--toPoint' :: (a, a, a) -> Point
--toPoint (x, y, z) = (Pt x y z)
\end{code}


Далее, определим класс VecAlg (векторной алгебры) над произвольным типом a, который сможет обеспечить векторы
(в нашем пакете это будет пока Vec, но можно в дальнейшем задействовать и другие типы).
В него включим сложение векторов (pls) и вычитание векторов (mns).
Потом используем моноидную операцию (<>) --- это мы можем сделать,
так как для типа (a) мы просим выполнения моноидности. И определим pls как моноидную операцию в нашем классе.

Затем определяем сигнатуры для kprod (умножения числа на вектор), sprod (скалярное произведение), vprod (векторное произведение), mixprod (смешанное произведение).
Смешанное произведение определим через векторное и скалярное, как обычно принято.

\begin{code}
class (Eq a, Monoid a) => VecAlg a where

  pls :: a -> a -> a
  mns :: a -> a -> a

  pls = (<>)

  kprod :: Double -> a -> a
  sprod :: a -> a -> Double
  vprod :: a -> a -> a
  mixprod :: a -> a -> a -> Double

  mixprod a b c = (a `vprod` b) `sprod` c

-- перпендикулярность
  perp :: a -> a -> Bool
  perp a b = (a `sprod` b == 0)

-- коллинераность
  coll :: a -> a -> Bool
  coll u w = ((u × w) == mempty)
--  coll u w = ( abs((norma u)*(norma w)) == (u `sprod` w) )

-- компланарность
  compl :: a -> a -> a -> Bool
  compl a b c = (mixprod a b c == 0)

-- норма
  norma :: a -> Double
  norma a = sqrt (a `sprod` a)

-- синонимы
  (×) :: a -> a -> a
  (×) = vprod
  (·) :: a -> a -> Double
  (·) = sprod
  (•) :: Double -> a -> a
  (•) = kprod
  (–) :: a -> a -> a
-- (–) Unicode En Dash
  (–) = mns
  (┴) :: a -> a -> Bool
  (┴) = perp
  (¦¦) :: a -> a -> Bool
  (¦¦) = coll

-- unit Vectors
  uX :: a
  uY :: a
  uZ :: a
\end{code}

Воплощения классов Semigroup для типа данных Vec, здесь функцию (<>) воплощаем как сложение векторов покоординатно.

Воплощения класса Monoid для типа данных Vec, здесь воплощаем mempty как нулевой вектор.

Воплощения класса VecAlg для типа данных Vec: воплощаем единичные вектора, разность и остальные операции.

\begin{code}
instance Semigroup Vec where
  u <> w = Vc
        ((vx u) + (vx w))
        ((vy u) + (vy w))
        ((vz u) + (vz w))

instance Monoid Vec where
  mempty = Vc 0 0 0

instance VecAlg Vec where

  uX = Vc 1 0 0
  uY = Vc 0 1 0
  uZ = Vc 0 0 1

  u `mns` w = Vc
        ((vx u) - (vx w))
        ((vy u) - (vy w))
        ((vz u) - (vz w))

  k `kprod` u = Vc
        (k*(vx u))
        (k*(vy u))
        (k*(vz u))

  u `sprod` w = ((vx u) * (vx w)) + ((vy u) * (vy w)) +  ((vz u) * (vz w))

  u `vprod` w =  (((vy u)*(vz w) - (vz u)*(vy w)) `kprod` uX) –
                 (((vx u)*(vz w) - (vz u)*(vx w)) `kprod` uY) <>
                 (((vx u)*(vy w) - (vy u)*(vx w)) `kprod` uZ)


\end{code}

Нахождение расстояния между двумя точками в пространстве

\begin{code}
pointPointDistance :: Point -> Point -> Double
pointPointDistance p q = norma ((fromPoint p) – (fromPoint q))
\end{code}


++ Отсюда нахождeние длины вектора
взято у Прокофьева

\begin{code}
vecLength :: Vec -> Double -- forTest
vecLength v = sqrt $ sum $ map (^(2::Int)) [vx v, vy v, vz v]
-- vecLength v = sqrt $ sum $ map (^2) [vx v, vy v, vz v]
\end{code}
