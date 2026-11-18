-- | Задачи к очной сессии.
module Level2 where

import Control.Applicative (ZipList (..))
import Examples qualified as E
import Level1 (Player (..))
import MetaUtils (todo, todoImpl)
import Data.Foldable (maximumBy)


-- 2.1. Точка в многомерном пространстве.
--
-- Самостоятельно напишите инстанс Applicative для PointN.
-- (<*>) соединяет соответствующие координаты, обрезая результат
-- до меньшей длины.
-- Выберите pure так, чтобы для любой точки p выполнялось:
--
--   pure id <*> p == p
--
-- Сначала найдите конечный вход, на котором E.badIdentity из Examples.hs
-- нарушает Identity: результат отличается от исходного списка.
-- Запишите вход литералом в identityCounterexample.
-- Законы Applicative и их имена — в лекции 8, слайд 23.

identityCounterexample :: [Int]
identityCounterexample = [1, 2]

newtype PointN a = PointN { getPointN :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative PointN where
  pure x = PointN $ repeat x
  PointN f <*> PointN a = PointN $ zipWith ($) f a

-- 2.2. Точка с произвольными индексами.
--
-- Координаты индексируются произвольным типом i.
-- Самостоятельно напишите Functor и Applicative для PointI.
-- Операции действуют в каждой координате с одним и тем же индексом.

newtype PointI i a = PointI { at :: i -> a }

pointIExample :: PointI String Int
pointIExample = PointI $ \case
  "x" -> 3
  "y" -> 4
  unknown -> error $ "Unexpected component index: " <> unknown

instance Functor (PointI s) where
  fmap f (PointI a) = PointI $ f . a

instance Applicative (PointI s) where
  pure x = PointI \_ -> x
  PointI f <*> PointI a = PointI \x -> f x (a x)

-- 2.3. Композиция аппликативных функторов.
--
-- Напишите полные инстансы Functor и Applicative для Compose.
-- Функция действует во внутреннем g, эффекты объединяются на обоих уровнях.
-- Укажите необходимые ограничения самостоятельно;
-- deriving здесь не используйте.
--
-- Например (числа имеют тип Int):
--   GHCi> getCompose $ fmap (+1) (Compose [Just 1, Nothing, Just 3])
--   [Just 2, Nothing, Just 4]
--
--   GHCi> getCompose $ liftA2 (+) (Compose [Just 1, Nothing]) (Compose [Just 10, Just 20])
--   [Just 11, Just 21, Nothing, Nothing]

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose a) = Compose $ fmap (fmap f)  a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  Compose f <*> Compose a = Compose $ liftA2 (<*>) f a

-- 2.4. Сравнение бинарных функций.
--
-- Сравните бинарные функции на всех парах входов конечных областей.
-- Используйте обычные списки для перебора всех пар
-- и ZipList для сравнения результатов.
-- Области должны быть малыми: например, Bool или Ordering, не Int.

type Walkable a = (Enum a, Bounded a)

sameBinary :: (Walkable a, Walkable b, Eq c) => (a -> b -> c) -> (a -> b -> c) -> Bool
sameBinary f g =
  let inputs = ZipList $ (,) <$> [minBound..maxBound] <*> [minBound..maxBound]
  in all id $ getZipList $ liftA2 (==) (uncurry f <$> inputs) (uncurry g <$> inputs)
-- Но по мне так проще:
-- sameBinary f g = all id do
--   a <- [minBound..maxBound]
--   b <- [minBound..maxBound]
--   pure $ f a b == g a b

-- 2.5. Разное: лучший игрок.
--
-- Найдите игрока с наибольшим количеством очков.
-- Для пустого списка верните Nothing. Если максимум набрали несколько
-- игроков, верните первого из них в исходном списке.
-- Способ решения выберите сами.

bestPlayer :: [Player] -> Maybe Player
bestPlayer = \case
  [] -> Nothing
  x:xs -> Just $ go x xs where
    go x  = \case
      [] -> x
      y:ys ->
        if playerScore x < playerScore y
          then go y ys
          else go x ys
