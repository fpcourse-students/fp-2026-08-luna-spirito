-- В задаче 3.3 явно используем join; оператор (=<<) будет в следующей теме.
{- HLINT ignore "Use =<<" -}

-- | Челленджи; не входят в зачёт.
module Level3 where

import Console (MyIO, readInt, readIntMaybe, writeInt)
import Control.Monad (join)
import Data.Kind (Type)
import Level2 (Compose (..))
import MetaUtils (todo, todoImpl)


-- 3.1. Моноидальные функторы.
--
-- Моноидальный интерфейс эквивалентен Applicative.
-- Напишите инстансы для Maybe, ((->) e) и ((,) e).
-- unit не производит эффектов; (*&*) объединяет эффекты слева направо.

class Functor f => Monoidal (f :: Type -> Type) where
  unit :: f ()

  (*&*) :: f a -> f b -> f (a, b)

infixl 4 *&*

instance Monoidal Maybe where
  unit = Just ()
  a *&* b = liftA2 (,) a b

instance Monoidal ((->) a) where
  unit = const ()
  a *&* b = \x -> (a x, b x)

instance Monoid a => Monoidal ((,) a) where
  unit = (mempty, ())
  (e1, a) *&* (e2, b) = (e1 <> e2, (a, b))

-- 3.2. Эквивалентность интерфейсов.
--
-- Покажите обе стороны эквивалентности интерфейсов.
-- Напишите Functor для обеих обёрток, Monoidal для ApplicativeMonoidal
-- и Applicative для MonoidalApplicative.

newtype ApplicativeMonoidal f a = ApplicativeMonoidal { runAM :: f a }
  deriving (Eq, Show)

instance Functor f => Functor (ApplicativeMonoidal f) where
  fmap f (ApplicativeMonoidal x) = ApplicativeMonoidal $ fmap f x

instance Applicative f => Monoidal (ApplicativeMonoidal f) where
  unit = ApplicativeMonoidal $ pure ()
  ApplicativeMonoidal a *&* ApplicativeMonoidal b = ApplicativeMonoidal $ liftA2 (,) a b

newtype MonoidalApplicative f a = MonoidalApplicative { runMA :: f a }
  deriving (Eq, Show)

instance Functor f => Functor (MonoidalApplicative f) where
  fmap f (MonoidalApplicative x) = MonoidalApplicative $ fmap f x

instance Monoidal f => Applicative (MonoidalApplicative f) where
  pure x = MonoidalApplicative $ const x <$> unit
  MonoidalApplicative f <*> MonoidalApplicative a = MonoidalApplicative $ (uncurry ($)) <$> (f *&* a)

-- 3.3. Сложение на консоли.
--
-- Прочитайте два числа, сложите, запишите результат на консоль.
--
-- liftA2 (+) readInt readInt имеет тип MyIO Int, а writeInt требует Int.
-- Сначала постройте MyIO (MyIO ()), затем используйте импортированный join:
--
--   join :: Monad m => m (m a) -> m a
--
-- Для MyIO он выполняет внешнее действие, получает внутреннее действие
-- и выполняет его в мире, оставшемся после внешнего.
-- Это мост к следующей теме: результат чтения выбирает последующее действие.
--
-- Написанную программу можно запустить командой make run.

summerAppMain :: MyIO ()
summerAppMain = join $ writeInt <$> ((+) <$> readInt <*> readInt)

-- 3.4. Композиция MyIO и Maybe.
--
-- Прочитайте n чисел и сложите.
-- При недостатке чисел верните Nothing; при n <= 0 — Just 0 без чтения.
-- Используйте свой Compose из 2.3, readIntMaybe и операции Applicative,
-- без Monad/join.
--
-- readIntMaybe потребляет одно число и возвращает Just с ним; на пустом входе
-- возвращает Nothing. Сохраните остаток входа и уже накопленный вывод.

type App = Compose MyIO Maybe

sumN :: Int -> App Int
sumN n = if n <= 0
  then pure 0
  else (+) <$> Compose readIntMaybe <*> sumN (n-1)
