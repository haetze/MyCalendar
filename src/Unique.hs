{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Unique where


class (Eq a) => Unique a where
  unique:: a -> a

instance Unique Int where
  unique x = x

instance Unique Integer where
  unique x = x

instance Unique Double where
  unique x = x

instance Unique String where
  unique x = x

instance Unique Bool where
  unique x = x

instance (Unique a) => Unique [a] where
  unique [] = []
  unique (x:xs) = f (x:xs) []
    where
      f [] ys = ys
      f (x:xs) ys | unique x `elem` ys = f xs ys
                  | otherwise          = f xs (ys ++ [unique x])

