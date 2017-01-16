
class Functor2 f where
  fmapa :: (a -> b) -> f a x -> f b x
  fmapb :: (a -> b) -> f x a -> f x b

