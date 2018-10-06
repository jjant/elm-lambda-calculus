module Church where

true :: a -> b -> a
true = \x y -> x

false :: a -> b -> b
false = \x y -> y

or_ = (\b1 b2 -> b1 true b2)

not_ = (\x -> x false true)

pair_ = (\first second b -> b first second)

fst_ = (\p -> p true)

snd_ = \p -> p false

zero = \f x -> x

one = \f x -> f x

succ_ = \n f x -> f (n f x)

iszero = \n -> n (\x -> false) true

plus = \m n f x -> m f (n f x)

----
toNumber :: ((Int -> Int) -> Int -> t) -> t
toNumber x = x (+ 1) 0

toBool :: (Bool -> Bool -> t) -> t
toBool x = x True False
