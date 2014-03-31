module Universes

%default total

data Sizable = INT | LIST Sizable | EITHER Sizable Sizable

interp : Sizable -> Type
interp INT = Int
interp (LIST x) = List (interp x)
interp (EITHER x y) = Either (interp x) (interp y)


size : (t : Sizable) -> interp t -> Int
size INT x = x
size (LIST y) xs = sum (map (size y) xs)
size (EITHER y z) (Left x) = 1 + size y x
size (EITHER y z) (Right x) = 1 + size z x

