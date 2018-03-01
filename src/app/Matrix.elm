module Matrix exposing (..)

type alias Matrix a = List (List a)


fromList: Int -> List a -> Matrix a
fromList row list =
    case list of
        [] -> []
        l -> List.take row l :: fromList row (List.drop row l)


add: Matrix Float -> Matrix Float -> Matrix Float
add = zipMatrix (+)

subtract: Matrix Float -> Matrix Float -> Matrix Float
subtract = zipMatrix (-)

multiply: Matrix Float -> Matrix Float -> Matrix Float
multiply = zipMatrix (*)


map: (a -> b) -> Matrix a -> Matrix b
map f = List.map (List.map f)

initialize: Int -> Int -> ((Int, Int) -> a) -> Matrix a
initialize row col f =
    List.map (\x -> List.map (\y -> f (x, y)) (List.range 1 col)) (List.range 1 row )


zero: Int -> Int -> Matrix Float
zero row col = initialize row col <| always 0


identity: Int -> Matrix Float
identity n =  initialize n n (\(i,j) -> if i == j then 1 else 0)


rowMatrix: List a -> Matrix a
rowMatrix m = [m]


colMatrix: List a -> Matrix a
colMatrix = transpose << rowMatrix


empty: Matrix a
empty = []


rows: Matrix a -> Int
rows = List.length


cols: Matrix a -> Int
cols m =
    case List.head m of
        Nothing -> 0
        Just x -> List.length x


transpose: Matrix a -> Matrix a
transpose matrix =
    case matrix of
        [] -> []
        ([]::_) -> []
        x -> List.filterMap List.head x :: transpose (List.filterMap List.tail x)



dot: Matrix Float -> Matrix Float -> Matrix Float
dot mA mB =  --let _ = Debug.log "" (cols mA == rows mB) in
    List.map (\x -> List.map (\y -> List.sum <| zipWith (*) x y) (transpose mB)) mA


zipMatrix: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipMatrix f a b =
    case (a,b) of
        ([], _) -> []
        (_, []) -> []
        (x::xs, y::ys) -> zipWith f x y :: zipMatrix f xs ys


zipWith: (a -> b -> c) -> List a -> List b -> List c
zipWith f a b =
    case (a,b) of
        ([], _) -> []
        (_, []) -> []
        (x::xs, y::ys) -> f x y :: zipWith f xs ys

flatten: Matrix a -> List a
flatten = List.concat
