module Matrix exposing (..)

type alias Matrix a = List (List a)


fromList: Int -> List a -> Matrix a
fromList row list =
    case list of
        [] -> []
        l -> List.take row l :: fromList row (List.drop row l)


add: Matrix Float -> Matrix Float -> Matrix Float
add = map2 (+)


subtract: Matrix Float -> Matrix Float -> Matrix Float
subtract = map2 (-)


multiply: Matrix Float -> Matrix Float -> Matrix Float
multiply = map2 (*)


map: (a -> b) -> Matrix a -> Matrix b
map f = List.map (List.map f)


repeat: Int -> Int -> a -> Matrix a
repeat row col val = initialize row col (always val)


initialize: Int -> Int -> ((Int, Int) -> a) -> Matrix a
initialize row col f =
    List.map (\x -> List.map (\y -> f (x, y)) (List.range 1 col)) (List.range 1 row )


zero: Int -> Int -> Matrix Float
zero row col = repeat row col 0


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



dot: Matrix Float -> Matrix Float -> Maybe (Matrix Float)
dot a b =
    case cols a == rows b of
        True ->
            Just <| List.map (\x -> List.map (\y -> List.sum <| List.map2 (*) x y) (transpose b)) a
        False ->
            Nothing


unsafeDot: Matrix Float -> Matrix Float -> Matrix Float
unsafeDot a b =
    case dot a b of
        Nothing -> Debug.crash "Columns and rows mismatch between matrices."
        Just result -> result


map2: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
map2 f a b =
    case (a,b) of
        ([], _) -> []
        (_, []) -> []
        (x::xs, y::ys) -> List.map2 f x y :: map2 f xs ys


flatten: Matrix a -> List a
flatten = List.concat


