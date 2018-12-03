module Basic exposing (Int6(..), charToInt6, intToChar, listFromMaybeList, quadMap, tripleMap)


listFromMaybeList : List (Maybe a) -> Maybe (List a)
listFromMaybeList xs =
    List.foldl
        (\hd res ->
            case ( res, hd ) of
                ( Nothing, _ ) ->
                    Nothing

                ( _, Nothing ) ->
                    Nothing

                ( Just tl, Just x ) ->
                    Just (x :: tl)
        )
        (Just [])
        xs
        |> Maybe.map List.reverse


tripleMap : (a -> Maybe a -> Maybe a -> b) -> List a -> List b
tripleMap fn xs =
    case xs of
        hd :: nk :: tr :: tl ->
            fn hd (Just nk) (Just tr) :: tripleMap fn tl

        [ hd, nk ] ->
            [ fn hd (Just nk) Nothing ]

        [ hd ] ->
            [ fn hd Nothing Nothing ]

        [] ->
            []


quadMap : (a -> Maybe a -> Maybe a -> Maybe a -> b) -> List a -> List b
quadMap fn xs =
    case xs of
        hd :: nk :: tr :: pl :: tl ->
            fn hd (Just nk) (Just tr) (Just pl) :: quadMap fn tl

        [ hd, nk, tr ] ->
            [ fn hd (Just nk) (Just tr) Nothing ]

        [ hd, nk ] ->
            [ fn hd (Just nk) Nothing Nothing ]

        [ hd ] ->
            [ fn hd Nothing Nothing Nothing ]

        [] ->
            []


type Int6
    = Number Int
    | Padding
    | Nope


charToInt6 : Char -> Int6
charToInt6 char =
    case char of
        'A' ->
            Number 0

        'B' ->
            Number 1

        'C' ->
            Number 2

        'D' ->
            Number 3

        'E' ->
            Number 4

        'F' ->
            Number 5

        'G' ->
            Number 6

        'H' ->
            Number 7

        'I' ->
            Number 8

        'J' ->
            Number 9

        'K' ->
            Number 10

        'L' ->
            Number 11

        'M' ->
            Number 12

        'N' ->
            Number 13

        'O' ->
            Number 14

        'P' ->
            Number 15

        'Q' ->
            Number 16

        'R' ->
            Number 17

        'S' ->
            Number 18

        'T' ->
            Number 19

        'U' ->
            Number 20

        'V' ->
            Number 21

        'W' ->
            Number 22

        'X' ->
            Number 23

        'Y' ->
            Number 24

        'Z' ->
            Number 25

        'a' ->
            Number 26

        'b' ->
            Number 27

        'c' ->
            Number 28

        'd' ->
            Number 29

        'e' ->
            Number 30

        'f' ->
            Number 31

        'g' ->
            Number 32

        'h' ->
            Number 33

        'i' ->
            Number 34

        'j' ->
            Number 35

        'k' ->
            Number 36

        'l' ->
            Number 37

        'm' ->
            Number 38

        'n' ->
            Number 39

        'o' ->
            Number 40

        'p' ->
            Number 41

        'q' ->
            Number 42

        'r' ->
            Number 43

        's' ->
            Number 44

        't' ->
            Number 45

        'u' ->
            Number 46

        'v' ->
            Number 47

        'w' ->
            Number 48

        'x' ->
            Number 49

        'y' ->
            Number 50

        'z' ->
            Number 51

        '0' ->
            Number 52

        '1' ->
            Number 53

        '2' ->
            Number 54

        '3' ->
            Number 55

        '4' ->
            Number 56

        '5' ->
            Number 57

        '6' ->
            Number 58

        '7' ->
            Number 59

        '8' ->
            Number 60

        '9' ->
            Number 61

        '+' ->
            Number 62

        '/' ->
            Number 63

        '=' ->
            Padding

        _ ->
            Nope


intToChar : Int -> Maybe Char
intToChar n =
    case n of
        0 ->
            Just 'A'

        1 ->
            Just 'B'

        2 ->
            Just 'C'

        3 ->
            Just 'D'

        4 ->
            Just 'E'

        5 ->
            Just 'F'

        6 ->
            Just 'G'

        7 ->
            Just 'H'

        8 ->
            Just 'I'

        9 ->
            Just 'J'

        10 ->
            Just 'K'

        11 ->
            Just 'L'

        12 ->
            Just 'M'

        13 ->
            Just 'N'

        14 ->
            Just 'O'

        15 ->
            Just 'P'

        16 ->
            Just 'Q'

        17 ->
            Just 'R'

        18 ->
            Just 'S'

        19 ->
            Just 'T'

        20 ->
            Just 'U'

        21 ->
            Just 'V'

        22 ->
            Just 'W'

        23 ->
            Just 'X'

        24 ->
            Just 'Y'

        25 ->
            Just 'Z'

        26 ->
            Just 'a'

        27 ->
            Just 'b'

        28 ->
            Just 'c'

        29 ->
            Just 'd'

        30 ->
            Just 'e'

        31 ->
            Just 'f'

        32 ->
            Just 'g'

        33 ->
            Just 'h'

        34 ->
            Just 'i'

        35 ->
            Just 'j'

        36 ->
            Just 'k'

        37 ->
            Just 'l'

        38 ->
            Just 'm'

        39 ->
            Just 'n'

        40 ->
            Just 'o'

        41 ->
            Just 'p'

        42 ->
            Just 'q'

        43 ->
            Just 'r'

        44 ->
            Just 's'

        45 ->
            Just 't'

        46 ->
            Just 'u'

        47 ->
            Just 'v'

        48 ->
            Just 'w'

        49 ->
            Just 'x'

        50 ->
            Just 'y'

        51 ->
            Just 'z'

        52 ->
            Just '0'

        53 ->
            Just '1'

        54 ->
            Just '2'

        55 ->
            Just '3'

        56 ->
            Just '4'

        57 ->
            Just '5'

        58 ->
            Just '6'

        59 ->
            Just '7'

        60 ->
            Just '8'

        61 ->
            Just '9'

        62 ->
            Just '+'

        63 ->
            Just '/'

        _ ->
            Nothing
