module IP exposing (IP(..), fromString)

import BigInt exposing (BigInt)


{-| An IP address.

IPv4 addresses are stored as 32-bit integers.
IPv6 addresses are stored as 128-bit integers,
Subnet masks are represented by the number of bits in the mask.

-}
type IP
    = IPv4 Int Int
    | IPv6 BigInt Int


{-| Convert a string to an IP address.

    fromString "10.10.10.10" == Just (IP 168430090 32)

    fromString "192.168.1.1/24" == Just (IP 3232235777 24)

    fromString "10.10.10" == Nothing

    fromString "10.10.10.256" == Nothing

-}
fromString : String -> Maybe IP
fromString str =
    let
        split =
            String.split "/" str
    in
    case split of
        [ ip, mask ] ->
            fromV4StringWithNetmask ip mask

        [ ip ] ->
            fromV4StringWithNetmask ip "32"

        _ ->
            Nothing


fromV4StringWithNetmask : String -> String -> Maybe IP
fromV4StringWithNetmask ip mask =
    let
        validMask : Int -> Maybe Int
        validMask m =
            if m >= 0 && m <= 32 then
                Just m

            else
                Nothing

        mask_ =
            String.toInt mask
                |> Maybe.andThen validMask

        toOctets : String -> List Int
        toOctets =
            String.split "." >> List.filterMap String.toInt
    in
    case toOctets ip of
        [ a, b, c, d ] ->
            if List.all (\x -> x >= 0 && x <= 255) [ a, b, c, d ] then
                let
                    ipInt =
                        a * 256 ^ 3 + b * 256 ^ 2 + c * 256 + d
                in
                Maybe.map (\m -> IPv4 ipInt m) mask_

            else
                Nothing

        _ ->
            Nothing
