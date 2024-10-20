module Parse exposing (parse)

import Expect
import IP
import Test exposing (..)


parse : Test
parse =
    describe "parse" <|
        [ test "parses a valid IPv4 address" <|
            \_ ->
                case IP.fromString "127.0.0.1/16" of
                    Just ip ->
                        Expect.equal ip (IP.IPv4 2130706433 16)

                    _ ->
                        Expect.fail "Expected an IP address"
        ]
