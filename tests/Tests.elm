module Tests exposing (paramSuite)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import Test exposing (..)

paramSuite : Test
paramSuite = describe "Parameterized Rover tests" <|
                         List.map
                         (\{name, rover, command, result} ->
                              test name <|
                                  \_ -> roverOneCommandTest rover command result
                          )
                          testData


roverOneCommandTest rover command result =
     let
         actual = execute rover command
     in
          (actual.position.x, actual.position.y, actual.facing) |> Expect.equal result

testData = [
            {name = "move FORWARD", rover = startRover, command =  FORWARD, result = ( 0, 1, NORTH) },
            {name = "move BACKWARD", rover = startRover, command =  BACKWARD, result = ( 0, -1, NORTH) },
            {name = "turn LEFT", rover = startRover, command =  LEFT, result = ( 0, 0, WEST) },
            {name = "turn RIGHT", rover = startRover, command =  RIGHT, result = ( 0, 0, EAST) },
            {name = "start EAST, move FORWARD", rover = Rover (Position 0 0) EAST, command =  FORWARD, result = ( 1, 0, EAST) },
            {name = "start EAST, move BACKWARD", rover = Rover (Position 0 0) EAST, command =  BACKWARD, result = ( -1, 0, EAST) }
            ]

startRover = Rover (Position 0 0) NORTH

type alias Rover = {position : Position, facing : Direction}

type alias Position = { x : Int, y : Int}

type Direction = NORTH | EAST | SOUTH | WEST

type Command = FORWARD | BACKWARD | LEFT | RIGHT

execute: Rover -> Command -> Rover
execute rover command =
        case command of
            FORWARD -> moveForward rover
            BACKWARD -> moveBackward rover
            LEFT -> { rover | facing = turnLeft rover.facing }
            RIGHT -> { rover | facing = turnRight rover.facing }

moveForward: Rover -> Rover
moveForward rover =
        let
            pos = rover.position
        in
            case rover.facing of
               NORTH -> { rover | position = { pos | y = pos.y + 1} }
               EAST -> { rover | position = { pos | x = pos.x + 1} }
               SOUTH -> { rover | position = { pos | y = pos.y - 1} }
               WEST -> { rover | position = { pos | x = pos.x - 1} }

moveBackward: Rover -> Rover
moveBackward rover =
        let
            pos = rover.position
        in
            case rover.facing of
               NORTH -> { rover | position = { pos | y = pos.y - 1} }
               EAST -> { rover | position = { pos | x = pos.x - 1} }
               SOUTH -> { rover | position = { pos | y = pos.y + 1} }
               WEST -> { rover | position = { pos | x = pos.x + 1} }

turnLeft: Direction -> Direction
turnLeft direction =
    case direction of
        NORTH -> WEST
        EAST -> NORTH
        SOUTH -> EAST
        WEST -> SOUTH

turnRight: Direction -> Direction
turnRight direction =
    case direction of
        NORTH -> EAST
        EAST -> SOUTH
        SOUTH -> WEST
        WEST -> NORTH


{-roverSuite : Test
roverSuite = describe "Rover tests"
                     [ test "startRover at 0,0 facing North" <|
                         \_ ->
                            (startRover.position.x, startRover.position.y, startRover.facing)
                                |> Expect.equal (0, 0, NORTH)
                     , test "startRover, command FORWARD, 0,1 facing North" <|
                         \_ ->
                            let
                                actual = execute startRover FORWARD
                            in
                                (actual.position.x, actual.position.y, actual.facing)
                                    |> Expect.equal (0, 1, NORTH)
                     , test "startRover, command BACKWARD, 1,0 facing North" <|
                         \_ ->
                            let
                                actual = execute startRover BACKWARD
                            in
                                (actual.position.x, actual.position.y, actual.facing)
                                    |> Expect.equal (0, -1, NORTH)
                     , test "startRover, command LEFT, 0,0 facing WEST" <|
                         \_ ->
                            let
                                actual = execute startRover LEFT
                            in
                                (actual.position.x, actual.position.y, actual.facing)
                                    |> Expect.equal (0, 0, WEST)
                     , test "startRover WEST, command FORWARD, 1,0 facing WEST" <|
                         \_ ->
                            let
                                start = Rover (Position 0 0) WEST
                                actual = execute start FORWARD
                            in
                                (actual.position.x, actual.position.y, actual.facing)
                                    |> Expect.equal (1, 0, WEST)
                     , test "startRover WEST, command BACKWARD, -1,0 facing WEST" <|
                         \_ ->
                            let
                                start = Rover (Position 0 0) WEST
                                actual = execute start BACKWARD
                            in
                                (actual.position.x, actual.position.y, actual.facing)
                                    |> Expect.equal (-1, 0, WEST)
                     ]-}



{-
   exampleTests : Test
   exampleTests =
       describe "Sample Test Suite"
           [ describe "Unit test examples"
               [ test "Addition" <|
                   \() ->
                       Expect.equal (3 + 7) 10
               , test "String.left" <|
                   \() ->
                       Expect.equal "a" (String.left 1 "abcdefg")
               ]
           , describe "Fuzz test examples, using randomly generated input"
               [ fuzz (list int) "Lists always have positive length" <|
                   \aList ->
                       List.length aList |> Expect.atLeast 0
               , fuzz (list int) "Sorting a list does not change its length" <|
                   \aList ->
                       List.sort aList |> List.length |> Expect.equal (List.length aList)
               , fuzzWith { runs = 1000 } int "List.member will get an integer in a list containing it" <|
                   \i ->
                       List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
               , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                   \s1 s2 ->
                       s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
               ]
           ]
-}
