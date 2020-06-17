module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight robot =
    { robot
        | bearing =
            case robot.bearing of
                North ->
                    East

                East ->
                    South

                South ->
                    West

                West ->
                    North
    }


turnLeft : Robot -> Robot
turnLeft robot =
    { robot
        | bearing =
            case robot.bearing of
                North ->
                    West

                East ->
                    North

                South ->
                    East

                West ->
                    South
    }


advance : Robot -> Robot
advance robot =
    let
        currentCoordinates =
            robot.coordinates

        newCoordinates =
            case robot.bearing of
                North ->
                    { currentCoordinates | y = robot.coordinates.y + 1 }

                East ->
                    { currentCoordinates | x = robot.coordinates.x + 1 }

                South ->
                    { currentCoordinates | y = robot.coordinates.y - 1 }

                West ->
                    { currentCoordinates | x = robot.coordinates.x - 1 }
    in
    { robot | coordinates = newCoordinates }

processInput : Char -> Robot -> Robot
processInput input robot =
    case input of
        'L' -> turnLeft robot
        'R' -> turnRight robot
        'A' -> advance robot
        _ -> robot

simulate : String -> Robot -> Robot
simulate directions robot =
    directions
    |> String.toList
    |> List.foldl processInput robot