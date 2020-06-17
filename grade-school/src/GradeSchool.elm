module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)

import Dict exposing (Dict)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Grade (List Student)


empty : School
empty =
    Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    Dict.insert grade (List.sort <| student :: studentsInGrade grade school) school


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    Maybe.withDefault [] (Dict.get grade school)


allStudents : School -> List ( Grade, List Student )
allStudents school =
    Dict.toList school
