module Project exposing (..)

import Json.Decode as Decode
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))

type alias Column =
  { id : ColumnId
  , name : String
  , updated_at : String
  }

type alias Issue =
  { url : String
  , html_url : String
  , number : Int
  , title : String
  , state : String
  , comments : Int
  , updated_at : String
  , updated_distance : String
  , labels : List Label
  }

type alias Label =
  { id : Int
  , url : String
  , name : String
  , color : String
  , default : Bool
  }

type alias ColumnId = Int

label : Decode.Decoder Label
label =
  Decode.object5 Label
    ("id" := Decode.int)
    ("url" := Decode.string)
    ("name" := Decode.string)
    ("color" := Decode.string)
    ("default" := Decode.bool)

issue : Decode.Decoder Issue
issue =
  Decode.succeed Issue
    |: ("url" := Decode.string)
    |: ("html_url" := Decode.string)
    |: ("number" := Decode.int)
    |: ("title" := Decode.string)
    |: ("state" := Decode.string)
    |: ("comments" := Decode.int)
    |: ("updated_at" := Decode.string)
    |: ("updated_distance" := Decode.string)
    |: ("labels" := Decode.list label)

columnId : Decode.Decoder ColumnId
columnId = Decode.int

column : Decode.Decoder Column
column =
  Decode.object3 Column
    ("id" := columnId)
    ("name" := Decode.string)
    ("updated_at" := Decode.string)

columns : Decode.Decoder (List Column)
columns = Decode.list column

issues : Decode.Decoder (List Issue)
issues = Decode.list issue
