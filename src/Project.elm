module Project exposing (..)

import Json.Decode as Decode
import Json.Decode exposing ((:=))

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
  , body : String
  }

type alias ColumnId = Int

issue : Decode.Decoder Issue
issue =
  Decode.object8 Issue
    ("url" := Decode.string)
    ("html_url" := Decode.string) 
    ("number" := Decode.int)
    ("title" := Decode.string)
    ("state" := Decode.string)
    ("comments" := Decode.int)
    ("updated_at" := Decode.string)
    ("body" := Decode.string)

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
