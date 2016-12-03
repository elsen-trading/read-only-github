import Base64 exposing (encode)
import Date.Extra as Date
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import HttpBuilder as HB exposing (Error, Response)
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (join, toInt)
import Task exposing (Task, perform, map)

import Project exposing (..)

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Credentials =
  { user : String
  , pass : String
  , token : Maybe String
  }

type alias Model =
  { columns : List Column
  , issues : Dict ColumnId (List Issue)
  , selectedColumn : Maybe ColumnId
  , name : String
  , link : String
  , authenticated : Bool
  , credentials : Credentials
  }

type Msg 
  = ColumnPass (List Column)
  | ColumnFail Http.Error
  | ColumnSelected ColumnId
  | IssuePass ColumnId (List Issue)
  | IssueFail Http.Error
  | NamePass String
  | NameFail Http.Error
  | LinkPass String
  | LinkFail Http.Error
  | UpdateUser String
  | UpdatePass String
  | AttemptLogin
  | LoginFail Http.Error
  | LoginPass String

defaultModel : Model
defaultModel =
  Model [] Dict.empty Nothing "" "" False defaultCreds

defaultCreds : Credentials
defaultCreds = Credentials "" "" Nothing

init : (Model, Cmd Msg)
init = (defaultModel, Cmd.none)

loadAll : Model -> Cmd Msg
loadAll model =
  let
    cmds = [ loadColumns model.credentials.token
           , loadName model.credentials.token
           , loadLink model.credentials.token
           ]
  in
    Cmd.batch cmds

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ColumnPass cs ->
      let
        model' = { model | columns = cs }
      in
        (model', loadAllColumns model')
    ColumnFail err ->
      (model, Cmd.none)
    ColumnSelected i ->
      let
        model' = { model | selectedColumn = Just i }
      in
        (model', loadIssues model.credentials.token model.issues i)
    IssuePass cid is ->
      let
        issues' = Dict.insert cid is model.issues
      in
        ({ model | issues = issues' }, Cmd.none)
    IssueFail err ->
      (model, Cmd.none)
    NamePass n ->
      ({ model | name = n }, Cmd.none)
    NameFail err ->
      (model, Cmd.none)
    LinkPass l ->
      ({ model | link = l }, Cmd.none)
    LinkFail err ->
      (model, Cmd.none)
    UpdateUser u ->
      let
        creds = model.credentials
        creds' = { creds | user = u }
        token = genToken { model | credentials = creds' }
        creds'' = { creds' | token = Just token }
        model' = { model | credentials = creds'' }
      in
        (model', Cmd.none)
    UpdatePass p ->
      let
        creds = model.credentials
        creds' = { creds | pass = p }
        token = genToken { model | credentials = creds' }
        creds'' = { creds' | token = Just token }
        model' = { model | credentials = creds'' }
      in
        (model', Cmd.none)
    AttemptLogin ->
      (model, tryLogin model)
    LoginPass _ ->
      let
        model' = { model | authenticated = True }
      in
        (model', loadAll model')
    LoginFail err ->
      ({ model | authenticated = False }, Cmd.none)

view : Model -> Html Msg
view model =
  case model.authenticated of
    False -> nonauthenticatedView model
    True  -> authenticatedView model

nonauthenticatedView : Model -> Html Msg
nonauthenticatedView model =
  Html.form []
  [ div
    [ class "form-group"
    ]
    [ input
      [ type' "text"
      , id "user"
      , class "form-control"
      , placeholder "Enter username"
      , value model.credentials.user
      , onInput UpdateUser
      ]
      []
    ]
  , div
    [ class "form-group"
    ]
    [ input
      [ type' "password"
      , id "pass"
      , class "form-control"
      , placeholder "Enter password"
      , value model.credentials.pass
      , onInput UpdatePass
      ]
      []
    ]
  , button
    [ type' "button"
    , class "btn btn-primary"
    , onClick AttemptLogin
    ]
    [ text "Login" ]
  ]

authenticatedView : Model -> Html Msg
authenticatedView model =
  div [ ]
  [
    div [ class "page-header" ]
    [ h1
      []
      [ text "Read-only mirror of "
      , a
        [ href model.link ]
        [ text model.name ]
      ]
    , dropdowns model.columns
    ]
    , issueTable model
  ]

issueTable : Model -> Html Msg
issueTable m =
  case m.selectedColumn of
    Nothing -> text "Loading project columns.."
    Just c  ->
      case Dict.get c m.issues of
        Nothing -> text "Loading column issues.."
        Just is -> generateTable is

generateTable : List Issue -> Html Msg
generateTable is =
  table
  [ class "table" ]
  [ thead []
    [ tr []
      [ th []
        [ text "#" ]
      , th []
        [ text "Title" ]
      , th []
        [ text "Last Updated" ]
      , th []
        [ text "Comments" ]
      , th []
        [ text "Status" ]
      ]
    ]
  , tbody []
    ( List.map issue2row is )
  ]

issue2row : Issue -> Html Msg
issue2row issue =
  tr []
  [ th [ scope "row" ]
    [ text ( toString issue.number ) ]
  , td []
    [ text issue.title ]
  , td []
    [ text (formatDate issue.updated_at) ]
  , td []
    [ text ( toString issue.comments ) ]
  , a
    [ href issue.html_url
    , target "blank"
    ]
    [ td
      [ statusColor issue.state ]
      [ text issue.state ]
    ]
  ]

formatDate : String -> String
formatDate date =
  case Date.fromIsoString date of
    Nothing     -> date
    Just parsed -> date

statusColor : String -> Attribute Msg
statusColor status =
  case status == "closed" of
    True  -> class "status-closed"
    False -> class "status-open"

dropdowns : List Column -> Html Msg
dropdowns cs =
  Html.form
  [ class "form-inline"
  ]
  [ columnsDropDown cs
  ]

selected : Decode.Decoder ColumnId
selected =
  targetValue `Decode.andThen` \val ->
    case toInt val of
      Ok i    -> Decode.succeed i
      Err err -> Decode.fail err

onSelect : (ColumnId -> Msg) -> Attribute Msg
onSelect m =
  on "change" (Decode.map ColumnSelected selected)

columnsDropDown : List Column -> Html Msg
columnsDropDown xs =
  select [ class "form-control"
         , onSelect ColumnSelected
         ] (List.map column2Option xs)

column2Option : Column -> Html Msg
column2Option x =
  option [ value (toString x.id) ] [ text x.name ]

server : String
server = "http://localhost:9999/api"

columnsURL : String
columnsURL = server ++ "/columns"

nameURL : String
nameURL = server ++ "/name"

linkURL : String
linkURL = server ++ "/link"

issuesURL : ColumnId -> String
issuesURL cid = columnsURL ++ "/" ++ toString cid

loadColumns : Maybe String -> Cmd Msg
loadColumns token =
  --Task.perform ColumnFail ColumnPass (Http.get columns columnsURL)
  Task.perform ColumnFail ColumnPass (get token columns columnsURL)

loadIssues : Maybe String -> Dict ColumnId (List Issue) -> ColumnId -> Cmd Msg
loadIssues token is cid =
  let
    is' = case Dict.get cid is of
      Nothing -> 
        --Http.get issues (issuesURL cid)
        get token issues (issuesURL cid)
      Just is ->
        Task.succeed is
  in
    Task.perform IssueFail (IssuePass cid) is'

loadName : Maybe String -> Cmd Msg
loadName token =
  --Task.perform NameFail NamePass (Http.get Decode.string nameURL)
  Task.perform NameFail NamePass (get token Decode.string nameURL)

loadLink : Maybe String -> Cmd Msg
loadLink token =
  --Task.perform LinkFail LinkPass (Http.get Decode.string linkURL)
  Task.perform LinkFail LinkPass (get token Decode.string linkURL)

loadAllColumns : Model -> Cmd Msg
loadAllColumns model =
  let
    ids  = List.map .id model.columns
    iss  = loadIssues model.credentials.token model.issues
    load = List.map iss ids
    sel1 = case List.head ids of
      Nothing -> Cmd.none
      Just id -> Task.perform ColumnSelected ColumnSelected (Task.succeed id)
    cmds = sel1 :: load
  in
    Cmd.batch cmds

genToken : Model -> String
genToken model =
  let
    user    = model.credentials.user
    pass    = model.credentials.pass
  in
    case encode (user ++ ":" ++ pass) of
      Err err -> err
      Ok s    -> s

tryLogin : Model -> Cmd Msg
tryLogin model =
  let
    t = get model.credentials.token Decode.string nameURL
  in
    Task.perform LoginFail LoginPass t

get :
     Maybe String
  -> Decode.Decoder a
  -> String
  -> Task Http.Error a
get token d url =
  let
    auth    = Maybe.withDefault "" token
    headers = [("Authorization", "Basic " ++ auth)]
    cmd     = Http.send Http.defaultSettings
            { verb = "GET"
            , headers = headers
            , url = url
            , body = Http.empty
            }
  in
    Http.fromJson d cmd