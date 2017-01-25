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
import Table exposing (defaultCustomizations)
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
  { columns : List Column -- the available columns
  , issues : List Issue -- issues of the selected column
  , selectedColumn : Maybe ColumnId
  , name : String
  , link : String
  , authenticated : Bool
  , credentials : Credentials
  , tableState : Table.State
  }

type Msg 
  = ColumnPass (List Column)
  | ColumnFail Http.Error
  | ColumnSelected ColumnId
  | IssuePass (List Issue)
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
  | AttemptRefresh
  | RefreshFail Http.Error
  | RefreshPass Bool
  | SetTableState Table.State

defaultModel : Model
defaultModel =
  { columns = []
  , issues = []
  , selectedColumn = Nothing
  , name = ""
  , link = ""
  , authenticated = False
  , credentials = defaultCreds
  , tableState = Table.initialSort "#"
  }

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
        (model', defaultSelect model')
    ColumnFail err ->
      (model, Cmd.none)
    ColumnSelected i ->
      let
        model' = { model | selectedColumn = Just i }
      in
        (model', loadIssues model.credentials.token i)
    IssuePass is ->
      ({ model | issues = is }, Cmd.none)
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
    AttemptRefresh ->
      (model, tryRefresh model)
    RefreshPass _ ->
      (model, loadColumns model.credentials.token)
    RefreshFail err ->
      (model, Cmd.none)
    SetTableState st ->
      ({ model | tableState = st }, Cmd.none)

view : Model -> Html Msg
view model =
  case model.authenticated of
    False -> nonauthenticatedView model
    True  -> authenticatedView model

nonauthenticatedView : Model -> Html Msg
nonauthenticatedView model =
  Html.form [ class "login-page" ]
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
    , class "btn btn-primary login-button"
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
    , buttons model.columns
    ]
    , Table.view config model.tableState model.issues
  ]

refreshButton : Html Msg
refreshButton =
  button
  [ type' "button"
  , class "btn btn-primary form-inline button-bar"
  , onClick AttemptRefresh
  ]
  [ text "Refresh" ]

tableCustomizations : Table.Customizations data Msg
tableCustomizations =
  { defaultCustomizations
  | tableAttrs =
      [ class "table table-hover"
      ]
  }

formatDate : Issue -> String
formatDate issue =
  case Date.fromIsoString issue.updated_at of
    Nothing     -> issue.updated_at
    Just parsed -> Date.toFormattedString "y-M-d" parsed

config : Table.Config Issue Msg
config =
  Table.customConfig
    { toId = (\x -> toString x.number)
    , toMsg = SetTableState
    , columns =
        [ Table.intColumn "#" .number
        , Table.stringColumn "Title" .title
        , labelColumn "Tags"
        , Table.stringColumn "Updated" formatDate
        , Table.intColumn "Comments" .comments
        , statusColumn "Status"
        ]
    , customizations = tableCustomizations
    }

labelColumn : String -> Table.Column Issue Msg
labelColumn name =
  Table.veryCustomColumn
    { name = name
    , viewData = combineLabels
    , sorter = Table.increasingOrDecreasingBy (\x -> List.length (.labels x))
    }

combineLabels : Issue -> Table.HtmlDetails Msg
combineLabels issue =
  Table.HtmlDetails []
    [ span [] (List.map createLabel issue.labels)
    ]

labelStyle : String -> Attribute Msg
labelStyle color =
  style
    [ ("backgroundColor", "#" ++ color)
    , ("color", "#fff")
    ]

createLabel : Label -> Html Msg
createLabel label =
  div
    [ labelStyle label.color
    , class "label"
    ]
    [ text label.name ]

statusColumn : String -> Table.Column Issue Msg
statusColumn name =
  Table.veryCustomColumn
    { name = name
    , viewData = statusLink
    , sorter = Table.increasingOrDecreasingBy .state
    }

statusLink : Issue -> Table.HtmlDetails Msg
statusLink issue =
  Table.HtmlDetails []
    [ a
      [ href issue.html_url
      , target "blank"
      ]
      [ td
        [ statusColor issue.state ]
        [ text issue.state ]
      ]
    ]

statusColor : String -> Attribute Msg
statusColor status =
  case status == "closed" of
    True  -> class "status-closed"
    False -> class "status-open"

buttons : List Column -> Html Msg
buttons cs =
  Html.form
  [ class "form-inline"
  ]
  [ columnsDropDown cs
  -- , refreshButton
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
  select [ class "form-control button-bar"
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

refreshURL : String
refreshURL = server ++ "/refresh"

issuesURL : ColumnId -> String
issuesURL cid = columnsURL ++ "/" ++ toString cid

loadColumns : Maybe String -> Cmd Msg
loadColumns token =
  Task.perform ColumnFail ColumnPass (get token columns columnsURL)

loadIssues :
     Maybe String
  -> ColumnId
  -> Cmd Msg
loadIssues token cid =
  Task.perform IssueFail IssuePass (get token issues (issuesURL cid))

loadName : Maybe String -> Cmd Msg
loadName token =
  Task.perform NameFail NamePass (get token Decode.string nameURL)

loadLink : Maybe String -> Cmd Msg
loadLink token =
  Task.perform LinkFail LinkPass (get token Decode.string linkURL)

-- select the first column, if there is one
defaultSelect : Model -> Cmd Msg
defaultSelect model =
  case List.head model.columns of
    Nothing -> Cmd.none
    Just c  ->
      Task.perform ColumnSelected ColumnSelected (Task.succeed (.id c))

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

tryRefresh : Model -> Cmd Msg
tryRefresh model =
  let
    t = get model.credentials.token Decode.bool refreshURL
  in
    Task.perform RefreshFail RefreshPass t

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
