module App

open Elmish
open Fable.FontAwesome
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fable.Remoting.Client
open Fulma
open Leaflet
open ReactLeaflet
open Shared

/// The different elements of the completed report.
type Report =
    { Location : LocationResponse
      Weather : WeatherResponse }

type ServerState = Idle | Loading | ServerError of string

/// The overall data model driving the view.
type Model =
    { PostalCode : string
      ValidationError : string option
      ServerState : ServerState
      Report : Report option }

/// The different types of messages in the system.
type Msg =
    | GetReport
    | PostalCodeChanged of string
    | GotReport of Report
    | ErrorMsg of exn
    | Clear

/// The init function is called to start the message pump with an initial view.
let init () =
    { PostalCode = ""
      Report = None
      ValidationError = None
      ServerState = Idle }, Cmd.ofMsg (PostalCodeChanged "")

let dojoApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IDojoApi>

let getResponse postalCode = async {
    let! location = dojoApi.GetDistance postalCode
    let! weather = dojoApi.GetWeather postalCode

    return
        { Location = location
          Weather = weather }
}

/// The update function knows how to update the model given a message.
let update msg model =
    match model, msg with
    | { ValidationError = None; PostalCode = postalCode }, GetReport ->
        { model with ServerState = Loading }, Cmd.OfAsync.either getResponse postalCode GotReport ErrorMsg
    | _, GetReport ->
        model, Cmd.none
    | _, GotReport response ->
        { model with
            ValidationError = None
            Report = Some response
            ServerState = Idle }, Cmd.none
    | _, PostalCodeChanged p ->
        { model with
            PostalCode = p
            ValidationError =
                match Validation.isValidPostalCode p with
                | true -> None
                | false -> Some "Please enter a valid Canadian postal code"
        }, Cmd.none
    | _, ErrorMsg e ->
        { model with ServerState = ServerError e.Message }, Cmd.none
    | _, Clear ->
        init ()

[<AutoOpen>]
module ViewParts =
    let basicTile title options content =
        Tile.tile options [
            Notification.notification [ Notification.Props [ Style [ Height "100%"; Width "100%" ] ] ] [
                Heading.h2 [] [ str title ]
                yield! content
            ]
        ]

    let makeMarker latLong description =
        marker [ MarkerProps.Position latLong ] [
            tooltip [ ] [ str description ]
        ]

    let mapTile (lr:LocationResponse) =
        let latLong = LatLngExpression.Case3(lr.Location.LatLong.Latitude, lr.Location.LatLong.Longitude)
        basicTile "Map" [ Tile.Size Tile.Is12 ] [
            map [
                MapProps.Center latLong
                MapProps.Zoom 15.
                MapProps.Style [ Height 500 ]
            ] [
                tileLayer [ TileLayerProps.Url "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" ] []
                makeMarker latLong (sprintf "%s - %s, %s" lr.PostalCode lr.Location.City lr.Location.Province)
            ]
        ]

    let weatherTile weatherReport =
        basicTile "Today's Weather" [] [
            Level.level [ ] [
                Level.item [ Level.Item.HasTextCentered ] [
                    div [ ] [
                        Level.heading [ ] [
                            Image.image [ Image.Is128x128 ] [
                                img [ Src(sprintf "https://www.metaweather.com/static/img/weather/%s.svg" weatherReport.WeatherType.Abbreviation) ]
                            ]
                        ]
                        Level.title [ ] [
                            Heading.h3 [ Heading.Is3; Heading.Props [ Style [ Width "100%" ] ] ] [
                                str (sprintf "Low: %.0f C" weatherReport.LowTemp)
                            ]
                        ]
                        Level.title [ ] [
                            Heading.h3 [ Heading.Is3; Heading.Props [ Style [ Width "100%" ] ] ] [
                                str (sprintf "High: %.0f C" weatherReport.HighTemp)
                            ]
                        ]
                    ]
                ]
            ]
        ]
    let locationTile model =
        basicTile "Location" [] [
            div [ ] [
                Heading.h3 [ ] [ str model.Location.Location.City ]
                Heading.h4 [ ] [ str model.Location.Location.Province ]
                Heading.h4 [ ] [ sprintf "%.1f KM to Parliament Hill" model.Location.DistanceToParliamentHill |> str ]
            ]
        ]


/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view (model:Model) dispatch =
    section [] [
        Hero.hero [ Hero.Color Color.IsInfo ] [
            Hero.body [ ] [
                Container.container [
                    Container.IsFluid
                    Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
                ] [
                    Heading.h1 [ ] [
                        str "SAFE Dojo - Canadian Flavour"
                    ]
                ]
            ]
        ]

        Container.container [] [
            Field.div [] [
                Label.label [] [ str "PostalCode" ]
                Control.div [ Control.HasIconLeft; Control.HasIconRight ] [
                    Input.text [
                        Input.Placeholder "e.g. K1A 0A9"
                        Input.Value model.PostalCode
                        Input.Modifiers [ Modifier.TextTransform TextTransform.UpperCase ]
                        Input.Color (if model.ValidationError.IsSome then Color.IsDanger else Color.IsSuccess)
                        Input.Props [ OnChange (fun ev -> dispatch (PostalCodeChanged !!ev.target?value)); onKeyDown Key.enter (fun _ -> dispatch GetReport) ]
                    ]
                    Fulma.Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ Fa.Solid.Home ] [] ]
                    match model with
                    | { ValidationError = Some _ } ->
                        Icon.icon [ Icon.Size IsSmall; Icon.IsRight ] [ Fa.i [ Fa.Solid.Exclamation ] [] ]
                    | { ValidationError = None } ->
                        Icon.icon [ Icon.Size IsSmall; Icon.IsRight ] [ Fa.i [ Fa.Solid.Check ] [] ]
                ]
                Help.help [
                    Help.Color (if model.ValidationError.IsNone then IsSuccess else IsDanger)
                ] [
                    str (model.ValidationError |> Option.defaultValue "")
                ]
            ]
            Field.div [ Field.IsGrouped ] [
                Level.level [ ] [
                    Level.left [] [
                        Level.item [] [
                            Button.button [
                                Button.IsFullWidth
                                Button.Color IsPrimary
                                Button.OnClick (fun _ -> dispatch GetReport)
                                Button.Disabled (model.ValidationError.IsSome)
                                Button.IsLoading (model.ServerState = ServerState.Loading)
                            ] [ str "Submit" ]
                        ]
                        Level.item [] [
                            Button.button [
                                Button.IsFullWidth
                                Button.Color IsDanger
                                Button.OnClick (fun _ -> dispatch Clear)
                            ] [ str "Clear" ]
                        ]
                    ]
                ]
            ]

            match model with
            | { Report = None; ServerState = (Idle | Loading) } -> ()
            | { ServerState = ServerError error } ->
                Field.div [] [
                    Tag.list [ Tag.List.HasAddons; Tag.List.IsCentered ] [
                        Tag.tag [ Tag.Color Color.IsDanger; Tag.Size IsMedium ] [
                            str error
                        ]
                    ]
                ]
            | { Report = Some report } ->
                Tile.ancestor [
                    Tile.Size Tile.Is12
                ] [
                    Tile.parent [ Tile.Size Tile.Is8 ] [
                        mapTile report.Location
                    ]
                    Tile.parent [ Tile.IsVertical; Tile.Size Tile.Is4 ] [
                        locationTile report
                        weatherTile report.Weather
                    ]
                ]
        ]

        br [ ]

        Footer.footer [] [
            Content.content [
                Content.Modifiers [ Fulma.Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
            ] [ safeComponents ]
        ]
    ]
