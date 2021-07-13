namespace Shared

type LatLong =
    { Latitude : float
      Longitude : float }

type Location =
    { City : string
      Province : string
      LatLong : LatLong }

type LocationResponse = { PostalCode : string; Location : Location; DistanceToParliamentHill : float }
type WeatherType =
    | Snow
    | Sleet
    | Hail
    | Thunder
    | HeavyRain
    | LightRain
    | Showers
    | HeavyCloud
    | LightCloud
    | Clear
    static member Parse =
        let weatherTypes = FSharp.Reflection.FSharpType.GetUnionCases typeof<WeatherType>
        fun (s:string) ->
            weatherTypes
            |> Array.find(fun w -> w.Name = s.Replace(" ", ""))
            |> fun u -> FSharp.Reflection.FSharpValue.MakeUnion(u, [||]) :?> WeatherType
    member this.Abbreviation =
        match this with
        | Snow -> "sn" | Sleet -> "sl" | Hail -> "h" | Thunder -> "t" | HeavyRain -> "hr"
        | LightRain -> "lr" | Showers -> "s" | HeavyCloud -> "hc" | LightCloud -> "lc" | Clear -> "c"

type WeatherResponse = { WeatherType : WeatherType; LowTemp : float; HighTemp: float; }

module Route =
    let builder = sprintf "/api/%s/%s"

type IDojoApi =
    { GetDistance : string -> LocationResponse Async
      GetWeather: string -> WeatherResponse Async }

/// Provides validation on data. Shared across both client and server.
module Validation =
    open System.Text.RegularExpressions
    let isValidPostalCode postalCode =
        Regex.IsMatch(postalCode, @"^[A-Za-z]\d[A-Za-z][ -]?\d[A-Za-z]\d$")