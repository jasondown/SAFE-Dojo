module DataAccess

open FSharp.Data
open Shared

[<AutoOpen>]
module GeoLocation =
    open FSharp.Data.UnitSystems.SI.UnitNames
    type PostalCodesIO = JsonProvider<"https://geocoder.ca/K1A0A9?json=1">

    let getLocation postalCode = async {
        let! postalCode = postalCode |> sprintf "https://geocoder.ca/%s?json=1" |> PostalCodesIO.AsyncLoad
        return
            { LatLong = { Latitude = float postalCode.Latt; Longitude = float postalCode.Longt }
              City = postalCode.Standard.City
              Province = postalCode.Standard.Prov } }

    let getDistanceBetweenPositions pos1 pos2 =
        let lat1, lng1 = pos1.Latitude, pos1.Longitude
        let lat2, lng2 = pos2.Latitude, pos2.Longitude
        let inline degreesToRadians degrees = System.Math.PI * float degrees / 180.0
        let R = 6371000.0
        let phi1 = degreesToRadians lat1
        let phi2 = degreesToRadians lat2
        let deltaPhi = degreesToRadians (lat2 - lat1)
        let deltaLambda = degreesToRadians (lng2 - lng1)
        let a = sin (deltaPhi / 2.0) * sin (deltaPhi / 2.0) + cos phi1 * cos phi2 * sin (deltaLambda / 2.0) * sin (deltaLambda / 2.0)
        let c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))
        R * c * 1.<meter>


[<AutoOpen>]
module Weather =
    type MetaWeatherSearch = JsonProvider<"https://www.metaweather.com/api/location/search/?lattlong=45.425507,-75.700233">
    type MetaWeatherLocation = JsonProvider<"https://www.metaweather.com/api/location/4118">
    let getWeatherForPosition location = async {
        let! locations =
            (location.Latitude, location.Longitude)
            ||> sprintf "https://www.metaweather.com/api/location/search/?lattlong=%f,%f"
            |> MetaWeatherSearch.AsyncLoad
        let bestLocationId = locations |> Array.sortBy (fun t -> t.Distance) |> Array.map (fun o -> o.Woeid) |> Array.head

        return!
            bestLocationId
            |> sprintf "https://www.metaweather.com/api/location/%d"
            |> MetaWeatherLocation.AsyncLoad }