module Api

open DataAccess
open FSharp.Data.UnitSystems.SI.UnitNames
open Shared

let private parliamentHill = { Latitude = 45.425507; Longitude = -75.700233 }

let getDistanceFromParliamentHill postalCode = async {
    if not (Validation.isValidPostalCode postalCode) then failwith "Invalid postal code"

    let! location = getLocation postalCode
    let distanceToParliamentHill = getDistanceBetweenPositions location.LatLong parliamentHill
    return { PostalCode = postalCode; Location = location; DistanceToParliamentHill = (distanceToParliamentHill / 1000.<meter>) }
}

let private asWeatherResponse (weather:DataAccess.Weather.MetaWeatherLocation.Root) =
    let today = weather.ConsolidatedWeather.[0]
    { WeatherType = today.WeatherStateName |> WeatherType.Parse
      LowTemp = today.MinTemp |> float
      HighTemp = today.MaxTemp |> float }

let getWeather postalCode = async {
    let! location = getLocation postalCode
    let! position = getWeatherForPosition location.LatLong
    let response = asWeatherResponse position
    return! async.Return { WeatherType = response.WeatherType; LowTemp = response.LowTemp; HighTemp = response.HighTemp }
}

let dojoApi =
    { GetDistance = getDistanceFromParliamentHill
      GetWeather = getWeather
    }