import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse

import scala.io.StdIn.readLine

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import scala.util.Try

object OpenMeteoRequest {
  case class Current(temperature_2m: Double, time: String)
  case class WeatherCurrent(current: Current)

  case class Daily(
      time: List[String],
      temperature_2m_max: List[Double],
      temperature_2m_min: List[Double],
      weather_code: List[Int]
  )
  case class WeatherForecast(daily: Daily)

  case class Region(
      name: String,
      country: String,
      latitude: Double,
      longitude: Double
  )
  case class Regions(results: List[Region])

  def main(args: Array[String]): Unit = {
    val regions = getRegions()
    val selectedRegion = getRegion(regions)
    displayForecast(selectedRegion)
    displayTemperature(selectedRegion)
  }

  def displayForecast(selectedRegion: (Double, Double)) = {
    // Define ANSI escape codes for colors
    val resetColor = "\u001B[0m"
    val yellowColor = "\u001B[33m"
    val cyanColor = "\u001B[36m"
    val redColor = "\u001B[31m"
    val greenColor = "\u001B[32m"
    val boldText = "\u001B[1m"
    val lightGray = "\u001B[37m"

    // Define the API endpoint and parameters
    val apiUrl = "https://api.open-meteo.com/v1/forecast"
    val latitude = selectedRegion._1.toString()
    val longitude = selectedRegion._2.toString()
    val params =
      s"?latitude=$latitude&longitude=$longitude&daily=weather_code,temperature_2m_max,temperature_2m_min&timezone=Europe%2FBerlin"
    val uri = s"$apiUrl$params"

    // Create an HttpClient
    val client = HttpClient.newHttpClient()

    // Create a GET request
    val request = HttpRequest
      .newBuilder()
      .uri(URI.create(uri))
      .build()

    // Send the request and get the response
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())

    // Parse the JSON response
    val jsonResponse = response.body()

    // Decode the JSON response into the WeatherResponse case class
    parse(jsonResponse).flatMap(_.as[WeatherForecast]) match {
      case Left(failure) =>
        println(s"${redColor}Failed to parse JSON: $failure$resetColor")
      case Right(weatherResponse) =>
        println(
          s"\n$boldText$lightGray--- 7-Day Weather Forecast ---$resetColor\n"
        )
        val times = weatherResponse.daily.time
        val temperatures_2m_min = weatherResponse.daily.temperature_2m_min
        val temperatures_2m_max = weatherResponse.daily.temperature_2m_max
        val weather_code = weatherResponse.daily.weather_code

        val zippedForecast = times
          .zip(temperatures_2m_min)
          .zip(temperatures_2m_max)
          .zip(weather_code)
          .map { case (((time, minTemp), maxTemp), weatherCode) =>
            (time, minTemp, maxTemp, weatherCode)
          }

        zippedForecast.foreach { case (time, minTemp, maxTemp, weatherCode) =>
          val weatherCodeDescribed = describeWeatherCode(weatherCode)
          val tempColor =
            if (maxTemp > 25) redColor
            else if (minTemp < 0) cyanColor
            else yellowColor

          println(s"$greenColor Date$resetColor $boldText$time$resetColor")
          println(
            f"$boldText Min Temp:$resetColor $tempColor$minTemp%4.1f°C$resetColor | $boldText Max Temp:$resetColor $tempColor$maxTemp%4.1f°C$resetColor"
          )
          println(
            s"$boldText Condition:$resetColor $lightGray$weatherCodeDescribed$resetColor"
          )
          println(s"${lightGray}--------------------------------$resetColor")
        }
    }
  }

  def displayTemperature(selectedRegion: (Double, Double)) = {
    // Define ANSI escape codes for colors
    val resetColor = "\u001B[0m"
    val yellowColor = "\u001B[33m"
    val cyanColor = "\u001B[36m"
    val redColor = "\u001B[31m"
    val boldText = "\u001B[1m"
    val lightGray = "\u001B[37m"

    // Define the API endpoint and parameters
    val apiUrl = "https://api.open-meteo.com/v1/forecast"
    val latitude = selectedRegion._1.toString()
    val longitude = selectedRegion._2.toString()
    val params =
      s"?latitude=$latitude&longitude=$longitude&current=temperature_2m&timezone=Europe%2FBerlin&forecast_days=1"
    val uri = s"$apiUrl$params"

    // Create an HttpClient
    val client = HttpClient.newHttpClient()

    // Create a GET request
    val request = HttpRequest
      .newBuilder()
      .uri(URI.create(uri))
      .build()

    // Send the request and get the response
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())

    // Parse the JSON response
    val jsonResponse = response.body()

    // Decode the JSON response into the WeatherResponse case class
    parse(jsonResponse).flatMap(_.as[WeatherCurrent]) match {
      case Left(failure) =>
        println(s"${redColor}Failed to parse JSON: $failure$resetColor")
      case Right(weatherCurrent) =>
        val temperature = weatherCurrent.current.temperature_2m
        val time = weatherCurrent.current.time
        val tempColor =
          if (temperature > 25) redColor
          else if (temperature < 0) cyanColor
          else yellowColor

        println(
          s"\n$boldText$lightGray--- Current Temperature ---$resetColor\n"
        )
        println(f"$boldText Time:$resetColor $lightGray$time$resetColor")
        println(
          f"$boldText Temperature:$resetColor $tempColor$temperature%.1f°C$resetColor"
        )
        println(s"${lightGray}----------------------------$resetColor\n")
    }
  }

  def getRegions() = {
    val apiUrl = "https://geocoding-api.open-meteo.com/v1/search?"

    printf("Enter a region: ")
    val name = readLine()

    val params = s"name=$name&count=5&language=en&format=json"
    val uri = s"$apiUrl$params"

    val client = HttpClient.newHttpClient()
    val request = HttpRequest
      .newBuilder()
      .uri(URI.create(uri))
      .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    val jsonResponse = response.body

    parse(jsonResponse).flatMap(_.as[Regions]) match {
      case Left(failure) =>
        println(s"Failed to parse JSON: $failure")
        List.empty[(Double, Double)]
      case Right(regions) =>
        regions.results.zipWithIndex.foreach { case (result, index) =>
          println(s"${index + 1}. ${result.name} in ${result.country}")
        }
        regions.results.map(result => (result.latitude, result.longitude))
    }
  }

  def getRegion(regions: List[(Double, Double)]) = {
    printf("Enter the number of the region to use (1-5): ")
    val index: Int = Try(readLine().toInt).getOrElse(1)
    val adjustIndex = if (index >= 1 && index <= 5) index - 1 else 0
    regions(adjustIndex)
  }

  def describeWeatherCode(code: Int): String = code match {
    case 0            => "Clear sky"
    case 1            => "Mainly clear"
    case 2            => "Partly cloudy"
    case 3            => "Overcast"
    case 45 | 48      => "Fog"
    case 51 | 53 | 55 => "Drizzle"
    case 61 | 63 | 65 => "Rain"
    case 66 | 67      => "Freezing rain"
    case 71 | 73 | 75 => "Snow fall"
    case 77           => "Snow grains"
    case 80 | 81 | 82 => "Rain showers"
    case 85 | 86      => "Snow showers"
    case 95           => "Thunderstorm"
    case 96 | 99      => "Thunderstorm with hail"
    case _            => "Unknown weather condition"
  }
}
