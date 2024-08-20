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
      temperature_2m_min: List[Double]
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
    // Define the API endpoint and parameters
    val apiUrl = "https://api.open-meteo.com/v1/forecast"
    val latitude = selectedRegion._1.toString()
    val longitude = selectedRegion._2.toString()
    val params =
      s"?latitude=$latitude&longitude=$longitude&daily=temperature_2m_max,temperature_2m_min&timezone=Europe%2FBerlin"
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

    println(jsonResponse)

    // Decode the JSON response into the WeatherResponse case class
    parse(jsonResponse).flatMap(_.as[WeatherForecast]) match {
      case Left(failure) => println(s"Failed to parse JSON: $failure")
      case Right(weatherResponse) =>
        val temperatures_2m_max = weatherResponse.daily.temperature_2m_max
        val temperatures_2m_min = weatherResponse.daily.temperature_2m_min
        val times = weatherResponse.daily.time

        val zippedForecast = times
          .zip(temperatures_2m_min)
          .zip(temperatures_2m_max)
          .map { case ((time, minTemp), maxTemp) =>
            (time, minTemp, maxTemp)
          }

        zippedForecast.foreach { case (time, minTemp, maxTemp) =>
          println(s"Date: $time - $minTemp <> $maxTemp")
        }
    }
  }

  def displayTemperature(selectedRegion: (Double, Double)) = {
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
      case Left(failure)         => println(s"Failed to parse JSON: $failure")
      case Right(weatherCurrent) =>
        // Extract and print the temperature and time
        val temperatures = weatherCurrent.current.temperature_2m
        val time = weatherCurrent.current.time

        println(s"Time: $time, Temperature: $temperatures°C")
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
}
