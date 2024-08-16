import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._

object OpenMeteoRequest {
  case class Current(temperature_2m: Double, time: String)
  case class WeatherResponse(current: Current)

  def main(args: Array[String]): Unit = {
    // Define the API endpoint and parameters
    val apiUrl = "https://api.open-meteo.com/v1/forecast"
    val latitude = "52.52" // Latitude for Sion
    val longitude = "13.41" // Longitude for Sion
    val params = s"?latitude=$latitude&longitude=$longitude&current=temperature_2m&timezone=Europe%2FBerlin&forecast_days=1"
    val uri = s"$apiUrl$params"

    // Create an HttpClient
    val client = HttpClient.newHttpClient()

    // Create a GET request
    val request = HttpRequest.newBuilder()
      .uri(URI.create(uri))
      .build()

    // Send the request and get the response
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())

    // Parse the JSON response
    val jsonResponse = response.body()

    // Decode the JSON response into the WeatherResponse case class
    parse(jsonResponse).flatMap(_.as[WeatherResponse]) match {
      case Left(failure) => println(s"Failed to parse JSON: $failure")
      case Right(weatherResponse) =>
        // Extract and print the temperature and time
        val temperatures = weatherResponse.current.temperature_2m
        val time = weatherResponse.current.time

        println(s"Time: $time, Temperature: $temperatures°C")
    }
  }
}
