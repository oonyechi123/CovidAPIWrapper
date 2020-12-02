#' Country Search
#'
#' @param country This should be the 2-letter country code for the country of interest.
#' If no country is specified, the function returns a data frame of the information for all countries.
#'
#' @return This function returns the new and total confirmed cases, new and total deaths, and new and total recoveries for the country inputted by the end user.
#' @export
#'
#' @examples Country_Search("US")
#' ##to find Covid-19 information for the rate in the United States.
#'
Country_Search <- function(country = df$CountryCode) {
  CovidURL = paste("https://api.covid19api.com/summary")
  Covidquery = jsonlite::fromJSON(CovidURL)
  info = Covidquery$Countries
  df = data.frame(Country = info$Country, CountryCode = info$CountryCode,
                  NewConfirmed = info$NewConfirmed, TotalConfirmed = info$TotalConfirmed,
                  NewDeaths = info$NewDeaths, TotalDeaths = info$TotalDeaths,
                  NewRecovered = info$NewRecovered, TotalRecovered = info$TotalRecovered)
  result = df[which(df$CountryCode == country), ]
  result
}
