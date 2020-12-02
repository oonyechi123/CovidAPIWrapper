#' Death Rate
#'
#' @param country This should be the 2-letter country code for the country of interest.
#' If no country is specified, the function returns a data frame of the death rates for all countries.
#'
#' @return This function returns the Covid-19 death rate (percentage) for the country inputted by the end user.
#' @export
#'
#' @examples Death_Rate("US")
#' ##to find the death rate in the United States.
#'
Death_Rate <- function(country = df$CountryCode) {
  CovidURL = paste("https://api.covid19api.com/summary")
  Covidquery = jsonlite::fromJSON(CovidURL)
  info = Covidquery$Countries
  df = data.frame(Country = info$Country, CountryCode = info$CountryCode,
                  DeathRatePercent = (info$TotalDeaths/info$TotalConfirmed)*100)
  result = df[which(df$CountryCode == country), ]
  result
}
