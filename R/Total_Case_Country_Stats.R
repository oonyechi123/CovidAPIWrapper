#' Total Case Country Statistics
#'
#' @param stat This should be the statistic the end user wants to analyze.
#' It can be "maximum", "minimum", "average" or "sum".
#'
#' @return The output depends on the stat parameter input.
#' If the stat is the maximum or minimum, the function outputs a data frame of the name and total Covid-19 cases for the country/countries with the most or fewest total cases, respectively.
#' If the stat is the average, it outputs the average total Covid-19 cases by a world country.
#' If the stat is the sum, it outputs the sum of all the world countries' total Covid-19 cases.
#' @export
#'
#' @examples Total_Case_Country_Stats("maximum")
#' ##to find the country with the most total Covid-19 cases.
#'
Total_Case_Country_Stats <- function(stat) {
  CovidURL = paste("https://api.covid19api.com/summary")
  Covidquery = jsonlite::fromJSON(CovidURL)
  info = Covidquery$Countries
  df = data.frame(Country = info$Country, CountryCode = info$CountryCode,
                  TotalConfirmed = info$TotalConfirmed)
  if (stat == "minimum") {
    result = df[which(df$TotalConfirmed == min(df$TotalConfirmed)), ]
  }

  if (stat == "maximum") {
    result = df[which(df$TotalConfirmed == max(df$TotalConfirmed)), ]
  }

  if (stat == "average") {
    result = mean(df$TotalConfirmed)
  }

  if (stat == "sum") {
    result = sum(df$TotalConfirmed)
  }
  result
}
