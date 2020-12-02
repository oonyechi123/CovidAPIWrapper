#' New Case Country Statistics
#'
#' @param stat This should be the statistic the end user wants to analyze.
#' It can be "maximum", "minimum", "average" or "sum".
#'
#' @return The output depends on the stat parameter input.
#' If the stat is the maximum or minimum, the function outputs a data frame of the name and number of new Covid-19 cases for the country/countries with the most or fewest new cases, respectively.
#' If the stat is the average, it outputs the average number of new Covid-19 cases by a world country.
#' If the stat is the sum, it outputs the sum of all the world countries' new Covid-19 cases.
#' @export
#'
#' @examples New_Case_Country_Stats("sum")
#' ##to find the number of new Covid-19 cases in the world.
#'
New_Case_Country_Stats <- function(stat) {
  CovidURL = paste("https://api.covid19api.com/summary")
  Covidquery = jsonlite::fromJSON(CovidURL)
  info = Covidquery$Countries
  df = data.frame(Country = info$Country, CountryCode = info$CountryCode,
                  NewConfirmed = info$NewConfirmed)
  if (stat == "minimum") {
    result = df[which(df$NewConfirmed == min(df$NewConfirmed)), ]
  }

  if (stat == "maximum") {
    result = df[which(df$NewConfirmed == max(df$NewConfirmed)), ]
  }

  if (stat == "average") {
    result = mean(df$NewConfirmed)
  }

  if (stat == "sum") {
    result = sum(df$NewConfirmed)
  }
  result
}
