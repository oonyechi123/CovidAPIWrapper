#' New Recoveries Country Statistics
#'
#' @param stat This should be the statistic the end user wants to analyze.
#' It can be "maximum", "minimum", "average" or "sum".
#'
#' @return The output depends on the stat parameter input.
#' If the stat is the maximum or minimum, the function outputs a data frame of the name and number of new Covid-19 recoveries for the country/countries with the most or fewest new recoveries, respectively.
#' If the stat is the average, it outputs the average number of new Covid-19 recoveries by a world country.
#' If the stat is the sum, it outputs the sum of all the world countries' new Covid-19 recoveries.
#' @export
#'
#' @examples New_Recoveries_Country_Stats("average")
#' ##to find the average number of new Covid-19 recoveries.
#'
New_Recoveries_Country_Stats <- function(stat) {
  CovidURL = paste("https://api.covid19api.com/summary")
  Covidquery = jsonlite::fromJSON(CovidURL)
  info = Covidquery$Countries
  df = data.frame(Country = info$Country, CountryCode = info$CountryCode,
                  NewRecovered = info$NewRecovered)
  if (stat == "minimum") {
    result = df[which(df$NewRecovered == min(df$NewRecovered)), ]
  }

  if (stat == "maximum") {
    result = df[which(df$NewRecovered == max(df$NewRecovered)), ]
  }

  if (stat == "average") {
    result = mean(df$NewRecovered)
  }

  if (stat == "sum") {
    result = sum(df$NewRecovered)
  }
  result
}
