#' Total Recoveries Country Statistics
#'
#' @param stat This should be the statistic the end user wants to analyze.
#' It can be "maximum", "minimum", "average" or "sum".
#'
#' @return The output depends on the stat parameter input.
#' If the stat is the maximum or minimum, the function outputs a data frame of the name and total Covid-19 recoveries for the country/countries with the most or fewest total recoveries, respectively.
#' If the stat is the average, it outputs the average total Covid-19 recoveries by a world country.
#' If the stat is the sum, it outputs the sum of all the world countries' total Covid-19 recoveries.
#' @export
#'
#' @examples Total_Recoveries_Country_Stats("sum")
#' ##to find the total number of Covid-19 recoveries in the world.
#'
Total_Recoveries_Country_Stats <- function(stat) {
  CovidURL = paste("https://api.covid19api.com/summary")
  Covidquery = jsonlite::fromJSON(CovidURL)
  info = Covidquery$Countries
  df = data.frame(Country = info$Country, CountryCode = info$CountryCode,
                  TotalRecovered = info$TotalRecovered)
  if (stat == "minimum") {
    result = df[which(df$TotalRecovered == min(df$TotalRecovered)), ]
  }

  if (stat == "maximum") {
    result = df[which(df$TotalRecovered == max(df$TotalRecovered)), ]
  }

  if (stat == "average") {
    result = mean(df$TotalRecovered)
  }

  if (stat == "sum") {
    result = sum(df$TotalRecovered)
  }
  result
}
