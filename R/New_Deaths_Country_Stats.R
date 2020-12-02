#' New Deaths Country Statistics
#'
#' @param stat This should be the statistic the end user wants to analyze.
#' It can be "maximum", "minimum", "average" or "sum".
#'
#' @return The output depends on the stat parameter input.
#' If the stat is the maximum or minimum, the function outputs a data frame of the name and number of new Covid-19 deaths for the country/countries with the most or fewest new deaths, respectively.
#' If the stat is the average, it outputs the average number of new Covid-19 deaths by a world country.
#' If the stat is the sum, it outputs the sum of all the world countries' new Covid-19 deaths.
#' @export
#'
#' @examples New_Deaths_Country_Stats("minimum")
#' ##to find the country with the fewest new Covid-19 deaths.
#'
New_Deaths_Country_Stats <- function(stat) {
  CovidURL = paste("https://api.covid19api.com/summary")
  Covidquery = jsonlite::fromJSON(CovidURL)
  info = Covidquery$Countries
  df = data.frame(Country = info$Country, CountryCode = info$CountryCode,
                  NewDeaths = info$NewDeaths)
  if (stat == "minimum") {
    result = df[which(df$NewDeaths == min(df$NewDeaths)), ]
  }

  if (stat == "maximum") {
    result = df[which(df$NewDeaths == max(df$NewDeaths)), ]
  }

  if (stat == "average") {
    result = mean(df$NewDeaths)
  }

  if (stat == "sum") {
    result = sum(df$NewDeaths)
  }
  result
}
