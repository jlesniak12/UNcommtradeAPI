
#function to take in years and months and output a list formmated for the UN COMMTRADe API

#'Time Conversion
#'
#'Function to take in years and months desired and output a list in the form of YYYYMM
#'
#' @param years A list containing desired years entered as text elements ie the string "2012"
#' @param months A list containing desired months as text elements of the form MM (01-12) ie the string "01" represents January and so on.
#'
#' @return a list of dates formated YYYYMM
#' @export
#'
#' @examples
#'   #Outputs a list of all first quarter months 2010 - 2020 formatted YYYYMM
#'   time_conversion(years = list("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020") months = list("01", "02", "03"))



time_conversion <- function(years, months) {

  month_list = list()
  for (i in 1:length(years)) {
    for (j in 1:length(months)) {

      yearmonth = str_c(years[[i]], months[[j]], sep="")
      month_list <- list.append(month_list, yearmonth)
    }
  }
  return(month_list)
}
