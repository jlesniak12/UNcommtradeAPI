
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

