

#function to take in parameters and return a dataframe with data availability information from UN COMMTRADE API


#'Check Available Trade Data
#'
#'This function checks the UN COMMTRADE API to see if requested trade data is available for download.
#'
#' @param type Refers to type of trade and takes a character string. Takes values of "C" (commodities), "S"(Services) or "any" (all trade). Default Value is "all"
#' @param frequency Refers to the frequency of reporting and takes a character string. Takes values of "A" (annual trade), "M" (monthly trade) or "all" (any trade). Default is "all"
#' @param time_period Refers to the time periods desired and takes a character string. Takes values of the format "YYYY" for years or "YYYYMM" for specific months or "all" all years. Can take a maximum of 5 entries of specific dates entered as a single comma seperated character string. Default is "all"
#' @param reporter character string. takes on the UN numeric country code for a country or the code "all" (all countries).   see (https://comtrade.un.org/Data/cache/reporterAreas.json).
#'  Can take a maximum of 5 specific country codes entered as a single comma separated character string. Default is "all".
#' @param classification Refers to the classification scheme used to define traded products and takes a character string. For commodities trade can be it "HS" (harmonized trade or HS classification), "ST" (standard international trade classification or SITC classification), or"BEC" (Broad economic catagories).
#' Can also enter specific revision numbers (ie "S1" for SITC1 or "H1" for HS1). For service trade it can be "EB02" (Extended Balance of Payments Services Classification).
#' Default is "all" which returns available data in all classifications.

#'
#' @return A tidy dataframe containing metadata about the requested records
#' @export
#'
#' @examples
#'
#'#returns information on all available trade data due to default parameters
#' default <- check_data()
#'
#'#returns information on annual data reported by the US and China between 2015-2019
#' specific_query<- check_data(type = "C", frequency = "A", reporter = "842,156", time_period = "2015,2016,2017,2018,2019")
#'
#'
#'
#' @seealso Documents for UN COMMTRADE API. Available at \url{https://comtrade.un.org/Data/Doc/API}
#'

check_data <- function(type = "all", frequency = "all", time_period = "all", reporter = "all",  classification = "all")   {

  datacheck_url = "https://comtrade.un.org/api/refs/da/view?"
  result <- GET(datacheck_url, query = list(type = type, freq = frequency, ps = time_period, r = reporter,  px = classification))

  #check that connection was achieved to the server and if not leave the function
  if (status_code(result) != 200) {
    print(paste("Something went wrong with connection. HTTP Status Code is", toString(status_code(result)), "Please try again.", sep=" "))

    return(NULL)
  }

  #convert results to a dataframe
  df <- fromJSON(content(result, "text"))

  #check if there is anything in results. If not leave function  and return nothing.
  if (length(df)==0) {
    print("No data avaialble for selected parameters. Please review your query.")

    return(NULL)
  }

  df <- arrange(df, type, freq, px, rDesc, ps)

  return(df)

}

