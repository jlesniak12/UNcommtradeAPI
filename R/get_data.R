
####Function to take in parameters and make a single query request to UN COMMTRADE API.####

#Note:Function can return either a list containing meta data about the API call and data requested or the HTTP status code if connection error occured

#' Get Data from UN COMMTRADE API
#'
#' This function takes user input and creates a single query to the UN COMMTRADE API for the requested trade data.
#'
#' @param type Refers to type of trade and takes a character string. Takes values of "C" (commodities), "S"(Services) or "any" (all trade). Default Value is "C"
#' @param trade_flow  Refers to the specific trade flow (imports or exports) and takes a character string. Can be "1", "2", "3", or "4" for Imports, Exports Re-Exports, or Re-Imports respectively as well as "all" (all trade flows).
#' Can pass in multiple elements as a single comma separated character string. Default value is "1,2" for imports and exports.
#' @param frequency Refers to the frequency of reporting and takes a character string. Takes values of "A" (annual trade), "M" (monthly trade) or "all" (any trade).  Default is "A"
#' @param time_period Refers to the time periods desired and takes a character string. Takes values of the format "YYYY" for years or "YYYYMM" for specific months or "now" for the most recent month/year with data.
#' Can take a maximum of 5 entries of specific dates entered as a single comma separated character string. Default is "now"
#' @param classification Refers to the classification scheme used to define traded products and takes a character string. For commodities trade can be it "HS" (harmonized trade or HS classification), "ST" (standard international trade classification or SITC classification), or"BEC" (Broad economic catagories).
#' Can also enter specific revision numbers (ie "S1" for SITC1 or "H1" for HS1) For service trade it can be "EB02" (Extended Balance of Payments Services Classification).
#' It can also be "all" for all classifications.
#'Default is "HS"
#' @param codes Refers to the level of detail in the product classification and takes a character string. Refers to the desired level of detail and varies by classification.
#' Generally can be "ALL" (all codes in a classification), "TOTAL" (total trade between countries with no detail breakdown).
#' Specific levels of aggregation ie can enter (AG1 AG2 AG3 AG4 AG5 AG6) are available depending on classification choice.
#' It is also possible to enter commodity codes directly, up to a maximum of 20 (see classification scheme documents for codes).
#' Default is "TOTAL"
#' @param reporter Character string. takes on the UN numeric country code for a country or the code "all" (all countries). see (https://comtrade.un.org/Data/cache/reporterAreas.json)
#' Can take a maximum of 5 specific country codes entered as a single comma separated character string. Default is "all".
#' @param partner character string. takes on the UN numeric country code for a country or the code "all" (all countries).   see (https://comtrade.un.org/Data/cache/reporterAreas.json)
#' Can take a maximum of 5 specific country codes entered as a single comma separated character string. Default is "0" which is the numeric code for World.

#' @param max_records Character string. Parameter from the API determining the size of download. Default is set to 10000 which is max for public use of API without registration.

#' @param header Character string. Parameter regarding variable names. Default is H which is more human readable. Can be M which is easier for computer parsing.
#' @param format Character string. File format download. Default is json can be CSV. Do not change this.
#'
#' @return \strong{If query successful:} A named list containing information about the query as well as an element named "dataset" containing a dataframe of the returned data.
#' \strong{If connection error:} returns a number representing the HTTP status/error code.
#' @export
#'
#' @examples
#'
#' # With default parameters returns total imports and exports reported by all countries with the rest of the world in the most recent year.
#' default<- get_data()
#'
#' #returns US reported imports and exports with the rest of the world for each 2 digit level code from the Harmonized Trade System (HS) product classification in the most recent year.
#' specific_query <- get_data(code = "AG2", classification = "HS", reporter = "842)
#'
#' @seealso Documents for UN COMMTRADE API. Available at \url{https://comtrade.un.org/Data/Doc/API}
#'
get_data <- function(type = "C", trade_flow = "1,2", frequency = "A", time_period = "now", classification = "HS", codes = "TOTAL", reporter="all", partner = "0", max_records = "100000", header = "H", format="json") {
  data_grab_url = "https://comtrade.un.org/api/get?"


  #download data
  result <- GET(data_grab_url, query = list(type = type, rg= trade_flow, freq = frequency, ps = time_period, px = classification, cc = codes, r = reporter, p = partner, max = max_records, head = header, fmt = format))


  #check for the hourly limit and if so stop the function print message and return the HTTP status code
  if (status_code(result) == 409) {
    print("Code 409 likley means you have exceeded the limit of 100 requests per hour for the unregistered API users. Wait 1 hour and try again.")
    return(status_code(result))
  }

  #Check other errors related to connection and if so print appropriate message and return HTTP status code
  if (status_code(result) != 200) {
    print(paste("Something went wrong with connection. HTTP Status Code is", toString(status_code(result)), sep=" "))
    return(status_code(result))
  }

  #gather data returned into a usable format
  download <-fromJSON(content(result, "text"))


  ##Print URL to console and message indicating something was returned (no connection errors)###
  print(paste("Query executed for the url:", result$url, sep = " "))




  #errors reported from the UN validation object

  if (download$validation$status$value != 0) {
    print(paste("There was an error message in the validation object. The error code is", toString(download$validation$status$value),  "which refers to the issue of:", toString(download$validation$status$name), sep = " "))
    print(paste("The specific error message given by UN COMMTRADE with this query is:", download$validation$message, sep = " "))

    if(download$validation$status$value == 5003) {
      print(paste("This query exceeded the limit for unregistered users of 10000 observations. This query actually contained", toString(download$validation$count$value), "Total Records.", sep = " "))
    }
  }


  #check to see if any data was returned and if not print message and convert the empty list to a dataframe for consistency
  if (download$validation$count$value ==0) {
    print("No data returned. Check to see that data exists using checking data function or UN COMMTRADe website and review the query")
    download[["dataset"]] <- as.data.frame(download[["dataset"]])
  }

  cat("\n")

  #Prepare output list including meta data about the query as well as the dataset collected
  output <- list(url = result$url, dataset = download$dataset, total_records = download$validation$count$value, http_status = status_code(result),
                 UN_status_code = download$validation$status$value, UN_error_name = download$validation$status$name, UN_error_description = download$validation$status$description, UN_error_message = download$validation$message,
                 reporter_code = reporter, partner_code = partner, trade_flow = trade_flow, frequency = frequency, time = time_period, classification = classification, digit = codes )
  return(output)

}
