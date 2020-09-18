#function to conduct multiple sequential queries of UN COMMTRADE API







time_period <- time_conversion(years = list("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"), months = list("01", "02", "03"))



list_dfs <- list()  #list to store downloaded files
list_meta <-list()

#loop to call api once for each time period

for (t in 1:length(time_period)) {

  Sys.sleep(30) #this 30 second pause ensures a max of 120 requests per hour and is easier on their API

  download <- get_data( trade_flow ="1", reporter = "156", partner = "344", frequency = "M", time_period = time_period[[t]], class = "HS", codes = "AG6" )


  #if function returns integer code rather than list there was a connection issue. Need to try again or pause program execution.
  if (is.integer(download)) {

    # try 4 more times to rule out a random API glitch
    counter = 0
    while (counter <4 ) {
      download <- get_data( trade_flow ="1", reporter = "156", partner = "344", frequency = "M", time_period = time_period[[t]], class = "HS", codes = "AG6" )

      if (is.list(download)) {
        break
      }
      counter = counter + 1
    }

    #if 409 http status returned after 4 tries likely means usage limit of 100 query per hour was hit. Wait one hour and try again
    if (is.integer(download)) {
      if (download == 409) {
        print("Program returned 409 error which is usually releated to exceeding the 100 query per hour limit. Waiting 1 hour before trying again........")
        Sys.sleep(3600)
        download <- get_data( trade_flow ="1", reporter = "842", partner = "156", frequency = "M", time_period = time_period[[t]], class = "HS", codes = "AG6" )

        if (is.integer(download)) {
          print("Even after waiting 1 hour there is still a connection issue. Aborting Query.......")
          break
        }

      }
    }
  }


  #if no data need to turn empty list into empty dataframe to combine with others
  if (length(download[["dataset"]]) == 0) {
    download[["dataset"]] <- as.data.frame(download[["dataset"]])
  }

  #store data in a list of dataframes
  len_dfs <- length(list_dfs)
  list_dfs[[len_dfs+1]] = download[["dataset"]]


  #store information about each query and result

  #strip out dataset and make a dataframe from function output
  x <- enframe(download[names(download) %in% "dataset" == FALSE])
  x <- pivot_wider(x, names_from = name, values_from= value)


  len_meta <- length(list_meta)
  list_meta[[len_meta + 1]] <- x



}

#combine data frames into one object
combined_dfs <- rbind.fill(list_dfs)

#combine metadata into one object
combined_results <- rbind.fill(list_meta)

setwd('D:/HK Trade Project/raw data')
save(combined_dfs, file= "US_imports_from_China_6digit_Monthly_2010_2020.RDS")

save(combined_results, file ="METADATA_US_imports_from_China_6digit_Monthly_2010_2020.RDS")

