



#return a subset of a list formatted for UN COMMTRADe API

list_sub <- function (start, winsize, list) {

  if (!(is.list(partner))) {
    print("Did not enter a valid list")
    return (NULL)
  }

  end <- start + winsize
  list_window <- partner[start:end] %>%
    discard(is.null)

  window_string <- str_c(list_window, collapse=",")

  return(window_string)
}
