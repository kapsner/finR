
query_cache <- function(dblist, ric, src, from_date = Sys.Date() - 3000, to_date = Sys.Date()) {
  add_data_query <- TRUE

  if (ric %in% names(dblist)) {
    dataset <- dblist[[ric]]
    last_date <- xts::.indexDate(dataset)[nrow(dataset)] |> as.Date(origin = "1970-01-01")
    if (last_date < Sys.Date()) {
      from_date <- last_date + 1
      if (from_date < Sys.Date()) {
        add_data_query <- TRUE
      } else {
        add_data_query <- FALSE
      }
    } else {
      add_data_query <- FALSE
    }
  }

  if (isTRUE(add_data_query)) {
    add_new_data <- query_data(
      ric = ric,
      src = src,
      from_date = from_date,
      to_date = to_date
    )
    if (ric %in% names(dblist)) {
      if (nrow(add_new_data) > 0) {
        message(sprintf("Adding %s rows to database", nrow(add_new_data)))
        dataset <- xts::rbind.xts(dataset, add_new_data)
      }
    } else {
      dataset <- add_new_data
    }
  }
  return(dataset)
}
