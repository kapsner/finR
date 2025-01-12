
query_data <- function(ric, src, from_date = Sys.Date() - 500, to_date = Sys.Date()) {
  success <- FALSE
  counter <- 0
  while (!success & counter < 10) {
    sleeping <- sample(1:1000/1000, size = 1)
    counter <- tryCatch(
      expr = {
        message(sprintf("Try #%s -- Getting symbol: '%s'...", counter, ric))
        dataset <- quantmod::getSymbols(
          Symbols = ric,
          src = src,
          auto.assign = FALSE,
          from = from_date,
          to = to_date
        ) |> #s, periodicity = "intraday") |>
          na.omit()
        success <- TRUE
      },
      error = function(e) {
        message(e)
        counter <- sum(counter, 1)
        message(sprintf("\nWaiting %s seconds.", sleeping))
        Sys.sleep(sleeping)
        counter
      },
      finally = function() {
        counter
      }
    )
  }
  return(dataset)
}

relative_distance <- function(numerator, denominator) {
  round((1 - (numerator / denominator)) * 100, 2)
}

xts_close_to_datatable <- function(dataset) {
  xts_datatable <- data.table::data.table(
    date = xts::.indexDate(dataset) |> as.Date(origin = "1970-01-01"),
    close = quantmod::Cl(dataset) |> as.numeric()
  )
  return(xts_datatable)
}

compute_ema <- function(dataset, ema_timepoints = c("200", "100", "50" , "21", "9")) {
  xts_datatable <- xts_close_to_datatable(dataset)
  # reduce to valid stocks
  ema_timepoints <- ema_timepoints[nrow(xts_datatable) >= as.integer(ema_timepoints)]
  xts_datatable[, (paste0("EMA_", ema_timepoints)) := lapply(
    ema_timepoints,
    function(x) {
      TTR::EMA(get("close"), n = as.integer(x))
    }
  )]
  return(xts_datatable)
}

