
close_relative_to_ema <- function(vector, last_close) {
  round((1 - (vector / last_close)) * 100, 2)
}

xts_close_to_datatable <- function(dataset) {
  xts_datatable <- data.table::data.table(
    date = xts::.indexDate(dataset) |> as.Date(origin = "1970-01-01"),
    close = quantmod::Cl(dataset) |> as.numeric()
  )
  return(xts_datatable)
}

compute_relative_to_ema <- function(dataset) {
  cutoff_date <- Sys.Date() - 1 - 366/2

  xts_datatable <- xts_close_to_datatable(dataset)
  ema_timepoints <- c("200", "100", "50" , "21", "9")
  # reduce to valid stocks
  ema_timepoints <- ema_timepoints[nrow(xts_datatable) >= as.integer(ema_timepoints)]
  xts_datatable[, (paste0("EMA_", ema_timepoints)) := lapply(
    ema_timepoints,
    function(x) {
      TTR::EMA(get("close"), n = as.integer(x))
    }
  )]

  rel_to_ema <- xts_datatable[
    ,
    close_relative_to_ema(.SD, get("close")),
    .SDcols = grep("EMA", colnames(xts_datatable), value = TRUE)
  ]
  colnames(rel_to_ema) <- paste0("rel_", colnames(rel_to_ema))
  xts_datatable <- cbind(xts_datatable, rel_to_ema)

  return(xts_datatable[get("date") >= cutoff_date, ])
}


index_query_wrapper <- function(ticker_symbols) {
  ret_dat <- sapply(
    ticker_symbols,
    function(x) {
      relative_to_ema(x, src = "yahoo")
    },
    simplify = FALSE
  ) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE)
}

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

relative_to_ema <- function(ric, src) {

  dataset <-  query_data(ric, src)
  ret_dat <- compute_relative_to_ema(dataset)

  # select columns
  vec <- c("date", grep("rel_EMA", x = colnames(ret_dat), value = TRUE))
  return(cbind(stock = ric, ret_dat[, vec, with = FALSE]))
}

gt_zero <- function(vector) {
  ifelse(vector > 0, 1, 0)
}

pct_of_above_ema <- function(vector, n_base) {
  round((sum(vector, na.rm = TRUE) / n_base) * 100, 2)
}

greater_than_ema_counts <- function(index_data) {
  all_relative2 <- cbind(
    index_data[, 1:2],
    index_data[
      ,
      lapply(.SD, gt_zero),
      .SDcols = grep("rel_EMA", x = colnames(index_data), value = TRUE)
    ]
  )

  n_stocks <- length(unique(all_relative2$stock))

  all_counts <- all_relative2[
    ,
    lapply(.SD, pct_of_above_ema, n_base = n_stocks),
    .SDcols = grep("rel_EMA", x = colnames(index_data), value = TRUE),
    by = "date"
  ]

  all_counts_long <- data.table::melt.data.table(
    data = all_counts,
    id.vars = "date",
    measure.vars = grep("rel_EMA", x = colnames(all_counts), value = TRUE),
    variable.name = "EMA",
    value.name = "value"
  )
  all_counts_long$EMA <- factor(gsub(
    pattern = "rel_EMA_",
    replacement = "",
    x = all_counts_long$EMA
  ), levels = c("9", "21", "50", "100", "200"))
  return(all_counts_long)
}

ema_plot <- function(data, index_name) {
  ret <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = date, y = value, color = EMA)
  ) +
    ggplot2::geom_hline(yintercept = 90, linetype = 'dotted', col = 'red') +
    ggplot2::geom_hline(yintercept = 50, linetype = 'dashed', col = 'gray') +
    ggplot2::geom_hline(yintercept = 10, linetype = 'dotted', col = 'red') +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', col = 'black') +
    ggplot2::geom_hline(yintercept = -10, linetype = 'dotted', col = 'red') +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(
      values = c("black", "darkgray", "blue", "darkgreen", "red", "orange")
    ) +
    ggplot2::ylim(pmin(-10, min(data[get("EMA") == "index", get("value")])), 100) +
    ggplot2::ggtitle(index_name, subtitle = paste0(
      "Pct. of stocks above EMA (", min(data$date), " to ", max(data$date), ")"
    )) +
    ggplot2::ylab("Pct. of stocks above EMA") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::theme_classic()
  return(ret)
}

get_index_data <- function(ric, min_date) {
  index_ric <- switch(
    EXPR = ric,
    "dax" = "^GDAXI",
    "nasdaq" = "^IXIC",
    "snp500" = "^GSPC",
    "dowjones" = "^DJI"
  )
  index_data <- query_data(ric = index_ric, src = "yahoo")
  index_data <- xts_close_to_datatable(index_data)
  index_data_long <- index_data_to_long(index_data, min_date, norm = -100)
  return(index_data_long)
}

index_data_to_long <- function(index_data, min_date, norm) {
  index_data <- index_data[get("date") >= min_date, ]
  index_data[,  `:=` (
    index = get("close") / index_data[1, 2] |> as.numeric() * 100 |> round(digits = 0)
  )]
  index_data[, ("index") := .(get("index") + norm)]
  index_data_long <- data.table::melt.data.table(
    data = index_data,
    id.vars = "date",
    measure.vars = "index",
    variable.name = "EMA",
    value.name = "value"
  )
  return(index_data_long)
}

combine_data <- function(all_counts_long, index_data_long) {
  data_combined <- data.table::rbindlist(
    l = list(index_data_long, all_counts_long),
    use.names = TRUE
  )
  data_combined$EMA <- factor(gsub(
    pattern = "rel_EMA_",
    replacement = "",
    x = data_combined$EMA
  ), levels = c("index", "9", "21", "50", "100", "200"))
  return(data_combined)
}
