
generate_chart <- function(ric, src, title, out_dir) {
  message(ric)
  dataset <- quantmod::getSymbols(
    Symbols = ric,
    src = src,
    auto.assign = FALSE,
    from = Sys.Date() - 500,
    to = Sys.Date()
  ) |>
    na.omit()
  n_rows <<- nrow(dataset)
  message(paste0("Rows: ", n_rows))
  if (n_rows < 9) {
    stop("Number of datapoints < 9, abort")
  }
  ta <- '
    quantmod::addEMA(n = 9, col = "darkgray");
    quantmod::addBBands(n = pmin(20, n_rows), sd = 2);
    quantmod::addVo();
    quantmod::addMACD(fast = pmin(12, n_rows), slow = pmin(26, n_rows), signal = 9, col = c("green", "red", "black", "blue"));
    quantmod::addRSI(n = pmin(14, n_rows));
    quantmod::addROC(n = 7)
    '
  if (n_rows > 21) {
    ta <- paste0(
      ta,
      # EMA 21
      ';quantmod::addEMA(n = 21, col = "blue")'
    )
  }
  if (n_rows > 50) {
    ta <- paste0(
      ta,
      # EMA 50
      ';quantmod::addEMA(n = 50, col = "darkgreen")'
    )
  }
  if (n_rows > 100) {
    ta <- paste0(
      ta,
      # EMA 100
      ';quantmod::addEMA(n = 100, col = "red")'
    )
  }
  if (n_rows > 200) {
    ta <- paste0(
      ta,
      # EMA 200
      ';quantmod::addEMA(n = 200, col = "orange")'
    )
  }
  if (src == "yahoo") {
    ta <- paste0(ta, ';\nquantmod::addSAR(col = "darkgray")')
    quantmod::chartSeries(
      dataset,
      name = paste0(ric, ": ", title),
      type = "candle",
      subset = "last 6 months",
      theme = quantmod::chartTheme("white"),
      up.col = "green",
      dn.col = "red",
      plot = TRUE,
      TA = ta,
      TAsep = ";"
    )
  } else if (src == "FRED") {
    quantmod::chartSeries(
      dataset,
      name = paste0(ric, ": ", title),
      type = "line",
      subset = "last 6 months",
      theme = quantmod::chartTheme("white", up.col = "black"),
      plot = TRUE,
      TA = ta,
      TAsep = ";"
    )
  }
  Sys.sleep(3)
  message("Plotting finished")

  fn <- file.path(
    path.expand(out_dir),
    gsub("[[:punct:]]", "_", ric)
  )

  save_args <- list(
    file = paste0(fn, "_a.pdf"),
    paper = "a4",
    height = 10
  )

  # save chart
  do.call(quantmod::saveChart, save_args)
  Sys.sleep(3)

  # zoom
  quantmod::zoomChart(subset = "last 6 weeks")
  Sys.sleep(3)

  save_args <- list(
    file = paste0(fn, "_zoom.pdf"),
    paper = "a4",
    height = 10
  )

  # save chart
  do.call(quantmod::saveChart, save_args)
}
