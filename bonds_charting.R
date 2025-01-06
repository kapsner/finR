# 10-yr treasury
ric <- c("^TNX", "DBXG.DE")
quantmod::getSymbols(
  Symbols = ric,
  auto.assign = TRUE,
  from = Sys.Date() - 500,
  to = Sys.Date() - 1
) |> #s, periodicity = "intraday") |>
  na.omit()

quantmod::getSymbols(
  Symbols = "DGS10",
  auto.assign = TRUE,
  from = Sys.Date() - 500,
  to = Sys.Date() - 1,
  src = "FRED"
) |> #s, periodicity = "intraday") |>
  na.omit()

tnx <- xts_close_to_datatable(TNX)
dbxg <- xts_close_to_datatable(DBXG.DE)
dsg <- data.table::as.data.table(DGS10)
colnames(dsg) <- c("date", "close")

dataset <- data.table::merge.data.table(
  x = dsg,
  y = dbxg,
  by = "date",
  all = TRUE
) |> na.omit()

colnames(dataset)[2:3] <- c("TNX", "DBXG")

dataset[,  `:=` (
  tnx_rel = get("TNX") / dataset[1, 2] |> as.numeric() * 100 |> round(0),
  dbxg_rel = get("DBXG") / dataset[1, 3] |> as.numeric() * 100 |> round(0)
)]
colnames(dataset)[4:5] <- c("US10yr treasury", "Eurozone Government Bond 25+")

dat_long <- data.table::melt.data.table(
  data = dataset,
  id.vars = "date",
  measure.vars = colnames(dataset)[4:5]
)

ggplot2::ggplot(
  data = dat_long,
  mapping = ggplot2::aes(x = date, y = value, color = variable)
) +
  ggplot2::geom_line() +
  ggpubr::theme_pubclean() +
  ggplot2::theme(legend.title = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank()) +
  ggplot2::ggtitle(
    label = "10yr US treasury interest rates vs. Xtrackers Eurozone Government Bond 25+",
    subtitle = paste0("100 = ", dataset[1, get("date")])
  )
