source(here::here("R", "general_functions.R"))
source(here::here("R", "relative_ema_computations.R"))
source(here::here("R", "slackr_handling.R"))

# csv_path <- "/home/user/development/git/miscellaneous2/r2source/fin"
# get_stocks(index = "nasdaq", csv_path = csv_path)
# get_stocks(index = "sup500", csv_path = csv_path)
# get_stocks(index = "dowjones", csv_path = csv_path)


start_time <- Sys.time()
time_limit <- 10*60 # 10 minutes

slackr_login()

base_url <- "https://raw.githubusercontent.com/kapsner/miscellaneous2/refs/heads/main/r2source/fin/"
indices <- c("dax", "nasdaq", "snp500", "dowjones")
debug <- FALSE

for (i in indices) {
  dataset <- data.table::fread(paste0(base_url, "ric_", i, ".csv"))
  if (isTRUE(debug)) {
    dataset <- dataset[1:10, ]
  }
  all_relative <- index_query_wrapper(dataset$ric)
  all_counts_long <- greater_than_ema_counts(all_relative)

  # get index_data
  index_data_long <- get_index_data(i, min_date = min(all_counts_long$date))
  data_combined <- combine_data(all_counts_long, index_data_long)
  final_plot_dax <- ema_plot(data_combined, toupper(i))

  ggplot2::ggsave(
    filename = paste0(i, "_ema.pdf"),
    plot = final_plot_dax,
    device = "pdf",
    path = here::here("output"),
    width = 10,
    height = 8,
    dpi = 300
  )

}

slackr_pdf_upload(
  name = "indices",
  out_dir = here::here("output"),
  reports_dir = here::here("reports"),
  slackr_channels = "#random"
)
