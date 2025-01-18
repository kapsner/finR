# https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/
# install.packages("quantmod")


base <- here::here()

source(here::here(base, "R", "general_functions.R"))
source(here::here(base, "R", "slackr_handling.R"))
source(here::here(base, "R", "generate_chart.R"))
source(here::here(base, "R", "relative_ema_computations.R"))
source(here::here(base, "R", "query_database.R"))
source(here::here(base, "R", "signals.R"))

out_dir <- here::here(base, "output")
reports_dir <- here::here(base, "reports")

slackr_login()

# reset start_time
start_time <- Sys.time()

success_init <- FALSE
connect_counter <- 0
while (!success_init & connect_counter < 10) {
  sleeping <- sample(1:1000/1000, size = 1)
  connect_counter <- tryCatch(
    expr = {
      source("https://raw.githubusercontent.com/kapsner/miscellaneous2/main/r2source/ric.R")
      success_init <- TRUE
    },
    error = function(e) {
      message(e)
      connect_counter <- sum(connect_counter, 1)
      message(sprintf("\nWaiting %s seconds.", sleeping))
      Sys.sleep(sleeping)
      connect_counter
    },
    finally = function() {
      connect_counter
    }
  )
}

looplist <- list(
  market = list3,
  watch = list1,
  portfolio = list2
)


# reset start_time
start_time <- Sys.time()

for (l in names(looplist)) {

  ric_list <- looplist[[l]]

  signal_msg <- "Signals:"
  signal_rsi <- "\n---\nRSI < 35:"
  signal_50_below_9 <- "\n\n---\nEMA50 near EMA9 (below):"
  signal_50_above_9 <- "\n\n---\nEMA50 near EMA9 (above):"
  signal_200_below_50 <- "\n\n---\nEMA200 near EMA50 (below):"
  signal_200_above_50 <- "\n\n---\nEMA200 near EMA50 (above):"

  # dblist-handling
  dblist_fn <- here::here(base, "dblist.rds")
  if (file.exists(dblist_fn)) {
    dblist <- readRDS(file = dblist_fn)
  } else {
    dblist <- list()
  }


  lapply(
    X = names(ric_list),
    FUN = function(ric) {
      dataset <- query_cache(
        dblist = dblist,
        ric = ric,
        src = ric_list[[ric]][["src"]],
        from_date = Sys.Date() - 3000,
        to_date = Sys.Date()
      )

      # update dblist
      if (!(ric %in% names(dblist)) || nrow(dblist[[ric]]) < nrow(dataset)) {
        dblist[[ric]] <<- dataset
      }

      generate_chart(
        dataset = dataset,
        ric = ric,
        src = ric_list[[ric]][["src"]],
        title = ric_list[[ric]][["name"]],
        out_dir = out_dir
      )

      if (l != "market") {
        # signal computations
        signal_dat <- compute_ema(dataset)
        signal_dat[, ("RSI") := TTR::RSI(
          price = get("close"),
          n = 14,
          maType = "EMA",
          wilder = TRUE
        )]
        signal_dat <- na.omit(signal_dat)

        # RSI
        rsi_sig <- rsi_signal(signal_dat)
        if (isTRUE(rsi_sig$signal)) {
          signal_rsi <<- paste0(
            signal_rsi,
            "\n- ", ric_list[[ric]][["name"]], sprintf(" (%s)", rsi_sig$value)
          )
        }

        # EMA crossing
        crossing_sig <- ema_crossing_signal(signal_dat)
        if (isTRUE(crossing_sig$signal)) {
          if (grepl(pattern = "50 : 9", x = crossing_sig$signal_below)) {
            signal_50_below_9 <<- paste0(
              signal_50_below_9,
              "\n- ", ric_list[[ric]][["name"]]
            )
          }
          if (grepl(pattern = "50 : 9", x = crossing_sig$signal_above)) {
            signal_50_above_9 <<- paste0(
              signal_50_above_9,
              "\n- ", ric_list[[ric]][["name"]]
            )
          }
          if (grepl(pattern = "200 : 50", x = crossing_sig$signal_below)) {
            signal_200_below_50 <<- paste0(
              signal_200_below_50,
              "\n- ", ric_list[[ric]][["name"]]
            )
          }
          if (grepl(pattern = "200 : 50", x = crossing_sig$signal_above)) {
            signal_200_above_50 <<- paste0(
              signal_200_above_50,
              "\n- ", ric_list[[ric]][["name"]]
            )
          }
        }
      }
    }
  )

  # save list
  saveRDS(object = dblist, file = dblist_fn)

  slackr_signal_evaluation(
    signal_msg = signal_msg,
    signal_rsi = signal_rsi,
    signal_50_below_9 = signal_50_below_9,
    signal_50_above_9 = signal_50_above_9,
    signal_200_below_50 = signal_200_below_50,
    signal_200_above_50 = signal_200_above_50
  )

  slackr_pdf_upload(
    name = paste0(l, "_list"),
    out_dir = out_dir,
    reports_dir = reports_dir,
    slackr_channels = "#random"
  )
}
