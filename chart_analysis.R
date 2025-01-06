# https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/
# install.packages("quantmod")


base <- here::here()

source(here::here(base, "R", "slackr_handling.R"))
source(here::here(base, "R", "generate_chart.R"))
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

  lapply(names(ric_list), function(x) {
    success_try <- FALSE
    try_counter <- 0
    while (!success_try & try_counter < 10) {
      sleeping <- sample(1:1000/1000, size = 1)
      try_counter <- tryCatch(
        expr = {
          generate_chart(
            ric = x,
            src = ric_list[[x]][["src"]],
            title = ric_list[[x]][["name"]],
            out_dir = out_dir
          )
          success_try <- TRUE
        },
        error = function(e) {
          message(e)
          try_counter <- sum(try_counter, 1)
          message(sprintf("\nWaiting %s seconds.", sleeping))
          Sys.sleep(sleeping)
          try_counter
        },
        finally = function() {
          try_counter
        }
      )
    }
  })

  slackr_pdf_upload(
    name = paste0(l, "_list"),
    out_dir = out_dir,
    reports_dir = reports_dir,
    slackr_channels = "#random"
  )
}
