slackr_login <- function() {

  success <- FALSE
  counter <- 0
  while (!success & counter < 10) {
    sleeping <- sample(1:1000/1000, size = 1)
    counter <- tryCatch(
      expr = {
        message(sprintf("Try #%s: connecting to slackr", counter))
        slackr::slackr_setup(
          config_file = "./slackr"
        )
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
}

slackr_pdf_upload <- function(name, out_dir, reports_dir, slackr_channels = "#random") {

  available_pdfs <- sort(list.files(
    path = out_dir,
    pattern = ".pdf",
    full.names = TRUE)
  )

  out_fn <- file.path(
    reports_dir,
    paste0(name, "_", Sys.Date(), ".pdf")
  )
  qpdf::pdf_combine(
    input = available_pdfs,
    output = out_fn
  )
  slackr::slackr_upload(
    filename = out_fn,
    channels = slackr_channels
  )

  lapply(available_pdfs, unlink)
  unlink(out_fn)
}

slackr_signal_evaluation <- function(
    signal_msg,
    signal_rsi,
    signal_50_below_9,
    signal_50_above_9,
    signal_200_below_50,
    signal_200_above_50
  ) {

  if (signal_rsi != "\n---\nRSI < 35:") {
    signal_msg <- paste0(signal_msg, signal_rsi)
  }

  if (signal_50_below_9 != "\n\n---\nEMA50 near EMA9 (below):") {
    signal_msg <- paste0(signal_msg, signal_50_below_9)
  }

  if (signal_50_above_9 != "\n\n---\nEMA50 near EMA9 (above):") {
    signal_msg <- paste0(signal_msg, signal_50_above_9)
  }

  if (signal_200_below_50 != "\n\n---\nEMA200 near EMA50 (below):") {
    signal_msg <- paste0(signal_msg, signal_200_below_50)
  }

  if (signal_200_above_50 != "\n\n---\nEMA200 near EMA50 (above):") {
    signal_msg <- paste0(signal_msg, signal_200_above_50)
  }

  if (signal_msg != "Signals:") {
    slackr::slackr_msg(
      txt = signal_msg,
      channel = "#random"
    )
  }
}
