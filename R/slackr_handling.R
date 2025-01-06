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
