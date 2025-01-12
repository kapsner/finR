
rsi_signal <- function(signal_dat) {
  outlist <- list(
    signal = ifelse(
      test = signal_dat[nrow(signal_dat), get("RSI")] < 35,
      yes = TRUE,
      no = FALSE
    ),
    value = as.character(signal_dat[nrow(signal_dat), get("RSI")] |> round(2))
  )
  return(outlist)
}

ema_crossing_signal <- function(signal_dat, cutoff = 1.2) {
  last_ema_row <- relations_of_emas(signal_dat)
  # distance between two emas < cutoff
  last_ema_row_abs <- abs(last_ema_row)
  crossing_signal <- last_ema_row_abs < cutoff

  signal_above <- intersect(
    colnames(last_ema_row)[last_ema_row > 0],
    colnames(last_ema_row)[crossing_signal]
  )
  signal_below <- intersect(
    colnames(last_ema_row)[last_ema_row <= 0],
    colnames(last_ema_row)[crossing_signal]
  )

  outlist <- list(
    signal = any(crossing_signal),
    signal_above = ifelse(
      test = length(unique(signal_above)) > 0,
      yes = signal_above,
      no = ""
    ),
    signal_below = ifelse(
      test = length(unique(signal_below)) > 0,
      yes = signal_below,
      no = ""
    )
  )
  return(outlist)
}

relations_of_emas <- function(signal_dat) {
  last_row <- signal_dat[
    nrow(signal_dat),
    .(
      "21 : 9" = relative_distance(get("EMA_9"), get("EMA_21")),
      "50 : 9" = relative_distance(get("EMA_9"), get("EMA_50")),
      "100 : 9" = relative_distance(get("EMA_9"), get("EMA_100")),
      "200 : 9" = relative_distance(get("EMA_9"), get("EMA_200")),
      "50 : 21" = relative_distance(get("EMA_21"), get("EMA_50")),
      "100 : 21" = relative_distance(get("EMA_21"), get("EMA_100")),
      "200 : 21" = relative_distance(get("EMA_21"), get("EMA_200")),
      "100 : 50" = relative_distance(get("EMA_50"), get("EMA_100")),
      "200 : 50" = relative_distance(get("EMA_50"), get("EMA_200")),
      "200 : 100" = relative_distance(get("EMA_100"), get("EMA_200"))
    )
  ]
  return(last_row)
}
