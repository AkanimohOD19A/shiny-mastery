#' Score a single transaction
score_one <- function(amount, velocity_5m, is_international) {
  base <- pmin(amount / 1000, 40)              # 0-40
  vel  <- pmin(velocity_5m * 4, 40)            # 0-40
  intl <- if (is_international) 20 else 0      # 0 or 20
  round(pmin(base + vel + intl, 100))
}

score_transactions <- function(df) {
  df$risk_score <- mapply(
    score_one,
    df$amount, df$velocity_5m, df$is_international
  )
  df$velocity_flag <- df$velocity_5m >= 5
  df
}

#' Apply analyst-defined thresholds; returns rows to triage
apply_thresholds <- function(df, th) {
  keep <- df$risk_score >= th$risk_cutoff |
    df$velocity_5m >= th$velocity_cutoff |
    (th$flag_international & df$is_international)
  df[keep, , drop = FALSE]
}

simulate_transactions <- function(n = 5) {
  data.frame(
    id               = paste0("TXN", sprintf("%06d", sample(1e6, n))),
    amount           = round(runif(n, 5, 5000), 2),
    velocity_5m      = sample(1:15, n, replace = TRUE),
    is_international = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(.2, .8)),
    timestamp        = Sys.time() - runif(n, 0, 3600),
    stringsAsFactors = FALSE
  )
}