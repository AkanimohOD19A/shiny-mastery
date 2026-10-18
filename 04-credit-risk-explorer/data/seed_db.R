# data/seed_db.R
# Run once: source("data/seed_db.R")
# Creates data/portfolio.db with a realistic loan-level dataset.

library(DBI)
library(RSQLite)

set.seed(2024)

con <- dbConnect(SQLite(), "data/portfolio.db")

# ---- Parameters ----
n_loans      <- 50000
vintages     <- seq(as.Date("2021-01-01"), as.Date("2024-06-01"), by = "quarter")
products     <- c("Personal", "Auto", "CreditCard", "Student")
regions      <- c("Northeast", "Southeast", "Midwest", "Southwest", "West")
risk_tiers   <- c("A", "B", "C", "D", "E")  # A = safest
max_mob      <- 36                            # months on book to simulate

# ---- Loans table ----
loans <- data.frame(
  loan_id       = sprintf("LN-%06d", 1:n_loans),
  vintage       = as.character(sample(vintages, n_loans, replace = TRUE)),
  product       = sample(products, n_loans, replace = TRUE,
                         prob = c(0.40, 0.25, 0.25, 0.10)),
  region        = sample(regions, n_loans, replace = TRUE),
  risk_tier     = sample(risk_tiers, n_loans, replace = TRUE,
                         prob = c(0.20, 0.25, 0.25, 0.20, 0.10)),
  fico_score    = NA_integer_,
  original_balance = NA_real_,
  ltv           = NA_real_,
  dti           = NA_real_,
  stringsAsFactors = FALSE
)

# FICO depends on tier (realistic ordering)
tier_fico <- c(A = 780, B = 720, C = 670, D = 620, E = 570)
loans$fico_score <- round(
  tier_fico[loans$risk_tier] + rnorm(n_loans, 0, 25)
)
loans$fico_score <- pmax(300, pmin(850, loans$fico_score))

loans$original_balance <- round(
  exp(rnorm(n_loans, log(15000), 0.8)), 2
)
loans$ltv <- round(runif(n_loans, 0.4, 1.1), 3)
loans$dti <- round(runif(n_loans, 0.05, 0.55), 3)

# ---- Performance table (loan x month-on-book) ----
# Delinquency probability rises with tier risk and months on book,
# then plateaus. This gives us nice vintage curves.
tier_base_dpd <- c(A = 0.008, B = 0.020, C = 0.045, D = 0.090, E = 0.170)

perf <- do.call(rbind, lapply(seq_len(nrow(loans)), function(i) {
  l  <- loans[i, ]
  # How many months has this loan been on book as of today?
  mob_end <- min(max_mob,
                 as.integer(difftime(as.Date("2024-12-31"), as.Date(l$vintage),
                                     units = "days")) %/% 30L)
  if (mob_end < 1) return(NULL)
  
  mob <- 1:mob_end
  base <- tier_base_dpd[l$risk_tier]
  # Aging curve: rises, then plateaus around month 18
  age_factor <- 0.3 + 0.7 * pmin(mob / 18, 1)
  dpd_prob   <- base * age_factor
  
  # Simulate first-time 30+ DPD flag per month
  dpd_30 <- rbinom(length(mob), 1, dpd_prob)
  # 90+ DPD is a fraction of 30+ DPD
  dpd_90 <- dpd_30 * rbinom(length(mob), 1, 0.45)
  # Charge-off is a fraction of 90+
  charge_off <- dpd_90 * rbinom(length(mob), 1, 0.30)
  
  # Outstanding balance amortizes
  balance <- l$original_balance * (1 - mob / (max_mob * 1.4))
  balance <- pmax(balance, 0)
  
  data.frame(
    loan_id       = l$loan_id,
    months_on_book = mob,
    balance       = round(balance, 2),
    dpd_30        = dpd_30,
    dpd_90        = dpd_90,
    charge_off    = charge_off
  )
}))

# ---- Write tables ----
dbWriteTable(con, "loans",       loans, overwrite = TRUE)
dbWriteTable(con, "performance", perf,  overwrite = TRUE)

# ---- Indexes (matters at 50k loans x ~20 months = ~1M perf rows) ----
dbExecute(con, "CREATE INDEX idx_loans_vintage   ON loans(vintage)")
dbExecute(con, "CREATE INDEX idx_loans_product   ON loans(product)")
dbExecute(con, "CREATE INDEX idx_loans_region    ON loans(region)")
dbExecute(con, "CREATE INDEX idx_loans_tier      ON loans(risk_tier)")
dbExecute(con, "CREATE INDEX idx_perf_loan       ON performance(loan_id)")
dbExecute(con, "CREATE INDEX idx_perf_mob        ON performance(months_on_book)")

cat("Seeded:", nrow(loans), "loans,", nrow(perf), "performance rows\n")
dbDisconnect(con)