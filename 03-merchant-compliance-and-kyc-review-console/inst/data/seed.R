library(dplyr); library(lubridate)

if(!exists("compute_risk_score", mode = "function")) {
  source("R/compliance_rules.R")
}

# --- define the sanctions list FIRST ---
sanctions_sample <- tibble(
  name    = c("Silk Road Trading", "Falcon Food Group",
              "Helios Energy International", "Lumen Financial"),
  aliases = c("Silk Road Exports", "Falcon Foods", "Helios Energy", "Lumen Fintech"),
  source  = c("OFAC", "EU", "UN", "OFAC")
)

set.seed(42)
n <- 12
cases_seed <- tibble(
  case_id          = sprintf("KYC-%05d", 1001:(1000 + n)),
  merchant_name    = c("Northwind Trading Ltd", "Acme Payments GmbH",
                       "Silk Road Exports", "Delta Logistics",
                       "Emerald Retail", "Falcon Foods",
                       "Global Freight Co", "Helios Energy",
                       "Ionic Labs", "Juniper Consulting",
                       "Kite Analytics", "Lumen Fintech"),
  declared_mcc     = sample(c("5411", "5999", "7372", "6012"), n, TRUE),
  txn_mcc          = sample(c("5411", "5999", "7372", "6012"), n, TRUE),
  jurisdiction_risk= sample(c("low","medium","high"), n, TRUE),
  pep_hit          = sample(c(TRUE, FALSE), n, TRUE, prob = c(.15,.85)),
  sanctions_hit    = sample(c(TRUE, FALSE), n, TRUE, prob = c(.08,.92)),
  doc_quality      = sample(c("good","partial","poor"), n, TRUE),
  tenure_days      = sample(c(20, 60, 200, 500), n, TRUE),
  opened_at        = Sys.time() - hours(sample(1:200, n, TRUE)),
  status           = "new",
  assigned_to      = NA_character_,
  decision         = NA_character_
) |>
  rowwise() |>
  mutate(
    risk_score = compute_risk_score(declared_mcc, txn_mcc,
                                    jurisdiction_risk, pep_hit,
                                    sanctions_hit, doc_quality, tenure_days),
    risk_tier  = risk_tier(risk_score),
    due_at     = sla_due_at(opened_at, risk_tier, status),
    sla_state  = sla_state(sla_remaining_hours(Sys.time(), due_at)),
    sanctions_hits = list(
      sanctions_match(merchant_name, sanctions_sample)
    )
  ) |>
  ungroup()

