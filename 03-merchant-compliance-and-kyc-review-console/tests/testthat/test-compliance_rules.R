test_that("risk_tier boundaries are correct", {
  expect_equal(risk_tier(0),   "low")
  expect_equal(risk_tier(35),  "medium")
  expect_equal(risk_tier(60),  "high")
  expect_equal(risk_tier(80),  "critical")
  expect_error(risk_tier(NA))
})

test_that("compute_risk_score is deterministic and capped at 100", {
  s <- compute_risk_score("5411", "5999", "high", TRUE, TRUE, "poor", 30)
  expect_equal(s, 100)
})

test_that("sanctions_match respects threshold", {
  lst <- data.frame(name = "Silk Road Trading", aliases = "Silk Road Exports",
                    stringsAsFactors = FALSE)
  m <- sanctions_match("Silk Road Exports", lst, threshold = 0.7)
  expect_true(nrow(m) >= 1)
  m2 <- sanctions_match("Unrelated Name", lst, threshold = 0.7)
  expect_equal(nrow(m2), 0)
})

test_that("validate_decision rejects short notes and decided cases", {
  cr <- list(status = "assigned", sanctions_hit = FALSE,
             sla_state = "ok", risk_tier = "low")
  expect_false(validate_decision(cr, "approve", "short")$ok)
  expect_true(validate_decision(cr, "approve",
                                "long enough note here")$ok)
  cr$status <- "decided"
  expect_false(validate_decision(cr, "approve",
                                 "long enough note here")$ok)
})

test_that("cannot approve while sanctions hit is open", {
  cr <- list(status = "assigned", sanctions_hit = TRUE,
             sla_state = "ok", risk_tier = "high")
  v <- validate_decision(cr, "approve", "justification ok here")
  expect_false(v$ok)
  expect_match(v$reason, "sanctions")
})

test_that("a decision needs an assigned case", {
  cr <- list(status = "new", sanctions_hit = FALSE,
             sla_state = "ok", risk_tier = "low")
  v <- validate_decision(cr, "reject", "justification ok here")
  expect_false(v$ok)
  expect_match(v$reason, "Assign the case")
})
