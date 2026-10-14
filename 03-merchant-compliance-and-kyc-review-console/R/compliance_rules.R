# ---------------------------------------------------------------------------
# Pure business-rule functions. NO shiny, NO reactive, NO side effects.
# These are the primary target of unit tests.
# ---------------------------------------------------------------------------

#' Risk tier from a numeric score
#' @export
risk_tier <- function(score) {
  stopifnot(is.numeric(score), length(score) == 1L, !is.na(score))
  if (score >= 80) return("critical")
  if (score >= 60) return("high")
  if (score >= 35) return("medium")
  "low"
}

#' Compute a composite risk score from case attributes.
#' Weights are deliberate and documented - compliance wants determinism.
#' @export
compute_risk_score <- function(declared_mcc,
                               txn_mcc,
                               jurisdiction_risk,
                               pep_hit,
                               sanctions_hit,
                               doc_quality,
                               tenure_days) {
  stopifnot(jurisdiction_risk %in% c("low", "medium", "high"))
  stopifnot(doc_quality %in% c("good", "partial", "poor"))
  
  base <- switch(jurisdiction_risk, low = 10, medium = 30, high = 55)
  
  mcc_mismatch <- if (!is.na(declared_mcc) && !is.na(txn_mcc) &&
                      declared_mcc != txn_mcc) 25 else 0
  
  doc_penalty <- switch(doc_quality, good = 0, partial = 10, poor = 20)
  
  # New merchants carry more risk
  tenure_penalty <- if (tenure_days < 90) 10 else if (tenure_days < 365) 5 else 0
  
  pep_penalty       <- if (isTRUE(pep_hit)) 15 else 0
  sanctions_penalty <- if (isTRUE(sanctions_hit)) 40 else 0
  
  total <- base + mcc_mismatch + doc_penalty + tenure_penalty +
    pep_penalty + sanctions_penalty
  min(100, as.numeric(total))
}

#' Sanctions/PEP fuzzy match against a candidate list.
#' Returns a data.frame of matches with a similarity score in [0,1].
#' Simple token overlap - deterministic and cheap for tests.
#' @export
sanctions_match <- function(subject_name, list_df,
                            fields = c("name", "aliases"),
                            threshold = 0.72) {
  if (is.null(subject_name) || !nzchar(subject_name)) {
    return(data.frame(name = character(), score = numeric(),
                      stringsAsFactors = FALSE))
  }
  norm <- function(x) tolower(gsub("[^a-z0-9 ]", "", x))
  tok  <- function(x) unique(strsplit(norm(x), "\\s+")[[1]])
  subj <- tok(subject_name)
  
  scores <- vapply(seq_len(nrow(list_df)), function(i) {
    combined <- paste(na.omit(unlist(list_df[i, fields, drop = FALSE])),
                      collapse = " ")
    cand <- tok(combined)
    if (length(cand) == 0) return(0)
    inter <- length(intersect(subj, cand))
    union <- length(union(subj, cand))
    if (union == 0) 0 else inter / union
  }, numeric(1))
  
  keep <- scores >= threshold
  if (!any(keep)) {
    return(data.frame(name = character(), score = numeric(),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    name  = list_df$name[keep],
    score = round(scores[keep], 3),
    stringsAsFactors = FALSE
  )[order(-scores[keep]), , drop = FALSE]
}

#' SLA due timestamp given a policy
#' Clock pauses while awaiting merchant.
#' @export
sla_due_at <- function(opened_at,
                       tier,
                       status,
                       paused_at = NULL,
                       resumed_at = NULL,
                       policy = list(critical = 24, high = 48,
                                     medium = 120, low = 240)) {
  hours <- policy[[tier]] %||% 168
  base  <- opened_at + lubridate::hours(hours)
  if (!is.null(paused_at) && !is.null(resumed_at) &&
      !is.na(paused_at) && !is.na(resumed_at)) {
    pause_len <- as.numeric(difftime(resumed_at, paused_at, units = "hours"))
    base <- base + lubridate::hours(pause_len)
  }
  base
}

#' Remaining hours on SLA; negative means breached.
#' @export
sla_remaining_hours <- function(now, due_at) {
  as.numeric(difftime(due_at, now, units = "hours"))
}

#' SLA state as a categorical flag for styling.
#' @export
sla_state <- function(remaining_hours) {
  if (remaining_hours < 0)   "breached"
  else if (remaining_hours < 8) "at_risk"
  else                          "ok"
}

#' Decision validation. Returns list(ok=, reason=).
#' @export
validate_decision <- function(case_row, decision, note) {
  if (!decision %in% c("approve", "reject", "escalate")) {
    return(list(ok = FALSE, reason = "Unknown decision"))
  }
  if (!nzchar(trimws(note %||% "")) || nchar(trimws(note)) < 10) {
    return(list(ok = FALSE,
                reason = "Justification must be at least 10 characters"))
  }
  if (case_row$status %in% c("decided")) {
    return(list(ok = FALSE,
                reason = "Case is already decided. Open a new review cycle."))
  }
  if (decision == "approve" && isTRUE(case_row$sanctions_hit)) {
    return(list(ok = FALSE,
                reason = "Cannot approve while a sanctions hit is open"))
  }
  if (decision == "approve" &&
      identical(case_row$sla_state, "breached") &&
      case_row$risk_tier %in% c("high", "critical")) {
    return(list(ok = FALSE,
                reason = "Breached high/critical cases require escalation"))
  }
  list(ok = TRUE, reason = NA_character_)
}

#' Case-state transition table. Pure: (from, action) -> to
#' @export
next_status <- function(from_status, action) {
  key <- paste(from_status, action, sep = "|")
  table <- c(
    "new|assign"                     = "assigned",
    "new|defer"                      = "deferred",
    "new|request_info"               = "awaiting_merchant",
    "assigned|request_info"          = "awaiting_merchant",
    "assigned|defer"                 = "deferred",
    "assigned|decide"                = "decided",
    "awaiting_merchant|resume"       = "assigned",
    "awaiting_merchant|defer"        = "deferred",
    "deferred|assign"                = "assigned",
    "deferred|decide"                = "decided",
    "decided|reopen"                 = "new"
  )
  out <- table[[key]]
  if (is.null(out)) {
    stop(sprintf("Illegal transition: %s + %s", from_status, action))
  }
  out
}

#' Append a row to the audit trail. Returns the NEW data.frame.
#' Never mutates in place - caller rebinds the reactiveVal.
#' @export
audit_append <- function(trail, user, case_id, action,
                         before_state, after_state, note, when = Sys.time()) {
  stopifnot(is.data.frame(trail))
  required <- c("timestamp", "user", "case_id", "action",
                "before_state", "after_state", "note")
  if (!all(required %in% names(trail))) {
    stop("Audit trail is missing required columns")
  }
  new_row <- data.frame(
    timestamp    = as.POSIXct(when),
    user         = as.character(user),
    case_id      = as.character(case_id),
    action       = as.character(action),
    before_state = as.character(before_state),
    after_state  = as.character(after_state),
    note         = as.character(note %||% ""),
    stringsAsFactors = FALSE
  )
  rbind(trail, new_row)
}

#' Empty audit trail scaffold.
#' @export
audit_trail_init <- function() {
  data.frame(
    timestamp    = as.POSIXct(character()),
    user         = character(),
    case_id      = character(),
    action       = character(),
    before_state = character(),
    after_state  = character(),
    note         = character(),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a