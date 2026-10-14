# R/db.R
library(DBI)
library(RSQLite)

db_connect <- function(path = "data/portfolio.db") {
  dbConnect(SQLite(), path, flags = RSQLite::SQLITE_RO)
}

#' Run a parameterized query. always use
db_query <- function(con, sql, params = NULL) {
  if (is.null(params)) dbGetQuery(con, sql) else dbGetQuery(con, sql, params = params)
}

#' Distinct values for filter dropdowns (cheap; called once per session).
db_filter_choices <- function(con) {
  list(
    vintage = db_query(con, "SELECT DISTINCT vintage FROM loans ORDER BY vintage")[[1]],
    product = db_query(con, "SELECT DISTINCT product FROM loans ORDER BY product")[[1]],
    region  = db_query(con, "SELECT DISTINCT region  FROM loans ORDER BY region")[[1]],
    tier    = db_query(con, "SELECT DISTINCT risk_tier FROM loans ORDER BY risk_tier")[[1]]
  )
}

#' Build a WHERE clause + params from filter inputs.
#' Returns list(sql = "...", params = list(...)).
build_filter_clause <- function(vintages, products, regions, tiers) {
  clauses <- character(0)
  params  <- list()
  
  if (length(vintages)) { clauses <- c(clauses, sprintf("l.vintage IN (%s)",
                                                        paste(rep("?", length(vintages)), 
                                                              collapse = ",")))
  params <- c(params, as.list(as.character(vintages))) }
  if (length(products)) { clauses <- c(clauses, sprintf("l.product IN (%s)",
                                                        paste(rep("?", length(products)), 
                                                              collapse = ",")))
  params <- c(params, as.list(products)) }
  if (length(regions))  { clauses <- c(clauses, sprintf("l.region IN (%s)",
                                                        paste(rep("?", length(regions)), 
                                                              collapse = ",")))
  params <- c(params, as.list(regions)) }
  if (length(tiers))    { clauses <- c(clauses, sprintf("l.risk_tier IN (%s)",
                                                        paste(rep("?", length(tiers)), 
                                                              collapse = ",")))
  params <- c(params, as.list(tiers)) }
  
  list(
    sql    = if (length(clauses)) paste("WHERE", paste(clauses, collapse = " AND ")) else "",
    params = params
  )
}

#' Portfolio KPIs ??? one query, aggregated in SQL.
db_portfolio_kpis <- function(con, filter) {
  sql <- sprintf("
        SELECT
            COUNT(DISTINCT l.loan_id)                              AS n_loans,
            SUM(DISTINCT 0)                                        AS _dummy,
            (SELECT SUM(p.balance)
               FROM performance p JOIN loans l2 ON l2.loan_id = p.loan_id
               %s)                                                 AS total_balance,
            AVG(l.fico_score)                                      AS avg_fico,
            AVG(l.dti)                                             AS avg_dti
        FROM loans l
        %s
    ",
                 gsub("l\\.", "l2.", filter$sql),
                 filter$sql)
  
  # SQLite doesn't love the correlated subquery with the same params twice;
  # simpler: two queries, merge in R.
  a <- db_query(con, sprintf("SELECT COUNT(*) AS n_loans, AVG(fico_score) AS avg_fico,
                                       AVG(dti) AS avg_dti
                                FROM loans l %s", filter$sql),
                params = filter$params)
  
  b <- db_query(con, sprintf("
        SELECT SUM(p.balance) AS total_balance,
               SUM(p.dpd_30)    AS n_dpd_30,
               SUM(p.dpd_90)    AS n_dpd_90,
               SUM(p.charge_off) AS n_charge_off
        FROM performance p JOIN loans l ON l.loan_id = p.loan_id
        %s", filter$sql),
                params = filter$params)
  
  cbind(a, b)
}

#' Vintage curves ??? delinquency rate by vintage x months_on_book.
#' This is the core analytic. Aggregated entirely in SQL.
db_vintage_curves <- function(con, filter, metric = "dpd_30") {
  metric <- match.arg(metric, c("dpd_30", "dpd_90", "charge_off"))
  sql <- sprintf("
        SELECT
            strftime('%%Y-Q' || ((CAST(strftime('%%m', l.vintage) AS INTEGER)-1)/3 + 1),
                     l.vintage)          AS vintage_label,
            p.months_on_book,
            SUM(p.%)s * 1.0 / COUNT(*) AS rate
        FROM performance p
        JOIN loans l ON l.loan_id = p.loan_id
        %s
        GROUP BY vintage_label, p.months_on_book
        ORDER BY vintage_label, p.months_on_book
    ", metric, filter$sql)
  db_query(con, sql, params = filter$params)
}

#' Loan-level sample for the drill-down table (server-side paginated).
db_loan_sample <- function(con, filter, limit = 500) {
  sql <- sprintf("
        SELECT l.loan_id, l.vintage, l.product, l.region, l.risk_tier,
               l.fico_score, l.original_balance, l.dti
        FROM loans l
        %s
        LIMIT %d
    ", filter$sql, as.integer(limit))
  db_query(con, sql, params = filter$params)
}