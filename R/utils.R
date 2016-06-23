
checkDriver <- function(pool, ...) {
  dots <- list(...)
  isDriver <- function(drv) length(grep(drv, dots)) == 1
  if (isDriver("SQLite")) attr(pool, "drv") <- "sqlite"
  else if (isDriver("MySQL")) attr(pool, "drv") <- "mysql"
  else if (isDriver("PostgreSQL")) attr(pool, "drv") <- "postgres"
}
