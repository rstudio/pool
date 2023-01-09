#' @include DBI.R
NULL

#' DBIConnection methods from dplyr and dbplyr
#'
#' Pool object wrappers around DBIConnection methods, whose generics are
#' defined either in `dplyr` or in `dbplyr`.
#' For the original documentation, see
#' [dplyr's reference page](https://dplyr.tidyverse.org/reference/index.html)
#' and [dbplyr's reference page](https://dbplyr.tidyverse.org/reference/index.html).
#'
#' @param dest,df,name,overwrite,temporary,...,src,from,con,table,columns,unique,indexes,types,fields,x,force,sql,values,y,vars,type,by,select,where,group_by,having,order_by,limit,distinct,anti,n,warn_incomplete,unique_indexes See original documentation.
#'
#' @name dplyr-db-methods
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   library(dplyr)
#'
#'   db <- tempfile()
#'   pool <- dbPool(RSQLite::SQLite(), dbname = db)
#'
#'   # copy a table into the database
#'   copy_to(pool, mtcars, "mtcars", temporary = FALSE)
#'
#'   # retrieve a table
#'   mtcars_db <- tbl(pool, "mtcars")
#'   mtcars_db
#'   mtcars_db %>% select(mpg, cyl, disp)
#'   mtcars_db %>% filter(cyl == 6) %>% collect()
#'
#'   poolClose(pool)
#' } else {
#'   message("Please install the 'RSQLite' package to run this example")
#' }
NULL

stopIfTemporary <- function(temporary) {
  temporaryErrorMessage <- paste0("You cannot use `temporary = TRUE` ",
                                  "when using a Pool object, since temporary tables are local to a ",
                                  "connection, and there's no guarantee you'll get the same ",
                                  "connection back next time. You must either create a permanent ",
                                  "table, or checkout a connection from `pool` directly with ",
                                  "`con <- poolCheckout(pool)`, and then release the connection ",
                                  "back to the pool when you're finished (`poolReturn(con)`).")
  if (temporary) stop(temporaryErrorMessage)
}

dbplyr_edition.Pool <- function(con) 2L

# --- These generics are set in dplyr (not database-specific)
#' @rdname dplyr-db-methods
copy_to.Pool <- function(dest, df, name = deparse(substitute(df)),
  overwrite = FALSE, temporary = TRUE, ...) {
    stopIfTemporary(temporary)
    db_con <- poolCheckout(dest)
    on.exit(poolReturn(db_con))

    dplyr::copy_to(db_con, df = df, name = name, overwrite = overwrite,
      temporary = temporary, ...)
    dplyr::tbl(dest, name, con = db_con)
}

#' @rdname dplyr-db-methods
tbl.Pool <- function(src, from, ..., vars = NULL, con = NULL) {
  dplyr::check_dbplyr()

  if (is.null(con)) {
    con <- poolCheckout(src)
    on.exit(poolReturn(con))
  }

  from <- dbplyr::as.sql(from, con)
  if (is.null(vars)) {
    vars <- dplyr::db_query_fields(con, from)
  }

  dbplyr::tbl_sql("Pool", dbplyr::src_dbi(src), from, ..., vars = vars)
}

# --- These generics are set in dplyr (database-specific)
#' @rdname dplyr-db-methods
sql_table_analyze.Pool <- function(con, table, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_table_(db_con, table = table, ...)
}

#' @rdname dplyr-db-methods
sql_table_index.Pool <- function(con, table, columns, name = NULL,
                                 unique = FALSE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_table_index(db_con, table = table, columns = columns,
                         name = name, unique = unique, ...)
}

#' @rdname dplyr-db-methods
db_connection_describe.Pool <- function(x) {
  db_con <- poolCheckout(x)
  on.exit(poolReturn(db_con))
  dbplyr::db_connection_describe(db_con)
}

#' @rdname dplyr-db-methods
sql_query_explain.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_query_explain(db_con, sql = sql, ...)
}

#' @rdname dplyr-db-methods
db_insert_into.Pool <- function(con, table, values, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dplyr::check_dbplyr()
  dplyr::db_insert_into(db_con, table = table, values = values, ...)
}

#' @rdname dplyr-db-method
sql_query_fields.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_query_fields(db_con, sql = sql, ...)
}

#' @rdname dplyr-db-methods
sql_query_save.Pool <- function(con, sql, name, temporary = TRUE, ...) {
  stopIfTemporary(temporary)
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_query_save(db_con, sql = sql, name = name,
                       temporary = temporary, ...)
}

#' @rdname dplyr-db-methods
db_write_table.Pool <- function(con, table, types, values,
                                temporary = FALSE, ...) {
  stopIfTemporary(temporary)
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dplyr::check_dbplyr()
  dplyr::db_write_table(db_con, table = table, types = types,
                        values = values, temporary = temporary, ...)
}

#' @rdname dplyr-db-methods
sql_query_join.Pool <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_query_join(db_con, x = x, y = y, vars = vars, type = type,
                  by = by, ...)
}

#' @rdname dplyr-db-methods
sql_query_select.Pool <- function(con, select, from, where = NULL,
                            group_by = NULL, having = NULL, order_by = NULL, limit = NULL,
                            distinct = FALSE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_select(db_con, select = select, from = from,
                    where = where, group_by = group_by, having = having,
                    order_by = order_by, limit = limit, distinct = distinct, ...)
}

#' @rdname dplyr-db-methods
sql_query_semi_join.Pool <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_query_semi_join(db_con, x = x, y = y, anti = anti, by = by, ...)
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

#' @rdname dplyr-db-methods
sql_query_wrap.Pool <- function(con, from,
                              name = random_table_name(), ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_query_wrap(db_con, from = from, name = name, ...)
}

#' @rdname dplyr-db-methods
sql_translation.Pool <- function(con) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_translation(db_con)
}

# --- These generics are set in dbplyr (database-specific)
#' @rdname dplyr-db-methods
db_collect.Pool <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_collect(db_con, sql = sql, n = n,
                     warn_incomplete = warn_incomplete, ...)
}

#' @rdname dplyr-db-methods
db_compute.Pool <- function(con, table, sql, temporary = TRUE,
                            unique_indexes = list(), indexes = list(), ...) {
  stopIfTemporary(temporary)
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_compute(db_con, table = table, sql = sql,
                     temporary = temporary, unique_indexes = unique_indexes,
                     indexes = indexes, ...)
}

#' @rdname dplyr-db-methods
db_sql_render.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_sql_render(db_con, sql = sql, ...)
}

#' @rdname dplyr-db-methods
sql_escape_logical.Pool <- function(con, x) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_escape_logical(db_con, x = x)
}

#' @rdname dplyr-db-methods
sql_join_suffix.Pool <- function(con, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_join_suffix(db_con, ...)
}
