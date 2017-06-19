#' @include DBI.R
NULL

#' DBIConnection methods from dbplyr.
#'
#' Pool object wrappers around \code{\link[dbplyr]{dbplyr}}'s
#' DBIConnection methods. For the original documentation, see:
#' \code{\link[dplyr]{backend_dbplyr}}.
#'
#' @name dbplyr-methods
NULL

#' @export
#' @rdname dbplyr-methods
db_analyze.Pool <- function(con, table, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_analyze(db_con, table = table, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_begin.Pool <- function(con, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_begin(db_con, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_collect.Pool <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_collect(db_con, sql = sql, n = n,
    warn_incomplete = warn_incomplete, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_commit.Pool <- function(con, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_commit(db_con, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_compute.Pool <- function(con, table, sql, temporary = TRUE,
  unique_indexes = list(), indexes = list(), ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    dbplyr::db_compute(db_con, table = table, sql = sql,
      temporary = temporary, unique_indexes = unique_indexes,
      indexes = indexes, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_copy_to.Pool <- function(con, table, values, overwrite = FALSE,
  types = NULL, temporary = TRUE, unique_indexes = NULL,
  indexes = NULL, analyze = TRUE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    dbplyr::db_copy_to(db_con, table = table, values = values,
      overwrite = overwrite, types = types, temporary = temporary,
      unique_indexes = unique_indexes, indexes = indexes,
      analyze = analyze, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_create_index.Pool <- function(con, table, columns, name = NULL,
  unique = FALSE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    dbplyr::db_create_index(db_con, table = table, columns = columns,
      name = name, unique = unique, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_create_indexes.Pool <- function(con, table, indexes = NULL,
  unique = FALSE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    dbplyr::db_create_indexes(db_con, table = table, indexes = indexes,
      unique = unique, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_create_table.Pool <- function(con, table, types, temporary = FALSE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_create_table(db_con, table = table, types = types,
    temporary = temporary, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_data_type.Pool <- function(con, fields) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_data_type(db_con, fields = fields)
}

#' @export
#' @rdname dbplyr-methods
db_desc.Pool <- function(x) {
  db_con <- poolCheckout(x)
  on.exit(poolReturn(db_con))
  dbplyr::db_desc(db_con)
}

#' @export
#' @rdname dbplyr-methods
db_drop_table.Pool <-  function(con, table, force = FALSE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_drop_table(db_con, table = table, force = force, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_explain.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_explain(db_con, sql = sql, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_has_table.Pool <- function(con, table) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_has_table(db_con, table = table)
}

#' @export
#' @rdname dbplyr-methods
db_insert_into.Pool <- function(con, table, values, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_insert_into(db_con, table = table, values = values, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_list_tables.Pool <- function(con) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_list_tables(db_con)
}

#' @export
#' @rdname dbplyr-methods
db_query_fields.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_query_fields(db_con, sql = sql, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_query_rows.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_query_rows(db_con, sql = sql, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_rollback.Pool <- function(con, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_rollback(db_con, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_save_query.Pool <- function(con, sql, name, temporary = TRUE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_save_query(db_con, sql = sql, name = name,
    temporary = temporary, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_sql_render.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::db_sql_render(db_con, sql = sql, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
db_write_table.Pool <- function(con, table, types, values,
  temporary = FALSE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    dbplyr::db_write_table(db_con, table = table, types = types,
      values = values, temporary = temporary, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
sql_escape_ident.Pool <- function(con, x) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_escape_ident(db_con, x = x)
}

#' @export
#' @rdname dbplyr-methods
sql_escape_logical.Pool <- function(con, x) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_escape_logical(db_con, x = x)
}

#' @export
#' @rdname dbplyr-methods
sql_escape_string.Pool <- function(con, x) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_escape_string(db_con, x = x)
}

#' @export
#' @rdname dbplyr-methods
sql_join.Pool <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_join(db_con, x = x, y = y, vars = vars, type = type,
    by = by, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
sql_select <- Pool <- function(con, select, from, where = NULL,
  group_by = NULL, having = NULL, order_by = NULL, limit = NULL,
  distinct = FALSE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    dbplyr::sql_select(db_con, select = select, from = from,
      where = where, group_by = group_by, having = having,
      order_by = order_by, limit = limit, distinct = distinct, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
sql_semi_join.Pool <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_semi_join(db_con, x = x, y = y, anti = anti, by = by,
    ... = ...)
}

#' @export
#' @rdname dbplyr-methods
sql_subquery.Pool <- function(con, from,
  name = dbplyr::random_table_name(), ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    dbplyr::sql_subquery(db_con, from = from, name = name, ... = ...)
}

#' @export
#' @rdname dbplyr-methods
sql_translate_env.Pool <- function(con) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  dbplyr::sql_translate_env(db_con)
}
