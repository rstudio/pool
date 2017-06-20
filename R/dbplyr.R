#' @include DBI.R
NULL

stopIfTemporary <- function(temporary) {
  temporaryErrorMessage <- paste0("You cannot use `temporary = TRUE`",
    "when using a Pool object, since temporary tables are local to a ",
    "connection, and there's no guarantee you'll get the same ",
    "connection back next time. You must either create a permanent ",
    "table, or checkout a connection from `pool` directly with ",
    "`con <- poolCheckout(pool)`, and then release the connection ",
    "back to the pool when you're finished (`poolReturn(con)`).")
  if (temporary) stop(temporaryErrorMessage)
}

# --- These generics are set in dplyr (not database-specific)
#' @export
copy_to.Pool <- function(dest, df, name = deparse(substitute(df)),
  overwrite = FALSE, ...) {
    db_con <- poolCheckout(dest)
    on.exit(poolReturn(db_con))
    copy_to(db_con, df = df, name = name, overwrite = overwrite, ... = ...)
}

#' @export
tbl.Pool <- function(src, from, ...) {
  db_con <- poolCheckout(src)
  on.exit(poolReturn(db_con))
  tbl(db_con, from = from, ... = ...)
}

# --- These generics are set in dplyr (database-specific)
#' @export
db_analyze.Pool <- function(con, table, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_analyze(db_con, table = table, ... = ...)
}

#' @export
db_begin.Pool <- function(con, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_begin(db_con, ... = ...)
}

#' @export
db_commit.Pool <- function(con, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_commit(db_con, ... = ...)
}

#' @export
db_compute.Pool <- function(con, table, sql, temporary = TRUE,
  unique_indexes = list(), indexes = list(), ...) {
    stopIfTemporary(temporary)
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    db_compute(db_con, table = table, sql = sql,
      temporary = temporary, unique_indexes = unique_indexes,
      indexes = indexes, ... = ...)
}

#' @export
db_create_index.Pool <- function(con, table, columns, name = NULL,
  unique = FALSE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    db_create_index(db_con, table = table, columns = columns,
      name = name, unique = unique, ... = ...)
}

#' @export
db_create_indexes.Pool <- function(con, table, indexes = NULL,
  unique = FALSE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    db_create_indexes(db_con, table = table, indexes = indexes,
      unique = unique, ... = ...)
}

#' @export
db_create_table.Pool <- function(con, table, types, temporary = FALSE, ...) {
  stopIfTemporary(temporary)
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_create_table(db_con, table = table, types = types,
    temporary = temporary, ... = ...)
}

#' @export
db_data_type.Pool <- function(con, fields) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_data_type(db_con, fields = fields)
}

#' @export
db_desc.Pool <- function(x) {
  db_con <- poolCheckout(x)
  on.exit(poolReturn(db_con))
  db_desc(db_con)
}

#' @export
db_drop_table.Pool <-  function(con, table, force = FALSE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_drop_table(db_con, table = table, force = force, ... = ...)
}

#' @export
db_explain.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_explain(db_con, sql = sql, ... = ...)
}

#' @export
db_has_table.Pool <- function(con, table) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_has_table(db_con, table = table)
}

#' @export
db_insert_into.Pool <- function(con, table, values, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_insert_into(db_con, table = table, values = values, ... = ...)
}

#' @export
db_list_tables.Pool <- function(con) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_list_tables(db_con)
}

#' @export
db_query_fields.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_query_fields(db_con, sql = sql, ... = ...)
}

#' @export
db_query_rows.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_query_rows(db_con, sql = sql, ... = ...)
}

#' @export
db_rollback.Pool <- function(con, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_rollback(db_con, ... = ...)
}

#' @export
db_save_query.Pool <- function(con, sql, name, temporary = TRUE, ...) {
  stopIfTemporary(temporary)
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_save_query(db_con, sql = sql, name = name,
    temporary = temporary, ... = ...)
}

#' @export
db_write_table.Pool <- function(con, table, types, values,
  temporary = FALSE, ...) {
    stopIfTemporary(temporary)
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    db_write_table(db_con, table = table, types = types,
      values = values, temporary = temporary, ... = ...)
}

#' @export
sql_escape_ident.Pool <- function(con, x) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  sql_escape_ident(db_con, x = x)
}

#' @export
sql_escape_string.Pool <- function(con, x) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  sql_escape_string(db_con, x = x)
}

#' @export
sql_join.Pool <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  sql_join(db_con, x = x, y = y, vars = vars, type = type,
    by = by, ... = ...)
}

#' @export
sql_select.Pool <- function(con, select, from, where = NULL,
  group_by = NULL, having = NULL, order_by = NULL, limit = NULL,
  distinct = FALSE, ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    sql_select(db_con, select = select, from = from,
      where = where, group_by = group_by, having = having,
      order_by = order_by, limit = limit, distinct = distinct, ... = ...)
}

#' @export
sql_semi_join.Pool <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  sql_semi_join(db_con, x = x, y = y, anti = anti, by = by,
    ... = ...)
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

#' @export
sql_subquery.Pool <- function(con, from,
  name = random_table_name(), ...) {
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    sql_subquery(db_con, from = from, name = name, ... = ...)
}

#' @export
sql_translate_env.Pool <- function(con) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  sql_translate_env(db_con)
}

# --- These generics are set in dbplyr (database-specific)
#' @export
db_collect.Pool <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_collect(db_con, sql = sql, n = n,
    warn_incomplete = warn_incomplete, ... = ...)
}

#' @export
db_compute.Pool <- function(con, table, sql, temporary = TRUE,
  unique_indexes = list(), indexes = list(), ...) {
    stopIfTemporary(temporary)
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    db_compute(db_con, table = table, sql = sql,
      temporary = temporary, unique_indexes = unique_indexes,
      indexes = indexes, ... = ...)
}

#' @export
db_copy_to.Pool <- function(con, table, values, overwrite = FALSE,
  types = NULL, temporary = TRUE, unique_indexes = NULL,
  indexes = NULL, analyze = TRUE, ...) {
    stopIfTemporary(temporary)
    db_con <- poolCheckout(con)
    on.exit(poolReturn(db_con))
    db_copy_to(db_con, table = table, values = values,
      overwrite = overwrite, types = types, temporary = temporary,
      unique_indexes = unique_indexes, indexes = indexes,
      analyze = analyze, ... = ...)
}

#' @export
db_sql_render.Pool <- function(con, sql, ...) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  db_sql_render(db_con, sql = sql, ... = ...)
}

#' @export
sql_escape_logical.Pool <- function(con, x) {
  db_con <- poolCheckout(con)
  on.exit(poolReturn(db_con))
  sql_escape_logical(db_con, x = x)
}
