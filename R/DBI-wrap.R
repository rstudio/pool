#' DBI methods (simple wrappers)
#'
#' @description
#' These pool method for DBI generics methods check out a connection
#' (with [poolCheckout()]), re-call the generic, then return the connection
#' to the pool (with [poolReturn()]).
#'
#' @name DBI-wrap
#' @keywords internal
#' @examples
#' mtcars1 <- mtcars[ c(1:16), ] # first half of the mtcars dataset
#' mtcars2 <- mtcars[-c(1:16), ] # second half of the mtcars dataset
#'
#' pool <- dbPool(RSQLite::SQLite())
#'
#' # write the mtcars1 table into the database
#' dbWriteTable(pool, "mtcars", mtcars1, row.names = TRUE)
#'
#' # list the current tables in the database
#' dbListTables(pool)
#'
#' # read the "mtcars" table from the database (only 16 rows)
#' dbReadTable(pool, "mtcars")
#'
#' # append mtcars2 to the "mtcars" table already in the database
#' dbWriteTable(pool, "mtcars", mtcars2, row.names = TRUE, append = TRUE)
#'
#' # read the "mtcars" table from the database (all 32 rows)
#' dbReadTable(pool, "mtcars")
#'
#' # get the names of the columns in the databases's table
#' dbListFields(pool, "mtcars")
#'
#' # use dbExecute to change the "mpg" and "cyl" values of the 1st row
#' dbExecute(pool,
#'   paste(
#'     "UPDATE mtcars",
#'     "SET mpg = '22.0', cyl = '10'",
#'     "WHERE row_names = 'Mazda RX4'"
#'   )
#' )
#'
#' # read the 1st row of "mtcars" table to confirm the previous change
#' dbGetQuery(pool, "SELECT * FROM mtcars WHERE row_names = 'Mazda RX4'")
#'
#' # drop the "mtcars" table from the database
#' dbRemoveTable(pool, "mtcars")
#'
#' # list the current tables in the database
#' dbListTables(pool)
#'
#' poolClose(pool)
NULL

setClass("Pool")

# Wrapped methods ---------------------------------------------------------

DBI_wrap <- function(fun_name) {
  fun <- utils::getFromNamespace(fun_name, "DBI")

  args <- formals(fun)
  con_arg <- sym(names(args)[[1]])

  call_args <- syms(set_names(names(args)))
  call_args[[1]] <- quote(db_con)
  ns_fun <- call2("::", quote(DBI), sym(fun_name))
  recall <- call2(ns_fun, !!!call_args)

  body <- expr({
    db_con <- poolCheckout(!!con_arg)
    on.exit(poolReturn(db_con))

    !!recall
  })

  new_function(args, body, env = ns_env("pool"))
}

#' @export
#' @rdname DBI-wrap
setMethod("dbDataType", "Pool", DBI_wrap("dbDataType"))

#' @export
#' @rdname DBI-wrap
setMethod("dbGetQuery", "Pool", DBI_wrap("dbGetQuery"))

#' @export
#' @rdname DBI-wrap
setMethod("dbExecute", "Pool", DBI_wrap("dbExecute"))

#' @export
#' @rdname DBI-wrap
setMethod("dbListFields", "Pool", DBI_wrap("dbListFields"))

#' @export
#' @rdname DBI-wrap
setMethod("dbListTables", "Pool", DBI_wrap("dbListTables"))

#' @export
#' @rdname DBI-wrap
setMethod("dbListObjects", "Pool", DBI_wrap("dbListObjects"))

#' @export
#' @rdname DBI-wrap
setMethod("dbReadTable", "Pool", DBI_wrap("dbReadTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbWriteTable", "Pool", DBI_wrap("dbWriteTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbCreateTable", "Pool", DBI_wrap("dbCreateTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbAppendTable", "Pool", DBI_wrap("dbAppendTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbExistsTable", "Pool", DBI_wrap("dbExistsTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbRemoveTable", "Pool", DBI_wrap("dbRemoveTable"))

#' @export
#' @rdname DBI-wrap
setMethod("dbIsReadOnly", "Pool", DBI_wrap("dbIsReadOnly"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlData", "Pool", DBI_wrap("sqlData"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlCreateTable", "Pool", DBI_wrap("sqlCreateTable"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlAppendTable", "Pool", DBI_wrap("sqlAppendTable"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlInterpolate", "Pool", DBI_wrap("sqlInterpolate"))

#' @export
#' @rdname DBI-wrap
setMethod("sqlParseVariables", "Pool", DBI_wrap("sqlParseVariables"))

#' @export
#' @rdname DBI-wrap
setMethod("dbQuoteIdentifier", "Pool", DBI_wrap("dbQuoteIdentifier"))

#' @export
#' @rdname DBI-wrap
setMethod("dbUnquoteIdentifier", "Pool", DBI_wrap("dbUnquoteIdentifier"))

#' @export
#' @rdname DBI-wrap
setMethod("dbQuoteLiteral", "Pool", DBI_wrap("dbQuoteLiteral"))

#' @export
#' @rdname DBI-wrap
setMethod("dbQuoteString", "Pool", DBI_wrap("dbQuoteString"))

#' @export
#' @rdname DBI-wrap
setMethod("dbAppendTableArrow", "Pool", DBI_wrap("dbAppendTableArrow"))

#' @export
#' @rdname DBI-wrap
setMethod("dbCreateTableArrow", "Pool", DBI_wrap("dbCreateTableArrow"))

#' @export
#' @rdname DBI-wrap
setMethod("dbGetQueryArrow", "Pool", DBI_wrap("dbGetQueryArrow"))

#' @export
#' @rdname DBI-wrap
setMethod("dbReadTableArrow", "Pool", DBI_wrap("dbReadTableArrow"))

#' @export
#' @rdname DBI-wrap
setMethod("dbSendQueryArrow", "Pool", DBI_wrap("dbSendQueryArrow"))

#' @export
#' @rdname DBI-wrap
setMethod("dbWriteTableArrow", "Pool", DBI_wrap("dbWriteTableArrow"))
