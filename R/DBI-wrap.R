#' DBI methods (simple wrappers)
#'
#' @description
#' These pool method for DBI generics methods check out a connection
#' (with [poolCheckout()]), re-call the generic, then return the connection
#' to the pool (with [poolReturn()]).
#' See [DBI-custom] for DBI methods that do not work with pool objects.
#'
#' @name DBI-wrap
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
    db_con <- localCheckout(!!con_arg)

    !!recall
  })

  new_function(args, body, env = ns_env("pool"))
}

#' @export
#' @inheritParams DBI::dbDataType
#' @rdname DBI-wrap
setMethod("dbDataType", "Pool", DBI_wrap("dbDataType"))

#' @export
#' @inheritParams DBI::dbGetQuery
#' @rdname DBI-wrap
setMethod("dbGetQuery", "Pool", DBI_wrap("dbGetQuery"))

#' @export
#' @inheritParams DBI::dbExecute
#' @rdname DBI-wrap
setMethod("dbExecute", "Pool", DBI_wrap("dbExecute"))

#' @export
#' @inheritParams DBI::dbListFields
#' @rdname DBI-wrap
setMethod("dbListFields", "Pool", DBI_wrap("dbListFields"))

#' @export
#' @inheritParams DBI::dbListTables
#' @rdname DBI-wrap
setMethod("dbListTables", "Pool", DBI_wrap("dbListTables"))

#' @export
#' @inheritParams DBI::dbListObjects
#' @rdname DBI-wrap
setMethod("dbListObjects", "Pool", DBI_wrap("dbListObjects"))

#' @export
#' @inheritParams DBI::dbReadTable
#' @rdname DBI-wrap
setMethod("dbReadTable", "Pool", DBI_wrap("dbReadTable"))

#' @export
#' @inheritParams DBI::dbWriteTable
#' @rdname DBI-wrap
setMethod("dbWriteTable", "Pool", DBI_wrap("dbWriteTable"))

#' @export
#' @inheritParams DBI::dbCreateTable
#' @rdname DBI-wrap
setMethod("dbCreateTable", "Pool", DBI_wrap("dbCreateTable"))

#' @export
#' @inheritParams DBI::dbAppendTable
#' @rdname DBI-wrap
setMethod("dbAppendTable", "Pool", DBI_wrap("dbAppendTable"))

#' @export
#' @inheritParams DBI::dbExistsTable
#' @rdname DBI-wrap
setMethod("dbExistsTable", "Pool", DBI_wrap("dbExistsTable"))

#' @export
#' @inheritParams DBI::dbRemoveTable
#' @rdname DBI-wrap
setMethod("dbRemoveTable", "Pool", DBI_wrap("dbRemoveTable"))

#' @export
#' @inheritParams DBI::dbIsReadOnly
#' @rdname DBI-wrap
setMethod("dbIsReadOnly", "Pool", DBI_wrap("dbIsReadOnly"))

#' @export
#' @inheritParams DBI::sqlData
#' @rdname DBI-wrap
setMethod("sqlData", "Pool", DBI_wrap("sqlData"))

#' @export
#' @inheritParams DBI::sqlCreateTable
#' @rdname DBI-wrap
setMethod("sqlCreateTable", "Pool", DBI_wrap("sqlCreateTable"))

#' @export
#' @inheritParams DBI::sqlAppendTable
#' @rdname DBI-wrap
setMethod("sqlAppendTable", "Pool", DBI_wrap("sqlAppendTable"))

#' @export
#' @inheritParams DBI::sqlInterpolate
#' @rdname DBI-wrap
setMethod("sqlInterpolate", "Pool", DBI_wrap("sqlInterpolate"))

#' @export
#' @inheritParams DBI::sqlParseVariables
#' @rdname DBI-wrap
setMethod("sqlParseVariables", "Pool", DBI_wrap("sqlParseVariables"))

#' @export
#' @inheritParams DBI::dbQuoteIdentifier
#' @rdname DBI-wrap
setMethod("dbQuoteIdentifier", "Pool", DBI_wrap("dbQuoteIdentifier"))

#' @export
#' @inheritParams DBI::dbUnquoteIdentifier
#' @rdname DBI-wrap
setMethod("dbUnquoteIdentifier", "Pool", DBI_wrap("dbUnquoteIdentifier"))

#' @export
#' @inheritParams DBI::dbQuoteLiteral
#' @rdname DBI-wrap
setMethod("dbQuoteLiteral", "Pool", DBI_wrap("dbQuoteLiteral"))

#' @export
#' @inheritParams DBI::dbQuoteString
#' @rdname DBI-wrap
setMethod("dbQuoteString", "Pool", DBI_wrap("dbQuoteString"))

#' @export
#' @inheritParams DBI::dbAppendTableArrow
#' @rdname DBI-wrap
setMethod("dbAppendTableArrow", "Pool", DBI_wrap("dbAppendTableArrow"))

#' @export
#' @inheritParams DBI::dbCreateTableArrow
#' @rdname DBI-wrap
setMethod("dbCreateTableArrow", "Pool", DBI_wrap("dbCreateTableArrow"))

#' @export
#' @inheritParams DBI::dbGetQueryArrow
#' @rdname DBI-wrap
setMethod("dbGetQueryArrow", "Pool", DBI_wrap("dbGetQueryArrow"))

#' @export
#' @inheritParams DBI::dbReadTableArrow
#' @rdname DBI-wrap
setMethod("dbReadTableArrow", "Pool", DBI_wrap("dbReadTableArrow"))

#' @export
#' @inheritParams DBI::dbSendQueryArrow
#' @rdname DBI-wrap
setMethod("dbSendQueryArrow", "Pool", DBI_wrap("dbSendQueryArrow"))

#' @export
#' @inheritParams DBI::dbWriteTableArrow
#' @rdname DBI-wrap
setMethod("dbWriteTableArrow", "Pool", DBI_wrap("dbWriteTableArrow"))
