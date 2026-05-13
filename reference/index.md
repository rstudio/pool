# Package index

## Generic pool tools

- [`poolCreate()`](http://rstudio.github.io/pool/reference/Pool-class.md)
  [`poolClose()`](http://rstudio.github.io/pool/reference/Pool-class.md)
  : Create a pool of reusable objects
- [`poolCheckout()`](http://rstudio.github.io/pool/reference/poolCheckout.md)
  [`poolReturn()`](http://rstudio.github.io/pool/reference/poolCheckout.md)
  [`localCheckout()`](http://rstudio.github.io/pool/reference/poolCheckout.md)
  : Check out and return object from the pool

## Database functions

- [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md) :
  Create a pool of database connections
- [`dbDataType(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbGetQuery(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbExecute(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbListFields(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbListTables(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbListObjects(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbReadTable(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbWriteTable(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbCreateTable(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbAppendTable(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbExistsTable(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbRemoveTable(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbIsReadOnly(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`sqlData(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`sqlCreateTable(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`sqlAppendTable(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`sqlInterpolate(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`sqlParseVariables(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbQuoteIdentifier(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbUnquoteIdentifier(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbQuoteLiteral(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbQuoteString(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbAppendTableArrow(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbCreateTableArrow(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbGetQueryArrow(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbReadTableArrow(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbSendQueryArrow(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  [`dbWriteTableArrow(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-wrap.md)
  : DBI methods (simple wrappers)
- [`poolWithTransaction()`](http://rstudio.github.io/pool/reference/poolWithTransaction.md)
  : Self-contained database transactions using pool
- [`tbl.Pool()`](http://rstudio.github.io/pool/reference/tbl.Pool.md)
  [`copy_to.Pool()`](http://rstudio.github.io/pool/reference/tbl.Pool.md)
  : Use pool with dbplyr
- [`dbSendQuery(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbSendStatement(`*`<Pool>`*`,`*`<ANY>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbDisconnect(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbGetInfo(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbIsValid(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbBegin(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbCommit(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbRollback(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  [`dbWithTransaction(`*`<Pool>`*`)`](http://rstudio.github.io/pool/reference/DBI-custom.md)
  : Unsupported DBI methods
