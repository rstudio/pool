# DBI methods (simple wrappers)

These pool method for DBI generics methods check out a connection (with
[`poolCheckout()`](http://rstudio.github.io/pool/reference/poolCheckout.md)),
re-call the generic, then return the connection to the pool (with
[`poolReturn()`](http://rstudio.github.io/pool/reference/poolCheckout.md)).
See [DBI-custom](http://rstudio.github.io/pool/reference/DBI-custom.md)
for DBI methods that do not work with pool objects.

## Usage

``` r
# S4 method for class 'Pool'
dbDataType(dbObj, obj, ...)

# S4 method for class 'Pool,ANY'
dbGetQuery(conn, statement, ...)

# S4 method for class 'Pool,ANY'
dbExecute(conn, statement, ...)

# S4 method for class 'Pool,ANY'
dbListFields(conn, name, ...)

# S4 method for class 'Pool'
dbListTables(conn, ...)

# S4 method for class 'Pool'
dbListObjects(conn, prefix = NULL, ...)

# S4 method for class 'Pool,ANY'
dbReadTable(conn, name, ...)

# S4 method for class 'Pool,ANY'
dbWriteTable(conn, name, value, ...)

# S4 method for class 'Pool'
dbCreateTable(conn, name, fields, ..., row.names = NULL, temporary = FALSE)

# S4 method for class 'Pool'
dbAppendTable(conn, name, value, ..., row.names = NULL)

# S4 method for class 'Pool,ANY'
dbExistsTable(conn, name, ...)

# S4 method for class 'Pool,ANY'
dbRemoveTable(conn, name, ...)

# S4 method for class 'Pool'
dbIsReadOnly(dbObj, ...)

# S4 method for class 'Pool'
sqlData(con, value, row.names = NA, ...)

# S4 method for class 'Pool'
sqlCreateTable(con, table, fields, row.names = NA, temporary = FALSE, ...)

# S4 method for class 'Pool'
sqlAppendTable(con, table, values, row.names = NA, ...)

# S4 method for class 'Pool'
sqlInterpolate(conn, sql, ..., .dots = list())

# S4 method for class 'Pool'
sqlParseVariables(conn, sql, ...)

# S4 method for class 'Pool,ANY'
dbQuoteIdentifier(conn, x, ...)

# S4 method for class 'Pool'
dbUnquoteIdentifier(conn, x, ...)

# S4 method for class 'Pool'
dbQuoteLiteral(conn, x, ...)

# S4 method for class 'Pool,ANY'
dbQuoteString(conn, x, ...)

# S4 method for class 'Pool'
dbAppendTableArrow(conn, name, value, ...)

# S4 method for class 'Pool'
dbCreateTableArrow(conn, name, value, ..., temporary = FALSE)

# S4 method for class 'Pool'
dbGetQueryArrow(conn, statement, ...)

# S4 method for class 'Pool'
dbReadTableArrow(conn, name, ...)

# S4 method for class 'Pool'
dbSendQueryArrow(conn, statement, ...)

# S4 method for class 'Pool'
dbWriteTableArrow(conn, name, value, ...)
```

## Arguments

- dbObj:

  A DBI Driver\]\[DBI::DBIDriver-class\] or [DBI
  Connection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

- obj:

  An R object whose SQL type we want to determine.

- ...:

  Other arguments passed on to methods.

- conn:

  A [DBI
  Connection](https://dbi.r-dbi.org/reference/DBIConnection-class.html).

- statement:

  a character string containing SQL.

- name:

  The table name, passed on to
  [`dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
  Options are:

  - a character string with the unquoted DBMS table name, e.g.
    `"table_name"`,

  - a call to [`Id()`](https://dbi.r-dbi.org/reference/Id.html) with
    components to the fully qualified table name, e.g.
    `Id(schema = "my_schema", table = "table_name")`

  - a call to [`SQL()`](https://dbi.r-dbi.org/reference/SQL.html) with
    the quoted and fully qualified table name given verbatim, e.g.
    `SQL('"my_schema"."table_name"')`

- prefix:

  A fully qualified path in the database's namespace, or `NULL`. This
  argument will be processed with
  [`dbUnquoteIdentifier()`](https://dbi.r-dbi.org/reference/dbUnquoteIdentifier.html).
  If given the method will return all objects accessible through this
  prefix.

- value:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) (or coercible
  to data.frame).

- fields:

  Either a character vector or a data frame.

  A named character vector: Names are column names, values are types.
  Names are escaped with
  [`dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
  Field types are unescaped.

  A data frame: field types are generated using
  [`dbDataType()`](https://dbi.r-dbi.org/reference/dbDataType.html).

- row.names:

  Must be `NULL`.

- temporary:

  If `TRUE`, will generate a temporary table.

- con:

  A database connection.

- table:

  The table name, passed on to
  [`dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
  Options are:

  - a character string with the unquoted DBMS table name, e.g.
    `"table_name"`,

  - a call to [`Id()`](https://dbi.r-dbi.org/reference/Id.html) with
    components to the fully qualified table name, e.g.
    `Id(schema = "my_schema", table = "table_name")`

  - a call to [`SQL()`](https://dbi.r-dbi.org/reference/SQL.html) with
    the quoted and fully qualified table name given verbatim, e.g.
    `SQL('"my_schema"."table_name"')`

- values:

  A data frame. Factors will be converted to character vectors.
  Character vectors will be escaped with
  [`dbQuoteString()`](https://dbi.r-dbi.org/reference/dbQuoteString.html).

- sql:

  A SQL string containing variables to interpolate. Variables must start
  with a question mark and can be any valid R identifier, i.e. it must
  start with a letter or `.`, and be followed by a letter, digit, `.` or
  `_`.

- .dots:

  A list of named arguments to interpolate.

- x:

  A character vector, [SQL](https://dbi.r-dbi.org/reference/SQL.html) or
  [Id](https://dbi.r-dbi.org/reference/Id.html) object to quote as
  identifier.

## Examples

``` r
mtcars1 <- mtcars[ c(1:16), ] # first half of the mtcars dataset
mtcars2 <- mtcars[-c(1:16), ] # second half of the mtcars dataset

pool <- dbPool(RSQLite::SQLite())

# write the mtcars1 table into the database
dbWriteTable(pool, "mtcars", mtcars1, row.names = TRUE)

# list the current tables in the database
dbListTables(pool)
#> [1] "mtcars"

# read the "mtcars" table from the database (only 16 rows)
dbReadTable(pool, "mtcars")
#>              row_names  mpg cyl  disp  hp drat    wt  qsec vs am gear
#> 1            Mazda RX4 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> 2        Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> 3           Datsun 710 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> 4       Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> 5    Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> 6              Valiant 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> 7           Duster 360 14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> 8            Merc 240D 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> 9             Merc 230 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> 10            Merc 280 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> 11           Merc 280C 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> 12          Merc 450SE 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> 13          Merc 450SL 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> 14         Merc 450SLC 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> 15  Cadillac Fleetwood 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> 16 Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#>    carb
#> 1     4
#> 2     4
#> 3     1
#> 4     1
#> 5     2
#> 6     1
#> 7     4
#> 8     2
#> 9     2
#> 10    4
#> 11    4
#> 12    3
#> 13    3
#> 14    3
#> 15    4
#> 16    4

# append mtcars2 to the "mtcars" table already in the database
dbWriteTable(pool, "mtcars", mtcars2, row.names = TRUE, append = TRUE)

# read the "mtcars" table from the database (all 32 rows)
dbReadTable(pool, "mtcars")
#>              row_names  mpg cyl  disp  hp drat    wt  qsec vs am gear
#> 1            Mazda RX4 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> 2        Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> 3           Datsun 710 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> 4       Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> 5    Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> 6              Valiant 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> 7           Duster 360 14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> 8            Merc 240D 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> 9             Merc 230 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> 10            Merc 280 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> 11           Merc 280C 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> 12          Merc 450SE 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> 13          Merc 450SL 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> 14         Merc 450SLC 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> 15  Cadillac Fleetwood 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> 16 Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> 17   Chrysler Imperial 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> 18            Fiat 128 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> 19         Honda Civic 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> 20      Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> 21       Toyota Corona 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> 22    Dodge Challenger 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> 23         AMC Javelin 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> 24          Camaro Z28 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> 25    Pontiac Firebird 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> 26           Fiat X1-9 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> 27       Porsche 914-2 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> 28        Lotus Europa 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> 29      Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> 30        Ferrari Dino 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> 31       Maserati Bora 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> 32          Volvo 142E 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#>    carb
#> 1     4
#> 2     4
#> 3     1
#> 4     1
#> 5     2
#> 6     1
#> 7     4
#> 8     2
#> 9     2
#> 10    4
#> 11    4
#> 12    3
#> 13    3
#> 14    3
#> 15    4
#> 16    4
#> 17    4
#> 18    1
#> 19    2
#> 20    1
#> 21    1
#> 22    2
#> 23    2
#> 24    4
#> 25    2
#> 26    1
#> 27    2
#> 28    2
#> 29    4
#> 30    6
#> 31    8
#> 32    2

# get the names of the columns in the databases's table
dbListFields(pool, "mtcars")
#>  [1] "row_names" "mpg"       "cyl"       "disp"      "hp"       
#>  [6] "drat"      "wt"        "qsec"      "vs"        "am"       
#> [11] "gear"      "carb"     

# use dbExecute to change the "mpg" and "cyl" values of the 1st row
dbExecute(pool,
  paste(
    "UPDATE mtcars",
    "SET mpg = '22.0', cyl = '10'",
    "WHERE row_names = 'Mazda RX4'"
  )
)
#> [1] 1

# read the 1st row of "mtcars" table to confirm the previous change
dbGetQuery(pool, "SELECT * FROM mtcars WHERE row_names = 'Mazda RX4'")
#>   row_names mpg cyl disp  hp drat   wt  qsec vs am gear carb
#> 1 Mazda RX4  22  10  160 110  3.9 2.62 16.46  0  1    4    4

# drop the "mtcars" table from the database
dbRemoveTable(pool, "mtcars")

# list the current tables in the database
dbListTables(pool)
#> character(0)

poolClose(pool)
```
