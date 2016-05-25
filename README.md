# pool

[![Travis-CI Build Status](https://travis-ci.org/bborgesr/pool.svg?branch=master)](https://travis-ci.org/bborgesr/pool)

Creates connection pools for various ypes of databases in R to make it less computationally expensive to get a connection.

## Sample Usage

```r
library(DBI)   ## must be new version of DBI (nto currently on CRAN)
library(R6)
library(RMySQL)

source('~/RStudio/pool/R/pool.R')

pool <- createPool(
  drv = MySQL(), 
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)

dbGetQuery(pool, "SELECT * from City WHERE ID < 10")
#>   ID           Name CountryCode      District Population
#> 1  1          Kabul         AFG         Kabol    1780000
#> 2  2       Qandahar         AFG      Qandahar     237500
#> 3  3          Herat         AFG         Herat     186800
#> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
#> 5  5      Amsterdam         NLD Noord-Holland     731200
#> 6  6      Rotterdam         NLD  Zuid-Holland     593321
#> 7  7           Haag         NLD  Zuid-Holland     440900
#> 8  8        Utrecht         NLD       Utrecht     234323
#> 9  9      Eindhoven         NLD Noord-Brabant     201843
```
