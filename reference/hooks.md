# Pooled object methods.

For backend authors only. Authors should implement all of these, which
are then called by the Pool class methods. These should not be called
directly either by backend authors or by the end users.

## Usage

``` r
onActivate(object)

onPassivate(object)

onDestroy(object)

onValidate(object, query)

# S4 method for class 'ANY'
onActivate(object)

# S4 method for class 'ANY'
onPassivate(object)

# S4 method for class 'ANY'
onDestroy(object)

# S4 method for class 'ANY'
onValidate(object, query)

# S4 method for class 'DBIConnection'
onPassivate(object)

# S4 method for class 'DBIConnection'
onDestroy(object)

# S4 method for class 'DBIConnection'
onValidate(object)
```

## Arguments

- object:

  A pooled object.

- query:

  A simple query that can be used to verify that the `object` functions
  as expected.
