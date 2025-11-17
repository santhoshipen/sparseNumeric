# Standardize a sparse_numeric vector

Subtract the mean and divide by the SD of the full vector.

## Usage

``` r
standardize(x, ...)

# S4 method for class 'sparse_numeric'
standardize(x, ...)
```

## Arguments

- x:

  A sparse_numeric vector.

- ...:

  Ignored, included for consistency with the generic.

## Value

A dense numeric vector of standardized values.
