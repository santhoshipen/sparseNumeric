# Arithmetic, display, and plot methods for sparse_numeric

These methods provide arithmetic operators, a custom `show()` method,
and a plotting method for sparse_numeric objects.

## Usage

``` r
# S4 method for class 'sparse_numeric,sparse_numeric'
e1 + e2

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 - e2

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 * e2

# S4 method for class 'sparse_numeric'
show(object)

# S4 method for class 'sparse_numeric,sparse_numeric'
plot(
  x,
  y,
  main = "Overlapping non-zero elements",
  xlab = "Position",
  ylab = "Value",
  ...
)
```

## Arguments

- e1, e2:

  sparse_numeric vectors used with the arithmetic operators `+`, `-`,
  and `*`.

- object:

  A sparse_numeric object to be printed by `show()`.

- x, y:

  sparse_numeric vectors for the plotting method.

- main:

  Plot title for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- xlab, ylab:

  Axis labels for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- ...:

  Additional arguments passed to underlying methods (for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)).
