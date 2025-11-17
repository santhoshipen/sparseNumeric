## tests/testthat/test_sparse_numeric.R

# Basic class and validity

test_that("validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("validity method accepts correct object", {
  x <- new("sparse_numeric",
           value  = c(1, 2, 0, 1),
           pos    = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("validity method rejects wrong length", {
  x <- new("sparse_numeric",
           value  = c(1, 2, 0, 1),
           pos    = c(1L, 2L, 3L, 5L),
           length = 5L)
  x@length <- 2L
  expect_error(validObject(x))
})

# Coercion & length

test_that("coercion numeric -> sparse_numeric returns correct class/slots", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
  expect_equal(x@value,  c(1, 2))
  expect_equal(x@pos,    c(4L, 5L))
  expect_equal(x@length, 5L)
})

test_that("coercion sparse_numeric -> numeric works", {
  x <- as(c(0, 2, 0, 5), "sparse_numeric")
  dense <- as(x, "numeric")
  expect_equal(dense, c(0, 2, 0, 5))
})

test_that("length() works for sparse_numeric", {
  x <- as(c(3, 0, 0, 1), "sparse_numeric")
  expect_equal(length(x), 4L)
})

# Methods existence

test_that("show, plot, and arithmetic methods exist", {
  expect_no_error(getMethod("show", "sparse_numeric"))
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("+",    c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("-",    c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("*",    c("sparse_numeric", "sparse_numeric")))
})

test_that("sparse_* generics exist and have at least 2 arguments", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_crossprod"))

  expect_true(length(formals(sparse_add))       >= 2L)
  expect_true(length(formals(sparse_mult))      >= 2L)
  expect_true(length(formals(sparse_sub))       >= 2L)
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

# Arithmetic behavior

test_that("sparse_add returns sparse_numeric and correct values", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")

  out <- sparse_add(x, y)
  expect_s4_class(out, "sparse_numeric")

  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal(out@value,  result@value)
  expect_equal(out@pos,    result@pos)
  expect_equal(out@length, result@length)
})

test_that("sparse_add works on fully dense equivalents", {
  x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")

  out <- sparse_add(x, y)
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal(as(out, "numeric"), as(result, "numeric"))
})

test_that("sparse_add errors on mismatched lengths", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9),  "sparse_numeric")
  expect_error(sparse_add(x, y))
})

test_that("sparse_sub and sparse_mult work as expected", {
  a <- as(c(1, 0, 5), "sparse_numeric")
  b <- as(c(0, 3, 2), "sparse_numeric")

  sub_out  <- sparse_sub(a, b)
  mult_out <- sparse_mult(a, as(c(2, 0, 5), "sparse_numeric"))

  expect_equal(as(sub_out,  "numeric"), c(1, -3, 3))
  expect_equal(as(mult_out, "numeric"), c(2, 0, 25))
})

test_that("sparse_crossprod uses overlapping non-zero positions", {
  a <- as(c(1, 0, 5), "sparse_numeric")
  b <- as(c(2, 3, 5), "sparse_numeric")

  # dot product: 1*2 + 5*5
  expect_equal(sparse_crossprod(a, b), 2 + 25)

  c <- as(c(0, 0, 1), "sparse_numeric")
  d <- as(c(1, 0, 0), "sparse_numeric")
  # no overlapping non-zero positions:
  expect_equal(sparse_crossprod(c, d), 0)
})

test_that("operator overloading (+, -, *) matches sparse_* functions", {
  a <- as(c(1, 0, 5), "sparse_numeric")
  b <- as(c(0, 3, 5), "sparse_numeric")

  expect_equal(as(a + b, "numeric"), as(sparse_add(a, b), "numeric"))
  expect_equal(as(a - b, "numeric"), as(sparse_sub(a, b), "numeric"))
  expect_equal(as(a * b, "numeric"), as(sparse_mult(a, b), "numeric"))
})

# Mean, norm, standardize (HW6)

test_that("mean() works and matches dense mean", {
  x_dense <- c(0, 2, 0, 6)
  x <- as(x_dense, "sparse_numeric")
  expect_equal(mean(x), mean(x_dense))

  y_dense <- c(0, 0, 0)
  y <- as(y_dense, "sparse_numeric")
  expect_equal(mean(y), 0)
})

test_that("mean on empty sparse_numeric returns NA", {
  x <- new("sparse_numeric",
           value = numeric(0),
           pos   = integer(0),
           length = 0L)
  expect_true(is.na(mean(x)))
})

test_that("norm() is Euclidean norm and matches dense computation", {
  x_dense <- c(3, 0, 4)
  x <- as(x_dense, "sparse_numeric")
  expect_equal(norm(x), sqrt(sum(x_dense^2)))

  y_dense <- c(0, 0, 0)
  y <- as(y_dense, "sparse_numeric")
  expect_equal(norm(y), 0)
})

test_that("standardize() returns mean 0 and sd 1", {
  x_dense <- c(1, 2, 3)
  x <- as(x_dense, "sparse_numeric")

  out <- standardize(x)
  expect_length(out, length(x_dense))
  expect_equal(mean(out), 0, tolerance = 1e-8)
  expect_equal(sd(out),   1, tolerance = 1e-8)
})

test_that("standardize() handles zeros correctly", {
  x_dense <- c(0, 0, 5, 0)
  x <- as(x_dense, "sparse_numeric")

  out <- standardize(x)
  expect_length(out, length(x_dense))
  expect_equal(mean(out), 0, tolerance = 1e-8)
  expect_equal(sd(out),   1, tolerance = 1e-8)
})

test_that("standardize() throws error for length <= 1", {
  x <- as(5, "sparse_numeric")
  expect_error(standardize(x))
})

# show and plot behavior

test_that("show() runs and prints something", {
  x <- as(c(0, 2, 0, 5), "sparse_numeric")
  expect_output(show(x))
})

test_that("plot() runs without error for overlapping and non-overlapping", {
  x <- as(c(0, 2, 0, 5), "sparse_numeric")
  y <- as(c(0, 2, 0, 5), "sparse_numeric")
  z <- as(c(1, 0, 0, 0), "sparse_numeric")

  expect_no_error(plot(x, y))  # overlapping
  expect_no_error(plot(x, z))  # mostly non-overlapping
})

# Extra operator tests to hit more branches

test_that("+ operator returns sparse_numeric and sensible values", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 1), "sparse_numeric")
  result <- x + y
  expect_s4_class(result, "sparse_numeric")
  expect_equal(as(result, "numeric"), c(1, 3, 3))
})

test_that("- operator returns sparse_numeric", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 1), "sparse_numeric")
  result <- x - y
  expect_s4_class(result, "sparse_numeric")
})

test_that("* operator returns sparse_numeric", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 2), "sparse_numeric")
  result <- x * y
  expect_s4_class(result, "sparse_numeric")
})

test_that("sparse_mult with no shared positions returns empty sparse_numeric", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  result <- sparse_mult(x, y)
  expect_equal(length(result@value), 0L)
  expect_equal(length(result@pos),   0L)
})

test_that("sparse_add that results in all zeros returns empty sparse_numeric", {
  x <- as(c(1, -1), "sparse_numeric")
  y <- as(c(-1, 1), "sparse_numeric")

  result <- sparse_add(x, y)

  expect_s4_class(result, "sparse_numeric")
  expect_equal(length(result@value), 0L)
  expect_equal(length(result@pos),   0L)
  expect_equal(result@length, 2L)
})

test_that("standardize() of a constant vector returns all zeros", {
  x_dense <- c(5, 5, 5)
  x <- as(x_dense, "sparse_numeric")

  out <- standardize(x)

  expect_equal(out, rep(0, length(x_dense)))
})
