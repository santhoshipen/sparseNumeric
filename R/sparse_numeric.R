#' sparse_numeric S4 Class
#'
#' A sparse representation of a numeric vector storing only non-zero
#' values and their positions.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions of those values.
#' @slot length Integer, length of the full (dense) vector.
#'
#' @importFrom methods new show
#' @importFrom graphics legend points segments text
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

## validity method (no roxygen needed)
setValidity("sparse_numeric", function(object) {
  value <- object@value
  pos <- object@pos
  len <- object@length

  if (length(value) != length(pos)) {
    return("value and pos must have the same length")
  }

  if (length(len) != 1 || !is.integer(len)) {
    return("length must be a single integer")
  }

  if (any(pos < 1L | pos > len)) {
    return("pos values must be between 1 and length")
  }

  if (any(duplicated(pos))) {
    return("pos must not contain duplicates")
  }

  if (!all(pos == sort(pos))) {
    return("pos must be sorted ascending")
  }

  TRUE
})

# -----------------------------
# CONSTRUCTORS & COERCION
# -----------------------------

setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value = as.numeric(from[nz]),
      pos = as.integer(nz),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0) {
    out[from@pos] <- from@value
  }
  out
})

# -----------------------------
# BASIC ACCESSORS
# -----------------------------

#' Length method for sparse_numeric
#'
#' @param x A sparse_numeric object.
#' @return The length of the underlying full vector.
#' @export
setMethod("length", "sparse_numeric", function(x) x@length)

# REQUIRED HW6 METHODS


#' Mean of a sparse_numeric vector
#'
#' Computes the mean of the full vector without expanding to dense form.
#'
#' @param x A sparse_numeric vector.
#' @param ... Ignored, included for consistency with the generic.
#' @return A numeric scalar, the mean of the full vector.
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  if (x@length == 0L) return(NA_real_)
  sum(x@value) / x@length
})

#' Norm (L2) generic
#'
#' @name norm
#' @param x A sparse_numeric vector.
#' @param ... Ignored, included for consistency with the generic.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' L2 norm of a sparse_numeric vector
#'
#' @rdname norm
#' @return A numeric scalar containing the Euclidean norm.
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})

#' Standardize a sparse_numeric vector
#'
#' Subtract the mean and divide by the SD of the full vector.
#'
#' @name standardize
#' @param x A sparse_numeric vector.
#' @param ... Ignored, included for consistency with the generic.
#' @return A dense numeric vector of standardized values.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- x@length
  if (n <= 1) stop("Cannot standardize vector of length <= 1")

  mu <- mean(x)
  k <- length(x@pos)

  ss_nonzero <- sum((x@value - mu)^2)
  ss_zero <- (n - k) * mu^2

  sd_x <- sqrt((ss_nonzero + ss_zero) / (n - 1))
  if (sd_x == 0) return(rep(0, n))

  out <- rep(-mu / sd_x, n)
  if (k > 0) {
    out[x@pos] <- (x@value - mu) / sd_x
  }

  out
})


# ARITHMETIC GENERICS


#' sparse_add generic
#'
#' @name sparse_add
#' @param x,y sparse_numeric vectors.
#' @param ... Ignored, included for consistency with the generic.
#' @return A sparse_numeric vector representing `x + y`.
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' sparse_sub generic
#'
#' @name sparse_sub
#' @param x,y sparse_numeric vectors.
#' @param ... Ignored, included for consistency with the generic.
#' @return A sparse_numeric vector representing `x - y`.
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' sparse_mult generic
#'
#' @name sparse_mult
#' @param x,y sparse_numeric vectors.
#' @param ... Ignored, included for consistency with the generic.
#' @return A sparse_numeric vector representing the element-wise product.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' sparse_crossprod generic
#'
#' @name sparse_crossprod
#' @param x,y sparse_numeric vectors.
#' @param ... Ignored, included for consistency with the generic.
#' @return A numeric scalar equal to the dot product of `x` and `y`.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# ARITHMETIC METHODS


#' @rdname sparse_add
#' @export
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must have the same length.")

            all_pos <- union(x@pos, y@pos)
            val_x <- ifelse(all_pos %in% x@pos, x@value[match(all_pos, x@pos)], 0)
            val_y <- ifelse(all_pos %in% y@pos, y@value[match(all_pos, y@pos)], 0)

            value <- val_x + val_y
            keep <- value != 0

            if (!any(keep)) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            kept_pos <- all_pos[keep]
            kept_val <- value[keep]
            o <- order(kept_pos)

            new("sparse_numeric",
                value = kept_val[o],
                pos = as.integer(kept_pos[o]),
                length = x@length)
          })

#' @rdname sparse_sub
#' @export
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must have the same length.")

            all_pos <- union(x@pos, y@pos)
            val_x <- ifelse(all_pos %in% x@pos, x@value[match(all_pos, x@pos)], 0)
            val_y <- ifelse(all_pos %in% y@pos, y@value[match(all_pos, y@pos)], 0)

            value <- val_x - val_y
            keep <- value != 0

            if (!any(keep)) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            kept_pos <- all_pos[keep]
            kept_val <- value[keep]
            o <- order(kept_pos)

            new("sparse_numeric",
                value = kept_val[o],
                pos = as.integer(kept_pos[o]),
                length = x@length)
          })

#' @rdname sparse_mult
#' @export
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must have the same length.")

            shared <- intersect(x@pos, y@pos)
            if (length(shared) == 0) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            val <- x@value[match(shared, x@pos)] *
              y@value[match(shared, y@pos)]

            keep <- val != 0

            new("sparse_numeric",
                value = val[keep],
                pos = as.integer(shared[keep]),
                length = x@length)
          })

#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Vectors must have the same length.")

            shared <- intersect(x@pos, y@pos)
            if (length(shared) == 0) return(0)

            sum(x@value[match(shared, x@pos)] *
                  y@value[match(shared, y@pos)])
          })


# OPERATORS, SHOW, AND PLOT


#' Arithmetic, display, and plot methods for sparse_numeric
#'
#' These methods provide arithmetic operators, a custom `show()` method,
#' and a plotting method for sparse_numeric objects.
#'
#' @param e1,e2 sparse_numeric vectors used with the arithmetic
#'   operators `+`, `-`, and `*`.
#' @param object A sparse_numeric object to be printed by `show()`.
#' @param x,y sparse_numeric vectors for the plotting method.
#' @param main Plot title for `plot()`.
#' @param xlab,ylab Axis labels for `plot()`.
#' @param ... Additional arguments passed to underlying methods (for `plot()`).
#'
#' @rdname sparse_numeric-ops
#' @export
setMethod("+", c(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_numeric-ops
#' @export
setMethod("-", c(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_numeric-ops
#' @export
setMethod("*", c(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

#' @rdname sparse_numeric-ops
#' @export
setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("Sparse numeric (length", object@length, ")\n", sep = "")
  cat("Non-zero elements:", nnz, "\n")

  if (nnz > 0) {
    k <- min(nnz, 10L)
    cat("Positions: ", paste(object@pos[1:k], collapse = ", "),
        if (nnz > k) ", ..." else "", "\n", sep = "")
    cat("Values:    ", paste(signif(object@value[1:k], 6), collapse = ", "),
        if (nnz > k) ", ..." else "", "\n", sep = "")
  }
})

## plot generic (not exported)
if (!isGeneric("plot")) {
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
}

#' @rdname sparse_numeric-ops
#' @export
setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, main = "Overlapping non-zero elements",
                   xlab = "Position", ylab = "Value", ...) {

            shared <- intersect(x@pos, y@pos)

            if (length(shared) == 0) {
              plot(0, 0, type = "n", xlab = xlab, ylab = ylab,
                   xlim = c(0, max(x@length, y@length)),
                   ylim = c(0, 1),
                   main = main, ...)
              text(0.5 * max(x@length, y@length), 0.5,
                   "No overlapping non-zero elements")
              return(invisible(NULL))
            }

            xv <- x@value[match(shared, x@pos)]
            yv <- y@value[match(shared, y@pos)]
            yr <- range(c(xv, yv), finite = TRUE)
            xr <- range(shared)

            plot(shared, xv,
                 xlab = xlab, ylab = ylab,
                 main = main, ylim = yr, xlim = xr, pch = 16, ...)
            points(shared, yv, pch = 1)
            segments(shared, xv, shared, yv, lty = 3)
            legend("topright", legend = c("x values", "y values"),
                   pch = c(16, 1), bty = "n")
            invisible(NULL)
          })
