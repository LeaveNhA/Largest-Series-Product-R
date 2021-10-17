library("purrr")

# supress warnings until I find a way to solve them:
options(warn=-1)

# definitions of problem space:
identity <- function(a) a
is_done <- function(a) (class(a) == "rlang_box_done")
wrap_if_done <- function(f) function(v) if(is_done(v)) v else f(v)
unwrap_done <- function(v) if(is_done(v)) v[[1]] else v
error_on_any_na <- function(a) if(any(is.na(a))) stop("Problem with non-digit characters present.") else a
error_on_span_less_than_zero <- function(span) function(a) if(span < 0 || span > length(a)) stop("Problem with span.") else a
return_one_if_span_zero <- function(span) function(a) if(span == 0) done(1) else a
indices <- function(span) function(ns) 1:(length(ns) - span + 1)
value_and_indices <- function(span) function(v) map(c(identity, (indices(span))), function(f) f(v))
squence_of_prod_f <- function(span) function(ns) function(a) prod(ns[seq(a, length.out = span)])

# problem function:
largest_series_product <- function(digits, span){
  to_numeric_list <- c(
    function(a) strsplit(a, NULL),
    unlist,
    as.numeric
  )

  error_control <- c(
    error_on_any_na,
    (error_on_span_less_than_zero(span)),
    (value_and_indices(span))
  )

  calculate_LSP <- c(
    function(vi) list(squence_of_prod_f(span)(vi[[1]]), vi[[2]]),
    function(vp) sapply(vp[[2]], vp[[1]]),
    function(a) max(a)
    )

  boxed_calculate_LSP <- map(calculate_LSP, wrap_if_done)

  LSP <- c(
    to_numeric_list,
    error_control,
    boxed_calculate_LSP,
    unwrap_done
  )

  reduce(.x=LSP, .f=function(acc, f) f(acc), .init=digits, .dir="forward")
}
