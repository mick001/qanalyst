#' Computes  max(x) - min(x)
#'
#' @param x numeric vector
#' @param ... other numeric vectors
#' @return A numeric vector of length one
#' @examples
#' diff_range(x = 1:5)
#' diff_range(x = c(1:5, NA), na.rm = TRUE)
#' @export
#'
diff_range <- function(x, ...) {diff(range(x, ...))}


#' Utility factory function. Returns fun if k is null, else returns f(x){k}
#'
#' @param fun stat_fun or shape_fun
#' @param k control chart parameter (ex: mu or sigma) set by the user or NULL
#'
#' @export
#'
set_function <- function(fun, k=NULL) {
    if(is.null(k)){
        stat_fun <- fun
    } else {
        stat_fun <- function(x, ...) k
    }
    stat_fun
}


#' Utility function. Returns arg_one if k is NULL otherwise returns arg_two
#'
#' @param arg_one
#' @param arg_two
#' @param k control chart parameter (ex: mu or sigma) set by the user or NULL
#'
#' @export
#'
set_constant <- function(arg_one, arg_two, k=NULL) {
    if(is.null(k)){
        out <- arg_one
    } else {
        out <- arg_two
    }
    out
}
