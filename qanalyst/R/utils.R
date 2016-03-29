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


#' Utility factory function. Returns fun if k is null, else returns f(x, ...){ k }
#'
#' @param fun stat_fun or shape_fun
#' @param k control chart parameter (ex: mu or sigma) set by the user or NULL
#' @importFrom chartconstants constant
#' @examples
#' set_function(mean)         # Returns mean
#' set_function(mean, k = 10) # Returns function(x, ...){ 10 }
#' @export
#'
set_function <- function(fun, k = NULL){
    if(is.null(k)){
        stat_fun <- fun
    } else {
        stat_fun <- function(x, ...) k
    }
    stat_fun
}
