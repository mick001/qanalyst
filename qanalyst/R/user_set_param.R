################################################################################
#' set_param_generic
#'
#' Set user defined parameters in control chart object. Example: set user defined
#' UCL, LCL, center or even stats.
#'
#' @param data an object of class spc
#' @param param parameter to be replaced (center, lcl, ucl or stat)
#' @param value a single numeric value (constant)
#' @importFrom lazyeval interp
#' @importFrom dplyr mutate
#' @return An object of class spc
#' @export
#'
set_param_generic_ <- function(data, param, value){

    # Keep class
    stat_data_class <- class(data)

    # Set names
    dot_dots <- setNames(list(value), c(param) )

    # (Mutate) Edit column
    stat_data <- data %>% mutate_(.dots = dot_dots)

    # Set class
    class(stat_data) <- stat_data_class

    # Return
    return(stat_data)
}

#-------------------------------------------------------------------------------
################################################################################
# S3 methods for parameter settings in each chart.

#' set_param
#'
#' Generic S3 method for setting chart parameters if known.
#'
#' @param data a SPC object
#' @export
#'
set_param <- function(data, ...){
    UseMethod("set_param", data)
}
