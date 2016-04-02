#-------------------------------------------------------------------------------
#' ucl_fun
#'
#' Generic S3 method for calculating ucl formula
#'
#' @param class class of the chart
#' @export
#'
ucl_fun <- function(class, ...){
    class(class) <- class
    UseMethod("ucl_fun", class)
}

#' lcl_fun
#'
#' Generic S3 method for calculating lcl formula
#'
#' @param class class of the chart
#' @export
#'
lcl_fun <- function(class, ...){
    class(class) <- class
    UseMethod("lcl_fun", class)
}

#-------------------------------------------------------------------------------
#' lcl_fun xbar-r
#'
#' This function returns the formula for calculating the lcl for the xbar-r
#' chart if no value is provided by the user. If a value is provided, then
#' the function returns value.
#'
#' @param class class of the chart
#' @param value lcl value set by the user
#' @examples
#' lcl_fun("xbar_r", 2)
#' lcl_fun("xbar_r")
#' @export
#'
lcl_fun.xbar_r <- function(value = NULL, ...){
    if(is.null(value)){
        ~center - ucl_const * avg_shape
    }else{
        value
    }
}

#' ucl_fun xbar-r
#'
#' This function returns the formula for calculating the lcl for the xbar-r
#' chart if no value is provided by the user. If a value is provided, then
#' the function returns value.
#'
#' @param class class of the chart
#' @param value ucl value set by the user
#' @examples
#' ucl_fun("xbar_r", 2)
#' ucl_fun("xbar_r")
#' @export
#'
ucl_fun.xbar_r <- function(value = NULL, ...){
    if(is.null(value)){
        ~center + ucl_const * avg_shape
    }else{
        value
    }
}

