# Convertirli tutti in metodi S3
# Aggiungere possibilità di modificare parametri delle altre carte (c,xbar-s,xbar-r,I,mr, ecc..)

################################################################################
#' set_center
#'
#' Set center parameters in control chart object
#'
#' @param data an object of class spc
#' @param new_center a single value (constant center) or a numeric vector
#' @importFrom dplyr %>% mutate
#' @return An object of class spc
#' @export
#'
set_center <- function(data, new_center){
    stat_data <- data %>% mutate(center = new_center)
    class(stat_data) <- class(data)
    return(stat_data)
}

################################################################################
#' set_lcl
#'
#' Set lcl parameters in control chart object
#'
#' @param data an object of class spc
#' @param new_lcl a single value (constant lcl) or a numeric vector
#' @importFrom dplyr %>% mutate
#' @return An object of class spc
#' @export
#'
set_lcl <- function(data, new_lcl){
    stat_data <- data %>% mutate(lcl = new_lcl)
    class(stat_data) <- class(data)
    return(stat_data)
}

################################################################################
#' set_ucl
#'
#' Set ucl parameters in control chart object
#'
#' @param data an object of class spc
#' @param new_ucl a single value (constant ucl) or a numeric vector
#' @importFrom dplyr %>% mutate
#' @return An object of class spc
#' @export
#'
set_ucl <- function(data, new_ucl){
    stat_data <- data %>% mutate(ucl = new_ucl)
    class(stat_data) <- class(data)
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

################################################################################
# Da errore siccome è necessario fare in modo che constant accetti dei vettori come input.
# Verificare

#' set_param.s
#'
#' Set the s chart parameter sigma.
#'
#' @param data an object of class s
#' @param param_sigma the parameter of the s chart
#' @importFrom dplyr mutate mutate_ %>%
#' @importFrom chartconstants constant
#' @return  An object of class s
#' @export
#'
set_param.s <- function(data, param_sigma){

    # Center lcl, ucl formulas
    new_center <- "center * c4"
    new_ucl <- "center + 3 * center * sqrt(1 - c4^2) / c4"
    new_lcl <- "center - 3 * center * sqrt(1 - c4^2) / c4"

    # Add constant c4
    stat_data <- data %>% mutate(center = param_sigma,
                                c4 = constant(n, c4))

    # Edit center
    stat_data <- stat_data %>% mutate_(center = new_center)

    # Edit lcl,ucl
    stat_data <- stat_data %>% mutate_(lcl = new_lcl,
                                       ucl = new_ucl)

    # Filter
    stat_data <- stat_data %>% select(-c4)

    # Set class
    class(stat_data) <- class(data)

    # Return
    return(stat_data)
}

################################################################################
#' set_param.p_chart
#'
#' If the fraction of non-conforming items P is known, then you can set it in the chart.
#'
#' @param data an object of class p_chart
#' @param param_p the P parameter of the p chart
#' @importFrom dplyr mutate mutate_ %>%
#' @return  An object of class p_chart
#' @export
#'
set_param.p_chart <- function(data, param_p){

    # lcl, ucl formulas
    new_ucl <- "center + 3 * sqrt(center*(1 - center) / n)"
    new_lcl <- "center - 3 * sqrt(center*(1 - center) / n)"

    # Edit center
    stat_data <- data %>% mutate(center = param_p)

    # Edit lcl,ucl
    stat_data <- stat_data %>% mutate_(lcl = new_lcl,
                                       ucl = new_ucl)

    # Set to 0 negative lcls
    stat_data <- stat_data %>% mutate(lcl = replace(lcl, which(lcl < 0), 0))

    # Set class
    class(stat_data) <- class(data)

    # Return
    return(stat_data)
}

################################################################################
#' set_param.c_chart
#'
#' Set the Poisson's distribution parameter C in the c chart.
#'
#' @param data an object of class c_chart
#' @param param_c the C parameter of the c chart
#' @importFrom dplyr mutate mutate_ %>%
#' @return  An object of class c_chart
#' @export
#'
set_param.c_chart <- function(data, param_c){

    # lcl, ucl formulas
    new_ucl <- "center + 3 * sqrt(center)"
    new_lcl <- "center - 3 * sqrt(center)"

    # Edit center
    stat_data <- data %>% mutate(center = param_c)

    # Edit lcl,ucl
    stat_data <- stat_data %>% mutate_(lcl = new_lcl,
                                       ucl = new_ucl)

    # Set to lcl in case it's negative
    stat_data <- stat_data %>% mutate(lcl = replace(lcl, which(lcl < 0), 0))

    # Set class
    class(stat_data) <- class(data)

    # Return
    return(stat_data)
}
