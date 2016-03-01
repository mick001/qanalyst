# Aggiungere possibilità di modificare parametri delle altre carte (c,xbar-s,xbar-r,I,mr, ecc..)

################################################################################
#' set_param_generic
#'
#' Set user defined parameters in control chart object
#'
#' @param data an object of class spc
#' @param new_param a single value (constant) or a numeric vector
#' @param replace_param parameter to be replaced (center, lcl or ucl)
#' @importFrom lazyeval interp
#' @importFrom dplyr %>% select_ mutate_ rename_
#' @return An object of class spc
#' @export
#'
set_param_generic_ <- function(data, new_param, replace_param){

    stat_data <- data %>% select_( interp(~ -x, x = as.name(replace_param)))
    stat_data <- stat_data %>% mutate_(new_param_ = new_param )
    new <- as.character(replace_param)
    stat_data <- rename_(stat_data, .dots=setNames(list("new_param_"), new))
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
