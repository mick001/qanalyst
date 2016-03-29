#######################################################
#' Generate a xbar R object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param mu average value of x variable (if known)
#' @param sigma standard deviation of x variable (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class xbar-R
#' @examples
#' data(ingots)
#' xbar_r (ingots, kg, group)
#' xbar_r(ingots, kg, group, mu=1, sigma=0.01)
#' @export
#'
xbar_r  <- function (data, x, g, mu = NULL, sigma = NULL){
    x <- substitute(x)
    g <-substitute(g)
    out <- xbar_r_ (data, x , g, mu = mu, sigma = sigma)
    return(out)
}

#########################################################
#' Generate a xbar-s object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param mu average value of x variable (if known)
#' @param sigma standard deviation of x variable (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class xbar-R
#' @examples
#' data(ingots)
#' xbar_s(production, x = dim_mm, g = batch)
#' xbar_s(production, dim_mm, batch, mu = 10, sigma = 1)
#' @export
#'
xbar_s  <- function (data, x, g, mu = NULL, sigma = NULL){
    x <- substitute(x)
    g <-substitute(g)
    out <- xbar_s_ (data, x , g, mu = mu, sigma = sigma)
    return(out)
}

#########################################################
#' Generate a R object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param sigma standard deviation of variable x (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ group_by_ mutate_ summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class xbar-R
#' @examples
#' data(ingots)
#' r(ingots, kg, group)
#' r(ingots, kg, group, sigma=0.05)
#' @export
#'
r  <- function (data, x, g, sigma = NULL){
    x <- substitute(x)
    g <-substitute(g)
    out <- r_ (data, x , g, sigma = sigma)
    return(out)
}

#########################################################
#' Generate a S object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param sigma standard deviation of x variable (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ group_by_ mutate_ summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class s
#' @examples
#' data(ingots)
#' s(production, x = dim_mm, g = batch)
#' s(production, x = dim_mm, g = batch, sigma=2)
#' @export
#'
s  <- function (data, x, g, sigma = NULL){
    x <- substitute(x)
    g <-substitute(g)
    out <- s_ (data, x , g, sigma = sigma)
    return(out)
}


################################################################################
#' C chart
#'
#' Generate a C control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param center parameter of the c chart, if known
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class c_chart
#' @examples
#' data(test_c)
#' c_chart(test_c, defect)
#' @export
#'
c_chart  <- function(data, x, center = NULL){
    x <- substitute(x)
    out <- c_chart_(data, x, g = x, center = center)
    return(out)
}


################################################################################
#' P chart
#'
#' Generate a P control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g size of each sample
#' @param p parameter of the p chart
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class p_chart
#' @examples
#' data(test_p)
#' p_chart(test_p, defect, sample_size)
#' @export
#'
p_chart <- function(data, x, g, p = NULL){
    x <- substitute(x)
    g <- substitute(g)
    out <- p_chart_(data, x, g, p)
    return(out)
}

################################################################################
#' MR chart
#'
#' Generate an MR control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g numeric integer step of the moving range
#' @importFrom dplyr select_ mutate %>% n
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class mr_chart
#' @examples
#' # Example with moving range of 2 (minumum moving range window size)
#' data(test_i_mr)
#' mr_chart(test_i_mr, cost, g = 2)
#' @export
#'
mr_chart <- function(data, x, g){
    x <- substitute(x)
    out <- mr_chart_(data, x, g)
    return(out)
}


################################################################################
#' I chart
#'
#' Generate a I control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g numeric integer step of the moving range
#' @importFrom dplyr select_ mutate %>%
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class i_chart
#' @examples
#' data(test_i_mr)
#' i_chart(test_i_mr, cost, 2)
#' @export
#'
i_chart  <- function (data, x, g){
    x <- substitute(x)
    out <- i_chart_ (data, x , g)
    return(out)
}

################################################################################
#' NP chart
#'
#' Generate a NP control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g size of each sample
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class np_chart
#' @examples
#' data(test_np)
#' np_chart(test_np, non_conforming_cans, sample_size)
#' @export
#'
np_chart  <- function (data, x, g){
    x <- substitute(x)
    g <- substitute(g)
    out <- np_chart_ (data, x , g)
    return(out)
}

################################################################################
#' U chart
#'
#' Generate a U control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g number of inspection units per sample (sample size within each inspection unit is assumed constant)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class u_chart
#' @examples
#' data(test_u)
#' u_chart(test_u, total_number_nonconformities, number_of_inspection_units)
#' @export
#'
u_chart  <- function (data, x, g){
    x <- substitute(x)
    g <- substitute(g)
    out <- u_chart_ (data, x , g)
    return(out)
}

################################################################################
#' set_param_generic
#'
#' Set user defined parameters in control chart (SPC) object. For instance: set user defined
#' UCL, LCL, center or even stats.
#'
#' @param data an object of class spc
#' @param param parameter to be replaced (center, lcl, ucl or stat)
#' @param value a single numeric value (constant)
#' @importFrom lazyeval interp
#' @importFrom dplyr mutate
#' @return An object of class spc
#' @examples
#'  data("test_p")
#'  p <- p_chart(test_p,defect,sample_size)
#'  set_param_generic(p,10,lcl)
#' @export
#'
set_param_generic <- function(data, param, value){
    param <- substitute(param)
    out <- set_param_generic_(data, param, value)
    return(out)
}
