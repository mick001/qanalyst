#######################################################
#' Generate a xbar R object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class xbar-R
#' @examples
#' data(ingots)
#' xbar_r (ingots, kg, group)
#' @export
#'
xbar_r  <- function (data, x, g){
    x <- substitute(x)
    g <-substitute(g)
    out <- xbar_r_ (data, x , g)
    return(out)
}


#########################################################
#' Generate a xbar-s object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class xbar-R
#' @examples
#' data(ingots)
#' xbar_s(production, x = dim_mm, g = batch)
#' @export
#'
xbar_s  <- function (data, x, g){
    x <- substitute(x)
    g <-substitute(g)
    out <- xbar_s_ (data, x , g)
    return(out)
}
#########################################################
#' Generate a R object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class xbar-R
#' @examples
#' data(ingots)
#' r(ingots, kg, group)
#' @export
#'
r  <- function (data, x, g){
    x <- substitute(x)
    g <-substitute(g)
    out <- r_ (data, x , g)
    return(out)
}

#########################################################
#' Generate a S object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class s
#' @examples
#' data(ingots)
#' s(production, x = dim_mm, g = batch)
#' @export
#'
s  <- function (data, x, g){
    x <- substitute(x)
    g <-substitute(g)
    out <- s_ (data, x , g)
    return(out)
}


################################################################################
#' C chart
#'
#' Generate a C control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ mutate_ summarise_ %>% mutate
#' @return An object of class c_chart
#' @examples
#' data(test_c)
#' c_chart(test_c, defect)
#' @export
#'
c_chart  <- function(data, x){
    x <- substitute(x)
    out <- c_chart_(data, x)
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
#' @importFrom lazyeval interp
#' @importFrom dplyr select_ mutate_ summarise_ %>% mutate
#' @return An object of class p_chart
#' @examples
#' data(test_p)
#' p_chart(test_p, defect, sample_size)
#' @export
#'
p_chart <- function(data, x, g){
    x <- substitute(x)
    g <- substitute(g)
    out <- p_chart_(data, x, g)
    return(out)
}

################################################################################
#' MR chart
#'
#' Generate an MR control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param mr_step step of the moving range
#' @importFrom dplyr select_ mutate %>%
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class mr_chart
#' @examples
#' # Example with moving range of 2 (minumum moving range window size)
#' data(test_i_mr)
#' mr_chart(test_i_mr, cost, mr_step = 2)
#' @export
#'
mr_chart <- function(data,x,mr_step){
    x <- substitute(x)
    out <- mr_chart_(data,x,mr_step)
    return(out)
}


################################################################################
#' I chart
#'
#' Generate a I control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param mr_step step of the moving range
#' @importFrom dplyr select_ mutate %>%
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class i_chart
#' @examples
#' data(test_i_mr)
#' i_chart(test_i_mr, cost, 2)
#' @export
#'
i_chart  <- function (data, x, mr_step){
    x <- substitute(x)
    out <- i_chart_ (data, x , mr_step)
    return(out)
}
