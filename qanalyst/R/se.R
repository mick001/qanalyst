################################################################################
#' xbar_r_
#' SE version of xbar-R
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param mu average value of x variable (if known)
#' @param sigma standard deviation of x variable (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class xbar-r
#' @export
#'
xbar_r_ <- function(data, x , g, mu=NULL, sigma=NULL){

    # lcl ucl formulas and constants name
    lcl <- "center - lcl_const * avg_shape"
    ucl <- "center + ucl_const * avg_shape"
    lcl_c <- ifelse(is.null(sigma), "A2", "A")
    ucl_c <- ifelse(is.null(sigma), "A2", "A")

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, ...) {mean(x)}
    shape_fun <- function(x, ...) {diff_range(x)}
    center_fun <- mean
    avg_shape_fun <- mean

    # Check if standards (mu and sigma) are given
    center_fun <- set_function(fun = center_fun, k = mu)
    shape_fun <- set_function(fun = shape_fun, k = sigma)

    # Class
    class <- "xbar-r"

    n_min <- 2
    n_max <- 10

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n <- interp(~n())
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add n(), stat(), shape(), "lcl_const", "ucl_const"
    stat_dots   <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat" , "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    ##column Filter
    stat_data <- stat_data %>% select_( group = g, "n",  "stat", "center", "lcl", "ucl")


    #Class attributes
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
}
################################################################################
#' SE version of xbar-S
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param mu average value of x variable (if known)
#' @param sigma standard deviation of x variable (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class xbar-s
#' @export
#'
xbar_s_ <- function(data, x , g, mu=NULL, sigma=NULL){

    # lcl, ucl formulas and constants name
    lcl <- "center - lcl_const * avg_shape"
    ucl <- "center + ucl_const * avg_shape"
    lcl_c <- ifelse(is.null(sigma), "A3", "A")
    ucl_c <- ifelse(is.null(sigma), "A3", "A")

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, ...){ mean(x) }
    shape_fun <- function(x, ...){ sd(x) }
    center_fun <- mean
    avg_shape_fun <- mean

    # Check if standards (mu and sigma) are given
    center_fun <- set_function(fun = center_fun, k = mu)
    shape_fun <- set_function(fun = shape_fun, k = sigma)

    # Class
    class <- "xbar-s"

    n_max <- 25
    n_min <- 8

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    n <- interp(~n())
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add n(), stat(), shape(), "lcl_const", "ucl_const"
    stat_dots   <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula),
        c("n", "stat" , "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    ##xbar Filter
    stat_data <- stat_data %>% select_( group = g, "n",  "stat", "center", "lcl", "ucl")


    ## Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
}

################################################################################
#' SE version of r
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param sigma standard deviation of variable x (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class r
#' @export
#'
r_ <- function(data, x , g, sigma = NULL){

    # lcl ucl formulas and constants name
    lcl <- "lcl_const * center"
    ucl <- "ucl_const * center"
    lcl_c <- ifelse(is.null(sigma), "D3", "D1")
    ucl_c <- ifelse(is.null(sigma), "D4", "D2")

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, ...){ diff_range(x) }
    shape_fun <- function(x, ...){ 0 } #shape qui non serve. Identity non va bene siccome ritorna più valori
    center_fun <- function(x, ...){ mean(x) }
    avg_shape_fun <- mean

    # Check if standards (mu and sigma) are given
    center_fun <- set_function(fun = center_fun, k = sigma)

    # Class
    class <- "r"

    n_min <- 2
    n_max <- 10

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    n <- interp(~n())
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## ## add n(), stat(), shape()
    stat_dots   <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    ##Filter
    stat_data <- stat_data %>% select_( group = g, "n",  "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
}

################################################################################
#' SE version of s
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param sigma standard deviation of x variable (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class r
#' @export
#'
s_ <- function(data, x , g, sigma = NULL){

    # lcl ucl formulas and constants name
    lcl <- "center * lcl_const"
    ucl <- "center * ucl_const"
    lcl_c <- ifelse(is.null(sigma), "B3", "B5")
    ucl_c <- ifelse(is.null(sigma), "B4", "B6")

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, ...){ sd(x) }
    shape_fun <- function(x, ...){ 0 } # altra shape fun inutile. Messa solo per avere shape sotto
    center_fun <- mean
    avg_shape_fun <- mean

    # Check if standards (sigma) are given
    center_fun <- set_function(fun = center_fun, k = sigma)

    # Class
    class <- "s"
    n_min <- 8
    n_max <- 25

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n <- interp(~n())
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add stat, shape and n()
    stat_dots   <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    ##Filter
    stat_data <- stat_data %>% select_( group = g, "n",  "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
}

################################################################################
#' C chart
#'
#' Generate a C control chart object
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g This variable is not used in the c chart. It is set to be equal to x by default in the NSE function.
#' @param center parameter of the c chart, if known
#' @importFrom lazyeval interp
#' @importFrom dplyr rename_ mutate_ summarise_ %>% mutate
#' @return An object of class c_chart
#' @export
#'
c_chart_ <- function(data, x, g, center = NULL){

    # lcl ucl formulas and constants name
    lcl <- "center - 3 * avg_shape"                     # avg_shape = sqrt(center)"
    ucl <- "center + 3 * avg_shape"
    lcl_c <- "one"                                      # constant(n, "one") returns 1 whatever n.
    ucl_c <- "one"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, ...){ identity(x) }
    shape_fun <- function(x, ...){ sqrt( center_fun(x) ) }
    center_fun <- mean
    avg_shape_fun <- mean

    # Check if standards (mu and sigma) are given
    center_fun <- set_function(fun = center_fun, k = center)

    # Class
    class <- "c_chart"

    ## add group
    data <- data %>% mutate_(group = interp( ~row_number() ))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n <- interp( ~n() )
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add "n", "stat", "center", "lcl_const", "ucl_const". Note: n is constant and equal to 1.
    center_dots <- setNames(
        list( n,
              stat_formula,
              shape_formula,
              lcl_const_formula,
              ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group", "n", "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
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
#' @importFrom dplyr select mutate_ summarise_ %>% mutate
#' @return An object of class p_chart
#' @export
#'
p_chart_ <- function(data, x, g, p = NULL){

    ucl <- "center + 3 * avg_shape"             # avg_shape = sqrt( center * (1 - center) / n)
    lcl <- "center - 3 * avg_shape"
    lcl_c <- "one"
    ucl_c <- "one"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, g){x / g}
    shape_fun <- function(x, g){ sqrt( center_fun(x, g) * (1-center_fun(x, g)) / g) }
    center_fun <- function(x, g){sum(x) / sum(g)}
    avg_shape_fun <- identity

    # Check if standards (p) are given
    center_fun <- set_function(fun = center_fun, k = p)

    # Class
    class <- "p_chart"

    # Add "group"
    data <- data %>% mutate_(group = interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n <- g
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add "n", "stat", "shape", "lcl_const", "ucl_const"
    center_dots <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(center = center_formula,
                                       avg_shape = avg_shape_formula,
                                       lcl = lcl, ucl = ucl)

    # Set to 0 negative lcl values (in case some are < 0)
    stat_data <- stat_data %>% mutate(lcl = replace(lcl, which(lcl < 0), 0))

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
}

################################################################################
#' MR chart
#'
#' Generate an MR control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g numeric integer step of the moving range
#' @importFrom dplyr select_ mutate %>%
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class mr_chart
#' @export
#'
mr_chart_ <- function(data, x, g){

    # lcl ucl formulas and constants name
    lcl <- "lcl_const * center"
    ucl <- "ucl_const * center"
    lcl_c <- "D3"
    ucl_c <- "D4"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, ...){ c(rep(NA, g - 1), abs(diff(x, g - 1)) ) }
    shape_fun <- function(x, ...){mean(x, na.rm = TRUE)}
    center_fun <- function(x){mean(x, na.rm = TRUE)}
    avg_shape_fun <- mean

    # Class
    class <- "mr_chart"

    # Add "group"
    data <- data %>% mutate_(group = interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################

    n <- g
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))


    ## add "n", "stat", "shape", "lcl_const", "ucl_const". Note: n is the step of the moving range
    center_dots <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group", "n", "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
}

################################################################################
#' I chart
#'
#' Generate a I control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g moving range step
#' @importFrom dplyr select_ mutate %>%
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class i_chart
#' @export
#'
i_chart_ <- function(data, x, g){

    # lcl ucl formulas and constants name
    lcl <- "center - lcl_const * avg_shape"
    ucl <- "center + ucl_const * avg_shape"
    lcl_c <- "i_chart_const"
    ucl_c <- "i_chart_const"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, ...) { identity(x) }
    shape_fun <- function(x, ...){mean(abs(diff(x, g - 1)))}
    center_fun <- mean
    avg_shape_fun <- mean

    # Class
    class <- "i_chart"

    # add group
    data <- data %>% mutate_(group =  interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n <- g
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add "n", "stat", "shape", "lcl_const", "ucl_const".  Note: n is the step of the moving range
    center_dots <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group", "n", "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
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
#' @importFrom dplyr select mutate_ summarise_ %>% mutate
#' @return An object of class np_chart
#' @export
#'
np_chart_ <- function(data, x, g){

                                                # Note: center = np
    lcl <- "center - 3 * avg_shape"             # avg_shape = sqrt(center*(1 - center / n))
    ucl <- "center + 3 * avg_shape"
    lcl_c <- "one"                              # No constant is needed, constant(n, "one") simply returns 1.
    ucl_c <- "one"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, g){ identity(x) }
    shape_fun <- function(x, g){ sqrt(center_fun(x, g) * (1 - center_fun(x, g) / g)) }
    center_fun <- function(x, g){g * sum(x) / sum(g)}
    avg_shape_fun <- identity

    # Class
    class <- "np_chart"

    # Add group
    data <- data %>% mutate_(group = interp( ~row_number() ))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n <- g
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add "n", "stat", "shape", "lcl_const", "ucl_const". Note: n is the sample size
    center_dots <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(center = center_formula,
                                       avg_shape = avg_shape_formula,
                                       lcl = lcl,
                                       ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
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
#' @importFrom dplyr select mutate_ summarise_ %>% mutate as.tbl
#' @return An object of class u_chart
#' @export
#'
u_chart_ <- function(data, x, g){

    # lcl ucl formulas and constants name
    lcl <- "center - 3 * avg_shape"
    ucl <- "center + 3 * avg_shape" # avg_shape = sqrt(center / n)"
    lcl_c <- "one"
    ucl_c <- "one"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, g){x / g}
    shape_fun <- function(x, g){ sqrt(center_fun(x, g) / g) }
    center_fun <- function(x, g){sum(x) / sum(g)}
    avg_shape_fun <- identity

    # Class
    class <- "u_chart"

    # Add group
    data <- data %>% mutate_(group = interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n <- g
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add "n", "stat", "shape", "lcl_const", "ucl_const"
    center_dots <- setNames(
        list(n,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(center = center_formula,
                                       avg_shape = avg_shape_formula,
                                       lcl = lcl,
                                       ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    #return
    return(stat_data)
}
