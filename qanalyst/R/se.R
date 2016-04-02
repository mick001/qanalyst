################################################################################
#' xbar_r_
#' SE version of xbar-R
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
#' @param mu average value of x variable (if known)
#' @param sigma standard deviation of x variable (if known)
#' @param lcl_value lower control limit value (if known)
#' @param ucl_value upper control limit value (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class xbar-r
#' @export
#'
xbar_r_ <- function(data, x , g, mu=NULL, sigma=NULL, lcl_value=NULL, ucl_value=NULL){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    # This is the case for instance of the xbar-r, xbar-s, r and s charts.
    lcl_constant <- if(is.null(sigma)){ "A2" }else{ "A" }
    ucl_constant <- if(is.null(sigma)){ "A2" }else{ "A" }

    ########################################
    # Replaced at line 81-82. These are some extra changes though. To be confirmed
    # and extended to the other charts, if approved.
    # Can I do the same for center_fun? This would help with R and S charts.

    # Set lcl ucl formulas.
    #lcl <- "center - lcl_const * avg_shape"
    #ucl <- "center + ucl_const * avg_shape"
    #lcl <- lcl_fun(class = "xbar_r", value = lcl_value)
    #ucl <- ucl_fun(class = "xbar_r", value = ucl_value)
    ########################################

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, ...) { mean(x) }
    shape_fun <- function(x, ...) { diff_range(x) }
    center_fun <- mean
    avg_shape_fun <- mean
    n_fun <- NULL

    # Check if standards (mu and sigma) are given.
    # If standards are given, then
    # set_function returns the following function: function(x, ...){ k },
    # otherwise set_function returns fun.
    center_fun <- set_function(fun = center_fun, k = mu)
    shape_fun <- set_function(fun = shape_fun, k = sigma)

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- TRUE

    # Set "group" variable
    group_var <- as.character(g)

    # Set object Class.
    class <- "xbar_r"

    # check that n > nmin & n <= n_max
    n_min <- 2
    n_max <- 10
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))
    lcl_formula <- lcl_fun(class = class, value = lcl_value)    ###
    ucl_formula <- ucl_fun(class = class, value = ucl_value)    ###

    # Add n(), stat(), shape(), lcl_const, ucl_const
    stat_dots   <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat" , "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl_formula,#############
                                        ucl = ucl_formula )############

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_( group = group_var, "n",  "stat", "center", "lcl", "ucl")


    # Class attributes
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class xbar-s
#' @export
#'
xbar_s_ <- function(data, x , g, mu=NULL, sigma=NULL){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    # This is the case for instance of the xbar-r, xbar-s, r and s charts.
    lcl_constant <- if(is.null(sigma)){ "A3" }else{ "A" }
    ucl_constant <- if(is.null(sigma)){ "A3" }else{ "A" }

    # Set lcl and ucl formulas
    lcl <- "center - lcl_const * avg_shape"
    ucl <- "center + ucl_const * avg_shape"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, ...){ mean(x) }
    shape_fun <- function(x, ...){ sd(x) }
    center_fun <- mean
    avg_shape_fun <- mean
    n_fun <- NULL

    # Check if standards (mu and sigma) are given.
    # If standards are given, then
    # set_function returns the following function: function(x, ...){ k },
    # otherwise set_function returns fun.
    center_fun <- set_function(fun = center_fun, k = mu)
    shape_fun <- set_function(fun = shape_fun, k = sigma)

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- TRUE

    # Set group variable
    group_var <- as.character(g)

    # Set object Class
    class <- "xbar_s"

    # check that n > nmin & n <= n_max
    n_max <- 25
    n_min <- 8
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # add n(), stat(), shape(), lcl_const, ucl_const
    stat_dots   <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula),
        c("n", "stat" , "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_( group = group_var, "n",  "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class r
#' @export
#'
r_ <- function(data, x , g, sigma = NULL){

    # Set lcl and ucl constants name
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- if(is.null(sigma)){ "D3" }else{ "D1" }
    ucl_constant <- if(is.null(sigma)){ "D4" }else{ "D2" }

    # Set lcl and ucl formulas
    lcl <- "lcl_const * center"
    ucl <- "ucl_const * center"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, ...){ diff_range(x) }
    shape_fun <- function(x, ...){ 0 }              #shape qui non serve. Identity non va bene siccome ritorna più valori
    center_fun <- function(x, ...){ mean(x) }
    avg_shape_fun <- mean
    n_fun <- NULL

    # Check if standards (mu and sigma) are given.
    # If standards are given, then
    # set_function returns the following function: function(x, ...){ k },
    # otherwise set_function returns fun.
    center_fun <- set_function(fun = center_fun, k = sigma)

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- TRUE

    # Set group variable
    group_var <- as.character(g)

    # Set object Class
    class <- "r"

    # Check that n > nmin & n <= n_max
    n_min <- 2
    n_max <- 10
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## Add n(), stat(), shape(), lcl_const, ucl_const
    stat_dots   <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_( group = group_var, "n",  "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>% n
#' @importFrom chartconstants constant
#' @return An object of class r
#' @export
#'
s_ <- function(data, x , g, sigma = NULL){

    # Set lcl and ucl constants name
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- if(is.null(sigma)){ "B3" }else{ "B5" }
    ucl_constant <- if(is.null(sigma)){ "B4" }else{ "B6" }

    # Set lcl and ucl formulas
    lcl <- "center * lcl_const"
    ucl <- "center * ucl_const"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, ...){ sd(x) }
    shape_fun <- function(x, ...){ 0 }      # altra shape fun inutile. Messa solo per avere shape sotto
    center_fun <- mean
    avg_shape_fun <- mean
    n_fun <- NULL

    # Check if standards (mu and sigma) are given.
    # If standards are given, then
    # set_function returns the following function: function(x, ...){ k },
    # otherwise set_function returns fun.
    center_fun <- set_function(fun = center_fun, k = sigma)

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- TRUE

    # Set group variable
    group_var <- as.character(g)

    # Set object Class
    class <- "s"

    #check that n > nmin & n <= n_max
    n_min <- 8
    n_max <- 25
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add n(), stat(), shape(), lcl_const, ucl_const
    stat_dots   <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_( group = group_var, "n",  "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @param center parameter of the c chart (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr rename_ mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class c_chart
#' @export
#'
c_chart_ <- function(data, x, g, center = NULL){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- "one"                                      # constant(n, "one") returns 1 whatever n.
    ucl_constant <- "one"
    # Set lcl and ucl formulas
    lcl <- "center - 3 * avg_shape"                     # avg_shape = sqrt(center)"
    ucl <- "center + 3 * avg_shape"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, ...){ identity(x) }
    shape_fun <- function(x, ...){ sqrt( center_fun(x) ) }
    center_fun <- mean
    avg_shape_fun <- mean
    n_fun <- function(x, ...){1}

    # Check if standards (mu and sigma) are given.
    # If standards are given, then
    # set_function returns the following function: function(x, ...){ k },
    # otherwise set_function returns fun.
    center_fun <- set_function(fun = center_fun, k = center)

    # Set object Class
    class <- "c_chart"

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- FALSE

    # Add group and set group variable.
    # Note: charts for single values, such as I and MR, and charts for attributes, such
    # as c and p, do not need any group variable. However, a group variable is added in
    # oderd to output a consistent dataframe across all charts.
    data <- data %>% mutate_(group = 1 )
    group_var <- "group"

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add n(), stat(), shape(), lcl_const, ucl_const. Note: n is constant and equal to 1.
    stat_dots <- setNames(
        list( n_formula,
              stat_formula,
              shape_formula,
              lcl_const_formula,
              ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Column Filter. Select relevant columns only.
    stat_data <- stat_data %>% select_(group = group_var, "n", "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @param p parameter of the p chart (if known)
#' @importFrom lazyeval interp
#' @importFrom dplyr select mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class p_chart
#' @export
#'
p_chart_ <- function(data, x, g, p = NULL){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- "one"
    ucl_constant <- "one"

    # Set lcl and ucl formulas
    ucl <- "center + 3 * avg_shape"             # avg_shape = sqrt( center * (1 - center) / n)
    lcl <- "center - 3 * avg_shape"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, g){x / g}
    shape_fun <- function(x, g){ sqrt( center_fun(x, g) * (1-center_fun(x, g)) / g) }
    center_fun <- function(x, g){sum(x) / sum(g)}
    avg_shape_fun <- identity
    n_fun <- function(x, g, ...){ as.character(g) }

    # Check if standards (mu and sigma) are given. If standards are given, then
    # set_function returns the following function: function(x, ...){ k }, otherwise
    # set_function returns fun.
    center_fun <- set_function(fun = center_fun, k = p)

    # Set object Class
    class <- "p_chart"

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- FALSE

    # Add group and set group variable.
    # Note: charts for single values, such as I and MR, and charts for attributes, such
    # as c and p, do not need any group variable. However, a group variable is added in
    # oderd to output a consistent dataframe across all charts.
    data <- data %>% mutate_(group = 1 )
    group_var <- "group"

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add n(), stat(), shape(), lcl_const, ucl_const
    stat_dots <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_(center = center_formula,
                                       avg_shape = avg_shape_formula,
                                       lcl = lcl, ucl = ucl)

    #######################################
    # Set to 0 negative lcl values (in case some are < 0)
    stat_data <- stat_data %>% mutate(lcl = replace(lcl, which(lcl < 0), 0))

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_(group = group_var,"n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @importFrom dplyr select_ mutate %>% n
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class mr_chart
#' @export
#'
mr_chart_ <- function(data, x, g){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- "D3"
    ucl_constant <- "D4"

    # Set lcl and ucl formulas
    lcl <- "lcl_const * center"
    ucl <- "ucl_const * center"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, ...){ c(rep(NA, g - 1), abs(diff(x, g - 1)) ) }
    shape_fun <- function(x, ...){mean(x, na.rm = TRUE)}
    center_fun <- function(x){mean(x, na.rm = TRUE)}
    avg_shape_fun <- mean
    n_fun <- function(...){ g }

    # Set object Class
    class <- "mr_chart"

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- FALSE

    # Add group and set group variable.
    # Note: charts for single values, such as I and MR, and charts for attributes, such
    # as c and p, do not need any group variable. However, a group variable is added in
    # oderd to output a consistent dataframe across all charts.
    data <- data %>% mutate_(group = 1 )
    group_var <- "group"

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add n(), stat(), shape(), lcl_const, ucl_const Note: n is the step of the moving range
    stat_dots <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_(group = group_var, "n", "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
    return(stat_data)
}

################################################################################
#' I chart
#'
#' Generate a I control chart
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g numeric integer step of the moving range
#' @importFrom dplyr select_ mutate %>% n
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class i_chart
#' @export
#'
i_chart_ <- function(data, x, g){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- "i_chart_const"
    ucl_constant <- "i_chart_const"

    # Set lcl and ucl formula
    lcl <- "center - lcl_const * avg_shape"
    ucl <- "center + ucl_const * avg_shape"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, ...) { identity(x) }
    shape_fun <- function(x, ...){mean(abs(diff(x, g - 1)))}
    center_fun <- mean
    avg_shape_fun <- mean
    n_fun <- function(...){ g }

    # Set object Class
    class <- "i_chart"

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- FALSE

    # Add group and set group variable.
    # Note: charts for single values, such as I and MR, and charts for attributes, such
    # as c and p, do not need any group variable. However, a group variable is added in
    # oderd to output a consistent dataframe across all charts.
    data <- data %>% mutate_(group = 1 )
    group_var <- "group"

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add n, stat, shape, lcl_const, ucl_const.  Note: n is the step of the moving range
    stat_dots <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_( center = center_formula,
                                        avg_shape = avg_shape_formula,
                                        lcl = lcl,
                                        ucl = ucl)

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_(group = group_var, "n", "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @importFrom dplyr select mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class np_chart
#' @export
#'
np_chart_ <- function(data, x, g){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- "one"                              # No constant is needed, constant(n, "one") simply returns 1.
    ucl_constant <- "one"

    # Set lcl and ucl formula
    lcl <- "center - 3 * avg_shape"             # avg_shape = sqrt(center*(1 - center / n))
    ucl <- "center + 3 * avg_shape"             # Note: center = np

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, g){ identity(x) }
    shape_fun <- function(x, g){ sqrt(center_fun(x, g) * (1 - center_fun(x, g) / g)) }
    center_fun <- function(x, g){g * sum(x) / sum(g)}
    avg_shape_fun <- identity
    n_fun <- function(x, y, ...){ as.character(y) }

    # Set object Class
    class <- "np_chart"

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- FALSE

    # Add group and set group variable.
    # Note: charts for single values, such as I and MR, and charts for attributes, such
    # as c and p, do not need any group variable. However, a group variable is added in
    # oderd to output a consistent dataframe across all charts.
    data <- data %>% mutate_(group = 1 )
    group_var <- "group"

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add n, stat, shape, lcl_const, ucl_const. Note: n is the sample size
    stat_dots <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_(center = center_formula,
                                       avg_shape = avg_shape_formula,
                                       lcl = lcl,
                                       ucl = ucl)

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_(group = group_var,"n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
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
#' @importFrom dplyr select mutate_ summarise_ %>% mutate n
#' @importFrom chartconstants constant
#' @return An object of class u_chart
#' @export
#'
u_chart_ <- function(data, x, g){

    # Set lcl and ucl constants.
    # (Note that, for some charts, constants may be different if chart parameters are given)
    lcl_constant <- "one"
    ucl_constant <- "one"

    # Set lcl and ucl formulas
    lcl <- "center - 3 * avg_shape"
    ucl <- "center + 3 * avg_shape" # avg_shape = sqrt(center / n)"

    # Set functions for calculating: stat, shape, center, avg_shape, n
    # Note: if n_fun is NULL, then dplyr::n is used.
    stat_fun <- function(x, g){x / g}
    shape_fun <- function(x, g){ sqrt(center_fun(x, g) / g) }
    center_fun <- function(x, g){sum(x) / sum(g)}
    avg_shape_fun <- identity
    n_fun <- function(x, y, ...){ as.character(y) }

    # Set object Class
    class <- "u_chart"

    # This variable is TRUE if the chart is a group chart (such as xbar-r, xbar-s, s and s)
    # otherwise it is set to FALSE.
    # If TRUE, a group_by(g) is performed before making the calculations.
    is_group <- FALSE

    # Add group and set group variable.
    # Note: charts for single values, such as I and MR, and charts for attributes, such
    # as c and p, do not need any group variable. However, a group variable is added in
    # oderd to output a consistent dataframe across all charts.
    data <- data %>% mutate_(group = 1 )
    group_var <- "group"

    ###################################
    #### No extra changes from here ###
    ###################################

    # Formulas
    n_formula <- if(is.null(n_fun)){ interp(~n()) }else{ interp(~n_fun(x, y), .values = list(x = as.name(x),y = as.name(g))) }
    stat_formula <- interp( ~stat_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    shape_formula <- interp( ~shape_fun(x, y), .values = list(x = as.name(x), y = as.name(g)) )
    lcl_const_formula <- interp( ~constant(n, lcl_constant))
    ucl_const_formula <- interp( ~constant(n, ucl_constant))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add n, stat, shape, lcl_const, ucl_const
    stat_dots <- setNames(
        list(n_formula,
             stat_formula,
             shape_formula,
             lcl_const_formula,
             ucl_const_formula ),
        c("n", "stat", "shape", "lcl_const", "ucl_const")
    )

    # If the chart is a "group chart" then group_by and summarise,
    # otherwise simply use mutate.
    if(is_group){
        stat_data <- data %>%
            group_by_(group_var) %>%
            summarise_(.dots = stat_dots)
    }else{
        stat_data <- data %>% mutate_(.dots = stat_dots)
    }

    # Compute center, average shape, lcl and ucl
    stat_data <- stat_data %>% mutate_(center = center_formula,
                                       avg_shape = avg_shape_formula,
                                       lcl = lcl,
                                       ucl = ucl)

    # Column Filter. Select relevant columns only
    stat_data <- stat_data %>% select_(group = group_var, "n", "stat", "center", "lcl", "ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # Return
    return(stat_data)
}
