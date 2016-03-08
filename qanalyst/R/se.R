################################################################################
#' xbar_r_
#' SE version of xbar-R
#'
#' @param data a dataframe object
#' @param x variable to be used
#' @param g group variable
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
    lcl_c <- "A2"                            # Constants used when standards are not given
    ucl_c <- "A2"
    lcl_c_sg <- "A"                          # Constatnts used when standards are given
    ucl_c_sg <- "A"

    # functions: stat, shape, center, avg_shape
    stat_fun <- mean
    shape_fun <- diff_range
    center_fun <- mean
    avg_shape_fun <- mean

    #----------------------------------------
    # Check if standards (mu and sigma) are given
    center_fun <- set_function(fun = center_fun, k = mu)
    shape_fun <- set_function(fun = shape_fun, k = sigma)
    lcl_c <- set_constant(lcl_c, lcl_c_sg, k=sigma)
    ucl_c <- set_constant(lcl_c, lcl_c_sg, k=sigma)
    #----------------------------------------

    # Class
    class <- "xbar-r"

    n_min <- 2
    n_max <- 10

    # Formulas
    n <- interp(~n())
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(x), x = as.name(x))
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

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
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class xbar-s
#' @export
#'
xbar_s_ <- function(data, x , g){

    # lcl, ucl formulas and constants name
    lcl <- "center - lcl_const * avg_shape"
    ucl <- "center + ucl_const * avg_shape"
    lcl_c <- "A3"
    ucl_c <- "A3"

    # functions: stat, shape, center, avg_shape
    stat_fun <- mean
    shape_fun <- sd
    center_fun <- mean
    avg_shape_fun <- mean

    # Class
    class <- "xbar-s"

    n_max <- 25
    n_min <- 8

    # Formulas
    n <- interp(~n())
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(x), x = as.name(x))
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

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
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class r
#' @export
#'
r_ <- function(data, x , g){

    # lcl ucl formulas and constants name
    lcl <- "lcl_const * center"
    ucl <- "ucl_const * center"
    lcl_c <- "D3"
    ucl_c <- "D4"

    # functions: stat, shape, center, avg_shape
    stat_fun <- diff_range
    shape_fun <- function(x){0}# In realta questa non serve a niente. Messa qui solo per avere no extra changes dopo
    center_fun <- mean
    avg_shape_fun <- mean

    # Class
    class <- "r"
    n_min <- 2
    n_max <- 10

    #Formulas
    n <- interp( ~n())
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(x), x = as.name(x))
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

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
    stat_data <- stat_data %>% mutate_( center = ~mean(stat),
                                        avg_shape = ~mean(shape),
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
#' @importFrom lazyeval interp
#' @importFrom dplyr select_  group_by_ mutate_ left_join summarise_  %>%
#' @importFrom chartconstants constant
#' @return An object of class r
#' @export
#'
s_ <- function(data, x , g){

    # lcl ucl formulas and constants name
    lcl <- "center * lcl_const"
    ucl <- "center * ucl_const"
    lcl_c <- "B3"
    ucl_c <- "B4"

    # functions: stat, shape, center, avg_shape
    stat_fun <- sd
    shape_fun <- function(x){0} # altra shape fun inutile solo per avere shape sotto
    center_fun <- mean
    avg_shape_fun <- mean

    # Class
    class <- "s"
    n_min <- 8
    n_max <- 25

    # Formulas
    n <- interp( ~n() )
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(x), x = as.name(x))
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    #check that n > nmin & n <= n_max
    test_n <- data %>% group_by_(g) %>% summarise_(n = ~n())
    if(any(test_n$n < n_min)) stop(paste("Error, group size cannot be smaller that", n_min))
    if(any(test_n$n > n_max)) stop(paste("Error, group size cannot be larger that", n_max))

    ###################################
    #### No extra changes from here ###
    ###################################

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
#' @importFrom lazyeval interp
#' @importFrom dplyr rename_ mutate_ summarise_ %>% mutate
#' @return An object of class c_chart
#' @export
#'
c_chart_ <- function(data, x){

    # lcl ucl formulas and constants name
    lcl <- "center - 3 * avg_shape" # avg_shape = sqrt(center)"
    ucl <- "center + 3 * avg_shape"
    lcl_c <- "one"                  # constant(n, "one") returns a vector of 1s
    ucl_c <- "one"

    # functions: stat, shape, center, avg_shape
    stat_fun <- identity
    shape_fun <- function(x){ sqrt( mean(x) ) }
    center_fun <- mean
    avg_shape_fun <- mean

    # Class
    class <- "c_chart"

    # Formulas
    n <- interp(~1)
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(x), x = as.name(x))
    lcl_const_formula <- interp( ~constant(2, lcl_c))
    ucl_const_formula <- interp( ~constant(2, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    ## add group
    data <- data %>% mutate_(group = interp( ~row_number() ))

    ###################################
    #### No extra changes from here ###
    ###################################

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
#' @importFrom lazyeval interp
#' @importFrom dplyr select mutate_ summarise_ %>% mutate
#' @return An object of class p_chart
#' @export
#'
p_chart_ <- function(data, x, g){

    ucl <- "center + 3 * avg_shape"# sqrt(center*(1 - center) / n)"
    lcl <- "center - 3 * avg_shape"#sqrt(center*(1 - center) / n)"
    lcl_c <- "one"
    ucl_c <- "one"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, g){x / g}
    shape_fun <- sqrt
    center_fun <- function(x, g){sum(x) / sum(g)}
    avg_shape_fun <- identity

    # Class
    class <- "p_chart"

    # Formulas
    n <- g
    stat_formula <- interp( ~stat_fun(x, y), x = as.name(x), y = as.name(g))
    shape_formula <- interp( ~shape_fun(center_fun(x, y) * (1 - center_fun(x, y)) / y), x = as.name(x), y = as.name(g))
    lcl_const_formula <- interp( ~constant(2, lcl_c))
    ucl_const_formula <- interp( ~constant(2, ucl_c))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add "group"
    data <- data %>% mutate_(group = interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################

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

    # Set to 0 negative lcl values (in case some are < 0)############################### Va bene???##################################################à
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
#' @param mr_step step of the moving range
#' @importFrom dplyr select_ mutate %>%
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class mr_chart
#' @export
#'
mr_chart_ <- function(data,x,mr_step){

    # lcl ucl formulas and constants name
    lcl <- "lcl_const * center"
    ucl <- "ucl_const * center"
    lcl_c <- "D3"
    ucl_c <- "D4"

    # functions: stat, shape, center, avg_shape
    stat_fun <- function(x, step = mr_step){ c(rep(NA,step-1), abs(diff(x,step-1)) ) }
    shape_fun <- function(x){mean(x, na.rm = TRUE)}
    center_fun <- mean
    avg_shape_fun <- mean

    # Class
    class <- "mr_chart"

    # Formulas
    n <- mr_step
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(x), x = as.name(x))
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat, na.rm=TRUE))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add "group"
    data <- data %>% mutate_(group = interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################


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
#' @param mr_step moving range step
#' @importFrom dplyr select_ mutate %>%
#' @importFrom lazyeval interp
#' @importFrom chartconstants constant
#' @return An object of class i_chart
#' @export
#'
i_chart_ <- function(data, x, mr_step){

    # lcl ucl formulas and constants name
    lcl <- "center - lcl_const * avg_shape"
    ucl <- "center + ucl_const * avg_shape"
    lcl_c <- "i_chart_const"
    ucl_c <- "i_chart_const"

    # functions: stat, shape, center, avg_shape
    stat_fun <- identity
    shape_fun <- function(x, step = mr_step){mean(abs(diff(x, step - 1)))}
    center_fun <- mean
    avg_shape_fun <- mean

    # Class
    class <- "i_chart"

    # Formulas
    n <- mr_step
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(x), x = as.name(x))
    lcl_const_formula <- interp( ~constant(n, lcl_c))
    ucl_const_formula <- interp( ~constant(n, ucl_c))
    center_formula <- interp( ~center_fun(stat))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # add group
    data <- data %>% mutate_(group =  interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################

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
    lcl <- "center - 3 * avg_shape"#"sqrt(center*(1 - center / n))"
    ucl <- "center + 3 * avg_shape"#sqrt(center*(1 - center / n))
    lcl_c <- "one"
    ucl_c <- "one"

    # functions: stat, shape, center, avg_shape
    stat_fun <- identity
    shape_fun <- sqrt
    center_fun <- function(x, g){g * sum(x) / sum(g)}
    avg_shape_fun <- identity

    # Class
    class <- "np_chart"

    # Formulas
    n <- g
    stat_formula <- interp( ~stat_fun(x), x = as.name(x))
    shape_formula <- interp( ~shape_fun(center_fun(x, y) * (1 - center_fun(x, y)/y)), x = as.name(x), y = as.name(g))
    lcl_const_formula <- interp( ~constant(2, lcl_c))
    ucl_const_formula <- interp( ~constant(2, ucl_c))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add group
    data <- data %>% mutate_(group = interp( ~row_number() ))

    ###################################
    #### No extra changes from here ###
    ###################################

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
    shape_fun <- sqrt
    center_fun <- function(x, g){sum(x) / sum(g)}
    avg_shape_fun <- identity

    # Class
    class <- "u_chart"

    # Formulas
    n <- g
    stat_formula <- interp( ~stat_fun(x, y), x = as.name(x), y = as.name(g))
    shape_formula <- interp( ~shape_fun( center_fun(x, h) / y),x = as.name(x),h = as.name(g), y=as.name(g))
    lcl_const_formula <- interp( ~constant(2, lcl_c))
    ucl_const_formula <- interp( ~constant(2, ucl_c))
    center_formula <- interp( ~center_fun(x, y), x = as.name(x), y = as.name(g))
    avg_shape_formula <- interp( ~avg_shape_fun(shape))

    # Add group
    data <- data %>% mutate_(group = interp( ~row_number() ) )

    ###################################
    #### No extra changes from here ###
    ###################################

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

    # set attributes
    #attr(stat_data, "x_lab") <- x

    #return
    return(stat_data)
}
