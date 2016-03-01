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
xbar_r_ <- function(data, x , g){
    lcl <- "center - 3 * avg_shape / (d2 * sqrt(n))"
    ucl <- "center + 3 * avg_shape / (d2 * sqrt(n))"
    stat_fun <- mean
    shape_fun <- diff_range
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

    ## add stat, shape and n()
    stat_dots   <- setNames(
        list(interp(~n()),
             interp( ~stat_fun(x), x = as.name(x)),
             interp( ~shape_fun(x), x = as.name(x)),
             interp( ~constant(n, d2)) ),
        c("n", "stat" , "shape", "d2")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = ~mean(stat),
                                        avg_shape = ~mean(shape),
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

    lcl <- "center - 3 * avg_shape / (c4 * sqrt(n))"
    ucl <- "center + 3 * avg_shape / (c4 * sqrt(n))"
    stat_fun <- mean
    shape_fun <- sd
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

    ## add stat, shape and n()
    stat_dots   <- setNames(
        list(interp(~n()),
             interp( ~stat_fun(x), x = as.name(x)),
             interp( ~shape_fun(x), x = as.name(x)),
             interp( ~constant(n, c4))),
        c("n", "stat" , "shape", "c4")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = ~mean(stat),
                                        avg_shape = ~mean(shape),
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

    lcl <- "D3 * center"
    ucl <- "D4 * center"
    stat_fun <- diff_range
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

    ## add stat, shape and n()
    stat_dots   <- setNames(
        list(interp( ~n()),
             interp( ~stat_fun(x), x = as.name(x)),
             interp( ~constant(n, D3)),
             interp( ~constant(n, D4))),
        c("n", "stat", "D3", "D4")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = ~mean(stat),
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

    lcl <- "center - 3 * center * sqrt(1 - c4^2)/c4"
    ucl <- "center + 3 * center * sqrt(1 - c4^2)/c4"
    stat_fun <- sd
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

    ## add stat, shape and n()
    stat_dots   <- setNames(
        list(interp( ~n()),
             interp( ~stat_fun(x), x = as.name(x)),
             interp( ~constant(n, c4))),
        c("n", "stat", "c4")
    )

    stat_data <- data %>%
        group_by_(g) %>%
        summarise_(.dots = stat_dots)

    ## compute center , lcl and ucl
    stat_data <- stat_data %>% mutate_( center = ~mean(stat),
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

    lcl <- "center - 3 * sqrt(center)"
    ucl <- "center + 3 * sqrt(center)"
    center_fun <- mean
    class <- "c_chart"

    ###################################
    #### No extra changes from here ###
    ###################################

    ## add "group", "n", "stat", "center"
    center_dots <- setNames(
        list(
            group = interp( ~row_number()),
            1,
            interp( ~identity(x), x = as.name(x)),
            interp( ~center_fun(x), x = as.name(x))),
        c("group", "n", "stat", "center")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(lcl = lcl, ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # set attributes
    attr(stat_data, "x_lab") <- x

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

    ucl <- "center + 3 * sqrt(center*(1 - center) / n)"
    lcl <- "center - 3 * sqrt(center*(1 - center) / n)"
    center_fun <- function(x, g){sum(x) / sum(g)}
    stat_fun <- function(x, g){x / g}
    class <- "p_chart"

    ###################################
    #### No extra changes from here ###
    ###################################

    ## add "group", "n", "stat", "center"
    center_dots <- setNames(
        list(
            group = interp( ~row_number() ),
            n = g,
            interp( ~stat_fun(x, y), x = as.name(x), y = as.name(g) ),
            interp( ~center_fun(x, y), x = as.name(x), y = as.name(g)) ),
        c("group", "n", "stat", "center")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(lcl = lcl, ucl = ucl)

    # Set to 0 negative lcl values (in case some are < 0)############################### Va bene???
    stat_data <- stat_data %>% mutate(lcl = replace(lcl, which(lcl < 0), 0))

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # set attributes
    attr(stat_data, "x_lab") <- x

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

    ucl <- "D4 * center"
    lcl <- "D3 * center"
    center_fun <- function(x){mean(x,na.rm = TRUE)}
    stat_fun <- function(x,step){c(rep(NA,step-1),abs(diff(x,step-1)))}
    class <- "mr_chart"

    ###################################
    #### No extra changes from here ###
    ###################################

    ## add "group", "n", "stat", "center"
    center_dots <- setNames(
        list(
            group = interp( ~row_number() ),
            1,
            interp( ~stat_fun(x, step), x = as.name(x), step = mr_step ),
            interp( ~center_fun(stat)),
            interp( ~constant(mr_step, D4)),
            interp( ~constant(mr_step, D3))),
        c("group", "n", "stat", "center","D4","D3")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(lcl = lcl, ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

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

    lcl <- "center - 3 * mr / d2"
    ucl <- "center + 3 * mr / d2"
    mr_fun <- function(x, step){mean(abs(diff(x, step - 1)))}
    stat_fun <- identity
    center_fun <- mean
    class <- "i_chart"

    ###################################
    #### No extra changes from here ###
    ###################################

    ## add "group", "n", "stat", "center"
    center_dots <- setNames(
        list(
            group = interp( ~row_number() ),
            1,
            interp( ~stat_fun(x), x = as.name(x)),
            interp( ~center_fun(x), x = as.name(x)),
            interp( ~mr_fun(x, step), x = as.name(x), step = mr_step),
            interp( ~constant(mr_step, d2)) ),
        c("group", "n", "stat", "center","mr","d2")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(lcl = lcl, ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # set attributes
    attr(stat_data, "x_lab") <- x

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
    ucl <- "center + 3 * sqrt(center*(1 - center / n))"
    lcl <- "center - 3 * sqrt(center*(1 - center / n))"
    center_fun <- function(x, g){g * sum(x) / sum(g)}
    stat_fun <- identity
    class <- "np_chart"

    ###################################
    #### No extra changes from here ###
    ###################################

    ## add "group", "n", "stat", "center"
    center_dots <- setNames(
        list(
            group = interp( ~row_number() ),
            n = g,
            interp( ~stat_fun(x), x = as.name(x) ),
            interp( ~center_fun(x, y), x = as.name(x), y = as.name(g)) ),
        c("group", "n", "stat", "center")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(lcl = lcl, ucl = ucl)

    # Set to 0 negative lcl values (in case some are < 0)############################### Va bene???
    stat_data <- stat_data %>% mutate(lcl = replace(lcl, which(lcl < 0), 0))

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # set attributes
    attr(stat_data, "x_lab") <- x

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
#' @importFrom dplyr select mutate_ summarise_ %>% mutate
#' @return An object of class u_chart
#' @export
#'
u_chart_ <- function(data, x, g){

    ucl <- "center + 3 * sqrt(center / n)"
    lcl <- "center - 3 * sqrt(center / n)"
    center_fun <- function(x, g){sum(x) / sum(g)}
    stat_fun <- function(x, g){x / g}
    class <- "u_chart"

    ###################################
    #### No extra changes from here ###
    ###################################

    ## add "group", "n", "stat", "center"
    center_dots <- setNames(
        list(
            group = interp( ~row_number() ),
            n = g,
            interp( ~stat_fun(x, y), x = as.name(x), y = as.name(g) ),
            interp( ~center_fun(x, y), x = as.name(x), y = as.name(g)) ),
        c("group", "n", "stat", "center")
    )

    stat_data <- data %>% mutate_(.dots = center_dots)

    # Add lcl and ucl
    stat_data <- stat_data %>% mutate_(lcl = lcl, ucl = ucl)

    # Filter
    stat_data <- stat_data %>% select_("group","n","stat","center","lcl","ucl")

    # Set class
    attr(stat_data, "class") <- c(class , "spc" , "tbl_df", "tbl", "data.frame")

    # set attributes
    attr(stat_data, "x_lab") <- x

    #return
    return(stat_data)
}
