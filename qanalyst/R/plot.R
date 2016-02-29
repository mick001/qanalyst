#' plot.spc
#' plot an object of class spc
#' @param data spc object
#' @param colour colours for statistics, center line, ucl and lcl
#' @importFrom ggplot2  ggplot geom_line aes
#' @return A ggplot object
#' @examples
#' #Ingots Data
#' ingots_xbar_r <- xbar_r (ingots, kg, group)
#' plot(ingots_xbar_r)
#' #Pistonrings Data
#' pistonrings_xbar_r <- xbar_r (pistonrings[1:125,], diameter, sample)
#' plot(pistonrings_xbar_r)
#' @export
#'
plot.spc <- function(data, colour = c("darkblue", "darkred", "darkred", "darkred")){

    stat_colour = colour[1]
    center_colour = colour[2]
    lcl_colour = colour[3]
    ucl_colour = colour[4]

    pl <- ggplot(data = data, mapping = aes(x = group, y = stat))
    pl <- pl  +
        geom_line(mapping = aes(y = center), colour = center_colour, lwd = 2) +
        geom_line(mapping = aes(y = ucl), colour = lcl_colour) +
        geom_line(mapping = aes(y = lcl), colour = ucl_colour) +
        geom_line(colour = stat_colour)

    #print plot
    print(pl)
    #return
    invisible(NULL)
}
