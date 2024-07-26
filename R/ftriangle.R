#' Geom Full Triangle Point
#'
#' This function creates a custom ggplot2 geom to draw **full** upside-down triangle points
#' with customizable width and height. Coordinates aligns with the **upside-down head** of
#' triangles, instead of the center of the shape like geom_point.
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If specified
#' and inherit.aes = TRUE (the default), it is combined with the default mapping at
#' the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value,
#' like colour = "red" or size = 3.5.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' @return A ggplot2 layer that can be added to a ggplot object.
#' @examples
#' df <- data.frame(x = 1:5, y = 1:5,
#'                  width = c(0.2, 0.4, 0.6, 0.4, 0.6),
#'                  height = c(0.2, 0.5, 0.6, 0.5, 0.2))
#' p <- ggplot(df) +
#'      geom_ftriangle(aes(x = x, y = y,
#'                         fill = x,
#'                         width = width,
#'                         height = height),
#'                     colour = "black")
#' print(p)
#' @import ggplot2
#' @export
geom_ftriangle <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE,
                           ...)
{
  layer(geom = GeomFTriangle, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,...))
}



GeomFTriangle <- ggproto("GeomFTriangle", Geom,
                         required_aes = c("x", "y"),
                         default_aes = aes(colour = NA, fill = "grey20", size = 0.5,
                                           linetype = 1, alpha = 1,
                                           height = 1, width = 1),

                         extra_params = c("na.rm"),
                         draw_key = draw_key_polygon,

                         draw_panel = function(data, panel_params, coord) {


                           data2 <- data
                           data2$x[1] <- data2$y[1] <- 0
                           coords <- coord$transform(data, panel_params)

                           make_triangle_y <- function(y, height) {

                             c(y, y + height, y+height)
                           }
                           make_triangle_x <- function(x, width){
                             return(c(x, x + width/2, x-width/2))
                           }

                           my_tree <- grid::gTree()

                           for(i in seq(nrow(coords))){
                             width <- coords$width[i]
                             height <- coords$height[i]
                             wid <- width / 10
                             hei <- height / 10
                             my_tree <- grid::addGrob(my_tree, grid::polygonGrob(
                               make_triangle_x(coords$x[i], wid),
                               make_triangle_y(coords$y[i], hei),
                               default.units = "native",
                               gp = grid::gpar(
                                 col = coords$colour[i],
                                 fill = scales::alpha(coords$fill[i], coords$alpha[i]),
                                 lwd = coords$size[i] * .pt,
                                 lty = coords$linetype[i]))) }
                           my_tree}
)


# df <- data.frame(x = 1:5, y = 1:5, width = c(0.2, 0.4, 0.6, 0.4, 0.6), height = c(0.2, 0.5, 0.6, 0.5, 0.2))
# ggplot(df) +
#   geom_ftriangle(aes(x = x, y = y, fill = x,
#                     width = width, height = height),
#                 colour = "black")
