#' Base half-triangle phylogeny plot
#'
#' Make base phylogeny plot as a half-triangle plot. Please note that the plot
#' can only work on certain number range of the dataframe.
#' (trials are encouraged)
#' @param data Input dataframe
#' @param x Group column
#' @param y_1 Stem height group A
#' @param y_2 Stem height 1 of group B
#' @param z_1 Triangle height of group A
#' @param z_2 Triangle height  of group B
#' @param t_1 Triangle width of group A
#' @param t_2 Triangle width of group B
#' @return A phylogeny plot for ggplot2
#' @examples
#' set.seed(123)
#' df <- data.frame(x = 1:10,
#'                  y1 = sample(1:50, 10, replace = TRUE),
#'                  y2 = sample(1:50, 10, replace = TRUE),
#'                  z1 = sample(1:50, 10, replace = TRUE),
#'                  z2 = sample(1:50, 10, replace = TRUE))
#' make_half_triangle_plot(df, x = as.factor(x),
#'                         y_1= y1, y_2= y2,
#'                         z_1 = z1, z_2 = z2)
#' @import ggplot2
#' @import colorspace
#' @export
make_half_triangle_plot <- function(data, x = sample_id,
                               y_1 = clonal_driver_count.snv, y_2 = clonal_count.cna,
                               z_1 = subclonal_driver_count.snv, z_2 = subclonal_count.cna,
                               t_1 = 0.1, t_2 = 0.1){
  ggplot2::ggplot(data, aes(x = {{x}})) +
    geom_linerange(aes(ymin = 0, ymax = {{y_1}}, color = "snv")) +
    geom_linerange(aes(ymin = {{y_1}}, ymax = {{y_1}} + {{y_2}}, color = "cna"))  +

    geom_htriangle(aes(
      height = {{z_1}}/70,
      width = {{t_1}},
      y ={{y_1}} + {{y_2}},
      fill = "snv"),

      right_side = F) +

    geom_htriangle(aes(
      height = {{z_2}}/70,
      width = {{t_1}},
      y ={{y_1}} + {{y_2}},
      fill = "cna")) +

    geom_text(aes(y = ({{y_1}} + {{y_2}})/2,
                  label = paste0({{y_1}}, "|", {{y_2}})),
              color = "black",
              nudge_x = -0.25,
              angle = 90,
              size = 3) +

    geom_text(aes(y = {{y_1}} + {{y_2}} +ifelse({{z_1}}>={{z_2}},{{z_1}}, {{z_2}})/7,
                  label = paste0({{z_1}}, "|", {{z_2}})),
              color = "black",
              size = 3,
              nudge_y = 0.7) +

    colorspace::scale_fill_discrete_qualitative() +
    colorspace::scale_color_discrete_qualitative() +
    labs(y = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# set.seed(123) # For reproducibility
# df <- data.frame(
#   x = 1:10,
#   y1 = sample(1:50, 10, replace = TRUE),
#   y2 = sample(1:50, 10, replace = TRUE),
#   z1 = sample(1:50, 10, replace = TRUE),
#   z2 = sample(1:50, 10, replace = TRUE)
# )
#
# make_half_triangle_plot(df, x = as.factor(x), y_1= y1, y_2=y2, z_1 = z1, z_2 = z2)
