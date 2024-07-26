# Donut + lollipop function
library(dplyr)
library(tidyr)
library(colorspace)
library(ggplot2)

#' Make donut plot
#'
#' Make donut plots combined with lolipop sticks
#' @param data Input dataframe
#' @param group Subgroup column
#' @param length Length of each subgroup (portion in the donut plot)
#' @param loli_ns Number of loli sticks in each subgroup
#' @param k Label text of each group
#' @return A donut plot with loli sticks
#' @examples
#' sample_df <- data.frame(group=c(1, 2, 3), length=c(2, 1, 9),
#'                         loli_ns = c(1, 2, 3), k=c(0.5, 0.3, 0.17))
#' make_donut_plot(sample_df, group = group, length = length, loli_ns = loli_ns, k = k)
#' @export
make_donut_plot <- function(data, group, length, loli_ns, k){
  mydata <- process_func(data, group = {{group}},
                         length = {{length}}, loli_ns = {{loli_ns}})
  donut_func(mydata, group = group,
             length = {{length}}, loli_ns = {{loli_ns}}, k = {{k}})
}

#' Processing the dataframe
#'
#' Process the dataframe
#' @param data Input dataframe
#' @param group Subgroup column
#' @param length Length of each subgroup (portion in the donut plot)
#' @param loli_ns Number of loli sticks in each subgroup
#' @return A data has been processed and summarised
process_func <- function(data, group = group, length = length, loli_ns = loli_ns){
  data %>%
    mutate(end = cumsum({{length}}),
           start = end - {{length}},
           group = as.character({{group}}),
           Sequence = mapply(generate_sequence, start, end, {{loli_ns}}, SIMPLIFY = FALSE))
}

#' Helper function to generate coordinates of lolli sticks
#'
#' generate coordinates of lolli sticks in donut plot
#' @param a coordinate lowerbound
#' @param b coordinate upperbound
#' @param c number of lollisticks
#' @return Coordinates of lollisticks given lower and upperbound coordinates
generate_sequence <- function(a, b, c) {
  length.out <- c + 2
  seq_values <- seq(from = a, to = b, length.out = length.out)
  if (length(seq_values) > 2) {
    return(seq_values[2:(length(seq_values) - 1)])
  } else {
    return(NA)  # If there are not enough elements, return NA
  }
}

#' Drawing donut
#'
#' Drawing donut plot with lolli sticks given processed data
#' @param df Input dataframe
#' @param group Subgroup column
#' @param length Length of each subgroup (portion in the donut plot)
#' @param loli_ns Number of loli sticks in each subgroup
#' @param k Label text of each group
#' @return Donut from processed data
donut_func <- function(df, group = group, length = length, loli_ns = loli_ns, k){
  df %>% ggplot() +
    geom_rect(
      aes(xmin = 0, xmax = sum({{length}}), ymin = 0, ymax = 2),
      fill = "grey97", color = "grey97") +
    geom_rect(aes(xmin=start, xmax=end, fill={{group}}, ymin = 2, ymax=4)) +

    geom_text(aes(label=paste0("K=", {{k}}), x=(start + end)/2, y = 3), size=3) +

    geom_segment( data = na.omit(df %>% unnest_longer(Sequence)),
                  aes(x = Sequence, xend = Sequence, y = 4, yend = 5),
                  linewidth = 1.2, na.rm = T) +
    geom_point(
      data = na.omit(df %>% unnest_longer(Sequence)),
      aes(x = Sequence, size = 1), y = 5, na.rm = T) +

    scale_fill_discrete_qualitative()+

    coord_polar() +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none",
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

# ========= Sample test ============= #
# sample_df <- data.frame(group=c(1, 2, 3),
#                         length=c(2, 1, 9),
#                         loli_ns = c(1, 2, 3),
#                         k=c(0.5, 0.3, 0.17))

# make_donut_plot(sample_df, group = group, length = length, loli_ns = loli_ns, k = k)
#
