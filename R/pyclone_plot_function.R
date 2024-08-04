
#' Summarise pyclone result file
#'
#' Summarise from pyclone data
#' @param data Pyclone-vi result + isDriver denoting if the mutation is driver.
#' @return Preprocessed pyclone data
#' @import dplyr
summarise_pyclone_data <- function(data){
  if (length(unique(data$sample_id)) != 1){
    warning("Warning: The 'sample_id' column does not contain only one unique value.")
  }

  if (!("isDriver" %in% colnames(data))){
    data$isDriver = FALSE
    warning(paste("Warning: Column 'isDriver' does not exist in the dataframe. The default value will be False."))
  }

  data %>%
    select(mutation_id, sample_id, cluster_id, cellular_prevalence, isDriver) %>%
    group_by(cluster_id) %>%
    summarise(freq = n(), countDriver = sum(isDriver), cellular_prevalence = mean(cellular_prevalence)) %>%
    arrange(desc(cellular_prevalence)) %>%
    mutate(K = c(abs(diff(cellular_prevalence)), 1-sum(abs(diff(cellular_prevalence)))))

}



#' Make driver-annotated pyclone donut plot
#'
#' Summarise pyclone data with clonal sizes which represented by K values,
#' number of drivers in each cluster per unique cluster with unique CCF is
#' represented by number of lollipops.
#' @param data Pyclone-vi result + isDriver denoting if the mutation is driver.
#' @return Donut plot with lollipops to summarize driver-annotated pyclone data
#' @examples
#' data <- load_sample_pyclone()
#' # head(data, 3)
#' #> mutation_id sample_id cluster_id cellular_prevalence cellular_prevalence_std
#' #>        MUT1       PM1          0              0.9951                  0.0081
#' #>        MUT2       PM1          0              0.9951                  0.0081
#' #>        MUT3       PM1          0              0.9951                  0.0081
#' #> cluster_assignment_prob isDriver
#' #>                 1.0000    FALSE
#' #>                 0.9999     TRUE
#' #>                 1.0000     TRUE
#' make_pyclone_donut_plot(data)
#' @export
make_pyclone_donut_plot <- function(data){
  mydata <- summarise_pyclone_data(data)
  sample_id = data$sample_id[1]
  make_donut_plot(mydata, group = cluster_id, length = freq, loli_ns = countDriver, k =K) + ggtitle(sample_id)
}








