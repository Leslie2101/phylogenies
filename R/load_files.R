#' Sample pyclone file for visualization
#'
#' Return a sample pyclone result, with driver genes annotated
#' @return A pyclone sample for drawing donut plot
#' @examples
#' data <- load_sample_pyclone()
#' print(head(data))
#' @export
load_sample_pyclone <- function(){
  readRDS(test_path("fixtures", "sample_pyclone_result.rds"))
}
