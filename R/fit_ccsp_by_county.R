#' Load the FIT and CCSP data by county
#'
#' @examples
#'
#' d <- fit_ccsp_by_county()
#'
#' head(d)
#'
#' summary(d)
#'
#' @export
fit_ccsp_by_county <- function() {
  # Load the FIT data
  fit_data <- readxl::read_excel(
    system.file("extdata/fit_and_ccsp_data_by_county.xlsx", package='AnschutzCOEData')
  )
  fit_data$FIT=as.logical(fit_data$FIT)
  fit_data$CCSP=as.logical(fit_data$CCSP)
  colnames(fit_data)[1] <- "county"
  fit_data$county=sub(' County','',fit_data$county)
  return(fit_data)
}
