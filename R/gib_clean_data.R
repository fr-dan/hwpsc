#' Join all gib datasets
#'
#' @param gib_raw_user_details parsed user details
#' @param gib_raw_character_timber_details parsed char timber details
#' @param gib_raw_gps_timber_details parsed gps timber details
#' @param gib_raw_numeric_details parsed num timber details
#'
#'
#'
#'
#' @importFrom dplyr full_join
#' @export
#' @rdname gib_clean_data
#'
#' @return clean GiB dataset
#' @examples
#' \dontrun{
#' gib_clean_data(gib_raw)
#' }


gib_clean_data <- function(gib_raw_user_details =  NULL,
                           gib_raw_character_timber_details = NULL,
                           gib_raw_gps_timber_details = NULL,
                           gib_raw_numeric_timber_details = NULL) {


  res <-full_join(full_join(full_join(gib_raw_user_details,gib_raw_character_timber_details)
                                       ,gib_raw_gps_timber_details),gib_raw_numeric_timber_details)
}
