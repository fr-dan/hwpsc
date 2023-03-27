#' Wrapped read in functionality
#'
#' @param path path to smart survey data
#' @export
#' @rdname full_gib_read
#'
#' @return clean GiB dataset
#' @examples
#' \dontrun{
#' full_gib_read("23-03-27.csv")
#' }


full_gib_read <- function(path = NULL) {


  res <- read_gib_raw(path)

  user <- parse_user_details(res)

  timber <- parse_timber_details(res)

  num_timber <- parse_num_timber_details(timber)

  char_timber <- parse_char_timber_details(timber)

  gps_timber <- parse_gps_timber_details(timber)

  df <- gib_clean_data(user_details, char_timber, gps_timber, num_timber)
}
