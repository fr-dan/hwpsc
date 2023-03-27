#' Parse character details from timber details GiB data
#'
#' @param df data.frame with parsed timber details from GiB survey
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_starts
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr slice_head
#' @importFrom tidyr pivot_wider
#' @export
#' @rdname parse_user_details
#'
#' @return GiB character timber details in data.frame
#' @examples
#' \dontrun{
#' parse_char_timber_details(gib_raw)
#' }



parse_char_timber_details <- function(df =  NULL) {

  res <- df %>%
    filter(str_starts(Qu_main,"Timber|Firewood")==FALSE)%>%
    filter(str_starts(Qu_main,"Location of timber parcel")==FALSE) %>%
    dplyr::select(UserID,Qu_main,value) %>%
    distinct() %>% arrange(desc(value)) %>%
    group_by(UserID,Qu_main)%>%
    slice_head(n=1) %>%
    pivot_wider(names_from=Qu_main, values_from=value)


}
