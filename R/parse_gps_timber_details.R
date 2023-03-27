#' Parse GPS details from timber details GiB data
#'
#' @param df data.frame with parsed timber details from GiB survey
#'

#' @importFrom stringr str_starts
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr slice_head
#' @importFrom tidyr pivot_wider
#' @export
#' @rdname parse_user_details
#'
#' @return GiB GPS timber details in data.frame
#' @examples
#' \dontrun{
#' parse_gps_timber_details(gib_raw)
#' }


parse_gps_timber_details <- function(df =  NULL) {


res <- df %>%
  filter(str_starts(Qu_main,"Timber|Firewood")==FALSE)%>%
  filter(str_starts(Qu_main,"Location of timber parcel")) %>%
  dplyr::select(UserID,Qu_sub_sub,value) %>%
  distinct() %>%
  arrange(desc(value)) %>%
  group_by(UserID,Qu_sub_sub)%>%
  slice_head(n=1) %>%
  pivot_wider(names_from=Qu_sub_sub, values_from=value)
}
