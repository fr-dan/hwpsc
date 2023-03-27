#' Parse numeric details from timber details GiB data
#'
#' @param df data.frame with parsed timber details from GiB survey
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_starts
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @export
#' @rdname parse_user_details
#'
#' @return GiB numeric timber details in data.frame
#' @examples
#' \dontrun{
#' parse_num_timber_details(gib_raw)
#' }



parse_num_timber_details <- function(df =  NULL) {


  res <- df %>%
    filter(str_starts(Qu_main,"Timber|Firewood")) %>%
    dplyr::select(UserID,Qu_main,Qu_sub,Qu_sub_sub,value) %>%
    group_by(UserID,Qu_main,Qu_sub,Qu_sub_sub) %>%
    summarise(value=sum(as.numeric(value),na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Timber_Firewood=ifelse(str_starts(Qu_main,"Timber"),"Timber","Firewood"),Tree_Species_Product=Qu_sub) %>%
    dplyr::select(UserID,Timber_Firewood,Tree_Species_Product,Qu_sub_sub,value) %>%
    pivot_wider(names_from=Qu_sub_sub, values_from=value)

}
