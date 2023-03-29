#' Read in raw GiB Smart Survey data
#'
#' @param path file path
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr select
#' @export
#' @rdname read_gib_raw
#'
#' @return raw GiB survey data
#' @examples
#' \dontrun{
#' read_gib_raw(here("data_raw", "23-03-27.csv"))
#' }



read_gib_raw <- function(path = NULL) {
  gib_raw = data.frame(t(read.csv(path)))

  gib_raw <-
    gib_raw %>% mutate(
      Qu_main = trim(paste0(row.names(df_raw))),
      Qu_sub = trim(paste0(X1)),
      Qu_sub_sub = trim(paste0(df_raw$X2)),
      Qu_main = ifelse(str_detect(Qu_sub, "Site specific or other factors that alter prices"),"QXX Site specific or other factors that alter prices",Qu_main),
      Qu_sub = ifelse(str_detect(Qu_main, "Are.there.any.hardwood.tree.species.that.were.not"),"Unfilled",Qu_sub),
      Qu_sub = ifelse(Qu_main %in% Qu_sub_not_fill_match, "Unfilled", Qu_sub),
      Qu_main = ifelse(str_detect(Qu_main,"UserID"), "UserID", Qu_main),
      Qu_main = ifelse(substr(Qu_main, 1, 1) == "X", NA, Qu_main),
      Qu_sub = ifelse(Qu_sub == "", NA, Qu_sub)
    ) %>% fill(Qu_main:Qu_sub) %>%
    mutate(Parameter = paste(Qu_main, Qu_sub, Qu_sub_sub, sep =":")) %>%
    dplyr::select(-c(X1:X2))
  row.names(gib_raw)<-NULL
}
