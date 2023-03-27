#' Parse timber details from raw GiB survey data
#'
#' @param df data.frame with raw GiB survey data
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr across
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @export
#' @rdname parse_user_details
#'
#' @return GiB timber details in data.frame
#' @examples
#' \dontrun{
#' parse_timber_details(gib_raw)
#' }



parse_timber_details <- function(df =  NULL) {


res <-
  df %>% filter(!Qu_main %in% c("UserNo","Name","Email","IP.Address","Unique.ID","Started","Ended","Q1..Please.enter.the.respondent.reference.number.provided.to.you.via.email..6.character.digit.alpha.numeric.reference..e.g..a12b3c..","Q2..How.many.timber.parcels.were.you.responsible.for.selling.buying.from.01.01.2022.to.31.12.2022.in.total.","Q3..Location.of.parcel..County","Q4..Location.of.parcel..Region","Q5..Overall.parcel.volume..please.answer.using.your.preferred.units...Please.provide.responses.in.either.cubic.metres.or.hoppus.feet.","Q6..Date.of.felling...estimated","Q7..Date.of.sale.purchase","Q8..Please.describe.whether.this.was.a.sale.or.purchase.of.a.felled.or.standing.timber.parcel.")) %>%
  pivot_longer(!Qu_main:Parameter) %>% group_by (name) %>% mutate(UserID=value[Qu_main=="UserID"]) %>%ungroup() %>%
  filter(!Qu_main=="UserID") %>%
  mutate(across(Qu_main:Qu_sub_sub,~gsub("[.]"," ",.x))) %>% mutate(Qu_main=trim(substr(Qu_main,4,nchar(Qu_main))), Qu_sub=ifelse(str_detect(Qu_sub,"Unfilled"),"Unfilled",trim(substr(Qu_sub,7,nchar(Qu_sub)))), Qu_sub_sub=ifelse(str_detect(Qu_sub_sub,"Unfilled"),"Unfilled",trim(substr(Qu_sub_sub,9,nchar(Qu_sub_sub)))))

}
