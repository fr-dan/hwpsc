#' Parse user details from raw GiB survey data
#'
#' @param df data.frame with raw GiB survey data
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @export
#' @rdname parse_user_details
#'
#' @return GiB user details in data.frame
#' @examples
#' \dontrun{
#' parse_user_details(gib_raw)
#' }



parse_user_details <- function(df =  NULL) {

  res <-
    df %>% filter(Qu_main %in% c("UserID","UserNo","Started","Ended","Q1..Please.enter.the.respondent.reference.number.provided.to.you.via.email..6.character.digit.alpha.numeric.reference..e.g..a12b3c..","Q2..How.many.timber.parcels.were.you.responsible.for.selling.buying.from.01.01.2022.to.31.12.2022.in.total.","Q3..Location.of.parcel..County","Q4..Location.of.parcel..Region","Q5..Overall.parcel.volume..please.answer.using.your.preferred.units...Please.provide.responses.in.either.cubic.metres.or.hoppus.feet.","Q6..Date.of.felling...estimated","Q7..Date.of.sale.purchase","Q8..Please.describe.whether.this.was.a.sale.or.purchase.of.a.felled.or.standing.timber.parcel.")) %>% mutate(Qu_main = ifelse(str_detect(Qu_main,"Q5.."),paste0(Qu_main,":",Qu_sub_sub),Qu_main)) %>%
    pivot_longer(!Qu_main:Parameter) %>%
    dplyr::select(-c(Parameter,Qu_sub,Qu_sub_sub))%>%
    pivot_wider(names_from=Qu_main,values_from = value) %>% dplyr::select(-name) %>%
    dplyr::rename("Unique_GiB_Ref"="Q1..Please.enter.the.respondent.reference.number.provided.to.you.via.email..6.character.digit.alpha.numeric.reference..e.g..a12b3c..","N_Timber_Parcels"="Q2..How.many.timber.parcels.were.you.responsible.for.selling.buying.from.01.01.2022.to.31.12.2022.in.total.","County"="Q3..Location.of.parcel..County","Region"="Q4..Location.of.parcel..Region","Parcel_Volume_m3"="Q5..Overall.parcel.volume..please.answer.using.your.preferred.units...Please.provide.responses.in.either.cubic.metres.or.hoppus.feet.:Q5.1.1. m3","Parcel_Volume_hft"="Q5..Overall.parcel.volume..please.answer.using.your.preferred.units...Please.provide.responses.in.either.cubic.metres.or.hoppus.feet.:Q5.1.2. hft","Date_felled_est"="Q6..Date.of.felling...estimated","Date_sale_purchase_est"="Q7..Date.of.sale.purchase","Cat_Key_Trt"="Q8..Please.describe.whether.this.was.a.sale.or.purchase.of.a.felled.or.standing.timber.parcel.")
}
