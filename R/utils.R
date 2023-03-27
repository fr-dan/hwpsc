trim <- function (x) gsub("^\\s+|\\s+$", "", x)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

str_break <- function(str, n) {
  paste(stringi::stri_wrap(str, n, normalise=TRUE,simplify=TRUE),collapse="\n")}

div_palette_gr_fun<-function(x){c(rgb(seq(98,134,length.out = x),seq(166,32,length.out = x),seq(14,65,length.out = x),maxColorValue = 255))}

div_palette_gp_fun<-function(x){c(rgb(seq(98,103,length.out = x),seq(166,37,length.out = x),seq(14,127,length.out = x),maxColorValue = 255))}

div_palette_gbr_fun<-function(x){c(rgb(seq(98,105,length.out = x),seq(166,65,length.out = x),seq(14,44,length.out = x),maxColorValue = 255))}

div_palette_gb_fun<-function(x){c(rgb(seq(98,0,length.out = x),seq(166,115,length.out = x),seq(14,144,length.out = x),maxColorValue = 255))}

qual_palette<-c(rgb(98,166,14,maxColorValue = 255),rgb(103,37,127,maxColorValue = 255),rgb(185,73,23,maxColorValue = 255),rgb(134,32,65,maxColorValue = 255),rgb(0,115,144,maxColorValue = 255),rgb(0,105,57,maxColorValue = 255),rgb(187,243,118,maxColorValue = 255),rgb(199,140,221,maxColorValue = 255),rgb(239,157,122,maxColorValue = 255),rgb(226,135,165,maxColorValue = 255),rgb(106,225,255,maxColorValue = 255),rgb(106,255,188,maxColorValue = 255))

Qu_sub_not_fill_match<-c("X...UserID","UserNo","Name","Email","IP.Address","Unique.ID","Started","Ended","Q1..Please.enter.the.respondent.reference.number.provided.to.you.via.email..6.character.digit.alpha.numeric.reference..e.g..a12b3c..","Q2..How.many.timber.parcels.were.you.responsible.for.selling.buying.from.01.01.2022.to.31.12.2022.in.total.","Q3..Location.of.parcel..County","Q4..Location.of.parcel..Region","Q8..Please.describe.whether.this.was.a.sale.or.purchase.of.a.felled.or.standing.timber.parcel.")

