#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(rmarkdown)
library(rsconnect)
library(callr)
library(fresh)
library(viridis)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#555555"
  ),
  adminlte_sidebar(
    dark_bg = "#9ACA3C",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#555555",
    box_bg = "#9ACA3C",
    info_box_bg = "#9ACA3C"
  )
)

report_path <- tempfile(fileext = ".Rmd")
file.copy("gib_report.Rmd", report_path, overwrite = TRUE)

render_report <- function(input, output, params) {
    rmarkdown::render(input,
                      output_file = output,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
}


#setwd("C:/Users/jack.forster/OneDrive - Forest Research/Documents/HomeDrive/144b22/R Code and Data/Whinlatter_CP")
ui <- dashboardPage(
    dashboardHeader(title = "Hardwood Prices",
                    tags$li(a(href = 'http://www.company.com',
                              img(src = 'FRlogo_stacked_no_strap_white box.jpg',
                                  title = "Company Home", height = "30px"),
                              style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown"),
                    tags$li(a(href = 'http://www.company.com',
                              img(src = 'GIB-logo-web.png',
                                  title = "Company Home", height = "30px"),
                              style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown")),
    dashboardSidebar(
    sidebarMenu(
        tags$style(".skin-purple .sidebar .shiny-download-link { color: #444; }"),
        # Input: Select a sp ----
        div(selectInput("sp_input", "Select species:",list("Ash","Beech","Cherry","Lime","Mixed","Oak",
                                                             "Poplar","Sweet Chestnut","Sycamore"),multiple=FALSE),style="margin: 0px 5px 0px 5px;"),
        # Horizontal line ----
        tags$hr(),
        div(selectInput("pred_input", "Select a predictor:",list("Log/Tree Size","Sale/Purchase Type","Product","Region"),multiple=FALSE),style="margin: 0px 5px 0px 5px;"),
        # Horizontal line ----
        tags$hr(),
        div(selectInput("prod_input", "Optional additional grouping(s):",list("Sale/Purchase Type","Product","Region"),multiple=TRUE),style="margin: 0px 5px 0px 5px;"),
        # Horizontal line ----
        tags$hr(),
        div(actionButton("process_jf", "Go!"),style="margin: 0px 0px 0px 80px;"),
        # Horizontal line ----
        tags$hr(),
        # Input: Download dashboard ----
        div(style="display:inline-block;width=32%;text-align: center;",
            downloadButton("downloadReport", "Download Report",style="padding: 5px 14px 5px 14px;margin: 5px 5px 5px 38px;")),
        # Horizontal line ----
        tags$hr(),
        # Input: Download data ----
        div(style="display:inline-block;width=32%;text-align: center;",
        downloadButton("downloadData", "Download Data",style="padding: 5px 14px 5px 14px;margin: 5px 5px 5px 38px;"))
        )
    ),
    dashboardBody(use_theme(mytheme),tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;border-top: none;}'))),
                  fluidPage(box(width = 12,plotlyOutput("jf_1_plot")),
             tags$style(type = "text/css", ".plotly  {height: calc(100vh - 100px) !important; width: calc(100vw - 380px) !important;}"),)
    ),
    tags$head(tags$style(HTML('* {font-family: "Arial"};')))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

#Increase max upload size to 30Mb
options(shiny.maxRequestSize=30*1024^2)

# End user session when app is closed.

if (!interactive()) {
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
}

plots_jf <- eventReactive(input$process_jf, {
  tree_sp_func<-input$sp_input
  tree_pred_func<-input$pred_input
  tree_additional_func<-input$prod_input
  gib_clean_data_units_aligned<-read_xlsx("C:/Users/jack.forster/OneDrive - Forest Research/Documents/HomeDrive/157b22_GiB/R Code and Data/gib_clean_data_units_aligned.xlsx")

  gib_clean_data_analysis<-gib_clean_data_units_aligned%>%filter(Tree_Species==tree_sp_func) #select relevant tree species

  tree_pred_func_orig<-tree_pred_func
  tree_pred_func<-dplyr::recode(tree_pred_func,"Sale/Purchase Type"="Cat_Key_Trt")
  tree_additional_func<-dplyr::recode(unlist(tree_additional_func),"Sale/Purchase Type"="Cat_Key_Trt")
  # recode inputs


  if(tree_pred_func == "Log/Tree Size") {
    if(length(tree_additional_func)>1) {tree_additional_func<-paste0("interaction(",paste(tree_additional_func,collapse=","),")")}
    tree_additional_func_group<-paste0("interaction(","Average_Size_Clean_m3",",tree_additional_func",")")
    gg_tree_size<-ggplot(gib_clean_data_analysis,aes_string(x="Average_Size_Clean_m3",y="Price_Clean_m3",colour=tree_additional_func,fill=tree_additional_func,group=tree_additional_func))+
      theme_bw()+
      geom_smooth(aes(text=paste0("Best fit line: ",scales::comma(Price_Clean_m3,accuracy=0.01))),method = 'lm')+
      geom_jitter(aes(text=paste0("Size = ",scales::comma(Average_Size_Clean_m3,accuracy=0.1)," m<sup>3</sup>","\n","Price = ",scales::comma(Price_Clean_m3,accuracy=0.01),"GBP\n",Cat_Key_Trt,"\n",Product,"\n",Region)),
                  size=1.5,alpha=0.7,height=0,width=0.025)+
      labs(x="Average log/tree size (m<sup>3</sup>)",y="Price (GBP)",colour="Grouping\n",fill="Grouping\n",title=paste(tree_sp_func," prices"))+
      scale_colour_viridis(discrete=TRUE)+
      scale_fill_viridis(discrete=TRUE)

    gg_tree_size<-ggplotly(gg_tree_size,tooltip=c("text"))%>%layout(title=list(x=0.5,font=list(size=20)),legend = list(x=1.1,y=0.5,yanchor="middle"),hoverlabel=list(xanchor="left",yanchor="middle"))%>%style(textposition = "left")
    for(i in 1: length(gg_tree_size$x$layout$shapes)) {gg_tree_size$x$layout$shapes[[i]]$line$width<-0.5}
    gg_tree_size

  } else {

    if(length(tree_additional_func)>1) {tree_additional_func<-paste0("interaction(",paste(tree_additional_func,collapse=","),")")}
  tree_additional_func_group<-paste0("interaction(",tree_pred_func,",",tree_additional_func,")")
  gg_tree_size<-ggplot(gib_clean_data_analysis,aes_string(x=tree_pred_func,y="Price_Clean_m3",colour=tree_additional_func,fill=tree_additional_func,group=tree_additional_func_group))+
    theme_bw()+
    geom_violin(colour=NA,alpha=0.4,position=position_dodge(width=0.4))+
    geom_point(aes(text=paste0("Size = ",scales::comma(Average_Size_Clean_m3,accuracy=0.1)," m<sup>3</sup>","\n","Price = ",scales::comma(Price_Clean_m3,accuracy=0.01),"GBP\n",Cat_Key_Trt,"\n",Product,"\n",Region)),position=position_jitterdodge(jitter.width=0.1,dodge.width=0.4),
               size=1.5,alpha=0.7,height=0,width=0.075)+
    labs(x=paste(tree_pred_func_orig),y="Price (GBP)",colour="Grouping\n",fill="Grouping\n",title=paste(tree_sp_func," prices"))+
    scale_colour_viridis(discrete=TRUE)+
    scale_fill_viridis(discrete=TRUE)

  gg_tree_size<-ggplotly(gg_tree_size,tooltip=c("text"))%>%layout(title=list(x=0.5,font=list(size=20)),legend = list(x=1.1,y=0.5,yanchor="middle"),hoverlabel=list(xanchor="left",yanchor="middle"))%>%style(textposition = "left")
  for(i in 1: length(gg_tree_size$x$layout$shapes)) {gg_tree_size$x$layout$shapes[[i]]$line$width<-0.5}
  for(i in 1:length(gg_tree_size$x$data))
  {if(length(gg_tree_size$x$data[[i]]$text)>1000) {gg_tree_size$x$data[[i]]$text<-""}}
  for(i in 1:length(gg_tree_size$x$data))
  {if(str_detect(gg_tree_size$x$data[[i]]$name,"[)]")) {gg_tree_size$x$data[[i]]$showlegend<-FALSE}}
  gg_tree_size}
  }
  )


output$jf_1_plot<-renderPlotly({plots_jf()})

output$downloadData <- downloadHandler(
    filename = 'data_file.csv',
    content = function(file) {
        df1<-data.frame(data_bind())
        write.csv(df1, file, row.names = FALSE)
    }
)



output$downloadReport <-downloadHandler(
    filename = 'whinlatter_report.html',

    content = function(file) {
        withProgress(message = 'Making report, please wait! Might take five mins...', {
        params <- list(df1_rmd=data.frame(data_bind()))

        callr::r(
            render_report,
            list(input = report_path, output = file, params = params)
        )})
    }
)



}

# Run the application
shinyApp(ui = ui, server = server)
