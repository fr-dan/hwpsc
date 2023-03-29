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
    dashboardHeader(title = "Grown in Britain: Hardwood Prices",
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
        # Input: Select a time ----
        div(selectInput("sp_input", "Select a species:",list("Ash","Beech","Cherry","Lime","Mixed","Oak",
                                                             "Poplar","Sweet Chestnut","Sycamore"),multiple=TRUE),style="margin: 0px 5px 0px 5px;"),
        # Horizontal line ----
        tags$hr(),
        div(selectInput("prod_input", "Select a product:",list("Beam","Biomass/Firewood","Mixed/Unknown","Planking","Sawlog"),multiple=TRUE),style="margin: 0px 5px 0px 5px;"),
        # Horizontal line ----
        tags$hr(),
        div(selectInput("pred_input", "Select a predictor:",list("Log/Tree Size","Species","Product","Region"),multiple=FALSE),style="margin: 0px 5px 0px 5px;"),
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
        box(width = 12,height=800,title = "Files uploaded: ",div(DT::dataTableOutput("table1")))
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

temp1 <- eventReactive(input$process_jf, {
    withProgress(message = 'Processing', value = 0, {
      load("traveltime_all_60.Rdata")
      step1<-traveltime_all_60 # replace with forest_10_tt(60)
      incProgress(0.25,detail = paste("25% complete"))
      step2<-forest_ons_linkage1(step1)
      incProgress(0.5,detail = paste("50% complete"))
      step3<-forest_ons_linkage2(step2)
      incProgress(0.75,detail = paste("75% complete"))
      step4<-forest_ons_bind(step3)
      incProgress(1,detail = paste("100% complete"))})
    list(step1, step2, step3, step4)
  }
  )

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
