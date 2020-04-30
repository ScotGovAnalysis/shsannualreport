# libraries ####

library(shiny)
library(shinyjs)
library(plyr)
library(dplyr)
library(DT)
library(magrittr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
# library(ggthemes)
# library(shinythemes)

# sources ####

source("source/variables.R")$values
source("source/functions.R")$values
source("source/report_data_processing.R")$values
source("source/report_functions.R")$values

# ui ####

ui <- fluidPage(

    # Welcome banner ####

    tagList(

        useShinyjs(),

        tags$div(id="welcome_banner",
                 "Welcome to the new Scottish Household Survey Data Explorer.",
                 br(),
                 fluidRow(
                     column(11, "We are still working on the site and welcome any comments and suggestions to ",
                            tags$a(href = "mailto:shs@gov.scot", "shs@gov.scot")),
                     column(1, actionButton("close_banner", "" , icon=icon("times")))
                 ),
                 style = "padding-top:20px; padding-left:20px; padding-bottom:20px; background-color:#ffd480;font-weight:bold;")
    ),

    # Navbar page ####

    navbarPage(title="SHS Data Explorer",
               id = "navbar",
               windowTitle = "SHS Data Explorer",
               theme = shinythemes::shinytheme("flatly"),
               collapsible = TRUE,

               # Home tab ####

               tabPanel("Home", value = "homeTab", style = "margin-left: 10%; margin-right: 10%",
                        fluidRow(
                            HTML('<center><img src = "home_logo.png"></center>'),
                            column(8, offset = 2, wellPanel(
                                tags$h1("Scottish Household Survey", style = "text-align: center"),
                                tags$h1("Data Explorer", style = "text-align: center"),
                                br(),
                                tags$h4("Since 1999, the Scottish Household Survey collects and provides information about Scottish households.", style = "text-align: center"),
                                tags$h4("This website provides up-to-date, comparable information on Scottish households at local authority level. The survey covers 11 different topics. Choose your topic of interest below and start exploring the data!", style = "text-align: center"),
                                br()
                            )
                            )
                        ),

                        fluidRow(column(8, offset = 2, actionButton("topic", "Topics", width = "100%", style = "color: #fff; background-color: #2C3E50; font-size: 200%"))),

                        fluidRow(
                            column(4, offset = 2, actionButton("home_to_demographics", "Demographics", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, actionButton("home_to_housing", "Housing", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, offset = 2, actionButton("home_to_neighbourhoods", "Neighbourhoods", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, actionButton("home_to_economic_activity", "Economic Activity", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, offset = 2, actionButton("home_to_finance", "Finance", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, actionButton("home_to_internet", "Internet", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, offset = 2, actionButton("home_to_physical_activity", "Physical Activity", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, actionButton("home_to_local_services", "Local Services", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, offset = 2, actionButton("home_to_environment", "Environment", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, actionButton("home_to_volunteering", "Volunteering", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%")),
                            column(4, offset = 2, actionButton("home_to_culture", "Culture", width = "100%", style = "color: #fff; background-color: #008080; font-size: 150%"))),

                        fluidRow(br(),
                                 h4("Note: SHS data on the topic 'Childcare' reported at national level is not reproduced at local authority level due to base sizes being too small to produce robust findings")
                        ),

                        br(), br(),
                        column(7, p(img(src = "SG_master_logo_RGB.jpg", width = "100%", height = "100%"))),
                        column(2, offset = 3, p(img(src = "nat_stat.png", width = 130, height = 130))),
                        br(), br(), br(), br(), br(),
                        actionButton("reload_modal", "Reload 'Take a Tour'", style = "text-align: right"),
                        br(), br()
               ),

               # About tab ####

               tabPanel("About", value = "aboutTab",
                        fluidRow(column(8, offset = 2, includeMarkdown("home.Rmd"))
                        )
               ),

               # Survey Results tab ####

               tabPanel(
                   div(icon("fas fa-chart-line"),
                       "Survey Results"), value = "surveyTab",
                   style = "margin-left: 4%; margin-right: 4%",

                   wellPanel(

                       fluidRow(
                           column(5, radioButtons("navigation_mode", "Select question", c("Browse by topic and question", "Search questions"), inline = TRUE))
                       ),

                       fluidRow(
                           conditionalPanel(condition = "input.navigation_mode == 'Search questions'",
                                            column(12, selectizeInput('searchbar', 'Search',
                                                                      choices = c(select_list_questions_topic_2,
                                                                                  select_list_questions_topic_3,
                                                                                  select_list_questions_topic_4,
                                                                                  select_list_questions_topic_5,
                                                                                  select_list_questions_topic_6,
                                                                                  select_list_questions_topic_7,
                                                                                  select_list_questions_topic_8,
                                                                                  select_list_questions_topic_9,
                                                                                  select_list_questions_topic_10,
                                                                                  select_list_questions_topic_11,
                                                                                  select_list_questions_topic_12),
                                                                      selected="Search",
                                                                      options = list(
                                                                          placeholder = "Type here to find what question you are looking for",
                                                                          onInitialize = I('function() { this.setValue(""); }')
                                                                      )
                                            ))
                           ),
                           conditionalPanel(condition = "input.navigation_mode == 'Browse by topic and question'",
                                            column(5, selectInput("select_topic", label = "Topic", choices = select_list_topics, width = "100%"))),
                           conditionalPanel(condition = "input.navigation_mode == 'Browse by topic and question'",
                                            column(7, selectInput("select_question", label = "Question", choices = c(), width = "100%")))
                       )
                   ),
                   wellPanel(
                       fluidRow(
                           column(3, selectInput("select_local_authority", label = "Local Authority", choices = local_authorities, selected = "Scotland", width = "100%")),
                           column(3, selectInput("select_year", label = "Year", choices = c(), width = "100%")),
                           column(3, selectInput("select_comparison_type", label = "Compare by", choices = c("No comparison", "Year", "Local Authority/Scotland"), selected = "No comparison", width = "100%")),
                           column(3, conditionalPanel(condition = "input.select_comparison_type == 'Year'", selectInput("select_year_comparator", label = "Comparator", choices = c(), width = "100%"))),
                           column(3, conditionalPanel(condition = "input.select_comparison_type == 'Local Authority/Scotland'",selectInput("select_local_authority_comparator", label = "Comparator", choices = c(), width = "100%")))
                       )
                   ),

                   fluidRow(
                       tabsetPanel(
                           tabPanel("Table",
                                    fluidRow(h3(textOutput("main_title"))),
                                    fluidRow(
                                        column(8, h4(textOutput("main_table_type_comment"))),
                                        column(2, conditionalPanel(condition = "output.question_type != '0'", downloadButton("download_table", "Download Table"))),
                                        column(1, offset = 1, actionButton("helpTable", icon("question")))
                                    ),
                                    fluidRow(h5(textOutput("comment"))),
                                    fluidRow(h5(htmlOutput("link"))),
                                    fluidRow(dataTableOutput("main_table")),
                                    fluidRow(h4(htmlOutput("statistical_significance_key"))),
                                    fluidRow(h3(textOutput("comparison_title"))),
                                    fluidRow(
                                        column(10, h4(textOutput("comparison_table_type_comment"))),
                                        column(2, conditionalPanel(condition = "input.select_comparison_type != 'No comparison' && output.question_type != '0'", downloadButton("download_comparison_table", "Download comparison table")))
                                    ),
                                    fluidRow(dataTableOutput("comparison_table"))
                           ),

                           tabPanel("Chart",
                                    fluidRow(
                                        column(6, h3(textOutput("main_plot_title"))),

                                        column(3, conditionalPanel(condition = "output.question_type != '0' && output.question_type != '4'", checkboxInput("ConfidenceInterval", "Display Confidence Intervals", value = TRUE))),
                                        column(2, conditionalPanel(condition = "output.question_type != '0' && output.question_type != '4'", radioButtons("zoomLevel_main",
                                                                                                                                                          "Y-axis zoom level:",
                                                                                                                                                          selected = "Full scale",
                                                                                                                                                          choices = c("Zoom to data", "Full scale")))),
                                        column(1, actionButton("help", icon("question")))
                                    ),
                                    fluidRow(h4(textOutput("main_chart_type_comment"))),


                                    fluidRow(plotly::plotlyOutput("main_chart")),
                                    fluidRow(h3(textOutput("comparison_plot_title"))),

                                    conditionalPanel(condition = "input.select_comparison_type != 'No comparison'",
                                                     fluidRow(plotly::plotlyOutput("comparison_chart")),
                                                     fluidRow(
                                                         column(3, offset = 6, checkboxInput("compareConfidenceInterval", "Display Confidence Intervals", value = TRUE)),
                                                         column(3, radioButtons("zoomLevel_comparator", "Y-axis zoom level:", selected = "Full scale", choices = c("Zoom to data", "Full scale")))
                                                     )
                                    )
                           )
                       )
                   )
               ),

               # LA Reports tab ####

               tabPanel("LA Reports",

                        wellPanel(style = "background: #ffd480",
                                  h4("This function is still under construction."),
                                  h5("Below you can download an example chapter for a local authority to see what it might look like in the future. In the mean time if you require local authority reports, please", tags$a(href = "https://www2.gov.scot/Topics/Statistics/16002/LAtables2018", target = "_blank", "click here!"))
                        ),

                        fluidRow(
                            column(8, selectInput("select_report_topic", label = "Topic", choices = select_list_topics, width = "100%"))
                        ),

                        fluidRow(
                            column(3, selectInput("select_report_local_authority", "Select Local Authority", choices = local_authorities)),
                            column(3, selectInput("select_report_year", "Select Year", choices = c("2018", "2017", "2016", "2015", "2014", "2013"))), # TODO: Update choices dynamically
                            column(3, selectInput("select_report_comparison_type", label = "Compare by", choices = c("No comparison", "Year", "Local Authority"), selected = "No comparison", width = "100%")), # TODO: Update choices
                            column(3, conditionalPanel(condition = "input.select_report_comparison_type == 'Year'", selectInput("select_report_year_comparator", label = "Comparator", choices = c("2018", "2017", "2016", "2015", "2014", "2013"), width = "100%"))), # TODO: Update choices dynamically
                            column(3, conditionalPanel(condition = "input.select_report_comparison_type == 'Local Authority'",selectInput("select_report_local_authority_comparator", label = "Comparator", choices = c(local_authorities), width = "100%")))
                        ),

                        fluidRow(
                            column(3, actionButton("generate", "Generate Report", icon = icon("file"))),
                            column(3, conditionalPanel(condition = "output.reportbuilt", downloadButton("download", "Download Report")))
                        )
               ),

               # Raw Data tab ####

               tabPanel(
                   div(icon("far fa-folder-open"), "Data"), value = "csv", style = "margin-left: 4%; margin-right: 4%; margin-bottom: 4%",

                   wellPanel(
                       h4("Below you will find all the data for each table and chart found in our survey results."),
                       h4("You can download the full Scottish Household Survey micro-level datasets at",  tags$a(href = "https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000048", target = "_blank", "UK Data Service.")
                       )),

                   fluidRow(
                       column(5, selectInput("select_excel_topic", label = "Topic", choices = select_list_topics, width = "100%")),
                       column(7, selectInput("select_excel_question", label = "Question", choices = c(), width = "100%"))
                   ),

                   fluidRow(dataTableOutput("excel_table"))
               ),

               # Resources tab ####

               tabPanel("Resources", value = "resourcesTab", style = "margin-left: 4%; margin-right: 4%",

                        fluidRow(
                            column(4, offset = 4, p(img(src = "new_logo.png", height = "100%", width = "100%")))
                        ),

                        fluidRow(
                            column(3,
                                   wellPanel(
                                       p(tags$h3("Get in touch...")), br(),
                                       p("SHS Project Team, Communities Analysis Division, Area 2H North, Victoria Quay, Edinburgh, EH6 6QQ"), br(),
                                       p("Email:", tags$a(href = "mailto:shs@gov.scot", "shs@gov.scot")),
                                       p("Telephone: 0131 244 1685"), br()
                                   )
                            ),

                            column(4,
                                   wellPanel(
                                       tags$h3("Technical resources"), br(),
                                       p("For the most recent annual SHS publication,", tags$a(href = "https://www.gov.scot/publications/scotlands-people-annual-report-results-2018-scottish-household-survey/", target = "_blank", "Click here!")),
                                       p("For the most recent key findings report,", tags$a(href = "https://www.gov.scot/publications/scottish-household-survey-key-findings-2018/", target = "_blank", "Click here!")),
                                       p("To see the full questionnarie for each year,", tags$a(href = "https://www2.gov.scot/Topics/Statistics/16002/PublicationQuestionnaire", target = "_blank", "Click here!")),
                                       p("To view the SHS methodology,", tags$a(href = "https://www.gov.scot/publications/scottish-household-survey-methodology-fieldwork-outcomes-2017/", target = "_blank", "Click here!")),
                                       br(), br(), br()
                                   )
                            ),

                            column(5, HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/OgMbrDXZK-s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                        ),

                        fluidRow(
                            column(7,
                                   wellPanel(
                                       tags$h3("Learn more about SHS"),
                                       tags$h5("The SHS team is passionate about finding new ways to share our data and findings. Below is a range of different media."),
                                       br(),
                                       p(icon("far fa-book"), tags$a(href = "https://shs.theapsgroup.scot/september-2018/", target = "_blank", "Inquality Data Comic"), tags$em("illustrated by Katie Quinn")),
                                       p(icon("far fa-book"), tags$a(href = "https://shs.theapsgroup.scot/", target = "_blank", "Housing and Finances Data Comic"), tags$em("illustrated by Katie Quinn")),
                                       p(icon("far fa-book"), tags$a(href = "https://www.gov.scot/publications/single-step/", target = "_blank", "Physical Activity Report")),
                                       p(icon("fas fa-video"), tags$a(href = "https://www.youtube.com/watch?v=OgMbrDXZK-s&feature=youtu.be", target = "_blank", "Scottish Household Survey Animation")),
                                       p(icon("fas fa-video"), tags$a(href = "https://twitter.com/scotgov/status/1171469690809401351?ref_src=twsrc%5Etfw%22%3E", target = "_blank", "SHS 2018 Findings Video")),
                                       p(icon("fas fa-microphone"), tags$a(href = "https://twitter.com/digitalscots/status/1183726105662050310?s=20&utm_source=FutureScot+-+MASTER&utm_campaign=7f20a2c022-EMAIL_CAMPAIGN_1_10_2019_10_25_COPY_01&utm_medium=email&utm_term=0_a4962b4dbd-7f20a2c022-147245485", target = "_blank", "Scottish Household Survey Podcast"), tags$em("with Roger Halliday and Emma McCallum")),
                                       p(icon("fas fa-window-restore"), tags$a(href = "https://scotland.shinyapps.io/sg-scottish-household-survey/", target = "_blank", "Scottish Household Survey App")),
                                       p(icon("fas fa-clipboard-list"), tags$a(href = "https://docs.google.com/forms/d/e/1FAIpQLScGNvzKa-cspn_-tLf4yzSzwDuojRgA6pWV5Hl54al_t-EBjA/viewform", target = "_blank", "SHS 2018 Findings Quiz"))
                                   )
                            ),

                            column(5,
                                   HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/LI-RlYzH0ug" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                            )
                        )
               )
    )
)

# server ####

server <- function(input, output, session) {


    # Welcome Modal ####

    welcome_modal <- modalDialog(

        p(img(src = "new_logo.png", height = "100%", width = "100%"), style = "text-align: center"),
        br(),
        tags$div(h4("This interactive tool provides information about Scottish homes, neighbourhoods, and their views on various aspects of society."),
                 style = "color: 0E3E5D; font-size:20px; text-align: center"),
        br(),
        h4("If this is your first time using the tool, take a tour to learn more about how to get the most out of it."),
        actionButton("tour", "Take a tour", icon("play-circle")),
        actionButton("modal_to_resources", "Resources", icon("wrench")),

        easyClose = TRUE, fade = FALSE
    )

    #Shows modal first time
    showModal(welcome_modal)


    #Links modal button to 'resources' tab
    observeEvent(input$modal_to_resources, {
        updateTabsetPanel(session, "navbar",
                          selected = "resourcesTab")
    })

    #Reloads modal on request from home tab
    observeEvent(input$reload_modal,
                 {showModal(welcome_modal)})


    #TOUR 1
    tour_modal_1 <- modalDialog(
        size = "l",
        fluidRow(
            br(),
            img(src = "modal-into.png", height = "50%", width = "50%"), style = "text-align: center"),
        h4("The Scottish Household Survey Data Explorer is an interactive tool created so that anyone can access the survey results, compare data over time and between different parts of Scotland. All the data and charts can be exported in various formats to use for your own analysis and reports. Let's take a tour!"),

        actionButton("next1", "Next", icon("play-circle"))
    )

    observeEvent(input$tour, {showModal(tour_modal_1)})
    observeEvent(input$next1, {showModal(tour_modal_2)})

    #TOUR 2
    tour_modal_2 <- modalDialog(
        size = "l",
        fluidRow(
            column(12,
                   p(tags$div("Local Authority Data in 'Survey Results'", style = " color: 0E3E5D; font-size:30px; width = 90%, text-align: left;")),
                   br(),
                   h4("This is where all the interactive data lives. Use the different drop-down menus to select different topics, questions, local authorities, years and comparators!"),
                   br())),
        fluidRow(
            img(src = "modal_survey2.png", height = "100%", width = "100%")),
        br(),
        actionButton("back1", "Back", icon("chevron-circle-left")),
        actionButton("next2", "Next", icon("play-circle"))

    )

    observeEvent(input$back1, {showModal((tour_modal_1))})
    observeEvent(input$next2, {showModal(tour_modal_3)})

    #TOUR 3
    tour_modal_3 <- modalDialog(
        size = "l",
        fluidRow(
            column(12,
                   p(tags$div("Reading the tables", style = " color: 0E3E5D; font-size:30px; width = 90%, text-align: left;")),
                   br())),
        fluidRow(
            img(src = "modal_table2.png", height = "100%", width = "100%")),
        actionButton("back2", "Back", icon("chevron-circle-left")),
        actionButton("next3", "Next", icon("play-circle"))
    )

    observeEvent(input$back2, {showModal(tour_modal_2)})
    observeEvent(input$next3, {showModal(tour_modal_4)})

    #TOUR 4
    tour_modal_4 <- modalDialog(
        size = "l",
        fluidRow(
            column(12,
                   p(tags$div("Visualising the data with graphs", style = " color: 0E3E5D; font-size:30px; width = 90%; text-align: left;")),
                   br(),
                   h4("The graphs related to each question are interactive. This means that you can modify them to fit your needs."),
                   br())),
        fluidRow(
            img(src = "modal_chart.png", height = "100%", width = "100%")),
        actionButton("back3", "Back", icon("chevron-circle-left")),
        actionButton("next4", "Next", icon("play-circle"))
    )

    observeEvent(input$back3, {showModal(tour_modal_3)})
    observeEvent(input$next4, {showModal(tour_modal_5)})

    #TOUR 5
    tour_modal_5 <- modalDialog(
        size = "l",
        fluidRow(
            p(tags$div("Statistical significance", style = "color: 0E3E5D; font-size: 30px; width = 90%; text-align: left")),
            br(),
            h5("The Scottish Household Survet can make generalisations about the Scottish population as a whole by only interviewing a random sample of all the Scottish households. This means that figures are estimates rather than precise percentages, and come with a degree of error. In fact, the 'true' value might be higher or lower than the estimate. This range is called the 'confidence interval'"),
            h5("For time or local authority comparisons, the tables and charts indicate whether any difference is statistically significant or not."),
            tags$li("Tables: Dark green colour indicates a value statistically significantly greater than the comparison. Light purple colour indicates a value stastically significantly lower than the comparison. Cells with no colour indicate no statistical difference."),
            tags$li("Charts: Hovering over the error bars will display the exact range of the confidence interval.")),
        fluidRow(
            column(6,
                   img(src = "sign_table.png", height = "100%", width = "100%")),
            column(6,
                   img(src = "ci_graph.png", height = "100%", width = "100%"))),
        actionButton("back4", "Back", icon("chevron-circle-left")),
        actionButton("next5", "Next", icon("play-circle"))
    )

    observeEvent(input$back4, {showModal(tour_modal_4)})
    observeEvent(input$next5, {showModal(tour_modal_6)})

    #TOUR 6
    tour_modal_6 <- modalDialog(
        size = "l",
        fluidRow(
            column(12,
                   p(tags$div("Downloading Data", style = " color: 0E3E5D; font-size:30px; width = 90%, text-align: left;")),
                   br(),
                   h4("You can modify and download the data you need. In the 'Download Data' tab you can view all the data used to produce the tables and charts. Either copy or download all the data for a specific question or you use the filter function to retrieve the data for a specific criteria."),
                   br())),
        fluidRow(
            img(src = "modal_download.png", height = "100%", width = "100%")
        ),
        actionButton("back5", "Back", icon("chevron-circle-left")),
        actionButton("next6", "Next", icon("play-circle"))
    )

    observeEvent(input$back5, {showModal(tour_modal_5)})
    observeEvent(input$next6, {showModal(tour_modal_7)})

    #TOUR 7
    tour_modal_7 <- modalDialog(
        size = "l",
        fluidRow(
            column(12,
                   p(tags$div("You're all set to go!", style = " color: 0E3E5D; font-size:30px; width = 90%, text-align: left;")),
                   br(),
                   h4("If you want more information and guidance about the app, visit the 'About' and 'Resources' tabs."),
                   br())),
        actionButton("back5", "Back", icon("chevron-circle-left"))
    )

    observeEvent(input$back6, {showModal(tour_modal_6)})

    # Test banner ####

    shinyjs::onclick("close_banner", shinyjs::hide(id = "welcome_banner", anim = TRUE))


    # Topic tab buttons ####

    observeEvent(input$home_to_demographics, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[1])
    })

    observeEvent(input$home_to_housing, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[2])
    })

    observeEvent(input$home_to_neighbourhoods, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[3])
    })

    observeEvent(input$home_to_economic_activity, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[4])
    })

    observeEvent(input$home_to_finance, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[5])
    })

    observeEvent(input$home_to_internet, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[6])
    })


    observeEvent(input$home_to_physical_activity, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[7])
    })

    observeEvent(input$home_to_local_services, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[8])
    })

    observeEvent(input$home_to_environment, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[9])
    })

    observeEvent(input$home_to_volunteering, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[10])
    })

    observeEvent(input$home_to_culture, {
        updateTabsetPanel(session, "navbar",
                          selected = "surveyTab")
        updateSelectInput(session, inputId = "select_topic", label = "Topic", choices = select_list_topics, selected = select_list_topics[11])
    })


    # Update input$select_question by input$select_topic ####

    observe({

        if (input$navigation_mode == "Browse by topic and question") {

            if (grepl("The Composition and Characteristics of Households in Scotland", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_2, selected = select_list_questions_topic_2[1])

            } else if (grepl("Housing", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_3)

            } else if (grepl("Neighbourhoods and Communities", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_4)

            } else if (grepl("Economic Activity", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_5)

            } else if (grepl("Finance", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_6)

            } else if (grepl("Internet", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_7)

            } else if (grepl("Physical Activity and Sport", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_8)

            } else if (grepl("Local Services", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_9)

            } else if (grepl("Environment", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_10)

            } else if (grepl("Volunteering", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_11)

            } else if (grepl("Culture and Heritage", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_12)

            } else if (grepl("Childcare", input$select_topic, fixed = TRUE)) {

                updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_topic_13)
            }

            updateSelectizeInput(session, 'searchbar', 'Search',
                                 choices = c(select_list_questions_topic_2,
                                             select_list_questions_topic_3,
                                             select_list_questions_topic_4,
                                             select_list_questions_topic_5,
                                             select_list_questions_topic_6,
                                             select_list_questions_topic_7,
                                             select_list_questions_topic_8,
                                             select_list_questions_topic_9,
                                             select_list_questions_topic_10,
                                             select_list_questions_topic_11,
                                             select_list_questions_topic_12),
                                 selected="Search",
                                 options = list(
                                     placeholder = "Type here to find what question you are looking for",
                                     onInitialize = I('function() { this.setValue(""); }')
                                 )
            )
        }
    })


    # Update input$select_topic by input$searchbar ####

    observeEvent(input$searchbar, {

        if (input$navigation_mode == "Search questions") {

            current_topic <- input$select_topic

            topic_number <- question_titles[question_titles$ID == input$searchbar,]$Topic

            topic <- topic_titles[topic_titles$code == paste0("Top", topic_number),]$title

            if (topic != current_topic) {

                topic_update_string <- paste0("updateSelectInput(session, inputId = \"select_topic\", label = \"Topic\", choices = select_list_topics, selected = \"", topic,"\")")

                eval(parse(text = topic_update_string))

            }

            topic_number <- question_titles[question_titles$ID == input$searchbar,]$Topic

            question <- question_titles[question_titles$ID == input$searchbar,]$ID

            question_update_string <- paste0("updateSelectInput(session, inputId = \"select_question\", label = \"Question\", choices = select_list_questions_topic_", topic_number, ", selected = \"", question,"\")")

            print(question_update_string)

            eval(parse(text = question_update_string))
        }
    })

    # Update input$select_excel_question by inpur$select_excel_topic ####

    observe({

        if (grepl("The Composition and Characteristics of Households in Scotland", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_2, selected = select_list_questions_topic_2[1])

        } else if (grepl("Housing", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_3)

        } else if (grepl("Neighbourhoods and Communities", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_4)

        } else if (grepl("Economic Activity", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_5)

        } else if (grepl("Finance", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_6)

        } else if (grepl("Internet", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_7)

        } else if (grepl("Physical Activity and Sport", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_8)

        } else if (grepl("Local Services", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_9)

        } else if (grepl("Environment", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_10)

        } else if (grepl("Volunteering", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_11)

        } else if (grepl("Culture and Heritage", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_12)

        } else if (grepl("Childcare", input$select_excel_topic, fixed = TRUE)) {

            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_topic_13)
        }
    })

    # Update inpu$select_comparison_type and hide input$select_year by type of selected question ####

    observe ({

        if (input$select_question %in% c(type_1_questions, type_4_questions)) {

            updateSelectInput(session, inputId = "select_comparison_type",  label = "Compare by", choices = c("No comparison", "Local Authority/Scotland"))

            shinyjs::showElement("select_local_authority")
            shinyjs::showElement("select_comparison_type")
            shinyjs::hideElement("select_year")
            shinyjs::showElement("select_year_comparator")
            shinyjs::showElement("select_local_authority_comparator")

        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

            updateSelectInput(session, inputId = "select_comparison_type",  label = "Compare by", choices = c("No comparison", "Year", "Local Authority/Scotland"))

            shinyjs::showElement("select_local_authority")
            shinyjs::showElement("select_year")
            shinyjs::showElement("select_comparison_type")
            shinyjs::showElement("select_year_comparator")
            shinyjs::showElement("select_local_authority_comparator")

        } else if (input$select_question %in% type_0_questions) {

            shinyjs::hideElement("select_local_authority")
            shinyjs::hideElement("select_year")
            shinyjs::hideElement("select_comparison_type")
            shinyjs::hideElement("select_year_comparator")
            shinyjs::hideElement("select_local_authority_comparator")

        }
    })

    # Update input$select_year by selected question type ####

    observe ({

        if (!is.null(df())) {

            df <- df()

            if (!input$select_question %in% c(type_1_questions, type_4_questions)) {

                updateSelectInput(session, inputId = "select_year", label = "Year", choices = sort(unique(df$Year), decreasing = TRUE))
            }
        }
    })

    # Update input$select_year_comparator by input$select_year ####

    observe ({

        if (!is.null(df())) {

            selected_year <- input$select_year

            if (!input$select_question %in% c(type_1_questions, type_4_questions)) {

                updateSelectInput(session, inputId = "select_year_comparator", label = "Year",
                                  choices = sort(unique(df()$Year)[!sort(unique(df()$Year)) %in% selected_year], decreasing = TRUE))
            }
        }
    })

    # Update input$select_local_authority_comparator by input$select_local_authority ####

    observe ({

        selected_local_authority <- input$select_local_authority

        updateSelectInput(session, inputId = "select_local_authority_comparator", label = "Local Authority/Scotland", choices = local_authorities[!local_authorities %in% selected_local_authority])
    })

    # IMPORT DATA ####

    # df() ####

    df <- reactive({

        data_file_path <- paste0("data/dataset/", gsub("/", " ", input$select_question), ".Rds")

        if (input$select_question > 0 & !input$select_question %in% type_0_questions) {

            df <- readRDS(data_file_path)

            return(df)

        } else {

            return(NULL)
        }
    })

    # excel_df() ####

    excel_df <- reactive({

        data_file_path <- paste0("data/dataset/", gsub("/", " ", input$select_excel_question), ".Rds")

        if (input$select_excel_question > 0 & !input$select_excel_question %in% type_0_questions) {

            df <- readRDS(data_file_path)

            column_names <- colnames(df)[!grepl("_l", colnames(df)) & !grepl("_u", colnames(df))]

            df <- dplyr::select(df, column_names)

            return(df)

        } else {

            return(NULL)
        }
    })

    # PROCESS DATA ####

    # Assign dynamic variables (column_names_count, measure_column_name, variable_column_names) ####

    column_names_count <- reactive({

        return(length(colnames(df())))
    })

    measure_column_name <- reactive({

        if (input$select_question %in% type_1_questions) {

            measure_column_name <- colnames(df())[2]

        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

            measure_column_name <- colnames(df())[3]
        }

        return(measure_column_name)
    })

    variable_column_names <- reactive({

        if (input$select_question %in% type_1_questions) {

            variable_column_names <- colnames(df())[3:column_names_count()]

        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

            variable_column_names <- colnames(df())[4:column_names_count()]
        }

        return(variable_column_names)
    })

    # comparison_df() ####

    comparison_df <- reactive ({

        if (input$select_question %in% c(type_1_questions, type_4_questions)) {

            df <- df()[df()$Council == input$select_local_authority_comparator,]

        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

            if (input$select_comparison_type == "Year") {

                df <- df()[df()$Year == input$select_year_comparator & df()$Council == input$select_local_authority,]

            } else if (input$select_comparison_type == "Local Authority/Scotland") {

                df <- df()[df()$Year == input$select_year & df()$Council == input$select_local_authority_comparator,]
            }

        } else if (input$select_question %in% c(type_0_questions)) {

            df <- NULL
        }

        if (!input$select_question %in% type_4_questions) {

            df <- eval(parse(text = round_comparison_df_values(variable_column_names())))

            df <- eval(parse(text = rename_comparison_df_columns(colnames(df), measure_column_name())))
        }

        return(df)
    })

    # base_df() ####

    base_df <- reactive({

        if (input$select_question %in% c(type_1_questions, type_4_questions)) {

            base_df <- df()[df()$Council == input$select_local_authority,]

        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

            base_df <- df()[df()$Year == input$select_year & df()$Council == input$select_local_authority,]

        } else {

            base_df <- NULL
        }

        if (input$select_question %in% c(type_1_questions, type_2_questions, type_3_questions)) {

            if (input$select_comparison_type != "No comparison") {

                base_df <- merge(base_df, comparison_df(), by = measure_column_name())

                if (input$select_question %in% c(type_1_questions, type_2_questions)) {

                    base_df <- eval(parse(text = statistical_significance(variable_column_names())))

                    eval(parse(text = remove_significance_from_rows(measure_column_name())))

                } else if (input$select_question %in% type_3_questions) {

                    stat_sig_columns <- colnames(base_df)[4:column_names_count()]

                    stat_sig_columns <- stat_sig_columns[stat_sig_columns != "All" & stat_sig_columns != "Base"]

                    base_df <- eval(parse(text = statistical_significance(stat_sig_columns)))

                    eval(parse(text = remove_significance_from_rows(measure_column_name())))
                }
            }
        }

        return(base_df)
    })

    # main_df() ####

    main_df <- reactive({

        comparison_type <- input$select_comparison_type

        if (!input$select_question %in% c(type_4_questions, type_0_questions)) {

            if (comparison_type == "No comparison") {

                main_df <- eval(parse(text = arrange_select_mutate(variable_column_names(), measure_column_name())))

            } else if (comparison_type != "No comparison") {

                main_df <- eval(parse(text = arrange_select_mutate_comparison(variable_column_names(), measure_column_name())))
            }

        } else {

            main_df <- base_df()
        }

        return(main_df)
    })

    # main_chart_df() ####

    main_chart_df <- reactive ({

        if (!input$select_question %in% type_0_questions) {

            main_chart_df <- base_df()

            main_chart_df <- main_chart_df[main_chart_df[1] != "All" & main_chart_df[1] != "Base",]

            variable_column_names <- variable_column_names()[!grepl("_l", variable_column_names()) & !grepl("_u", variable_column_names()) & !variable_column_names() %in% c(measure_column_name(), "Year", "Council", "All", "Base")]

            main_chart_df <- suppressWarnings(eval(parse(text = chart_data_processing(variable_column_names, measure_column_name(), "main_chart_df"))))

        } else {

            main_chart_df <- NULL
        }
        return(main_chart_df)
    })

    # comparison_chart_df() ####

    comparison_chart_df <- reactive ({

        if (input$select_comparison_type != "No comparison") {

            if (input$select_question %in% type_1_questions) {

                comparison_chart_df <- comparison_df()[-1]

            } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

                comparison_chart_df <- comparison_df()[-c(1, 2)]
            }

            column_names <- colnames(comparison_chart_df)

            new_names <- c()

            for (column_name in column_names) {

                if (substr(column_name, nchar(column_name), nchar(column_name)) == "2") {
                    column_name <- substr(column_name, 1, nchar(column_name) - 1)
                    new_names <- c(new_names, column_name)
                } else {
                    new_names <- c(new_names, column_name)
                }
            }

            colnames(comparison_chart_df) <- new_names

            comparison_chart_df <- comparison_chart_df[comparison_chart_df[1] != "All" & comparison_chart_df[1] != "Base",]

            variable_column_names <- variable_column_names()[!grepl("_l", variable_column_names()) & !grepl("_u", variable_column_names()) & !variable_column_names() %in% c(measure_column_name(), "Year", "Council", "All", "Base")]

            comparison_chart_df <- suppressWarnings(eval(parse(text = chart_data_processing(variable_column_names, measure_column_name(), "comparison_chart_df"))))

        } else {

            comparison_chart_df <- NULL
        }

        return(comparison_chart_df)
    })

    # OUTPUTS ####

    # TEXT ####

    # output$question_type ####

    output$question_type <- renderText({

        if (input$select_question %in% type_0_questions) {

            "0"

        } else if (input$select_question %in% type_1_questions) {

            "1"

        } else if (input$select_question %in% type_2_questions) {

            "2"

        } else if (input$select_question %in% type_3_questions) {

            "3"

        } else if (input$select_question %in% type_4_questions) {

            "4"

        } else {

            NULL
        }

    })

    outputOptions(output, "question_type", suspendWhenHidden = FALSE)

    # output$statistical_significance_key ####

    output$statistical_significance_key <- renderText ({

        if (input$select_comparison_type != "No comparison" & !input$select_question %in% type_0_questions) {

            if (input$select_comparison_type == "Year") {

                statistical_significance_key <- paste0("<font color=\"#00A3A3\">&#9646;</font> Significantly greater than ", input$select_local_authority, " (", input$select_year_comparator, ") | <font color=\"#C3C3FF\">&#9646;</font> Significantly lower than ", input$select_local_authority, " (", input$select_year_comparator, ")")

            } else if (input$select_comparison_type == "Local Authority/Scotland") {

                if (input$select_comparison_type == "Local Authority/Scotland") {

                    if (question_titles[question_titles$ID == input$select_question,]$Type != "1") {

                        statistical_significance_key <- paste0("<font color=\"#00A3A3\">&#9646;</font> Significantly greater than ", input$select_local_authority_comparator, " (", input$select_year, ") | <font color=\"#C3C3FF\">&#9646;</font> Significantly lower than ", input$select_local_authority_comparator, " (", input$select_year, ")")

                    } else {

                        statistical_significance_key <- paste0("<font color=\"#00A3A3\">&#9646;</font> Significantly greater than ", input$select_local_authority_comparator, " | <font color=\"#C3C3FF\">&#9646;</font> Significantly lower than ", input$select_local_authority_comparator)
                    }
                }
            }
        }
    })

    # main_table_type_comment ####
    output$main_table_type_comment <- renderText({

        coverage <- question_titles[question_titles$ID == input$select_question,]$Coverage

        if (input$select_question %in% type_0_questions) {

            paste0("Base numbers at local authority level are too small to produce robust analysis.")

        } else if (input$select_question %in% type_1_questions) {

            paste0("Column percentages, ", coverage)

        } else if (input$select_question %in% type_2_questions) {

            paste0("Column percentages, ", coverage)

        } else if (input$select_question %in% type_3_questions) {

            paste0("Row percentages, ", input$select_year, " data, ", coverage)

        } else if (input$select_question %in% type_4_questions) {

            paste0("Grossed-up estimates (Rounded to the nearest 10,000)")
        }

    })

    # comparison_table_type_comment ####
    output$comparison_table_type_comment <- renderText({

        coverage <- question_titles[question_titles$ID == input$select_question,]$Coverage

        if (input$select_question %in% type_0_questions) {

            NULL

        } else if (input$select_question %in% type_1_questions & input$select_comparison_type != "No comparison") {

            paste0("Column percentages, ", coverage)

        } else if (input$select_question %in% type_2_questions & input$select_comparison_type != "No comparison") {

            if (input$select_comparison_type == "Year") {

                paste0("Column percentages, ", input$select_year_comparator, " data, ", coverage)

            } else if (input$select_comparison_type == "Local Authority/Scotland") {

                paste0("Column percentages, ", input$select_year, " data, ", coverage)

            } else {

                NULL

            }

        } else if (input$select_question %in% type_3_questions) {

            if (input$select_comparison_type == "Year") {

                paste0("Row percentages, ", input$select_year_comparator, " data, ", coverage)

            } else if (input$select_comparison_type == "Local Authority/Scotland") {

                paste0("Row percentages, ", input$select_year, " data, ", coverage)

            } else {

                NULL
            }

        } else if (input$select_question %in% type_4_questions) {

            if (input$select_comparison_type == "Local Authority/Scotland") {

                paste0("Grossed-up estimates (Rounded to the nearest 10,000)")

            } else {

                NULL
            }
        }
    })

    # main_chart_type_comment
    output$main_chart_type_comment <- renderText({

        coverage <- question_titles[question_titles$ID == input$select_question,]$Coverage

        if (input$select_question %in% type_0_questions) {

            paste0("Base numbers at local authority level are too small to produce robust analysis.")}
    })

    # main_title ####

    output$main_title <- renderText({

        if (input$select_question %in% c(type_1_questions, type_4_questions)) {

            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ")")

        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ", ", input$select_year, ")")

        } else {

            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title)
        }
    })

    # main_plot_title ####

    output$main_plot_title <- renderText({

        if (input$select_question %in% c(type_1_questions, type_4_questions)) {

            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ")")

        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ", ", input$select_year, ")")

        } else {

            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title)
        }
    })

    # comparison_title ####

    output$comparison_title <- renderText({

        if (!input$select_question %in% type_0_questions) {

            if (input$select_question %in% c(type_1_questions, type_4_questions) & input$select_comparison_type == "Local Authority/Scotland") {

                paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority_comparator, ")")

            } else {

                if (input$select_comparison_type == "Year") {

                    paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ", ", input$select_year_comparator, ")")

                } else if (input$select_comparison_type == "Local Authority/Scotland") {

                    paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority_comparator, ", ", input$select_year, ")")
                }
            }
        }
    })

    # comparison_plot_title ####
    output$comparison_plot_title <- renderText({

        if (!input$select_question %in% type_0_questions) {

            if (input$select_question %in% type_1_questions & input$select_comparison_type == "Local Authority/Scotland") {

                paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority_comparator, ")")

            } else {

                if (input$select_comparison_type == "Year") {

                    paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ", ", input$select_year_comparator, ")")

                } else if (input$select_comparison_type == "Local Authority/Scotland") {

                    paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority_comparator, ", ", input$select_year, ")")
                }
            }
        }
    })

    # comment ####
    output$comment <- renderText({

        comment <- question_titles[question_titles$ID == input$select_question,]$Comment

        if (!is.na(comment)) {

            return(comment)

        } else {

            return(NULL)
        }
    })

    # link ####
    output$link <- renderText({

        link <- question_titles[question_titles$ID == input$select_question,]$Link

        if (!is.na(link)) {

            return(link)

        } else {

            return(NULL)
        }
    })

    # TABLES ####
    # output$main_table ####
    output$main_table <- renderDataTable({

        if (input$select_question %in% c(type_1_questions, type_2_questions, type_3_questions)) {

            if (input$select_comparison_type != "No comparison") {

                hide_columns <- grep("_sig", colnames(main_df()))
                start_of_hide <- hide_columns[1]
                end_of_hide <- hide_columns[length(hide_columns)]
                hide_columns <- paste0(start_of_hide, ":", end_of_hide)

                variable_column_names <- colnames(main_df())[3:start_of_hide - 1]

                data_table <- eval(parse(text = main_df_comparison_output(variable_column_names, hide_columns)))

            } else {

                data_table <- DT::datatable(main_df(),

                                            # extensions = "Buttons",
                                            options = list(
                                                # buttons = c("copy", "csv", "excel"),
                                                dom = "t",
                                                digits = 1,
                                                na = "-",
                                                paging = FALSE,
                                                ordering = FALSE,
                                                info = FALSE,
                                                searching = FALSE,
                                                columnDefs = list(list(targets = c(0),
                                                                       visible = FALSE))))

            }

        } else if (input$select_question %in% type_4_questions) {

            data_table <- DT::datatable(main_df(),
                                        # extensions = "Buttons",
                                        options = list(
                                            # buttons = c("copy", "csv", "excel"),
                                            dom = "t",
                                            digits = 1,
                                            na = "-",
                                            paging = FALSE,
                                            ordering = FALSE,
                                            info = FALSE,
                                            searching = FALSE,
                                            columnDefs = list(list(targets = c(0:1),
                                                                   visible = FALSE))))

        } else if (input$select_question %in% type_0_questions){

            data_table <- NULL
        }

        return(data_table)
    })

    # comparison_table ####
    output$comparison_table <- renderDataTable({

        if (!input$select_question %in% c(type_0_questions, type_4_questions)) {

            if(input$select_comparison_type != "No comparison") {

                hide_columns <- grep("_l|_u", colnames(comparison_df()))
                start_of_hide <- hide_columns[1]
                end_of_hide <- hide_columns[length(hide_columns)]
                hide_columns <- paste0(start_of_hide, ":", end_of_hide)

                if (input$select_question %in% type_1_questions) {

                    measure_column_name <- colnames(comparison_df())[2]
                    variable_column_names <- colnames(comparison_df())[4:start_of_hide - 1]
                    variable_column_names <- substr(variable_column_names,1,nchar(variable_column_names)-1)

                    data_table <- eval(parse(text = comparison_df_output(measure_column_name, variable_column_names, hide_columns, "1")))

                } else if (input$select_question %in% type_2_questions | input$select_question %in% type_3_questions) {

                    measure_column_name <- colnames(comparison_df())[3]
                    variable_column_names <- colnames(comparison_df())[5:start_of_hide - 1]
                    variable_column_names <- substr(variable_column_names,1,nchar(variable_column_names)-1)
                    data_table <- eval(parse(text = comparison_df_output(measure_column_name, variable_column_names, hide_columns, "2")))
                }

                return(data_table)

            } else {

                NULL
            }

        } else if (input$select_question %in% type_4_questions & input$select_comparison_type != "No comparison"){

            DT::datatable(comparison_df(),
                          # extensions = "Buttons",
                          options = list(
                              # buttons = c("copy", "csv", "excel"),
                              dom = "t",
                              digits = 1,
                              na = "-",
                              paging = FALSE,
                              ordering = FALSE,
                              info = FALSE,
                              searching = FALSE,
                              columnDefs = list(list(targets = c(0:1),
                                                     visible = FALSE))))

        } else {

            NULL
        }
    })

    # Download main table
    output$download_table <- downloadHandler(
        filename = function() {
            paste("shs_data.csv")
        },
        content = function(file) {
            write.csv(main_df(), file, row.names = FALSE)
        })

    # Download comparison table
    output$download_comparison_table <- downloadHandler(
        filename = function() {
            paste("shs_data.csv")
        },
        content = function(file) {
            write.csv(comparison_df(), file, row.names = FALSE)
        })


    # excel_table ####
    output$excel_table <- DT::renderDataTable({

        excel_datatable <- DT::datatable(excel_df(),

                                         extensions = "Buttons",
                                         options = list(

                                             buttons = c("copy", "csv", "excel"),
                                             dom = "Bftpl",
                                             columnDefs = list(list(targets = c(0), visible = FALSE)),
                                             pageLength = 25,
                                             lengthMenu = list(c(10, 25, 50, 100, 200, -1), list('10', '25', '50', '100', '200', 'All')),
                                             paging = TRUE
                                         ),
                                         class = "display",
                                         filter = 'top'
        )

        return(excel_datatable)
    })

    # CHARTS ####

    # output$main_chart ####

    output$main_chart <- plotly::renderPlotly({

        if (!input$select_question %in% c(type_0_questions, type_4_questions)) {

            df <- main_chart_df()

            df_string <- paste0("df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",]")

            df <- eval(parse(text = df_string))

            gather_key <- colnames(df[2])

            if (input$select_question %in% type_1_questions) {

                line_chart_string <- paste0("ggplot(data = df, mapping = aes(x = `", gather_key,"`, y = Percent, group = `", measure_column_name(), "`, colour = `", measure_column_name(), "`)) +
                                        geom_line(size = 1, aes(text = paste(\"Value: \", Percent, \"%\", \"\n\",
                                                                         \"Lower Confidence Limit: \", df$LowerConfidenceLimit, \"%\", \"\n\",
                                                                         \"Upper Confidence Limit: \", df$UpperConfidenceLimit, \"%\", \"\n\",
                                                                         measure_column_name(), \": \",`", measure_column_name(), "`,\"\n\",
                                                                         gather_key, \": \",", gather_key,"))) +
                                        theme(panel.grid.minor = element_blank(),
                                              panel.background = element_rect(\"transparent\"),
                                              panel.grid.major.y = element_line(colour = \"#b8b8ba\", size = 0.3),
                                              text = element_text(family = \"Arial\")) +
                                              scale_colour_manual(values = shs_colours) +
                                              labs(title = input$question, x = \"Year\")")

                chart <- eval(parse(text = line_chart_string))

            } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

                bar_chart_string <- paste0("ggplot(data = df, mapping = aes(x = `", gather_key, "`, y = `Percent`, fill = `", measure_column_name(), "`, text = paste(\"Value: \", Percent, \"%\", \"\n\",
                                        \"Lower Confidence Limit: \", df$LowerConfidenceLimit, \"%\", \"\n\",
                                        \"Upper Confidence Limit: \", df$UpperConfidenceLimit, \"%\", \"\n\",
                                        \"Group: \",", gather_key,"))) +

                                       geom_bar(position = \"dodge\", stat = \"identity\") +
                                       theme(panel.grid.minor = element_blank(),
                                       panel.grid.major.x = element_blank(),
                                       panel.grid.major.y = element_line(colour = \"#b8b8ba\", size = 0.3),
                                       panel.background = element_rect(\"transparent\"),
                                       legend.position = \"bottom\") +

                                     scale_fill_manual(values = shs_colours) +
                                       labs(title = input$question, x = NULL)")

                chart <- eval(parse(text = bar_chart_string))

            }

            else {

                chart <- NULL
            }

            if(input$ConfidenceInterval == TRUE & input$select_question %in% c(type_2_questions, type_3_questions)) {

                chart <- chart + geom_errorbar(aes(ymin = df$LowerConfidenceLimit,
                                                   ymax = df$UpperConfidenceLimit

                ),
                width = 0.4,
                position = position_dodge(width = 0.9))
            }

            else if (input$ConfidenceInterval == TRUE & input$select_question %in% type_1_questions) {

                confidence_intervals_string <- paste0("chart + geom_errorbar(aes(ymin = df$LowerConfidenceLimit,
                                               ymax = df$UpperConfidenceLimit,
                                               text = paste(\"Value: \", Percent, \"%\", \"\n\",
                                        \"Lower Confidence Limit: \", df$LowerConfidenceLimit, \"%\", \"\n\",
                                        \"Upper Confidence Limit: \", df$UpperConfidenceLimit, \"%\", \"\n\",
                                        \"Group: \",", gather_key,")),
            width = 0.3)")


                chart <- eval(parse(text=confidence_intervals_string))
            }


            else {

                chart
            }

            if(input$zoomLevel_main == "Full scale") {
                chart <- chart + ylim(0,100)
            }

            chart <- ggplotly(tooltip = "text") %>%
                config(displaylogo = FALSE,
                       displayModeBar = TRUE,
                       modeBarButtonsToRemove = list("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))

        }
    })

    # output$comparison_chart ####

    output$comparison_chart <- plotly::renderPlotly({

        if (input$select_comparison_type != "No comparison" & !input$select_question %in% type_0_questions) {

            df <- comparison_chart_df()

            df_string <- paste0("df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",]")

            df <- eval(parse(text = df_string))

            gather_key <- colnames(df[2])

            if (input$select_question %in% type_1_questions) {

                line_chart_string <- paste0("ggplot(data = df, mapping = aes(x = `", gather_key,"`, y = Percent, group = `", measure_column_name(), "`, colour = `", measure_column_name(), "`)) +
                                        geom_line(size = 1, aes(text = paste(\"Value: \", Percent, \"%\", \"\n\",
                                                                         \"Lower Confidence Limit: \", df$LowerConfidenceLimit, \"%\", \"\n\",
                                                                         \"Upper Confidence Limit: \", df$UpperConfidenceLimit, \"%\", \"\n\",
                                                                         measure_column_name(), \": \",`", measure_column_name(), "`,\"\n\",
                                                                         gather_key, \": \",", gather_key,"))) +
                                        theme(panel.grid.minor = element_blank(),
                                              panel.background = element_rect(\"transparent\"),
                                              panel.grid.major.y = element_line(colour = \"#b8b8ba\", size = 0.3),
                                              text = element_text(family = \"Arial\")) +
                                              scale_colour_manual(values = shs_colours) +
                                              labs(title = input$question, x = \"Year\")")

                chart <- eval(parse(text = line_chart_string))

            } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {

                bar_chart_string <- paste0("ggplot(data = df, mapping = aes(x = `", gather_key, "`, y = `Percent`, fill = `", measure_column_name(), "`, , text = paste(\"Value: \", Percent, \"%\", \"\n\",
                                                                         \"Lower Confidence Limit: \", df$LowerConfidenceLimit, \"%\", \"\n\",
                                                                         \"Upper Confidence Limit: \", df$UpperConfidenceLimit, \"%\", \"\n\",

                                                                        \"Group: \",", gather_key,"
))) +
                                           geom_bar(position = \"dodge\", stat = \"identity\") +
                                           theme(panel.grid.minor = element_blank(),
                                           panel.grid.major.x = element_blank(),
                                           panel.grid.major.y = element_line(colour = \"#b8b8ba\", size = 0.3),
                                           panel.background = element_rect(\"transparent\"),
                                           legend.position = \"bottom\") +
                                           scale_fill_manual(values = shs_colours) +
                                           labs(title = input$question, x = NULL)")

                chart <- eval(parse(text = bar_chart_string))

            } else if (input$select_question %in% c(type_4_questions)) {

                bar_chart_string <- paste0("ggplot(data = df, mapping = aes(x = `", gather_key, "`, y = `", measure_column_name(), "`, fill = `Percent`)) + geom_bar(position = \"dodge\", stat = \"identity\")")

                chart <- eval(parse(text = bar_chart_string))

            }

        } else {

            chart <- NULL
        }

        if(input$compareConfidenceInterval == TRUE & input$select_question %in% c(type_2_questions, type_3_questions)) {

            chart <- chart + geom_errorbar(aes(ymin = df$LowerConfidenceLimit,
                                               ymax = df$UpperConfidenceLimit

            ),
            width = 0.4,
            position = position_dodge(width = 0.9))
        }

        else if (input$compareConfidenceInterval == TRUE & input$select_question %in% type_1_questions) {

            confidence_intervals_string <- paste0("chart + geom_errorbar(aes(ymin = df$LowerConfidenceLimit,
                                               ymax = df$UpperConfidenceLimit,
                                               text = paste(\"Value: \", Percent, \"%\", \"\n\",
                                        \"Lower Confidence Limit: \", df$LowerConfidenceLimit, \"%\", \"\n\",
                                        \"Upper Confidence Limit: \", df$UpperConfidenceLimit, \"%\", \"\n\",
                                        \"Year: \",", gather_key,")),
            width = 0.3)")


            chart <- eval(parse(text=confidence_intervals_string))
        }


        else {

            chart
        }

        if(input$zoomLevel_comparator == "Full scale") {
            chart <- chart + ylim(0,100)
        }

        #Removes tooltip duplicates and plotly modebar options
        ggplotly(tooltip = "text") %>%
            config(displaylogo = FALSE,
                   displayModeBar = TRUE,
                   modeBarButtonsToRemove = list("zoom2d", "pan2d", "select2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))

    })

    # Chart help modal ####

    chartModal <- modalDialog(
        size = "l",
        br(),
        p(tags$h4("How go use the SHS charts", style = "text-align: center"),
          tags$br(),
          p(img(src = "modal_chart.png", height = "100%", width = "100%"), style = "text-align: center"),
          tags$b("Whenever possible, choose full-scale Y-axis when comparing charts. Different zoomed in scales can potentially create misleading data visualisation and comparison."),
          easyClose = TRUE, fade = FALSE, footer = NULL))


    observeEvent(input$help, {
        showModal(chartModal)
    })

    # Table help modal ####

    tableModal <- modalDialog(
        size = "l",
        br(),
        p(tags$h4("How go use the SHS tables", style = "text-align: center"),
          tags$br(),
          p(img(src = "modal_table2.png", height = "100%", width = "100%"), style = "text-align: center"),
          p(img(src = "sign_table.png", height = "100%", width = "100%"), style = "text-align: center"),
          tags$b("Statistical significance is indicated through different coloured cells. Dark green cells indicate a statistically significantly different value to the corresponding comparator table, where light purple cells indicate that the value is statistically significantly lower."),
          easyClose = TRUE, fade = FALSE, footer = NULL))

    observeEvent(input$helpTable, {
        showModal(tableModal)
    })

    # output$report ####

    report <- reactiveValues(filepath = NULL)

    observeEvent(input$generate, {

        id <- topic_titles[topic_titles$title == input$select_report_topic,]$code

        tmp_dir <- tempdir()

        tmp_report <- paste0(tmp_dir, "/", id, ".pdf")

        # print(tmp_report)

        # file.copy(c("reports/topic_2.Rmd", "www/SG_master_logo_RGB.jpg"), tmp_dir, overwrite = TRUE)

        progress <- shiny::Progress$new()

        on.exit(progress$close())

        progress$set(message = "Gathering data and building report.",
                     detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 1)

        if (input$select_report_comparison_type == "Local Authority") {

            comparator <- input$select_report_local_authority_comparator

        } else if (input$select_report_comparison_type == "Year") {

            comparator <- input$select_report_year_comparator

        } else {

            comparator <- NULL
        }

        topic <-  topic_titles[topic_titles$title == input$select_report_topic, ]$code
        topic <- gsub("Top", "", topic)

        data <- report_data_processing(topic = topic,
                                       local_authority =  input$select_report_local_authority,
                                       year =  input$select_report_year,
                                       comparison_type = input$select_report_comparison_type,
                                       comparator = comparator)

        report_title_value <- paste0("Scotland's People Local Authority Tables | ",
                                     topic_titles[topic_titles$title == input$select_report_topic, ]$title, " | ",
                                     input$select_report_local_authority, " (",
                                     input$select_report_year, ")")

        if (input$select_report_comparison_type == "Local Authority") {

            report_title_value <- paste0(report_title_value, " compared to ", comparator, " (", input$select_report_year, ")")

            } else if (input$select_report_comparison_type == "Year") {

                report_title_value <- paste0(report_title_value, " compared to ", input$select_report_local_authority, " (", comparator, ")")

            }

        params <- list(report_title = report_title_value,
                       local_authority = input$select_report_local_authority,
                       year = input$select_report_year,
                       topic_data = data,
                       comparison_type = input$select_report_comparison_type,
                       comparator = comparator)

        rmarkdown::render(paste0("reports/", id, ".Rmd"),
                          output_format = "pdf_document",
                          output_file = tmp_report,
                          params = params,
                          envir = new.env()
        )

        detach("package:kableExtra", unload = TRUE)

        report$filepath <- tmp_report
    })

    output$reportbuilt <- reactive({

        return(!is.null(report$filepath))
    })

    outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)

    output$download <- downloadHandler(

        filename = function() {
            paste0(input$select_topic, "_", Sys.Date(), ".pdf") %>%
                gsub(" ", "_", .)
        },

        content = function(file) {

            file.copy(report$filepath, file)
        }
    )
}

# app ####

shinyApp(ui = ui, server = server)
