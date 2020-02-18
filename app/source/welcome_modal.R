welcome_modal <- modalDialog(

  p(img(src = "modal-into.png", height = "100%", width = "100%"), style = "text-align: center"),
  br(),
  tags$div(h4("This interactive app provides information about Scottish homes, neighbourhoods, and their views on various aspects of society."),
           style = "color: 0E3E5D; font-size:20px; text-align: center"),
  br(),
  h4("If this is your first time using the app, take a tour to learn more about how to get the most out of it."),
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
  h4("This app allows your to look at trends and  "),

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
    h5("The SHS data is able to make generalisations about the Scottish population as a whole by only interviewing a random sample of all the Scottish households. However, this means that figures are estimates rather than precise percentages, and come with a degree of error. In fact, the 'true' value might be higher or lower than the estimate. This range is called the 'confidence interval'"),
    h5("For time or local authority comparisons, the tables and charts indicate whether any difference is statistically significant or not."),
    tags$li("Tables: Dark green colour indicates a value significantly greater. Light purple colour indicates a value significantly lower. Cells with no colour indicate no statistical difference."),
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
           h4("You can modify and download the data you need. In the 'Raw Data' tab you can view the raw data used to produce the tables and charts. Either copy or download all the data for a specific question or you use the filter function to retrieve the raw data for a specific criteria."),
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
