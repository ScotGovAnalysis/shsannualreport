# libraries ####
library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(magrittr)
library(ggplot2)

# sources ####
source("source/variables.R")$values
source("source/functions.R")$values

# ui ####
ui <- fluidPage(
    
    useShinyjs(),
    
    theme = "styles.css",
    
    navbarPage(title="SHS Annual Report",
               
               tabPanel("Home",
                        fluidRow(
                            column(8, offset = 2, includeMarkdown("home.Rmd"))
                        )
               ),
               
               tabPanel("Survey Results",
                        
                        fluidRow(
                            column(5, selectInput("select_chapter", label = "Chapter", choices = select_list_chapters, width = "100%")),
                            column(7, selectInput("select_question", label = "Question", choices = c(), width = "100%"))
                        ),
                        
                        fluidRow(
                            column(3, selectInput("select_local_authority", label = "Local Authority", choices = local_authorities, selected = "Scotland", width = "100%")),
                            column(3, selectInput("select_year", label = "Year", choices = c(), width = "100%")),
                            column(3, selectInput("select_comparison_type", label = "Compare by", choices = c("No comparison", "Year", "Local Authority"), selected = "No comparison", width = "100%")),
                            column(3, conditionalPanel(condition = "input.select_comparison_type == 'Year'", selectInput("select_year_comparator", label = "Comparator", choices = c(), width = "100%"))),
                            column(3, conditionalPanel(condition = "input.select_comparison_type == 'Local Authority'",selectInput("select_local_authority_comparator", label = "Comparator", choices = c(), width = "100%")))
                        ),
                        
                        fluidRow(h3(textOutput("main_title"))),
                        
                        fluidRow(h4(textOutput("main_table_type_comment"))),
                        
                        fluidRow(h5(textOutput("comment"))),
                        
                        fluidRow(h5(htmlOutput("link"))),
                        
                        fluidRow(dataTableOutput("main_table")),
                        
                        fluidRow(htmlOutput("statistical_significance_key")),
                        
                        fluidRow(h3(textOutput("comparison_title"))),
                        
                        fluidRow(h4(textOutput("comparison_table_type_comment"))),
                        
                        fluidRow(dataTableOutput("comparison_table")),
                        
                        fluidRow(plotly::plotlyOutput("main_chart")),
                        
                        fluidRow(plotly::plotlyOutput("comparison_chart"))
               ),
               
               tabPanel("Download PDF",
                        
                        selectInput("select_chapter", label = "Chapter", choices = select_list_chapters, width = "100%"),
                        selectInput("select_report_local_authority", "Select Local Authority", choices = local_authorities),
                        selectInput("select_report_year", "Select Year", choices = c("2018", "2017", "2016", "2015", "2014", "2013")),
                        selectInput("select_report_comparison_type", label = "Compare by", choices = c("No comparison", "Year", "Local Authority"), selected = "No comparison", width = "100%"),
                        conditionalPanel(condition = "input.select_report_comparison_type == 'Year'", selectInput("select_report_year_comparator", label = "Comparator", choices = c("2018", "2017", "2016", "2015", "2014", "2013"), width = "100%")),
                        conditionalPanel(condition = "input.select_report_comparison_type == 'Local Authority'",selectInput("select_report_local_authority_comparator", label = "Comparator", choices = c(local_authorities), width = "100%")),
                        downloadButton("report", "Generate report")
                        ),
               
               tabPanel("Download CSV",
                        
                        fluidRow(
                            column(5, selectInput("select_excel_chapter", label = "Chapter", choices = select_list_chapters, width = "100%")),
                            column(7, selectInput("select_excel_question", label = "Question", choices = c(), width = "100%"))
                        ),
                        
                        fluidRow(dataTableOutput("excel_table"))
               )
    )
)

# server ####
server <- function(input, output, session) {
    
    # UPDATE SELECT INPUTS ####  
    # Update select_question by selected chapter ####
    observe({
        
        if (grepl("The Composition and Characteristics of Households in Scotland", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_2, selected = select_list_questions_chapter_2[1])
            
        } else if (grepl("Housing", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_3)
            
        } else if (grepl("Neighbourhoods and Communities", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_4)
            
        } else if (grepl("Economic Activity", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_5)
            
        } else if (grepl("Finance", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_6)
            
        } else if (grepl("Internet", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_7)
            
        } else if (grepl("Physical Activity and Sport", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_8)
            
        } else if (grepl("Local Services", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_9)
            
        } else if (grepl("Environment", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_10)
            
        } else if (grepl("Volunteering", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_11)
            
        } else if (grepl("Culture and Heritage", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_12)
            
        } else if (grepl("Childcare", input$select_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_question", label = "Question", choices = select_list_questions_chapter_13)
        }
    })
    
    # Update select_excel_question by selected chapter ####
    observe({
        
        if (grepl("The Composition and Characteristics of Households in Scotland", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_2, selected = select_list_questions_chapter_2[1])
            
        } else if (grepl("Housing", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_3)
            
        } else if (grepl("Neighbourhoods and Communities", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_4)
            
        } else if (grepl("Economic Activity", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_5)
            
        } else if (grepl("Finance", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_6)
            
        } else if (grepl("Internet", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_7)
            
        } else if (grepl("Physical Activity and Sport", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_8)
            
        } else if (grepl("Local Services", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_9)
            
        } else if (grepl("Environment", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_10)
            
        } else if (grepl("Volunteering", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_11)
            
        } else if (grepl("Culture and Heritage", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_12)
            
        } else if (grepl("Childcare", input$select_excel_chapter, fixed = TRUE)) {
            
            updateSelectInput(session, inputId = "select_excel_question", label = "Question", choices = select_list_questions_chapter_13)
        }
    })
    
    # Update select_comparison_type and hide select_year by type of selected question ####
    observe ({
        
        if (input$select_question %in% type_1_questions) {
            
            updateSelectInput(session, inputId = "select_comparison_type",  label = "Compare by", choices = c("No comparison", "Local Authority"))
            
            shinyjs::showElement("select_local_authority")
            shinyjs::hideElement("select_year")
            shinyjs::showElement("select_comparison_type")
            
        } else if (input$select_question %in% c(type_2_questions, type_3_questions, type_4_questions)) {
            
            updateSelectInput(session, inputId = "select_comparison_type",  label = "Compare by", choices = c("No comparison", "Year", "Local Authority"))
            
            shinyjs::showElement("select_local_authority")
            shinyjs::showElement("select_year")
            shinyjs::showElement("select_comparison_type")
            
        } else if (input$select_question %in% type_0_questions) {
            shinyjs::hideElement("select_local_authority")
            shinyjs::hideElement("select_year")
            shinyjs::hideElement("select_comparison_type")
            
        }
    })
    
    # Update select_year by selected question ####
    
    observe ({
        
        if (!is.null(df())) {
            
            df <- df()
            
            if (!input$select_question %in% type_1_questions) {
                
                updateSelectInput(session, inputId = "select_year", label = "Year", choices = sort(unique(df$Year), decreasing = TRUE))
            }
        }
    })
    
    # Update select_year_comparator by selected year ####
    
    observe ({
        
        if (!is.null(df())) {
            
            selected_year <- input$select_year
            
            if (!input$select_question %in% type_1_questions) {
                
                updateSelectInput(session, inputId = "select_year_comparator", label = "Year", 
                                  choices = sort(unique(df()$Year)[!sort(unique(df()$Year)) %in% selected_year], decreasing = TRUE))
            }
        }
    })
    
    # Update select local_authority_comparator by selected local authority ####
    
    observe ({
        
        selected_local_authority <- input$select_local_authority
        
        updateSelectInput(session, inputId = "select_local_authority_comparator", label = "Local Authority", choices = local_authorities[!local_authorities %in% selected_local_authority])
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
        
        if (input$select_question %in% type_1_questions) {
            
            df <- df()[df()$Council == input$select_local_authority_comparator,]
            
        } else if (input$select_question %in% c(type_2_questions, type_3_questions, type_4_questions)) {
            
            if (input$select_comparison_type == "Year") {
                
                df <- df()[df()$Year == input$select_year_comparator & df()$Council == input$select_local_authority,]
                
            } else if (input$select_comparison_type == "Local Authority") {
                
                df <- df()[df()$Year == input$select_year & df()$Council == input$select_local_authority_comparator,]
            }
            
        } else if (input$select_question %in% c(type_0_questions)) {
            
            NULL
        }
        
        if (!input$select_question %in% type_4_questions) {
            
            df <- eval(parse(text = round_comparison_df_values(variable_column_names())))
            
            df <- eval(parse(text = rename_comparison_df_columns(colnames(df), measure_column_name())))
        }

        return(df)
    })
    
    # base_df() ####
    base_df <- reactive({
        
        if (input$select_question %in% type_1_questions) {
            
            base_df <- df()[df()$Council == input$select_local_authority,]
            
        } else if (input$select_question %in% c(type_2_questions, type_3_questions, type_4_questions)) {
            
            base_df <- df()[df()$Year == input$select_year & df()$Council == input$select_local_authority,]
            
        } else {
            
            NULL
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
        
        if (!input$select_question %in% c(type_4_questions, type_0_questions)) {
        
        main_chart_df <- base_df()
        
        saveRDS(main_chart_df, "test.Rds")
        
        # main_chart_df <- main_chart_df[main_chart_df[1] != "All" & main_chart_df[1] != "Base",] 
        
        variable_column_names <- variable_column_names()[!grepl("_l", variable_column_names()) & !grepl("_u", variable_column_names()) & !variable_column_names() %in% c(measure_column_name(), "Year", "Council", "All", "Base")]
        
        main_chart_df <- suppressWarnings(eval(parse(text = chart_data_processing(variable_column_names, measure_column_name(), "main_chart_df"))))
        
        return(main_chart_df)
        
        }
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
    # statistical_significance_key ####
    output$statistical_significance_key <- renderText ({
        
        if (input$select_comparison_type != "No comparison") {
            
            if (input$select_comparison_type == "Year") {
                
                statistical_significance_key <- paste0("<font color=\"#00A3A3\">&#9646;</font> Significantly greater than ", input$select_local_authority, " (", input$select_year_comparator, ") | <font color=\"#C3C3FF\">&#9646;</font> Significantly lower than ", input$select_local_authority, " (", input$select_year_comparator, ")")
                
            } else if (input$select_comparison_type == "Local Authority") {
                
                if (input$select_comparison_type == "Local Authority") {
                    
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
            
        } else if (input$select_question %in% type_1_questions & !is.null(input$select_comparison_type)) {
            
            paste0("Column percentages, ", coverage)
            
        } else if (input$select_question %in% type_2_questions) {
            
            if (input$select_comparison_type == "Year") {
                
                paste0("Column percentages, ", input$select_year_comparator, " data, ", coverage)
                
            } else if (input$select_comparison_type == "Local Authority") {
                
                paste0("Column percentages, ", input$select_year, " data, ", coverage)
                
            } else {
                
                NULL
                
            }
            
        } else if (input$select_question %in% type_3_questions) {
            
            if (input$select_comparison_type == "Year") {
                
                paste0("Row percentages, ", input$select_year_comparator, " data, ", coverage)
                
            } else if (input$select_comparison_type == "Local Authority") {
                
                paste0("Row percentages, ", input$select_year, " data, ", coverage)
                
            } else {
                
                NULL
            }
            
        } else if (input$select_question %in% type_4_questions) {
            
            paste0("Grossed-up estimates (Rounded to the nearest 10,000)")
        }
    })
    
    # main_title ####
    output$main_title <- renderText({
        
        if (input$select_question %in% type_1_questions) {
            
            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ")")
            
        } else if (input$select_question %in% c(type_2_questions, type_3_questions, type_4_questions)) {
            
            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ", ", input$select_year, ")")
        
        } else {
                
            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title)
            }
    })
    
    # comparison_title ####
    output$comparison_title <- renderText({
        
        if (input$select_question %in% type_1_questions & input$select_comparison_type == "Local Authority") {
            
            paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority_comparator, ")")
            
        } else {
            
            if (input$select_comparison_type == "Year") {
                
                paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority, ", ", input$select_year_comparator, ")")
                
            } else if (input$select_comparison_type == "Local Authority") {
                
                paste0(input$select_question, ": ", question_titles[question_titles$ID == input$select_question,]$Title, " (", input$select_local_authority_comparator, ", ", input$select_year, ")")
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
    # main_table ####
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
                                            options = list(
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
                                        options = list(
                                            digits = 1,
                                            na = "-",
                                            paging = FALSE,
                                            ordering = FALSE,
                                            info = FALSE,
                                            searching = FALSE,
                                            columnDefs = list(list(targets = c(0:2),
                                                                   visible = FALSE))))
            
        } else if (input$select_question %in% type_0_questions){
            
            data_table <- NULL
        } 
        
        return(data_table)
    })
    
    # comparison_table ####
    output$comparison_table <- renderDataTable({
        
        if (!input$select_question %in% type_4_questions) {
            
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
                          options = list(
                              digits = 1,
                              na = "-",
                              paging = FALSE,
                              ordering = FALSE,
                              info = FALSE,
                              searching = FALSE,
                              columnDefs = list(list(targets = c(0:2),
                                                     visible = FALSE))))
            
        } else {
            
            NULL 
        }
    })
    
    # excel_table ####
    output$excel_table <- DT::renderDataTable({
        
        excel_datatable <- DT::datatable(excel_df(),
                                         
                                         extensions = "Buttons",
                                         
                                         options = list(
                                             paging = FALSE,
                                             buttons = c("copy", "csv", "excel"),
                                             dom = "tB",
                                             columnDefs = list(list(targets = c(0), visible = FALSE))
                                         ),
                                         
                                         class = "display"
        )
        
        return(excel_datatable)
    })
    
    # CHARTS ####
    # main_chart ####
    output$main_chart <- plotly::renderPlotly({
        
        df <- main_chart_df()
        
        gather_key <- colnames(df[2])
        
        if (input$select_question %in% type_0_questions) {
            
            chart <- NULL
            
        } else if (input$select_question %in% type_1_questions) {
            
            line_chart_string <- paste0("ggplot(data = df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",], mapping = aes(x = `", gather_key, "`, y = Percent, group = `", measure_column_name(), "`, colour = `", measure_column_name(), "`)) + geom_line()")
            
            chart <- eval(parse(text = line_chart_string))
            
        } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {
            
            bar_chart_string <- paste0("ggplot(data = df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",], mapping = aes(x = `", gather_key, "`, y = `Percent`, fill = `", measure_column_name(), "`)) + geom_bar(position = \"dodge\", stat = \"identity\")")
            
            chart <- eval(parse(text = bar_chart_string))
            
        } else if (input$select_question %in% c(type_4_questions)) {
            
            bar_chart_string <- paste0("ggplot(data = df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",], mapping = aes(x = `", gather_key, "`, y = `", measure_column_name(), "`, fill = `Percent`)) + geom_bar(position = \"dodge\", stat = \"identity\")")
            
            chart <- eval(parse(text = bar_chart_string))
        }
        
        chart
    })
    
    # comparison_chart ####
    output$comparison_chart <- plotly::renderPlotly({
        
        if (input$select_comparison_type != "No comparison") {
            
            df <- comparison_chart_df()
            
            gather_key <- colnames(df[2])
            
            if (input$select_question %in% type_1_questions) {
                
                line_chart_string <- paste0("ggplot(data = df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",], mapping = aes(x = `", gather_key, "`, y = Percent, group = `", measure_column_name(), "`, colour = `", measure_column_name(), "`)) + geom_line()")
                
                chart <- eval(parse(text = line_chart_string))
                
            } else if (input$select_question %in% c(type_2_questions, type_3_questions)) {
                
                bar_chart_string <- paste0("ggplot(data = df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",], mapping = aes(x = `", gather_key, "`, y = `Percent`, fill = `", measure_column_name(), "`)) + geom_bar(position = \"dodge\", stat = \"identity\")")
                
                chart <- eval(parse(text = bar_chart_string))
                
            } else if (input$select_question %in% c(type_4_questions)) {
                
                bar_chart_string <- paste0("ggplot(data = df[df$`", measure_column_name(), "` != \"All\" & df$`", measure_column_name(), "` != \"Base\",], mapping = aes(x = `", gather_key, "`, y = `", measure_column_name(), "`, fill = `Percent`)) + geom_bar(position = \"dodge\", stat = \"identity\")")
                
                chart <- eval(parse(text = bar_chart_string))
            } else {
                
                chart <- NULL
            }
            
        } else {
            
            chart <- NULL
        }
        
        chart
    })
    #Report ####
    output$report <- downloadHandler(
        filename = paste0("report.pdf"),
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(local_authority = input$select_report_local_authority, 
                           year = input$select_report_year)
            
            rmarkdown::render(tempReport, output_format = "pdf_document", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
            
            detach("package:kableExtra", unload = TRUE)
        }
    )
}

# app ####
shinyApp(ui = ui, server = server)