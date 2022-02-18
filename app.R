library(shiny)
library(datamations)
library(dplyr)
library(shinyAce)

# Define UI for application that draws a histogram
ui <- fluidPage(
  style = "max-width: 1200px;",
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

  # Show a plot of the generated distribution
  shiny::h1("Datamations"), 
  uiOutput('constructors'),
  uiOutput('editor'),
  uiOutput('datamation_ui')
    )

# Define server logic required to draw a histogram
server <- function(session, input, output) {

    # output$datamation <- renderDatamationSandDance({
    #   "small_salary %>% group_by(Degree) %>% summarise(mean = mean(Salary))" %>%
    #     datamation_sanddance()
    # })
    
    
    output$constructors <- renderUI({
      tagList(
        shiny::p("Construct a tidyverse pipeline by choosing from the options below. You select a data set, then up to three variables to group by, and finally a variable to summarize and a summary function to apply to it."),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::selectInput(inputId = "dataset",
                               "Dataset",
                               choices = c("small_salary", "penguins")
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "group_by",
              "Group by",
              choices = c("Work", "Degree"),
              selected = "Degree",
              multiple = TRUE
            )
          ),
          shiny::column(
            width = 2,
            shiny::selectInput(
              inputId = "summary_variable",
              "Summary variable",
              choices = "Salary"
            )
          ),
          shiny::column(
            width = 2,
            shiny::selectInput(
              inputId = "summary_function",
              "Summary function",
              choices = c("mean", "median", "min", "max")
            )
          ),
          shiny::column(
            width = 2,
            shiny::actionButton(inputId = "go", "Go", width = "100%", style = "margin-top: 28px;")
          )
        )
      )
    })
    
    
    
    ## EDITOR AND PIPELINE HANDLING
    
    output$editor <- renderUI({
      shiny::tagList(
        shiny::hr(),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            solidHeader = TRUE,
            shiny::h2("Pipeline"),
            shinyAce::aceEditor(
              outputId = "pipeline_editor",
              mode = "r",
              fontSize = 16,
              readOnly = TRUE,
              highlightActiveLine = FALSE,
              autoScrollEditorIntoView = TRUE,
              minLines = 2,
              maxLines = 30,
              value = "# Code will appear here based on selections above"
            ),
          )
        )
      )
    })
    
    pipeline <- shiny::eventReactive(input$go, {
      pipeline_group_by <- !is.null(input$group_by)
      if (pipeline_group_by) {
        glue::glue("{input$dataset} %>% group_by({paste0(input$group_by, collapse = ', ')}) %>% summarize({input$summary_function} = {input$summary_function}({input$summary_variable}, na.rm = TRUE))")
      } else {
        glue::glue("{input$dataset} %>% summarize({input$summary_function} = {input$summary_function}({input$summary_var}, na.rm = TRUE))")
      }
    })
    
    
    shiny::observeEvent(input$go, {
      text <- c("library(dplyr)\n", pipeline())
      # Load dplyr
      eval(parse(text = "library(dplyr)"))
      if (input$dataset == "penguins") {
        text <- c("library(palmerpenguins)\n", text)
        # Load palmerpenguins
        eval(parse(text = "library(palmerpenguins)"))
      }
      text <- styler::style_text(text)
      shinyAce::updateAceEditor(session, "pipeline_editor", value = paste0(text, collapse = "\n"))
    })
    
    output$datamation_ui <- renderUI({
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          solidHeader = TRUE,
          shiny::column(
            width = 6,
            shiny::h2("Datamation"),
            datamations::datamationSandDanceOutput("datamation")
          ),
          shiny::column(
            width = 6,
            shiny::h2("Data Stages"),
            #mod_data_tabs_ui("data_tabs")
          )
        )
      )
    })
    
    
    shiny::observeEvent(pipeline(), {
      
      # Generate datamation -----
      datamation <- shiny::reactive({
        datamation_sanddance(pipeline(), height = 300, width = 300)
      })
      
      # Create an output for it
      output$datamation <- datamations::renderDatamationSandDance({
        datamation()
      })
      
      # For some reason, doing renderUI is causing the javascript code to run twice (even when the actual R code is not) - so just use datamationSandDanceOutput directly, even if it means the slider shows initially!
      # output$datamation_ui <- shiny::renderUI({
      #   datamations::datamationSandDanceOutput(ns("datamation"))
      # })
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)