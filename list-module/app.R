library(shiny)
library(shinyjs)

# Module UI
listModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "list-container",
        div(class = "list-body",
            tags$ul(class = "list", id = ns("scrollable-list"))
        ),
        div(class = "list-paginate",
            actionButton(ns("button_up"), label = NULL, icon = icon("arrow-up"), class = "c-button"),
            actionButton(ns("button_down"), label = NULL, icon = icon("arrow-down"), class = "c-button")
        )
    )
  )
}

# Module Server
listModuleServer <- function(id, initial_data_func, load_more_func) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_end <- reactiveVal(0)
    
    observeEvent(TRUE, {
      initial_data <- initial_data_func()
      current_end(nrow(initial_data))
      for (i in seq_len(nrow(initial_data))) {
        insertUI(
          selector = paste0("#", ns("scrollable-list")),
          where = "beforeEnd",
          ui = tags$li(
            div(class = "list-item",
                div(class = "item-label",
                    div(class = "item-bubble", i),
                    div(class = "item-name", initial_data$name[i])
                ),
                div(class = "item-value", initial_data$value[i])
            )
          )
        )
      }
      
      # JavaScript to detect scroll
      shinyjs::runjs(sprintf("
        $('#%s').on('scroll', function() {
          var scrollTop = $(this).scrollTop();
          var scrollHeight = $(this).prop('scrollHeight');
          var height = $(this).height();
          if (scrollTop + height >= scrollHeight - 10) {
            Shiny.setInputValue('%s', Math.random());
          }
        });
      ", ns("scrollable-list"), ns("load_more")))
    }, once = TRUE)
    
    observeEvent(input$load_more, {
      new_data <- load_more_func()
      for (i in seq_len(nrow(new_data))) {
        insertUI(
          selector = paste0("#", ns("scrollable-list")),
          where = "beforeEnd",
          ui = tags$li(
            div(class = "list-item",
                div(class = "item-label",
                    div(class = "item-bubble", current_end() + i),
                    div(class = "item-name", new_data$name[i])
                ),
                div(class = "item-value", new_data$value[i])
            )
          )
        )
      }
      current_end(current_end() + nrow(new_data))
    })
    
    observeEvent(input$button_down, {
      shinyjs::runjs(sprintf("
        var element = document.getElementById('%s');
        element.scrollTop += 40;
      ", ns("scrollable-list")))
    })
    
    observeEvent(input$button_up, {
      shinyjs::runjs(sprintf("
        var element = document.getElementById('%s');
        element.scrollTop -= 40;
      ", ns("scrollable-list")))
    })
  })
}

# Example data generation functions
generate_initial_data <- function() {
  generate_data(1, 10)
}

generate_more_data <- function() {
  generate_data(11, 15)
}

generate_data <- function(start, end) {
  data.frame(
    name = paste("Region", start:end),
    value = paste(seq(from = start, to = end), "B"),
    stringsAsFactors = FALSE
  )
}

# CSS styling
css <- "
.list-container {
  max-height: 400px;
  overflow-y: auto;
  border: 1px solid #ddd;
  border-radius: 8px;
  padding: 10px;
  background-color: #f9f9f9;
}
.list-body {
  max-height: 300px;
  overflow-y: auto;
}
.list {
  list-style: none;
  padding: 0;
  margin: 0;
  max-height: 300px;
  overflow-y: auto;
}
.list-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px 0;
  border-bottom: 1px solid #eee;
}
.item-label {
  display: flex;
  align-items: center;
}
.item-bubble {
  background-color: #007bff;
  color: white;
  font-weight: bold;
  border-radius: 50%;
  width: 24px;
  height: 24px;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-right: 10px;
}
.item-value {
  font-size: 1rem;
  color: #333;
}
.list-paginate {
  display: flex;
  justify-content: center;
  margin-top: 10px;
}
.c-button {
  background-color: #007bff;
  border: none;
  color: white;
  padding: 5px 10px;
  text-align: center;
  display: inline-block;
  font-size: 14px;
  margin: 4px 2px;
  cursor: pointer;
  border-radius: 50%;
}
"

# Define UI for the application
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML(css))),
  
  # Use the module
  listModuleUI(
    id = "list1"
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Use the module with dynamic content loading
  listModuleServer(
    id = "list1", 
    initial_data_func = generate_initial_data, 
    load_more_func = generate_more_data
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
