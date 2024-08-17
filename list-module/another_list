library(shiny)
library(shinyjs)

# Generate some sample data to simulate server-side updates
generate_data <- function(start, end) {
  data.frame(
    name = paste("Region", start:end),
    value = paste(seq(from = start, to = end), "ha"),
    stringsAsFactors = FALSE
  )
}

# Define UI for the application
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # Main widget structure
  div(id = "treeLossFires", style = "width: 300px; margin: 20px auto;",
      h4("Regions with most tree cover loss due to fires in Turkey"),
      div(id = "scrollable-list", style = "height: 200px; overflow-y: auto; border: 1px solid #ddd;",
          tags$ul(class = "list-group")
      ),
      div(style = "margin-top: 10px; text-align: center;",
          actionButton("button_up", "Up", class = "btn btn-primary"),
          actionButton("button_down", "Down", class = "btn btn-primary")
      )
  ),
  
  # JavaScript to trigger the `load_more` event when scrolling to the bottom
  tags$script(HTML("
    $(document).on('shiny:sessioninitialized', function() {
      $('#scrollable-list').on('scroll', function() {
        var element = $(this)[0];
        if (element.scrollHeight - element.scrollTop === element.clientHeight) {
          Shiny.setInputValue('load_more', Math.random());
        }
      });
    });
  "))
)

# Define server logic
server <- function(input, output, session) {
  current_end <- reactiveVal(10)
  
  load_data <- function() {
    start <- current_end() + 1
    end <- start + 4
    current_end(end)
    generate_data(start, end)
  }
  
  # Load initial data
  observeEvent(TRUE,{
    initial_data <- generate_data(1, 10)
    for (i in seq_len(nrow(initial_data))) {
      insertUI(
        selector = "#scrollable-list ul",
        where = "beforeEnd",
        ui = tags$li(
          class = "list-group-item",
          paste(initial_data$name[i], "-", initial_data$value[i])
        )
      )
    }
  }, once = TRUE)
  
  # Load more data when `load_more` is triggered
  observeEvent(input$load_more, {
    new_data <- load_data()
    for (i in seq_len(nrow(new_data))) {
      insertUI(
        selector = "#scrollable-list ul",
        where = "beforeEnd",
        ui = tags$li(
          class = "list-group-item",
          paste(new_data$name[i], "-", new_data$value[i])
        )
      )
    }
  })
  
  # Scroll down by clicking button
  observeEvent(input$button_down, {
    shinyjs::runjs("
      var element = document.getElementById('scrollable-list');
      element.scrollTop += 40;
    ")
  })
  
  # Scroll up by clicking button
  observeEvent(input$button_up, {
    shinyjs::runjs("
      var element = document.getElementById('scrollable-list');
      element.scrollTop -= 40;
    ")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
