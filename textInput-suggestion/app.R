library(shiny)
library(shinyjs)

# Simulated Elasticsearch API
elasticsearch_api <- function(query) {
  Sys.sleep(2)  # Simulate a delay for data fetching
  data_source <- c("Elma", "Elma Altın", "Erik", "Armut", "Ayva", "Elmas", "Elastik", "Elon", "Eren", "Ece", "Elif")
  matching_items <- data_source[grepl(query, data_source, ignore.case = TRUE)]
  
  return(head(matching_items, 5))  # Return up to 5 matches
}

# UI
ui <- fluidPage(
  useShinyjs(),
  
  # CSS for suggestion dropdown and custom loader
  tags$head(
    tags$style(HTML("
      .search-container {
        position: relative;
        width: 300px;
      }
      #search {
        width: 100%;
        padding-right: 30px;
      }
      .loader {
        border: 4px solid #f3f3f3;
        border-radius: 50%;
        border-top: 4px solid #3498db;
        width: 20px;
        height: 20px;
        animation: spin 2s linear infinite;
        position: absolute;
        right: 10px;
        top: 50%;
        transform: translateY(-50%);
        display: none;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      #suggestions {
        border: 1px solid #ccc;
        max-height: 100px;
        overflow-y: auto;
        background-color: white;
        position: absolute;
        z-index: 1000;
        width: 100%;
        display: none;
      }
      #suggestions div {
        padding: 5px;
        cursor: pointer;
      }
      #suggestions div:hover {
        background-color: #f1f1f1;
      }
    "))
  ),
  
  div(class = "search-container",
      textInput("search", "Search:", value = ""),
      div(class = "loader", id = "loader"),
      tags$div(id = "suggestions")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to control if the loader should be shown
  show_loader <- reactiveVal(TRUE)
  
  # Debounced search reactive to avoid sending requests on every keystroke
  debounced_search <- debounce(reactive(input$search), 500)
  
  observeEvent(debounced_search(), {
    search_term <- debounced_search()
    
    if (nchar(search_term) > 0 && show_loader()) {
      # Show loader and disable input
      shinyjs::runjs("$('#loader').show();")
      shinyjs::disable("search")
      
      # Elasticsearch API call simulation
      matching_items <- elasticsearch_api(search_term)
      
      # Hide loader and re-enable input
      shinyjs::runjs("$('#loader').hide();")
      shinyjs::enable("search")
      
      # Display suggestions if there are matches
      if (length(matching_items) > 0) {
        suggestions_html <- paste0("<div>", matching_items, "</div>", collapse = "")
        shinyjs::runjs(sprintf("$('#suggestions').html('%s').show();", suggestions_html))
      } else {
        shinyjs::runjs("$('#suggestions').hide();")
      }
    } else {
      shinyjs::runjs("$('#suggestions').hide();")
    }
    
    # Reset show_loader to TRUE for future searches
    show_loader(TRUE)
  })
  
  # Handle suggestion click
  shinyjs::runjs("
    $(document).on('click', '#suggestions div', function() {
      var text = $(this).text();
      $('#search').val(text);
      $('#suggestions').hide();
      Shiny.setInputValue('search', text, {priority: 'event'});
      Shiny.setInputValue('skip_loader', true, {priority: 'event'});
    });
  ")
  
  # Observe the skip_loader input and prevent the loader if needed
  observeEvent(input$skip_loader, {
    show_loader(FALSE)
  })
  
  # Reset the show_loader and suggestion visibility when the input changes manually
  observeEvent(input$search, {
    if (nchar(input$search) == 0) {
      shinyjs::runjs("$('#suggestions').hide();")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
