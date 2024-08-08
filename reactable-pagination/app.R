library(shiny)
library(reactable)

ui <- fluidPage(
  numericInput(inputId = "max_rows", label = "max_rows", value = 11),
  verbatimTextOutput("page_info"),

  tags$script(HTML("
  $(document).on('click', '.rt-page-button', function() {
    var tableId = 'table';
    var tableState = Reactable.getState(tableId);
    var currentPage = tableState.pageIndex + 1;
    Shiny.setInputValue(tableId + '_current_page', currentPage, {priority: 'event'});
  });

  $(document).ready(function() {
    var tableId = 'table';
    var tableState = Reactable.getState(tableId);
    if (tableState) {
      var currentPage = tableState.pageIndex + 1;
      Shiny.setInputValue(tableId + '_current_page', currentPage, {priority: 'event'});
    }
  });
")),
  reactableOutput(outputId = "table"),
)

server <- function(input, output, session) {
  
  output$page_info <- renderText({
    validate(
      need(!is.null(input$table_current_page),"")
    )
    paste("Current page:", input$table_current_page)
  })

  output$table <- renderReactable({
    reactable(
      data = iris[1:11,],
      pagination = TRUE,
      defaultPageSize = 10,
      paginationType = "numbers",
      showPagination = TRUE,
      selection = "single",
      onClick = "select"
    )
  })
  
  max_page <- reactiveVal(1)

  observe({
    max_page( max(isolate(max_page()),input$table_current_page ))
  })
  
  data_load <- reactiveVal(10) 
  
  observeEvent(max_page(),{
    data_load <- data_load(data_load() + 10 )
    updateReactable(
      outputId = "table",
      data = iris[1:data_load(),],
      page = max_page()
    )
  })
}

shinyApp(ui, server)
