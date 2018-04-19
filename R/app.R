library(shiny);
library(quantmod);



ui = fluidPage(
  
  ## dates_vec -- Date vector of length 2
  dateRangeInput(
    inputId = "dates_vec",
    label = "Specify dates range",
    min = as.Date("2010-01-01"),
    max = Sys.Date(),
    weekstart = 1
  ),
  sidebarPanel(
    fileInput(
      inputId = "ticker_file_df",
      label = "Import txt file with tickers",
      accept = "text/plain"
    ),
    checkboxInput(
      inputId = "is_ticker_header",
      "Header",
      FALSE
    )
  ),
  textOutput(outputId = "dates_string"),
  tableOutput(outputId = "ticker_df")
  
  
);

server = function(input, output) {
  
  ## import tickers
  output$ticker_df = renderTable({
    file_df = input$ticker_file_df;
    
    if (is.null(file_df))
    {
      return(NULL);
    }
    
    read.csv(
      file_df$datapath, header = input$is_ticker_header,
      stringsAsFactors = FALSE
    )
  })
  
  output$dates_string = renderText({
    paste(class(input$dates_vec), length(input$dates_vec))
    #paste0(input$dates_vec, collapse = " ")
  })
  
};

shinyApp(ui = ui, server = server);


# getSymbols