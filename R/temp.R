## temp dates structure output
output$dates_string = renderText({
  paste(class(input$dates_vec), length(input$dates_vec))
})



## use quantmod to import market data
quantmod_data = reactiveValues(dt = NULL);



##
observeEvent(input$load_ohlcv_button, {
  
  if (is.null(quantmod_data$dt) && !is.null(output$ticker_df)) {
    
    quantmod_data$dt =
      load_market_data(input$dates_vec, output$ticker_df[[1]]);
    
  }
})

##
observeEvent(input$clear_ohlcv_button, {
  quantmod_data$dt = NULL;
})



## import tickers
output$ticker_df = renderTable({
  file_df = input$ticker_list_file;
  
  if (is.null(file_df)) {
    return(NULL);
  }
  
  ticker_df = 
    read.csv(
      file_df$datapath, header = input$is_ticker_header,
      stringsAsFactors = FALSE
    )
  colnames(ticker_df) = c("Ticker");
  
  ticker_df
})

