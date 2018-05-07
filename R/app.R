library(shiny);
library(quantmod);



ui = fluidPage(
  
  tabsetPanel(
    tabPanel(
      "Parameters",
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
          inputId = "ticker_list_file",
          label = "Import txt file with tickers",
          accept = "text/plain"
        ),
        checkboxInput(
          inputId = "is_ticker_header",
          "Header",
          FALSE
        )
      ),
      actionButton(
        inputId = "load_ohlcv_button",
        label = "Load market data"
      ),
      actionButton(
        inputId = "clear_ohlcv_button",
        label = "Clear market data"
      )
    ),
    tabPanel(
      "Algorithm performance",
      textOutput(outputId = "dates_string"),
      tableOutput(outputId = "ticker_df")
    ),
    tabPanel(
      "Export results"
    )
  )
  
)



server = function(input, output) {
  
  ## init section
  
  ## use quantmod to import market data
  quantmod_data = reactiveValues(dt = NULL);
  
  
  ## import tickers
  output$ticker_df = renderTable({
    file_df = input$ticker_list_file;
    
    if (is.null(file_df)) {
      return(NULL);
    }
    
    read.csv(
      file_df$datapath, header = input$is_ticker_header,
      stringsAsFactors = FALSE
    )
  })
  
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
  
  ##
  output$dates_string = renderText({
    paste(class(input$dates_vec), length(input$dates_vec))
  })
  
}

# getSymbols

load_market_data = function(first_last_dates_vec, ticker_vec) {
  
  temp_env = new.env();
  
  invisible(
    getSymbols(
      env = temp_env,
      Symbols = ticker_vec,
      auto.assign = TRUE
    )
  );
  
  market_data_dt =
    rbindlist(
      lapply(
        temp_env, convert_xts_to_dt, first_last_dates_vec
      )
    );
  
  setkey(market_data_dt, Date, Ticker, Variable);
  return(market_data_dt);
  
}

convert_xts_to_dt = function(xts_obj, first_last_dates_vec = NULL) {
  
  XTS_CURRENT_NAMES_VEC =
    c("Open", "High", "Low", "Close", "Volume", "AdjClose");
  
  ticker = names(xts_obj)[1];
  ticker = substr(ticker, 1, str_locate(ticker, "[.]")[1, 1] - 1);
  
  dates_vec = index(xts_obj);
  xts_obj = as.data.frame(xts_obj);
  setDT(xts_obj);
  setnames(xts_obj, old = names(xts_obj), new = XTS_CURRENT_NAMES_VEC);
  xts_obj[, ':='(Date = dates_vec, Ticker = ticker)];
  
  if (!is.null(first_last_dates_vec)) {
    xts_obj = xts_obj[Date %between% first_last_dates_vec];
  }
  
  xts_obj =
    melt(
      xts_obj, id.vars = c("Date", "Ticker"),
      measure.vars = XTS_CURRENT_NAMES_VEC,
      variable.name = "Variable",
      value.name = "Value"
    );
  
  return(xts_obj);
}

shinyApp(ui = ui, server = server)
