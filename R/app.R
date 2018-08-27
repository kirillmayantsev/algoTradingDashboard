library(shiny);
library(ggplot2);
library(stringr);
library(quantmod);
library(data.table);

ui = fluidPage(
  
  tabsetPanel(
    
    tabPanel(
      "Parameters",
      ## dates_vec -- Date vector of length 2
      sidebarPanel(
        dateRangeInput(
          inputId = "dates_vec",
          label = "Specify dates range",
          min = as.Date("2010-01-01"),
          max = Sys.Date(),
          weekstart = 1
        ),
        fileInput(
          inputId = "ticker_list_file",
          label = "Import txt file with tickers",
          accept = "text/plain"
        ),
        checkboxInput(
          inputId = "is_ticker_header",
          "Header",
          FALSE
        ),
        fileInput(
          inputId = "ticker_sector_file",
          label = "Ticker-sector division",
          accept = "text/plain"
        ),
        checkboxInput(
          inputId = "is_ticker_sector_header",
          "Header",
          FALSE
        )
      ),
      
      sidebarPanel(
        tableOutput(outputId = "ticker_dt"),
        tableOutput(outputId = "ticker_sector_dt")
      ),
      
      sidebarPanel(
        actionButton(
          inputId = "load_ohlcv_button",
          label = "Load data"
        ),
        actionButton(
          inputId = "clear_ohlcv_button",
          label = "Clear data"
        ),
        textOutput(outputId = "market_data_rows")
      )
    ),
    
    tabPanel(
      "Algorithm performance",
      plotOutput(
        outputId = "profit_plot",
        dblclick = "profit_plot_dblclick",
        brush = brushOpts(id = "profit_plot_brush", resetOnNew = TRUE)
      )
    ),
    
    tabPanel(
      "Export results"
    )
    
  )
  
)



server = function(input, output) {
  
  ## INIT
  
  ## set market data reactive values
  market_data =
    reactiveValues(ticker_dt = NULL, ticker_sector_dt = NULL, dt = NULL);
  
  ## load tickers
  observeEvent(c(input$ticker_list_file, input$is_ticker_header), {
    if (!is.null(input$ticker_list_file)) {
      ticker_df = 
        read.csv(
          input$ticker_list_file$datapath,
          header = input$is_ticker_header,
          stringsAsFactors = FALSE
        );
      colnames(ticker_df) = c("Ticker");
      setDT(ticker_df);
      setorder(ticker_df, Ticker);
      market_data$ticker_dt = ticker_df;
    }
  })
  
  ## load sectors
  observeEvent(c(input$ticker_sector_file, input$is_ticker_sector_header, market_data$ticker_dt), {
    if (!is.null(input$ticker_sector_file) && !is.null(market_data$ticker_dt)) {
      sector_df = 
        read.csv(
          input$ticker_sector_file$datapath,
          header = input$is_ticker_sector_header,
          stringsAsFactors = FALSE
        );
      colnames(sector_df) = c("Ticker", "Sector");
      setDT(sector_df);
      setorder(sector_df, Ticker);
      sector_df = sector_df[Ticker %in% market_data$ticker_dt$Ticker];
      market_data$ticker_sector_dt = sector_df;
    }
  })
  
  ## load data with quantmod
  observeEvent(input$load_ohlcv_button, {
    if (is.null(market_data$dt) && !is.null(market_data$ticker_dt)) {
      market_data$dt =
        load_market_data(input$dates_vec, market_data$ticker_dt$Ticker);
    }
  })
  
  ## clear market data
  observeEvent(input$clear_ohlcv_button, {
    market_data$dt = NULL;
  })
  
  ## print ticker_dt
  output$ticker_dt = renderTable(market_data$ticker_dt[1:min(4, .N)]);
  
  ## print ticker_sector_dt
  output$ticker_sector_dt = renderTable(market_data$ticker_sector_dt[1:min(4, .N)]);
  
  ## report number of rows in market_data$dt
  output$market_data_rows = renderText({
    ifelse(
      is.null(market_data$dt),
      "No market data loaded",
      paste("Loaded", nrow(market_data$dt), "rows")
    )
  })
  
  
  
  
  ## PLOT
  
  ## plot algorithm profit curve
  profit_range = reactiveValues(x = NULL, y = NULL);
  
  output$profit_plot = renderPlot({
    
    profit_dt = get_algorithm_profit();
    
    ggplot(profit_dt, mapping = aes(x = Date, y = Balance)) +
      geom_line(colour = "green", size = 1) +
      coord_cartesian(xlim = profit_range$x, ylim = profit_range$y, expand = TRUE) +
      theme_bw()
  })
  
  observeEvent(input$profit_plot_dblclick, {
    brush_vec = input$profit_plot_brush;
    if (!is.null(brush_vec)) {
      profit_range$x = as.Date(c(brush_vec$xmin, brush_vec$xmax));
      profit_range$y = c(brush_vec$ymin, brush_vec$ymax);
      
    } else {
      profit_range$x = NULL;
      profit_range$y = NULL;
    }
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
  xts_obj = as.data.frame(xts_obj, stringsAsFactors = FALSE);
  setDT(xts_obj);
  setnames(xts_obj, old = names(xts_obj), new = XTS_CURRENT_NAMES_VEC);
  
  ## Apply adjustments to all columns (use AdjClose/Close ratio)
  adj_vec = xts_obj$AdjClose / xts_obj$Close;
  xts_obj = lapply(xts_obj, function(x) {x * adj_vec});
  setDT(xts_obj);
  xts_obj[, AdjClose := NULL];
  XTS_CURRENT_NAMES_VEC = XTS_CURRENT_NAMES_VEC[1:5];
  
  xts_obj[, ':='(Date = dates_vec, Ticker = ticker)];
  
  if (!is.null(first_last_dates_vec)) {
    xts_obj = xts_obj[Date %between% first_last_dates_vec];
  }
  
  xts_obj =
    melt(
      xts_obj, id.vars = c("Date", "Ticker"),
      measure.vars = XTS_CURRENT_NAMES_VEC,
      variable.name = "Variable",
      value.name = "Value",
      variable.factor = FALSE
    );
  
  return(xts_obj);
}

get_algorithm_profit = function() {
  
  profit_dt =
    data.table(
      Date = seq(as.Date("2017/01/01"), as.Date("2017/12/31"), by = 1),
      #Date = seq(1, 365, by = 1),
      Balance = cumsum(rnorm(365, 1))
    );
  
  return(profit_dt);
}

shinyApp(ui = ui, server = server)


##