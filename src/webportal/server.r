library(shiny)
library(plotly)
library(rusquant)
library(shinythemes)
library(data.table)
library(quantmod)
library(xts)
library(jsonlite)




shinyServer(
  function(input, output) {
    
    output$hist <- renderPlot({
    })
    
    output$heat <- renderPlotly({
      universe = get_universe()
      
      z_data = get_heatmap_data()
      dts = z_data$date
      z_data[,date:=NULL]
      plot_ly(z = as.matrix(z_data),
              x = names(z_data), y = dts, colors = colorRamp(c("red", "green")),
              type = "heatmap"
      )
      
    })
    
    get_universe <- reactive(
      {
        moex_stock_universe = getSymbolList(src='moex')
        liquidity_universe = getSymbols.Gigapack(paste(moex_stock_universe$SECID,collapse = ','),date='2020-01-03',field = 'spread_bbo.q80',type = 'candles')
        liquidity_universe = liquidity_universe[order(liquidity_universe[,3])]
        c("XXL liquidity","XL liquidity","L liquidity","M liquidity","S liquidity","XS liquidity")
        
        if(input$universe == 'XL liquidity') universe = liquidity_universe$symbol[1:42]
        if(input$universe == 'L liquidity') universe = liquidity_universe$symbol[43:84]
        if(input$universe == 'M liquidity') universe = liquidity_universe$symbol[85:126]
        if(input$universe == 'S liquidity') universe = liquidity_universe$symbol[127:168]
        if(input$universe == 'XS liquidity') universe = liquidity_universe$symbol[169:210]
        
        return(universe)
      })
    
    get_heatmap_data <- reactive(
      {
        universe = get_universe()
        algo_pack_data = getSymbols.Gigapack(paste(universe,collapse = ','),field = input$gigafield,type = 'candles')
        algo_pack_data = algo_pack_data[date>'2022-04-01' | date<'2022-02-21']
        z_data = dcast(algo_pack_data, date ~ symbol)
        z_date = as.character(z_data$date)
        setnafill(z_data[,-1], fill = 0,cols=names(z_data)[-1])
        return(z_data)
      })
    
    
    output$myplot <- renderUI({
      if (input$more_plots=="Indel_Histogram"){
        plot <- plotOutput("hist", height=800)
      }else plot <- plotlyOutput("heat", height=900)
      
    })
  }
)
