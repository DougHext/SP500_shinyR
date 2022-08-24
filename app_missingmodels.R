library(tidyverse)
library(shiny)
library(DT)
library(OpenImageR)
library(rsconnect)
library(tidyquant)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(modeltime)
library(modeltime.ensemble)
library(timetk)
library(parsnip)

df = read_csv("data/sp500_stocks.csv")
df = na.omit(df)

load("models.RData")
models_list = list(model_fit_glmnet,model_fit_rf,model_fit_xgboost)
FORECAST_HORIZON <- 7
MONTHS_OF_TRAINING <- 40

lag_transformer_grouped <- function(data){
  data %>%
    group_by(Symbol) %>%
    tk_augment_lags(Close, .lags = 1:MONTHS_OF_TRAINING) %>%
    ungroup()
}

ui <- fluidPage(
  tabsetPanel(
    tabPanel("EDA",fluid=TRUE,
             
             sidebarLayout(
               sidebarPanel(
                 actionButton("Update" ,"Refresh Data", icon("sync")),
                 dateRangeInput("daterange_EDA", label = "Date Range:",
                                start = df$Date[1],
                                end   = df$Date[length(df$Date)],
                                min   = df$Date[1],
                                max   = df$Date[length(df$Date)]),
                 selectInput('var1', 'Stock', choices = c(unique(df$Symbol))), 
                 selectInput('var2', 'Concatenate?', choices = c("Yes","No")),
                 conditionalPanel(
                   condition = "input.var2 == 'No'",
                   selectInput("var3", "Variable", choices = colnames(df)[3:8])
                 ),
               ),
               
               mainPanel(
                 DT::DTOutput("table") %>% withSpinner(type=6),
                 plotOutput("plot") %>% withSpinner(type=6),
                 plotOutput("conditional_plot") %>% withSpinner(type=6),
                 plotOutput("gaf") %>% withSpinner(type = 6)
               )
             )
    ),
    tabPanel("Modeling",fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("daterange_modeling", label = "Date Range:",
                                start = df$Date[1],
                                end   = df$Date[length(df$Date)]+FORECAST_HORIZON,
                                min   = df$Date[1],
                                max   = df$Date[length(df$Date)]+FORECAST_HORIZON),
                                 selectInput('modeltype', 'Model', choices = c("GlmNet","Random Forest","XGBoost")),
                 selectInput('stockforecast', 'Stock', choices = c(unique(df$Symbol))),
                 selectInput('response', 'Variable', choices = c("Close"))
               ),
               mainPanel(
                 plotOutput("forecast") %>% withSpinner(type=6),
               )
             )
    )
  )
)
server <- function(input, output, session) {
  
  observeEvent(input$Update, {
    showModal(modalDialog("Pulling New Data", footer=NULL))
    
    SP500 <- tq_index("SP500")
    StockList <- SP500$symbol
    StockList = str_replace(StockList,"\\.","-")
    
    
    df <- tq_get(StockList)
    df = rename(df,Symbol=symbol, Date=date, Open=open, 'Adjusted Close'= adjusted, Close=close, High=high, Volume=volume, Low=low)
    write_csv(df,"data/sp500_stocks.csv")
    df = na.omit(df)
    updateDateRangeInput(session,inputId = "daterange",
                         start = df$Date[1],
                         end   = df$Date[length(df$Date)],
                         min   = df$Date[1],
                         max   = df$Date[length(df$Date)]
    )
    
    
    removeModal()
  })
  
  
  
  # Reactive dataframe for EDA
  df_filtered <- reactive({
    df2 <- subset(df, Date >= input$daterange_EDA[1] & Date <= input$daterange_EDA[2] & Symbol== input$var1) 
    if (input$var2 == 'Yes') {
      df2 = df2
    }else{
      df2 = df2 %>% select(Date,Symbol,input$var3)
    }
  })
  
  
  
  output$table <- DT::renderDataTable({
    df_filtered()
  })
  
  
  output$plot = renderPlot({
    req(input$var1,input$var2)
    if(input$var2 == "No"){
      df_filtered() %>% ggplot(aes(x=Date,y = !!as.name(input$var3))) + geom_point(colour="#0f52ba") +
        xlab("Date") +
        ylab(input$var3) + labs(paste("Value of",input$var1,input$var3,sep = " "))
    }else{
      df_filtered() %>% select(-Volume) %>% pivot_longer("Open":"Adjusted Close", names_to = "variable", values_to = "value") %>% 
        ggplot(aes(
          x = Date,
          y = value,
          color = variable
        ) 
        )+ geom_point(size=.5) + labs("Date vs Values") + ylab("Values") + guides(colour = guide_legend(override.aes = list(size=5)))
    }
  })
  
  output$conditional_plot <- renderPlot({
    # specify condition
    req(input$var2 == "Yes")
    
    # execute only if condition is met
    df_filtered() %>% ggplot(aes(x=Date,y = Volume)) + geom_point(colour="#0f52ba") +
      xlab("Date") +
      ylab("Volume") + labs("Date vs Volume")
  })
  
  
  output$gaf = renderPlot({
    if(input$var2 == "No"){
      
      a = df_filtered() %>% select(input$var3)
      amin = min(a,na.rm=T)
      amax = max(a,na.rm=T)
      
      
      scaled_a = (2*a - amax-amin)/(amax-amin)
      if(amax==0 && amin == 0){
        scaled_a = a
      }
      
      phi = na.omit(unlist(acos(scaled_a)))
      
      gafmat = as.matrix(cos(outer(phi,phi,"+")))
      gafmat = resizeImage(gafmat,224,224)
      
      image(gafmat)
    }else{
      l = list()
      for(i in 1:6){
        a = df_filtered() %>% select(colnames(df_filtered())[i+2])
        amin = min(a,na.rm=T)
        amax = max(a,na.rm=T)
        
        
        scaled_a = (2*a - amax-amin)/(amax-amin)
        if(amax==0 && amin == 0){
          scaled_a = a
        }
        
        phi = na.omit(unlist(acos(scaled_a)))
        
        gafmat = as.matrix(cos(outer(phi,phi,"+")))
        gafmat = resizeImage(gafmat,224,224)
        l[[i]] = gafmat
      }
      
      
      image(rbind(cbind(l[[1]],l[[2]],l[[3]]),cbind(l[[4]],l[[5]],l[[6]])))
    }
  })
  
  
  
  # Reactive Objects for Modeling
  modeltime_df <- reactive({
    df2 = df %>% filter(Symbol==input$stockforecast) %>%  select(Date,Symbol,input$response) %>%
      group_by(Symbol) %>%
      future_frame(
        .length_out = FORECAST_HORIZON,
        .bind_data  = TRUE
      ) %>%
      ungroup() %>% 
      lag_transformer_grouped()
  })
  
  modeltime_train_data = reactive({
    train_data <- modeltime_df() %>%
      drop_na()
  })
  
  
  modeltime_future_data = reactive({
    future_data <- modeltime_df() %>%
      filter(is.na(Close))
  })  
  
  model_index = reactive({
    model_idx = which(c("GlmNet","Random Forest","XGBoost") ==input$modeltype)  
  })    
  
  
  
  modeltime_ensemble_panel = reactive({
    recursive_ensemble_panel <- modeltime_table(
      models_list[[model_index()]],
      models_list[[model_index()]]
    ) %>%
      ensemble_weighted(loadings = c(1,1)) %>%
      recursive(
        transform  = lag_transformer_grouped,
        train_tail = panel_tail(modeltime_train_data(), Symbol, MONTHS_OF_TRAINING),
        id         = "Symbol"
      )
  })   
  
  modeltime_model_tbl = reactive({
    model_tbl <- modeltime_table(
      modeltime_ensemble_panel()
    )
  })
  

  
  modeltime_tbl_res = reactive({
    
    tbl_res = modeltime_model_tbl() %>%
      modeltime_forecast(
        new_data    = modeltime_future_data(),
        actual_data = modeltime_df(),
        keep_data   = TRUE
      ) 
    
  })    
  
  
  output$forecast = renderPlot({
    
    modeltime_tbl_res() %>% filter(Date>=input$daterange_modeling[1] & Date <= input$daterange_modeling[2]) %>% ggplot(aes(
      x=Date,
      y=.value,
      color=.key)) + geom_line() + xlab("Date") + ylab("Close")
  })
  
  
  
}

shinyApp(ui,server)