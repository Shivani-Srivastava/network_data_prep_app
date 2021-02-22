shinyServer(function(input, output,session) {
  data <- reactive({
    if (is.null(input$file)) { 
      return(NULL)
    }else{
      df <- read.csv(input$file$datapath,
                     stringsAsFactors = FALSE,
                     header = TRUE)
      
      return(df)
      }
    })
  
  output$sample_data <- renderDataTable({head(data())})
  output$sel_id_var <- renderUI({
    if(is.null(input$file)){
      return(NULL)
    }else{
    #print(colnames(data()))
      selectInput("id","Select Identity column",
                  choices = colnames(data()),
                  multiple = FALSE,
                  selected = colnames(data()))
    }
  })
  
  values <- reactiveValues(df_data = NULL) 
  
  observeEvent(input$apply,{
    input_df <- data()
    adj0 = df2adjacency(input_df,
                        cutoff_percentile = input$cut_off,
                        id_var = input$id)
    values$df_data <- adj0
  })
  
  
  output$sample_adj <- renderDataTable({
    datatable(values$df_data[1:8,1:8], rownames = TRUE )
    
  })
  
  
  
  # sample data download
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "mtcars.csv" },
    content = function(file) {
      write.csv(read.csv("data/mtcars.csv"), file,row.names = FALSE)
    }
  )
  
  #df_csv <- reactive({})
  # adjaceny download
  output$download_adj_mat <- downloadHandler(
    filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_adj_mat.csv",collapse = " ") },
    content = function(file) {
      #df_csv <- values$df_data
      #rownames(df_csv) <- colnames(df_csv)
      write.csv(values$df_data, file,row.names=TRUE)
    }
  )
  
  
})