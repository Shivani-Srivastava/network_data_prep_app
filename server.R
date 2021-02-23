options(qwraps2_markup = "markdown")

shinyServer(function(input, output,session) {
  data <- reactive({
    if (is.null(input$file)) { 
      return(NULL)
    }else{
      df <- read.csv(input$file$datapath,
                     stringsAsFactors = TRUE,
                     header = TRUE)
      
      return(df)
      }
    })
  
  output$sample_data <- renderDataTable({head(data())})
  output$df_size <- renderText({paste0("Uploaded data has ",dim(data())[1]," rows and ", dim(data())[2]," columns")})
  
  output$summ <- renderPrint(
    if (is.null(input$file)) { 
      return(NULL)
    }else{
    ds_screener(data())
      }
    )
 # output$summ <- renderText(summary_table(mtcars))
  
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
  
  
  output$node_attr <- renderUI({
    if(is.null(input$file)){
      return(NULL)
    }else{
      #print(colnames(data()))
      df <- data()#[,-(input$id)]
      selectInput("attr","Select Node Attr",
                  choices = colnames(df)[!colnames(df) %in% input$id],
                  multiple = TRUE,
                  selected = colnames(df)[!colnames(df) %in% input$id])
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
  
  
  # adjaceny download
  output$download_adj_mat <- downloadHandler(
    filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_adj_mat.csv",collapse = " ") },
    content = function(file) {
      #df_csv <- values$df_data
      #rownames(df_csv) <- colnames(df_csv)
      write.csv(values$df_data, file,row.names=TRUE)
    }
  )
  
  attr_df <- reactive({
    df1 <- data()
    rownames(df1) <-  make.names(df1[,input$id], unique=TRUE)
    df1 <- df1[,input$attr]
    df1 <- tibble::rownames_to_column(df1, input$id)
    #df1[,input$id] <- rownames(df1)
    df1
  })
  

  output$download_node_attr <- downloadHandler(
    filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_node_attr.csv",collapse = " ") },
    content = function(file) {
      #df_csv <- values$df_data
      #rownames(df_csv) <- colnames(df_csv)
      write.csv(attr_df(), file,row.names=FALSE)
    }
  )
  
})