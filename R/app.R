#################### Load necessary packages 
library(shiny)
library(readxl)
library(writexl)
library(tidyverse)
library(fmsb)

#################### Shiny UI #################### 
ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("WASPAS for Dummies"),
  sidebarLayout(
    sidebarPanel(
      tags$div(HTML("<strong>WELCOME!</strong>"),
               hr(),
               h5("This is a web machine that implements the WASPAS method. 
                  All you have to do is to download the template spreadsheet, fill it with your data 
                  (alternatives, criteria, weights, the Cost-Benefit flags and the values), and upload it.
                  the web WASPAS machine will do the whole hard work for you.
                  "), 
               hr(),
               h4("Download the template Spreadsheet"), 
               p("Please, use the link below to download the template for data input."),
               a(href="https://flaviob.shinyapps.io/waspas/WASPAS_Data_Template.xlsx"
                 , "Download Template", download=NA, target="_blank"),                 
               hr(),
      ),
      fluidRow(tags$img(height = 124, width = 90, src = "aluno.jpeg"),
               tags$img(height = 124, width = 90, src = "orientador.jpeg")),                 
      hr(),
      a("flavio.barbara@gmail.com", href="mailto:flavio.barbara@gmail.com"),
      hr(),
      em("All rights reserved. Non-commercial (academic) use of this worksheet is free.
          The only thing asked in return is to cite this tool when the results are used.
          in publications."),
      br("To cite the source:"),
      em("BARBARA, Flavio; SANTOS, Marcos, WASPAS in R/Shiny 2023.")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Input",
                 h4("Load CSV from Spreadsheet"),
                 p("Please, upload the file (.csv or .xlsx)."),
                 fileInput("upload", NULL, accept = c(".csv", ".xlsx")),
                 HTML("<hr>"),
                 htmlOutput("uploaded_data"),
                 HTML("<hr>"),
                 htmlOutput("valid_flags"),
                 htmlOutput("valid_weights"),
                 htmlOutput("valid_values"),
                 actionButton("buttonView", "View your data"),
        ),
        tabPanel("View",
                 actionButton("buttonCalc", "Calculate WASPAS"),
                 tableOutput("upfile")
        ),
        tabPanel("Output",
                 sliderInput(inputId = "lambda",
                             label = "Define the lambda Value:",
                             min = 0,
                             max = 1,
                             value = 0.5),
                 tableOutput("waspas")
        ),
        tabPanel("Radar Graph",
                 sliderInput(inputId = "lambdaR",
                             label = "Define the lambda Value:",
                             min = 0,
                             max = 1,
                             value = 0.5),
                 plotOutput('radarPlot')
        )
      )
    )))

#################### Shiny Server #################### 
#################### spreadsheet loader
server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = read.csv(input$upload$datapath, header = F),
           xlsx = read_xlsx(input$upload$datapath, col_names = F),
           validate("Invalid file! Please upload a .csv or .xlsx file")
    )
  })
  output$upfile <- renderTable({data()})
  
  observeEvent(input$buttonView, {
    updateTabsetPanel(session, "tabset", selected = "View")
  })
  
  ############# cost-benefit flags validation
  observeEvent(input$upload, {
    req(data())
    itsok = TRUE
    for(iCol in 2:(ncol(data()))){
      if(data()[1,iCol] != "Cost" && data()[1,iCol] != "Benefit"){
        output$valid_flags <- renderText(paste("<span style=\"color:red\">Invalid data! First row must have only values 'Cost' or 'Benefit'.</span>"))
        itsok = FALSE
        break
      }
    }
    if (itsok) {output$valid_flags <- renderText("Flags Cost-Benefit are ok.")}
    
    ############# weights validation
    itsok <- TRUE
    stops <- FALSE
    totalweight <- 0
    for(iCol in 2:(ncol(data()))){
      weight <- as.numeric(data()[2,iCol])
      if (!is.numeric(weight) || is.na(weight)){
        output$valid_weights <- renderText(paste("<span style=\"color:red\">Invalid data! Weights must be numeric values. There is a strange '",data()[2,iCol],"' value in the data.</span>"))
        itsok = FALSE
        break
      } else {
        output$valid_weights <- renderText("Ok. Weights are all numeric.")
        totalweight <- totalweight + weight
      }
    }
    if (itsok) {
      if (toString(totalweight) != toString(1)) {
        output$valid_weights <- renderText(
          paste("<span style=\"color:red\">Invalid data! The total sum of the weights must be 1. But they are summing ", totalweight,"</span>"))
      } else {
        output$valid_weights <- renderText("Weights are all numeric and totalize 1.")
      }
    }
    ############# general values validation
    for(iRow in 4:(nrow(data()))){
      for(iCol in 2:(ncol(data()))){
        value <- as.numeric(data()[iRow,iCol])
        if (!is.numeric(value) || is.na(value)){
          output$valid_values <- renderText(paste(
            "<span style=\"color:red\">Invalid data! There is a non numeric value: '",data()[iRow,iCol]
            ,"' at line '", data()[iRow,1], "' column '", data()[3,iCol]
            ,"'. Please fix it.</span>"))
          stops <- TRUE 
          break
        }
      }
      if (stops) break
      output$valid_values <- renderText("The values entered are all numeric. Congratulations!")
    }
  })
  
  #################### render the successful results
  observeEvent(input$upload, {
    req(data())
    output$uploaded_data <- renderUI(
      tagList(tags$p(paste("Uploaded file: ",input$upload$name)),
              tags$p(paste("Size: ",input$upload$size, " bytes")),
              tags$p(paste("Type: ",input$upload$type)),
              tags$p(paste("Number of Criteria: ",ncol(data())-1)),
              tags$p(paste("Number of Alternatives: ",nrow(data())-3))
      )
    )})
  
  #################### normalize values; calculates wsm, wpm and waspas
  #################### renders result table and radar chart
  observeEvent(input$buttonCalc, {
    updateTabsetPanel(session, "tabset", selected = "Output")
    myAxCNorm <- normalize(data())    ## Normalization
    myAxC_WSM <- calcWSM(myAxCNorm)   ## WSM Calculation
    myAxC_WPM <- calcWPM(myAxCNorm)   ## WSM Calculation
    vWSM <- na.omit(select(myAxC_WSM, 1, ncol(myAxC_WSM)))
    vWPM <- na.omit(select(myAxC_WPM, 1, ncol(myAxC_WPM)))
    waspas <- cbind(vWSM,vWPM[,"Points"], WASPAS=0.0)
    colnames(waspas) <- c("Alternative", "WSM_Rank","WPM_Rank","WASPAS_Rank")
    observe({
      val <- input$lambda
      waspas$WASPAS_Rank <- waspas$WSM_Rank * input$lambda + waspas$WPM_Rank * (1-input$lambda)
      waspas<-waspas[order(waspas$WASPAS_Rank),]
      output$waspas <- renderTable(waspas)
      radardf <- create_radardf(waspas)
      output$radarPlot <- renderPlot({radarchart(
        radardf,
        axistype = 4,
        plwd = 4 ,
        plty = 1,
        cglcol = "grey",
        cglty = 1,
        axislabcol = "grey",
        cglwd = 0.8,
        vlcex = 0.8
      )})
    })
  })
  
  observeEvent(input$lambdaR, {
    req(data())
    updateSliderInput(session, "lambda", value = input$lambdaR)
    # legend(x=0.7, y=1.2, legend = c("WSM","WPM","WASPAS"))
  })
  
}

#################### Generic Functions #################### 
#################### Radar chart
create_radardf  <- function(inputdf) {
  # Transposing the matrix (df) rows will be column names
  radardf <- data.frame(matrix(ncol = nrow(inputdf), nrow = 0))
  colnames(radardf) <- inputdf$Alternative
  radardf[1,]<-1.0
  radardf[2,]<-0.0
  radardf[3,]<-inputdf[,2]
  radardf[4,]<-inputdf[,3]
  radardf[5,]<-inputdf[,4]
  return(radardf)
}

#################### Normalization: AxCWork Matrix  ==>  AxCNorm Matrix
normalize <- function(AxCWork) {
  AxCNorm <- AxCWork
  for(iCol in 2:ncol(AxCWork)){
    v <- AxCWork[4:nrow(AxCWork),iCol]
    v <- sapply(v,as.numeric)
    maxv <- max(v)
    minv <- min(v)
    for(iRow in 4:nrow(AxCWork)){
      if(AxCWork[1, iCol] == "Cost"){
        AxCNorm[iRow,iCol] <- toString(minv / as.numeric(AxCWork[iRow,iCol]))
      } else {  #"Cost-Benefit"] == "Benefit"
        AxCNorm[iRow,iCol] <- toString(as.numeric(AxCWork[iRow,iCol]) / maxv)
      }
    }}
  return(AxCNorm)
}

#################### Ranking for WSM Method: AxCNorm Matrix  ==>  AxC_WSM Matrix
calcWSM <- function(AxCNorm) {
  Points = rep(0,nrow(AxCNorm))
  AxC_WSM <- cbind(AxCNorm, Points)
  for(iCol in 2:(ncol(AxC_WSM)-1)){
    for(iRow in 4:nrow(AxC_WSM)){
      AxC_WSM[iRow,iCol] <- toString(as.numeric(AxC_WSM[iRow,iCol]) * as.numeric(AxC_WSM[2,iCol]))
    }}
  # calculate ranking
  for(iRow in 4:nrow(AxC_WSM)){
    v <- AxC_WSM[iRow, 3:ncol(AxC_WSM)-1]
    v <- sapply(v,as.numeric)
    AxC_WSM[iRow,"Points"] <- sum(v)
  }
  return(AxC_WSM)
}

#################### Ranking for WPM Method: AxCNorm Matrix  ==>  AxC_WPM Matrix
calcWPM <- function(AxCNorm) {
  Points = rep(0,nrow(AxCNorm))
  AxC_WPM <- cbind(AxCNorm, Points)
  for(iCol in 2:(ncol(AxC_WPM)-1)){
    for(iRow in 4:nrow(AxC_WPM)){
      AxC_WPM[iRow,iCol] <- toString(as.numeric(AxC_WPM[iRow,iCol]) ^ as.numeric(AxC_WPM[2,iCol]))
    }}
  # calculate ranking
  for(iRow in 4:nrow(AxC_WPM)){
    v <- AxC_WPM[iRow, 3:ncol(AxC_WPM)-1]
    v <- sapply(v,as.numeric)
    AxC_WPM[iRow,"Points"] <- prod(v)
  }
  return(AxC_WPM)
}

####### run app
shinyApp(ui = ui, server = server)