library(shiny)
library(shinydashboard)
library(ggplot2)
library(ei)
library(eiPack)
library(eiCompare)
library(shinycssloaders)

shinyServer(function(input, output, session) {
  
  url1 <- a("King's EI page", href='https://gking.harvard.edu/category/research-interests/methods/ecological-inference')
  output$king <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url1))
  })
  
  url2 <- a('Notes from Gingles Expert Witness (.pdf)', href='http://www.socsci.uci.edu/~bgrofman/74%20Grofman%201992.%20Expert%20Witness%20Testimony....pdf')
  output$groffman <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url2))
  })
  
  url3 <- a('Blacksher & Menefee (HeinOnline)', href='http://heinonline.org/HOL/LandingPage?handle=hein.journals/hastlj34&div=9&id=&page=')
  output$blacksher <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url3))
  })
  
  filedata <- reactive({ # Take in file
    req(input$file1) # require that the input is available
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)}
    read.csv(inFile$datapath, stringsAsFactors=F)
  })
  
  output$dependent1 <- renderUI({ #Prompt for candidate 1 data (column name)
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('dependent1','Candidate 1 data:',items, selected='')
  })
  
  output$candName1 <- renderUI({ #Prompt for candidate 1 name
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('candidate1', 'Name of candidate 1:', '')
  })
  
  output$dependent2 <- renderUI({ #Prompt for candidate 2 data (column name)
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('dependent2','Candidate 2 data:',items, selected='')
  })
  
  output$candName2 <- renderUI({ #Prompt for candidate 2 name
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('candidate2', 'Name of candidate 2:', '')
  })
  
  
  output$independent <- renderUI({ #Prompt for demographic data
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('independent', 'Racial demographic variable:', items, selected='')
  })
  
  output$raceVar <- renderUI({ #Prompt for user inputted name of race
    df <- filedata()  
    if (is.null(df)) return(NULL)
    textInput('racename', 'Name of minority race:', '')
  })
  
  
  output$tot.votes <- renderUI({ #Prompt for column to use for total votes
    df <- filedata()
    if(is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('tot.votes', 'Total votes cast:',items, selected='')
  })
  
  output$ui.slider <- renderUI({
    if (is.null(input$file1)) return()
    sliderInput('slider', 'Homogeneous precincts threshold', width='100%', min=0, max=25, step=1, ticks=T, post='%', value=5)
  })
  
  output$ui.action <- renderUI({
    if (is.null(input$file1)) return()
    actionButton('action', ' Run', icon('refresh', lib='glyphicon'))
  })
  
  
  run_model <- function(independent, dependent, tot.votes, candidate){
    # Function that generates the table, goodman plot, and EI metric (with confidence plot), given variables
    
    df <- filedata()[,c(independent, dependent, tot.votes)]
    names(df) <- c('x', 'y', 'z')
    
    # homogeneous precincts
    df <- df[order(df$x),]
    hp <- round(input$slider/100*dim(df)[1], digits=0)
    hp.low <- 1:hp
    hp.high <- (dim(df)[1]-hp):dim(df)[1]
    
    df$threshold <- 0
    df$threshold[hp.low] <- 1
    df$threshold[hp.high] <-1
    
    df$hp <- NA
    df$hp[hp.low] <- 1
    df$hp[hp.high] <- 1
    
    df$hp.text <- NA
    df$hp.text[hp.low] <- 'low'
    df$hp.text[hp.high] <- 'high'
    
    hp.low.mean <- mean(df$y[df$hp.text=='low'], na.rm=T)
    hp.high.mean <- mean(df$y[df$hp.text=='high'], na.rm=T)
    
    # goodman estimates
    ger <- lm(y~x, data=df)
    
    # ei estimate for table and confidence interval
    table.names <- c('ei.minority', 'ei.white')
    ei.out <- ei_est_gen('y', '~ x', 'z',
                         data = df[,c(1:3),], table_names = table.names, sample=1000) # eiCompare
    #ei.out <- ei(y~x, total=input$tot.votes, data=df) # ei
    edf.t <- data.frame(w=c(paste('All but ', input$racename, ' support', sep=''),
                            hp.low.mean,
                            ger$coefficients[1],
                            ei.out$ei.white[1]/100,
                            ei.out$ei.white[2]/100),
                        m=c(paste(input$racename, ' support', sep=''), 
                            hp.high.mean, 
                            ger$coefficients[1]+ger$coefficients[2], 
                            ei.out$ei.minority[1]/100,
                            ei.out$ei.minority[2]/100))
    row.names(edf.t) <- c(candidate, 'Homogeneous precincts', 'Goodman ER', 'Ecol Inf', 'EI.se')
    
    # generates goodman plot
    gr.plot <- ggplot(df, aes(x=x,y=y)) + 
      xlab(independent) + ylab(dependent) +
      geom_smooth(method='lm', se=T, colour='black', fullrange=TRUE) +
      scale_x_continuous(expand=c(0,0), limits=c(0,1)) +
      scale_y_continuous(expand=c(0,0), limits=c(-1.5,1.5)) +
      coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
      geom_point(size=3, aes(colour=as.factor(df$threshold))) +
      geom_point(pch=1, size=3) +
      geom_point(pch=1, size=5, aes(colour=as.factor(df$hp))) +
      scale_color_manual('Homogeneous precincts', breaks=c(0,1), values=c('Gray', 'Red'), labels=c('No', paste('Most extreme ', input$slider,'%', sep=''))) +
      geom_hline(yintercept=0.5, linetype=2, colour='lightgray') +
      theme_bw() + ggtitle("Goodman's Ecological Regression") +
      xlab(paste('% population ', input$racename, sep='')) + ylab(paste('% vote for ', candidate, sep=''))
    
    # generates ei table
    ei.table <- as.data.frame(t(edf.t))
    for(i in 2:5){
      ei.table[,i] <- as.numeric(as.character(ei.table[,i]))
    }
    ei.table.final <- ei.table[,c(1:4)]
    
    # original data with ei estimates
    #df.ei <- df[,c(1:3)]
    #df.ei$EI.est.min <- eiread(ei.out, 'betab')
    #df.ei$EI.est.white <- eiread(ei.out, 'betaw')
    
    # generates ei dotplot
    
    ei.plot.df <- ei.table[,c(1,4,5)]
    names(ei.plot.df) <- c('race', 'ei.est', 'ei.se')
    
    ei.plot <- ggplot(ei.plot.df, aes(x=ei.est, y=1, col=as.factor(race))) +
      geom_hline(yintercept=1, col='black') +
      geom_point(size=6, shape=3) +
      ylab('') + xlab(paste('Support for candidate ', candidate, sep='')) +
      scale_x_continuous(limits=c(-.25,1.25)) +
      scale_y_continuous(limits=c(0,2), breaks=c(0,0.5,1,1.5,2), labels=c('','','','','')) +
      scale_color_manual('Race', values=c('gray40', 'midnightblue'), labels=c(paste('All but ', input$racename, sep=''), input$racename)) +
      geom_errorbarh(aes(xmin=(ei.est) - 2*(ei.se), xmax=(ei.est) + 2*(ei.se), height=0.3), size=2, alpha=0.7, height=0.3) +
      theme_bw() + ggtitle('Ecological Inference')
    
    
    list(gr.plot = gr.plot, ei.table = ei.table.final, ei.plot = ei.plot) 
  }
  
  # Note: the same output cannot be called wtice in R Shiny, so there are duplicate copies below of
  # outputs in order to generate tables, plots, and explanations for each candidate tab
  model1 <- eventReactive(input$action, {
    # runs model on candidate 1
    run_model(input$independent, input$dependent1, input$tot.votes, input$candidate1)
  })    

  
  model2 <- eventReactive(input$action, {
    # runs model on candidate 2
    run_model(input$independent, input$dependent2, input$tot.votes, input$candidate2)
  })     
  
  observeEvent(input$action, {
    # generates goodman plots for candidates 1 and 2
    output$goodman1 <- renderPlot({
      model1()$gr.plot
    })
    output$goodman2 <- renderPlot({
      model2()$gr.plot
    })
  })
  
  output$est1 <- renderTable({
    # generates table for candidate 1
    req(input$action)
    model1()$ei.table}, align='c', digits=3)
  
  output$est2 <- renderTable({
    # generates table for candidate 2
    req(input$action)
    model2()$ei.table}, align='c', digits=3)
  
  observeEvent(input$action, {
    # generates EI bounds plot for candidate 1
    output$ei.bounds1 <- renderPlot({
      model1()$ei.plot
    }, width=650, height=200)
  })
  
  observeEvent(input$action, {
    # generates EI bounds plot for candidate 2
    output$ei.bounds2 <- renderPlot({
      model2()$ei.plot
    }, width=650, height=200)
  })
  
  observeEvent(input$action,{
    output$est_expl1 <- renderUI({
      HTML(paste("First, we compare predictions from three different models for",input$candidate1,
                 "'s vote share given demographic and total vote data.", "<br/>","<br/>"))
    })
    output$est_expl2 <- renderUI({
      HTML(paste("First, we compare predictions from three different models for",input$candidate2,
                 "'s vote share given demographic and total vote data.", "<br/>","<br/>"))
    })
    
    
    output$goodman_expl1 <- renderUI({ 
      HTML(paste("<br/>","Next, we plot votes for", input$candidate1, "by the proportion of the population that is", 
                 input$racename, "according to Goodman's regression predictions.","<br/>","<br/>"))
    })
    output$goodman_expl2 <- renderUI({ 
      HTML(paste("<br/>","Next, we plot votes for", input$candidate2, "by the proportion of the population that is", 
                 input$racename, "according to Goodman's regression predictions.","<br/>","<br/>"))
    })
    
    output$bounds_expl1 <- renderUI({ 
      HTML(paste("<br/>","Finally, we calculate ecological inference predictions for",input$candidate1, "'s vote share and plot them with credible intervals.",
       "If the intervals overlap, we cannot conclude that there was racially polarized voting for", input$candidate2,".","<br/>","<br/>"))
    })
    output$bounds_expl2 <- renderUI({ 
      HTML(paste("<br/>","Finally, we calculate ecological inference predictions for",input$candidate2,"'s vote share and plot them with credible intervals.",
        "If the intervals overlap, we cannot conclude that there was racially polarized voting for", input$candidate2,".","<br/>","<br/>"))
    })
  })
  
  output$ei.compare <- renderTable({
    filedata()
  })
  
  
  observeEvent(input$action, {
  output$report <- downloadHandler(
    filename = "report.pdf",
    
    
    content = function(file) {
      
      #copy report to temporary file
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      
      # Knit the document, passing in the `params` list
      rmarkdown::render(tempReport, output_file = file,
                        params = list(file1 = input$file1,
                                      independent = input$independent, 
                                      dependent1 = input$dependent1, 
                                      dependent2 = input$dependent2,
                                      tot.votes = input$tot.votes, 
                                      candidate1 = input$candidate1,
                                      candidate2 = input$candidate2,
                                      input_slider = input$slider,
                                      racename = input$racename),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  })
})