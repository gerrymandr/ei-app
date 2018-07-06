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
  
  output$numCandidates <- renderUI({ #Prompt for number of candidates
    df <- filedata()
    if (is.null(df)) return(NULL)
    numericInput("numCandidates", label = "Number of candidates:", value = 1, min = 1, max = 20, step=1)
  })
  
  output$source1 <- renderUI({ #Prompt for source of elections data
    df <- filedata()  
    if (is.null(df)) return(NULL)
    textInput('electionSource', 'Source for elections data:', placeholder='For graph citation')
  })
  
  output$source2 <- renderUI({ #Prompt for source of demographic data
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('demsource', 'Source for demographic data:', placeholder='For graph citation')
    textInput('demographicSource', 'Source for demographic data:', placeholder='For graph citation')
  })
  
  
  # Prompt for candidate names and data
  output$candDataPrompts <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$numCandidates)) return(NULL)
    numCandidates <- as.integer(input$numCandidates)
    items=names(df)
    names(items)=items

    lapply(1:numCandidates, function(i) {
      varName1 <- paste("dependent",i, sep = "")
      text1 <- paste("Candidate ", i, " data: ", sep= "")
      selectInput(varName1,text1,items)
    })

  })
  
  output$candNamePrompts <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$numCandidates)) return(NULL)
    numCandidates <- as.integer(input$numCandidates)
    items=names(df)
    names(items)=items

    lapply(1:numCandidates, function(i) {
      varName2 <- paste("candidate",i, sep = "")
      text2 <- paste("Name of candidate ", i, ": ", sep= "")
      textInput(varName2, text2)
    })
  })

  
  ## Defaults for testing:
  # output$dependent1 <- renderUI({ #Prompt for candidate 1 data (column name)
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('dependent1','Candidate 1 data:',items, selected='pct_for_hardy2')
  # })
  # 
  # output$candName1 <- renderUI({ #Prompt for candidate 1 name
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   textInput('candidate1', 'Name of candidate 1:', value='hardy')
  # })
  # 
  # output$dependent2 <- renderUI({ #Prompt for candidate 2 data (column name)
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('dependent2','Candidate 2 data:',items, selected='pct_for_kolstad2')
  # })
  # 
  # output$candName2 <- renderUI({ #Prompt for candidate 2 name
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   textInput('candidate2', 'Name of candidate 2:', value='kolstad')
  # })
  # 
  # output$dependent3 <- renderUI({ #Prompt for candidate 2 data (column name)
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('dependent3','Candidate 3 data:',items, selected='pct_for_nadeem2')
  # })
  # 
  # output$candName3 <- renderUI({ #Prompt for candidate 2 name
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   textInput('candidate3', 'Name of candidate 3:', value='nadeem')
  # })
  
  
  output$independent <- renderUI({ #Prompt for demographic data
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('independent', 'Demographic variable:', items)
  })
  
  output$raceVar <- renderUI({ #Prompt for user inputted name of race
    df <- filedata()  
    if (is.null(df)) return(NULL)
    textInput('racename', 'Name of demographic group:', placeholder='X-axis label')
  })
  
  
  output$tot.votes <- renderUI({ #Prompt for column to use for total votes
    df <- filedata()
    if(is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('tot.votes', 'Total votes cast:',items) 
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
      theme_bw() + ggtitle("Goodman's Ecological Regression") + labs(x = paste('% population ', input$racename, sep=''),
      y= paste('% vote for ', candidate, sep=''), 
      caption = paste('Election data from', input$electionSource, 'and demographic data from', input$demographicSource, sep = ' '))
    
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
      geom_point(size=6, shape=3) + labs(y=(''), x = paste('Support for candidate ', candidate, sep=''), 
      caption = paste('Election data from', input$electionSource, 'and demographic data from', input$demographicSource, sep = ' '))  + scale_x_continuous(limits=c(-.25,1.25)) +
      scale_y_continuous(limits=c(0,2), breaks=c(0,0.5,1,1.5,2), labels=c('','','','','')) +
      scale_color_manual('Race', values=c('gray40', 'midnightblue'), labels=c(paste('All but ', input$racename, sep=''), input$racename)) +
      geom_errorbarh(aes(xmin=(ei.est) - 2*(ei.se), xmax=(ei.est) + 2*(ei.se), height=0.3), size=2, alpha=0.7, height=0.3) +
      theme_bw() + ggtitle('Ecological Inference')
    
    
    list(gr.plot = gr.plot, ei.table = ei.table.final, ei.plot = ei.plot) 
  }
  
  dependents <- eventReactive(input$action, {
    numCandidates <- input$numCandidates
    cands <- c()
    candNames <- c()
    for(i in 1:numCandidates){
      cands <- c(cands, input[[paste("dependent",i,sep="")]])
      candNames <- c(candNames, input[[paste("candidate",i,sep="")]])
    }
    list(cands = cands, candNames = candNames, numCandidates = numCandidates)
    
  })
  
  models <- eventReactive(input$action, {
    models <- list()
    for(i in 1:dependents()$numCandidates){
      #name = paste("model",i, sep = "")
      new <- run_model(input$independent, dependents()$cands[i],
                                   input$tot.votes, dependents()$candNames[i])
      models[[i]] <- new
    }
    models
  })
  
  
  ##Generate tables for candidates
  
  observeEvent(input$action, {
    
    lapply(1:input$numCandidates, function(i) {
      output[[paste0('est', i)]] <- renderTable({
        models()[[i]]$ei.table}, align='c', digits=3)
    })
  })
  
  ##Generates plots for candidates
  observeEvent(input$action, {
    lapply(1:input$numCandidates, function(i) {
      output[[paste0('goodman', i)]] <- renderPlot({
        models()[[i]]$gr.plot
      })
    })
    
    lapply(1:input$numCandidates, function(i) {
      output[[paste0('ei.bounds', i)]] <- renderPlot({
        models()[[i]]$ei.plot
      }, width=650, height=200)
      })
    })
    
  
  ##Explanation of table
  observeEvent(input$action,{
    lapply(1:input$numCandidates, function(i) {
      output[[paste0('est_expl', i)]] <- renderUI({
        HTML(paste0("First, we compare predictions from three different models for ",input[[paste0('candidate',i)]],
                   "'s vote share given demographic and total vote data.", "<br/>","<br/>"))
      })
    })
  
  ##Explanations of Goodman plots
    lapply(1:input$numCandidates, function(i) {
      output[[paste0('goodman_expl', i)]] <- renderUI({
        withMathJax(HTML(paste("<br/>","Next, we plot votes for", input[[paste0('candidate',i)]], "by the proportion of the population that is", 
                               input$racename, "according to Goodman's regression predictions. Every point represents a precinct. The best fit is given by: <br/><br/>",
                               input[[paste0('dependent',i)]],"=\\(\\beta_0 + \\beta_1\\)",input$independent, "<br/><br/>Least squares gives us \\(\\beta_0 = \\)",
                               round(models()[[i]]$ei.table[1,3],3), "and \\(\\beta_1 =\\)", round(models()[[i]]$ei.table[2,3]-models()[[i]]$ei.table[1,3],3), ".<br/><br/>")))
      })
    })
    
  ##Explanations of EI bounds plots
    lapply(1:input$numCandidates, function(i) {
      output[[paste0('bounds_expl', i)]] <- renderUI({
        HTML(paste("<br/>","Finally, we calculate ecological inference predictions for",input[[paste0('candidate',i)]], "'s vote share and plot them with credible intervals. These credible intervals
                 give us ranges of possible vote shares by race. We are 95% confident that the true vote shares for", input$candidate1, " will fall in these two ranges. In other 
                   words, if we did 100 ecological inference predictions, 95 times out of 100, the vote share would fall in these intervals. <br/> <br/>",
                   "If the intervals do not overlap for each candidate, we can infer that difference in preference is statistically signficiant and
                   this may be evidence to suggest racially polarized voting.", "<br/>","<br/>"))      })
    })
  })

  
  output$ei.compare <- renderTable({
    filedata()
  }, spacing="xs")
  
  output$template <- downloadHandler(
    filename = "template.docx",
    content = function(file) {
      file.copy("ExpertWitnessTemplate.docx", file)
    }
  )
  
  output$templatePages <- downloadHandler(
    filename = "template.pages",
    content = function(file) {
      file.copy("ExpertWitnessTemplate.pages", file)
    }
  )
  
  output$sample1 <- downloadHandler(
    filename = "SantaClaraSampleData.csv",
    content = function(file) {
      file.copy("santaClara.csv", file)
    }
  )
  
  output$sample2 <- downloadHandler(
    filename = "WaterburySampleData.csv",
    content = function(file) {
      file.copy("waterbury.csv", file)
    }
  )
  
  output$welcome <- renderUI({
    req(is.null(input$file1)) # require that the input is null
    HTML(paste("<br/><br/><br/><br/><br/><br/>", tags$h2(tags$b("Welcome"), align="center"),
               tags$h5(tags$i("No data is currently loaded."), align="center") ))
  })
  
  output$mytabs = renderUI({
    if (is.null(input$file1)){
      output$welcome <- renderUI({
        req(is.null(input$file1)) # require that the input is null
        HTML(paste("<br/><br/><br/><br/><br/><br/>", tags$h2(tags$b("Welcome"), align="center"),
                   tags$h5(tags$i("No data is currently loaded."), align="center"),
                   "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>"))
      })
    }
    req(input$numCandidates>0)
    nTabs = input$numCandidates
    myTabs = lapply(1: nTabs, function(i) {
      tabPanel(paste0("Candidate ",i), 
               htmlOutput(paste0("est_expl",i)),
               tableOutput(paste0("est",i)),
               htmlOutput(paste0("goodman_expl", i)),
               plotOutput(paste0("goodman",i)),
               htmlOutput(paste0("bounds_expl",i)),
               plotOutput(paste0("ei.bounds", i)))
    })
    myTabs[[nTabs + 1]] <- tabPanel('Data', div(style = 'overflow-x: scroll', tableOutput('ei.compare')))
    do.call(tabsetPanel, myTabs)
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
                                      candidate3 = input$candidate3,
                                      candidate4 = input$candidate4,
                                      input_slider = input$slider,
                                      racename = input$racename,
                                      numCands = input$numCandidates),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  })
  
  
  
 # observeEvent(input$action, {
 #   if(input$numCandidates == 2) {
 #    insertTab(inputId = "tabs",
 #              tabPanel('Candidate 1 Figures', htmlOutput("welcome"), withSpinner(tableOutput('est1')),
 #                        htmlOutput("goodman_expl1"), plotOutput('goodman1'),
 #                        htmlOutput("bounds_expl1"), plotOutput('ei.bounds1')),
 #              tabPanel('Candidate 2 Figures', htmlOutput("est_expl2"), withSpinner(tableOutput('est2')),
 #                       htmlOutput("goodman_expl2"), plotOutput('goodman2'),
 #                       htmlOutput("bounds_expl2"), plotOutput('ei.bounds2')))
 #  }
 #  if (input$numCandidates > 2) {
 #   return(NULL)
 #  }
 #  })
  
  })

