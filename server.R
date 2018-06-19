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
  
  filedata <- reactive({
    req(input$file1) # require that the input is available
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)}
    read.csv(inFile$datapath, stringsAsFactors=F)
  })
  
  output$dependent <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('dependent','Candidate data:',items, selected='')
  })
  
  output$candName <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('candidate', 'Name of candidate', '')
  })
  
  
  output$independent <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('independent', 'Racial demographic variable:', items, selected='')
  })
  
  output$raceVar <- renderUI({
    df <- filedata()  
    if (is.null(df)) return(NULL)
    textInput('racename', 'Name of minority race', '')
  })
  
  
  output$tot.votes <- renderUI({
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
  
  ## run models
  
  model <- eventReactive(input$action, {
    
    df <- filedata()[,c(input$independent, input$dependent, input$tot.votes)]
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
    
    # ei estimate
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
    row.names(edf.t) <- c(input$candidate, 'Homogeneous precincts', 'Goodman ER', 'Ecol Inf', 'EI.se')
    
    # goodman plot
    gr.plot <- ggplot(df, aes(x=x,y=y)) + 
      xlab(input$independent) + ylab(input$dependent) +
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
      xlab(paste('% population ', input$racename, sep='')) + ylab(paste('% vote for ', input$candidate, sep=''))
    
    # ei table
    ei.table <- as.data.frame(t(edf.t))
    for(i in 2:5){
      ei.table[,i] <- as.numeric(as.character(ei.table[,i]))
    }
    ei.table.final <- ei.table[,c(1:4)]
    
    # original data with ei estimates
    #df.ei <- df[,c(1:3)]
    #df.ei$EI.est.min <- eiread(ei.out, 'betab')
    #df.ei$EI.est.white <- eiread(ei.out, 'betaw')
    
    # ei dotplot
    
    ei.plot.df <- ei.table[,c(1,4,5)]
    names(ei.plot.df) <- c('race', 'ei.est', 'ei.se')
    
    ei.plot <- ggplot(ei.plot.df, aes(x=ei.est, y=1, col=as.factor(race))) +
      geom_hline(yintercept=1, col='black') +
      geom_point(size=6, shape=3) +
      ylab('') + xlab(paste('Support for candidate ', input$candidate, sep='')) +
      scale_x_continuous(limits=c(-.25,1.25)) +
      scale_y_continuous(limits=c(0,2), breaks=c(0,0.5,1,1.5,2), labels=c('','','','','')) +
      scale_color_manual('Race', values=c('gray40', 'midnightblue'), labels=c(paste('All but ', input$racename, sep=''), input$racename)) +
      geom_errorbarh(aes(xmin=(ei.est) - 2*(ei.se), xmax=(ei.est) + 2*(ei.se), height=0.1), size=5, alpha=0.7, height=0.1) +
      theme_bw() + ggtitle('Ecological Inference')
    
    
    list(gr.plot = gr.plot, ei.table = ei.table.final, ei.plot = ei.plot)      
  })    
  
  observeEvent(input$action, {
    output$goodman <- renderPlot({
      
      #withProgress(message='Running EI: Maximizing likelihood...importance sampling.',
      #             detail= 'This process may take several minutes...', value=4, {
      #               for(i in 1:10){
      #                 incProgress(1/10)
      #                 Sys.sleep(20)
      #               }
      #             })
      
      model()$gr.plot
    })
  })
  
  observeEvent(input$action, {
    output$est <- renderTable({
      
      model()$ei.table
    }, align='c', digits=3)
  })
  
  observeEvent(input$action, {
    output$ei.bounds <- renderPlot({
      
      model()$ei.plot
      
    }, width=750, height=200)
  })
  
  observeEvent(input$action,{
    output$est_expl <- renderUI({
      HTML(paste("First, we compare predictions from three different \n models for",input$dependent,
                 "given demographic and total vote data.", "<br/>","<br/>"))
    })
    
    
    output$goodman_expl <- renderUI({ 
      HTML(paste("<br/>","Next, we plot votes for",input$dependent, "against", input$independent,
                 "according to Goodman's regression predictions","<br/>","<br/>"))
    })
    
    output$bounds_expl <- renderUI({ 
      HTML(paste("<br/>","Finally, we calculate ecological inference predictions for",input$independent,
                 "with credible intervals.","<br/>","<br/>"))
    })
  })
  
  output$ei.compare <- renderTable({
    filedata()
  })
})