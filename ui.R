library(shiny)
library(shinydashboard)
library(ggplot2)
library(ei)
library(eiPack)
library(eiCompare)
library(shinycssloaders)

dashboardPage(
  
  dashboardHeader(title = "Ecological Inference Analysis",
                          titleWidth=285
                  ),

  dashboardSidebar(width=285,
      fileInput('file1', 'Upload CSV file', accept=c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv')
                ),
                            
      uiOutput('dependent'),
      uiOutput('candName'),
        tags$hr(),
      uiOutput('independent'),
      uiOutput('raceVar'),
        tags$hr(),
      uiOutput('tot.votes'),
        tags$hr(),
      uiOutput('ui.slider'),
        br(),
      uiOutput('ui.action')
                  ),

  dashboardBody(
      
    fluidRow(column(width=3,
           
           box(
             width=NULL, height='380px',
             title = 'Instructions', status='primary', solidHeader=TRUE,
             '1. Upload CSV file on the left.',
             tags$br(),
             '(Code variables 0 to 1)',
             tags$br(), tags$br(),
             '2. Select the variables of interest.',
             tags$br(), tags$br(),
             '3. Adjust the slider to select homogeneous precinct threshold.',
             tags$br(), '(by % of precincts in sample)',
             tags$br(),tags$br(),
             '4. Click "Run."',
             tags$div(tags$ul(tags$li('Note that EI analysis can take several minutes depending on the size of your dataset.'))),
             #tags$br(),tags$br(),
             '5. Review figures & tables.'
           ),
           
           box(
             width=NULL, height='150px', status='info',
             #title='Resources', 
             tags$h6('R pkgs: ', 
                     a('ei |', href='https://cran.r-project.org/web/packages/ei/index.html'), 
                     a('eiPack |', href='https://cran.r-project.org/web/packages/eiPack/index.html'), 
                     a('eiCompare |', href='https://cran.r-project.org/web/packages/eiCompare/index.html'),
                     a('MCMCpack', href='https://cran.r-project.org/web/packages/MCMCpack/index.html')),
             #tags$br(),
             uiOutput('king'),
             uiOutput('groffman'),
             uiOutput('blacksher'),
             tags$h6(a('More...', href='https://scholar.google.com/scholar?q=ecological+inference+voting+rights&btnG=&hl=en&as_sdt=0%2C7'))
           ),
           
           box(icon('globe', lib='glyphicon'), width=NULL, background='black',
               'MGGG @ Tufts/MIT 2017',
               br(),
               #icon('random', lib='glyphicon'),
               tags$code('GIS-Hackathon 1.0')
           )  
    ),
    
    column(width=9,
           
           tabBox(
             width=NULL, side='right', height='625px',
             selected='Figures',
             tabPanel('Map', 'Coming soon!', br(), tags$div(tags$ul(tags$li('User uploads shapeFiles and EI analysis is paired with choropleth of precincts by EI estimates.')))),
             tabPanel('Data', tableOutput('ei.compare')),
             tabPanel('Figures', htmlOutput("est_expl"), withSpinner(tableOutput('est')), 
                      htmlOutput("goodman_expl"), plotOutput('goodman'), 
                      htmlOutput("bounds_expl"), plotOutput('ei.bounds'))
                )
          )
      )
   )
)



