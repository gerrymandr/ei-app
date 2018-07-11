# ei-app
An R Shiny app that allows the user to run ecological inference 2x2 and nx2 analysis to determine whether or not there was racially polarized voting in a particular election. The app can be accessed on the web at https://vrdi.shinyapps.io/ei-app/.

Required to run analysis:
- Precinct-level data on vote totals by candidate for a given election (as percentages)
- Precinct-level demographic data

Output:
- Homogeneous precinct analysis results
- Goodman's regression results
- Ecological inference analysis results
- Text interpretation of results
- PDF report containing results and interpretation

Files:
- ExpertWitnessTemplate.docx : template for expert witness report on racially polarized voting
- ExpertWitnessTemplate.pages	: template for expert witness report on racially polarized voting
- report.Rmd	: PDF report of results
- santaClara.csv	: sample data for Santa Clara's 2014 city council election
- waterbury.csv : sample data for Waterbury election results
- server.R : R shiny file containing functions to run analysis and create plots
- ui.R	: R shiny file creating user interface
- www/userGuide.pdf : guide to using the app, including an example

More information in the user guide.
