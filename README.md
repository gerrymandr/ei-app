# ei-app
An R Shiny app that allows the user to run ecological inference analysis to determine whether or not there was racially polarized voting in a particular election.
uses a user inputted CSV file to run 2x2 Homogenous Precincts, Goodman's Regression and Ecological Inference.
UI.R and Server.R are default R Shiny files and we added a report.Rmd to generate a PDF report of results.

Required to run analysis:
- Precinct-level data on vote totals for a given election
- Precinct-level demographic 

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

More information in the user guide and walkthrough.
