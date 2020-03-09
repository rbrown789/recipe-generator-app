#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(tinytex)
options(tinytex.engine_args = '-shell-escape') # https://tex.stackexchange.com/questions/93917/knit-with-pdflatex-shell-escape-myfile-tex

# Define UI for application that draws a histogram
ui <- fluidPage(
    textInput('firstname', 'First name', value = 'Jimmy'),
    textInput('lastname', 'Last name', value = 'John'),
    downloadButton('report')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$report = downloadHandler(
        filename = 'myreport.pdf',
        
        content = function(file) {
            out = knit2pdf('input.Rnw', clean = TRUE, compiler = "xelatex")
            file.rename(out, file) # move pdf to file for downloading
        },
        
        contentType = 'application/pdf'
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
