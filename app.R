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
library(fs)
options(tinytex.engine_args = '-shell-escape') # https://tex.stackexchange.com/questions/93917/knit-with-pdflatex-shell-escape-myfile-tex

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
    column(width=6,
    textInput("name","Recipe Name:",width=500),
    textInput("auth","Recipe Author:",width=500),
    textInput("time","Recipe Time:",width=300),
    numericInput("serves","How many servings:",value=1,min=1,max=50,step=1,width=150),
    textAreaInput("ingred","Ingredients:",value="List ingredients, one per line.", width=750,height=250),
    textAreaInput("instruct","Instructions:",value="List instructions, one per line.", width=750,height=250),
    actionButton('createpdf','Create PDF'),
    downloadButton('downloadPDF')
    ),
    column(width=6,uiOutput('showpdf'))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    # generate the temporary Rnw path name, markdown path name, tex path name, and PDF path name
    # these will be overwritten each time the user clicks "Create PDF"
    rnwpath <- fs::file_temp("input", tmp_dir = "www", ext = ".rnw")
    mdpath <- paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".md")
    texpath <- paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".tex")
    pdfpath <- strsplit(paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".pdf"),"/")[[1]][2]
    
    
    # create Rnw, markdown, and PDF files based on user input
    observeEvent(input$createpdf,{
        
        # create and write the dynamic Rnw file that sources the markdown filie
        rnwtxt <- paste(
                    c("% !TeX program = XeLaTeX","\\documentclass{article}","\\usepackage{scrextend}",
                      "\\changefontsizes[25pt]{13pt}","\\usepackage{simple-recipe}","\\usepackage{textcomp}",
                      "\\recipestyle{classic}","\\usepackage[margin=0.75in,includefoot,bottom=0.3in,footskip=2em]{geometry}",
                      "\\begin{document}",paste0("\\markdownInput{",strsplit(mdpath,"/")[[1]][2],"}"),"\\end{document}"),
                    collapse="\n")
        
        write(rnwtxt,file=rnwpath)
        
        # create the dynamic markdown file from the text inputs
        mdpath <- paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".md")
        mdtxt <- paste( paste0("# ",input$name) )
        write(mdtxt,file=mdpath)
        
        # knit the pdf 
        blah <- knit2pdf(input=rnwpath,output=texpath, clean = TRUE, compiler = "xelatex")
        
        # show pdf file in the iframe
        output$showpdf <- renderUI({
            tags$iframe(style = "height:1400px; width:100%", src = pdfpath )
        })
        
    })
    
    # download pdf
    output$downloadPDF <- downloadHandler(
        filename = function() {paste0(input$name,".pdf")},
        content = function(file) { file.copy(paste0("www/",pdfpath), file) }
    )
    
    # delete all temporary session files on session end
    session$onSessionEnded(function() {
        todelete <- dir(path="www",pattern = strsplit(pdfpath,".",fixed=T)[[1]][1])
        unlink(paste0("www/",todelete),recursive=T )
        })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
