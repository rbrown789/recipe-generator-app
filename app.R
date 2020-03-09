#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(knitr)
library(tinytex)
library(fs)
options(tinytex.engine_args = '-shell-escape') # https://tex.stackexchange.com/questions/93917/knit-with-pdflatex-shell-escape-myfile-tex


parseIngredients <- function(txt)
{
    # txt <- "1 egg\n2 pinches salt\nlove"
    ingvec <- strsplit(txt,"\n")[[1]]
    out <- paste0("- ",ingvec)
    return(out)
}

parseInstructions <- function(txt)
{
    instvec <- strsplit(txt,"\n")[[1]]
    out <- paste0("#. ",instvec)
    return(out)
}



# Define UI for application that draws a histogram

header <- dashboardHeaderPlus( 
    title = "Recipe Generator"
)

sidebar <- dashboardSidebar(
    sidebarMenu(

        selectInput("stylechoice","Choose Style",c("Classic","Modern","Scribbly"),selected="Classic"),
        
        numericInput("fontsize","Font Size",13,min=8,max=25,step=1),
        
        numericInput("ingred_cols","# of Ingredients Columns",3,min=1,max=5,step=1)

        # # # Custom CSS to hide the default logout panel
        # # tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        # #
        # # # The dynamically-generated user panel
        # # uiOutput("userpanel"),
        #
        # uiOutput('StudyIDInput'),
        # menuItem("Main", tabName = "main", icon = icon("dashboard")),
        # #menuItem("Activities", icon = icon("th"), tabName = "activities"),
        # #menuItem("Participant Manager", tabName = "participants"),
        # #menuItem("Device Manager", icon = icon("phone", lib = "glyphicon"), tabName = "devices"),
        # menuItem("Data Download", tabName = "data", icon = icon("download"))
    )
)

body <- dashboardBody(
    #  shinyDashboardThemes(
    #    theme = "grey_light"
    #  ),
    fluidRow(
        column(width=6,
               textInput("name","Recipe Name:","Recipe Name",width=500),
               textInput("auth","Recipe Author:","Author",width=500),
               textInput("time","Recipe Time:","e.g. 1 hour",width=300),
               numericInput("serves","How many servings:",value=1,min=1,max=50,step=1,width=150),
               textAreaInput("ingred","Ingredients:",value="List ingredients, one per line. Do not include bullets.", width=750,height=250),
               textAreaInput("instruct","Instructions:",value="List instructions, one per line. Do not include numbers.", width=750,height=250),
               actionButton('createpdf','Create PDF'),
               downloadButton('downloadPDF')
        ),
        column(width=6,uiOutput('showpdf'))
    )
)

ui <- dashboardPagePlus(    title = "Roland's Recipe Generator",
                            enable_preloader = TRUE,
                            header, sidebar, body,
                            skin = "blue")


# 
# ui <- fluidPage(
#     fluidRow(
#     column(width=6,
#     textInput("name","Recipe Name:","Recipe Name",width=500),
#     textInput("auth","Recipe Author:","Author",width=500),
#     textInput("time","Recipe Time:","e.g. 1 hour",width=300),
#     numericInput("serves","How many servings:",value=1,min=1,max=50,step=1,width=150),
#     textAreaInput("ingred","Ingredients:",value="List ingredients, one per line. Do not include bullets.", width=750,height=250),
#     textAreaInput("instruct","Instructions:",value="List instructions, one per line. Do not include numbers.", width=750,height=250),
#     actionButton('createpdf','Create PDF'),
#     downloadButton('downloadPDF')
#     ),
#     column(width=6,uiOutput('showpdf'))
#     )
# )


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
                      "\\usepackage{multicol}",
                      "\\begin{document}",paste0("\\markdownInput{",strsplit(mdpath,"/")[[1]][2],"}"),"\\end{document}"),
                    collapse="\n")
        
        write(rnwtxt,file=rnwpath)
        
        # create the dynamic markdown file from the text inputs
        mdpath <- paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".md")
        
        mdtxt <- paste( 
                   c(paste0("# ",input$name,"\n"),
                     "\\rcAuthorSymbol{} By",paste0(": ",input$auth),"\n",
                     "\\rcClockSymbol{} Ready in",paste0(": ",input$time),"\n",
                     "\\rcServingSymbol{} Serves",paste0(": ",input$serves),"\n",
                     "## Ingredients \n","\\begin{multicols}{3} \n", parseIngredients(input$ingred),"\n \\end{multicols}",
                     "## Instructions","\n",parseInstructions(input$instruct)),
                    collapse="\n")
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
