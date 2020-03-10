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
library(colourpicker)
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



palettecols <- data.frame(palette=c("Pastel Green/Gold","Bold Red/Blue/Ochre","Red/Teal"),
                          title=c("#95A382","#990000","#AA3939"),
                          ornament=c("#C2CCB0","#990000","#AA3939"),
                          icon=c("#D6B878","#19347C","#D46A6A"),
                          icontxt=c("#95A382","#19347C","#D46A6A"),
                          bullet=c("#D6B878","#DA8035","#407F7F"),
                          number=c("#D6B878","#19347C","#D46A6A"),
                          noteaccent=c("#D6B878","#DA8035","#407F7F"),
                          text=c("#000000","#000000","#000000"),
                          stringsAsFactors = F)

getColor <- function(palette,item){ return(palettecols[[item]][palettecols$palette == palette]) }
getColors <- function(palette) { unlist( palettecols[palettecols$palette == palette,-1]) }


# Define UI for application that draws a histogram

header <- dashboardHeaderPlus( 
    title = "Recipe Generator"
)

sidebar <- dashboardSidebar(width="250px",
    sidebarMenu(

        selectInput("stylechoice","Choose Style",c("Classic","Modern","Scribbly"),selected="Classic"),
        
        hr(),
        
        uiOutput("palSelect"),
        
        fluidRow(column(5,offset=1,style='padding: 0px 0px;',uiOutput("titColSel") ),
                  column(5,style='padding: 0px 0px;',uiOutput("ornColSel")) ),
        
        fluidRow(column(5,offset=1,style='padding: 0px 0px;',uiOutput("iconColSel")),
                 column(5,style='padding: 0px 0px;',uiOutput("icontxtColSel"))),
        
        fluidRow(column(5,offset=1,style='padding: 0px 0px;',uiOutput("bullColSel")),
                 column(5,style='padding: 0px 0px;',uiOutput("numColSel"))),
        
        fluidRow(column(5,offset=1,style='padding: 0px 0px;',uiOutput("accColSel")),
                 column(5,style='padding: 0px 0px;',uiOutput("txtColSel"))),
        
        hr(),
        
        numericInput("fontsize","Font Size",13,min=8,max=25,step=1),
        
        numericInput("ingred_cols","# of Ingredients Columns",3,min=1,max=5,step=1)
       
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
    
    
    
    reValues <- reactiveValues(pal=palettecols$palette[1],
                               titcol=getColor(palettecols$palette[1],"title"),
                               orncol=getColor(palettecols$palette[1],"ornament"),
                               iconcol=getColor(palettecols$palette[1],"icon"),
                               icontxtcol=getColor(palettecols$palette[1],"icontxt"),
                               bullcol=getColor(palettecols$palette[1],"bullet"),
                               numcol=getColor(palettecols$palette[1],"number"),
                               acccol=getColor(palettecols$palette[1],"noteaccent"),
                               txtcol=getColor(palettecols$palette[1],"text"),
                               colvec = c(getColor(palettecols$palette[1],"title"),getColor(palettecols$palette[1],"ornament"),
                                           getColor(palettecols$palette[1],"icon"),getColor(palettecols$palette[1],"icontxt"),
                                           getColor(palettecols$palette[1],"bullet"),getColor(palettecols$palette[1],"number"),
                                           getColor(palettecols$palette[1],"noteaccent"),getColor(palettecols$palette[1],"text"))
                               )
    
    #####################################################################################################################
    
    #### Color Input Reactivity ####
    
    # Reactive Palette Selector UI (reacts to color choice)
    output$palSelect <- renderUI({
        selectInput("palette","Color Palette",c(palettecols$palette,"Custom"),selected=reValues$pal) 
    })
    
    # Reactive Color selectors UIs for each (reacts to palette choice)
    output$titColSel <- renderUI({ colourInput("titcol","Title/Headers",value=reValues$titcol,palette="square") })
    output$ornColSel <- renderUI({ colourInput("orncol","Ornament",value=reValues$orncol,palette="square") })
    output$iconColSel <- renderUI({ colourInput("iconcol","Icons",value=reValues$iconcol,palette="square") })
    output$icontxtColSel <- renderUI({ colourInput("icontxtcol","Icon Text",value=reValues$icontxtcol,palette="square") })
    output$bullColSel <- renderUI({ colourInput("bullcol","Bullets",value=reValues$bullcol,palette="square") })
    output$numColSel <- renderUI({ colourInput("numcol","Numbers",value=reValues$numcol,palette="square") })
    output$accColSel <- renderUI({ colourInput("acccol","Note Accent",value=reValues$acccol,palette="square") })
    output$txtColSel <- renderUI({ colourInput("txtcol","Body Text",value=reValues$txtcol,palette="square") })
    
    # change color reactive values based on palette selection
    observeEvent(input$palette,{
        if(input$palette=="Custom"){
        } else{
            reValues$titcol <- getColor(input$palette,"title")
            reValues$orncol <- getColor(input$palette,"ornament")
            reValues$iconcol <- getColor(input$palette,"icon")
            reValues$icontxtcol <- getColor(input$palette,"icontxt")
            reValues$bullcol <- getColor(input$palette,"bullet")
            reValues$numcol <- getColor(input$palette,"number")
            reValues$acccol <- getColor(input$palette,"noteaccent")
            reValues$txtcol <- getColor(input$palette,"text")
            reValues$colvec <- c(getColor(input$palette,"title"),getColor(input$palette,"ornament"),
                                 getColor(input$palette,"icon"),getColor(input$palette,"icontxt"),
                                 getColor(input$palette,"bullet"),getColor(input$palette,"number"),
                                 getColor(input$palette,"noteaccent"),getColor(input$palette,"text"))
        }
    })
    
    # change color reactive values based on color selection
    observeEvent(input$titcol,{
        reValues$titcol <- input$titcol
        reValues$colvec[1] <- input$titcol
    })
    
    observeEvent(input$orncol,{
        reValues$orncol <- input$orncol
        reValues$colvec[2] <- input$orncol
    })
    
    observeEvent(input$iconcol,{
        reValues$iconcol <- input$iconcol
        reValues$colvec[3] <- input$iconcol
    })
    
    observeEvent(input$icontxtcol,{
        reValues$icontxtcol <- input$icontxtcol
        reValues$colvec[4] <- input$icontxtcol
    })
    
    observeEvent(input$bullcol,{
        reValues$bullcol <- input$bullcol
        reValues$colvec[5] <- input$bullcol
    })
    
    observeEvent(input$numcol,{
        reValues$numcol <- input$numcol
        reValues$colvec[6] <- input$numcol
    })
    
    observeEvent(input$acccol,{
        reValues$acccol <- input$acccol
        reValues$colvec[7] <- input$acccol
    })
    
    observeEvent(input$txtcol,{
        reValues$txtcol <- input$txtcol
        reValues$colvec[8] <- input$txtcol
    })
    
    # change Palette reactive value based on color choices
    observeEvent(reValues$colvec,{
        
        palList <- sapply(palettecols$palette,getColors,simplify=F,USE.NAMES=T)
        
        nmatch <- sapply(palList,function(x){sum(reValues$colvec==x)})
        
        if(8 %in% nmatch){
            reValues$pal <- names(palList)[which.max(nmatch)]
        } else { reValues$pal <- "Custom"}
        
    })
    
    #####################################################################################################################
    
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
