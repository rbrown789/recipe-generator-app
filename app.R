#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

######## PREAMBLE ######

# libraries and options
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(colourpicker)
library(knitr)
library(tinytex)
library(fs)
library(base64enc)
library(magick)
options(tinytex.engine_args = '-shell-escape') # https://tex.stackexchange.com/questions/93917/knit-with-pdflatex-shell-escape-myfile-tex
options(shiny.reactlog = T)

# global functions and data
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

parseNotes <- function(txt)
{
    notesvec <- strsplit(txt,"\n")[[1]]
    out <- paste0("> ",notesvec)
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


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

####### UI CODE #######

header <- dashboardHeaderPlus( 
    titleWidth="250px",
    title = "Recipe Generator"
)

sidebar <- dashboardSidebar(width="250px",
    sidebarMenu(
        
        # h5("PDF Appearance Options"),
        # h5("(no effect currently)"),

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
        
        numericInput("ingred_cols","# of Ingredients Columns",3,min=2,max=5,step=1)
       
    )
)

body <- dashboardBody(
    
    tags$head(tags$script(src="doCrop.js")),
    #  shinyDashboardThemes(
    #    theme = "grey_light"
    #  ),
    fluidRow(
        column(width=6,
                textInput("name","Recipe Name:","Recipe Name",width=500),
                textInput("auth","Recipe Author:","Author",width=500),
                textInput("time","Recipe Time:","e.g. 1 hour",width=300),
                numericInput("serves","How many servings:",value=1,min=1,max=50,step=1,width=150),
                textAreaInput("ingred","Ingredients:",value="List ingredients.\nOne per line.\nDo not include bullets.", width=750,height=200),
                textAreaInput("instruct","Instructions:",value="List instructions.\nOne per line.\nDo not include numbers.", width=750,height=200),
                textAreaInput("notes","Notes:",value="Additional notes. \nLeave blank if no notes desired.", width=750,height=100),
                
                fluidRow( 
                    column(6,
                           fileInput("image_upload",label = "Upload Image (Optional)", width = "300px",accept = c("image/png", "image/jpeg", "image/jpg")),
                           imwidgets::cropperOutput("imgcropper",width="350px",height="350px")),
                    column(6,h5(strong("Cropped Image")))
                    ),
               
                actionButton("crop", "Crop Image"),
                br(),
                br(),
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


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

####### SERVER CODE #######

server <- function(input, output, session) {
    
    options(shiny.maxRequestSize=10*1024^2) 
    
    #####################################################################################################################
    
    #### Color Input Reactivity ####
    
    # all required reactive values
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
                                          getColor(palettecols$palette[1],"noteaccent"),getColor(palettecols$palette[1],"text")),
                               
                               # the iterator
                               cropiterator = 0,
                               imgpath = NULL,
                               image_upload = NULL
                               
    )
    
    
    # Reactive Palette Selector UI (reacts to color choice)
    output$palSelect <- renderUI({
        selectInput("palette","Color Palette",c(palettecols$palette,"Custom"),selected=reValues$pal) 
    })
    
    # Reactive Color selectors UIs for each (reacts to palette choice)
    output$titColSel <- renderUI({ colourpicker::colourInput("titcol","Title/Headers",value=reValues$titcol,palette="square") })
    output$ornColSel <- renderUI({ colourpicker::colourInput("orncol","Ornament",value=reValues$orncol,palette="square") })
    output$iconColSel <- renderUI({ colourpicker::colourInput("iconcol","Icons",value=reValues$iconcol,palette="square") })
    output$icontxtColSel <- renderUI({ colourpicker::colourInput("icontxtcol","Icon Text",value=reValues$icontxtcol,palette="square") })
    output$bullColSel <- renderUI({ colourpicker::colourInput("bullcol","Bullets",value=reValues$bullcol,palette="square") })
    output$numColSel <- renderUI({ colourpicker::colourInput("numcol","Numbers",value=reValues$numcol,palette="square") })
    output$accColSel <- renderUI({ colourpicker::colourInput("acccol","Note Accent",value=reValues$acccol,palette="square") })
    output$txtColSel <- renderUI({ colourpicker::colourInput("txtcol","Body Text",value=reValues$txtcol,palette="square") })
    
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
    
    ##### Generating the Rnw, Markdown, tex, and PDF file names #####
    
    # generate the temporary Rnw path name, markdown path name, tex path name, and PDF path name
    # these will be overwritten each time the user clicks "Create PDF"
    rnwpath <- fs::file_temp("input", tmp_dir = "www", ext = ".rnw")
    stypath <- paste0("www/rc-",strsplit(strsplit(rnwpath,".",fixed=T)[[1]][1],"/",fixed=T)[[1]][2],".sty")
    mdpath <- paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".md")
    texpath <- paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".tex")
    pdfpath <- strsplit(paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".pdf"),"/")[[1]][2]
    filepref <- strsplit(pdfpath,".",fixed=T)[[1]][1]

    
    #####################################################################################################################
    
    ### image upload and cropping ####
    observeEvent(input$image_upload, {
        reValues$image_upload <- input$image_upload
        reValues$cropiterator  <- reValues$cropiterator + 1
    })
    

    observe({
        inFile <- reValues$image_upload
        if (is.null(inFile))
            return()
        
        if(reValues$cropiterator > 1){ unlink(paste0("www/",reValues$imgpath)) }
        
        if(inFile$type == "image/png") {
            reValues$imgpath <- paste0(filepref,"_",reValues$cropiterator,".png")
        } else if(inFile$type %in% c("image/jpeg", "image/jpg") ){
            reValues$imgpath <- paste0(filepref,"_",reValues$cropiterator,".jpg")
        }

        file.copy(inFile$datapath, file.path("www/",reValues$imgpath) )
        
        output$imgcropper <- imwidgets::renderCropper({
            req(reValues$image_upload)
            imwidgets::cropper(reValues$imgpath)
        })
    })
    
    
    observeEvent(input$crop,{
        session$sendCustomMessage("crop","")
    })
    
    observeEvent(input$iniateCrop,{
        
        ext <- strsplit(reValues$imgpath,".",fixed=T)[[1]][2]
        
        if(grepl("Area",input$dimensions)){
            
            image_read(paste0("www/",reValues$imgpath) ) %>%
                image_crop(geometry= substr(input$dimensions,7,nchar(input$dimensions))) %>%
                image_write( paste0("www/",filepref,"_cropped.",ext) )
            
        } else{
            image_read(paste0("www/",reValues$imgpath) ) %>%
                image_write( paste0("www/",filepref,"_cropped.",ext) )
        }
    })
    
    #####################################################################################################################
    
    # create Rnw, markdown, and PDF files based on user input
    observeEvent(input$createpdf,{
        
       
        # create and write the dynamic latex style file 
        if(input$stylechoice=="Modern"){
            stytxt <- paste(
                c("\\RequirePackage{fontspec}",
                  "\\setsansfont[Path = fonts/,Ligatures=TeX,UprightFont = *-Regular,BoldFont=*-Medium,ItalicFont = *-Italic,Scale=0.92]{FiraSans}",
                  "\\setmainfont[Path = fonts/,Ligatures=TeX,UprightFont = *-Regular,BoldFont=*-Medium,ItalicFont = *-Italic,Scale=0.92]{FiraSans}",
                  "\\setmonofont[Path = fonts/,UprightFont = *-Regular,BoldFont=*-Medium,Scale=0.92]{FiraMono}",
                  paste0("\\definecolor{titcol}{HTML}{",substr(input$titcol,2,7),"}"),
                  paste0("\\definecolor{orncol}{HTML}{",substr(input$orncol,2,7),"}"),
                  paste0("\\definecolor{iconcol}{HTML}{",substr(input$iconcol,2,7),"}"),
                  paste0("\\definecolor{icontxtcol}{HTML}{",substr(input$icontxtcol,2,7),"}"),
                  paste0("\\definecolor{bullcol}{HTML}{",substr(input$bullcol,2,7),"}"),
                  paste0("\\definecolor{numcol}{HTML}{",substr(input$numcol,2,7),"}"),
                  paste0("\\definecolor{acccol}{HTML}{",substr(input$acccol,2,7),"}"),
                  paste0("\\definecolor{txtcol}{HTML}{",substr(input$txtcol,2,7),"}"),
                  "\\RequirePackage{fontawesome}",
                  "\\RequirePackage{shapepar}",
                  "\\renewcommand{\\rcAuthorSymbol}{\\faPencil}",
                  "\\renewcommand{\\rcClockSymbol}{\\faClockO}",
                  "\\renewcommand{\\rcServingSymbol}{\\faCutlery}",
                  "\\setlist*[itemize]{label=\\faAngleDoubleRight,font={\\color{bullcol}}}",
                  "\\setlist*[enumerate]{font=\\color{numcol}}",
                  "\\setlist*[description]{font=\\color{icontxtcol}}",
                  "\\titleformat{\\section}{\\color{titcol}\\sffamily\\LARGE}{\\thesection}{0pt}{}[\\vskip-1.25em{\\color{orncol}\\rule{\\textwidth}{.2ex}}\\vskip-1em]",
                  "\\titleformat*{\\subsection}{\\color{titcol}\\sffamily\\large\\itshape}",
                  "\\RequirePackage{ragged2e}",
                  "\\renewcommand{\\maketitle}{",
                  "{\\color{titcol}\\raggedright\\Huge\\sffamily\\bfseries\\@title\\par}{\\color{orncol}\\rule{\\textwidth}{0.25ex}}",
                  "\\bigskip",
                  "\\vfill",
                  "{\\raggedright\\Large\\sffamily\\@author\\par}",
                  "{\\raggedright\\large\\sffamily\\@date\\par}",
                  "\\thispagestyle{empty}",
                  "\\setcounter{page}{0}",
                  "}",
                  "\\RequirePackage{etoolbox}",
                  "\\AtBeginEnvironment{quotation}{{\\huge\\color{iconcol}\\faQuoteLeft}\\vspace*{-1.5\\baselineskip}\\itshape}"),
                collapse="\n")
        } else if(input$stylechoice=="Classic"){
            stytxt <- paste(
                c("\\RequirePackage{fontspec}",
                  "\\setsansfont[Path = fonts/]{Anaktoria}",
                  "\\setmainfont[Path = fonts/,UprightFont = *-Regular,BoldFont = *-Bold,ItalicFont = *-Italic]{CharisSIL}",
                  "\\setmonofont[Path = fonts/,Scale=0.92,UprightFont = *-Regular,BoldFont = *-Bold,ItalicFont = *-Italic]{FantasqueSansMono}",
                  paste0("\\definecolor{titcol}{HTML}{",substr(input$titcol,2,7),"}"),
                  paste0("\\definecolor{orncol}{HTML}{",substr(input$orncol,2,7),"}"),
                  paste0("\\definecolor{iconcol}{HTML}{",substr(input$iconcol,2,7),"}"),
                  paste0("\\definecolor{icontxtcol}{HTML}{",substr(input$icontxtcol,2,7),"}"),
                  paste0("\\definecolor{bullcol}{HTML}{",substr(input$bullcol,2,7),"}"),
                  paste0("\\definecolor{numcol}{HTML}{",substr(input$numcol,2,7),"}"),
                  paste0("\\definecolor{acccol}{HTML}{",substr(input$acccol,2,7),"}"),
                  paste0("\\definecolor{txtcol}{HTML}{",substr(input$txtcol,2,7),"}"),
                  "\\RequirePackage{pgfornament}",
                  "\\renewcommand{\\rcAuthorSymbol}{\\pgfornament[width=1.5em,ydelta=-0.5em,color=iconcol]{130}}",
                  "\\renewcommand{\\rcClockSymbol}{\\pgfornament[width=1.5em,ydelta=-0.5em,color=iconcol]{126}}",
                  "\\renewcommand{\\rcServingSymbol}{\\pgfornament[width=1.5em,ydelta=-0.5em,color=iconcol]{166}}",
                  "\\setlist*[itemize]{label={\\pgfornament[height=1em,ydelta=-0.25em]{12}},font={\\color{bullcol}}}",
                  "\\setlist*[enumerate]{font=\\color{numcol}}",
                  "\\setlist*[description]{font=\\color{icontxtcol},leftmargin=3em}",
                  "\\titleformat{\\section}{\\centering\\color{titcol}\\sffamily\\LARGE}{\\thesection}{0pt}{}[\\vskip-0.5\\baselineskip{\\pgfornament[color=orncol]{88}}]",
                  "\\titleformat*{\\subsection}{\\color{titcol}\\sffamily\\large}",
                  "\\renewcommand{\\maketitle}{",
                  "{\\color{titcol}\\centering\\Huge\\sffamily\\@title\\par}",
                  "\\bigskip",
                  "{\\centering\\pgfornament[color=orncol]{88}\\par}",
                  "\\vfill",
                  "{\\centering\\pgfornament[color=iconcol]{181}\\par}",
                  "{\\centering\\Large\\sffamily\\@author\\par}",
                  "{\\centering\\large\\sffamily\\@date\\par}",
                  "\\thispagestyle{empty}",
                  "\\setcounter{page}{0}",
                  "}",
                  "\\RequirePackage{etoolbox}",
                  "\\AtBeginEnvironment{quotation}{\\pgfornament[width=3.5em,color=iconcol]{37}\\par\\vspace*{-2.5\\baselineskip}\\itshape}"),
                collapse="\n")
        } else if(input$stylechoice=="Scribbly"){
            stytxt <- paste(
                c("\\RequirePackage{fontspec}",
                  "\\setsansfont[Path = fonts/,Ligatures=TeX,Scale=0.92]{Domestic_Manners}",
                  "\\setmainfont[Path = fonts/,Ligatures=TeX,Scale=0.92]{Domestic_Manners}",
                  "\\setmonofont[Path = fonts/,Scale=0.92]{Domestic_Manners}",
                  paste0("\\definecolor{titcol}{HTML}{",substr(input$titcol,2,7),"}"),
                  paste0("\\definecolor{orncol}{HTML}{",substr(input$orncol,2,7),"}"),
                  paste0("\\definecolor{iconcol}{HTML}{",substr(input$iconcol,2,7),"}"),
                  paste0("\\definecolor{icontxtcol}{HTML}{",substr(input$icontxtcol,2,7),"}"),
                  paste0("\\definecolor{bullcol}{HTML}{",substr(input$bullcol,2,7),"}"),
                  paste0("\\definecolor{numcol}{HTML}{",substr(input$numcol,2,7),"}"),
                  paste0("\\definecolor{acccol}{HTML}{",substr(input$acccol,2,7),"}"),
                  paste0("\\definecolor{txtcol}{HTML}{",substr(input$txtcol,2,7),"}"),
                  "\\RequirePackage{tikz}",
                  "\\usetikzlibrary{decorations}",
                  "\\usetikzlibrary{decorations.pathmorphing}",
                  "\\RequirePackage{fontawesome}",
                  "\\RequirePackage{shapepar}",
                  "\\renewcommand{\\rcAuthorSymbol}{\\faPencil}",
                  "\\renewcommand{\\rcClockSymbol}{\\faClockO}",
                  "\\renewcommand{\\rcServingSymbol}{\\faCutlery}",
                  "\\setlist*[itemize]{label=»,font={\\color{bullcol}}}",
                  "\\setlist*[enumerate]{font=\\color{numcol}}",
                  "\\setlist*[description]{font=\\color{icontxtcol}}",
                  "\\titleformat{\\section}{\\color{titcol}\\sffamily\\LARGE}{\\thesection}{0pt}{}[\\vskip-1em",
                  "{\\color{orncol}\\tikz[decoration={random steps,segment length=1ex}]",
                  "\\draw [decorate,line width=0.2ex](0,0) -- (0.6\\textwidth,0);}\\vskip-1em]",
                  "\\titleformat*{\\subsection}{\\color{titcol}\\sffamily\\large\\itshape}",
                  "\\RequirePackage{ragged2e}",
                  "\\renewcommand{\\maketitle}{",
                  "{\\color{titcol}\\raggedright\\Huge\\sffamily\\bfseries\\@title\\par}",
                  "{\\color{orncol}\\tikz[decoration={random steps,segment length=0.75ex}]",
                  "\\draw [decorate,line width=0.15ex](0,0) -- (\\textwidth,0);}",
                  "\\bigskip",
                  "\\vfill",
                  "{\\raggedright\\Large\\sffamily\\@author\\par}",
                  "{\\raggedright\\large\\sffamily\\@date\\par}",
                  "\\thispagestyle{empty}",
                  "\\setcounter{page}{0}",
                  "}",
                  "\\RequirePackage{etoolbox}",
                  "\\AtBeginEnvironment{quotation}{{\\huge\\color{iconcol}\\faQuoteLeft}\\vspace*{-1.5\\baselineskip}\\itshape}"),
                collapse="\n")
        }
        
        write(stytxt,file=stypath)
        
        
        
        # create and write the dynamic Rnw file that sources the markdown filie
        rnwtxt <- paste(
                    c("% !TeX program = XeLaTeX","\\documentclass{article}","\\usepackage{scrextend}",
                      paste0("\\changefontsizes[25pt]{",input$fontsize,"pt}"),"\\usepackage{simple-recipe}","\\usepackage{textcomp}",
                      paste0("\\recipestyle{",filepref,"}"),"\\usepackage[margin=0.75in,includefoot,bottom=0.3in,footskip=2em]{geometry}",
                      "\\usepackage{multicol}",
                      "\\begin{document}",paste0("\\markdownInput{",strsplit(mdpath,"/")[[1]][2],"}"),"\\end{document}"),
                    collapse="\n")
        
        write(rnwtxt,file=rnwpath)
        
        # create the dynamic markdown file from the text inputs
        mdpath <- paste0(strsplit(rnwpath,".",fixed=T)[[1]][1],".md")
        
        mdvec <- c(paste0("# ",input$name,"\n"),
                   "\\rcAuthorSymbol{} By",paste0(": ",input$auth),"\n",
                   "\\rcClockSymbol{} Ready in",paste0(": ",input$time),"\n",
                   "\\rcServingSymbol{} Serves",paste0(": ",input$serves),"\n",
                   "## Ingredients \n",paste0("\\begin{multicols}{",input$ingred_cols,"} \n"), 
                   parseIngredients(input$ingred),"\n \\end{multicols}",
                   "## Instructions","\n",parseInstructions(input$instruct))
        
        # if there are notes add them
        if(input$notes!=""){
            mdvec <- c(mdvec,"\n","\\vspace{0.6in}","\n",parseNotes(input$notes))
        }
        
        mdtxt <- paste(mdvec,collapse="\n")
        write(mdtxt,file=mdpath)
        
        # knit the pdf 
        blah <- knit2pdf(input=rnwpath,output=texpath, clean = TRUE, compiler = "xelatex")
        
        # show pdf file in the iframe
        output$showpdf <- renderUI({
            tags$iframe(style = "height:1400px; width:100%", src = pdfpath )
        })
        
    })
    
    #####################################################################################################################
    
    # download pdf
    output$downloadPDF <- downloadHandler(
        filename = function() {paste0(input$name,".pdf")},
        content = function(file) { file.copy(paste0("www/",pdfpath), file) }
    )
    
    #####################################################################################################################
    
    # delete all temporary session files on session end
    session$onSessionEnded(function() {
        todelete <- dir(path="www",pattern = strsplit(pdfpath,".",fixed=T)[[1]][1])
        unlink(paste0("www/",todelete),recursive=T )
        })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
