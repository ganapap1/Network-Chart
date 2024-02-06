# Load required Libraries
library(shiny)
library(shinyjs)           # to perform common useful JavaScript operations in Shiny apps
library(ggplot2)           # to draw or take advantage of ggplot functions
library(ggthemes)          # Special ggtheses are there in this eg theme_economist(),theme_stata(): etc
library(DT)                # for using %>% which works as a pipe in R code for filter etc
library(shinyalert)        # for alert message very nice format
library(plotly)            # to prepare interactive graphs/ charts
library(scales)            # date_format: Formatted dates
library(dplyr)             # for data manipulation and filter
library(htmlwidgets)       # to export plot as HTML file
library(shinyWidgets)      # to access all widgets- sliderTextInput 

#Setup-turn off scientific notation like 1e+06
options(scipen=999)
library(igraph)

########################################################################################
#function to display numbers in million 
########################################################################################
fnformatMillion <- function(x){
  return(paste(formatC(as.numeric(x)/1000000, format= 'f', digits=2, big.mark= ','),' M'))
}

########################################################################################
#This function is to format percentage, default 2 digit, but you can specify as 0 or 1 or 2 
########################################################################################
fnpercent <- function(x, digits = 2, format = 'f') {      
  paste0(formatC(x * 100, format = format, digits = digits), '%')
}


ui <- fluidPage(
  id = 'test',  
  
  # Formatting fluidPage with background black and font white
  ############################################################
  tags$style('#test {background-color: #000000;color:#ffffff;}'),         # #where you got:https://stackoverflow.com/questions/58745090/change-background-color-of-fluid-page-in-r-shiny

  column(width=12,
         column(
           width = 6,
           HTML(paste('<h4><b>',"GG Plot Demo using R Shiny App",'</b><h5>')),
           plotOutput('mggplot',width = '100%',height = '350px')
         ),
         actionButton(inputId = 'mClickme',label = 'Click Me!'),
         column(
           width = 6,
           HTML(paste('<h4><b>',"R script",'</b><h5>')),
           tags$head(
             tags$style(
               paste0("#mggplotTXT{color:black; font-size:11px; font-style:bold;overflow-y:scroll;
                                            width: '100%';height: 350px;max-height: 350px; background: #ffffcd;text-align: left;}")
             )
           ),
           uiOutput(outputId = 'mscriptortable')
         )
  )
)

server<-function(input, output, session) {
  
  output$mscriptortable <- renderUI({
    textAreaInput(inputId ='mggplotTXT',label = NULL,width = '100%',height = '350px',value = NULL )
  })

  
observeEvent(input$mClickme,{
pp <- paste("library(igraph)","\n",

"# Create a simple graph with at least 5 nodes inter-connected","\n",
"nodes <- c('Node1', 'Node2', 'Node3', 'Node4', 'Node5')",'\n',
"edges <- data.frame(from = c('Node1', 'Node1', 'Node2', 'Node3', 'Node4'),
                    to = c('Node2', 'Node3', 'Node4', 'Node5', 'Node1'),
                    weight = c(2, 1.5, 3, 2.5, 1),  
                    nodeposition = c(7, -7.5, 6,- 5.5, 7.5),
                    nodedegree = c(45, -7.5, 6,- 5.5, 7.5),
                     nodevalues <- c(10, 15, 20, 25, 30) 
)","\n",
"edges <- data.frame(from = c('Node1', 'Node1', 'Node2', 'Node3', 'Node4'),
                    to = c('Node2', 'Node3', 'Node4', 'Node5', 'Node1'),
                    weight = c(2, 1.5, 3, 2.5, 1),  # Assign weights to edges
                    nodeposition = c(7, -7.5, 6,- 5.5, 7.5),
                    nodedegree = c(45, -7.5, 6,- 5.5, 7.5),
                     nodevalues <- c(10, 15, 20, 25, 30) 
)","\n",
"# Create a graph object","\n",
"graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)","\n",

# Assign numeric values as labels to each node,"\n",
"V(graph)$label <- paste(nodes, '\n', E(graph)$nodevalues)","\n",


"# Plot the network graph with varying edge width","\n",
"plot(graph, main = 'Medical Research Network', vertex.label.cex = 1.5, vertex.size = 30,
     edge.width = E(graph)$weight*4,  # Use edge weight for varying width
     layout = layout.circle, 
     vertex.label.dist =  E(graph)$nodeposition,
     vertex.label.degree =E(graph)$nodedegree ) ","\n")

       updateTextAreaInput(session,inputId = 'mggplotTXT',value = pp )
      output$mggplot <- renderPlot({
        eval(parse(text = input$mggplotTXT))
      })        
      
})    
  
}

shinyApp(ui = ui, server = server)



