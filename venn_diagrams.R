#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(VennDiagram)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
   # Application title
   titlePanel("Venn Diagram Tool"),
  
   sidebarLayout(
     sidebarPanel(
   # Sidebar with a slider input for number of bins
   textAreaInput("dataset1","Data set 1", value= " ", rows = 5, placeholder = "Paste your first data set here"),
   textInput("label1", "Enter data set name"), 
   textAreaInput("dataset2","Data set 2", value= " ", rows = 5, placeholder = "Paste your second data set here"),
   textInput("label2", "Enter data set name"), 
   downloadButton("image2", "Download Venn Diagram as image")
      ),
    # Show a plot of the generated distribution
      mainPanel(
        h3("Venn Diagram for 2 data sets"),
        plotOutput("venn2")
      )
  
   ),
   
   tags$hr(),
   
   # Application title
   sidebarLayout(
     sidebarPanel(
       # Sidebar with a slider input for number of bins
       textAreaInput("set1","Data set 1", value= "", rows = 5, placeholder = "Paste your first data set here"),
       textInput("label3", "Enter data set name"), 
       textAreaInput("set2","Data set 2", value= "", rows = 5, placeholder = "Paste your second data set here"),
       textInput("label4", "Enter data set name"), 
       textAreaInput("set3","Data set 3", value= "", rows = 5, placeholder = "Paste your third data set here"),
       textInput("label5", "Enter data set name"), 
       downloadButton("image3", "Download Venn Diagram as Image")
     ),
     # Show a plot of the generated distribution
     mainPanel(
       h3("Venn Diagram for 3 data sets"),
       plotOutput("venn3")
     )
  )
   
)

# Define server logic required
server <- function(input, output) {

#  set1=reactive(as.vector(unlist(strsplit(x = input$dataset1, split = "\n")))))
    a = reactive(unlist(strsplit(x = input$dataset1, split = "\n")))
    b = reactive(unlist(strsplit(x = input$dataset2, split = "\n")))
    ab = reactive(intersect(a(), b()))
    a2 = reactive(unlist(strsplit(x = input$set1, split = "\n")))
    b2 = reactive(unlist(strsplit(x = input$set2, split = "\n")))
    ab2 = reactive(intersect(a2(), b2()))
    d = reactive(unlist(strsplit(x = input$set3, split = "\n")))
    ad = reactive(intersect(a2(), d()))
    bd = reactive(intersect(b2(), d()))
    abd = reactive(intersect(intersect(a2(), b2()), d()))
    e = reactive(unlist(strsplit(x = input$input3, split = "\n")))
    f = reactive(unlist(strsplit(x = input$input4, split = "\n")))
    ae = reactive(intersect(a(), e()))
    af = reactive(intersect(a(), f()))
    be = reactive(intersect(b(), e()))
    bf = reactive(intersect(b(), f()))
    ef = reactive(intersect(e(), f()))
    abe = reactive(intersect(intersect(a(), b()), e()))
    abf = reactive(intersect(intersect(a(), b()), f()))
    aef = reactive(intersect(intersect(a(), e()), f()))
    bef = reactive(intersect(intersect(b(), e()), f()))
    abef = reactive(intersect(intersect(a(), b()), intersect(e(), f())))
    label1 = reactive(input$label1)
    label2 = reactive(input$label2)
    label3 = reactive(input$label3)
    label4 = reactive(input$label4)
    
    label5 = reactive(input$label5)
    output$venn2 = renderPlot({draw.pairwise.venn(length(a()), length(b()), length(ab()), fill = c("green", "blue"), category = c(label1(), label2()), 
                                                  cat.cex = rep(2,2))})
    output$venn3 = renderPlot({draw.triple.venn(length(a2()), length(b2()), length(d()), length(ab2()), length(bd()), length(ad()), length(abd()), 
                                                fill = c("green", "blue", "yellow"), category = c(label3(), label4(), label5()), cat.cex = rep(2,3))})
      output$image2 <- downloadHandler(
      filename = "VennDiagram.png",
      content = function(file) {
        png(file)
        print(draw.pairwise.venn(length(a()), length(b()), length(ab()), fill = c("green", "blue"), category = c(label1(), label2()),cat.cex = rep(2,2)))
        dev.off()
      }
    )
    output$image3 <- downloadHandler(
      filename = "VennDiagram.png",
      content = function(file) {
        png(file)
        print(draw.triple.venn(length(a2()), length(b2()), length(d()), length(ab2()), length(bd()), length(ad()), length(abd()), 
                               fill = c("green", "blue", "yellow"), category = c(label3(), label4(), label5()),cat.cex = rep(2,3)))
        dev.off()
      }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)

