library(shiny)
library(ggbiplot)
library(ca)
library(shinyjs)

data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

ir_data <- iris[-5]
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

draw_ca_plot <- function(center){
  km <- kmeans(ir_data, centers = center, nstart = 10)
  data_ca <- cbind(iris,km$cluster)
  mytable <- with(data_ca, table(Species,km$cluster)) 
  fit <- ca(mytable)
  plot(fit, mass = TRUE, contrib = "absolute", map =
         "rowgreen", arrows = c(FALSE, TRUE))
}

draw_pca_plot <- function(pc_x,pc_y){
  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,choices = c(pc_x,pc_y), groups = ir.species,ellipse = TRUE)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
  print(g)
}

ui <- tagList(
  useShinyjs(),
  navbarPage("戴士瑋的PCA作業",
             tabPanel("PCA",
                      tabsetPanel(
                        tabPanel("pca plot",
                                 column(3,
                                        tags$h2("PCA"),
                                        tags$h4("x axis"),
                                        actionButton("PC1_x","PC1",class = "btn-primary"),
                                        actionButton("PC2_x","PC2",class = "btn-info"),
                                        actionButton("PC3_x","PC3",class = "btn-success"),
                                        actionButton("PC4_x","PC4",class = "btn-warning"),
                                        
                                        tags$h4("y axis"),
                                        actionButton("PC1_y","PC1",class = "btn-primary"),
                                        actionButton("PC2_y","PC2",class = "btn-info"),
                                        actionButton("PC3_y","PC3",class = "btn-success"),
                                        actionButton("PC4_y","PC4",class = "btn-warning"),
                                        
                                        
                                 ),
                                 column(9,
                                        plotOutput(outputId = 'pca_plot')
                                 )
                                 
                        ),
                        tabPanel("result data",
                                 column(3,
                                        tags$h2("result data")
                                 ),
                                 column(9,
                                        dataTableOutput("result_data")
                                 )
                        ),
                        tabPanel("input data(log)",
                                 column(3,
                                        tags$h2("input data(log)")
                                 ),
                                 column(9,
                                        dataTableOutput("log_data")
                                 ),
                                 
                        ),
                        
                      )
             ),
             tabPanel("CA",fluidRow(
               column(3,
                      h4("CA(use kmeans)"),
                      sliderInput("centers",
                                  "centers(k)",
                                  value = 3,
                                  min = 3,
                                  max = 10,
                                  step = 1)
               ),
               column(9,
                      plotOutput("ca_plot")
               )
             )),
             tabPanel("iris data",fluidRow(
               column(3,
                      tags$h2("iris data"),
                      
                      tags$p("The Iris dataset was used in R.A. Fisher's classic 1936 paper, The Use of Multiple Measurements in Taxonomic Problems, and can also be found on the UCI Machine Learning Repository."),
                      tags$p("It includes three iris species with 50 samples each as well as some properties about each flower. One flower species is linearly separable from the other two, but the other two are not linearly separable from each other."),
                      tags$br(),
                      tags$p("(from ",tags$a(href = "https://www.kaggle.com/datasets/uciml/iris", "Kaggle - iris dataset"),")"),
               ),
               column(9,
                      dataTableOutput("iris")
               )
             ))
             
  )
)

server <- function(input,output){
  
  x <- reactiveVal(1)
  y <- reactiveVal(2)
  
  observeEvent(input$PC1_x,{x(1)})
  observeEvent(input$PC2_x,{x(2)})
  observeEvent(input$PC3_x,{x(3)})
  observeEvent(input$PC4_x,{x(4)})
  observeEvent(input$PC1_y,{y(1)})
  observeEvent(input$PC2_y,{y(2)})
  observeEvent(input$PC3_y,{y(3)})
  observeEvent(input$PC4_y,{y(4)})
  
  observeEvent(x(),{
    show("PC1_y")
    show("PC2_y")
    show("PC3_y")
    show("PC4_y")
    hide(paste0("PC",x(),"_y"))
  })
  
  #點選y使得x按鈕隱藏
  observeEvent(y(),{
    show("PC1_x")
    show("PC2_x")
    show("PC3_x")
    show("PC4_x")
    hide(paste0("PC",y(),"_x"))
  })
  
  #生成pca圖
  output$pca_plot <- renderPlot(draw_pca_plot(x(),y()))
  
  #生成ca圖
  output$ca_plot <- renderPlot(draw_ca_plot(input$centers))
  
  output$result_data <- renderDataTable(ir.pca$x, options = list(pageLength = 25))
  
  output$log_data <- renderDataTable(log.ir, options = list(pageLength = 25))
  output$iris <- renderDataTable(iris, options = list(pageLength = 25))
}
shinyApp(ui = ui,server = server)