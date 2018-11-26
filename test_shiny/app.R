


# Example web app plotting Bumpus measurements
library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Histograms of total population for provinces in Cambodia"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          radioButtons(inputId = "radio", label = "",
                       choices = list("Mondul Kiri"  = 7, 
                                      "Ratank Kiri"       = 8,
                                      "Kandal" = 9,
                                      "Battambang"       = 10,
                                      "Kracheh"     = 11),
                       inline   = FALSE, 
                       width    = '800px', 
                       selected = 7)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("provinceplot")
      )
   )
);

# Define server logic required to draw a histogram
server <- function(input, output) {
   dat <- read.csv(file = "Socioeconomic_variables.csv");
   output$provinceplot <- renderPlot({
       n_col <- as.numeric(input$radio);
       par(mar = c(5, 5, 1, 1));
       hist(x = dat[, n_col], main = "", xlab = "Province",
            ylab = "Frequency", cex.lab = 1.5, cex.axis = 1.5, col = "blue");
   })
}

# Run the application 
shinyApp(ui = ui, server = server);