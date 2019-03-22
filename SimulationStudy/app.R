#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simulation Study"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mval",
                        "Choose m:",
                        min = 1,
                        max = 50,
                        value = 30)
        ,
        sliderInput("pval",
                    "Choose probability:",
                    min = 0,
                    max = 1,
                    value = .5,
                    step = .1)
        
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        #simulate the data using the actuar package's data
        m <- input$mval
        p <- input$pval
        sim.dat <- rztbinom(100, size = m, prob = p)
        #sim.dat <- rbinom(100, m, p)
        #True Mean
        TM <- m*p/(1-(1-p)^m)
        
        
        #sample mean
        BIN.mle <- mean(sim.dat)/m #gives us the sum of the x_i divided by the number of reps, i.e. xbar
        BIN.wald.sd <- sqrt(BIN.mle *(1-BIN.mle)/2) #gives the wald CI
        BIN.alpha <- rgb(200,30,120,alpha = 80, maxColorValue = 255)
        #mean(sim.dat) gives the same output
        
        #binomial estimator
        ZBIN.mle <- mean(sim.dat)/2 #sample output
        ZBIN.var <- sqrt(ZBIN.mle *(1-ZBIN.mle)/20) #test output
        Z.alpha <- rgb(60,200,100,alpha = 80, maxColorValue = 255)
        
        ## How quickly could I put together a shiny app for this? 
        #     sliders for m, p
        #     show the two estimators
        
        plot(0:2, 0:2, type = "n", main = "Truncated vs. Binomial Simulated Means")
        abline(v = p, col = "blue", lwd = 2)
        abline(v = ZBIN.mle, col = rgb(60,200,100, maxColorValue = 255)) #adds Zero-Truncated Version
        rect(xleft = ZBIN.mle-ZBIN.var, xright = ZBIN.mle + ZBIN.var,ybottom = .5, ytop = 1, col = Z.alpha)
        abline(v = BIN.mle, col = rgb(200,30,120, maxColorValue = 255)) #adds MLE in 
        rect(xleft = BIN.mle-BIN.wald.sd, xright = BIN.mle + BIN.wald.sd,ybottom = 0, ytop = .5, col = BIN.alpha)
        legend('topleft', fill = c(rgb(60,200,100, maxColorValue = 255),rgb(200,30,120, maxColorValue = 255),'blue'), legend = c('Truncated','Binomial','True Prob'), bty = 'n')  
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
