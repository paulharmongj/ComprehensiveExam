#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(shinythemes);library(actuar)

# Define UI for application that draws a histogram
ui <- fluidPage( 
    #shiny Theme
    theme = shinytheme("united"),
    # Application title
    titlePanel("Simulation Study"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mval",
                        "Choose m:",
                        min = 1,
                        max = 50,
                        value = 5)
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
        m <- input$mval #number of trials
        p <- input$pval #probablility of success on each trial
        
        sim.dat <- rztbinom(m, size = 2, prob = p)
        #sim.dat <- rbinom(100, m, p)
        #True Mean
        TM <- m*p/(1-(1-p)^m)
        
        #bootstrap_function: 
        boot_bin <- function(n.boot, data){
            #initializes a matrix to store bootstrapped values
            boot_mean <- rep(0, n.boot)
            for (j in 1:n.boot){
                #samples indices from observed data (a vector)
                bs.index <- sample(1:length(data),n.boot,replace = TRUE)
                bs <- data[bs.index]
                #calculates the estimate we need
                boot_mean[j] <- mean(bs)/(2*m)
            }
            
            return(boot_mean)}
        
        #sample mean
        BIN.mle <- mean(sim.dat)/(2) #gives us the sum of the x_i divided by the number of reps, i.e. xbar
        #create a bootstrapped estimate of the 95% CI
        BIN.sd.boot <- 1000*sd(boot_bin(500, data = sim.dat))
        BIN.alpha <- rgb(200,30,120,alpha = 80, maxColorValue = 255)
        #mean(sim.dat) gives the same output
        
        #zero-truncated binomial estimator (still iffy on this one!)
        x <- sim.dat
        ZBIN.mle <- (-(2*sum(x) +4*m - 4) + sqrt((-2*sum(x)-4*m + 4)^2 - 4*(-2*sum(x) - 4*m + 4)*(2*sum(x) + 2)))/(4*(sum(x) + m - 1))
        
        boot_Zbin <- function(n.boot, data){
            #initializes a matrix to store bootstrapped values
            boot_mean <- rep(0, n.boot)
            for (j in 1:n.boot){
                #samples indices from observed data (a vector)
                bs.index <- sample(1:length(data),n.boot,replace = TRUE)
                x <- data[bs.index]
                #calculates the estimate we need
                boot_mean[j] <-  (-(2*sum(x) +4*m - 4) + sqrt((-2*sum(x)-4*m + 4)^2 - 4*(-2*sum(x) - 4*m + 4)*(2*sum(x) + 2)))/(4*(sum(x) + m - 1))
                
            }
            
            return(boot_mean)}
        ZBIN.var <- 1000*sd(boot_Zbin(500, data = sim.dat)) #var of bootstrapped version
        Z.alpha <- rgb(60,200,100,alpha = 80, maxColorValue = 255)
        
        
        plot(0:1, 0:1, type = "n", main = "Truncated vs. Binomial Simulated Means", xlab = "Probability", ylab = "", yaxt = 'n')
        abline(v = p, col = "blue", lwd = 2, lty = 2)
        abline(v = ZBIN.mle, col = rgb(60,200,100, maxColorValue = 255)) #adds Zero-Truncated Version
        rect(xleft = ZBIN.mle-ZBIN.var, xright = ZBIN.mle + ZBIN.var,ybottom = .5, ytop = 1, col = Z.alpha)
        abline(v = BIN.mle, col = rgb(200,30,120, maxColorValue = 255)) #adds MLE in 
        rect(xleft = BIN.mle-BIN.sd.boot, xright = BIN.mle + BIN.sd.boot,ybottom = 0, ytop = .5, col = BIN.alpha)
        legend('topleft', fill = c(rgb(60,200,100, maxColorValue = 255),rgb(200,30,120, maxColorValue = 255),'blue'), legend = c('Truncated','Binomial','True Prob'), bty = 'n')  
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
