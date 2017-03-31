library(shinydashboard)
library(dplyr)
library(ggplot2)
library(xtable)
library(gridExtra)

# library(MASS)
# data()
# 
# data(diamonds)
# 
# head(diamonds)

# The binomial distribution

# Argument 1: n
# number of observations
# (The number of times the poll was taken)

# Argument 2: size
# The number of trials for each observation (zero or more). 
# (the number of persons included in the poll.)

# Argument 3: probability
# The probability that a person will vote for a particular candidate 
# (the number of persons included in the poll.)

get_axis_parameters <- function(prob){
    prob <- 100 * prob
    if(prob > 98) {
        max <- 100
        min <-  95
    } else if (prob > 96){
        max <- 100
        min <-  90
    } else if (prob > 94){
        max <- 100
        min <-  85
    } else if (prob > 91){
        max <- 100
        min <-  80
    } else if (prob >= 88){
        max <- 100
        min <-  75
    } else if (prob >= 85){
        max <- 100
        min <-  70
    } else if (prob >= 80){
        max <- 100
        min <-  70
    } else if (prob >= 75){
        max <- 100
        min <-  60
    } else if (prob >= 70){
        max <- 95
        min <-  55
    } else if (prob >= 65){
        max <- 90
        min <-  50
    } else if (prob >= 60){
        max <- 85 
        min <- 45 
    } else if (prob >= 55){
        max <- 80 
        min <-  40
    } else if (prob > 50){
        max <- 75 
        min <-  35
    } else if (prob >= 45){
        max <- 70 
        min <-  30
    } else if (prob >= 40){
        max <- 65 
        min <-  25
    } else if (prob >= 35){
        max <- 60 
        min <-  20
    } else if (prob >= 30){
        max <- 55
        min <-  15
    } else if (prob >= 25){
        max <- 50
        min <-  10
    } else if (prob >= 20){
        max <- 45
        min <-  5
    } else if (prob >= 15){
        max <- 40
        min <-  0
    } else if (prob > 12){
        max <- 30 
        min <-  0
    } else if (prob > 9){
        max <- 25 
        min <-  0
    } else if (prob > 6){
        max <- 20
        min <-  0
    } else if (prob > 4){
        max <- 15
        min <-  0
    } else if (prob > 1){
        max <- 10
        min <-  0
    } else {
        max <- 5
        min <-  0
    } 
        
    # }
    # max <- 10 * ceiling((1.2 * prob)/10)
    # min <-  10 * floor((0.8 * prob)/10)
    
    c(min,max)
}

shinyServer(
  
  function(input, output) {
    
    plot_typical <- reactive({ 
      pollsize <- input$poll_size
      support <- input$support / 100
      seedval <- input$seedvalue
      number_of_polls <- 5
      x <- number_of_polls
      pollster <- c("A","B","C","D","E")
      set.seed(seedval)
      poll_results <- 100 * rbinom(x,pollsize,support) / pollsize
 
      poll_data <- data.frame(pollster, poll_results)

      ggplot(poll_data, aes(pollster,poll_results)) +
            geom_point(size=5,color="darkgreen") +
            ggtitle("Typical Polling Results") +
            labs(x="Pollster", y="Percent (%)") + # , title="Poll Results", subtitle="") +
            
            theme(
      plot.title=element_text(size=20),
      axis.text = element_text(size = rel(1.2)),
      axis.title.x= element_text(size = rel(1.8)), 
      axis.title.y= element_text(size = rel(1.8), margin=margin(0,10,10,0)), 
      panel.background = element_rect(fill = "lightblue",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white") 
    )
  
})
    
    y <- reactive({ 
        seedval <- input$seedvalue
        pollsize <- input$poll_size
        support <- input$support / 100
        Any_Candidate <- rep("Any_Candidate", 100)
        number_of_polls <- 100
        x <- number_of_polls
        set.seed(seedval)
        polls100 <- 100 * rbinom(x,pollsize,support) /pollsize
        probmin <- rep(get_axis_parameters(support)[1],100)
        probmax <-  rep(get_axis_parameters(support)[2],100)
        poll_data100 <- data.frame(Any_Candidate, polls100,probmin,probmax)

        ggplot(poll_data100, aes(polls100)) +
          geom_histogram(binwidth=1,color="darkgreen") +
          ggtitle("Random Sample of 100 Polls") +
          labs(x="Percent (%)", y="Frequency") +
          coord_cartesian(xlim = c(poll_data100[1,"probmin"], poll_data100[1,"probmax"])) +
          theme(
                plot.title=element_text(size=20),
                axis.text = element_text(size = rel(1.2)),
                axis.title.x= element_text(size = rel(1.8)), 
                axis.title.y= element_text(size = rel(1.8), margin=margin(0,10,10,0)), 
                panel.background = element_rect(fill = "lightblue",
                                                colour = "lightblue",
                                                size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                colour = "white"), 
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                colour = "white") 
          )
  })
    
    z1 <<- 0
    z <- reactive({ 
        
        # Consider the results of 100 random polls for a candidate.
        Any_Candidate <- rep("Any_Candidate", 100)
        seedval <- input$seedvalue
        pollsize <- input$poll_size
        support <- input$support / 100
        number_of_polls <- 100
        x <- number_of_polls
        set.seed(seedval)
        polls100 <- 100 * rbinom(x,pollsize,support) / pollsize
        summ <- summary(polls100)
        
        values <- c(summ[1], summ[2], summ[3], summ[4], summ[5], summ[6])
        z1 <<- summ[1]
        summdf <- data.frame(x=values)
        summdf <- t(summdf)

        tworows <- rbind(summdf,summdf)
        summ[4]

    })
    
binom_dist <- reactive({ 

##  dbinom this time
    p <- 0.5
    pollsize <- input$poll_size
    support <- input$support / 100
    number_of_polls <- 100
    support_vec <- c(0:pollsize) 
    vec_len <- length(support_vec)
##  x is the number of voters that will vote for a given candidate
##  The binomial distribution gives the probablity for each possible value of x

##  choose(N, n)* p^n   *   ((1-p)^(N-n))
   
    prob <- 100 * dbinom(support_vec, pollsize, support) 
    probmin <- rep(get_axis_parameters(support)[1], vec_len)
    probmax <-  rep(get_axis_parameters(support)[2], vec_len)
    xx <- 100*support_vec/pollsize
     
    binom_data <- data.frame(xx,prob,probmin,probmax)

    ggplot(binom_data, aes(xx,prob)) +
            geom_point(size=4,color="darkgreen") +
            ggtitle("Expected Distribution of Polling Results") +
            labs(x="Percentage of Voters", y="Probability (%)") +
            coord_cartesian(xlim = c(binom_data[1,"probmin"], binom_data[1,"probmax"])) +
            theme(
                plot.title=element_text(size=20),
                axis.text = element_text(size = rel(1.2)),
                axis.title.x= element_text(size = rel(1.8)), 
                axis.title.y= element_text(size = rel(1.8), margin=margin(0,10,10,0)), 
                panel.background = element_rect(fill = "lightblue",
                                                colour = "lightblue",
                                                size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                colour = "white"), 
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                colour = "white") 
            )
    })

output$plottypical <- renderPlot({plot_typical()})
output$plot2 <- renderPlot({y()})
output$plotbinom <- renderPlot({binom_dist()})
output$inputseedvalue <- renderPrint({input$seedvalue})
output$text1 <- renderText({z()})

output$ex1 <- renderUI({
    withMathJax(helpText('$$P(x;N,p) = {N \\choose x} p^x (1-p)^{(N-x)}$$'))
})

})
  
  





  
