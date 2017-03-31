library(shiny)

library(shinydashboard)
library(xtable)
library(gridExtra)

header <- dashboardHeader(
    title = "Polling Accuracy"
)

body <- dashboardBody(
    fluidRow(
        column(width = 10,
               box(solidHeader = TRUE, plotOutput('plotbinom')),
               box(h3('Law of Probability'),
                h4('The well known binomial distribution gives the expected distribution of voters that will vote for a candidate
                provided everyone who votes has an equal chance of being included the poll. The formula below gives the probability
                that x voters will vote for the candidate where x can be any number of voters from 0 up to the poll size of N voters.
                The binomial distibution for a sample of N voters is as follows:'),
                   withMathJax(),
                   helpText('$$P(x;N,p) = {N \\choose x} p^x (1-p)^{(N-x)}$$'),
                h4('Parameters:'),
                h4('N: the number of voters included in the poll'),
                h4('x: the number of voters in the poll who say they will vote for the candidate'),
                h4('p: the decimal fraction of all voters who will vote for the candidate'),
                h4('(For example, p = 0.5 if half the voters favor the candidate.')                   )
               ),
        column(width = 2, box(width = NULL, title = "Candidate Support Level",
                              numericInput('support', 'Enter level of support for candidate in percent:', 50, min = 1, max = 100, step = 1),
                              tags$br(),
                              h4('Average mean of 100 polls:'),
                              textOutput('text1')
        )
        )   
        ), 
    

    fluidRow(
        column(width = 10, 
               box(solidHeader = TRUE, plotOutput('plot2')), 
               box(solidHeader = TRUE, plotOutput('plottypical'))
        ),
        column(width = 2, box(width = NULL, status = "warning",
                              title = "Take new poll.",
                              h4('Number of Voters'),
                              numericInput('poll_size', 'Select size of poll:', 500, min = 100, max = 1000, step = 100),
                              tags$br(),
                              h3('Generate new poll results.'),
                              numericInput('seedvalue', 'Select integer:', 1, min = 1, max =20, step = 1),
                              h4('Selected integer is seed for pseudo random number sequence.')
        )
        )
        
    ),
    
    fluidRow(
        column(width = 10,
               
               h4('Website automatically generated with R tools by Marcel Merchat.'),
               h4('December 11, 2016')
               
        )
    )
    
)


dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)


