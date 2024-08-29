library(shiny)
library(ggplot2)

coin <- function(theta, prior, n, z, alpha = 0.05) {
  ntheta <- length(theta)
  # Prior
  pr <- switch(prior,
               uniform = {
                 rep(1,ntheta)/ntheta
               },
               triangle = {
                 tr <- pmin(theta, 1-theta)
                 tr/sum(tr)
               }
  )
  ptheta <- pr
  lik <- dbinom(x = z, size = n, prob = theta)
  h <- ptheta * lik
  post <- h / sum(h)

  # Bayes' Box
  baybox <- matrix(c(theta, ptheta, lik, h, post), nrow = ntheta, ncol = 5, byrow = FALSE)
  colnames(baybox) <- c("theta", "prior", "lik", "h", "post")
  cp <- cumsum(post)
  df <- as.data.frame(baybox)

  # BCI
  L <- max(which(cp < alpha/2))
  U <- min(which(cp > 1 - alpha/2))
  BCI <- df$theta[c(L, U)]

  # Bayesian Point Estimate (Posterior Mean)
  postmean <- sum(df$post*theta)
  # Prior Mean
  priormean <- sum(df$prior*theta)
  # Plot
  g <- ggplot(df, aes(x = theta)) +
    geom_point(aes(y = prior, color = 'Prior'), size = 3) +
    geom_point(aes(y = lik, color = 'Likelihood'), size = 3) +
    geom_point(aes(y = post, color = 'Posterior'), size = 3)
  g <- g + ggtitle("Rosa's Plot") +
    labs(alt = "Grid approximation") +
    ylab("Post/Prior/Lik") +
    xlab("Theta")
  g <- g + scale_color_manual(name = 'Bayesian Analysis',
                              breaks = c('Posterior', 'Likelihood', 'Prior'),
                              values = c('Posterior' = 'green',
                                         'Likelihood' = 'blue',
                                         'Prior' = 'red'))
  print(g)
  # Named List
  list(Posterior=post, BCI=BCI, Post.Mean=postmean, Prior.Mean=priormean)
}

myBCI <- function(theta, prior, n, z, alpha = 0.05) {
  ntheta <- length(theta)
  # Prior
  pr <- switch(prior,          # Prior="uniform" or "triangle"
               uniform = {
                 rep(1,ntheta)/ntheta
               },
               triangle = {
                 tr <- pmin(theta, 1-theta)
                 tr/sum(tr)
               }
  )
  ptheta <- pr
  lik <- dbinom(x = z, size = n, prob = theta)
  h <- ptheta * lik
  post <- h / sum(h)
  cp <- cumsum(post)

  L <- max(which(cp < alpha/2))
  U <- min(which(cp > 1 - alpha/2))
  BCI <- theta[c(L, U)]
  BCI
}

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),

    # Application title
    titlePanel("Shiny Coins"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            sliderInput("theta",
                        "Length of Theta:",
                        min = 0,
                        max = 200,
                        value = 101),

            radioButtons(inputId = "prior",
                         label = "Prior Distribution",
                         choices = c("uniform", "triangle")),

            sliderInput("alpha",
                        "Alpha Level",
                        min = 0,
                        max = 1,
                        value = 0.05)
        ),

        # mainpanel
        mainPanel(
           plotOutput("distPlot"),
           tableOutput("tab")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    len <- reactive(input$theta)
    thetaa <- reactive(seq(0,1,length=len()))
    priorr <- reactive(input$prior)
    alphaa <- reactive(input$alpha)

    output$distPlot <- renderPlot({
      coin(theta = thetaa(), prior = priorr(), n = 10, z = 4, alpha = alphaa())
    })

    output$tab <- renderPrint({
      bb <- myBCI(theta = thetaa(), prior = priorr(), n = 10, z = 4, alpha = alphaa())
      cat("BCI = [", bb[1], ",", bb[2], "]")
      })
}

# Run the application
shinyApp(ui = ui, server = server)
