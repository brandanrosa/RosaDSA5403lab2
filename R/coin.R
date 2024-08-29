#' coin
#'
#' A function which produces a plot and useful information pertaining to a coin problem in a Bayesian context.
#'
#' @param theta a seq(0,1...) function
#' @param prior "uniform" or "triangle"
#' @param n number of trials
#' @param z number of successes
#' @param alpha alpha level
#'
#' @return a named list containing the posterior probabilities, BCI, Posterior mean, and the Prior Mean. Also a plot of the Prior/Post/Likelihood
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point aes labs ggtitle ylab xlab scale_color_manual
#' @importFrom stats dbinom
#'
#' @examples \dontrun{coin(theta = seq(0,1,length = 101), prior = "uniform", n = 10, z = 4, alpha = 0.05)}
coin <- function(theta, prior, n, z, alpha = 0.05) {# theta=P(H), prior="uh, shape?", n=num.trials, z=num.succ, alpha=alpha

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
