# Expectation maximization algorithm
em <- function (values, model_healthy, model_type) {
  
  runs <- 10
  
  for(i in 1:runs) {
    
    cat ("Run", i, "of", runs, "\n")
    
    # If a model cannot be found to fit the initial random set of probabilities,
    # search for other random sets of probabilities until one can be found
    
    while (TRUE) {
  
      # Get initial set of probabilities using the healthy model and a random
      # version of the diseased model
      probabilities <- get_initial_probabilities (values, model_healthy, model_type)
    
      # Calculate priors for the healthy and diseased models from the initial
      # set of probabilities      
      priors <- get_priors (probabilities)
      
      # Fit the diseased model using the observed FEV1/FVC values and the initial
      # set of probabilities
      model_diseased <- get_model (values, probabilities [2,], model_type)
     
      # If there was no error in fitting the model, then break out of this loop
      if (!inherits (model_diseased, "try-error")) break
      
    }
    
    difference <- 1
    
    iterations <- 1
    
    while (difference > 0.1 && iterations <= 100) {
      
      cat ("Iteration", iterations, "\n")
      
      #### M step in the EM algorithm ####

      # Calculate priors for the healthy and diseased models from the current
      # set of probabilities
      priors <- get_priors (probabilities)
    
      # Fit the diseased model using the observed FEV1/FVC values and the current
      # set of probabilities
      model_diseased <- get_model (values, probabilities [2,], model_type)
      
      #### E step in the EM algorithm ####

      # Calculate the probabilities of the observed FEV1/FVC values using the
      # healthy and diseased models, and the current set of priors
      probabilities <- get_probabilities (
        values,
        model_healthy,
        model_diseased,
        priors
      )
      
      # Calculate the log likelihood of the observed FEV1/FVC values using the
      # healthy and diseased models and the current set of priors
      log_likelihood <- get_log_likelihood (
        values,
        model_healthy,
        model_diseased,
        priors
      )
    
      if (iterations > 1) difference <- abs (log_likelihood - log_likelihood_old)
        
      log_likelihood_old <- log_likelihood
        
      iterations <- iterations + 1
      
    }
    
    if (i == 1) {
      
      best_model_diseased <- model_diseased
      best_priors <- priors
      best_log_likelihood <- log_likelihood
          
    } else {
      
      if (log_likelihood > best_log_likelihood) {
        
        best_log_likelihood <- log_likelihood
        best_model_diseased <- model_diseased
        best_priors <- priors
        
      }
      
    }
    
  }
  
  bic <- get_bic (
    values,
    model_healthy,
    best_model_diseased,
    best_priors
  )
  
  results <- list (
    "bic" = bic,
    "log_likelihood" = best_log_likelihood,
    "model" = best_model_diseased,
    "priors" = best_priors
  )
  
  return (results)
  
}

get_bic <- function (values, model_healthy, model_diseased, priors) {
  
  n <- length (values)
  
  k <- length (model_diseased$parameters) + 4
  
  L <- get_log_likelihood (values, model_healthy, model_diseased, priors)
  
  bic <- k * log (n) - 2 * L
  
  return (bic)
  
}

get_initial_probabilities <- function (values, model_healthy, type) {
  
  n <- length (values)
  
  probabilities <- matrix (NA, nrow = 2, ncol = n)
  
  sums <- array (NA, dim = n)
  
  prior_healthy <- runif (1, 0.7, 1.0)
  prior_diseased <- 1 - prior_healthy
  
  if (type == "bccg") {
    
    mu_healthy <- get_mu (model_healthy)
    sigma_healthy <- get_sigma (model_healthy)
    nu_healthy <- get_nu (model_healthy)
    
    mu_diseased <- runif (1, 0.4, 0.7)
    sigma_diseased <- runif (1, 0.3, 0.4)
    nu_diseased <- runif (1, 2, 3)

    probabilities [1, ] <- prior_healthy * 
      dBCCG (values, mu_healthy, sigma_healthy, nu_healthy)
    
    probabilities [2, ] <- prior_diseased *
      dBCCG (values, mu_diseased, sigma_diseased, nu_diseased)    
    
  }
  
  if (type == "bcpe") {
    
    mu_healthy <- get_mu (model_healthy)
    sigma_healthy <- get_sigma (model_healthy)
    nu_healthy <- get_nu (model_healthy)
    
    probabilities [1, ] <-  prior_healthy *
      dBCCG (values, mu_healthy, sigma_healthy, nu_healthy)
    
    mu_diseased <- runif (1, 0.4, 0.7)
    sigma_diseased <- runif (1, 0.3, 0.4)
    nu_diseased <- runif (1, 2, 3)
    tau_diseased <- runif (1, 6, 10)
    
    probabilities [2, ] <- prior_diseased *
      dBCPE (values, mu_diseased, sigma_diseased, nu_diseased, tau_diseased)       
    
  }
  
  if (type == "no") {
    
    mu_healthy <- get_mu (model_healthy)
    sigma_healthy <- get_sigma (model_healthy)
    
    mu_diseased <- runif (1, 0.4, 0.7)
    sigma_diseased <- runif (1, 0.05, 0.1)
    
    probabilities [1, ] <- prior_healthy * 
      dNO (values, mu_healthy, sigma_healthy)
    
    probabilities [2, ] <- prior_diseased *
      dNO (values, mu_diseased, sigma_diseased)

  }
  
  for (i in 1:n) {
    
    sums [i] <- sum (probabilities [, i])
    
  }
  
  for (i in 1:2) {
    
    for (j in 1:n) {
      
      probabilities [i, j] <- probabilities [i, j] / sums [j]
      
    }
    
  }
  
  return (probabilities)  
  
}

get_log_likelihood <- function (values, model_healthy, model_diseased, priors) {
  
  n <- length (values)
  
  type <- get_type (model_diseased)
  
  mu_healthy <- get_mu (model_healthy)
  sigma_healthy <- get_sigma (model_healthy)
  nu_healthy <- get_nu (model_healthy)
  
  log_likelihood_healthy <- priors [1] * sum (
    log (dBCCG (values, mu_healthy, sigma_healthy, nu_healthy))
  )
  
  if (type == "bccg") {
    
    mu_diseased <- get_mu (model_diseased)
    sigma_diseased <- get_sigma (model_diseased)
    nu_diseased <- get_nu (model_diseased)
    
    log_likelihood_diseased <- priors [2] * sum (
      log (dBCCG (values, mu_diseased, sigma_diseased, nu_diseased))
    )
    
  }
  
  if (type == "bcpe") {
    
    mu_diseased <- get_mu (model_diseased)
    sigma_diseased <- get_sigma (model_diseased)
    nu_diseased <- get_nu (model_diseased)
    tau_diseased <- get_tau (model_diseased)
    
    log_likelihood_diseased <- priors [2] * sum (
      log (dBCPE (values, mu_diseased, sigma_diseased, nu_diseased, tau_diseased))
    )    
    
  }
  
  if (type == "no") {
    
    mu_diseased <- get_mu (model_diseased)
    sigma_diseased <- get_sigma (model_diseased)
    
    log_likelihood_diseased <- priors [2] * sum (
      log (dNO (values, mu_diseased, sigma_diseased))
    )      
    
  }
  
  log_likelihood <- log_likelihood_healthy + log_likelihood_diseased
  
  return (log_likelihood)
  
}

get_model <- function (values, weights, type) {
  
  data <- tibble (
    values = values,
    weights = weights
  )
  
  if (type == "bccg") {
    
    model <- try (
      
      gamlss (
        values ~ 1,
        mu.start = 0.5,
        family = BCCGo,
        method = RS (100),
        weights = weights,
        data = data,
        trace = FALSE,
      ), silent = TRUE
      
    )
      
  }
  
  if (type == "bcpe") {
    
    model <- try (
      
      gamlss (
        values ~ 1,
        mu.start = 0.5,
        family = BCPEo,
        method = RS (100),
        weights = weights,
        data = data,
        trace = FALSE
      ), silent = TRUE
      
    )
    
  }
    
  if (type == "no") {
    
    model <- try (
      
      gamlss (
        values ~ 1,
        family = NO,
        method = RS (100),
        mu.start = 0.5,
        weights = weights,
        data = data,
        trace = FALSE
      ), silent = TRUE
      
    )

  }

  return (model)
  
}

get_mu <- function (model) {
  
  type = get_type (model)
  
  if (type == "no") {
  
    mu <- model$mu.coefficients [[1]]
      
  } else {
    
    mu <- exp (model$mu.coefficients [[1]])
    
  }
  
  return (mu)
  
}

get_nu <- function (model) {
  
  nu <- model$nu.coefficients [[1]]
  
  return (nu)
  
}

get_priors <- function (probabilities) {
  
  priors <- array (NA, dim = 2)
  
  n <- ncol (probabilities)
  
  priors [1] <- sum (probabilities [1,]) / n
  priors [2] <- sum (probabilities [2,]) / n
  
  return (priors)
  
}

get_probabilities <- function (values, model_healthy, model_diseased, priors) {

  n <- length (values)
  
  type = get_type (model_diseased)
  
  probabilities <- matrix (NA, nrow = 2, ncol = n)
  
  sums <- array (NA, dim = n)
  
  mu_healthy <- get_mu (model_healthy)
  sigma_healthy <- get_sigma (model_healthy)
  nu_healthy <- get_nu (model_healthy)

  probabilities [1,] <- priors [1] * dBCCG (
    values,
    mu_healthy,
    sigma_healthy,
    nu_healthy
  )
  
  if (type == "bccg") {
    
    mu_diseased <- get_mu (model_diseased)
    sigma_diseased <- get_mu (model_diseased)
    nu_diseased <- get_mu (model_diseased)
    
    probabilities [2,] <- priors [2] * dBCCG (
      values,
      mu_diseased,
      sigma_diseased,
      nu_diseased
    )
    
  }
  
  if (type == "bcpe") {
    
    mu_diseased <- get_mu (model_diseased)
    sigma_diseased <- get_mu (model_diseased)
    nu_diseased <- get_mu (model_diseased)
    tau_diseased <- get_tau (model_diseased)
    
    probabilities [2,] <- priors [2] * dBCPE (
      values,
      mu_diseased,
      sigma_diseased,
      nu_diseased,
      tau_diseased
    )    
    
  }
  
  if (type == "no") {
    
    mu_diseased <- get_mu (model_diseased)
    sigma_diseased <- get_mu (model_diseased)
    
    probabilities [2,] <- priors [2] * dNO (
      values,
      mu_diseased,
      sigma_diseased
    )
    
  }
  
  for (i in 1:n) {
    
    sums [i] <- sum (probabilities [, i])
    
  }
  
  for (i in 1:2) {
    
    for (j in 1:n) {
      
      probabilities [i, j] <- probabilities [i, j] / sums [j]
      
    }
    
  }
  
  return (probabilities)

}  
  
get_sigma <- function (model) {
  
  sigma <- exp (model$sigma.coefficients [[1]])
  
  return (sigma)
  
}

get_tau <- function (model) {
  
  tau <- exp (model$tau.coefficients [[1]])
  
  return (tau)
  
}

get_type <- function (model) {
  
  type <- model$family [[1]]
  
  if (type == "BCCGo") return ("bccg")
  if (type == "BCPEo") return ("bcpe")
  if (type == "NO") return ("no")
  
}