#' Fellegi Sunter
#'
#' @param full.data
#'
#' @param initial.m
#'
#' @param initial.u
#'
#' @param initial.p
#'
#' @param converge.at
#'
#' @param verbose
#'
#' @return Blah blah
#'
#' @export
FellegiSunter <- function(full.data,
                          initial.m=NULL,
                          initial.u=NULL,
                          initial.p=NULL,
                          converge.at=NULL,
                          verbose=FALSE){
  ##############
  # Initialize
  ##############

  if(is.null(initial.m)){
    initial.m <- runif(ncol(full.data), min=.9, max=1)
  } else{
    initial.m <- initial.m
  }
  if(is.null(initial.u)){
    initial.u <- runif(ncol(full.data), min=0, max=.49)
  } else{
    initial.u <- initial.u
  }
  if(is.null(initial.p)){
    initial.p <- runif(1, 0, .5)
  } else{
    initial.p <- initial.p
  }

  if(is.null(converge.at)){
    converge.at <- 1e-05
  } else{
    converge.at <- converge.at
  }

  unique.data <- plyr::count(full.data)
  tau.data <- unique.data[, -ncol(unique.data)]
  n.j <- matrix(unique.data$freq, nrow=1)
  if(verbose) print(dim(tau.data))

  # rows are j=1...j
  tau.rows <- nrow(tau.data)
  # cols are k=1...k
  tau.cols <- ncol(tau.data)

  # turn into matrix
  tau.data <- as.matrix(tau.data)

  # repeat until we converge
  repeat{
    #######
    # E
    #######

    tau.given.m <-apply(t(apply(tau.data, 1, function(x) initial.m^x )) *
                          t(apply(1-tau.data, 1, function(x) (1-initial.m)^x )) ,
                        1, prod)


    tau.given.u <-apply(t(apply(tau.data, 1, function(x) initial.u^x )) *
                          t(apply(1-tau.data, 1, function(x) (1-initial.u)^x )) ,
                        1, prod)



    # if(verbose) print(c(tau.given.m[ 1:10], tau.given.u[ 1:10]))



    m.given.tau <- (initial.p*tau.given.m) / #jx1
      ((initial.p*tau.given.m) + ((1-initial.p) *tau.given.u))
    u.given.tau <- ((1-initial.p) * tau.given.u) / #jx1
      ((initial.p*tau.given.m) + ((1-initial.p) *tau.given.u))

    # if(verbose) print(c(m.given.tau[ 1:10], u.given.tau[ 1:10]))

    #######
    # M
    #######
    estep.mj <- matrix(m.given.tau, nrow=1) #1xj
    estep.uj <- matrix(u.given.tau, nrow=1) #1xj

    if(verbose) print(estep.mj + estep.uj)

    # if(verbose) print(c(dim(estep.mj), dim(estep.uj), dim(tau.data)))

    m.k <- as.vector( ((n.j * estep.mj) %*% tau.data) / sum(n.j * estep.mj)) #kx1
    u.k <- as.vector( ((n.j * estep.uj) %*% tau.data) / sum(n.j * estep.uj)) #kx1
    p.hat <- as.vector(sum(n.j * estep.mj) / sum(n.j)) #1x1

    #######################
    # Check for convergence
    #######################

    if(verbose) print(c(initial.m, m.k, initial.u, u.k, initial.p, p.hat))

    if(all(abs(initial.m - m.k) < converge.at) &
       all(abs(initial.u - u.k) < converge.at) &
       (abs(initial.p - p.hat) < converge.at)){
      break
    }
    # If it hasn't converged, update and repeat
    initial.m <- m.k
    initial.u <- u.k
    initial.p <- p.hat
  }

  # Now that we have estimates for m, u, p we can
  # calculate the ratio for each vector

  ############
  # Get Ratios
  ############

  M <- apply(t(apply(tau.data, 1, function(x) m.k^x )) *
               t(apply(1-tau.data, 1, function(x) (1-m.k)^x )) ,
             1, prod)


  U <- apply(t(apply(tau.data, 1, function(x) u.k^x )) *
               t(apply(1-tau.data, 1, function(x) (1-u.k)^x )) ,
             1, prod)


  unique.indentifier <- apply(full.data, 1, function(x){
    return(which(apply(tau.data, 1, function(y) return(all(y == x)) )))
  })

  unique.ratios <- M/U
  unique.ratios.log2 <- log2(unique.ratios)


  results <- list(tau.data=tau.data,
                  unique.ratios=unique.ratios,
                  ratios = unique.ratios[unique.indentifier],
                  log2.ratios = unique.ratios.log2[unique.indentifier],
                  final.m = m.k,
                  final.u = u.k,
                  final.p = p.hat)
  return(results)
}

