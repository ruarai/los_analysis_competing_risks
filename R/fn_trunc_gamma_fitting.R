
ll_gamma_h_nr <- function(
  n, r, h_nr,
  i_shape, i_rate)
{
  h_nr * (
    log( 
      pdgamma(
        r - n + 1,
        shape = i_shape,
        rate = i_rate) -
        pdgamma(
          r - n,
          shape = i_shape,
          rate = i_rate)
    ) -
      log(
        pdgamma(
          r + 1,    
          shape = i_shape,
          rate = i_rate
        ))
  )
}

neg_ll_gamma <- function(
  p,
  h_nr_tibble,
  
  fixed_shape = NULL,
  fixed_rate = NULL
) {
  if(length(p) == 2) {
    i_shape <- exp(p[1])
    i_rate <- exp(p[2])
  } else {
    if(!is.null(fixed_shape)){
      i_shape <- fixed_shape
      i_rate <- p[1]
    } else if(!is.null(fixed_rate)) {
      i_shape <- p[1]
      i_rate <- fixed_rate
    }
  }
  
  
  
  ll <- ll_gamma_h_nr(
    h_nr_tibble$n, 
    h_nr_tibble$r, 
    h_nr_tibble$h_nr,
    i_shape, i_rate
  )
  
  return(-sum(ll))
}