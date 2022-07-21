#perform relative abundance transformation for microbiota data
# code written by Brandie Wagner 04-03-2013

per_transform =  function(mat)
{
  #calculate number of samples
  n_samp <- nrow(mat)

  #calculate number of taxa 
  n_org <- ncol(mat)

  #define matrices
  ra <- matrix (rep(0,n_org*n_samp), n_samp, n_org)
  l_ra <- matrix (rep(0,n_org*n_samp), n_samp, n_org)
  clr <- matrix (rep(0,n_org*n_samp), n_samp, n_org)
  gm <- rep(0,n_samp)
  c <- rep(0,n_samp)
  total <- rep(0,n_samp)
  
  for (i in 1:n_samp) {
	  #calc weight
    total[i] = sum(mat[i,])

  	for (j in 1:n_org) {
	    #transform counts to relative abundance and add small amount and log transform
	    ra[i,j] <- mat[i,j]/total[i]
	  }
  }
  return(ra)
}
