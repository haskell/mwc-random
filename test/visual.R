# Ugly script for displaying distributions alogside with theoretical
# distribution.


view.dumps <- function() {
  load.d <- function(name) read.table(name)[,1]
  plot.d <- function(name, dens, rng) {
    smp <- load.d( name )
    plot( density(smp), xlim=rng, main=name, col='blue', lwd=2)
    hist( smp, probability=TRUE, breaks=100, add=TRUE)
    plot( dens, xlim=rng, col='red', add=TRUE, lwd=2)
  }
  ################################################################
  # Normal
  plot.d ("distr/normal-0-1",
          function(x) dnorm( x, 0, 1 ),
          c(-4,4) )
  readline()
  # 
  plot.d ("distr/normal-1-2",
          function(x) dnorm( x, 1, 2 ),
          c(-6,8) )
  readline();

  ################################################################
  # Gamma
  plot.d ("distr/gamma-1.0-1.0",
          function(x) dgamma( x, 1, 1 ),
          c(-1,8) )
  readline();
  #
  plot.d ("distr/gamma-0.3-0.4",
          function(x) dgamma( x, 0.3, scale=0.4 ),
          c(-0.25,2) )
  readline();
  #
  plot.d ("distr/gamma-0.3-3.0",
          function(x) dgamma( x, 0.3, scale=3.0 ),
          c(-1,5) )
  readline();
  #
  plot.d ("distr/gamma-3.0-0.4",
          function(x) dgamma( x, 3.0, scale=0.4 ),
          c(-1,6) )
  readline();
  #
  plot.d ("distr/gamma-3.0-3.0",
          function(x) dgamma( x, 3.0, scale=3.0 ),
          c(-1,32) )
  readline();
  ################################################################
  # Exponential
  plot.d ("distr/exponential-1",
          function(x) dexp(x,1),
          c(-0.5, 9) )
  readline()
  #
  plot.d ("distr/exponential-3",
          function(x) dexp(x,3),
          c(-0.5, 3) )
  readline()
}
