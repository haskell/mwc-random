
# Load CSV with MWC profiling data
mwc.load <- function(nm) {
  q      <- read.csv(nm)
  q      <- q[ grep("mwc", q$Name) , ] # Select only MWC
  q$Name <- sub("mwc/","",q$Name)      # Strip prefix
  return(q)
}

# Plot side by side barplots of two benchmarks
mwc.compare <- function(nm1, nm2) {
  # Load data
  q1 <- mwc.load(nm1)
  q2 <- mwc.load(nm2)
  qq <- rbind(q1$Mean * 1e9, q2$Mean * 1e9)
  # Plot
  barplot( qq,
           legend = c(nm1,nm2),          #
           beside = TRUE,                # Do not stack
           names.arg=q1$Name,            # Labels
           las=2,                        # Vertical labels
           col = c('gray','lightgreen'), # Colors
           space = c(0,0.5)              # Bar spacing
          )
}

# Plot ratios of two benchmarks
mwc.ratios <- function(nm1, nm2) {
  # Load data
  q1      <- mwc.load(nm1)
  q2      <- mwc.load(nm2)
  speedup <- q2$Mean / q1$Mean - 1
  # Plot
  barplot( speedup,
           names.arg = q1$Name,
          las = 2
       )
  abline(h=0)
}
