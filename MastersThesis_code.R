n <- 3
m <- 2
a = ((n + 1)^2) / (m * ((n - m + 2) ^2))
synergy = (1 - a)
p = (((n - m + 1) * ((n + 1)^2)) - (n - (m * synergy)) * ((n - m + 2)^2)) / ((n - m) * (((n + 1)^2) - ((n - m + 2)^2)))

SynProbRelationship <- function(n, m){
  a_lim = ((n + 1)^2) / (m * ((n - m + 2) ^2))
  a_vals <- c()
  p_vals <- c()
  
  a_seq <- seq(0, a_lim, 0.0001)
  for(a in a_seq){
    # synergy = (1 - a)
    # p = (((n - m + 1) * ((n + 1)^2)) - (n - (m * synergy)) * ((n - m + 2)^2)) / ((n - m) * (((n + 1)^2) - ((n - m + 2)^2)))
    p = ((((n + 1)^2) * (n - m + 1)) + (((n - m + 2)^2)*(n - (m * (1 + a))))) / ((n - m) * (((n + 1)^2) + ((n - m + 2)^2)))
    
    print((cbind(a, p)))
    
    if(p <= 1){
      a_vals <- c(a_vals, a)
      p_vals <- c(p_vals, p)
    }
  }
  #print(a_vals)
  
  vals <- cbind(a_vals, p_vals)
  return(vals)
}

vals <- SynProbRelationship(3, 2)
#a = ((n + 1)^2) / (m * ((n - m + 2) ^2))
#a_seq <- seq(0, a, 0.1)

SynToeRelationship <- function(n, m, p){
  a_lim = ((n + 1)^2) / (m * ((n - m + 2) ^2))
  a_vals <- c()
  syn_vals <- c()
  t_vals <- c()
  
  a_seq <- seq(0.5, a_lim, 0.0001)
  for(a in a_seq){
    
    t = ((((n + 1)^2) * (n - m - 1)) - (((n - m + 1)^2) * (n - (m * (1+a))))) / ((n-m) * (((2-p)*((n+1)^2)) - (p * ((n - m + 2)^2))))
      
    #print((cbind(a, t)))
    
    syn = 1 - a
    
    a_vals <- c(a_vals, a)
    syn_vals <- c(syn_vals, syn)
    t_vals <- c(t_vals, t)
    
    
  }
  #print(a_vals)
  
  vals <- cbind.data.frame(syn_vals, t_vals)
  return(vals)
}

toevals_0p <- SynToeRelationship(3, 2, 0)
toevals_10p <- SynToeRelationship(3, 2, 0.1)
toevals_50p <- SynToeRelationship(3, 2, 0.5)
toevals_90p <- SynToeRelationship(3, 2, 0.9)
toevals_100p <- SynToeRelationship(3, 2, 1)


mins <- min(min(toevals_0p$t_vals), min(toevals_10p$t_vals), min(toevals_50p$t_vals), min(toevals_90p$t_vals), min(toevals_100p$t_vals))
maxs <- max(max(toevals_0p$t_vals), max(toevals_10p$t_vals), max(toevals_50p$t_vals), max(toevals_90p$t_vals), max(toevals_100p$t_vals))


plot(toevals_0p$syn_vals, toevals_0p$t_vals, type = "l", ylim = c(mins, maxs), xlab = "Synergy Level (1 - a)", ylab = "Toehold Fraction (t)",  col = "darkred", main = "Merger of 2 out of 3 firms")
legend("topright", legend = c("p = 100% (Fully Informed)", "p = 90%", "p = 50%", "p = 10%", "p = 0% (Silent)"),col = c("black", "magenta", "purple", "blue", "darkred"), lty = 1)
lines(toevals_10p$syn_vals, toevals_10p$t_vals, col = "blue")
lines(toevals_50p$syn_vals, toevals_50p$t_vals, col = "purple")
lines(toevals_90p$syn_vals, toevals_90p$t_vals, col = "magenta")
lines(toevals_100p$syn_vals, toevals_100p$t_vals)



