# EcoSyst-GAD typification functions

# Olav Skarpaas and Thijs Christiaan van Son, GEco, NHM, UiO

## Simulate species data
sim.sp <- function(lcelen = 100,      # length of lce
                   numsp = 10)          # number of species
{
  lce <- 1:lcelen
  nsp <- numsp
  
  ## Means of species distributions along the LCE
  sp_mn <- sort(runif(nsp, 
                      min(lce), 
                      max(lce)))
  
  
  ## SD of species distributions along the LCE
  sp_sd <- runif(nsp, 
                 max(lce) / 20, 
                 max(lce) / 2)
  
  
  ## Simulated abundance along LCE (rows) for each species (cols)
  sp_abun <- mapply(dnorm,
                    mean = sp_mn,
                    sd = sp_sd,
                    MoreArgs = list(x = lce))
  
  list(sp_abun = sp_abun,
       sp_mn = sp_mn,
       sp_sd = sp_sd)
}


## Plot simulated species
plt.sim.sp <- function(nsp = 10,       # Number of simulated species
                       simabun)        # Simulated abundances
{
  ## Defining species colours
  sp.col <- tim.colors(nsp)
  par(mfrow = c(1, 1))
  par(mar = c(2, 2, 1, 1),
      mgp = c(0.5, 0.5, 0))
  plot(simabun,
       type = "n",
       bty = "l",
       xaxt = "n",
       yaxt = "n",
       ylim = c(0, 1.4 * max(simabun)),
       xlab = "Key source of variation (local complex-variable",
       ylab = "Key characteristic")
  for(i in 1:snp) 
  {
    lines(simabun[, i],
          col.sp = sp.col[i])
  }
}

# sp.col <- tim.colors(n.sp)
# par(mfrow=c(1,1))
# par(mar=c(2,2,1,1),mgp=c(0.5,0.5,0))
# plot(y1[,1],type="l",bty="l",xaxt="n",yaxt="n",ylim=c(0,1.4*max(y1)),col=sp.col[1],
#      xlab="Key source of variation (complex-variable)",ylab="Key characteristic")
# for(i in 2:n.sp) lines(y1[,i],col=sp.col[i])