# EcoSyst-GAD typification functions

# Olav Skarpaas and Thijs Christiaan van Son, GEco, NHM, UiO

## Simulate species data
sim.sp <- function(lcelen = 100,      # length of lce
                   numsp = 10)          # number of species
{
  lce <- 1:lcelen
  nsp <- numsp
  
  ## Means of species distributions along the LCE
  sp_mn1 <- sort(runif(nsp, 
                       min(lce), 
                       max(lce)))
  sp_mn2 <- sort(runif(nsp,
                       quantile(lce, 0.25),
                       quantile(lce, 0.75)))
  sp_mn3 <- floor(max(lce) / nsp) * (1:nsp)
  
  ## SD of species distributions along the LCE
  sp_sd1 <- rep(max(lce) / 10, nsp) 
  sp_sd2 <- runif(nsp, 
                  max(lce) / 8, 
                  max(lce) / 2)
  sp_sd3 <- runif(nsp,
                  max(lce) / 10, 
                  max(lce) / 3)
  
  ## Simulated abundance along LCE (rows) for each species (cols)
  sp_abun <- mapply(dnorm,
                    mean = sp_mn1,
                    sd = sp_sd1,
                    MoreArgs = list(x = lce))
  sp_abun
}


## Plot simulated species
plt.sim.sp <- function(n.sp = 10,      # Number of simulated species
                       sim.abun)        # Simulated abundances
{
  ## Defining species colours
  sp.col <- tim.colors(n.sp)
  par(mfrow = c(1, 1))
  par(mar = c(2, 2, 1, 1),
      mgp = c(0.5, 0.5, 0))
  plot()
}

sp.col <- tim.colors(n.sp)
par(mfrow=c(1,1))
par(mar=c(2,2,1,1),mgp=c(0.5,0.5,0))
plot(y1[,1],type="l",bty="l",xaxt="n",yaxt="n",ylim=c(0,1.4*max(y1)),col=sp.col[1],
     xlab="Key source of variation (complex-variable)",ylab="Key characteristic")
for(i in 2:n.sp) lines(y1[,i],col=sp.col[i])