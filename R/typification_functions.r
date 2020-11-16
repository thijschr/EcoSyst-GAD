# EcoSyst-GAD typification functions

# Olav Skarpaas and Thijs Christiaan van Son, GEco, NHM, UiO

## Simulate species data
sim.sp <- function(lce.len = 100,      # length of lce
                   n.sp = 10)          # number of species
{
  lce <- 1:lce.len
  nsp <- n.sp
  
  ## Means of species distributions along the LCE
  sp.mn1 <- sort(runif(nsp, 
                       min(lce), 
                       max(lce)))
  sp.mn2 <- sort(runif(nsp,
                       quantile(lce, 0.25),
                       quantile(lce, 0.75)))
  sp.mn3 <- floor(max(lce) / nsp) * (1:nsp)
  
  ## SD of species distributions along the LCE
  sp.sd1 <- rep(max(lce) / 10, nsp) 
  sp.sd2 <- runif(nsp, 
                  max(lce) / 8, 
                  max(lce) / 2)
  sp.sd3 <- runif(nsp,
                  max(lce) / 10, 
                  max(lce) / 3)
  
  ## Simulated abundance along LCE (rows) for each species (cols)
  sp.abun <- mapply(dnorm,
                    mean = sp.mn1,
                    sd = sp.sd1,
                    MoreArgs = list(x = lce))
  sp.abun
}