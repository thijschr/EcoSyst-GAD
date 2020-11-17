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
                 max(lce) / 25, 
                 max(lce) / 4)
  
  
  ## Simulated abundance along LCE (rows) for each species (cols)
  sp_abun <- mapply(dnorm,
                    mean = sp_mn,
                    sd = sp_sd,
                    MoreArgs = list(x = lce))
  
  list(sp_abun = sp_abun,
       sp_mn = sp_mn,
       sp_sd = sp_sd)
}

