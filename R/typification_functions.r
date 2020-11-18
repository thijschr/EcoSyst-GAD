# EcoSyst-GAD typification functions

# Olav Skarpaas and Thijs Christiaan van Son, GEco, NHM, UiO

## Simulate species data
sim_sp <- function(lcelen = 100,      # length of lce
                   numsp = 10)          # number of species
{
  lce <- 1:lcelen
  nsp <- numsp
  
  ## Means of species distributions along the LCE
  mn <- sort(runif(nsp, 
                   min(lce), 
                   max(lce)))
  
  
  ## SD of species distributions along the LCE
  std <- runif(nsp, 
               max(lce) / 25, 
               max(lce) / 4)
  
  
  ## Simulated abundance along LCE (rows) for each species (cols)
  abun <- mapply(dnorm,
                 mean = mn,
                 sd = std,
                 MoreArgs = list(x = lce))
  
  list(sp_abun = abun,
       sp_mn = mn,
       sp_sd = std)
}

