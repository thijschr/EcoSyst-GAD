# EcoSyst-GAD typification functions

# Olav Skarpaas and Thijs Christiaan van Son, GEco, NHM, UiO

## Simulate species data
sim_sp <- function(lcelen = 100,      # length of lce
                   numsp = 10)        # number of species
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


## Transforming simulated species data to standardised 7-step scale
transf_seven_scale <- function(abund) {
  as.numeric(cut(abund,
                 breaks = c(0, 1/64, 1/16, 1/4, 3/4, 1))) + 
    as.numeric(abund > 3/4 & mean(abund) > 1/8)
}

## Creation of generalised species data from simulated species data
# Simulated species data need to be standardised (values between 0 and 1).
# The standardised value then represent "konstans", which can be translated to standardised abundances
# Then, at each observation point with a candidate type, generate generalised species lists.
gen_sp_lst <- function(simabun) {
  abun_stand <- simabun / max(simabun)
  gad <- apply(X = abun_stand, MARGIN = 2,
               FUN = transf_seven_scale)
}
