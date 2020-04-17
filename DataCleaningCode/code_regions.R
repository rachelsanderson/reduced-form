code_regions <- function(df, state_var){
  
  # Define regions and assign states to regions
  
  northwest = c("WA", "OR", "ID",'MT','WY','NV','UT')
  caiso = c("CA")
  southwest = c('AZ','NM','CO')
  ercot = c("TX")
  spp = c('OK', 'NE', 'SD','KS')
  miso = c('ND','MN','IA','IL','MS','WI','LA','IN', 'MI', 'MO','AR')
  southeast = c('FL','GA','AL','SC','NC','TN')
  pjm = c('OH','KY','VA','WV','DE','PA','MD','NJ')
  nyiso = c('NY')
  iso_ne = c('VT','NH','MA','CT','RI','ME')
  regionList <- list(northwest, caiso, southwest, ercot, spp,
                     miso, southeast,pjm,nyiso,iso_ne)
  
  regions <- data.frame(state = unique(df[state_var]))
  colnames(regions) <- c('state')
  regions$region[is.element(regions$state,northwest)] <- "northwest"
  regions$region[is.element(regions$state,caiso)] <- "caiso"
  regions$region[is.element(regions$state,ercot)] <- "ercot"
  regions$region[is.element(regions$state,spp)] <- "spp"
  regions$region[is.element(regions$state,southwest)] <- "southwest"
  regions$region[is.element(regions$state,miso)] <- "miso"
  regions$region[is.element(regions$state,southeast)] <- "southeast"
  regions$region[is.element(regions$state,pjm)] <- "pjm"
  regions$region[is.element(regions$state,nyiso)] <- "nyiso"
  regions$region[is.element(regions$state,iso_ne)] <- "iso_ne"
  regions$region[is.element(regions$state,c('HI','AK'))] <- 'hawaii + alaska'
  new_df <- df %>% full_join(regions, by= c(state_var = 'state'))
}

code_regions(wind_solar, plant_state)
