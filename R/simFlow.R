#------------------------------------------------------------------------------
# This function generates random noise around modeled Snake and Columbia river
# flows.  Modeled inflows were collected from the LWG and BON from 2006-2018.
# The function requires the output of 02_summarise_hydro_data script.
#
# In the future may need to load data directly in function arguements.
#
# Author: Ryan N. Kinzer
# Created on: 7 July 2018
#------------------------------------------------------------------------------

simFlow <- function(river = c('Snake', 'Columbia'), flow_year = c('Low', 'Average', 'High')){
  
  load('./data/sim_flow_data.rda')
  
  river <- match.arg(river)
  river <- enquo(river)
  
  flow_year <- match.arg(flow_year)
  flow_year <- enquo(flow_year)
  
  tmp_sd <- sd_flow_dat %>%
    filter(River == !!river,
           flow_grp == !!flow_year) %>%
    pull(sd_diff)

  tmp_flow <- sim_flow_dat %>%
    filter(River == !!river,
           flow_grp == !!flow_year) %>%
    mutate(sim_flow = fit + rnorm(n(),0,tmp_sd)) #47
  
  # tmp_flow <- sim_flow_dat %>%
  #   filter(River == !!river,
  #          flow_grp == !!flow_year) %>%
  #   mutate(sim_flow = abs(fit + cumsum(rnorm(n(),0,tmp_sd)))) #47
  
  #plot(tmp_flow$Day, tmp_flow$sim_flow,type ='l')
  #lines(tmp_flow$Day, tmp_flow$fit)
  # 
  # tmp_start <- sd_flow_dat %>%
  #   filter(River == !!river,
  #          flow_grp == !!flow_year) %>%
  #   pull(mu_start)
  # 
  # tmp_flow <- tibble(River = rep(!!river, 122),
  #                    flow_grp = rep(!!flow_year, 122),
  #                    Day = 60:181,
  #                    start = c(tmp_start, rep(0, 121)),
  #                    noise = rnorm(122, 0, tmp_sd),
  #                    sim_flow = cumsum(start + noise))
  # 
  # tmp_flow <- sim_flow_dat %>%
  #    filter(River == !!river,
  #           flow_grp == !!flow_year) %>%
  #   select(River:fit)
  
  
  return(tmp_flow %>%
           select(River, flow_grp, Day, fit, sim_flow))
  
}


# ggplot(tmp_flow) +
#   geom_line(aes(x = Day, y = sim_flow))
