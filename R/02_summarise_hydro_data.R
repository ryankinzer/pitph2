#------------------------------------------------------------------------------
# This script summarizes the hydrosystem data gathered after sourcing the 
# 01_gather_hydro_data script.

# Author: Ryan N. Kinzer
# Created on: 7 July 2018
#------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)

load("../pitph_data/hydro_data.rda")

# rearrange flow data into wide format with only inflow and spill metrics.
hydro_wide <- hydro_data %>%
  filter(variable %in% c("Inflow", "Spill", "Spill_percent")) %>%
  spread(key = variable, value = value) %>%
  #mutate(Date = str_sub(Date, 6)) %>%
  select(Year:Date, Day:Spill_percent)

save(hydro_wide, file = './data/flow_data.rda')

hydro_wide%>%
  filter(Year == 2018) %>%
  ggplot() +
  #geom_smooth(aes(x = Date, y = fit), method = 'lm', formula = y ~ poly(x,2)) +
  geom_line(aes(x = Date, y = Inflow)) +
  #scale_x_date(date_breaks = '2 weeks') +
  #geom_line(aes(x = Date, y = fit)) +
  facet_wrap(~Site) +
  theme_bw()+
  labs(x = 'Date',
       y = 'Inflow (kcfs)')


# classify flow years into low, average, high based off flow

flow_data <- hydro_data %>%
  filter(Site %in% c('LWG', 'BON'),
         variable == 'Inflow') %>%
  group_by(Year, Site) %>%
  mutate(River = ifelse(Site == 'LWG', 'Snake', 'Columbia'),
         log_flow = log(value),
         mu = mean(value),
         diff = value - lag(value)) %>%
  ungroup() %>%
  group_by(River) %>%
  mutate(flow_grp = cut(mu, breaks = 3, labels = c("Low", "Average", "High"))) %>%
  ungroup()

#------------------------------------------------------------------------------
# model average flow levels for BON and LWG for each flow group
#------------------------------------------------------------------------------
# visualize the modeled inflow volumes based on flow group
flow_data %>%
  #filter(Year == '2016') %>%
  group_by(River, Year) %>%
  ggplot(aes(x = Day, y = value, colour = flow_grp)) +
  geom_line(aes(group = Year)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
  scale_color_viridis_d() +
  facet_wrap(~River)+
  theme_bw()

# calculate inflow coefficients for each flow group using a quadratic
# regression model similar to above figure
# need to perform model selecting and check assumptions

flow_mod <- lm(value ~ Day*River*flow_grp + River:I(Day^2) + flow_grp:I(Day^2), data = flow_data)
flow_coef <- coef(flow_mod)

# get fitted flow values for each day

day <- seq(yday(ymd(20180301)),yday(ymd(20180630)),by = 1)
River <- c('Columbia', 'Snake')
grp <- c('Low', 'Average', 'High')

newdat <- as.data.frame(expand.grid(River = River, flow_grp = grp, Day = day)) 
sim_flow_dat <- cbind(newdat, predict(flow_mod, newdat, se.fit = TRUE))

# check fitted values

sim_flow_dat %>%
  ggplot(aes(x = Day, y = fit, colour = flow_grp)) +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit,
                  ymax = fit + 1.96 * se.fit),
                  fill = 'grey', alpha = .5) +
  geom_line() +
  scale_color_viridis_d() +
  facet_wrap(~River)+
  theme_bw()

#------------------------------------------------------------------------------
# estimate daily fluctuations in flow for each flow group
#------------------------------------------------------------------------------
# visualize daily differences

flow_data %>%
  group_by(River, Year) %>%
  ggplot(aes(x = Date, y = diff, colour = flow_grp)) +
  geom_line(aes(group = Year)) +
  scale_color_viridis_d() +
  facet_wrap(~Site)+
  theme_bw()

# gather standard deviation of daily differences to generate random noise
sd_flow_dat <- inner_join(flow_data %>%
       group_by(River, flow_grp) %>%
       summarise(sd_diff = sd(diff, na.rm = TRUE)),
flow_data %>%
  group_by(River, Year, flow_grp) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(River, flow_grp) %>%
  summarise(min = min(log_flow),
            max = max(log_flow),
            mu_start = mean(log_flow),
            sd = sd(log_flow))
)
  
save(sim_flow_dat, sd_flow_dat, file = './data/sim_flow_data.rda')
