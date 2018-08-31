#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

source('./R/simFlow.R')
source('./R/pitph.R')
source('./R/pitphOps.R')
source('./R/queryRiverData.R')
source('./R/GasGen.R')
load(file = './data/flow_data.rda')


# Define server logic required
shinyServer(function(input, output) {
  
  # Static Values
  project = c('Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor', 'McNary', 'John Day', 'Dalles', 'Bonneville')
  project2 = c('Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor', 'McNary', 'John Day', 'Dalles', 'Bonneville')
  project_code = c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON')
  
  
  # Model flow type
  output$flow_value_input <- renderUI({
    
    switch(input$mod_flow,
      'Static' = list(sliderInput(inputId = 'snake_values', label = h4(tags$b('Snake River Inflow:'), ' (Low = 70, Average = 100, High = 130)'), min = 10, max = 250, step = 5, value = 100),
                      sliderInput(inputId = 'columbia_values', label = h4(tags$b('Columbia River Inflow:'), ' (Low = 175, Average = 250, High = 400)'), min = 100, max = 450, step = 10, value = 250)),
      
      'Simulated Year' = radioButtons(inputId = 'ave_flow', label = h4(tags$b('Average Season-wide In-flow:')),
                                    choiceNames = c('Low Flow', 'Average Flow', 'High Flow'),
                                    choiceValues = c('Low', 'Average', 'High'),
                                    selected = 'Average'),
      
      'Observed Year' = sliderInput(inputId = 'obs_flow_year', label = h4(tags$b('Flow Year:'), '(Accessed from DART)'),
                  min = 2006,
                  max = year(Sys.Date()),
                  step = 1,
                  value = year(Sys.Date()), sep = '')
      ) # close switch
    
  })
  
  # Based on model flow type selected generate river flow conditions
  # River Flow Values
  river_dat <- reactive({
      
      switch(input$mod_flow,
             'Static' = tibble(river = rep(c('Snake River', 'Columbia River'), each = 4),
                                     project = project,
                                     project_code = project_code,
                                     date = ymd("20180601"),
                                     flow = rep(c(input$snake_values, input$columbia_values), each = 4)),
             
             'Simulated Year' =     simFlow(river = 'Snake', flow_year = input$ave_flow) %>%  #input$ave_flow
                                  bind_rows(simFlow(river = 'Columbia', flow_year = input$ave_flow)) %>%  #input$ave_flow
                                  select(river = River, date = Day, flow = sim_flow) %>%
                                  mutate(river = paste0(river, " River"),
                                         date = as.Date(date, origin = "2017-01-01")),

             'Observed Year' = map_dfr(.x = project_code,
                                     .f = function(x) {queryRiverData(site = x, year = input$obs_flow_year,
                                                                 start_day = '03/01',
                                                                 end_day = '06/30') %>%
                                         select(project_code = Site, date = Date, flow = Inflow,
                                                obs_spill_prop = Spill_percent, obs_spill_vol = Spill,
                                                obs_dissolved_gas = Dissolved_Gas, obs_tdg = TDG)})
            )
    
  })
  
# get powerhouse spillway passage efficiency
  # PITPH * (1 - PSP) = the proportion going through PH and JBS
  psp_dat <- reactive({
    tibble(project_code = project_code,
           psp = c(input$psp_lwg, input$psp_lgs, input$psp_lmn, input$psp_ihr, 0, 0, 0, 0))
  })
    
  #------------------------------------------------------------------------------  
  # Create spill sliders based on user input; volume or proportion
  # 'LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'
  #------------------------------------------------------------------------------
  
  # Lower Granite
  
  output$lwg_high_spill_slider <- renderUI({
    if(input$lwg_high_spill_value == 'Volume'){
      #sliderInput(inputId = 'spill_lwg', label = "", min = 0, max = 250, step = 10, value = 20)
      sliderInput(inputId = 'high_spill_lwg', label = "", min = 0, max = 250, value = 20)
    } else {
      #sliderInput(inputId = 'spill_lwg', label = "", min = 0, max = 1, step = .05, value = .25)
      sliderInput(inputId = 'high_spill_lwg', label = "", min = 0, max = 1.0, value = .25)
    }
  })
  
  output$lwg_low_spill_slider <- renderUI({
    if(input$lwg_low_spill_value == 'Volume'){
      #sliderInput(inputId = 'spill_lwg', label = "", min = 0, max = 250, step = 10, value = 20)
      sliderInput(inputId = 'low_spill_lwg', label = "", min = 0, max = 250, value = 20)
    } else {
      #sliderInput(inputId = 'spill_lwg', label = "", min = 0, max = 1, step = .05, value = .25)
      sliderInput(inputId = 'low_spill_lwg', label = "", min = 0, max = 1.0, value = .25)
    }
  })
  
  # Little Goose
  output$lgs_high_spill_slider <- renderUI({
    if(input$lgs_high_spill_value == 'Volume'){
      sliderInput(inputId = 'high_spill_lgs', label = "", min = 0, max = 250, value = 30)
    } else {
      sliderInput(inputId = 'high_spill_lgs', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  output$lgs_low_spill_slider <- renderUI({
    if(input$lgs_low_spill_value == 'Volume'){
      sliderInput(inputId = 'low_spill_lgs', label = "", min = 0, max = 250, value = 30)
    } else {
      sliderInput(inputId = 'low_spill_lgs', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  # Lower Monumental - lmn
  
  output$lmn_high_spill_slider <- renderUI({
    if(input$lmn_high_spill_value == 'Volume'){
      sliderInput(inputId = 'high_spill_lmn', label = "", min = 0, max = 250, value = 25)
    } else {
      sliderInput(inputId = 'high_spill_lmn', label = "", min = 0, max = 1, value = .25)
    }
  })
  
  output$lmn_low_spill_slider <- renderUI({
    if(input$lmn_low_spill_value == 'Volume'){
      sliderInput(inputId = 'low_spill_lmn', label = "", min = 0, max = 250, value = 25)
    } else {
      sliderInput(inputId = 'low_spill_lmn', label = "", min = 0, max = 1, value = .25)
    }
  })
  
  
  # Ice Harbor - ihr
  output$ihr_high_spill_slider <- renderUI({
    if(input$ihr_high_spill_value == 'Volume'){
      sliderInput(inputId = 'high_spill_ihr', label = "", min = 0, max = 250, value = 30)
    } else {
      sliderInput(inputId = 'high_spill_ihr', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  output$ihr_low_spill_slider <- renderUI({
    if(input$ihr_low_spill_value == 'Volume'){
      sliderInput(inputId = 'low_spill_ihr', label = "", min = 0, max = 250, value = 30)
    } else {
      sliderInput(inputId = 'low_spill_ihr', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  # McNary - mcn
  output$mcn_high_spill_slider <- renderUI({
    if(input$mcn_high_spill_value == 'Volume'){
      sliderInput(inputId = 'high_spill_mcn', label = "", min = 0, max = 450, value = 125)
    } else {
      sliderInput(inputId = 'high_spill_mcn', label = "", min = 0, max = 1, value = .50)
    }
  })
  
  output$mcn_low_spill_slider <- renderUI({
    if(input$mcn_low_spill_value == 'Volume'){
      sliderInput(inputId = 'low_spill_mcn', label = "", min = 0, max = 450, value = 125)
    } else {
      sliderInput(inputId = 'low_spill_mcn', label = "", min = 0, max = 1, value = .50)
    }
  })
  
  # John Day - jda
  output$jda_high_spill_slider <- renderUI({
    if(input$jda_high_spill_value == 'Volume'){
      sliderInput(inputId = 'high_spill_jda', label = "", min = 0, max = 450, value = 75)
    } else {
      sliderInput(inputId = 'high_spill_jda', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  output$jda_low_spill_slider <- renderUI({
    if(input$jda_low_spill_value == 'Volume'){
      sliderInput(inputId = 'low_spill_jda', label = "", min = 0, max = 450, value = 75)
    } else {
      sliderInput(inputId = 'low_spill_jda', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  # The Dalles - tda
  output$tda_high_spill_slider <- renderUI({
    if(input$tda_high_spill_value == 'Volume'){
      sliderInput(inputId = 'high_spill_tda', label = "", min = 0, max = 450, value = 100)
    } else {
      sliderInput(inputId = 'high_spill_tda', label = "", min = 0, max = 1, value = .40)
    }
  })
  
  output$tda_low_spill_slider <- renderUI({
    if(input$tda_low_spill_value == 'Volume'){
      sliderInput(inputId = 'low_spill_tda', label = "", min = 0, max = 450, value = 100)
    } else {
      sliderInput(inputId = 'low_spill_tda', label = "", min = 0, max = 1, value = .40)
    }
  })
  
  # Bonneville - bon
  output$bon_high_spill_slider <- renderUI({
    if(input$bon_high_spill_value == 'Volume'){
      sliderInput(inputId = 'high_spill_bon', label = "", min = 0, max = 450, value = 100)
    } else {
      sliderInput(inputId = 'high_spill_bon', label = "", min = 0, max = 1, value = .40)
    }
  })
  
  output$bon_low_spill_slider <- renderUI({
    if(input$bon_low_spill_value == 'Volume'){
      sliderInput(inputId = 'low_spill_bon', label = "", min = 0, max = 450, value = 100)
    } else {
      sliderInput(inputId = 'low_spill_bon', label = "", min = 0, max = 1, value = .40)
    }
  })
  
 
  spill <- reactive({
  tibble(species = rep(input$spp_input, 16),  #input$spp_input
         river = rep(c('Snake River', 'Columbia River'), each = 8),
         project = rep(project, each = 2), project_code = rep(project_code, each = 2),
         period = rep(c(16, 8), 8),
         spill = as.numeric(c(input$high_spill_lwg, input$low_spill_lwg,
                        input$high_spill_lgs, input$low_spill_lgs,
                        input$high_spill_lmn, input$low_spill_lmn,
                        input$high_spill_ihr, input$low_spill_ihr,
                        input$high_spill_mcn, input$low_spill_mcn,
                        input$high_spill_jda, input$low_spill_jda,
                        input$high_spill_tda, input$low_spill_tda,
                        input$high_spill_bon, input$low_spill_bon))
      )
  })
  
  #------------------------------------------------------------------------------
  # Season-wide PITPH estimates
  #------------------------------------------------------------------------------

  
  
  dat <- reactive({
   
    switch(input$mod_flow,
           'Static' = full_join(river_dat(), spill(), by = c('river', 'project', 'project_code')) %>%
             full_join(psp_dat()) %>%
             select(species, everything()) %>%
             pitphOps() %>%
             select(-spill) %>%
             rowwise() %>%
             mutate(estPITPH = pitph(species = species, project = project_code, flow = flow, spill = actual_spill_prop),
                    pspPITPH = estPITPH *(1-psp),
                    w_estPITPH = (pspPITPH*period/24),
                    w_estSPE = 1 - w_estPITPH,
                    TDGmon = zTDGMON(site = project_code, FBgas = 100, spill = actual_spill_prop, flow = flow),
                    TDGspill = zTDGSpill(site = project_code, spill = actual_spill_prop, flow = flow)) %>%
             ungroup(),
           
           'Simulated Year' = full_join(river_dat(), spill(), by = 'river') %>%
             full_join(psp_dat(), by = 'project_code') %>%
             select(species, river, project, project_code, date, flow, period, psp, spill) %>%
             pitphOps() %>%
             select(-spill) %>%
             rowwise() %>%
             mutate(estPITPH = pitph(species = species, project = project_code, flow = flow, spill = actual_spill_prop),
                    pspPITPH = estPITPH *(1-psp),
                    w_estPITPH = (pspPITPH*period/24),
                    w_estSPE = 1 - w_estPITPH) %>%
             ungroup(),
           
           'Observed Year' = full_join(river_dat(), spill(), by = 'project_code') %>%
             full_join(psp_dat()) %>%
             select(species, river, project, project_code, date, flow, period, psp, obs_spill_prop, obs_spill_vol,
                    obs_dissolved_gas, obs_tdg, spill) %>%
             pitphOps() %>%
             select(-spill) %>%
             rowwise() %>%
             mutate(estPITPH = pitph(species = species, project = project_code, flow = flow, spill = actual_spill_prop),
                    pspPITPH = estPITPH *(1-psp),
                    w_estPITPH = (pspPITPH*period/24),
                    w_estSPE = 1 - w_estPITPH) %>%
             ungroup()
           ) 
  })
  
  output$param_dat_table <- DT::renderDT({
    DT::datatable(dat() %>%
      mutate(date = str_sub(as.character(date),start = 6)),
      options = list(pageLength = 16))
    })

# Modeled flow figure
  output$flow_plot <- renderPlot({#renderPlot({
  
   switch(input$mod_flow,
   
    "Static" = dat() %>%
      mutate(project = fct_inorder(project),
             river = fct_inorder(river),
             period = paste0(period, " - Hour Period")) %>%
      ggplot(aes(x = project, y =flow)) +
      geom_point(aes(colour = project, size = w_estPITPH, shape = period), position = position_dodge(width = .2)) +
      scale_colour_viridis_d() +
      facet_wrap(~river, ncol = 2, scales = "free") +
      theme_bw() +
      labs(x = 'Project',
           y = 'Inflow (kcfs)'),
                 
    "Simulated Year" = dat() %>%
      mutate(project = fct_inorder(project),
             period = paste0(period, " - Hour Period")) %>%
      ggplot(aes(x = date, y = flow)) +
      geom_line(aes(colour = project)) +
      geom_point(aes(colour = project, size = w_estPITPH)) +
      scale_x_date(date_breaks = '2 weeks', date_labels = format("%d-%m")) +
      scale_colour_viridis_d() +
      facet_grid(project~period, scales = 'free') +
      theme_bw()+
      labs(x = 'Date',
           y = 'Inflow (kcfs)'),
    
    "Observed Year" = dat() %>%
      mutate(project = fct_inorder(project),
             period = paste0(period, " - Hour Period")) %>%
      ggplot(aes(x = date, y = flow)) +
      geom_line(aes(colour = project)) +
      geom_point(aes(colour = project, size = w_estPITPH)) +
      scale_x_date(date_breaks = '2 weeks', date_labels = format("%d-%m")) +
      scale_colour_viridis_d() +
      facet_grid(project~period) +
      theme_bw()+
      labs(x = 'Date',
           y = 'Inflow (kcfs)') 
   )
    
  })
  
  output$sized_flow <- renderUI({
    
    switch(input$mod_flow,
    "Static" = plotOutput("flow_plot"),
    "Simulated Year" = plotOutput("flow_plot", height = 800),
    "Observed Year" = plotOutput("flow_plot"), height = 800)
  }) 
   
  output$project_table <- renderTable({ 
    
    dat() %>%
      mutate(project = factor(project, levels=project2)) %>%
      group_by(project, date) %>%
      summarise(PITPH = sum(w_estPITPH, na.rm = TRUE),
                SPE = 1 - PITPH, na.rm = TRUE) %>%
      ungroup() %>%
      group_by(project) %>%
      summarise(PITPH = mean(PITPH, na.rm = TRUE),
                SPE = mean(SPE, na.rm = TRUE)) %>%
      rename(Project = project) %>%
      arrange(Project)

  })
  
  output$system_table <- renderTable({ 
    
    dat() %>%
      mutate(project = factor(project, levels=project2)) %>%
      group_by(project, date) %>%
      summarise(PITPH = sum(w_estPITPH, na.rm = TRUE),
                SPE = 1 - PITPH, na.rm = TRUE) %>%
      ungroup() %>%
      group_by(project) %>%
      summarise(PITPH = mean(PITPH, na.rm = TRUE),
                SPE = mean(SPE, na.rm = TRUE)) %>%
      ungroup() %>%
      summarise(`Total PITPH` = sum(PITPH),
                `Average SPE` = mean(SPE))
  })
  


  

  
})
