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
#library(plotly)

source('./R/simFlow.R')
source('./R/pitph.R')
source('./R/getSPE.R')
source('./R/mingenOps.R')
source('./R/queryRiverData.R')
#source('./R/GasGen.R')
source('./R/zTDGSpill.R')
source('./R/zTDGMON.R')
load(file = './data/flow_data.rda')


# Define server logic required
shinyServer(function(input, output) {
  
  # Static Values
  #project = c('Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor', 'McNary', 'John Day', 'Dalles', 'Bonneville')
  project2 = c('Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor', 'McNary', 'John Day', 'Dalles', 'Bonneville')
  project_code2 = c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON')
  
  
  # Model flow type
  output$flow_value_input <- renderUI({
    
    switch(input$mod_flow,
      'Static' = list(sliderInput(inputId = 'snake_values', label = h4('Snake River Inflow: (Low = 70, Average = 100, High = 130)'), min = 10, max = 250, step = 5, value = 100),
                      sliderInput(inputId = 'columbia_values', label = h4('Columbia River Inflow: (Low = 175, Average = 250, High = 400)'), min = 100, max = 450, step = 10, value = 250)),
      
      'Simulated Year' = list(radioButtons(inputId = 'ave_flow', label = h4('Average Season-wide In-flow:'),
                                    choiceNames = c('Low Flow', 'Average Flow', 'High Flow'),
                                    choiceValues = c('Low', 'Average', 'High'),
                                    selected = 'Average'),
                              
                              radioButtons(inputId = 'sim_plot',
                                           label = h4('Simulated Year Figure:'),
                                           choiceNames = c('PITPH and TDG Percents', 'Flow and Spill Volumes'),
                                           choiceValues = c('pitph_tdg', 'flow_vol'),
                                           selected = 'flow_vol')
                              ),
      
      'Observed Year' = list(sliderInput(inputId = 'obs_flow_year', label = h4('Flow Year: (Accessed from DART)'),
                  min = 2006,
                  max = year(Sys.Date()),
                  step = 1,
                  value = year(Sys.Date()), sep = ''),
                  
                  radioButtons(inputId = 'sim_plot',
                               label = h4('Observed Year Figure:'),
                               choiceNames = c('Flow and Spill Volumes', 'PITPH Estimates', 'TDG Estimates' ),
                               choiceValues = c('flow_vol','pitph','tdg'),
                               selected = 'flow_vol')
      )
                  
      ) # close switch
    
  })
  
  # Based on model flow type selected generate river flow conditions
  # River Flow Values
  river_dat <- reactive({
      
      switch(input$mod_flow,
             'Static' = tibble(river = rep(c('Snake River', 'Columbia River'), each = 4),
                                     project = project2,
                                     project_code = project_code2,
                                     date = ymd("20180601"),
                                     flow = rep(c(input$snake_values, input$columbia_values), each = 4)),
                                     
             
             'Simulated Year' =     simFlow(river = 'Snake', flow_year = input$ave_flow) %>%  #input$ave_flow
                                  bind_rows(simFlow(river = 'Columbia', flow_year = input$ave_flow)) %>%  #input$ave_flow
                                  select(river = River, date = Day, flow = sim_flow) %>%
                                  ungroup() %>%
                                  mutate(river = paste0(river, " River"),
                                         date = as.Date(date, origin = "2017-01-01")),
             
             'Observed Year' = map_dfr(.x = project_code2,
                                     .f = function(x) {queryRiverData(site = x, year = input$obs_flow_year,
                                                                 start_day = '03/01',
                                                                 end_day = '06/30') %>%
                                         select(project_code = Site, date = Date, flow = Inflow,
                                                obs_spill_prop = Spill_percent, obs_spill_vol = Spill,
                                                obs_dissolved_gas = Dissolved_Gas, obs_tdg = TDG)
                                         })
            )
    
  })
  
# get powerhouse spillway passage efficiency
  # PITPH * (1 - PSP) = the proportion going through PH and JBS

  psp_dat <- reactive({
    tibble(project_code = project_code2,
           psp = c(input$psp_lwg, input$psp_lgs, input$psp_lmn, input$psp_ihr, input$psp_mcn, input$psp_jda, input$psp_tda, input$psp_bon))
  })
  
  observeEvent(psp_dat(), {
    
    tmp_psp <- psp_dat() %>% pull(psp)
    
    if(!is.null(input$psp_lwg) & length(which(tmp_psp>1))>0 | length(which(tmp_psp<0))>0){
      showNotification("Error:  Powerhouse surface passage efficiency must be between 0.0 and 1.0.",
                       duration = NULL,
                       closeButton = TRUE, type = 'error')
    }
  })


    
  #------------------------------------------------------------------------------  
  # Create spill sliders based on user input; volume or proportion
  # 'LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'
  #------------------------------------------------------------------------------
  
  # Lower Granite
  
  output$lwg_high_spill_slider <- renderUI({
    if(input$lwg_high_spill_value == 'Volume'){
      #sliderInput(inputId = 'spill_lwg', label = "", min = 0, max = 250, step = 10, value = 20)
      numericInput(inputId = 'high_spill_lwg', label = "", min = 0, max = 250, value = 20)
    } else {
      #numericInput(inputId = 'spill_lwg', label = "", min = 0, max = 1, step = .05, value = .25)
      numericInput(inputId = 'high_spill_lwg', label = "", min = 0, max = 1.0, value = .25)
    }
  })
  
  output$lwg_low_spill_slider <- renderUI({
    if(input$lwg_low_spill_value == 'Volume'){
      #sliderInput(inputId = 'spill_lwg', label = "", min = 0, max = 250, step = 10, value = 20)
      numericInput(inputId = 'low_spill_lwg', label = "", min = 0, max = 250, value = 20)
    } else {
      #numericInput(inputId = 'spill_lwg', label = "", min = 0, max = 1, step = .05, value = .25)
      numericInput(inputId = 'low_spill_lwg', label = "", min = 0, max = 1.0, value = .25)
    }
  })
  
  # Little Goose
  output$lgs_high_spill_slider <- renderUI({
    if(input$lgs_high_spill_value == 'Volume'){
      numericInput(inputId = 'high_spill_lgs', label = "", min = 0, max = 250, value = 30)
    } else {
      numericInput(inputId = 'high_spill_lgs', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  output$lgs_low_spill_slider <- renderUI({
    if(input$lgs_low_spill_value == 'Volume'){
      numericInput(inputId = 'low_spill_lgs', label = "", min = 0, max = 250, value = 30)
    } else {
      numericInput(inputId = 'low_spill_lgs', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  # Lower Monumental - lmn
  
  output$lmn_high_spill_slider <- renderUI({
    if(input$lmn_high_spill_value == 'Volume'){
      numericInput(inputId = 'high_spill_lmn', label = "", min = 0, max = 250, value = 25)
    } else {
      numericInput(inputId = 'high_spill_lmn', label = "", min = 0, max = 1, value = .25)
    }
  })
  
  output$lmn_low_spill_slider <- renderUI({
    if(input$lmn_low_spill_value == 'Volume'){
      numericInput(inputId = 'low_spill_lmn', label = "", min = 0, max = 250, value = 25)
    } else {
      numericInput(inputId = 'low_spill_lmn', label = "", min = 0, max = 1, value = .25)
    }
  })
  
  
  # Ice Harbor - ihr
  output$ihr_high_spill_slider <- renderUI({
    if(input$ihr_high_spill_value == 'Volume'){
      numericInput(inputId = 'high_spill_ihr', label = "", min = 0, max = 250, value = 30)
    } else {
      numericInput(inputId = 'high_spill_ihr', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  output$ihr_low_spill_slider <- renderUI({
    if(input$ihr_low_spill_value == 'Volume'){
      numericInput(inputId = 'low_spill_ihr', label = "", min = 0, max = 250, value = 30)
    } else {
      numericInput(inputId = 'low_spill_ihr', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  # McNary - mcn
  output$mcn_high_spill_slider <- renderUI({
    if(input$mcn_high_spill_value == 'Volume'){
      numericInput(inputId = 'high_spill_mcn', label = "", min = 0, max = 450, value = 125)
    } else {
      numericInput(inputId = 'high_spill_mcn', label = "", min = 0, max = 1, value = .50)
    }
  })
  
  output$mcn_low_spill_slider <- renderUI({
    if(input$mcn_low_spill_value == 'Volume'){
      numericInput(inputId = 'low_spill_mcn', label = "", min = 0, max = 450, value = 125)
    } else {
      numericInput(inputId = 'low_spill_mcn', label = "", min = 0, max = 1, value = .50)
    }
  })
  
  # John Day - jda
  output$jda_high_spill_slider <- renderUI({
    if(input$jda_high_spill_value == 'Volume'){
      numericInput(inputId = 'high_spill_jda', label = "", min = 0, max = 450, value = 75)
    } else {
      numericInput(inputId = 'high_spill_jda', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  output$jda_low_spill_slider <- renderUI({
    if(input$jda_low_spill_value == 'Volume'){
      numericInput(inputId = 'low_spill_jda', label = "", min = 0, max = 450, value = 75)
    } else {
      numericInput(inputId = 'low_spill_jda', label = "", min = 0, max = 1, value = .30)
    }
  })
  
  # The Dalles - tda
  output$tda_high_spill_slider <- renderUI({
    if(input$tda_high_spill_value == 'Volume'){
      numericInput(inputId = 'high_spill_tda', label = "", min = 0, max = 450, value = 100)
    } else {
      numericInput(inputId = 'high_spill_tda', label = "", min = 0, max = 1, value = .40)
    }
  })
  
  output$tda_low_spill_slider <- renderUI({
    if(input$tda_low_spill_value == 'Volume'){
      numericInput(inputId = 'low_spill_tda', label = "", min = 0, max = 450, value = 100)
    } else {
      numericInput(inputId = 'low_spill_tda', label = "", min = 0, max = 1, value = .40)
    }
  })
  
  # Bonneville - bon
  output$bon_high_spill_slider <- renderUI({
    if(input$bon_high_spill_value == 'Volume'){
      numericInput(inputId = 'high_spill_bon', label = "", min = 0, max = 450, value = 100)
    } else {
      numericInput(inputId = 'high_spill_bon', label = "", min = 0, max = 1, value = .40)
    }
  })
  
  output$bon_low_spill_slider <- renderUI({
    if(input$bon_low_spill_value == 'Volume'){
      numericInput(inputId = 'low_spill_bon', label = "", min = 0, max = 450, value = 100)
    } else {
      numericInput(inputId = 'low_spill_bon', label = "", min = 0, max = 1, value = .40)
    }
  })
  
 
  spill <- reactive({
  tibble(species = rep(input$spp_input, 16),  #input$spp_input
         river = rep(c('Snake River', 'Columbia River'), each = 8),
         project = rep(project2, each = 2), project_code = rep(project_code2, each = 2),
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
  
  flow_dat <- reactive({
   
   switch(input$mod_flow,
           
           'Static' = full_join(river_dat(), spill(), by = c('river', 'project', 'project_code')) %>%
             full_join(psp_dat(), by = 'project_code') %>%
             select(species, everything()) %>%
             mingenOps(),
            
           'Simulated Year' = full_join(river_dat(), spill(), by = 'river') %>%
             full_join(psp_dat(), by = 'project_code') %>%
             select(species, river, project, project_code, date, flow, period, psp, spill) %>%
             mingenOps(),
            
           'Observed Year' = full_join(river_dat(), spill(), by = 'project_code') %>%
             full_join(psp_dat(), by = 'project_code') %>%
             select(species, river, project, project_code, date, flow, period, psp, obs_spill_prop, obs_spill_vol,
                    obs_dissolved_gas, obs_tdg, spill) %>%
             mingenOps() %>%
             mutate(obsPITPH = pitph(species = species, project = project_code, flow = flow, spill = obs_spill_prop))
           )
  })
  
  mod_dat <- reactive({
    
    switch(input$spe_curve,
            'CSS' = flow_dat() %>%
             mutate(ph_entrance = pitph(species = species, project = project_code, flow = flow, spill = actual_spill_prop)),

            'COMPASS' = flow_dat() %>%
             mutate(tmp_spe = getSPE(species = species, project = project_code, flow = flow, spill = actual_spill_prop),
                    ph_entrance = 1-tmp_spe) %>%
             select(-tmp_spe)
    )
    
  })
  
  dat <- reactive({
      
      mod_dat() %>%
      mutate(CSS_PITPH = pitph(species = species, project = project_code, flow = flow, spill = actual_spill_prop),
             NOAA_PITPH = 1 - (getSPE(species = species, project = project_code, flow = flow, spill = actual_spill_prop)),
             transport = if_else(yday(date) >= 115 & project_code %in% c('LWG', 'LGS', 'LMN'), 1, 0),
             passage_weir = ph_entrance * psp,
             ph = ph_entrance - passage_weir,
             jbs = ph * fge,
             turbine = ph - jbs,
             transported = jbs * transport,
             rr = jbs - transported,
             spillway = 1 - (passage_weir + turbine + transported + rr),
             PITPH = (turbine + rr)/(turbine + rr + spillway + passage_weir),
             SPE = 1-PITPH,
             TDGmon = zTDGMON(project_code = project_code, forebay_gas = 10, flow = flow, spill_prop = actual_spill_prop),
             TDGspill = zTDGSpill(project_code = project_code, flow = flow, spill_prop = actual_spill_prop),
             w_spill = (actual_spill_prop * period)/24,
             w_css = (CSS_PITPH * period)/24,
             w_noaa = (NOAA_PITPH * period)/24,
             w_ph_entrance = (ph_entrance * period) /24,
             w_psp = (passage_weir * period)/24,
             w_turbine = (turbine * period)/24,
             w_transported = (transported * period)/24,
             w_rr = (rr * period)/24,
             w_spillway = (spillway * period)/24,
             w_PITPH = (PITPH * period)/24,
             w_TDG = (TDGmon * period)/24)

  })


  output$param_dat_table <- DT::renderDT({
    DT::datatable(dat() %>%
      mutate(date = str_sub(as.character(date),start = 6)) %>%
      mutate_if(is.numeric, round, 2),
      options = list(pageLength = 16))
    })
  
  # function for downloading data
  output$data_export <- downloadHandler(
    
    filename = function() {
      paste0("pitph_data_", Sys.Date(), "_.csv")
    },
    content = function(filename) {
      write.csv(mutate_all(dat(), as.character), filename, row.names = FALSE)
    }
  )

  
  output$project_table <- renderTable({ 

    dat() %>%
      mutate(project = factor(project, levels=project2)) %>%
      group_by(project, date) %>%
      summarise(CSS_PITPH = sum(w_css, na.rm = TRUE),
                NOAA_PITPH = sum(w_noaa, na.rm = TRUE),
                PH_Entrance = sum(w_ph_entrance, na.rm = TRUE),
                Passage_Weir = sum(w_psp, na.rm = TRUE),
                Turbine = sum(w_turbine, na.rm = TRUE),
                Transported = sum(w_transported, na.rm = TRUE),
                Return_River = sum(w_rr, na.rm = TRUE),
                Spillway = sum(w_spillway, na.rm = TRUE),
                PITPH = sum(w_PITPH, na.rm = TRUE),
                TDG = sum(w_TDG, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(project) %>%
      summarise(#PH_Entrance = mean(PH_Entrance, na.rm = TRUE),
                CSS_PITPH = mean(CSS_PITPH, na.rm = TRUE),
                NOAA_PITPH = mean(NOAA_PITPH, na.rm = TRUE),
                Passage_Weir = mean(Passage_Weir, na.rm = TRUE),
                Turbine = mean(Turbine, na.rm = TRUE),
                Transported = mean(Transported, na.rm = TRUE),
                Return_River = mean(Return_River, na.rm = TRUE),
                Spillway = mean(Spillway, na.rm = TRUE),
                #All = Spillway + Passage_Weir + Turbine + Transported + Return_River,
                Transported_PITPH = mean(PITPH, na.rm = TRUE),
                SPE = 1-Transported_PITPH,
                TDG = mean(TDG, na.rm = TRUE)+100) %>%
      rename(Project = project) %>%
      select(Project, Spillway, everything()) %>%
      arrange(Project)
  })
  
  output$system_table <- renderTable({ 
    
    dat() %>%
      mutate(project = factor(project, levels=project2)) %>%
      group_by(project, date) %>%
      summarise(CSS_PITPH = sum(w_css, na.rm = TRUE),
                NOAA_PITPH = sum(w_noaa, na.rm = TRUE),
                PH_Entrance = sum(w_ph_entrance, na.rm = TRUE),
                Passage_Weir = sum(w_psp, na.rm = TRUE),
                Turbine = sum(w_turbine, na.rm = TRUE),
                Transported = sum(w_transported, na.rm = TRUE),
                Return_River = sum(w_rr, na.rm = TRUE),
                Spillway = sum(w_spillway, na.rm = TRUE),
                PITPH = sum(w_PITPH, na.rm = TRUE),
                TDG = sum(w_TDG, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(project) %>%
      summarise(#PH_Entrance = mean(PH_Entrance, na.rm = TRUE),
        CSS_PITPH = mean(CSS_PITPH, na.rm = TRUE),
        NOAA_PITPH = mean(NOAA_PITPH, na.rm = TRUE),
        Passage_Weir = mean(Passage_Weir, na.rm = TRUE),
        Turbine = mean(Turbine, na.rm = TRUE),
        Transported = mean(Transported, na.rm = TRUE),
        Return_River = mean(Return_River, na.rm = TRUE),
        Spillway = mean(Spillway, na.rm = TRUE),
        PITPH = mean(PITPH, na.rm = TRUE),
        SPE = 1-PITPH) %>%
      ungroup() %>%
      summarise(`CSS PITPH` = sum(CSS_PITPH),
                `NOAA PITPH` = sum(NOAA_PITPH),
                `Total PITPH` = sum(PITPH),
                `Average SPE` = mean(SPE)) 

      # 
      # mutate(inriver = Spillway + Return_River + Turbine + Passage_Weir) %>%
      # rename(Project = project) %>%
      # arrange(Project)
      # 
      # 
      # 
      # 
      # 
      # dat() %>%
      #   mutate(project = factor(project, levels=project2)) %>%
      #   group_by(project, date) %>%
      #   summarise(PITPH = sum(w_estPITPH, na.rm = TRUE),
      #             SPE = 1 - PITPH) %>%
      #   ungroup() %>%
      #   group_by(project) %>%
      #   summarise(PITPH = mean(PITPH, na.rm = TRUE),
      #             SPE = mean(SPE, na.rm = TRUE)) %>%
      #   ungroup() %>%
      #   summarise(`Total PITPH` = sum(PITPH),
      #             `Average SPE` = mean(SPE))
  })
  
  
# Modeled flow figure
  output$flow_plot <- renderPlot({#renderPlot({
    
   switch(input$mod_flow,
   
    "Static" = dat() %>%
      group_by(species, river, project, project_code, date) %>%
    summarise(w_spill = sum(w_spill),
              w_TDG = sum(w_TDG) + 100,
              w_PITPH = sum(w_PITPH)) %>%
      ungroup() %>%
      mutate(project = fct_relevel(project, project2),
             river = fct_inorder(river)) %>%
      ggplot(aes(x = project)) +
      geom_bar(aes(y = w_spill, fill = project), stat = 'identity') +
      #geom_line(aes(x = project, y = w_PITPH), colour = 'black', size = 2) +
      geom_point(aes(y = w_PITPH, colour = w_TDG), size = 20) +
      scale_fill_viridis_c(option = "D", direction = 1) +
      scale_fill_viridis_d() +
      #scale_color_viridis_c(option = "B", direction = -1, begin = .5, end = 1) +
      scale_colour_gradient(breaks = c(95,105,115,125), labels = c("<=95","105", "115", ">=125"), limits = c(95,125), na.value = 'red', low = "yellow", high = "red") +
      scale_y_continuous(limits = c(0,1)) +
      guides(shape = guide_legend(override.aes = list(size = 4))) +
      theme_bw() +
      theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
            axis.title = element_text(size = 14), axis.text = element_text(size = 12),
            legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
      labs(x = 'Project',
           y = 'PITPH and Spill Proportion',
           colour = 'TDG Percent',
           fill = 'Project',
           title = "Estimated PITPH (point) and total dissolved gas (TDG; point color) at static in-flow volumes and set spill proportions (bars)."
           ),
                 
    "Simulated Year" =  switch(input$sim_plot,
      "flow_vol" = dat() %>%
        select(species, river, project, project_code, date, period, Inflow = flow, actual_spill_vol) %>%
        mutate(period = paste0(period, " - Hour Period"),
               project = fct_inorder(project)) %>%
        spread(period, actual_spill_vol) %>%
        gather(key, value, Inflow, contains('Period')) %>%
        #mutate(key = fct_relevel(key, 'Inflow')) %>%
      ggplot(aes(x = date, y = value)) +
      geom_line(aes(group = key, colour = key), size = 1) +
      scale_x_date(date_breaks = '2 weeks', date_labels = format("%d-%b")) +
      scale_colour_viridis_d(begin = 0, end = .7) +
      facet_wrap(~project, scales = 'free', ncol = 2) +
      theme_bw()+
        theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
              axis.title = element_text(size = 14), axis.text = element_text(size = 12),
              legend.title = element_text(size = 12), legend.text = element_text(size = 12),
              strip.text = element_text(size = 12)) +
      labs(x = 'Date',
           colour = 'Flow',
           y = 'Flow Volume (kcfs)',
           title = 'Simulated seasonal in-flow and set spill volumes for daily duck spill periods.'),
    
    "pitph_tdg" = dat() %>%
      group_by(species, river, project, project_code, date) %>%
      summarise(w_spill = sum(w_spill),
                w_TDG = sum(w_TDG) + 100,
                w_PITPH = sum(w_estPITPH)) %>%
      ungroup() %>%
      #gather(key, value, w_TDG, w_spill) %>%
      mutate(project = fct_relevel(project, project2)) %>%
      ggplot(aes(x = date)) +
      geom_bar(aes(y = w_PITPH, fill = w_PITPH), stat = 'identity') +
      geom_line(aes(y = w_spill, colour = w_TDG), size = 1) +
      scale_x_date(date_breaks = '2 weeks', date_labels = format("%d-%b")) +
      scale_y_continuous(limits = c(0,1)) +
      scale_fill_viridis_c(option = "D", direction = 1) +
      scale_colour_gradient(breaks = c(95,105,115,125), labels = c("<=95","105", "115", ">=125"), limits = c(95,125), na.value = 'red', low = "yellow", high = "red") +
      facet_wrap(~project, ncol = 2) +
      theme_bw()+
      theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
            axis.title = element_text(size = 14), axis.text = element_text(size = 12),
            legend.title = element_text(size = 12), legend.text = element_text(size = 12),
            strip.text = element_text(size = 12)) +
      labs(title = 'Season-wide set spill proportion (line) and daily estimated PITPH (bars) and total dissolved gas (TDG).',
           colour = 'TDG Percent',
           fill = 'PITPH',
           x = 'Date',
           y = 'PITPH and Spill Proportion')
    ),
    
    
    "Observed Year" = switch(input$sim_plot,
                             "flow_vol" = dat() %>%
                               mutate(project = fct_inorder(project),
                                      period = paste0(period, " - Hour Period")) %>%
                               select(project, project_code, date, period, flow, obs_spill_vol, set_spill_vol, actual_spill_vol) %>%
                               gather(key = key, value = value, flow:actual_spill_vol) %>%
                               ggplot(aes(x = date, y = value)) +
                               geom_line(aes(group = key, colour = key), size = 1) +
                               scale_x_date(date_breaks = '2 weeks', date_labels = format("%d-%m")) +
                               scale_colour_viridis_d(name = '', labels = c('Actual Spill', 'Inflow', 'Observed Spill', 'Set Spill')) +
                               facet_grid(project~period, scales = 'free') +
                               theme_bw()+
                               theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
                                     axis.title = element_text(size = 14), axis.text = element_text(size = 12),
                                     legend.title = element_text(size = 12), legend.text = element_text(size = 12),
                                     strip.text = element_text(size = 12)) +
                               labs(x = 'Date',
                                    y = 'Flow Volume (kcfs)',
                                    title = 'Observed in-flow and spill volumes for selected migratory year and set spill rates.'),
                             
                             "tdg" = dat() %>%
                               mutate(project = fct_inorder(project),
                                      period = paste0(period, " - Hour Period")) %>%
                               select(project, project_code, date, period, obs_tdg, TDGmon) %>%
                               mutate(TDGmon = TDGmon + 100) %>%
                               gather(key = key, value = value, obs_tdg:TDGmon) %>%
                               ggplot(aes(x = date, y = value)) +
                               geom_line(aes(group = key, colour = key), size = 1) +
                               scale_x_date(date_breaks = '2 weeks', date_labels = format("%d-%m")) +
                               scale_colour_viridis_d(name = '', labels = c('Observed TDG', 'Modeled TDG')) +
                               facet_grid(project~period, scales = 'free') +
                               theme_bw()+
                               theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
                                     axis.title = element_text(size = 14), axis.text = element_text(size = 12),
                                     legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
                               labs(x = 'Date',
                                    y = 'Percent',
                                    title = 'Estimated total dissolved gas (TDG) for selected migratory year and observed in-flow and spill conditions and for set spill rates.'),
                             
                             "pitph" = dat() %>%
                               mutate(project = fct_inorder(project),
                                      period = paste0(period, " - Hour Period")) %>%
                               select(project, project_code, date, period, obsPITPH, pspPITPH) %>%
                               gather(key = key, value = value, obsPITPH:pspPITPH) %>%
                               ggplot(aes(x = date, y = value)) +
                               geom_line(aes(group = key, colour = key), size = 1) +
                               scale_x_date(date_breaks = '2 weeks', date_labels = format("%d-%m")) +
                               scale_colour_viridis_d(name = '', labels = c('Observed PITPH', 'Modeled PITPH')) +
                               facet_grid(project~period, scales = 'free') +
                               theme_bw()+
                               theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
                                     axis.title = element_text(size = 14), axis.text = element_text(size = 12),
                                     legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
                               labs(x = 'Date',
                                    y = 'PITPH',
                                    title = 'Estimated PITPH for selected migratory year and observed in-flow and spill conditions and for set spill rates.')
    )
    
   )
    
  })
  
  output$sized_flow <- renderUI({
    
    switch(input$mod_flow,
    "Static" = plotOutput("flow_plot"),
    "Simulated Year" = plotOutput("flow_plot", height = 800),
    "Observed Year" = plotOutput("flow_plot", height = 800)
    )
  }) 
   

  # SPE Curves and TDG

  output$spe_curve<- renderPlot({

    spill_df <- as.tibble(expand.grid(spill = seq(0, 1, by = .01),
                                      project_code = project_code2,
                                      stringsAsFactors = FALSE)) %>%
      left_join(tibble(project_code = project_code2,
                       project = project2), by = 'project_code')
    
    if(input$mod_flow == 'Static'){
      tmp_df <- inner_join(spill_df, river_dat(), by = 'project_code') %>%
        inner_join(psp_dat(), by = 'project_code') %>%
        left_join(tibble(project_code = project_code2,
                         project = project2), by = 'project_code') %>%
        mutate(species = rep(input$spp_input,n()),
               transport = if_else(yday(date) >= 115 & project_code %in% c('LWG', 'LGS', 'LMN'), 1, 0)) %>%
        mingenOps() %>%
        mutate(PITPH = pitph(species = species, project = project_code, flow = flow, spill = actual_spill_prop),
               pspPITPH = PITPH * (1-psp),
               tranPITPH = pspPITPH - ((pspPITPH * fge) * transport),
               actual_spill_prop = round(actual_spill_prop, 2),
               actual_spill_vol = round(actual_spill_vol, 0),
               project_code = fct_inorder(project_code),
               project = fct_inorder(project),
               TDG = zTDGMON(project_code = project_code, forebay_gas = 10, flow = flow, spill_prop = actual_spill_prop))
      
      ggplot(tmp_df, aes(x = actual_spill_prop, y = tranPITPH, group = project_code, colour = project)) + # changed data from tmp_curve
        geom_line(size = 2) +
        #geom_text(aes(label = round(TDG)+100),hjust = 1, colour = 'black') +
        #geom_point(aes(fill = round(TDG)+100), shape = 21, size = 3) +
        scale_color_viridis_d() +
        scale_x_continuous(breaks = seq(0,1, by = .1), labels = seq(0, 1, by = .1)) +
        scale_y_continuous(breaks = seq(0,1, by = .1), labels = seq(0, 1, by = .1)) +
        scale_fill_gradient(breaks = c(95,105,115,125), labels = c("<=95","105", "115", ">=125"), limits = c(95,125), na.value = 'red', low = "yellow", high = "red") +
        theme_bw() +
        theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
              axis.title = element_text(size = 14), axis.text = element_text(size = 12),
              legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
        labs(x = 'Spill Proportion',
             y = 'PITPH',
             title = 'Estimated PITPH (y-axis) and total dissolved gas (point color) at static in-flow volumes and set spill proportions (x-axis).',
             caption = '',
             colour = 'Project',
             fill = 'TDG Percent')
      
    } else {

    snake_df <- as.tibble(expand.grid(river = "Snake",
                                      project_code = project_code2[1:4],
                                      flow = seq(25,250, by = 25),
                                      stringsAsFactors = FALSE)) %>%
                inner_join(spill_df, by = 'project_code')

    col_df <- as.tibble(expand.grid(river = "Columbia",
                                    project_code = project_code2[5:8],
                                    flow = seq(100, 450, by = 25),
                                    stringsAsFactors = FALSE)) %>%
                inner_join(spill_df, by = 'project_code')

    tmp_df <- bind_rows(snake_df, col_df) %>%
      inner_join(psp_dat(), by = 'project_code') %>%
      mutate(species = rep(input$spp_input,n())) %>%
      mingenOps() %>%
      mutate(PITPH = pitph(species = species, project = project_code, flow = flow, spill = actual_spill_prop),
             pspPITPH = PITPH * (1-psp),
             actual_spill_prop = round(actual_spill_prop, 2),
             actual_spill_vol = round(actual_spill_vol, 0),
             project = fct_inorder(project),
             project_code = fct_inorder(project_code),
             TDG = zTDGMON(project_code = project_code, forebay_gas = 10, flow = flow, spill_prop = actual_spill_prop)) #%>%
     #rowwise() %>%
     # mutate(TDG = zTDGMON(project_code, forebay_gas = 10, spill_prop = actual_spill_prop, flow = flow)) %>%
     # ungroup()

    samp_df <- tmp_df %>%
      group_by(project) %>%
      filter(row_number() %% 35 == 0) %>%
      mutate(TDG = round(TDG)+100)

tmp_df %>%
      ggplot() +
      geom_raster(aes(x = flow, y = actual_spill_prop, fill = pspPITPH)) +
      #geom_contour(aes(x = flow, y = set_spill_prop, z = pspPITPH), colour = "white", binwidth = .1) +
      geom_text(data = samp_df, aes(x = flow, y = actual_spill_prop, label = TDG, colour = TDG)) +
      #scale_color_viridis_c(option = "B", direction = -1, begin = .5, end = 1) +
      scale_color_gradient(breaks = c(95,105,115,125), labels = c("<=95","105", "115", ">=125"), limits = c(95,125), na.value = 'red', low = "yellow", high = "red") +
      scale_fill_viridis_c(option = "D", direction = 1) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap(~project, scales = "free", ncol = 2) +
      theme_bw() +
  theme(plot.title = element_text(color="#536872", face = 'bold', size=16, hjust=.5),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) +
      labs(title = 'Estimated PITPH and total dissolved gas (TDG) for set in-flow volumes and spill proportions.',
           fill = 'PITPH',
           colour = 'TDG Percent',
           x = 'Inflow Volume (kcfs)',
           y = 'Spill Proportion')
    
    }

  })

  output$sized_pitph <- renderUI({
    plotOutput("spe_curve", height = 800)
  })

  
}) # server instance
