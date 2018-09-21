#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(tidyverse)
library(lubridate)

shinyUI(
  navbarPage(
    title = div(
      div(id = 'logo-id', img(src = "NPTlogos2.png", height = 70)),
      tags$a("DFRM Home", href = 'http://www.nptfisheries.org')
    ),
    id = "kus_navbar",
    windowTitle = "PitPH Planning",
    theme = "styles.css",
    position = "fixed-top",
    collapsible = TRUE,
    footer = div(
      hr(),
      div(
        id = "footer-id",
        "The data presented in this web application may not be solely collected, managed or owned
        by the Nez Perce Tribe. All data should be considered draft and is not guaranteed for
        accuracy.  Permission to use the data should be sought from the original collectors and data managers."
      )
      ),
    
    tabPanel(
      "PitPH",
      fluidPage(fluidRow(column(
        6,
        h3(
          "Snake and Columbia River Hydrosystem Spill Operations and Fish Passage Route Planning"
        ),
        h5(
          "The web applicaiton tool dynamically illustrates predicted power house encounter rates for juvenile Chinook Salmon as
          river inflow and spill operations at each dam are adjusted."
        ),
        h6('Created by:'),
        h6(
          tags$a(
            "Nez Perce Tribe, Department of Fisheries Resources Management",
            href = 'http://nptfisheries.org'
          )
        )
        )
        ),# ends fluid row,),
        # ends fluid page,
        hr(),
        tabsetPanel(
          tabPanel('PitPH Predictions',
                   fluidRow(
                     column(4,
                       wellPanel(
                        helpText("Select desired SPE curves, species, modeled flow type, and powerhouse surface passage collection efficiency"),
                         
                        #fluidRow(
                        #  column(3,
                                 radioButtons(inputId = 'spe_curve',
                                              label = h3(tags$b('Spill Passage Efficiency Curves:')),
                                              choices = c('CSS', 'COMPASS (Currently not available.)'),
                                              selected = 'CSS', inline = TRUE),#),
                         #  column(3,
                            radioButtons(inputId = 'spp_input',
                                         label = h3(tags$b('Species:')),
                                         choices = c('Chinook', 'Steelhead'),
                                         selected = 'Chinook', inline = TRUE),#),
                          # column(3,
                                  radioButtons(inputId = 'mod_flow',
                                               label = h3(tags$b('Modeled Flow:')),
                                               choices = c('Static', 'Simulated Year', 'Observed Year'),
                                               selected = 'Static', inline = TRUE),#), 
                                  uiOutput('flow_value_input'),
                          # column(3,
                                  h3(tags$b("Powerhouse Surface Passage:")),
                        fluidRow(column(6,
                                  numericInput(inputId = 'psp_lwg',
                                               label = h4('Lower Granite PSP Efficiency:'),
                                               min = 0,
                                               max = 1,
                                               value = 0,
                                               step = .05, width = '100%'),
                                  renderText('psp_lwg_error')
                                  ),
                                 column(6,
                                  numericInput(inputId = 'psp_lgs',
                                               label = h4('Little Goose PSP Efficiency:'),
                                               min = 0,
                                               max = 1,
                                               value = 0,
                                               step = .05, width = '100%')
                                 )),
                        fluidRow(column(6,
                                        numericInput(inputId = 'psp_lmn',
                                               label = h4('L. Monumental PSP Efficiency:'),
                                               min = 0,
                                               max = 1,
                                               value = 0,
                                               step = .05, width = '100%')
                        ),
                        column(6,
                               numericInput(inputId = 'psp_ihr',
                                               label = h4('Ice Harbor PSP Efficiency:'),
                                               min = 0,
                                               max = 1,
                                               value = 0,
                                               step = .05, width = '100%')
                        )),
                        fluidRow(column(6,
                                        numericInput(inputId = 'psp_mcn',
                                     label = h4('McNary PSP Efficiency:'),
                                     min = 0,
                                     max = 1,
                                     value = 0,
                                     step = .05, width = '100%')
                        ),
                        column(6,
                               numericInput(inputId = 'psp_jda',
                                     label = h4('John Day PSP Efficiency:'),
                                     min = 0,
                                     max = 1,
                                     value = 0,
                                     step = .05, width = '100%')
                        )),
                        fluidRow(column(6,
                                        numericInput(inputId = 'psp_tda',
                                     label = h4('Dalles PSP Efficiency:'),
                                     min = 0,
                                     max = 1,
                                     value = 0,
                                     step = .05, width = '100%')
                        ),
                        column(6,
                               numericInput(inputId = 'psp_bon',
                                     label = h4('Bonneville PSP Efficiency:'),
                                     min = 0,
                                     max = 1,
                                     value = 0,
                                     step = .05, width = '100%')
                        )),

                         
                        h3(tags$b('Spill Level Control and Rate:')),
           
              # Lower Granite - lwg             
                        h4(tags$b('Lower Granite Dam')),
                        fluidRow(
                          column(6,
                                 radioButtons(
                           inputId = 'lwg_high_spill_value',
                           label = h5(tags$b('High Spill Period (16 Hours):')),
                           choices = c('Proportion', 'Volume'),
                           selected = 'Volume',
                           inline = TRUE
                         )),
                         column(6,
                                radioButtons(
                           inputId = 'lwg_low_spill_value',
                           label = h5(tags$b('Low Spill Period (8 Hours):')),
                           choices = c('Proportion', 'Volume'),
                           selected = 'Volume',
                           inline = TRUE
                         ))
                         ),
                        fluidRow(
                        column(6, uiOutput("lwg_high_spill_slider")),
                        column(6, uiOutput("lwg_low_spill_slider"))
                        ),
              
              # Little Goose - lgs
              h4(tags$b('Little Goose')),
              fluidRow(
                column(6,
                       radioButtons(
                         inputId = 'lgs_high_spill_value',
                         label = h5(tags$b('High Spill Period (16 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       )),
                column(6,
                       radioButtons(
                         inputId = 'lgs_low_spill_value',
                         label = h5(tags$b('Low Spill Period (8 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       ))
              ),
              fluidRow(
                column(6, uiOutput("lgs_high_spill_slider")),
                column(6, uiOutput("lgs_low_spill_slider"))
              ),
              
              # Lower Monumental - lmn
              
              h4(tags$b('Lower Monumental')),
              fluidRow(
                column(6,
                       radioButtons(
                         inputId = 'lmn_high_spill_value',
                         label = h5(tags$b('High Spill Period (16 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Volume',
                         inline = TRUE
                       )),
                column(6,
                       radioButtons(
                         inputId = 'lmn_low_spill_value',
                         label = h5(tags$b('Low Spill Period (8 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Volume',
                         inline = TRUE
                       ))
              ),
              fluidRow(
                column(6, uiOutput("lmn_high_spill_slider")),
                column(6, uiOutput("lmn_low_spill_slider"))
              ),
              
              # Ice Harbor - ihr
              
              h4(tags$b('Ice Harbor')),
              fluidRow(
                column(6,
                       radioButtons(
                         inputId = 'ihr_high_spill_value',
                         label = h5(tags$b('High Spill Period (16 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Volume',
                         inline = TRUE
                       )),
                column(6,
                       radioButtons(
                         inputId = 'ihr_low_spill_value',
                         label = h5(tags$b('Low Spill Period (8 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Volume',
                         inline = TRUE
                       ))
              ),
              fluidRow(
                column(6, uiOutput("ihr_high_spill_slider")),
                column(6, uiOutput("ihr_low_spill_slider"))
              ),
              
              # McNary - mcn
              
              h4(tags$b('McNary Dam')),
              fluidRow(
                column(6,
                       radioButtons(
                         inputId = 'mcn_high_spill_value',
                         label = h5(tags$b('High Spill Period (16 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       )),
                column(6,
                       radioButtons(
                         inputId = 'mcn_low_spill_value',
                         label = h5(tags$b('Low Spill Period (8 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       ))
              ),
              fluidRow(
                column(6, uiOutput("mcn_high_spill_slider")),
                column(6, uiOutput("mcn_low_spill_slider"))
              ),
              
              # John Day - jda
              
              h4(tags$b('John Day')),
              fluidRow(
                column(6,
                       radioButtons(
                         inputId = 'jda_high_spill_value',
                         label = h5(tags$b('High Spill Period (16 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       )),
                column(6,
                       radioButtons(
                         inputId = 'jda_low_spill_value',
                         label = h5(tags$b('Low Spill Period (8 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       ))
              ),
              fluidRow(
                column(6, uiOutput("jda_high_spill_slider")),
                column(6, uiOutput("jda_low_spill_slider"))
              ),
              
              # The Dalles - tda
              
              h4(tags$b('The Dalles')),
              fluidRow(
                column(6,
                       radioButtons(
                         inputId = 'tda_high_spill_value',
                         label = h5(tags$b('High Spill Period (16 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       )),
                column(6,
                       radioButtons(
                         inputId = 'tda_low_spill_value',
                         label = h5(tags$b('Low Spill Period (8 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Proportion',
                         inline = TRUE
                       ))
              ),
              fluidRow(
                column(6, uiOutput("tda_high_spill_slider")),
                column(6, uiOutput("tda_low_spill_slider"))
              ),
              
              # Bonneville - bon
              
              h4(tags$b('Bonneville')),
              fluidRow(
                column(6,
                       radioButtons(
                         inputId = 'bon_high_spill_value',
                         label = h5(tags$b('High Spill Period (16 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Volume',
                         inline = TRUE
                       )),
                column(6,
                       radioButtons(
                         inputId = 'bon_low_spill_value',
                         label = h5(tags$b('Low Spill Period (8 Hours):')),
                         choices = c('Proportion', 'Volume'),
                         selected = 'Volume',
                         inline = TRUE
                       ))
              ),
              fluidRow(
                column(6, uiOutput("bon_high_spill_slider")),
                column(6, uiOutput("bon_low_spill_slider"))
              )

                         
                       ) # close wellPanel
                       #) # close fixPanel
                     ),
                     # close column
                     column(
                      8,
                      
                      fluidRow(
                        column(
                          12,
                          align = 'center',
                          h4("Average PITPH and SPE values weighted for daily hours of operation at each hydrosystem project for the set user defined parameters."),
                          tableOutput('project_table'),
                          h4("System-wide total PITPH and estimated average SPE across all 8 projects and weighted for daily hours of project operations for the set user
                             defined parameters."),
                          tableOutput("system_table")
                        )
                      ),
                      
                      fluidRow(column(12, align = 'center',
                                      #h4("Observed and estimated river environment metrics calculated for set user defined parameters."),
                                      uiOutput("sized_flow"),
                                      #h4("Spill passage efficiency curves and estimated total dissolved gas (TDG)"),
                                      #h5("TDG is currently missing because the functions are computational slow and render the application unusable."),
                                      uiOutput("sized_pitph"))),

                       h4(tags$b('References:')),
                       
                       h5("Beer, W.N. 2018. Modeling total dissolved gas (TDG) in the Columbia and Snake Rivers. Unpublished manuscript
                          dated 30 August 2018 and updated from original (15 January 2015)."),
                       
                       h5("Columbia Basin Research, University of Washington. (2018). Data Access in Real-Time (DART) website. 
                          River Environment Graphics & Text. Available from http://www.cbr.washington.edu/dart/query/river_graph_text"),
                       
                       h5("McCann, J., B. Chockley, E. Cooper, H. Schaller, S. Haeseker, R. Lessard, C. Petrosky, E. Tinus,
                          E. Van Dyke and R. Ehlke. 2015. Comparative Survival Study (CSS) of PIT tagged
                          Spring/Summer Chinook and Summer Steelhead. 2015 Annual Report. Project No.
                          199602000. http://fpc.org/documents/CSS/CSS_2015AnnualReport.pdf. (November 2015). Appendix J."),
                       
                       h5("Zabel, R. W., J. Faulkner, S. G. Smith, J. J. Anderson, C. Van Holmes, N. Beer, S. Iltis, J. Krinke,
                          G. Fredicks, B. Bellerud, J. Sweet and A. Giorgi. 2008. Comprehensive passage model: a model of 
                          downstream migration and survival of juvenile salmonids through a hydropower system. Hydrobiologia 69: 289-300.")
                       
                     )
        ) # close column
       ), # close fluidRow),
       
       tabPanel('Dataset',
                downloadButton("data_export", label = "Export .CSV File"),
                DT::DTOutput('param_dat_table')
                ),
       tabPanel('Probability Diagram',
                div(id = 'diagram', img(src = "FCRPS_prob_diagram.png"))
                )
       
      ) # closes tabSetPanel
     ) # close fluidpage 
    ) # PITPH tab
   ) # close navbar
  )# close shinyUI