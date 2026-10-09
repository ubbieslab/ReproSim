#Authors======================================================================
#           app.R
#
#   R-code written to describe a shiny app for vaginal and intrauterine delivery in humans
# 
#
#Notes========================================================================
#


# runApp("app.R")
#
# only parameters defined in $param can be updated in shiny (not $MAIN)

#Libraries=======================================================================

library(shiny)
library(shinyBS)
library(shinyjs)
library(mrgsolve)
library(dplyr)
library(readxl)
library(DT)
library(truncnorm)
source('www/plotAll_app.R', chdir = TRUE)


# UI ----------------------------------------------------------------------



# Set plot height
plot_height <- "600px"



ui <- fluidPage(
  useShinyjs(),
  div("Physiologically-Based Simulator for Vaginal and Intrauterine Delivery ", class = "title-bar"),
  div(class = "body-container", id="mainBody",

# 1. Parameters and Options -----------------------------------------------

      
      div(width = "30%", class = "params-container",
          div(id = "params", class = "params-stack",
              div(class = "simulation-action-bar",
                  span(actionButton("resetParams", "Reset", class = "btn-default"), class = "action-button-half"),
                  span(actionButton("runSim", "Run Simulation", class = "btn-primary"), class = "action-button-half"),
                  div(class = "report-button-row",
                      downloadButton("report_html", "Generate Report", class = "btn-info")
                  )
              ),
              div(class = "param-block block-vignettes",
                  h4("Vignettes", class = "block-header"),
                  bsCollapse(id = "paramsVignettes",  multiple = TRUE, open = c(11),

# * 1.2 Vignettes -----------------------------------------------------------


                     bsCollapsePanel(title = "Vignettes", value=11, #id="vignettePanel",
                                     span(
                                       selectInput("study", "Study",
                                                   list("Select Study" = "",
                                                        "Nilsson IUD"="Nilsson_IUD",
                                                        "Nilsson Oral"="Nilsson_Oral",
                                                        "User Defined"="User_Defined"), selected = "Nilsson_IUD")
                                     )
                                     )
                  )
              ),
              div(class = "param-block block-dosing",
                  h4("Dosing", class = "block-header"),
                  bsCollapse(id = "paramsDosing",  multiple = TRUE, open = c(1),
                     

# * 1.1 Dosing ------------------------------------------------------------


                     bsCollapsePanel(title = "Dosing Options", value = 1, #id="dosingPanel",
                                     span(radioButtons("admin_route", "Route of Administration",
                                                      c("Oral", "Uterine", "Vaginal"), inline = TRUE), class="half"),
                                     # span(radioButtons("dose_type", "Dose Type",
                                     #                   c("Bolus", "Infusion"), inline=TRUE), class="half"),
                                     # hr(),
                                     span(radioButtons("input_phases", "Number of Input Phases",
                                                       c("1", "2", "3"), inline=TRUE),
                                          class="half"),
                                     span(radioButtons("input_seq", "Sequence", c("Sequential"),inline=TRUE)),
                                     hr(),
                                     span(id="phase_1_options", selectInput("input_type_1", "Phase 1 Input",
                                                       c("Zero-Order", "First-Order")),
                                          checkboxInput("input_1_lag", "Include Lag Time"),
                                          radioButtons("input_1_scale", "Time Units",
                                                       c("Hours", "Days"), selected="Days"),
                                          style="text-align:justify", class="third"),
                                     span(id="phase_2_options", selectInput("input_type_2", "Phase 2 Input",
                                                       c("Zero-Order", "First-Order", "Higuchi")),
                                          checkboxInput("input_2_lag", "Include Lag Time"),
                                          radioButtons("input_2_scale", "Time Units",
                                                       c("Hours", "Days"), selected="Days"),
                                          style="text-align:justify", class="third"),
                                     span(id="phase_3_options", selectInput("input_type_3", "Phase 3 Input",
                                                       c("Zero-Order", "First-Order", "Higuchi")),
                                          checkboxInput("input_3_lag", "Include Lag Time"),
                                          radioButtons("input_3_scale", "Time Units",
                                                       c("Hours", "Days"), selected="Days"),
                                          style="text-align:justify", class="third"),
                                     hr(),
                                     span(numericInput("dose_total", "Dose (mg)", 0.75, 0, 52, step=0.25),
                                          class="half", style="vertical-align:top"),
                                     span(sliderInput("dose_F", "Dose Fraction Released", min=0, value=1, max=1, step=0.01), class="half"),
                                     hidden(
                                       div(
                                         id = "release_function_dose_warning",
                                         class = "alert alert-warning",
                                         role = "alert",
                                         "The drug concentrations are driven by the release function. If zero-order or Higuchi release function is selected, especially for IUS products, changing the dose does not necessarily result in different concentrations unless the release profile also changes."
                                       )
                                     ),
                                     span(numericInput("num_dose", "Number of Doses", value=1, min=1, step=1), class="third"),
                                     span(numericInput("II", "Inter-dose Interval (h)", value=24, min=0, step=0.5), class="third"),
                                    span(checkboxInput("strict_dur", "Remove Dose After Specified Duration", value=FALSE), class="third",
                                         bsTooltip("strict_dur", "Simulate the removal of formulation after the selected phase duration. Sets dose compartment amount to zero but does not reset system.")),
                                    span(selectInput("removal_phase", "Remove After Phase",
                                                     choices = c("Phase 1" = "1")),
                                         class="third"),
                                     div(id="input_1_params",
                                       hr(),
                                       h4("Input Phase 1", style="text-align:center"),
                                       span(disabled(
                                         sliderInput("frac_1", "Fraction of Total Dose", min=0, max=1, value=1, step=0.01),
                                            checkboxInput("lock_frac_1", "Lock")
                                         ),
                                            bsTooltip("lock_frac_1", "Lock this slider so it is not automatically adjusted when changing the fractions released in the other dosing phases"),
                                            class="half"),
                                       span(numericInput("dur_1", "Duration of Release (Days)", 7, 0, 1000, step=1),
                                            checkboxInput("calc_dur_1", "Calculate from Rate", value=TRUE),
                                            class="half", style="vertical-align:top"),
                                       uiOutput("rate_1")
                                        ),
                                     hidden(
                                       div(id="input_2_params",
                                           hr(),
                                           h4("Input Phase 2", style="text-align:center"),
                                           span(sliderInput("frac_2", "Fraction of Total Dose", min=0, max=1, value=0, step=0.01),
                                                checkboxInput("lock_frac_2", "Lock"),
                                                bsTooltip("lock_frac_2", "Lock this slider so it is not automatically adjusted when changing the fractions released in the other dosing phases"),
                                                class="half"),
                                           span(numericInput("dur_2", "Duration of Release (Days)", 7, 0, 1000, step=1),
                                                checkboxInput("calc_dur_2", "Calculate from Rate", value=TRUE),
                                                class="half", style="vertical-align:top"),
                                           uiOutput("rate_2")
                                       )
                                     ),
                                     hidden(
                                       div(id="input_3_params",
                                           hr(),
                                           h4("Input Phase 3", style="text-align:center"),
                                           span(sliderInput("frac_3", "Fraction of Total Dose", min=0, max=1, value=0, step=0.01),
                                                checkboxInput("lock_frac_3", "Lock"),
                                                bsTooltip("lock_frac_3", "Lock this slider so it is not automatically adjusted when changing the fractions released in the other dosing phases"),
                                                class="half"),
                                           span(numericInput("dur_3", "Duration of Release (Days)", 7, 0, 1000, step=1),
                                                checkboxInput("calc_dur_3", "Calculate from Rate", value=TRUE),
                                                class="half", style="vertical-align:top"),
                                           uiOutput("rate_3")
                                       )
                                     )
                                     )

                  )
              ),
              div(class = "param-block block-drug",
                  h4("Drug-Specific Parameters", class = "block-header"),
                  bsCollapse(id = "paramsDrug",  multiple = TRUE, open = NULL,

# * 1.3 Vaginal Tissue ----------------------------------------------------

                    
                     bsCollapsePanel(title = "Vaginal Rate Constants (1/hrs)", value = 2, #id="vaginalPanel",
                                     span(numericInput("k_LE", "Lumen to Epith", 5, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_EL", "Epith to Lumen", 0.01, 0, 1, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_EVS", "Epith to Stroma", 0.43, 0, 1, step =0.01 ), class = "half"),
                                     span(numericInput("k_VSE", "Stroma to Epith", 0.01, 0, 1, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_VS_TtoB", "Stroma Tissue to Blood", 0.15, 0, 1, step =0.01 ), class = "half"),
                                     span(numericInput("k_VS_BtoT", "Stroma Blood to Tissue", 0.2, 0, 1, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_activeEtoS", "active Epith to Stroma", 0, 0, 2, step =1E-4 ), class = "half"),
                                     span(numericInput("k_active_StrTtoB", "active Stroma Tissue to Blood", 0, 0, 2, step =1E-7 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_L_CF", "Lumen to CF", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_CF_L", "CF to Lumen", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_VE_CE", "Vag to Cerv Epi", 0.001, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_CE_VE", "Cerv to Vag Epi", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_VS_CM", "V Stroma to Cerv Myometrium", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_CM_VS", "Cerv Myometrium to V Stroma", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     hr()
                                     ),

# * 1.4 Cervical Tissue ---------------------------------------------------

               
                     bsCollapsePanel(title = "Cervical Rate Constants (1/hrs)", value = 3, #id="cervicalPanel",
                                     span(numericInput("k_CF_CE", "CF to Cerv Epi", 2, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_CE_CF", "Cerv Epi to CF", 0.212, 0, 10, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_CE_CM", "Cerv Epi to Myometrium", 0.043, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_CM_CE", "Cerv Myometrium to Epi", 0.212, 0, 10, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_CM_TtoB", "Cerv Myometrium to Blood", 0.043, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_CM_BtoT", "Blood to Cerv Myometrium", 1, 0, 10, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_CF_UF", "CF to Uterine Fluid", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_UF_CF", "Uterine Fluid to CF", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_CE_UE", "Cerv Epi to Endometrium", 0.01, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_UE_CE", "Endometrium to Cerv Epi", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("k_CM_UM", "Cerv to Uterine Myometrium", 0.0001, 0, 10, step =0.01 ), class = "half"),
                                     span(numericInput("k_UM_CM", "Uterine to Cerv Myometrium", 0.0001, 0, 10, step =0.01 ), class = "half")
                                     ),

# * 1.5 Uterine Tissue ----------------------------------------------------


                    bsCollapsePanel(title = "Uterine Rate Constants (1/hrs)", value = 4, #id="uterinePanel",
                                    span(numericInput("k_UF_UE", "Uterine Fluid to Endometrium", value=2, min=0, step =0.01 ), class = "half"),
                                    span(numericInput("k_UM_BtoT", "Blood to Myometrium", value= 3, min=0, step =0.01 ), class = "half"),
                                    hr(),
                                    span(numericInput("k_UE_UF_mean_uterine", "Endometrium to Uterine Fluid Mean", value=0.148, min=0, step=0.01), class="half"),
                                    span(numericInput("k_UE_UM_mean_uterine", "Endometrium to Myometrium Mean", value=0.076, min=0, step=0.01), class="half"),
                                    hr(),
                                    span(numericInput("k_UM_UE_mean_uterine", "Myometrium to Endometrium Mean", value=0.097, min=0, step=0.01), class="half"),
                                    span(numericInput("k_UM_TtoB_mean_uterine", "Myometrium to Blood Mean", value=0.076, min=0, step=0.01), class="half")
                     ),

# * 1.6 Clearance ---------------------------------------------------------


                     bsCollapsePanel(title = "Clearance (L/hr)", value = 5, #id="CLPanel",
                                     span(numericInput("CL_R", "Renal Clearance", 0, min=0, step=0.1), class="half"),
                                     hr(),
                                     span(numericInput("CL_L_mean_clearance", "Luminal Clearance Mean", value=2.5E-5, min=0, step=1e-6), class="half"),
                                     span(numericInput("CL_H_mean_clearance", "Hepatic Clearance Mean", value=7.06, min=0, step=.1), class="half")),

                    

# * 1.7 Solubility and Protein Binding ------------------------------------


                     bsCollapsePanel(title = "Solubility and Binding", value = 6, #id="solubilityPanel",
                                     span(sliderInput("pKa", "pKa", 0, 14, 14, step = 0.1 ), class="half"),
                                     span(numericInput("sol", "Aqueous Solubility (mg/L)", value=2.05, min=0, step = 0.01), class="half"),
                                     hr(),
                                     span(sliderInput("pH_Lumen", "pH Lumen",0, 14, 4.4,  step =0.1 ), class = "half"),
                                     span(sliderInput("pH_Body", "pH Body", 0, 14, 7, step =0.1 ), class = "half"),
                                     hr(),
                                     span(sliderInput("f_u_percent", "Fraction Unbound in Plasma", min=0, max=100, 2, step=0.1 ))),


# * 1.8 Partition Coefficients --------------------------------------------

# bsCollapsePanel(title = "Partition Coefficients", value = 7, #id="KpPanel",
#                 span(numericInput("P_H", "Liver", 1.4, 0,  15,   step =0.1 ), class = "half"),
#                 span(numericInput("P_R", "Kidneys", 1.3, 0, 15, step = 0.1), class = "half"),
#                 hr(),
#                 span(numericInput("P_Lungs", "Lungs",1.7, 0,  15,   step =0.01 ), class = "half"),
#                 span(numericInput("P_VS", "Vaginal Stroma", 0.01, 0,  15,  step =0.01 ), class = "half"),
#                 hr(),
#                 span(numericInput("P_CM", "Cervical Myometrium",1.0, 0,  15,  step =0.01 ), class = "half"),
#                 span(numericInput("P_UM", "Uterine Myometrium", 0.015, 0,  15,  step =0.01 ), class = "half"),
#                 hr(),
#                 span(numericInput("P_B", "Body", 2, 0,  15,  step =0.01 ), class = "half"),
#                 span(numericInput("P_G", "GI", 2.7, 0,  15,  step =0.01 ), class = "half"),
#                 span(numericInput("P_AD", "Adipose Tissue", 6.6, 0, 15, step = 0.01), class = "half")),
                     bsCollapsePanel(title = "Partition Coefficients", value = 7, #id="KpPanel",
                                     span(numericInput("P_H", "Liver", 2.1, 0,  15,   step =0.1 ), class = "half"),
                                     span(numericInput("P_R", "Kidneys", 1.9, 0, 15, step = 0.1), class = "half"),
                                     hr(),
                                     span(numericInput("P_Lungs", "Lungs",2.5, 0,  15,   step =0.01 ), class = "half"),
                                     span(numericInput("P_VS", "Vaginal Stroma", 0.01, 0,  15,  step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("P_CM", "Cervical Stroma",0.01, 0,  15,  step =0.01 ), class = "half"),
                                     span(numericInput("P_UM", "Uterine Myometrium", 0.01, 0,  15,  step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("P_B", "Body", 0.6, 0,  15,  step =0.01 ), class = "half"),
                                     span(numericInput("P_G", "GI", 4, 0,  15,  step =0.01 ), class = "half"),
                                     hr(),
                                     span(numericInput("P_S", "Spleen", 1.1, 0,  15,  step =0.01 ), class = "half"),
                                     span(numericInput("P_AD", "Adipose Tissue", 5, 0, 15, step = 0.01), class = "half"),
                                     hr(),
                                     span(numericInput("BP", "BP Ratio", 1.07, 0, 10, step = 0.01 ), class = "half"))
                  )
              ),
              div(class = "param-block block-sim",
                  h4("Simulation", class = "block-header"),
                  bsCollapse(id = "paramsSim",  multiple = TRUE, open = c(9),


# * 1.9  Plot Options -----------------------------------------------------

                     bsCollapsePanel( title="Plot Options", value=8, #id="plotPanel",
                                      
                                      # * * 1.9.1 Vaginal Lumen ----
                                     div(id = "plot1Ops",
                                       span(numericInput("x_min1", "X Min (Days)", value=0, min=0), class="half"),
                                       span(numericInput("x_max1", "X Max (Days)", value=7), class="half"),
                                       hr(),
                                       span(numericInput("y_min1", "Y Min (mg/L)", value=0, min=0), class="half"),
                                       span(numericInput("y_max1", "Y Max (mg/L)", value=1), class="half"),
                                       hr(),
                                       span(checkboxInput("fit_plot1", "Fit Plot to Data Range", value=1))
                                     ),
                                     
                                     # * * 1.9.2 Vaginal Epithelium ----
                                     hidden(
                                       div(id = "plot2Ops",
                                           span(numericInput("x_min2", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max2", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min2", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max2", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot2", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.3 Vaginal Stroma ----
                                     hidden(
                                       div(id = "plot3Ops",
                                           span(numericInput("x_min3", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max3", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min3", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max3", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot3", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.4 Plasma ----
                                     hidden(
                                       div(id = "plot4Ops",
                                           span(numericInput("x_min4", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max4", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min4", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max4", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot4", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.5 Cervical Fluid ----
                                     hidden(
                                       div(id = "plot5Ops",
                                           span(numericInput("x_min5", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max5", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min5", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max5", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot5", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.6 Cervical Epithelium ----
                                     hidden(
                                       div(id = "plot6Ops",
                                           span(numericInput("x_min6", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max6", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min6", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max6", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot6", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.7 Cervical Myometrium ----
                                     hidden(
                                       div(id = "plot7Ops",
                                           span(numericInput("x_min7", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max7", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min7", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max7", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot7", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.8 Uterine Fluid ----
                                     hidden(
                                       div(id = "plot8Ops",
                                           span(numericInput("x_min8", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max8", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min8", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max8", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot8", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.9 Endometrium ----
                                     hidden(
                                       div(id = "plot9Ops",
                                           span(numericInput("x_min9", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max9", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min9", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max9", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot9", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.10 Uterine Myometrium ----
                                     hidden(
                                       div(id = "plot10Ops",
                                           span(numericInput("x_min10", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max10", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min10", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max10", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot10", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.11 Stroma Blood ----
                                     hidden(
                                       div(id = "plot11Ops",
                                           span(numericInput("x_min11", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max11", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min11", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max11", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot11", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # * * 1.9.12 Depot Compartment ----
                                     hidden(
                                       div(id = "plot12Ops",
                                           span(numericInput("x_min12", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max12", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min12", "Y Min (mg/L)", value=0, min=0), class="half"),
                                           span(numericInput("y_max12", "Y Max (mg/L)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot12", "Fit Plot to Data Range", value=1))
                                       )
                                     ),
                                     
                                     # # * * 1.9.12 Mass Balance ----
                                     hidden(
                                       div(id = "plot13Ops",
                                           span(numericInput("x_min13", "X Min (Days)", value=0, min=0), class="half"),
                                           span(numericInput("x_max13", "X Max (Days)", value=7), class="half"),
                                           hr(),
                                           span(numericInput("y_min13", "Y Min (mg)", value=0, min=0), class="half"),
                                           span(numericInput("y_max13", "Y Max (mg)", value=1), class="half"),
                                           hr(),
                                           span(checkboxInput("fit_plot13", "Fit Plot to Data Range", value=1))
                                       )
                                     )

                                     ),

# * 1.10 Population Parameters --------------------------------------------

                    bsCollapsePanel(title = "Population Parameters", value = 10,
                                    h4("Body Weight"),
                                    span(numericInput("BW_mean", "Mean (kg)", 58.0, min = 35, step = 0.1), class="half"),
                                    span(numericInput("BW_sd", "Standard Deviation (kg)", 8.6, min = 0, step = 0.1), class="half"),
                                    span(numericInput("BW_min", "Min (kg)", 35, min = 0, step = 0.1), class="half"),
                                    span(numericInput("BW_max", "Max (kg)", 100, min = 0, step = 0.1), class="half"),
                                    #span(radioButtons("BW_dist", "Distribution Type", choices=c("Normal", "Log-normal"), selected="Normal", inline=TRUE)),
                                    hr(),
                                    h4("Uterine Rate Constants (1/hr)"),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Endometrium to Uterine Fluid"),
                                        span(numericInput("k_UE_UF_mean", "Mean", value= 0.148, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UE_UF_sd", "SD", value=0.062, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UE_UF_min", "Min", value=0.001, min=0, step =0.001 ), class = "half"),
                                        span(numericInput("k_UE_UF_max", "Max", value=0.148+4*0.062, min=0, step =0.01 ), class = "half")),
                                    hr(),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Endometrium to Myometrium"),
                                        span(numericInput("k_UE_UM_mean", "Mean", value= 0.076, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UE_UM_sd", "SD", value=0.048, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UE_UM_min", "Min", value=0.001, min=0, step =0.001 ), class = "half"),
                                        span(numericInput("k_UE_UM_max", "Max", value=0.076+4*0.048, min=0, step =0.01 ), class = "half")),
                                    hr(),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Myometrium to Endometrium"),
                                        span(numericInput("k_UM_UE_mean", "Mean", value= 0.097, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UM_UE_sd", "SD", value=0.053, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UM_UE_min", "Min", value=0.001, min=0, step =0.001 ), class = "half"),
                                        span(numericInput("k_UM_UE_max", "Max", value=0.097+4*0.053, min=0, step =0.01 ), class = "half")),
                                    hr(),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Myometrium to Blood"),
                                        span(numericInput("k_UM_TtoB_mean", "Mean", value=0.076, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UM_TtoB_sd", "SD", value=0.041, min=0, step =0.01 ), class = "half"),
                                        span(numericInput("k_UM_TtoB_min", "Min", value=0.001, min=0, step =0.001 ), class = "half"),
                                        span(numericInput("k_UM_TtoB_max", "Max", value=0.076+4*0.041, min=0, step =0.01 ), class = "half")),
                                    hr(),
                                    h4("Physiology"),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Pelvic Blood Flow (L/hr)"),
                                        span(numericInput("Q_VS_mean", "Mean", value= 2.44, min=0, step=0.1), class="half"),
                                        span(numericInput("Q_VS_sd", "SD", value= 1.37, min=0, step=0.1), class="half"),
                                        span(numericInput("Q_VS_min", "Min", value=1, min=0, step=0.1), class="half"),
                                        span(numericInput("Q_VS_max", "Max", value=50, min=0, step=0.1), class="half")),
                                    hr(),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Uterine Volume Fraction"),
                                        span(numericInput("F_Uterus_mean", "Mean", value = 1.68E-3, min=0, step=1e-4), class="half"),
                                        span(numericInput("F_Uterus_sd", "SD", value = 6.12E-4, min=0, step=1e-4), class="half"),
                                        span(numericInput("F_Uterus_min", "Min", value=3.25E-4, min=0, step=1e-4), class="half"),
                                        span(numericInput("F_Uterus_max", "Max", value=0.021, min=0, step=1e-4), class="half")),
                                    hr(),
                                    h4("Clearance (L/hr)"),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Luminal Clearance"),
                                        span(numericInput("CL_L_mean", "Mean", value = 2.5E-5, min=0, step=1e-6), class="half"),
                                        span(numericInput("CL_L_sd", "SD", value= 1E-5, min=0, step=1e-6), class="half"),
                                        span(numericInput("CL_L_min", "Min", value=1E-6, min=0, step=1e-6), class="half"),
                                        span(numericInput("CL_L_max", "Max", value=0.01, min=0, step=1e-6), class="half")),
                                    hr(),
                                    div(class="population-var-group",
                                        div(class="population-var-label", "Hepatic Clearance"),
                                        span(numericInput("CL_H_mean", "Mean", value = 7.06, min=0, step =.1 ), class="half"),
                                        span(numericInput("CL_H_sd", "SD", value = 0.46, min=0, step =.1 ), class="half"),
                                        span(numericInput("CL_H_min", "Min", value=0.1, min=0, step =.1 ), class="half"),
                                        span(numericInput("CL_H_max", "Max", value=67.62, min=0, step =.1 ), class="half")),
                                    hr(),
                                    span(numericInput("num_ind", "Number of individuals", 1, min=1, step=1), class="half"),
                                    span(numericInput("pop_seed", "Population simulation seed", 12345, min=0, max=.Machine$integer.max, step=1), class="half")),



# * 1.11 Simulation Options -----------------------------------------------


                     bsCollapsePanel(title = "Simulation Options", value=9,
                                     span(numericInput("SimTime","Simulation Length (days)", 7, 1/24, 1000, step = 1/24),class = "half"),
                                     span(numericInput("SimStep","Simulation Time Step (hours)", 1, .1, 1000, step = .1),class = "half"),
                                     span(checkboxInput("auto_sliders", "Update dose fraction sliders automatically", value=TRUE)),
                                     hr(),
                                     span(downloadButton("sim_data", "Download Simulation Output"), class="half"),
                                     span(actionButton("data_prev", "View Input Dataset"), class="half"),
                                     hr(),
                                     fileInput("obs_file", "Upload Observation Data (.csv)", accept = ".csv"),
                                     checkboxInput("show_user_obs", "Show uploaded observations", value = TRUE),
                                     tags$small("Template units: time in hours; concentration, lower, and upper in mg/L. Leave lower/upper blank if unavailable."),
                                     br(),
                                     downloadButton("obs_template", "Download Observation Template"),
                                     actionButton("clear_obs", "Remove Uploaded Observations", class = "btn-default"),
                                     hr(),
                                     downloadButton("downloadData2", "Download parameters"),
                                     downloadButton("downloadData", "Download Input")
                                     #checkboxInput("autoRun", "Update Simulation Automatically", value=FALSE)
                                     )
                     
                     )
                  )
              )
          ),


# 2. Outputs -----------------------------------------------------------------

            
      div(
        hidden(
          div(id = "user_defined_prompt",
              class = "user-defined-prompt",
              h4("Please provide your input"),
              textOutput("user_defined_prompt_text"))
        ),
        div(id = "simulation_output_tabs",
          tabsetPanel(id="outputTabset",
                    

# * 2.1 Vaginal Lumen --------------------------------------------------------


          tabPanel(style="overflow-y:scroll",
            "Lumen", value="Lumen",
            span(
              plotOutput("plot1", height=plot_height)
            ),
                   fluidRow(
                     column(2, checkboxInput("log_Lumen", "Log scale", value = 0)),
                     column(2, checkboxInput("lumen_total", "Total", value = 1)),
                     column(2, checkboxInput("lumen_solution", "In Solution", value = 0)),
                     column(2, checkboxInput("lumen_unbound", "Unbound", value = 0)),
                     column(2, checkboxInput("lumen_bound", "Protein Bound", value = 0)),
                     column(2, checkboxInput("lumen_precip", "In Suspension", value = 0))
                    ),
                   fluidRow(
                     column(3, checkboxInput("CHIC_cvl_COC", "CHIC Oral", value = 1)),
                     column(3, checkboxInput("CHIC_cvl_IUD", "CHIC IUD", value = 1))
                   ),
            tableOutput("table2"),
                   ),


# * 2.2 Vaginal Epithelium ------------------------------------------------


            tabPanel(style="overflow-y:scroll",
                     "Epithelium", value="Epi",
                     span(
                       plotOutput("plot2", height=plot_height)
                     ), 
                column(3, checkboxInput("log_Epi", "Log scale", value = 0))
                ),

# * 2.3 Vaginal Stroma ----------------------------------------------------


            tabPanel(style="overflow-y:scroll",
              "Stroma", value="Stroma",
              span(
                plotOutput("plot3", height=plot_height)
              ),
                column(3, checkboxInput("log_Stroma", "Log scale", value = 0))
                ),


# * 2.4 Plasma ------------------------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Plasma", value="Plasma",
                   span(plotOutput("plot4", height=plot_height)),
                   span(
                     column(3, checkboxInput("log_Plasma", "Log scale", value = FALSE),
                            checkboxInput("Plasma_error", "Error bars", value = TRUE)),
                     column(9, selectizeInput("Plasma_selector", "Dataset",
                                           list("CHIC Oral"="CHIC_COC",
                                                "CHIC IUD"="CHIC_IUD",
                                                "He Hungarian Pill"="He_HU",
                                                "He Chinese Pill"="He_CN",
                                                "Tremblay A"="Tremblay_A",
                                                "Tremblay B"="Tremblay_B",
                                                "Tremblay C"="Tremblay_C",
                                                "Landgren"="Landgren",
                                                "Johansson A"="Johansson_A",
                                                "Johansson B"="Johansson_B",
                                                "Johansson C"="Johansson_C",
                                                "Kook"="Kook",
                                                "FDA Mirena"="FDA_Mirena",
                                                "FDA Kyleena"="FDA_Kyleena",
                                                "FDA Skyla 1"="FDA_Skyla_1",
                                                "FDA Skyla 2"="FDA_Skyla_2",
                                                "Nilsson IUD"="Nilsson_IUD",
                                                "Nilsson Oral"="Nilsson_Oral",
                                                "Devoto Oral"="Devoto_Oral",
                                                "Devoto Vaginal"="Devoto_Vaginal"
                                                ), multiple=TRUE, width="200%"))
                   )
          ),


# * 2.5 Cervical Fluid ----------------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Cervical Fluid", value="CF",
                   span(
                     plotOutput("plot5", height=plot_height)
                   ),
                   column(3, checkboxInput("log_CF", "Log scale", value = 0))
          ),


# * 2.6 Cervical Epithelium -----------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Cervical Epithelium", value="CE",
                   span(
                     plotOutput("plot6", height=plot_height)
                   ),
                   column(3, checkboxInput("log_CE", "Log scale", value = 0))
          ),


# * 2.7 Cervical Myometrium -------------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Cervical Myometrium", value="CM",
                   span(
                     plotOutput("plot7", height=plot_height)
                   ),
                   column(3, checkboxInput("log_CM", "Log scale", value = 0))
          ),


# * 2.8 Uterine Fluid -----------------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Uterine Fluid", value="UF",
                   span(
                     plotOutput("plot8", height=plot_height)
                   ),
                   column(3, checkboxInput("log_UF", "Log scale", value = 0))
          ),


# * 2.9 Endometrium -------------------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Endometrium", value="UE",
                   span(
                     plotOutput("plot9", height=plot_height)
                   ),
                   column(3, checkboxInput("log_UE", "Log scale", value = 0)),
                   column(3, checkboxInput("UE_error", "Error bars", value = 1)),
                   span(
                     column(3, checkboxInput("Nilsson_IUD_Endo", "Nilsson IUD", value = 0)),
                     column(3, checkboxInput("Nilsson_Oral_Endo", "Nilsson Oral", value = 0)),
                     column(3, checkboxInput("Devoto_Oral_Endo", "Devoto Oral", value=0)),
                     column(3, checkboxInput("Devoto_Vaginal_Endo", "Devoto Vaginal", value=0))
                   )
          ),


# * 2.10 Uterine Myometrium -----------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Uterine Myometrium", value="UM",
                   span(
                     plotOutput("plot10", height=plot_height)
                   ),
                   column(3, checkboxInput("log_UM", "Log scale", value = 0)),
                   column(3, checkboxInput("UM_error", "Error Bars", value = 1)),
                   span(
                     column(3, checkboxInput("Nilsson_IUD_Myo", "Nilsson IUD", value = 0)),
                     column(3, checkboxInput("Nilsson_Oral_Myo", "Nilsson Oral", value = 0))
                   )
          ),


# * 2.11 Stroma Blood -----------------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Stroma Blood", value="SM",
                   span(
                     plotOutput("plot11", height=plot_height)
                   ),
                   column(3, checkboxInput("log_SB", "Log scale", value = 0))
          ),


# * 2.12 Depot Compartment ------------------------------------------------


          tabPanel(style="overflow-y:scroll",
                   "Depot Compartment", value="Depot",
                   span(
                     plotOutput("plot12", height=plot_height)
                   ),
                   column(3, checkboxInput("log_depot", "Log scale", value = 0))),

# * 2.13 Mass Balance Check -----------------------------------------------------


# tabPanel(style="overflow-y:scroll",
#          "Mass Balance", value="MB",
#          span(
#            plotOutput("plot13", height=plot_height)
#          ),
#          column(3, checkboxInput("log_MB", "Log scale", value = 0))
# ),




# * 2.13 Input Dataset ----------------------------------------------------


          tabPanel("Input Dataset",
                   column(12,DT::dataTableOutput("table1"),
                          style="height=600px; overflow-y: scroll; overflow-x:scroll")),
          tabPanel("Uploaded Observations",
                   column(12,DT::dataTableOutput("obs_preview"),
                          style="height=600px; overflow-y: scroll; overflow-x:scroll"))
          )
        ),
        class = "output-container")

      ),
  includeCSS("www/style.css")
)


# Server ------------------------------------------------------------------


server <- function(input, output,session) {
  
  # Stop the app when the browser tab is closed
  session$onSessionEnded(stopApp)
  
  userDefinedReady <- reactiveVal(FALSE)
  userDefinedPrompt <- reactiveVal("Please provide your input, then click Run Simulation.")
  
  output$user_defined_prompt_text <- renderText({
    userDefinedPrompt()
  })


# 3. Simulation -----------------------------------------------------------




   mod <- mread("PBPK", project = "www")#MI
    
    #autoRunState <- reactive(input$runSim)
    
    # # Function to replace NULL values with zero
    # rep_null <- function(test_param) {
    #   if(is.null(test_param)) {
    #     new_param <- 0
    #   } else {
    #     new_param <- test_param
    #   }
    #   return(new_param)
    # }
    
    # Note: all durations and rates are now in units of hours
    results <- eventReactive(input$runSim,{
      
      # Create dependency on the "Run Simulation" button
      #input$runSim
      
      if(isTRUE(input$study == "User_Defined")) {
        missing_message <- user_defined_missing_inputs()
        validate(
          need(is.null(missing_message), missing_message)
        )
        userDefinedReady(TRUE)
      }
      
      # validate(
      #   need(near(slider_sum(), 1.00), paste0("Error: Current total dose input fraction is ", slider_sum())),
      #   if(input$strict_dur == TRUE){
      #     if(input$input_seq == "Sequential") {
      #       need(input$dur_1 > (input$tlag_1+input$tlag_2+input$tlag_3),
      #            "Error: When simulating removal of dosing device with sequential phases, sum of lag times must be smaller than dose duration.")
      #     }
      #     else {
      #       need(input$dur_1 > input$tlag_1 & input$dur_1 > input$tlag_2 & input$dur_1 > input$tlag_3,
      #            "Error: When simulating removal of dosing device with simultaneous phases, all lag times must be smaller than dose duration.")
      #     }
      #   }
      # )
# * 3.1 Parameter List ----
      
      # Unit conversions
      # Final units should be ug, L, and h
      # Most conversions are days -> h, but zero-order rate has to be converted from ug/time -> mg/time
      param_converted <- list()
      for (i in 1:3){
        scalevar <- input[[paste("input",i,"scale",sep="_")]]
        if(scalevar=="Days"){
          param_converted[paste0("dur_",i)] <- rep_null(input[[paste0("dur_",i)]])*24
          param_converted[paste0("td_",i)] <- rep_null(input[[paste0("td_",i)]])*24
          param_converted[paste0("tlag_",i)] <- rep_null(input[[paste0("tlag_",i)]])*24
          param_converted[paste0("zo_rate_",i)] <- rep_null(input[[paste0("zo_rate_",i)]])/24
          param_converted[paste0("fo_rate_",i)] <- rep_null(input[[paste0("fo_rate_",i)]])/24
          param_converted[paste0("sq_rate_",i)] <- rep_null(input[[paste0("sqrt_rate_",i)]])/sqrt(24)
        }
        else if(scalevar=="Hours"){
          param_converted[paste0("dur_",i)] <- rep_null(input[[paste0("dur_",i)]])
          param_converted[paste0("td_",i)] <- rep_null(input[[paste0("td_",i)]])
          param_converted[paste0("tlag_",i)] <- rep_null(input[[paste0("tlag_",i)]])
          param_converted[paste0("zo_rate_",i)] <- rep_null(input[[paste0("zo_rate_",i)]])
          param_converted[paste0("fo_rate_",i)] <- rep_null(input[[paste0("fo_rate_",i)]])
          param_converted[paste0("sq_rate_",i)] <- rep_null(input[[paste0("sqrt_rate_",i)]])
        }
        
      }
      param_converted["dose"] <- input$dose_total*1000


        parameters <- list(
        dur1 = input$dur_1,
        dur2 = input$dur_2,
        dur3 = input$dur_3,
        F = input$dose_F,
        f1 = input$frac_1,
        f2 = input$frac_2,
        f3 = input$frac_3,
        dose = input$dose_total,
        k_LE = input$k_LE,
        k_EL = input$k_EL,
        k_EVS = input$k_EVS,
        k_VSE = input$k_VSE,
        k_VS_TtoB = input$k_VS_TtoB,
        k_VS_BtoT = input$k_VS_BtoT,
        k_activeEtoS = input$k_activeEtoS,
        k_active_StrTtoB = input$k_active_StrTtoB,
        # CL_H = input$CL_H,
        CL_R = input$CL_R,
        P_H = input$P_H,
        P_R = input$P_R,
        P_B = input$P_B,
        P_G = input$P_G,
        P_S = input$P_S,
        P_AD = input$P_AD,
        P_Lungs = input$P_Lungs,
        P_VS = input$P_VS,
        pH_Lumen = input$pH_Lumen,
        pH_Body = input$pH_Body,
        pKa = input$pKa,
        solubility = input$sol,
        f_unbound = input$f_u_percent/100,
        k_L_CF = input$k_L_CF,
        k_CF_L = input$k_CF_L,
        k_VE_CE = input$k_VE_CE,
        k_CE_VE = input$k_CE_VE,
        k_VS_CM = input$k_VS_CM,
        k_CM_VS = input$k_CM_VS,
        k_CF_CE = input$k_CF_CE,
        k_CE_CF = input$k_CE_CF,
        k_CE_CM = input$k_CE_CM,
        k_CM_CE = input$k_CM_CE,
        k_CM_TtoB = input$k_CM_TtoB,
        k_CM_BtoT = input$k_CM_BtoT,
        k_CF_UF = input$k_CF_UF,
        k_UF_CF = input$k_UF_CF,
        k_CE_UE = input$k_CE_UE,
        k_UE_CE = input$k_UE_CE,
        k_CM_UM = input$k_CM_UM,
        k_UM_CM = input$k_UM_CM,
        k_UF_UE = input$k_UF_UE,
        #k_UE_UF = input$k_UE_UF,
        #k_UE_UM = input$k_UE_UM,
        #k_UM_UE = input$k_UM_UE,
        #k_UM_TtoB = input$k_UM_TtoB,
        k_UM_BtoT = input$k_UM_BtoT,
        P_CM = input$P_CM,
        P_UM = input$P_UM,
        ZO1 = input$zo_rate_1,
        ZO2 = input$zo_rate_2,
        ZO3 = input$zo_rate_3,
        FO1 = input$fo_rate_1,
        FO2 = input$fo_rate_2,
        FO3 = input$fo_rate_3,
        SQ1 = input$sqrt_rate_1,
        SQ2 = input$sqrt_rate_2,
        SQ3 = input$sqrt_rate_3,
        TD1 = input$td_1,
        TD2 = input$td_2,
        TD3 = input$td_3,
        lag1 = input$tlag_1,
        lag2 = input$tlag_2,
        lag3 = input$tlag_3,
        #CL_L = input$CL_L
        P_Heart = input$P_Heart,
        P_Bone = input$P_Bone,
        P_Muscle = input$P_Muscle,
        P_Skin = input$P_Skin,
        BP = input$BP,
        population_seed = input$pop_seed
        )
        
        
        # Update parameter values with converted units
        for (i in 1:3){
          parameters[paste0("dur",i)] <- param_converted[paste0("dur_",i)]
          parameters[paste0("TD",i)] <- param_converted[paste0("td_",i)]
          parameters[paste0("lag",i)] <- param_converted[paste0("tlag_",i)]
          parameters[paste0("ZO",i)] <- param_converted[paste0("zo_rate_",i)]
          parameters[paste0("FO",i)] <- param_converted[paste0("fo_rate_",i)]
          parameters[paste0("SQ",i)] <- param_converted[paste0("sq_rate_",i)]
        }
        parameters["dose"] <- param_converted["dose"]
        
        # Set flags for route of administration
        if(input$admin_route=="Oral") {
          parameters["G_Flag"] = 1
          parameters["U_Flag"] = 0
          parameters["V_Flag"] = 0
        }
        
        if(input$admin_route=="Uterine") {
          parameters["G_Flag"] = 0
          parameters["U_Flag"] = 1
          parameters["V_Flag"] = 0
        }
        
        if(input$admin_route=="Vaginal") {
          parameters["G_Flag"] = 0
          parameters["U_Flag"] = 0
          parameters["V_Flag"] = 1
        }
        
        # Set flag for simultaneous vs sequential dosing
        parameters["Simult"] <- switch(input$input_seq,
                                       "Simultaneous" = 1,
                                       "Sequential" = 0
        )
        
        # Function to replace NULL values with 0
        # rep_null <- function(test_param) {
        #   if(is.null(test_param)) {
        #     new_param <- 0
        #   } else {
        #     new_param <- test_param
        #   }
        #   return(new_param)
        # }
        
       
        
        
        # # Convert durations to hours
        # for (i in 1:3){
        #   scalevar <- input[[paste("input",i,"scale",sep="_")]]
        #   if(scalevar=="Days"){
        #     parameters[paste0("dur",i)] <- rep_null(input[[paste0("dur_",i)]])*24
        #     parameters[paste0("TD",i)] <- rep_null(input[[paste0("td_",i)]])*24
        #     parameters[paste0("lag",i)] <- rep_null(input[[paste0("tlag_",i)]])*24
        #     parameters[paste0("ZO",i)] <- rep_null(input[[paste0("zo_rate_",i)]])/24
        #     parameters[paste0("FO",i)] <- rep_null(input[[paste0("fo_rate_",i)]])/24
        #     parameters[paste0("SQ",i)] <- rep_null(input[[paste0("sqrt_rate_",i)]])/sqrt(24)
        #   }
        # 
        # }
        

        
        ## Create event data set based on user input
        # Specify dosing compartments
        # dose_cmt <- switch(input$admin_route,
        #                    "Oral" = "GUT",
        #                    "Uterine" = "U_DEPOT",
        #                    "Vaginal" = "V_DEPOT"
        # )
        
# * 3.2 Simulation Execution ----
        
        #ev_set <- expand.ev(ID=1, amt=input$dose_total, ii=input$II, addl=input$num_dose-1, cmt=dose_cmt)
        
        #print(parameters)

        
        parameters <- parameters %>% lapply(rep_null)
        

        input_set <- inputData() %>% as.data.frame()
        
        mod <- update(mod, maxsteps=8e5)
          
        out <- mod %>%
            param(parameters) %>%
            data_set(input_set) %>%
            mrgsim(end = 24 * input$SimTime, delta=input$SimStep, nocb = FALSE) %>%     # Convert  SimTime from Days to hours
            as.data.frame() %>% filter(time<= 24 * input$SimTime)
        
        #IMPORTANT NOTE: by default, mrgsolve will carry the value for the next observation backwards when dealing with
        # time-varying covariates. This causes problems when a dose is removed after a set duration, as the input dataset will set all of
        # the release rates to zero when the dose is removed. If nocb = TRUE is set in mrgsim(), these zeroed rates will be carried backward
        # to the previous time point where a rate was specified, which is usually t = 0. The result is that the release rates will be correct 
        # at time = 0, then immediately be shut off, leaving all of the drug stuck in the depot compartment.
        
        out$time <- out$time/24 #Convert output back to days
        
        
        return(out)
        
        
        
    })
    

# 4. Utility Functions ----------------------------------------------------



    # Function to replace NULL values with zero
    rep_null <- function(test_param) {
      if(is.null(test_param)) {
        new_param <- 0
      } else {
        new_param <- test_param
      }
      return(new_param)
    }
    
    observation_compartment_map <- c(
      VF = "Lumen",
      VE = "Epi",
      VM = "Stroma",
      Plasma = "Plasma",
      CF = "CF",
      CE = "CE",
      CM = "CM",
      UF = "UF",
      UE = "UE",
      UM = "UM"
    )
    
    observation_template <- data.frame(
      time = c(0, 24, 48, 24, 48, 72),
      compartment = c("Plasma", "Plasma", "Plasma", "VF", "UE", "UM"),
      concentration = c(0.000, 0.150, 0.220, 0.003, 0.450, 0.620),
      lower = c(NA, 0.100, 0.180, NA, NA, NA),
      upper = c(NA, 0.200, 0.260, NA, NA, NA),
      dataset = rep("Example study", 6),
      stringsAsFactors = FALSE
    )
    
    output$obs_template <- downloadHandler(
      filename = function(){"observation_template.csv"},
      content = function(file) {
        write.csv(observation_template, file, row.names = FALSE, na = "")
      }
    )
    
    observationUploadCleared <- reactiveVal(FALSE)
    
    observeEvent(input$obs_file, {
      observationUploadCleared(FALSE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$clear_obs, {
      observationUploadCleared(TRUE)
      reset("obs_file")
      updateCheckboxInput(session, "show_user_obs", value = TRUE)
      for (plot_index in 1:12) {
        updateCheckboxInput(session, paste0("fit_plot", plot_index), value = TRUE)
      }
      updateTabsetPanel(session, "outputTabset", selected = "Plasma")
    })
    
    uploadedObservations <- reactive({
      if (isTRUE(observationUploadCleared()) || is.null(input$obs_file)) {
        return(NULL)
      }
      
      obs_raw <- read.csv(input$obs_file$datapath, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      names(obs_raw) <- trimws(names(obs_raw))
      
      required_cols <- c("time", "compartment", "concentration")
      missing_cols <- setdiff(required_cols, names(obs_raw))
      validate(
        need(length(missing_cols) == 0, paste("Uploaded observation file is missing required column(s):", paste(missing_cols, collapse = ", ")))
      )
      
      optional_cols <- c("lower", "upper", "dataset")
      for (col_name in optional_cols) {
        if (!col_name %in% names(obs_raw)) {
          obs_raw[[col_name]] <- NA
        }
      }
      
      obs_raw$compartment <- trimws(obs_raw$compartment)
      invalid_compartments <- setdiff(unique(obs_raw$compartment), names(observation_compartment_map))
      validate(
        need(length(invalid_compartments) == 0,
             paste("Invalid compartment label(s):", paste(invalid_compartments, collapse = ", "),
                   ". Allowed labels are:", paste(names(observation_compartment_map), collapse = ", ")))
      )
      
      obs_clean <- data.frame(
        time = suppressWarnings(as.numeric(obs_raw$time)),
        time_days = suppressWarnings(as.numeric(obs_raw$time)) / 24,
        compartment = obs_raw$compartment,
        plot_compartment = unname(observation_compartment_map[obs_raw$compartment]),
        concentration = suppressWarnings(as.numeric(obs_raw$concentration)),
        conc_mg_L = suppressWarnings(as.numeric(obs_raw$concentration)),
        lower_mg_L = suppressWarnings(as.numeric(obs_raw$lower)),
        upper_mg_L = suppressWarnings(as.numeric(obs_raw$upper)),
        dataset = ifelse(is.na(obs_raw$dataset) | trimws(obs_raw$dataset) == "", "Uploaded observations", obs_raw$dataset),
        stringsAsFactors = FALSE
      )
      
      validate(
        need(all(is.finite(obs_clean$time) & obs_clean$time >= 0), "Observation time must be numeric hours greater than or equal to 0."),
        need(all(is.finite(obs_clean$concentration)), "Observation concentration must be numeric and in mg/L.")
      )
      
      lower_provided <- !is.na(obs_raw$lower)
      upper_provided <- !is.na(obs_raw$upper)
      bounds_provided <- lower_provided & upper_provided
      validate(
        need(all(!lower_provided | is.finite(obs_clean$lower_mg_L)), "Observation lower values must be numeric mg/L when provided."),
        need(all(!upper_provided | is.finite(obs_clean$upper_mg_L)), "Observation upper values must be numeric mg/L when provided."),
        need(all(!bounds_provided | obs_clean$lower_mg_L <= obs_clean$upper_mg_L), "Observation lower values must be less than or equal to upper values.")
      )
      
      obs_clean
    })
    
    uploaded_observation_plot_map <- data.frame(
      plot_compartment = c("Lumen", "Epi", "Stroma", "Plasma", "CF", "CE", "CM", "UF", "UE", "UM"),
      plot_index = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      stringsAsFactors = FALSE
    )
    
    simulation_axis_values <- function(plot_compartment) {
      sim_out <- tryCatch(isolate(results()), error = function(e) NULL)
      if (is.null(sim_out) || nrow(sim_out) == 0) {
        return(list(x = numeric(0), y = numeric(0)))
      }
      
      mass_conv_factor <- 1e-3
      y_values <- switch(
        plot_compartment,
        "Lumen" = sim_out$C_totLUMEN * mass_conv_factor,
        "Epi" = sim_out$C_totEPI * mass_conv_factor,
        "Stroma" = sim_out$C_totSTROMA * mass_conv_factor,
        "Plasma" = sim_out$VEINS * mass_conv_factor / input$BP,
        "CF" = sim_out$CERVIX_FLUID * mass_conv_factor,
        "CE" = sim_out$CERVIX_EPITHELIUM * mass_conv_factor,
        "CM" = sim_out$CERVIX_MYOMETRIUM * mass_conv_factor,
        "UF" = sim_out$UTERUS_FLUID * mass_conv_factor,
        "UE" = sim_out$UTERUS_ENDOMETRIUM * mass_conv_factor,
        "UM" = sim_out$UTERUS_MYOMETRIUM * mass_conv_factor,
        numeric(0)
      )
      
      list(
        x = sim_out$time[is.finite(sim_out$time)],
        y = y_values[is.finite(y_values)]
      )
    }
    
    observeEvent(uploadedObservations(), {
      obs <- uploadedObservations()
      if (is.null(obs) || nrow(obs) == 0) {
        return(NULL)
      }
      
      for (i in seq_len(nrow(uploaded_observation_plot_map))) {
        plot_compartment <- uploaded_observation_plot_map$plot_compartment[i]
        plot_index <- uploaded_observation_plot_map$plot_index[i]
        plot_obs <- obs[obs$plot_compartment == plot_compartment, , drop = FALSE]
        if (nrow(plot_obs) == 0) {
          next
        }
        
        sim_values <- simulation_axis_values(plot_compartment)
        x_values <- c(sim_values$x, plot_obs$time_days)
        x_values <- x_values[is.finite(x_values)]
        y_values <- c(sim_values$y, plot_obs$conc_mg_L, plot_obs$lower_mg_L, plot_obs$upper_mg_L)
        y_values <- y_values[is.finite(y_values)]
        
        if (length(x_values) > 0) {
          updateNumericInput(session, paste0("x_min", plot_index), value = max(0, min(x_values, na.rm = TRUE) * 0.9))
          updateNumericInput(session, paste0("x_max", plot_index), value = max(x_values, na.rm = TRUE) * 1.1)
        }
        
        if (length(y_values) > 0) {
          updateNumericInput(session, paste0("y_min", plot_index), value = max(0, min(y_values, na.rm = TRUE) * 0.9))
          updateNumericInput(session, paste0("y_max", plot_index), value = max(y_values, na.rm = TRUE) * 1.1)
        }
        
        updateCheckboxInput(session, paste0("fit_plot", plot_index), value = FALSE)
      }
    }, ignoreInit = TRUE)
    
    user_obs_for_plot <- function(plot_compartment) {
      if (!isTRUE(input$show_user_obs)) {
        return(NULL)
      }
      
      obs <- uploadedObservations()
      if (is.null(obs)) {
        return(NULL)
      }
      
      obs[obs$plot_compartment == plot_compartment, , drop = FALSE]
    }
    
    output$obs_preview <- renderDataTable({
      obs <- uploadedObservations()
      if (is.null(obs)) {
        return(datatable(
          data.frame(Status = "No observation data uploaded yet."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }
      
      datatable(obs, options = list(pageLength = 10))
    })
    
    # Handler for downloading simulation data
    output$sim_data <- downloadHandler(
      filename = function(){"sim_output.csv"},
      content = function(file) {
        # data_out <- results()
        # data_out <- data_out %>%
        #   rename(time_days=time) %>%
        #   mutate(conc_units = "ug/L")
        write.csv(results(), file, row.names = FALSE)
      }
    )

    output$report_html <- downloadHandler(
      filename = function() {
        paste0("simulation_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        simulation_output <- results()
        if (is.null(simulation_output)) {
          stop("Run a simulation before generating a report.")
        }
        uploaded_observations <- uploadedObservations()

        html_table <- function(data, max_rows = 1000) {
          data <- as.data.frame(data)
          row_count <- nrow(data)
          col_count <- ncol(data)
          shown <- if (row_count > max_rows) utils::head(data, max_rows) else data
          note <- if (row_count > max_rows) {
            paste("Showing first", max_rows, "of", row_count, "rows.")
          } else {
            paste("Showing all", row_count, "rows.")
          }

          header <- paste0(
            "<tr>",
            paste0("<th>", htmltools::htmlEscape(names(shown)), "</th>", collapse = ""),
            "</tr>"
          )
          rows <- apply(shown, 1, function(row) {
            paste0(
              "<tr>",
              paste0("<td>", htmltools::htmlEscape(as.character(row)), "</td>", collapse = ""),
              "</tr>"
            )
          })
          paste0(
            "<p>", note, " Dimensions: ", row_count, " rows x ", col_count, " columns.</p>",
            "<div class='table-wrap'><table>",
            header,
            paste(rows, collapse = ""),
            "</table></div>"
          )
        }

        parameter_descriptions <- function(parameter_names) {
          descriptions <- c(
            dur1 = "Duration of dosing phase 1 (unit: h)",
            dur2 = "Duration of dosing phase 2 (unit: h)",
            dur3 = "Duration of dosing phase 3 (unit: h)",
            F = "Fraction of administered dose available for absorption (unitless)",
            f1 = "Fraction of dose released in dosing phase 1 (unitless)",
            f2 = "Fraction of dose released in dosing phase 2 (unitless)",
            f3 = "Fraction of dose released in dosing phase 3 (unitless)",
            dose = "Total administered dose converted to micrograms (unit: ug)",
            k_LE = "First order rate constant from vaginal lumen fluid to vaginal epithelium (unit: 1/h)",
            k_EL = "First order rate constant from vaginal epithelium to vaginal lumen fluid (unit: 1/h)",
            k_EVS = "First order rate constant from vaginal epithelium to vaginal stroma (unit: 1/h)",
            k_VSE = "First order rate constant from vaginal stroma to vaginal epithelium (unit: 1/h)",
            k_VS_TtoB = "First order rate constant from vaginal stroma tissue to blood (unit: 1/h)",
            k_VS_BtoT = "First order rate constant from blood to vaginal stroma tissue (unit: 1/h)",
            k_activeEtoS = "Active transport rate from vaginal epithelium to stroma (unit: 1/h)",
            k_active_StrTtoB = "Active transport rate from vaginal stroma tissue to blood (unit: 1/h)",
            CL_R = "Renal clearance (unit: L/h)",
            P_H = "Liver partition coefficient (unitless)",
            P_R = "Kidney partition coefficient (unitless)",
            P_B = "Body partition coefficient (unitless)",
            P_G = "Gut partition coefficient (unitless)",
            P_S = "Spleen partition coefficient (unitless)",
            P_AD = "Adipose tissue partition coefficient (unitless)",
            P_Lungs = "Lung partition coefficient (unitless)",
            P_VS = "Vaginal stroma partition coefficient (unitless)",
            pH_Lumen = "Luminal fluid pH (unitless)",
            pH_Body = "Body pH (unitless)",
            pKa = "Drug pKa used for ionization calculations (unitless)",
            solubility = "Aqueous solubility (unit: mg/L)",
            f_unbound = "Fraction of drug unbound (unitless)",
            k_L_CF = "First order rate constant from vaginal lumen fluid to cervical fluid (unit: 1/h)",
            k_CF_L = "First order rate constant from cervical fluid to vaginal lumen fluid (unit: 1/h)",
            k_VE_CE = "First order rate constant from vaginal epithelium to cervical epithelium (unit: 1/h)",
            k_CE_VE = "First order rate constant from cervical epithelium to vaginal epithelium (unit: 1/h)",
            k_VS_CM = "First order rate constant from vaginal stroma to cervical myometrium (unit: 1/h)",
            k_CM_VS = "First order rate constant from cervical myometrium to vaginal stroma (unit: 1/h)",
            k_CF_CE = "First order rate constant from cervical fluid to cervical endometrium (unit: 1/h)",
            k_CE_CF = "First order rate constant from cervical endometrium to cervical fluid (unit: 1/h)",
            k_CE_CM = "First order rate constant from cervical endometrium to cervical myometrium (unit: 1/h)",
            k_CM_CE = "First order rate constant from cervical myometrium to cervical endometrium (unit: 1/h)",
            k_CM_TtoB = "First order rate constant from cervical myometrium tissue to blood (unit: 1/h)",
            k_CM_BtoT = "First order rate constant from blood to cervical myometrium tissue (unit: 1/h)",
            k_CF_UF = "First order rate constant from cervical fluid to uterine fluid (unit: 1/h)",
            k_UF_CF = "First order rate constant from uterine fluid to cervical fluid (unit: 1/h)",
            k_CE_UE = "First order rate constant from cervical endometrium to uterine endometrium (unit: 1/h)",
            k_UE_CE = "First order rate constant from uterine endometrium to cervical endometrium (unit: 1/h)",
            k_CM_UM = "First order rate constant from cervical myometrium to uterine myometrium (unit: 1/h)",
            k_UM_CM = "First order rate constant from uterine myometrium to cervical myometrium (unit: 1/h)",
            k_UF_UE = "First order rate constant from uterine fluid to uterine endometrium (unit: 1/h)",
            k_UE_UF = "First order rate constant from uterine endometrium to uterine fluid (unit: 1/h)",
            k_UE_UM = "First order rate constant from uterine endometrium to uterine myometrium (unit: 1/h)",
            k_UM_UE = "First order rate constant from uterine myometrium to uterine endometrium (unit: 1/h)",
            k_UM_TtoB = "First order rate constant from uterine myometrium tissue to blood (unit: 1/h)",
            k_UM_BtoT = "First order rate constant from blood to uterine myometrium tissue (unit: 1/h)",
            P_CM = "Cervical myometrium partition coefficient (unitless)",
            P_UM = "Uterine myometrium partition coefficient (unitless)",
            ZO1 = "Zero-order release rate for dosing phase 1 (unit: ug/h)",
            ZO2 = "Zero-order release rate for dosing phase 2 (unit: ug/h)",
            ZO3 = "Zero-order release rate for dosing phase 3 (unit: ug/h)",
            FO1 = "First-order release rate for dosing phase 1 (unit: 1/h)",
            FO2 = "First-order release rate for dosing phase 2 (unit: 1/h)",
            FO3 = "First-order release rate for dosing phase 3 (unit: 1/h)",
            SQ1 = "Higuchi square-root release rate for dosing phase 1 (unit: ug/sqrt(h))",
            SQ2 = "Higuchi square-root release rate for dosing phase 2 (unit: ug/sqrt(h))",
            SQ3 = "Higuchi square-root release rate for dosing phase 3 (unit: ug/sqrt(h))",
            TD1 = "Time dependence for dosing phase 1",
            TD2 = "Time dependence for dosing phase 2",
            TD3 = "Time dependence for dosing phase 3",
            lag1 = "Lag time for dosing phase 1 (unit: h)",
            lag2 = "Lag time for dosing phase 2 (unit: h)",
            lag3 = "Lag time for dosing phase 3 (unit: h)",
            P_Heart = "Heart partition coefficient (unitless)",
            P_Bone = "Bone partition coefficient (unitless)",
            P_Muscle = "Muscle partition coefficient (unitless)",
            P_Skin = "Skin partition coefficient (unitless)",
            BP = "Blood-to-plasma concentration ratio (unitless)",
            population_seed = "Random seed used for population simulation (unitless)",
            G_Flag = "Oral administration route flag (unitless)",
            U_Flag = "Uterine administration route flag (unitless)",
            V_Flag = "Vaginal administration route flag (unitless)",
            Simult = "Dosing sequence flag: simultaneous equals 1 and sequential equals 0 (unitless)"
          )

          description_values <- unname(descriptions[parameter_names])
          description_values[is.na(description_values)] <- ""
          description_values
        }
        
        create_report_figures <- function(simulation_output) {
          mass_conv_factor <- 1e-3
          list(
            list(
              title = "Plasma",
              plot_compartment = "Plasma",
              y_data = simulation_output$VEINS * mass_conv_factor / input$BP,
              y_label = "Plasma Concentration [mg/L]",
              log_scale = isTRUE(input$log_Plasma)
            ),
            list(
              title = "Total Vaginal Lumen Fluid",
              plot_compartment = "Lumen",
              y_data = simulation_output$C_totLUMEN * mass_conv_factor,
              y_label = "Lumen Concentration [mg/L]",
              log_scale = isTRUE(input$log_Lumen)
            ),
            list(
              title = "Vaginal Stroma",
              plot_compartment = "Stroma",
              y_data = simulation_output$C_totSTROMA * mass_conv_factor,
              y_label = "Stroma Concentration [mg/L]",
              log_scale = isTRUE(input$log_Stroma)
            ),
            list(
              title = "Cervical Myometrium",
              plot_compartment = "CM",
              y_data = simulation_output$CERVIX_MYOMETRIUM * mass_conv_factor,
              y_label = "Cervical Myometrium Concentration [mg/L]",
              log_scale = isTRUE(input$log_CM)
            ),
            list(
              title = "Uterine Endometrium",
              plot_compartment = "UE",
              y_data = simulation_output$UTERUS_ENDOMETRIUM * mass_conv_factor,
              y_label = "Endometrium Concentration [mg/L]",
              log_scale = isTRUE(input$log_UE)
            ),
            list(
              title = "Uterine Myometrium",
              plot_compartment = "UM",
              y_data = simulation_output$UTERUS_MYOMETRIUM * mass_conv_factor,
              y_label = "Uterine Myometrium Concentration [mg/L]",
              log_scale = isTRUE(input$log_UM)
            )
          )
        }
        
        report_y_range <- function(y_data, log_scale) {
          finite_y <- y_data[is.finite(y_data)]
          if (length(finite_y) == 0) {
            return(c(1e-12, 1))
          }
          
          if (isTRUE(log_scale)) {
            positive_y <- finite_y[finite_y > 0]
            if (length(positive_y) == 0) {
              return(c(1e-12, 1))
            }
            y_min <- max(min(positive_y, na.rm = TRUE) * 0.9, 1e-12)
            y_max <- max(positive_y, na.rm = TRUE) * 1.1
          } else {
            y_min <- min(finite_y, na.rm = TRUE) * 0.9
            y_max <- max(finite_y, na.rm = TRUE) * 1.1
          }
          
          if (!is.finite(y_min) || !is.finite(y_max) || y_min == y_max) {
            y_min <- 0
            y_max <- max(finite_y, na.rm = TRUE) + 1
          }
          
          c(max(y_min, 1e-12), max(y_max, 1e-12))
        }
        
        render_report_figure <- function(fig, simulation_output, num_ind, uploaded_observations = NULL) {
          fig_obs <- NULL
          if (!is.null(uploaded_observations) && "plot_compartment" %in% names(uploaded_observations)) {
            fig_obs <- uploaded_observations[uploaded_observations$plot_compartment == fig$plot_compartment, , drop = FALSE]
          }
          base_y_range <- report_y_range(fig$y_data, fig$log_scale)
          y_range <- range_with_user_observations(
            base_y_range[1],
            base_y_range[2],
            fig_obs,
            fig$log_scale
          )
          x_range <- range(0, max(simulation_output$time, na.rm = TRUE))
          
          par(oma = c(1, 1, 1, 1))
          par(mar = c(5, 6, 4, 2))
          plot_simulated_profile(
            simulation_output,
            fig$y_data,
            x_range,
            y_range,
            fig$log_scale,
            num_ind
          )
          axis(side = 1, cex.axis = 1.1)
          axis(side = 2, las = 1, cex.axis = 1.1)
          box()
          title(main = fig$title)
          mtext(side = 1, "Time [days]", line = 3, cex = 1.1)
          mtext(side = 2, fig$y_label, line = 4, cex = 1.1)
          
          sim_legend <- simulated_profile_legend(num_ind)
          user_legend <- plot_user_observations(
            fig_obs,
            sim_legend$labels,
            sim_legend$col,
            sim_legend$lty,
            sim_legend$pch,
            errorBars = TRUE,
            logScale = fig$log_scale
          )
          legend(
            "topright",
            legend = user_legend$labels,
            col = user_legend$col,
            lty = user_legend$lty,
            pch = user_legend$pch,
            lwd = 2,
            bty = "n",
            x.intersp = 0.7,
            seg.len = 1.4
          )
        }
        
        report_figures_html <- function(report_params) {
          if (!requireNamespace("knitr", quietly = TRUE)) {
            return("<p>Simulation figures could not be embedded because the knitr package is unavailable.</p>")
          }
          
          paste0(
            vapply(report_params$figures, function(fig) {
              image_file <- tempfile(fileext = ".png")
              plot_error <- NULL
              grDevices::png(image_file, width = 900, height = 650, res = 120)
              tryCatch(
                render_report_figure(fig, report_params$simulation_output, report_params$num_ind, report_params$uploaded_observations),
                error = function(e) {
                  plot_error <<- e
                },
                finally = {
                  grDevices::dev.off()
                }
              )
              
              if (!is.null(plot_error)) {
                unlink(image_file)
                return(paste0(
                  "<h3>", htmltools::htmlEscape(fig$title), "</h3>",
                  "<p>Unable to render this figure: ",
                  htmltools::htmlEscape(conditionMessage(plot_error)),
                  "</p>"
                ))
              }
              
              image_uri <- knitr::image_uri(image_file)
              unlink(image_file)
              paste0(
                "<h3>", htmltools::htmlEscape(fig$title), "</h3>",
                "<img class='report-figure' src='", image_uri, "' alt='",
                htmltools::htmlEscape(fig$title), "' />"
              )
            }, character(1)),
            collapse = ""
          )
        }

        write_fallback_report <- function(report_params, output_file) {
          html <- paste0(
            "<!DOCTYPE html><html><head><meta charset='utf-8'>",
            "<title>PBPK Simulation Report</title>",
            "<style>",
            "body{font-family:Arial,sans-serif;margin:30px;color:#222;}",
            "h1{color:#337ab7;} h2{border-bottom:1px solid #ddd;padding-bottom:4px;}",
            "table{border-collapse:collapse;font-size:12px;} th,td{border:1px solid #ddd;padding:4px 6px;}",
            "th{background:#f5f5f5;} .table-wrap{overflow-x:auto;margin-bottom:24px;}",
            ".download-link{display:inline-block;margin:8px 0 16px 0;padding:10px 14px;background:#337ab7;color:#fff;text-decoration:none;border-radius:4px;font-weight:bold;}",
            ".report-figure{display:block;max-width:100%;margin:8px 0 24px 0;border:1px solid #ddd;}",
            "pre{background:#f7f7f7;border:1px solid #ddd;padding:10px;overflow-x:auto;}",
            "</style></head><body>",
            "<h1>PBPK Simulation Report</h1>",
            "<h2>Scenario Summary</h2>",
            "<p><strong>Generated at:</strong> ", htmltools::htmlEscape(report_params$generated_at), "</p>",
            "<p><strong>Population simulation seed:</strong> ", htmltools::htmlEscape(as.character(report_params$population_seed)), "</p>",
            "<p><strong>Model file:</strong> www/PBPK.cpp</p>",
            "<h2>Converted Model Parameters</h2>",
            "<p><a class='download-link' href='parameters.csv' download='parameters.csv'>Download parameters.csv</a></p>",
            html_table(report_params$parameters, max_rows = Inf),
            "<h2>Generated Input Dataset</h2>",
            "<p><a class='download-link' href='input_dataset.csv' download='input_dataset.csv'>Download input_dataset.csv</a></p>",
            html_table(report_params$input_data, max_rows = 20),
            "<h2>Simulation Output</h2>",
            "<p><a class='download-link' href='simulation_output.csv' download='simulation_output.csv'>Download full simulation_output.csv</a></p>",
            html_table(report_params$simulation_output, max_rows = 20),
            if (!is.null(report_params$uploaded_observations)) {
              paste0(
                "<h2>Uploaded Observations</h2>",
                "<p><a class='download-link' href='uploaded_observations.csv' download='uploaded_observations.csv'>Download uploaded_observations.csv</a></p>",
                html_table(report_params$uploaded_observations, max_rows = 20)
              )
            } else {
              ""
            },
            "<h2>Simulation Figures</h2>",
            report_figures_html(report_params),
            "<h2>Reproducibility Notes</h2>",
            "<p>For population simulations, use the listed population seed and retain the generated input dataset above to reproduce the exact simulated population scenario.</p>",
            "<h2>R Session Info</h2><pre>",
            htmltools::htmlEscape(paste(report_params$session_info, collapse = "\n")),
            "</pre></body></html>"
          )
          writeLines(html, output_file)
        }

        converted_parameters <- unlist(pmt(), use.names = TRUE)

        report_params <- list(
          generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          parameters = data.frame(
            Parameter = names(converted_parameters),
            Description = parameter_descriptions(names(converted_parameters)),
            Value = as.character(converted_parameters),
            stringsAsFactors = FALSE
          ),
          input_data = inputData(),
          simulation_output = simulation_output,
          uploaded_observations = uploaded_observations,
          population_seed = input$pop_seed,
          num_ind = input$num_ind,
          figures = create_report_figures(simulation_output),
          session_info = capture.output(sessionInfo())
        )

        report_dir <- tempfile("simulation_report_")
        dir.create(report_dir)
        report_html <- file.path(report_dir, "report.html")
        report_csv <- file.path(report_dir, "simulation_output.csv")
        input_csv <- file.path(report_dir, "input_dataset.csv")
        parameters_csv <- file.path(report_dir, "parameters.csv")
        uploaded_obs_csv <- file.path(report_dir, "uploaded_observations.csv")
        write.csv(simulation_output, report_csv, row.names = FALSE)
        write.csv(report_params$input_data, input_csv, row.names = FALSE)
        write.csv(report_params$parameters, parameters_csv, row.names = FALSE)
        if (!is.null(report_params$uploaded_observations)) {
          write.csv(report_params$uploaded_observations, uploaded_obs_csv, row.names = FALSE)
        }

        if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
          template <- file.path("www", "report_template.Rmd")
          temp_report <- file.path(report_dir, "report_template.Rmd")
          file.copy(template, temp_report, overwrite = TRUE)

          rmarkdown::render(
            input = temp_report,
            output_file = "report.html",
            output_dir = report_dir,
            params = report_params,
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
        } else {
          write_fallback_report(report_params, report_html)
        }

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(report_dir)
        report_files <- c("report.html", "parameters.csv", "simulation_output.csv", "input_dataset.csv")
        if (!is.null(report_params$uploaded_observations)) {
          report_files <- c(report_files, "uploaded_observations.csv")
        }
        utils::zip(zipfile = file, files = report_files)
      }
    )
    


# 5. Input Dataset --------------------------------------------------------



     inputData<- reactive({
         
         # # Function to replace NULL values with zero
         # rep_null <- function(test_param) {
         #   if(is.null(test_param)) {
         #     new_param <- 0
         #   } else {
         #     new_param <- test_param
         #   }
         #   return(new_param)
         # }
         
         # Unit conversions
         # Final units should be ug, L, and h
         param_converted <- list()
         for (i in 1:3){
           scalevar <- input[[paste("input",i,"scale",sep="_")]]
           if(scalevar=="Days"){
             param_converted[paste0("dur_",i)] <- rep_null(input[[paste0("dur_",i)]])*24
             param_converted[paste0("td_",i)] <- rep_null(input[[paste0("td_",i)]])*24
             param_converted[paste0("tlag_",i)] <- rep_null(input[[paste0("tlag_",i)]])*24
             param_converted[paste0("zo_rate_",i)] <- rep_null(input[[paste0("zo_rate_",i)]])/24
             param_converted[paste0("fo_rate_",i)] <- rep_null(input[[paste0("fo_rate_",i)]])/24
             param_converted[paste0("sq_rate_",i)] <- rep_null(input[[paste0("sqrt_rate_",i)]])/sqrt(24)
           }
           else if(scalevar=="Hours"){
             param_converted[paste0("dur_",i)] <- rep_null(input[[paste0("dur_",i)]])
             param_converted[paste0("td_",i)] <- rep_null(input[[paste0("td_",i)]])
             param_converted[paste0("tlag_",i)] <- rep_null(input[[paste0("tlag_",i)]])
             param_converted[paste0("zo_rate_",i)] <- rep_null(input[[paste0("zo_rate_",i)]])
             param_converted[paste0("fo_rate_",i)] <- rep_null(input[[paste0("fo_rate_",i)]])
             param_converted[paste0("sq_rate_",i)] <- rep_null(input[[paste0("sqrt_rate_",i)]])
           }
         }
         param_converted["dose"] <- input$dose_total*1000
         
         # Convert the admin route to the corresponding depot compartment
         dose_cmt <- switch(input$admin_route,
                            "Oral" = 21,
                            "Uterine" = 23,
                            "Vaginal" = 22
         )
         
         
        validate(
          need(input$num_dose >= 1, "Number of doses must be at least 1."),
          need(input$num_dose == floor(input$num_dose), "Number of doses must be an integer."),
          need(input$num_dose <= 1 || input$II > 0, "Inter-dose interval must be greater than zero for repeated dosing.")
        )
        dose_count <- as.integer(input$num_dose)
        
        # Create vector of explicit dose times. Each dose gets its own full
        # multiphase event schedule, so addl/ii are not used here.
        dose_times <- seq(0, by=input$II, length.out=dose_count)
     
        addl_vals <- 0
        ii_vals <- 0
         
         
         # Create vector of ID values
         ID_vals <- seq(1, to=input$num_ind)
         
         shiny::req(input$pop_seed)
         set.seed(as.integer(input$pop_seed))
         
         if(input$num_ind > 1) {
           validate(
             need(input$BW_min < input$BW_max, "Population Body Weight Min must be less than Max."),
             need(input$k_UE_UF_min < input$k_UE_UF_max, "Population Endometrium to Uterine Fluid Min must be less than Max."),
             need(input$k_UE_UM_min < input$k_UE_UM_max, "Population Endometrium to Myometrium Min must be less than Max."),
             need(input$k_UM_UE_min < input$k_UM_UE_max, "Population Myometrium to Endometrium Min must be less than Max."),
             need(input$k_UM_TtoB_min < input$k_UM_TtoB_max, "Population Myometrium to Blood Min must be less than Max."),
             need(input$Q_VS_min < input$Q_VS_max, "Population Pelvic Blood Flow Min must be less than Max."),
             need(input$F_Uterus_min < input$F_Uterus_max, "Population Uterine Volume Fraction Min must be less than Max."),
             need(input$CL_L_min < input$CL_L_max, "Population Luminal Clearance Min must be less than Max."),
             need(input$CL_H_min < input$CL_H_max, "Population Hepatic Clearance Min must be less than Max.")
           )
         }
         
         # Create vector of simulated body weights
         if(input$num_ind > 1) {
           weight_vals <- rtruncnorm(input$num_ind, a = input$BW_min, b = input$BW_max, input$BW_mean, input$BW_sd)
         } else {
           weight_vals <- input$BW_mean
         }
         
         # Create vector of k_UE_UF
         if(input$num_ind > 1) {
           k_UE_UF_val <- rtruncnorm(input$num_ind, a = input$k_UE_UF_min, b = input$k_UE_UF_max, input$k_UE_UF_mean, input$k_UE_UF_sd)
         } else {
           k_UE_UF_val <- input$k_UE_UF_mean
         }
         
         # Create vector of k_UE_UM 
         if(input$num_ind > 1) {
           k_UE_UM_val <- rtruncnorm(input$num_ind, a = input$k_UE_UM_min, b = input$k_UE_UM_max, input$k_UE_UM_mean, input$k_UE_UM_sd)
         } else {
           k_UE_UM_val <- input$k_UE_UM_mean
         }
         
         # Create vector of k_UM_UE 
         if(input$num_ind > 1) {
           k_UM_UE_val <- rtruncnorm(input$num_ind, a = input$k_UM_UE_min, b = input$k_UM_UE_max, input$k_UM_UE_mean, input$k_UM_UE_sd)
         } else {
           k_UM_UE_val <- input$k_UM_UE_mean
         }
         
         # Create vector of k_UM_TtoB 
         if(input$num_ind > 1) {
           k_UM_TtoB_val <- rtruncnorm(input$num_ind, a = input$k_UM_TtoB_min, b = input$k_UM_TtoB_max, input$k_UM_TtoB_mean, input$k_UM_TtoB_sd)
         } else {
           k_UM_TtoB_val <- input$k_UM_TtoB_mean
         }
         
         # Create vector of uterine blood flow
         if(input$num_ind > 1) {
           Q_VS_val <- rtruncnorm(input$num_ind, a = input$Q_VS_min, b = input$Q_VS_max, input$Q_VS_mean, input$Q_VS_sd)
         } else {
           Q_VS_val <- input$Q_VS_mean
         }
         
         # Create vector of uterine volume
         if(input$num_ind > 1) {
           F_Uterus_val <- rtruncnorm(input$num_ind, a = input$F_Uterus_min, b = input$F_Uterus_max, input$F_Uterus_mean, input$F_Uterus_sd)
         } else {
           F_Uterus_val <- input$F_Uterus_mean
         }
         
         # Create vector of luminal cl
         if(input$num_ind > 1) {
           CL_L_val <- rtruncnorm(input$num_ind, a = input$CL_L_min, b = input$CL_L_max, input$CL_L_mean, input$CL_L_sd)
         } else {
           CL_L_val <- input$CL_L_mean
         }
         
         # Create vector of hepatic cl
         if(input$num_ind > 1) {
           CL_H_val <- rtruncnorm(input$num_ind, a = input$CL_H_min, b = input$CL_H_max, input$CL_H_mean, input$CL_H_sd)
         } else {
           CL_H_val <- input$CL_H_mean
         }
         
         
         # Bind ID and weight columns
         weight_comb <- cbind(ID_vals, weight_vals)
         
         
         # Create initial data frame containing the rate constants and body weights 
         init_rates <- data.frame(
                                  ZO1 = param_converted$zo_rate_1, FO1 = param_converted$fo_rate_1, SQ1 = param_converted$sq_rate_1,
                                  ZO2 = param_converted$zo_rate_2, FO2 = param_converted$fo_rate_2, SQ2 = param_converted$sq_rate_2,
                                  ZO3 = param_converted$zo_rate_3, FO3 = param_converted$fo_rate_3, SQ3 = param_converted$sq_rate_3
                                  )
         
         # Create list of selected drug release functions
         func_list <- list()
         for (n in 1:input$input_phases) {
           func_name <- switch(input[[paste0("input_type_",n)]],
                  "Zero-Order" = paste0("ZO",n),
                  "First-Order" = paste0("FO",n),
                  "Higuchi" = paste0("SQ",n))
           func_list <- append(func_list, func_name)
         }
         
         #print(func_list)

        selected_rate_names <- unlist(func_list, use.names = FALSE)
        
        # Set the rates for any release functions not selected to zero
        init_rates[!(names(init_rates) %in% selected_rate_names)] = 0
        
        
        phase_count <- as.integer(input$input_phases)
        active_phases <- seq_len(phase_count)
        selected_rate_messages <- vapply(active_phases, function(phase_index) {
          rate_name <- selected_rate_names[phase_index]
          rate_value <- suppressWarnings(as.numeric(init_rates[[rate_name]][1]))
          if(length(rate_value) == 1 && is.finite(rate_value) && rate_value > 0) {
            return(NA_character_)
          }
          paste0("Phase ", phase_index, " ", input[[paste0("input_type_", phase_index)]],
                 " rate must be greater than zero.")
        }, character(1))
        active_fraction_values <- c(input$frac_1, input$frac_2, input$frac_3)[active_phases]
        duration_values <- c(param_converted$dur_1, param_converted$dur_2, param_converted$dur_3)
        lag_values <- c(param_converted$tlag_1, param_converted$tlag_2, param_converted$tlag_3)
        lag_enabled <- c(isTRUE(input$input_1_lag), isTRUE(input$input_2_lag), isTRUE(input$input_3_lag))
        selected_removal_phase <- suppressWarnings(as.integer(input$removal_phase))
        if(length(selected_removal_phase) == 0 || is.na(selected_removal_phase)) {
          selected_removal_phase <- phase_count
        }
        
        validate(
          need(sum(active_fraction_values, na.rm=TRUE) <= 1,
               "Active input phase fractions must sum to 1.00 or less."),
          need(!any(!is.na(selected_rate_messages)),
               paste(stats::na.omit(selected_rate_messages), collapse = " ")),
          need(all(duration_values[active_phases] > 0), "Active input phase durations must be greater than zero."),
          need(!isTRUE(input$strict_dur) || selected_removal_phase %in% active_phases,
               "Dose removal phase must be one of the active input phases.")
        )
        
        if(input$input_seq == "Sequential"){
          phase_start_times <- c(0, duration_values[1], duration_values[1] + duration_values[2])
          phase_end_times <- cumsum(duration_values)
        } else {
          phase_start_times <- c(0, 0, 0)
          phase_end_times <- duration_values
        }
        
        release_start_times <- phase_start_times + ifelse(lag_enabled, lag_values, 0)
        removal_time <- phase_end_times[selected_removal_phase]
        simulated_phases <- active_phases
        if(isTRUE(input$strict_dur)) {
          simulated_phases <- active_phases[active_phases <= selected_removal_phase]
        }
        schedule_end_time <- if(isTRUE(input$strict_dur)) {
          removal_time
        } else {
          max(phase_end_times[active_phases])
        }
        
        validate(
          need(all(release_start_times[simulated_phases] < phase_end_times[simulated_phases]),
               "Lag time must be shorter than the available release duration for each simulated phase."),
          need(!isTRUE(input$strict_dur) || all(release_start_times[simulated_phases] < removal_time),
               "Lag time places a simulated phase start at or after the selected dose removal time."),
          need(input$num_dose <= 1 || input$II >= schedule_end_time,
               paste0("Inter-dose interval must be at least ",
                      round(schedule_end_time, 2),
                      " hours to avoid overlapping multiphase dose schedules."))
        )
        
        zero_phase_rates <- function(phase_data, phase_num) {
          phase_data[[paste0("ZO", phase_num)]] <- 0
          phase_data[[paste0("FO", phase_num)]] <- 0
          phase_data[[paste0("SQ", phase_num)]] <- 0
          return(phase_data)
        }
        
        zero_all_rates <- function(phase_data) {
          phase_data <- phase_data %>%
            mutate(ZO1 = 0, FO1 = 0, SQ1 = 0,
                   ZO2 = 0, FO2 = 0, SQ2 = 0,
                   ZO3 = 0, FO3 = 0, SQ3 = 0)
          return(phase_data)
        }
        
        # Create set of start times
         start_1 <- data.frame(ID=ID_vals, time=0, addl=addl_vals, ii=ii_vals,
                               amt=param_converted$dose*input$dose_F*input$frac_1, cmt=dose_cmt, evid=1, phase=1,
                               weight = weight_vals, k_UE_UF = k_UE_UF_val, k_UE_UM = k_UE_UM_val,k_UM_UE = k_UM_UE_val, k_UM_TtoB = k_UM_TtoB_val, F_Uterus = F_Uterus_val, Q_VS = Q_VS_val, CL_L = CL_L_val, CL_H = CL_H_val
                               #ZO1 = param_converted$zo_rate_1, FO1 = rep_null(param_converted$fo_rate_1), SQ1 = rep_null(param_converted$sq_rate_1)
         ) %>%
           add_row(ID=ID_vals, time=0, addl=addl_vals, ii=ii_vals, amt=0, cmt=24, evid=8, phase=1, weight = weight_vals, k_UE_UF = k_UE_UF_val, k_UE_UM = k_UE_UM_val, k_UM_UE = k_UM_UE_val, k_UM_TtoB = k_UM_TtoB_val, F_Uterus = F_Uterus_val, Q_VS = Q_VS_val, CL_L = CL_L_val, CL_H = CL_H_val) %>%
           cbind(init_rates)
         
         
         start_2 <- data.frame(ID=ID_vals, time=0, addl=addl_vals, ii=ii_vals,
                               amt=param_converted$dose*input$dose_F*input$frac_2, cmt=dose_cmt, evid=1, phase=2,
                               weight = weight_vals, k_UE_UF = k_UE_UF_val, k_UE_UM = k_UE_UM_val ,k_UM_UE = k_UM_UE_val, k_UM_TtoB = k_UM_TtoB_val, F_Uterus = F_Uterus_val, Q_VS = Q_VS_val, CL_L = CL_L_val, CL_H = CL_H_val
                               #ZO2 = rep_null(param_converted$zo_rate_2), FO2 = rep_null(param_converted$fo_rate_2), SQ2 = rep_null(param_converted$sq_rate_2)
         ) %>%
           add_row(ID=ID_vals, time=0, addl=addl_vals, ii=ii_vals, amt=0, cmt=25, evid=8, phase=2, weight = weight_vals, k_UE_UF = k_UE_UF_val, k_UE_UM = k_UE_UM_val ,k_UM_UE = k_UM_UE_val, k_UM_TtoB = k_UM_TtoB_val, F_Uterus = F_Uterus_val, Q_VS = Q_VS_val, CL_L = CL_L_val, CL_H = CL_H_val) %>%
           cbind(init_rates)
         
         start_3 <- data.frame(ID=ID_vals, time=0, addl=addl_vals, ii=ii_vals,
                               amt=param_converted$dose*input$dose_F*input$frac_3, cmt=dose_cmt, evid=1, phase=3,
                               weight = weight_vals, k_UE_UF = k_UE_UF_val, k_UE_UM = k_UE_UM_val ,k_UM_UE = k_UM_UE_val, k_UM_TtoB = k_UM_TtoB_val, F_Uterus = F_Uterus_val, Q_VS = Q_VS_val, CL_L = CL_L_val, CL_H = CL_H_val
                               #ZO3 = rep_null(param_converted$zo_rate_3), FO3 = rep_null(param_converted$fo_rate_3), SQ3 = rep_null(param_converted$sq_rate_3)
         ) %>%
           add_row(ID=ID_vals, time=0, addl=addl_vals, ii=ii_vals, amt=0, cmt=26, evid=8, phase=3, weight = weight_vals, k_UE_UF = k_UE_UF_val, k_UE_UM = k_UE_UM_val ,k_UM_UE = k_UM_UE_val, k_UM_TtoB = k_UM_TtoB_val, F_Uterus = F_Uterus_val, Q_VS = Q_VS_val, CL_L = CL_L_val, CL_H = CL_H_val) %>%
           cbind(init_rates)
         
         
         
         if(input$input_seq == "Sequential"){
           start_1 <- start_1 %>% mutate(ZO2=0, FO2=0, SQ2=0, ZO3=0, FO3=0, SQ3=0)
          start_2$time <- phase_start_times[2]
           start_2 <- start_2 %>% mutate(ZO1=0, FO1=0, SQ1=0, ZO3=0, FO3=0, SQ3=0)
          start_3$time <- phase_start_times[3]
           start_3 <- start_3 %>% mutate(ZO1=0, FO1=0, SQ1=0, ZO2=0, FO2=0, SQ2=0)
         }
         
        start_1$time <- release_start_times[1]
        start_2$time <- release_start_times[2]
        start_3$time <- release_start_times[3]
        
        make_phase_end <- function(start_data, phase_num) {
          end_data <- start_data %>%
            filter(cmt == 23 + phase_num) %>%
            mutate(time = phase_end_times[phase_num],
                   amt = 0,
                   evid = 8)
          end_data <- zero_phase_rates(end_data, phase_num)
          return(end_data)
        }
        
        make_lag_off <- function(start_data, phase_num) {
          lag_off <- start_data %>%
            filter(cmt == 23 + phase_num) %>%
            mutate(time = phase_start_times[phase_num],
                   amt = 0,
                   evid = 8)
          lag_off <- zero_phase_rates(lag_off, phase_num)
          return(lag_off)
        }
        
        end_1 <- make_phase_end(start_1, 1)
        end_2 <- make_phase_end(start_2, 2)
        end_3 <- make_phase_end(start_3, 3)
         
         
         
         if(input$input_1_lag) {
          start_1 <- rbind(make_lag_off(start_1, 1), start_1)
         }
         if(input$input_2_lag) {
          start_2 <- rbind(make_lag_off(start_2, 2), start_2)
         }
         if(input$input_3_lag) {
          start_3 <- rbind(make_lag_off(start_3, 3), start_3)
         }
         
         
         # func_set <- start_1 %>%
         #   select(ID, time, evid, phase, ZO1, FO1, SQ1) %>%
         #   full_join(select(start_2, ID, time, evid, phase, ZO2, FO2, SQ2), by=c("ID","time","evid", "phase"))
         # func_set <- func_set %>%
         #   full_join(select(start_3, ID, time, evid, phase, ZO3, FO3, SQ3), by=c("ID","time","evid", "phase"))
         
         
         # input_prelim <- start_1 #%>%
         #   #select(ID, time, amt, cmt, evid, phase)
         # 
         # input_prelim <- start_2 %>%
         #   #select(ID, time, amt, cmt, evid, phase) %>%
         #   rbind(input_prelim)
         # 
         # input_prelim <- start_3 %>%
         #   #select(ID, time, amt, cmt, evid, phase) %>%
         #   rbind(input_prelim)
         
         
       #input_set <- full_join(input_prelim, func_set, by=c("ID","time","evid", "phase"))
       input_set <- rbind(start_1, start_2, start_3, end_1, end_2, end_3)

         
         if(input$strict_dur==TRUE){
          end_dose <- start_1 %>%
            filter(cmt == dose_cmt) %>%
            mutate(time = removal_time,
                   amt = 0,
                   evid = 8,
                   phase = selected_removal_phase)
          end_dose <- zero_all_rates(end_dose)
           
           #input_set <- left_join(input_set, end_dose, by=c("ID", "time", "cmt", "evid"))
           input_set <- rbind(input_set, end_dose) #, all.x=TRUE, all.y=TRUE)
         } 
        
        input_set <- bind_rows(lapply(dose_times, function(dose_time) {
          input_set %>%
            mutate(time = time + dose_time)
        }))
         
         
         #input_set <- left_join(input_set, weight_comb, by="ID")
         
         input_set[is.na(input_set)] <- 0
         input_set <-input_set %>%
          filter(phase %in% simulated_phases) %>%
           arrange(ID,time,phase)
         
         
         
         return(input_set)
     })
    
    table_vals <- eventReactive(input$data_prev,{
      inputData()
    })
    
    observeEvent(input$data_prev,{
      updateTabsetPanel(session, "outputTabset",selected="Input Dataset")
    })
    
    output$table1 <- renderDataTable({
      datatable(table_vals(), options=list()) 
    })

    output$downloadData <- downloadHandler(
      filename = "Input.csv",
      content = function(file) {
        write.csv(inputData(), file, row.names = FALSE)
      }
    )
    
   


# 6. Input Handlers -------------------------------------------------------



    ## Restrict conflicting input options
    
    # * 6.1 Input Phases ----
    observeEvent(input$input_phases, {
      phase_count <- as.integer(input$input_phases)
      removal_choices <- setNames(as.character(seq_len(phase_count)), paste("Phase", seq_len(phase_count)))
      current_removal_phase <- suppressWarnings(as.integer(isolate(input$removal_phase)))
      if(length(current_removal_phase) == 0 || is.na(current_removal_phase) || current_removal_phase > phase_count) {
        current_removal_phase <- phase_count
      }
      updateSelectInput(session, "removal_phase",
                        choices = removal_choices,
                        selected = as.character(current_removal_phase))
      
      if(input$input_phases==1) {
        disable("phase_2_options")
        disable("phase_3_options")
        disable("input_seq")
        disable("frac_1")
        disable("lock_frac_1")
        
        updateCheckboxInput(session, "lock_frac_2", value=FALSE)
        updateCheckboxInput(session, "lock_frac_3", value=FALSE)
        updateSliderInput(session, "frac_2", value=0)
        updateSliderInput(session, "frac_3", value=0)
        shinyjs::hide(id="input_2_params")
        shinyjs::hide(id="input_3_params")
        shinyjs::reset(id="input_2_params")
        shinyjs::reset(id="input_3_params")
      }
      
      if(input$input_phases==2) {
        enable("phase_2_options")
        disable("phase_3_options")
        enable("input_seq")
        enable("frac_1")
        disable("lock_frac_1")
        disable("lock_frac_2")
        
        updateCheckboxInput(session,"lock_frac_3", value=FALSE)
        updateSliderInput(session, "frac_3", value=0)
        shinyjs::show(id="input_2_params")
        shinyjs::hide(id="input_3_params")
        shinyjs::reset(id="input_3_params")
      }
      
      if(input$input_phases==3) {
        enable("phase_2_options")
        enable("phase_3_options")
        enable("input_seq")
        enable("frac_1")
        enable("lock_frac_1")
        enable("lock_frac_2")
        
        shinyjs::show(id="input_2_params")
        shinyjs::show(id="input_3_params")
      }
      
    })
    
    observeEvent(input$strict_dur, {
      if(isTRUE(input$strict_dur)) {
        enable("removal_phase")
      } else {
        disable("removal_phase")
      }
    }, ignoreInit = FALSE)
    
    
    # * 6.2 Administration Route ----
    observeEvent(input$admin_route, {
      if(input$admin_route=="Oral") {
        updateRadioButtons(session, "dose_type", selected="Bolus")
        disable("dose_type")
      }
      else {
        enable("dose_type")
      }
    })
    
    observe({
      shiny::req(input$admin_route, input$input_phases)
      phase_count <- as.integer(input$input_phases)
      active_input_types <- vapply(seq_len(phase_count), function(phase_index) {
        input_type <- input[[paste0("input_type_", phase_index)]]
        if (is.null(input_type)) "" else input_type
      }, character(1))
      
      toggle(
        id = "release_function_dose_warning",
        condition = any(active_input_types %in% c("Zero-Order", "Higuchi"))
      )
    })
    
    # Ensure that only the selected input function is being sent to the simulator
    observeEvent(input$input_type_1, {
      if(input$input_type_1=="Zero-Order") {
        updateNumericInput(session, "fo_rate_1", value=0)
        updateNumericInput(session, "sqrt_rate_1", value=0)
      }
      if(input$input_type_1=="First-Order") {
        updateNumericInput(session, "zo_rate_1", value=0)
        updateNumericInput(session, "sqrt_rate_1", value=0)
      }
      if(input$input_type_1=="Higuchi") {
        updateNumericInput(session, "zo_rate_1", value=0)
        updateNumericInput(session, "fo_rate_1", value=0)
      }
    })
    
    observeEvent(input$input_type_2, {
      if(input$input_type_2=="Zero-Order") {
        updateNumericInput(session, "fo_rate_2", value=0)
        updateNumericInput(session, "sqrt_rate_2", value=0)
      }
      if(input$input_type_2=="First-Order") {
        updateNumericInput(session, "zo_rate_2", value=0)
        updateNumericInput(session, "sqrt_rate_2", value=0)
      }
      if(input$input_type_2=="Higuchi") {
        updateNumericInput(session, "zo_rate_2", value=0)
        updateNumericInput(session, "fo_rate_2", value=0)
      }
    })
    
    observeEvent(input$input_type_3, {
      if(input$input_type_3=="Zero-Order") {
        updateNumericInput(session, "fo_rate_3", value=0)
        updateNumericInput(session, "sqrt_rate_3", value=0)
      }
      if(input$input_type_3=="First-Order") {
        updateNumericInput(session, "zo_rate_3", value=0)
        updateNumericInput(session, "sqrt_rate_3", value=0)
      }
      if(input$input_type_3=="Higuchi") {
        updateNumericInput(session, "zo_rate_3", value=0)
        updateNumericInput(session, "fo_rate_3", value=0)
      }
    })
    
    # * 6.3 Dose Removal ----
    strictDur <- reactiveVal(FALSE)
    
    observeEvent(input$strict_dur, {
      if(input$strict_dur==TRUE){
        strictDur(TRUE)
        updateNumericInput(session, "dur_2", value=input$dur_1)
        updateNumericInput(session, "dur_3", value=input$dur_1)
        updateCheckboxInput(session, "calc_dur_1", value=FALSE)
        disable("dur_2")
        disable("dur_3")
        disable("calc_dur_2")
        disable("calc_dur_3")
      }
      else {
        strictDur(FALSE)
        enable("dur_2")
        enable("dur_3")
        enable("calc_dur_2")
        enable("calc_dur_3")
      }
    })
    
    observe({
      shiny::req(input$strictDur==TRUE)
      updateNumericInput(session, "dur_2", value=input$dur_1)
      updateNumericInput(session, "dur_3", value=input$dur_1)
    })
    
    
    # * 6.4 Number of Doses----
    ## Ensure that only integer numbers are entered as number of doses
    ## Also disables inter-dose interval option when single dose is selected
    observeEvent(input$num_dose, {
      shiny::req(input$num_dose)
      if(input$num_dose != floor(input$num_dose)){
        updateNumericInput(session, "num_dose", value=round(input$num_dose,0))
      }
      
      
      else if(input$num_dose<=1){
        disable("II")
        updateNumericInput(session, "num_dose", value=1)
      }
      else{
        enable("II")
      }
    })
    
    
    # * 6.5 Dose Fraction Locks ----
    ## Disable dose fraction sliders when the corresponding "lock" checkbox is selected
    observeEvent(input$lock_frac_1, {
      if(input$lock_frac_1){
        disable("frac_1")
        enable("frac_2")
        enable("frac_3")
        updateCheckboxInput(session, "lock_frac_2", value=FALSE)
        updateCheckboxInput(session, "lock_frac_3", value=FALSE)
      } else {
        enable("frac_1")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$lock_frac_2, {
      if(input$lock_frac_2){
        disable("frac_2")
        enable("frac_1")
        enable("frac_3")
        updateCheckboxInput(session, "lock_frac_1", value=FALSE)
        updateCheckboxInput(session, "lock_frac_3", value=FALSE)
      } else {
        enable("frac_2")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$lock_frac_3, {
      if(input$lock_frac_3){
        disable("frac_3")
        enable("frac_2")
        enable("frac_1")
        updateCheckboxInput(session, "lock_frac_2", value=FALSE)
        updateCheckboxInput(session, "lock_frac_1", value=FALSE)
      } else {
        enable("frac_3")
      }
    }, ignoreInit=TRUE)
    
    
    # * 6.6 Rate and Duration ----
    ## Disable rate and duration calculation options when either lag time or
    ## time-dependance are selected
#    timeOps1 <- reactive({
#      list(input$input_1_td, input$input_1_lag)
#    })
    
#    observeEvent(timeOps1(), {
#      delay(200,
#            if(input$input_1_td | input$input_1_lag) {
#              updateCheckboxInput(session, "calc_dur_1", value=FALSE)
#              updateCheckboxInput(session, "zo_calc_1", value=FALSE)
#              updateCheckboxInput(session, "fo_calc_1", value=FALSE)
#              updateCheckboxInput(session, "sqrt_calc_1", value=FALSE)
#              calc1$dur <- FALSE
#              calc1$zo <- FALSE
#              calc1$fo <- FALSE
#              calc1$sqrt <- FALSE
#              disable("calc_dur_1")
#              disable("zo_calc_1")
#              disable("fo_calc_1")
#              disable("sqrt_calc_1")
#              enable("dur_1")
#              enable("zo_rate_1")
#              enable("fo_rate_1")
#              enable("sqrt_rate_1")
#            }
#            else {
#              enable("calc_dur_1")
#              enable("zo_calc_1")
#              enable("fo_calc_1")
#              enable("sqrt_calc_1")
#            }
#      )
#    })
#    
#    timeOps2 <- reactive({
#      list(input$input_2_td, input$input_2_lag)
#    })
#    
#    observeEvent(timeOps2(), {
#      delay(200,
#            if(input$input_2_td | input$input_2_lag) {
#              updateCheckboxInput(session, "calc_dur_2", value=FALSE)
#              updateCheckboxInput(session, "zo_calc_2", value=FALSE)
#              updateCheckboxInput(session, "fo_calc_2", value=FALSE)
#              updateCheckboxInput(session, "sqrt_calc_2", value=FALSE)
#              calc2$dur <- FALSE
#              calc2$zo <- FALSE
#              calc2$fo <- FALSE
#              calc2$sqrt <- FALSE
#              disable("calc_dur_2")
#              disable("zo_calc_2")
#              disable("fo_calc_2")
#              disable("sqrt_calc_2")
#              enable("dur_2")
#              enable("zo_rate_2")
#              enable("fo_rate_2")
#              enable("sqrt_rate_2")
#            }
#            else {
#              enable("calc_dur_2")
#              enable("zo_calc_2")
#              enable("fo_calc_2")
#              enable("sqrt_calc_2")
#            }
#      )
#    })
#    
#    timeOps3 <- reactive({
#      list(input$input_3_td, input$input_3_lag)
#    })
#    
#    observeEvent(timeOps3(), {
#      delay(200,
#              updateCheckboxInput(session, "calc_dur_3", value=FALSE)
#              updateCheckboxInput(session, "zo_calc_3", value=FALSE)
#              updateCheckboxInput(session, "fo_calc_3", value=FALSE)
#              updateCheckboxInput(session, "sqrt_calc_3", value=FALSE)
#              calc3$dur <- FALSE
#              calc3$zo <- FALSE
#              calc3$fo <- FALSE
#              calc3$sqrt <- FALSE
#              disable("calc_dur_3")
#              disable("zo_calc_3")
#              disable("fo_calc_3")
#              disable("sqrt_calc_3")
#              enable("dur_3")
#              enable("zo_rate_3")
#              enable("fo_rate_3")
#              enable("sqrt_rate_3")
#            }
#            else {
#              enable("calc_dur_3")
#              enable("zo_calc_3")
#              enable("fo_calc_3")
#              enable("sqrt_calc_3")
#            }
#      )
#    })
#   
    # * 6.7 Population ----
    mirroredNumericValuesEqual <- function(x, y) {
      if (is.null(x) || is.null(y)) {
        return(FALSE)
      }
      if (is.na(x) && is.na(y)) {
        return(TRUE)
      }
      isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = sqrt(.Machine$double.eps)))
    }
    
    syncMirroredMeanInput <- function(population_id, parameter_block_id) {
      observeEvent(input[[population_id]], {
        if (!mirroredNumericValuesEqual(input[[population_id]], input[[parameter_block_id]])) {
          updateNumericInput(session, parameter_block_id, value = input[[population_id]])
        }
      }, ignoreInit = FALSE)
      
      observeEvent(input[[parameter_block_id]], {
        if (!mirroredNumericValuesEqual(input[[parameter_block_id]], input[[population_id]])) {
          updateNumericInput(session, population_id, value = input[[parameter_block_id]])
        }
      }, ignoreInit = TRUE)
    }
    
    syncMirroredMeanInput("k_UE_UF_mean", "k_UE_UF_mean_uterine")
    syncMirroredMeanInput("k_UE_UM_mean", "k_UE_UM_mean_uterine")
    syncMirroredMeanInput("k_UM_UE_mean", "k_UM_UE_mean_uterine")
    syncMirroredMeanInput("k_UM_TtoB_mean", "k_UM_TtoB_mean_uterine")
    syncMirroredMeanInput("CL_L_mean", "CL_L_mean_clearance")
    syncMirroredMeanInput("CL_H_mean", "CL_H_mean_clearance")
    
    # Round number of individuals to the nearest integer
    observeEvent(input$num_ind, {
      shiny::req(input$num_ind)
      if(input$num_ind != floor(input$num_ind)){
        updateNumericInput(session, "num_ind", value=round(input$num_ind,0))
      }
    })
    
    # Round population seed to the nearest integer for reproducible random sampling
    observeEvent(input$pop_seed, {
      shiny::req(input$pop_seed)
      if(input$pop_seed != floor(input$pop_seed)){
        updateNumericInput(session, "pop_seed", value=round(input$pop_seed,0))
      }
    })

# 7. Time Scales ----------------------------------------------------------


## Update UI and parameter values based on selected time units
    observeEvent(input$input_1_scale, {
      t_unit <- input$input_1_scale
      
      if(t_unit=="Hours") {
        new_time <- input$dur_1*24
        new_td <- input$td_1*24
        new_lag <- input$tlag_1*24
        new_zo <- input$zo_rate_1/24
        new_fo <- input$fo_rate_1/24
        new_sqrt <- input$sqrt_rate_1/sqrt(24)
      }
      if(t_unit=="Days") {
        new_time <- input$dur_1/24
        new_td <- input$td_1/24
        new_lag <- input$tlag_1/24
        new_zo <- input$zo_rate_1*24
        new_fo <- input$fo_rate_1*24
        new_sqrt <- input$sqrt_rate_1*sqrt(24)
      }
      
      updateNumericInput(session, "dur_1", label=paste0("Duration of Release (",t_unit,")"), value=new_time)
      updateNumericInput(session, "zo_rate_1", value=new_zo)
      updateNumericInput(session, "fo_rate_1", value=new_fo)
      updateNumericInput(session, "sqrt_rate_1", value=new_sqrt)
      updateNumericInput(session, "td_1", value=new_td)
      updateNumericInput(session, "tlag_1", value=new_lag)
    }, ignoreInit=TRUE)
    
    observeEvent(input$input_2_scale, {
      t_unit <- input$input_2_scale
      
      if(t_unit=="Hours") {
        new_time <- input$dur_2*24
        new_td <- input$td_2*24
        new_lag <- input$tlag_2*24
        new_zo <- input$zo_rate_2/24
        new_fo <- input$fo_rate_2/24
        new_sqrt <- input$sqrt_rate_2/sqrt(24)
      }
      if(t_unit=="Days") {
        new_time <- input$dur_2/24
        new_td <- input$td_2/24
        new_lag <- input$tlag_2/24
        new_zo <- input$zo_rate_2*24
        new_fo <- input$fo_rate_2*24
        new_sqrt <- input$sqrt_rate_2*sqrt(24)
      }
      
      updateNumericInput(session, "dur_2", label=paste0("Duration of Release (",t_unit,")"), value=new_time)
      updateNumericInput(session, "zo_rate_2", value=new_zo)
      updateNumericInput(session, "fo_rate_2", value=new_fo)
      updateNumericInput(session, "sqrt_rate_2", value=new_sqrt)
      updateNumericInput(session, "td_2", value=new_td)
      updateNumericInput(session, "tlag_2", value=new_lag)
    }, ignoreInit=TRUE)
    
    observeEvent(input$input_3_scale, {
      t_unit <- input$input_3_scale
      
      if(t_unit=="Hours") {
        new_time <- input$dur_3*24
        new_td <- input$td_3*24
        new_lag <- input$tlag_3*24
        new_zo <- input$zo_rate_3/24
        new_fo <- input$fo_rate_3/24
        new_sqrt <- input$sqrt_rate_3/sqrt(24)
      }
      if(t_unit=="Days") {
        new_time <- input$dur_3/24
        new_td <- input$td_3/24
        new_lag <- input$tlag_3/24
        new_zo <- input$zo_rate_3*24
        new_fo <- input$fo_rate_3*24
        new_sqrt <- input$sqrt_rate_3*sqrt(24)
      }
      
      updateNumericInput(session, "dur_3", label=paste0("Duration of Release (",t_unit,")"), value=new_time)
      updateNumericInput(session, "zo_rate_3", value=new_zo)
      updateNumericInput(session, "fo_rate_3", value=new_fo)
      updateNumericInput(session, "sqrt_rate_3", value=new_sqrt)
      updateNumericInput(session, "td_3", value=new_td)
      updateNumericInput(session, "tlag_3", value=new_lag)
    }, ignoreInit=TRUE)
    


# 8. UI Rendering ----------------------------------------------------------
    ## RenderUI functions for each phase of dosing


    # * 8.1 Input Phase 1 ----
    ## The input box displayed will change depending on which input function is selected
    output$rate_1 <- renderUI({

      input_ops <- c(input$input_1_td, input$input_1_lag,
                     ifelse(input$input_type_1 == "First-Order", TRUE, FALSE))

      num_ops <- sum(input_ops, na.rm=TRUE)

      # numeric input to switch() starts at 1; 0 or negative numbers return NULL
      span_class <- switch(num_ops+1, "full", "half", "third", "quarter")
      col_width <- 12/(num_ops+1)


      t_unit <- input$input_1_scale
      # search through t_unit and removes any "s" at the end of a string
      t_unit_singular <- gsub("s\\>", "", t_unit)

      tagList(
        switch(input$input_type_1,
               
               # UI elements for zero-order function
               "Zero-Order" = span(column(col_width, numericInput("zo_rate_1", paste0("Zero-Order Rate (\U00B5g/",t_unit_singular,")"), min=0,value=0,step=0.1),
                                   div(id="zo1",checkboxInput("zo_calc_1", "Calculate from Duration", value=FALSE))),
                                   bsTooltip("zo_calc_1", "Zero-Order rate is calculated as (Dose*F*Phase 1 Fraction) / Duration")),
               
               # UI elecments for first-order function
               "First-Order" = span(column(col_width, numericInput("fo_rate_1", paste0("First-Order Rate Constant (1/",t_unit,")"), min=0,value=0,step=0.1),
                                           checkboxInput("fo_calc_1", "Calculate from Duration", value=FALSE)),
                                    column(col_width, numericInput("n_tHalf_1", "Number of Absorption Half-Lives", min=0, value=8)),
                                    bsTooltip("fo_calc_1", "First-Order rate constant is calculated as 1/ln(2) * Duration/Number of Half-Lives"),
                                    bsTooltip("n_tHalf_1", "Percent remaining to be released after n half-lives = 100/(2^n)")),
               
               # UI elements for Higuchi function
               "Higuchi" = span(column(col_width, numericInput("sqrt_rate_1",HTML(paste0("Higuchi (Square-Root Time) Constant (\U00B5g/",t_unit_singular,")",tags$sup("1/2"),")")),
                                                               min=0,value=0,step=0.1),
                                checkboxInput("sqrt_calc_1", "Calculate from Duration", value=FALSE)),
                                bsTooltip("sqrt_calc_1", HTML(paste0("Higuchi constant is calculated as (Dose*F*Phase 1 Fraction) / (Duration",tags$sup("1/2"),")"))))
        ),
        
        # Time-dependence parameter input box
        # if(input$input_1_td){
        #   span(column(col_width, numericInput("td_1", paste0("Time-Dependence Parameter (",t_unit,")"), min=0,value=0,step=0.1)))
        # },

        # Lag time input box
        if(input$input_1_lag){
          span(column(col_width, numericInput("tlag_1", paste0("Lag Time (",t_unit,")"), min=0,value=0,step=0.1)))
        }
      )
    })
    
    
    # * 8.2 Input Phase 2 ----
    output$rate_2 <- renderUI({
      
      input_ops <- c(input$input_2_td, input$input_2_lag,
                     ifelse(input$input_type_1 == "First-Order", TRUE, FALSE))
      
      num_ops <- sum(input_ops, na.rm=TRUE)
      
      # numeric input to switch starts at 1; 0 or negative numbers return NULL
      span_class <- switch(num_ops+1, "full", "half", "third", "quarter")
      col_width <- 12/(num_ops+1)
      
      
      
      t_unit <- input$input_2_scale
      # search through t_unit and removes any "s" at the end of a string
      t_unit_singular <- gsub("s\\>", "", t_unit)
      
      tagList(
        switch(input$input_type_2,
               "Zero-Order" = span(column(col_width, numericInput("zo_rate_2", paste0("Zero-Order Rate (\U00B5g/",t_unit_singular,")"), min=0,value=0,step=0.1),
                                          checkboxInput("zo_calc_2", "Calculate from Duration", value=FALSE)),
                                   bsTooltip("zo_calc_2", "Zero-Order rate is calculated as (Dose*F*Phase 2 Fraction) / Duration")),
               "First-Order" = span(column(col_width, numericInput("fo_rate_2", paste0("First-Order Rate Constant (1/",t_unit,")"), min=0,value=0,step=0.1),
                                           checkboxInput("fo_calc_2", "Calculate from Duration", value=FALSE)),
                                    column(col_width, numericInput("n_tHalf_2", "Number of Absorption Half-Lives", min=0, value=8)),
                                    bsTooltip("fo_calc_2", "First-Order rate constant is calculated as 1/ln(2) * Duration/Number of Half-Lives"),
                                    bsTooltip("n_tHalf_2", "Percent remaining to be released after n half-lives = 100/(2^n)")),
               "Higuchi" = span(column(col_width, numericInput("sqrt_rate_2",HTML(paste0("Higuchi (Square-Root Time) Constant (\U00B5g/",t_unit_singular,")",tags$sup("1/2"),")")),
                                                               min=0,value=0,step=0.1),
                                       checkboxInput("sqrt_calc_2", "Calculate from Duration", value=FALSE)),
                                bsTooltip("sqrt_calc_2", HTML(paste0("Higuchi constant is calculated as (Dose*F*Phase 2 Fraction) / (Duration",tags$sup("1/2"),")"))))
        ),
        
        # if(input$input_2_td){
        #   span(column(col_width, numericInput("td_2", paste0("Time-Dependence Parameter (",t_unit,")"), min=0,value=0,step=0.1)))
        # },
        # 
        if(input$input_2_lag){
          span(column(col_width, numericInput("tlag_2", paste0("Lag Time (",t_unit,")"), min=0,value=0,step=0.1)))
        }
      )
    })
    
    
    # * 8.3 Input Phase 3 ----
    output$rate_3 <- renderUI({
      
      input_ops <- c(input$input_3_td, input$input_3_lag)
      num_ops <- sum(input_ops, na.rm=TRUE)
      
      # numeric input to switch starts at 1; 0 or negative numbers return NULL
      span_class <- switch(num_ops+1, "full", "half", "third", "quarter")
      col_width <- 12/(num_ops+1)
      
      
      
      t_unit <- input$input_3_scale
      # search through t_unit and removes any "s" at the end of a string
      t_unit_singular <- gsub("s\\>", "", t_unit)
      
      tagList(
        switch(input$input_type_3,
               "Zero-Order" = span(column(col_width, numericInput("zo_rate_3", paste0("Zero-Order Rate (\U00B5g/",t_unit_singular,")"), min=0,value=0,step=0.1),
                                          checkboxInput("zo_calc_3", "Calculate from Duration", value=FALSE)),
                                   bsTooltip("zo_calc_3", "Zero-Order rate is calculated as (Dose*F*Phase 3 Fraction) / Duration")),
               "First-Order" = span(column(col_width, numericInput("fo_rate_3", paste0("First-Order Rate Constant (1/",t_unit,")"), min=0,value=0,step=0.1),
                                           checkboxInput("fo_calc_3", "Calculate from Duration", value=FALSE)),
                                    column(col_width, numericInput("n_tHalf_3", "Number of Absorption  Half-Lives", min=0, value=8)),
                                    bsTooltip("fo_calc_3", "First-Order rate constant is calculated as 1/ln(2) * Duration/Number of Half-Lives"),
                                    bsTooltip("n_tHalf_3", "Percent remaining to be released after n half-lives = 100/(2^n)")),
               "Higuchi" = span(column(col_width, numericInput("sqrt_rate_3",HTML(paste0("Higuchi (Square-Root Time) Constant (\U00B5g/",t_unit_singular,")",tags$sup("1/2"),")")),
                                                               min=0,value=0,step=0.1),
                                       checkboxInput("sqrt_calc_3", "Calculate from Duration", value=FALSE)),
                                bsTooltip("sqrt_calc_3", HTML(paste0("Higuchi constant is calculated as (Dose*F*Phase 3 Fraction) / (Duration",tags$sup("1/2"),")"))))
        ),
        
        # if(input$input_3_td){
        #   span(column(col_width, numericInput("td_3", paste0("Time-Dependence Parameter (",t_unit,")"), min=0,value=0,step=0.1)))
        #   
        # },
        
        if(input$input_3_lag){
          span(column(col_width, numericInput("tlag_3", paste0("Lag Time (",t_unit,")"), min=0,value=0,step=0.1)))
        }
      )
    })



# 9. Dose Fraction Sliders ------------------------------------------------
## See slider 1 code for detailed description of how each handler works

    # Display modal dialog to user if they deselect the automatic slider control option
    observeEvent(input$auto_sliders, {
      if(input$auto_sliders != TRUE) {
        showModal(modalDialog(
          title= "Warning!",
          "The simulation will not run if the sum of the active dose fractions is greater than 1.00."
          )
        )
      } else if(input$auto_sliders == TRUE) {
        
      }
    }, ignoreInit = TRUE)
    
        
    # Create reactive variables to store previous slider position
    # Initialize Slider_1 at 1.0 and all others at 0
    slider_1 <- reactiveValues(prev=1, cur=1)
    slider_2 <- reactiveValues(prev=0, cur=0)
    slider_3 <- reactiveValues(prev=0, cur=0)
    
    # Create reactive list of slider inputs to observe
    slider_change <- reactive({
      list(input$frac_1, input$frac_2, input$frac_3)
    }) %>% debounce(500)
    # The debounce() function tells the reactive expression to ignore "noisy" or
    # rapid inputs and wait for things to quiet down. Debounce(x) will start a
    # timer of x milliseconds whenever the reactive expression is triggered. Any
    # new inputs received before the end of the timer will restart it. The code
    # within the expression will not execute until the debounce timer reaches 0.
    # This should prevent the slider adjustment from triggering until the user has
    # finished moving the sliders
    
    
    ## Observer is triggered byt the slider_change reactive expression
    observeEvent(slider_change(), {
      
      # Require the automatic slider adjustment option to be selected (on by default)
      shiny::req(input$auto_sliders==TRUE)
      
      # When a slider changes, move all "current" values to "previous"
      # Then set the current values to the current value of the slider
      slider_1$prev <- slider_1$cur
      slider_1$cur <- input$frac_1
      slider_2$prev <- slider_2$cur
      slider_2$cur <- input$frac_2
      slider_3$prev <- slider_3$cur
      slider_3$cur <- input$frac_3
      active_frac_values <- c(slider_1$cur, slider_2$cur, slider_3$cur)[seq_len(as.integer(input$input_phases))]
      active_frac_total <- sum(active_frac_values, na.rm=TRUE)
      
      if(active_frac_total <= 1) {
        return()
      }
      
      if(slider_1$prev != slider_1$cur) {
        slider_1$cur <- max(0, slider_1$cur - (active_frac_total - 1))
        slider_1$prev <- slider_1$cur
        updateSliderInput(session, "frac_1", value=slider_1$cur)
        return()
      }
      if(slider_2$prev != slider_2$cur) {
        slider_2$cur <- max(0, slider_2$cur - (active_frac_total - 1))
        slider_2$prev <- slider_2$cur
        updateSliderInput(session, "frac_2", value=slider_2$cur)
        return()
      }
      if(slider_3$prev != slider_3$cur) {
        slider_3$cur <- max(0, slider_3$cur - (active_frac_total - 1))
        slider_3$prev <- slider_3$cur
        updateSliderInput(session, "frac_3", value=slider_3$cur)
        return()
      }
      
      
    # * 9.1 Slider 1 ----
      
      ##  Only run if slider 1 changed
      if(slider_1$prev != slider_1$cur) {
        # Calculate the change in the dose fraction
        change_val = slider_1$cur - slider_1$prev
        
        # If only 2 input phases are selected, adjust the "current" value of
        # slider 2 by the amount slider 1 moved, then update "previous" values
        # for sliders 1 and 2, and finally adjust slider 2 in the UI
        if(input$input_phases == 2) {
          slider_2$cur = slider_2$prev - change_val
          slider_2$prev = slider_2$cur
          slider_1$prev = slider_1$cur
          updateSliderInput(session, "frac_2", value = slider_2$cur)
        }
        
        ## Run if three input phases are selected
        else if(input$input_phases == 3) {
          
          # If sliders 2 and 3 are both unlocked, attempt to move both by half
          # the value slider 1 changed by
          if(input$lock_frac_2==FALSE & input$lock_frac_3==FALSE) {
            
            # First divide the amount slider 1 changed in half. If the amount
            # changed is an odd number, adjust one of the halves by 0.5% to
            # ensure final value for both is an integer percentage
            changeHalf1 <- ifelse(near((change_val*100)%%2,1), change_val/2 - 0.005, change_val/2)
            changeHalf2 <- change_val - changeHalf1
            
            # If the current value of slider 2 is less than the amount it would
            # need to change, set slider 2 to zero and adjust slider 3 by the
            # remaining amount needed for the sum of all sliders to equal 100%
            if(slider_2$cur < changeHalf1) {
              slider_3$cur = 1 - slider_1$cur
              slider_2$cur = 0
              slider_3$prev = slider_3$cur
              slider_2$prev = slider_2$cur
            }
            
            # Repeat the same check for slider 3 using the other half
            else if(slider_3$cur < changeHalf2) {
              slider_2$cur = 1 - slider_1$cur
              slider_3$cur = 0
              slider_2$prev = slider_2$cur
              slider_3$prev = slider_3$cur
            }
            
            # If both sliders can move enough to accept half the change value,
            # apply the adjustment to the currently stored value of each one
            else{
              slider_2$cur = slider_2$prev - changeHalf1
              slider_3$cur = slider_3$prev - changeHalf2
              
              # Final check to ensure sum is 1.00 (this may now be redundant,
              # but there is no real reason to remove it)
              if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
               slider_3$cur = slider_3$cur + 0.01
              }
              
              # Store current values as the new previous values
              slider_2$prev = slider_2$cur
              slider_3$prev = slider_3$cur
            }
            slider_1$prev = slider_1$cur
            
            # Update the sliders in the UI
            updateSliderInput(session, "frac_2", value=slider_2$cur)
            updateSliderInput(session, "frac_3", value=slider_3$cur)
          }
          
          # Run if slider 2 is locked
          else if(input$lock_frac_2==TRUE & input$lock_frac_3==FALSE) {
            
            # Check whether slider 3 can move enough to accommodate the change.
            # If not, set slider 3 to zero, then move slider 1 as far as possible
            if(slider_3$cur < change_val) {
              slider_1$cur = 1 - slider_2$cur
              slider_3$cur = 0
              slider_1$prev = slider_1$cur
              slider_3$prev = slider_3$cur
              updateSliderInput(session, "frac_3", value=slider_3$cur)
              updateSliderInput(session, "frac_1", value=slider_1$cur)
            }
            
            # If slider 3 can be moved far enough to account for the change, do so
            else {
              slider_3$cur = slider_3$prev - change_val
              if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
                slider_3$cur = slider_3$cur + 0.01
              }
              slider_3$prev <- slider_3$cur
              slider_1$prev <- slider_1$cur
              updateSliderInput(session, "frac_3", value=slider_3$cur)
            }
          } 
          
          # If slider 3 is locked, repeat the same checks as above
          else if(input$lock_frac_2==FALSE & input$lock_frac_3==TRUE) {
            if (slider_2$cur < change_val) {
              slider_1$cur = 1 - slider_3$cur
              slider_2$cur = 0
              slider_1$prev = slider_1$cur
              slider_2$prev = slider_2$cur
              updateSliderInput(session, "frac_2", value=slider_2$cur)
              updateSliderInput(session, "frac_1", value=slider_1$cur)
            }
            else {
              slider_2$cur = slider_2$prev - change_val
              if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
                slider_2$cur = slider_2$cur + 0.01
              }
              slider_2$prev <- slider_2$cur
              slider_1$prev <- slider_1$cur
              updateSliderInput(session, "frac_2", value=slider_2$cur)
            }
          }
        }
      }

      
      # * 9.2 Slider 2 ----
      
      else if(slider_2$prev != slider_2$cur) {
        change_val = slider_2$cur - slider_2$prev
        if(input$input_phases == 2) {
          slider_1$cur = slider_1$prev - change_val
          slider_1$prev = slider_1$cur
          slider_2$prev = slider_2$cur
          updateSliderInput(session, "frac_1", value = slider_1$cur)
        }
        else if(input$input_phases == 3) {
          if(input$lock_frac_1==FALSE & input$lock_frac_3==FALSE) {
            changeHalf1 <- ifelse(near((change_val*100)%%2,1), change_val/2 - 0.005, change_val/2)
            changeHalf2 <- change_val - changeHalf1
            if(slider_1$cur < changeHalf1) {
              #slider_3$cur = slider_3$prev + slider_1$cur - change_val %>% round(2)
              slider_3$cur = 1 - slider_2$cur
              slider_1$cur = 0
              slider_3$prev = slider_3$cur
              slider_1$prev = slider_1$cur
            }
            else if(slider_3$cur < changeHalf1) {
              #slider_1$cur = slider_1$prev + slider_3$cur - change_val %>% round(2)
              slider_1$cur = 1 - slider_2$cur
              slider_3$cur = 0
              slider_1$prev = slider_1$cur
              slider_3$prev = slider_3$cur
            }
            else{
              slider_1$cur = slider_1$prev - changeHalf1
              slider_3$cur = slider_3$prev - changeHalf2
              if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
                slider_3$cur = slider_3$cur + 0.01
              }
              slider_1$prev = slider_1$cur
              slider_3$prev = slider_3$cur
            }
            updateSliderInput(session, "frac_1", value=slider_1$cur)
            updateSliderInput(session, "frac_3", value=slider_3$cur)
          }
          else if(input$lock_frac_1==TRUE & input$lock_frac_3==FALSE) {
            if(slider_3$cur < change_val) {
              #slider_2$cur = slider_2$prev - slider_1$cur %>% round(2)
              slider_2$cur = 1 - slider_1$cur
              slider_3$cur = 0
              slider_2$prev = slider_2$cur
              slider_3$prev = slider_3$cur
              updateSliderInput(session, "frac_2", value=slider_2$cur)
              updateSliderInput(session, "frac_3", value=slider_3$cur)
            }
            else {
              slider_3$cur = slider_3$prev - change_val
              if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
                slider_3$cur = slider_3$cur + 0.01
              }
              slider_3$prev <- slider_3$cur
              slider_2$prev <- slider_2$cur
              updateSliderInput(session, "frac_3", value=slider_3$cur)
            }
          } else if(input$lock_frac_1==FALSE & input$lock_frac_3==TRUE) {
            if (slider_1$cur < change_val) {
              #slider_2$cur = slider_2$prev - slider_3$cur %>% round(2)
              slider_2$cur = 1 - slider_3$cur
              slider_1$cur = 0
              slider_1$prev = slider_1$cur
              slider_2$prev = slider_2$cur
              slider_3$prev = slider_3$cur
              updateSliderInput(session, "frac_2", value=slider_2$cur)
              updateSliderInput(session, "frac_1", value=slider_1$cur)
            }
            else {
              slider_1$cur = slider_1$prev - change_val
              if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
                slider_1$cur = slider_1$cur + 0.01
              }
              slider_1$prev <- slider_1$cur
              slider_3$prev <- slider_3$cur
              updateSliderInput(session, "frac_1", value=slider_1$cur)
            }
          }
        }
      }
      
      
      # * 9.3 Slider 3----
      
      else if(slider_3$prev != slider_3$cur) {
        change_val = slider_3$cur - slider_3$prev
        if(input$lock_frac_1==FALSE & input$lock_frac_2==FALSE) {
          changeHalf1 <- ifelse(near((change_val*100)%%2,1), change_val/2 - 0.005, change_val/2)
          changeHalf2 <- change_val - changeHalf1
          if(slider_1$cur < changeHalf1) {
            slider_2$cur = 1 - slider_3$cur
            slider_1$cur = 0
            slider_2$prev = slider_2$cur
            slider_1$prev = slider_1$cur
          }
          else if(slider_2$cur < changeHalf1) {
            slider_1$cur = 1 - slider_3$cur
            slider_2$cur = 0
            slider_1$prev = slider_1$cur
            slider_2$prev = slider_2$cur
          }
          else{
            slider_1$cur = slider_1$prev - changeHalf1
            slider_2$cur = slider_2$prev - changeHalf2
            if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
              slider_2$cur = slider_2$cur + 0.01
            }
            slider_1$prev = slider_1$cur
            slider_2$prev = slider_2$cur
          }
          slider_3$prev = slider_3$cur
          updateSliderInput(session, "frac_1", value=slider_1$cur)
          updateSliderInput(session, "frac_2", value=slider_2$cur)
        }
        else if(input$lock_frac_1==TRUE & input$lock_frac_2==FALSE) {
          if(slider_2$cur < change_val) {
            #slider_3$cur = slider_3$cur - input$frac_1 %>% round(2)
            slider_3$cur = 1 - slider_1$cur
            slider_2$cur = 0
            slider_2$prev = slider_2$cur
            slider_3$prev = slider_3$cur
            updateSliderInput(session, "frac_3", value=slider_3$cur)
            updateSliderInput(session, "frac_2", value=slider_2$cur)
          }
          else {
            slider_2$cur = slider_2$prev - change_val
            if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
              slider_2$cur = slider_2$cur + 0.01
            }
            slider_2$prev <- slider_2$cur
            slider_3$prev = slider_3$cur
            updateSliderInput(session, "frac_2", value=slider_2$cur)
          }
        } else if(input$lock_frac_1==FALSE & input$lock_frac_2==TRUE) {
          if (slider_1$cur < change_val) {
            #slider_3$cur = slider_3$cur - input$frac_2 %>% round(2)
            slider_3$cur = 1 - slider_2$cur
            slider_1$cur = 0
            slider_1$prev = slider_1$cur
            slider_3$prev = slider_3$cur
            updateSliderInput(session, "frac_3", value=slider_3$cur)
            updateSliderInput(session, "frac_1", value=slider_1$cur)
          }
          else {
            slider_1$cur = slider_1$prev - change_val
            if(slider_1$cur + slider_2$cur + slider_3$cur == 0.99) {
              slider_1$cur = slider_1$cur + 0.01
            }
            slider_1$prev = slider_1$cur
            slider_3$prev = slider_3$cur
            updateSliderInput(session, "frac_1", value=slider_1$cur)
          }
        }
      }
    },
    # Do not run this observer the first time it is initialized
    ignoreInit = TRUE)

    

# 10. Rate and Duration Calculators ----------------------------------------------------

    # ## Observers for the option to calculate rate from duration and vice versa
    
    # Create list of reactive values holding the states of the calculation buttons
    calc1 <- reactiveValues(
      dur = FALSE,
      zo = FALSE,
      fo = FALSE,
      sqrt = FALSE
    )
    
    calc2 <- reactiveValues(
      dur = FALSE,
      zo = FALSE,
      fo = FALSE,
      sqrt = FALSE
    )
    
    calc3 <- reactiveValues(
      dur = FALSE,
      zo = FALSE,
      fo = FALSE,
      sqrt = FALSE
    )
    
    
    # * 10.1 Duration ----
    
    # If the option to calculate duration is selected, calculate it based on the
    # selected input function
    observe({
      shiny::req(calc1$dur)
      amt = input$dose_total * input$dose_F * input$frac_1 * 1000
      
      
      # Zero-order: Duration = Dose/Rate
      # First-order: Duration = n * ln(2)/Rate, where n is the number of half-lives
      # that have to pass for release to be considered complete. Generally, n >= 4.
      # Higuchi = Duration = (Dose/Rate)^2
      
      dur <- switch(input$input_type_1,
             "Zero-Order" = {
               amt/input$zo_rate_1
             },
             "First-Order" = {
               input$n_tHalf_1 * log(2)/input$fo_rate_1
             },
             "Higuchi" = {
               (amt/input$sqrt_rate_1)^2
             }
      )
      
      # Update the duration in the input box to the caclculated value
      updateNumericInput(session, inputId="dur_1", value=dur)
    })
    
    observe({
      shiny::req(calc2$dur)
      amt = input$dose_total * input$dose_F * input$frac_2 * 1000
      dur <- switch(input$input_type_2,
                    "Zero-Order" = {
                      amt/input$zo_rate_2
                    },
                    "First-Order" = {
                      input$n_tHalf_2 * log(2)/input$fo_rate_2
                    },
                    "Higuchi" = {
                      (amt/input$sqrt_rate_2)^2
                    }
      )
      updateNumericInput(session, inputId="dur_2", value=dur)
    })
    
    observe({
      shiny::req(calc3$dur)
      amt = input$dose_total * input$dose_F * input$frac_3 * 1000
      dur <- switch(input$input_type_3,
                    "Zero-Order" = {
                      amt/input$zo_rate_3
                    },
                    "First-Order" = {
                      input$n_tHalf_3 * log(2)/input$fo_rate_3
                    },
                    "Higuchi" = {
                      (amt/input$sqrt_rate_3)^2
                    }
      )
      updateNumericInput(session, inputId="dur_3", value=dur)
    })
    
    
    # * 10.2 Rate ----
    
    # Calculate rates based on duration if the option is selected
    observe({
      amt = input$dose_total * input$dose_F * input$frac_1 * 1000
      
      # Zero-Order: Rate = Amount/Duration
      # First-order: Rate = n * ln(2)/Duration, where n is number of half-lives
      # required for release to be considered complete
      # Higuchi: Rate = Amount/Duration^(1/2)
      switch(input$input_type_1,
             "Zero-Order" = {
               shiny::req(calc1$zo)
               rate = amt/input$dur_1
               name = "zo_rate_1"
             },
             "First-Order" = {
               shiny::req(calc1$fo)
               rate = input$n_tHalf_1*log(2)/input$dur_1
               name = "fo_rate_1"
             },
             "Higuchi" = {
               shiny::req(calc1$sqrt)
               rate = amt/sqrt(input$dur_1)
               name = "sqrt_rate_1"
             })
      updateNumericInput(session, inputId=name, value=rate)
    })
    
    observe({
      amt = input$dose_total * input$dose_F * input$frac_2 * 1000
      switch(input$input_type_2,
             "Zero-Order" = {
               shiny::req(calc2$zo)
               rate = amt/input$dur_2
               name = "zo_rate_2"
             },
             "First-Order" = {
               shiny::req(calc2$fo)
               rate = input$n_tHalf_2*log(2)/input$dur_2
               name = "fo_rate_2"
             },
             "Higuchi" = {
               shiny::req(calc2$sqrt)
               rate = amt/sqrt(input$dur_2)
               name = "sqrt_rate_2"
             })
      updateNumericInput(session, inputId=name, value=rate)
    })
    
    observe({
      amt = input$dose_total * input$dose_F * input$frac_3 * 1000
      switch(input$input_type_3,
             "Zero-Order" = {
               shiny::req(calc3$zo)
               rate = amt/input$dur_3
               name = "zo_rate_3"
             },
             "First-Order" = {
               shiny::req(calc3$fo)
               rate = input$n_tHalf_3*log(2)/input$dur_3
               name = "fo_rate_3"
             },
             "Higuchi" = {
               shiny::req(calc3$sqrt)
               rate = amt/sqrt(input$dur_3)
               name = "sqrt_rate_3"
             })
      updateNumericInput(session, inputId=name, value=rate)
    })




# 11. Rate and Duration Controllers ---------------------------------------

    
        
    # * 11.1 Duration ----
    # If the option is selected, enable the appropriate options
    observeEvent(input$calc_dur_1, {
      if(input$calc_dur_1==TRUE) {
        amt = input$dose_total * input$dose_F * input$frac_1 * 1000
        calc1$zo = FALSE
        calc1$fo = FALSE
        calc1$sqrt = FALSE
        if(input$input_type_1=="Zero-Order") {
          enable("zo_rate_1")
          updateCheckboxInput(session, "zo_calc_1", value=FALSE)
          dur = amt/input$zo_rate_1
        }
        if(input$input_type_1=="First-Order") {
          enable("fo_rate_1")
          updateCheckboxInput(session, "fo_calc_1", value=FALSE)
          dur = input$n_tHalf_1 * log(2)/input$fo_rate_1
        }
        if(input$input_type_1=="Higuchi") {
          enable("sqrt_rate_1")
          updateCheckboxInput(session, "sqrt_calc_1", value=FALSE)
          dur = (amt/input$sqrt_rate_1)^2
        }
        updateNumericInput(session, "dur_1", value = dur)
        disable("dur_1")
        calc1$dur = TRUE
      } else {
        enable("dur_1")
        calc1$dur = FALSE
      }
    })

    observeEvent(input$calc_dur_2, {
      if(input$calc_dur_2==TRUE) {
        amt = input$dose_total * input$dose_F * input$frac_2 * 1000
        calc2$zo = FALSE
        calc2$fo = FALSE
        calc2$sqrt = FALSE
        if(input$input_type_2=="Zero-Order") {
          enable("zo_rate_2")
          updateCheckboxInput(session, "zo_calc_2", value=FALSE)
          dur = amt/input$zo_rate_2
        }
        if(input$input_type_2=="First-Order") {
          enable("fo_rate_2")
          #enable("n_tHalf_2")
          updateCheckboxInput(session, "fo_calc_2", value=FALSE)
          dur = input$n_tHalf_2 * log(2)/input$fo_rate_2
        }
        if(input$input_type_2=="Higuchi") {
          enable("sqrt_rate_2")
          updateCheckboxInput(session, "sqrt_calc_2", value=FALSE)
          dur = (amt/input$sqrt_rate_2)^2
        }
        updateNumericInput(session,"dur_2", value = dur)
        disable("dur_2")
        calc2$dur = TRUE
      } else {
        enable("dur_2")
        calc2$dur = FALSE
      }
    })

    observeEvent(input$calc_dur_3, {
      if(input$calc_dur_3==TRUE) {
        amt = input$dose_total * input$dose_F * input$frac_3 * 1000
        calc3$zo = FALSE
        calc3$fo = FALSE
        calc3$sqrt = FALSE
        if(input$input_type_3=="Zero-Order") {
          enable("zo_rate_3")
          updateCheckboxInput(session, "zo_calc_3", value=FALSE)
          dur = amt/input$zo_rate_3
        }
        if(input$input_type_3=="First-Order") {
          enable("fo_rate_3")
          #enable("n_tHalf_3")
          updateCheckboxInput(session, "fo_calc_3", value=FALSE)
          dur = input$n_tHalf_3 * log(2)/input$fo_rate_3
        }
        if(input$input_type_3=="Higuchi") {
          enable("sqrt_rate_3")
          updateCheckboxInput(session, "sqrt_calc_3", value=FALSE)
          dur = (amt/input$sqrt_rate_3)^2
        }
        updateNumericInput(session, "dur_3", value = dur)
        disable("dur_3")
        calc3$dur = TRUE
      } else {
        enable("dur_3")
        calc3$dur = FALSE
      }
    })

    # * 11.2 Zero-order Rates ----
    observeEvent(input$zo_calc_1, {
      if(input$zo_calc_1) {
        enable("dur_1")
        disable("zo_rate_1")
        calc1$dur = FALSE
        calc1$zo = TRUE
        updateCheckboxInput(session, "calc_dur_1", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_1 * 1000
        rate = amt/input$dur_1
        updateNumericInput(session, "zo_rate_1", value=rate)
      }
      else {
        enable("zo_rate_1")
        calc1$zo = FALSE
      }
    })

    observeEvent(input$zo_calc_2, {
      if(input$zo_calc_2) {
        enable("dur_2")
        disable("zo_rate_2")
        calc2$dur = FALSE
        calc2$zo = TRUE
        updateCheckboxInput(session, "calc_dur_2", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_2 * 1000
        rate = amt/input$dur_2
        updateNumericInput(session, "zo_rate_2", value=rate)
      }
      else {
        enable("zo_rate_2")
        calc2$zo = FALSE
      }
    })

    observeEvent(input$zo_calc_3, {
      if(input$zo_calc_3) {
        enable("dur_3")
        disable("zo_rate_3")
        calc3$dur = FALSE
        calc3$zo = TRUE
        updateCheckboxInput(session, "calc_dur_3", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_3 * 1000
        rate = amt/input$dur_3
        updateNumericInput(session, "zo_rate_3", value=rate)
      }
      else {
        enable("zo_rate_3")
        calc3$zo = FALSE
      }
    })

    # * 11.3 First-order Rates ----
    observeEvent(input$fo_calc_1, {
      if(input$fo_calc_1) {
        enable("dur_1")
        disable("fo_rate_1")
        calc1$dur = FALSE
        calc1$fo = TRUE
        updateCheckboxInput(session, "calc_dur_1", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_1 * 1000
        rate = input$n_tHalf_1*log(2)/input$dur_1
        updateNumericInput(session, "fo_rate_1", value=rate)
      }
      else {
        enable("fo_rate_1")
        calc1$fo = FALSE
      }
    })

    observeEvent(input$fo_calc_2, {
      if(input$fo_calc_2) {
        enable("dur_2")
        disable("fo_rate_2")
        calc2$dur = FALSE
        calc2$fo = TRUE
        updateCheckboxInput(session, "calc_dur_2", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_2 * 1000
        rate = input$n_tHalf_2*log(2)/input$dur_2
        updateNumericInput(session, "fo_rate_2", value=rate)
      }
      else {
        enable("fo_rate_2")
        calc2$fo = FALSE
      }
    })

    observeEvent(input$fo_calc_3, {
      if(input$fo_calc_3) {
        enable("dur_3")
        disable("fo_rate_3")
        calc3$dur = FALSE
        calc3$fo = TRUE
        updateCheckboxInput(session, "calc_dur_3", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_3 * 1000
        rate = input$n_tHalf_3*log(2)/input$dur_3
        updateNumericInput(session, "fo_rate_3", value=rate)
      }
      else {
        enable("fo_rate_3")
        calc3$fo = FALSE
      }
    })

    # 11.3 Square-root-time Rates ----
    observeEvent(input$sqrt_calc_1, {
      if(input$sqrt_calc_1) {
        enable("dur_1")
        disable("sqrt_rate_1")
        calc1$dur = FALSE
        calc1$sqrt = TRUE
        updateCheckboxInput(session, "calc_dur_1", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_1 * 1000
        rate = amt/sqrt(input$dur_1)
        updateNumericInput(session, "sqrt_rate_1", value=rate)
      }
      else {
        enable("sqrt_rate_1")
        calc1$sqrt = FALSE
      }
    })

    observeEvent(input$sqrt_calc_2, {
      if(input$sqrt_calc_2) {
        enable("dur_2")
        disable("sqrt_rate_2")
        calc2$dur = FALSE
        calc2$sqrt = TRUE
        updateCheckboxInput(session, "calc_dur_2", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_2 * 1000
        rate = amt/sqrt(input$dur_2)
        updateNumericInput(session, "sqrt_rate_2", value=rate)
      }
      else {
        enable("sqrt_rate_2")
        calc2$sqrt = FALSE
      }
    })

    observeEvent(input$sqrt_calc_3, {
      if(input$sqrt_calc_3) {
        enable("dur_3")
        disable("sqrt_rate_3")
        calc3$dur = FALSE
        calc3$sqrt = TRUE
        updateCheckboxInput(session, "calc_dur_3", value=FALSE)
        amt = input$dose_total * input$dose_F * input$frac_3 * 1000
        rate = amt/sqrt(input$dur_3)
        updateNumericInput(session, "sqrt_rate_3", value=rate)
      }
      else {
        enable("sqrt_rate_3")
        calc3$sqrt = FALSE
      }
    })
    



# 12. Plotting Functions --------------------------------------------------
## See plot 1 code for detailed description

    
    # * 12.1 Vaginal Lumen ----
    output$plot1 <- renderPlot({
      
      # Plots will automatically update whenever the simulation is run
        out <- results()
        
        simResults = out
        
        # Output is in mg/L, but amounts in the model are in ug
        mass_conv_factor = 1e-3
        
        # Extract the total amount in the lumen compartment and multiply by mass conversion factor
        yData_total = out$C_totLUMEN*mass_conv_factor
        yData_solution = out$LUMEN_SOLUTION*mass_conv_factor
        yData_unbound = out$LUMEN_UNBOUND*mass_conv_factor
        yData_bound = out$LUMEN_BOUND*mass_conv_factor
        yData_precip = out$LUMEN_PRECIP*mass_conv_factor
        
        # Indicate which compartment is being plotted
        CMT = "C_totLUMEN"
        # x and y labels
        xLabel = "Time [days]"
        yLabel = "Lumen Concentration [mg/L]"
        
        
        
        # Assign minimum x value from plot options panel
          x_min = input$x_min1
        
        # Check if the option to fit the plot window to data has been selected
          if(input$fit_plot1) {
            
          # If so, set minimum x value to the duration of the simulation,
          # Set y minimum and maximum to 90% of data minimum and 110% of data maximum, respectively
            x_max = input$SimTime
            y_min = min(yData_total)*0.9
            y_max = max(yData_total)*1.1
          } else {
            
          # If not, assign x max, y min, and y max based on user input  
            x_max = input$x_max1
            y_min = input$y_min1
            y_max = input$y_max1
          }
        

        user_obs <- user_obs_for_plot("Lumen")
        panelPlots_C_totLUMEN(simResults, yData_total, yData_solution, yData_unbound, yData_bound, yData_precip, xLabel, yLabel, CMT,
                              total = input$lumen_total, solution = input$lumen_solution, unbound = input$lumen_unbound, bound = input$lumen_bound, precip = input$lumen_precip,
                              logScale=input$log_Lumen, CHIC_COC=input$CHIC_cvl_COC, CHIC_IUD=input$CHIC_cvl_IUD,
                              xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind, user_obs = user_obs)
    })

    
    # * 12.2 Vaginal Epithelium ----
    output$plot2 <- renderPlot({
        out <- results()
        
        simResults = out
        mass_conv_factor = 1e-3
        yData = out$C_totEPI*mass_conv_factor
        CMT = "C_totEPI"
        xLabel = "Time [days]"
        yLabel = "Epithelium Concentration [mg/L]"
        
        x_min = input$x_min2
        
        if(input$fit_plot2) {
          x_max = input$SimTime
          y_min = min(yData)*0.9
          y_max = max(yData)*1.1
        }
        else {
          x_max = input$x_max2
          y_min = input$y_min2
          y_max = input$y_max2
        }
        
        user_obs <- user_obs_for_plot("Epi")
        panelPlots_C_tissue(simResults, yData, xLabel, yLabel, CMT, logScale=input$log_Epi,
                            xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind, user_obs = user_obs)
    })
    
    # * 12.3 Vaginal Stroma ----
    output$plot3 <- renderPlot({
        out <- results()
        
        simResults = out
        mass_conv_factor = 1e-3
        yData = out$C_totSTROMA*mass_conv_factor
        CMT = "C_totSTROMA"
        xLabel = "Time [days]"
        yLabel = "Stroma Concentration [mg/L]"
        
        x_min = input$x_min3
        
        if(input$fit_plot3) {
          x_max = input$SimTime
          y_min = min(yData)*0.9
          y_max = max(yData)*1.1
        }
        else {
          x_max = input$x_max3
          y_min = input$y_min3
          y_max = input$y_max3
        }
        
        user_obs <- user_obs_for_plot("Stroma")
        panelPlots_C_tissue(simResults, yData, xLabel, yLabel, CMT, logScale=input$log_Stroma,
                            xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind, user_obs = user_obs)
        
    })
    
    # * 12.4 Plasma ----
    output$plot4 <- renderPlot({
        out <- results()
        
        simResults = out
        mass_conv_factor = 1e-3
        BP = input$BP
        yData = out$VEINS*mass_conv_factor/BP
        CMT = "VEINS"
        xLabel = "Time [days]"
        yLabel = "Plasma Concentration [mg/L]"
        
        x_min = input$x_min4
        
        if(input$fit_plot4) {
          x_max = input$SimTime
          y_min = min(yData)*0.9
          y_max = max(yData)*1.1
        }
        else {
          x_max = input$x_max4
          y_min = input$y_min4
          y_max = input$y_max4
        }
        
        user_obs <- user_obs_for_plot("Plasma")
        panelPlots_plasma (simResults, yData, xLabel, yLabel, CMT,
                           logScale=input$log_Plasma,
                           selector=input$Plasma_selector,
                           errorBars=input$Plasma_error,
                           xmin=x_min, xmax=x_max,
                           ymin=y_min, ymax=y_max,
                           num_ind = input$num_ind,
                           user_obs = user_obs,
                           fitRange = input$fit_plot4)
    })
    
    # * 12.5 Cervical Fluid ----
    output$plot5 <- renderPlot({
      out <- results()
      
      simResults = out
      mass_conv_factor = 1e-3
      yData = out$CERVIX_FLUID*mass_conv_factor
      CMT = "CERVIX_FLUID"
      xLabel = "Time [days]"
      yLabel = "Cervical Fluid Concentration [mg/L]"
      
      x_min = input$x_min5
      
      if(input$fit_plot5) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max5
        y_min = input$y_min5
        y_max = input$y_max5
      }
      
      user_obs <- user_obs_for_plot("CF")
      panelPlots_CF (simResults, yData, xLabel, yLabel, CMT, logScale=input$log_CF,
                     xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind, user_obs = user_obs)
      
    })
    
    # * 12.6 Cervical Epithelium ----
    output$plot6 <- renderPlot({
      out <- results()
      
      simResults = out
      mass_conv_factor = 1e-3
      yData = out$CERVIX_EPITHELIUM*mass_conv_factor
      CMT = "CERVIX_EPITHELIUM"
      xLabel = "Time [days]"
      yLabel = "Cervical Epithelium Concentration [mg/L]"
      
      x_min = input$x_min6
      
      if(input$fit_plot6) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max6
        y_min = input$y_min6
        y_max = input$y_max6
      }
      
      user_obs <- user_obs_for_plot("CE")
      panelPlots_C_Epi (simResults, yData, xLabel, yLabel, CMT, logScale=input$log_CE,
                        xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind, user_obs = user_obs)
      
    })
    
    
    # * 12.7 Cervical Myometrium ----
    output$plot7 <- renderPlot({
      out <- results()
      
      simResults = out
      mass_conv_factor = 1e-3
      yData = out$CERVIX_MYOMETRIUM*mass_conv_factor
      CMT = "CERVIX_MYOMETRIUM"
      xLabel = "Time [days]"
      yLabel = "Cervical Myometrium Concentration [mg/L]"
      
      x_min = input$x_min7
      
      if(input$fit_plot7) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max7
        y_min = input$y_min7
        y_max = input$y_max7
      }
      
      user_obs <- user_obs_for_plot("CM")
      panelPlots_C_Myo (simResults, yData, xLabel, yLabel, CMT, logScale=input$log_CM,
                        xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind, user_obs = user_obs)
      
    })
    
    # * 12.8 Uterine Fluid ----
    output$plot8 <- renderPlot({
      out <- results()
      
      simResults = out
      mass_conv_factor = 1e-3
      yData = out$UTERUS_FLUID*mass_conv_factor
      CMT = "UTERUS_FLUID"
      xLabel = "Time [days]"
      yLabel = "Uterine Fluid Concentration [mg/L]"
      
      x_min = input$x_min8
      
      if(input$fit_plot8) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max8
        y_min = input$y_min8
        y_max = input$y_max8
      }
      
      user_obs <- user_obs_for_plot("UF")
      panelPlots_UF (simResults, yData, xLabel, yLabel, CMT, logScale=input$log_UF, 
                     xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind, user_obs = user_obs)
      
    })
    
    # * 12.9 Endometrium ----
    output$plot9 <- renderPlot({
      out <- results()
      
      simResults = out
      mass_conv_factor = 1e-3
      yData = out$UTERUS_ENDOMETRIUM*mass_conv_factor
      CMT = "UTERUS_ENDOMETRIUM"
      xLabel = "Time [days]"
      yLabel = "Endometrium Concentration [mg/L]"
      
      x_min = input$x_min9
      
      if(input$fit_plot9) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max9
        y_min = input$y_min9
        y_max = input$y_max9
      }
      
      user_obs <- user_obs_for_plot("UE")
      panelPlots_U_Endo (simResults, yData, xLabel, yLabel, CMT,logScale=input$log_UE, errorBars=input$UE_error,
                         Nilsson_IUD=input$Nilsson_IUD_Endo, Nilsson_Oral=input$Nilsson_Oral_Endo,
                         Devoto_Oral=input$Devoto_Oral_Endo, Devoto_Vaginal=input$Devoto_Vaginal_Endo,
                         xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind,
                         user_obs = user_obs, fitRange = input$fit_plot9)
      
    })
    
    # * 12.10 Uterine Myometrium ----
    output$plot10 <- renderPlot({
      out <- results()
      
      simResults = out
      mass_conv_factor = 1e-3
      yData = out$UTERUS_MYOMETRIUM*mass_conv_factor
      CMT = "UTERUS_MYOMETRIUM"
      xLabel = "Time [days]"
      yLabel = "Uterine Myometrium Concentration [mg/L]"
      
      x_min = input$x_min10
      
      if(input$fit_plot10) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max10
        y_min = input$y_min10
        y_max = input$y_max10
      }
      
      user_obs <- user_obs_for_plot("UM")
      panelPlots_U_Myo (simResults, yData, xLabel, yLabel, CMT, logScale=input$log_UM, errorBars=input$UM_error,
                        Nilsson_IUD=input$Nilsson_IUD_Myo, Nilsson_Oral=input$Nilsson_Oral_Myo,
                        xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind,
                        user_obs = user_obs, fitRange = input$fit_plot10)
      
    })
    
    # * 12.11 Stroma Blood ----
    output$plot11 <- renderPlot({
      out <- results()
      
      simResults = out
      mass_conv_factor = 1e-3
      yData = out$STROMA_BLOOD*mass_conv_factor
      CMT = "STROMA_BLOOD"
      xLabel = "Time [days]"
      yLabel = "Stroma Blood Concentration [mg/L]"
      
      x_min = input$x_min11
      
      if(input$fit_plot11) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max11
        y_min = input$y_min11
        y_max = input$y_max11
      }
      
      panelPlots_stroma_blood (simResults, yData, xLabel, yLabel, CMT,
                         logScale=input$log_SB,
                         xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind)
    })
    
    depot_CMT <- reactive(input$admin_route)
    
    # * 12.12 Depot Compartment ----
    output$plot12 <- renderPlot({
      out <- results()
      simResults <- out
      mass_conv_factor = 1e-3
      yLabel <- switch(depot_CMT(),
             "Oral" = "Mass in Gut Depot [mg]",
             "Uterine" = "Mass in Uterine Depot [mg]",
             "Vaginal" = "Mass in Vaginal Depot [mg]"
              )
      
      yData <- switch(depot_CMT(),
                            "Oral" = out$G_DEPOT*mass_conv_factor,
                            "Uterine" = out$U_DEPOT*mass_conv_factor,
                            "Vaginal" = out$V_DEPOT*mass_conv_factor
                            )
      
      CMT <- switch(depot_CMT(),
                      "Oral" = "G_DEPOT",
                      "Uterine" = "U_DEPOT",
                      "Vaginal" = "V_DEPOT"
                    )
      
      xLabel <- "Time [days]"
      
      x_min = input$x_min12
      
      if(input$fit_plot12) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max12
        y_min = input$y_min12
        y_max = input$y_max12
      }
      
      panelPlots_depot(simResults, yData, xLabel, yLabel, CMT,
                       logScale=input$log_depot,
                       xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, num_ind = input$num_ind)
      
      
    })
    
    
    
    observeEvent(input$resetParams, {
      reset("params")
      delay(100, updateSelectInput(session, "study", selected = ""))
    })
    
    # * 12.13 Mass Balance Check ----
    output$plot13 <- renderPlot({
      out <- results()
      
      simResults = out

      yData = out$mass_balance
      CMT = "mass_balance"
      xLabel = "Time [days]"
      yLabel = "Mass Balance [mg]"
      
      x_min = input$x_min13
      
      if(input$fit_plot13) {
        x_max = input$SimTime
        y_min = min(yData)*0.9
        y_max = max(yData)*1.1
      }
      else {
        x_max = input$x_max13
        y_min = input$y_min13
        y_max = input$y_max13
      }
      
      panelPlots_MB(simResults, yData, xLabel, yLabel, CMT, logScale=input$log_MB,
                               xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max)
    })
    
    
# 13. Plot Interaction ----------------------------------------------

    # Observer to show and hide plot options
    observeEvent(input$outputTabset, {
      plotList <- list("Lumen", "Epi", "Stroma", "Plasma",
                       "CF", "CE", "CM", "UF",
                       "UE", "UM", "SM", "Depot")
      
      activePlot <- input$outputTabset
      
      for (n in 1:length(plotList)) {
        toggle(id=paste0("plot", n,"Ops"), condition = plotList[n] == activePlot)
      }
    })
    
    user_defined_missing_inputs <- function() {
      phase_count <- suppressWarnings(as.integer(input$input_phases))
      if(length(phase_count) == 0 || is.na(phase_count)) {
        return("Please select the number of input phases.")
      }
      
      required_inputs <- list(
        "dose" = input$dose_total,
        "number of doses" = input$num_dose,
        "simulation time" = input$SimTime
      )
      
      if(!is.null(input$num_dose) && !is.na(input$num_dose) && input$num_dose > 1) {
        required_inputs[["inter-dose interval"]] <- input$II
      }
      
      for (i in seq_len(phase_count)) {
        required_inputs[[paste0("phase ", i, " duration")]] <- input[[paste0("dur_", i)]]
        rate_input <- switch(input[[paste0("input_type_", i)]],
                             "Zero-Order" = paste0("zo_rate_", i),
                             "First-Order" = paste0("fo_rate_", i),
                             "Higuchi" = paste0("sqrt_rate_", i))
        required_inputs[[paste0("phase ", i, " release rate")]] <- input[[rate_input]]
      }
      
      missing_labels <- names(required_inputs)[vapply(required_inputs, function(value) {
        is.null(value) || length(value) == 0 || is.na(value)
      }, logical(1))]
      
      if(length(missing_labels) > 0) {
        return(paste0("Please provide: ", paste(missing_labels, collapse = ", "), "."))
      }
      
      NULL
    }
    
    observe({
      if(isTRUE(input$study == "User_Defined") && !isTRUE(userDefinedReady())) {
        hide("simulation_output_tabs")
        show("user_defined_prompt")
      } else {
        hide("user_defined_prompt")
        show("simulation_output_tabs")
      }
    })
    
    observeEvent(input$runSim, {
      if(isTRUE(input$study == "User_Defined")) {
        missing_message <- user_defined_missing_inputs()
        if(is.null(missing_message)) {
          userDefinedReady(TRUE)
        } else {
          userDefinedReady(FALSE)
          userDefinedPrompt(missing_message)
        }
      }
    })
    
    


# 14. Vignettes ---------------------------------------------------------------
  
  # * 14.1 Nilsson 1982 Oral ----
  applyStudyVignette <- function(studyVignette) {
    if(is.null(studyVignette) || studyVignette=="") {
      return(invisible(NULL))
    }
    
    if(studyVignette=="User_Defined") {
      userDefinedReady(FALSE)
      userDefinedPrompt("Please provide your input, then click Run Simulation.")
      updateRadioButtons(session, "admin_route", selected="Uterine")
      updateRadioButtons(session, "input_phases", selected="1")
      updateSelectInput(session, "input_type_1", selected="Zero-Order")
      updateRadioButtons(session, "input_1_scale", selected="Days")
      updateNumericInput(session, "dose_total", value=30)
      updateNumericInput(session, "num_dose", value=1)
      updateNumericInput(session, "II", value=24)
      updateNumericInput(session, "SimTime", value=7)
      updateCheckboxInput(session, "calc_dur_1", value=FALSE)
      updateCheckboxInput(session, "strict_dur", value=FALSE)
      updateNumericInput(session, "dur_1", value=30*1000/20)
      delay(300, {
        updateNumericInput(session, "zo_rate_1", value=20)
        updateCheckboxInput(session, "calc_dur_1", value=TRUE)
        updateNumericInput(session, "dur_1", value=30*1000/20)
      })
      updateSelectInput(session, "Plasma_selector", selected=character(0))
      updateCheckboxInput(session, "Nilsson_IUD_Endo", value = 0)
      updateCheckboxInput(session, "Nilsson_Oral_Endo", value = 0)
      updateCheckboxInput(session, "Nilsson_IUD_Myo", value = 0)
      updateCheckboxInput(session, "Nilsson_Oral_Myo", value = 0)
      return(invisible(NULL))
    }
    
    if(studyVignette=="Nilsson_Oral") {
      userDefinedReady(TRUE)
      updateRadioButtons(session, "admin_route", selected="Oral")
      updateRadioButtons(session, "input_phases", selected="1")
      updateSelectInput(session, "input_type_1", selected="First-Order")
      updateRadioButtons(session, "input_1_scale", selected="Hours")
      updateNumericInput(session, "dose_total", value=0.25)
      updateNumericInput(session, "num_dose", value=7)
      updateNumericInput(session, "SimTime", value=9)
      updateRadioButtons(session, "input_1_scale", selected="Hours")
      updateCheckboxInput(session, "calc_dur_1", value=FALSE)
      updateCheckboxInput(session, "strict_dur", value=FALSE)
      delay(300, {
        updateNumericInput(session, "dur_1", value=8*log(2)/0.75)
        updateNumericInput(session, "fo_rate_1", value=0.75)
      })
      updateTabsetPanel(session, "outputTabset", selected="Plasma")
      updateSelectInput(session, "Plasma_selector", selected="Nilsson_Oral")
      updateCheckboxInput(session, "log_Plasma", value=0)
      updateCheckboxInput(session, "Nilsson_Oral_Endo", value = 1)
      updateCheckboxInput(session, "Nilsson_Oral_Myo", value = 1)
      updateCheckboxInput(session, "fit_plot4", value=1)
      updateCheckboxInput(session, "fit_plot9", value=1)
      updateCheckboxInput(session, "fit_plot10", value=1)
      updateNumericInput(session, "x_max4",  value=10)
      updateNumericInput(session, "y_max4",  value=0.0005)
      updateNumericInput(session, "x_max9",  value=10)
      updateNumericInput(session, "y_max9",  value=0.006)
      updateNumericInput(session, "x_max10",  value=10)
      updateNumericInput(session, "y_max10",  value=0.0025)
      delay(1000, click("runSim"))
    }
    
    # * 14.1 Nilsson 1982 IUD ----
    if(studyVignette=="Nilsson_IUD") {
      userDefinedReady(TRUE)
      updateRadioButtons(session, "admin_route", selected="Uterine")
      updateRadioButtons(session, "input_phases", selected="1")
      updateSelectInput(session, "input_type_1", selected="Zero-Order")
      updateRadioButtons(session, "input_1_scale", selected="Days")
      updateNumericInput(session, "dose_total", value=63)
      updateNumericInput(session, "num_dose", value=1)
      updateNumericInput(session, "SimTime", value=60)
      updateRadioButtons(session, "input_1_scale", selected="Days")
      updateCheckboxInput(session, "calc_dur_1", value=FALSE)
      delay(300, {
        updateNumericInput(session, "dur_1", value=50)
        updateNumericInput(session, "zo_rate_1", value=30)
      })
      updateTabsetPanel(session, "outputTabset", selected="UE")
      updateSelectInput(session, "Plasma_selector", selected="Nilsson_IUD")
      updateCheckboxInput(session, "log_Plasma", value=0)
      updateCheckboxInput(session, "Nilsson_IUD_Endo", value = 1)
      updateCheckboxInput(session, "Nilsson_IUD_Myo", value = 1)
      updateCheckboxInput(session, "fit_plot4", value=1)
      updateCheckboxInput(session, "fit_plot9", value=1)
      updateCheckboxInput(session, "fit_plot10", value=1)
      updateNumericInput(session, "x_max4",  value=60)
      updateNumericInput(session, "y_max4",  value=0.0005)
      updateNumericInput(session, "x_max9",  value=60)
      updateNumericInput(session, "y_max9",  value=1.5)
      updateNumericInput(session, "x_max10",  value=60)
      updateNumericInput(session, "y_max10",  value=0.0055)
      updateCheckboxInput(session, "strict_dur", value=TRUE)
      delay(1000, click("runSim"))
    }
    
    invisible(NULL)
  }
  
  observeEvent(input$study, {
    applyStudyVignette(input$study)
  }, ignoreInit = FALSE)
    pmt <- reactive({
    param_converted <- list()
    for (i in 1:3){
      scalevar <- input[[paste("input",i,"scale",sep="_")]]
      if(scalevar=="Days"){
        param_converted[paste0("dur_",i)] <- rep_null(input[[paste0("dur_",i)]])*24
        param_converted[paste0("td_",i)] <- rep_null(input[[paste0("td_",i)]])*24
        param_converted[paste0("tlag_",i)] <- rep_null(input[[paste0("tlag_",i)]])*24
        param_converted[paste0("zo_rate_",i)] <- rep_null(input[[paste0("zo_rate_",i)]])/24
        param_converted[paste0("fo_rate_",i)] <- rep_null(input[[paste0("fo_rate_",i)]])/24
        param_converted[paste0("sq_rate_",i)] <- rep_null(input[[paste0("sqrt_rate_",i)]])/sqrt(24)
      }
      else if(scalevar=="Hours"){
        param_converted[paste0("dur_",i)] <- rep_null(input[[paste0("dur_",i)]])
        param_converted[paste0("td_",i)] <- rep_null(input[[paste0("td_",i)]])
        param_converted[paste0("tlag_",i)] <- rep_null(input[[paste0("tlag_",i)]])
        param_converted[paste0("zo_rate_",i)] <- rep_null(input[[paste0("zo_rate_",i)]])
        param_converted[paste0("fo_rate_",i)] <- rep_null(input[[paste0("fo_rate_",i)]])
        param_converted[paste0("sq_rate_",i)] <- rep_null(input[[paste0("sqrt_rate_",i)]])
      }
      
    }
    param_converted["dose"] <- input$dose_total*1000
    
    
    parameters <- list(
      dur1 = input$dur_1,
      dur2 = input$dur_2,
      dur3 = input$dur_3,
      F = input$dose_F,
      f1 = input$frac_1,
      f2 = input$frac_2,
      f3 = input$frac_3,
      dose = input$dose_total,
      k_LE = input$k_LE,
      k_EL = input$k_EL,
      k_EVS = input$k_EVS,
      k_VSE = input$k_VSE,
      k_VS_TtoB = input$k_VS_TtoB,
      k_VS_BtoT = input$k_VS_BtoT,
      k_activeEtoS = input$k_activeEtoS,
      k_active_StrTtoB = input$k_active_StrTtoB,
      # CL_H = input$CL_H,
      CL_R = input$CL_R,
      P_H = input$P_H,
      P_R = input$P_R,
      P_B = input$P_B,
      P_G = input$P_G,
      P_S = input$P_S,
      P_AD = input$P_AD,
      P_Lungs = input$P_Lungs,
      P_VS = input$P_VS,
      pH_Lumen = input$pH_Lumen,
      pH_Body = input$pH_Body,
      pKa = input$pKa,
      solubility = input$sol,
      f_unbound = input$f_u_percent/100,
      k_L_CF = input$k_L_CF,
      k_CF_L = input$k_CF_L,
      k_VE_CE = input$k_VE_CE,
      k_CE_VE = input$k_CE_VE,
      k_VS_CM = input$k_VS_CM,
      k_CM_VS = input$k_CM_VS,
      k_CF_CE = input$k_CF_CE,
      k_CE_CF = input$k_CE_CF,
      k_CE_CM = input$k_CE_CM,
      k_CM_CE = input$k_CM_CE,
      k_CM_TtoB = input$k_CM_TtoB,
      k_CM_BtoT = input$k_CM_BtoT,
      k_CF_UF = input$k_CF_UF,
      k_UF_CF = input$k_UF_CF,
      k_CE_UE = input$k_CE_UE,
      k_UE_CE = input$k_UE_CE,
      k_CM_UM = input$k_CM_UM,
      k_UM_CM = input$k_UM_CM,
      k_UF_UE = input$k_UF_UE,
      #k_UE_UF = input$k_UE_UF,
      #k_UE_UM = input$k_UE_UM,
      #k_UM_UE = input$k_UM_UE,
      #k_UM_TtoB = input$k_UM_TtoB,
      k_UM_BtoT = input$k_UM_BtoT,
      P_CM = input$P_CM,
      P_UM = input$P_UM,
      ZO1 = input$zo_rate_1,
      ZO2 = input$zo_rate_2,
      ZO3 = input$zo_rate_3,
      FO1 = input$fo_rate_1,
      FO2 = input$fo_rate_2,
      FO3 = input$fo_rate_3,
      SQ1 = input$sqrt_rate_1,
      SQ2 = input$sqrt_rate_2,
      SQ3 = input$sqrt_rate_3,
      TD1 = input$td_1,
      TD2 = input$td_2,
      TD3 = input$td_3,
      lag1 = input$tlag_1,
      lag2 = input$tlag_2,
      lag3 = input$tlag_3,
      P_Heart = input$P_Heart,
      P_Bone = input$P_Bone,
      P_Muscle = input$P_Muscle,
      P_Skin = input$P_Skin,
      #CL_L = input$CL_L
      BP = input$BP,
      population_seed = input$pop_seed
    )
    
    
    # Update parameter values with converted units
    for (i in 1:3){
      parameters[paste0("dur",i)] <- param_converted[paste0("dur_",i)]
      parameters[paste0("TD",i)] <- param_converted[paste0("td_",i)]
      parameters[paste0("lag",i)] <- param_converted[paste0("tlag_",i)]
      parameters[paste0("ZO",i)] <- param_converted[paste0("zo_rate_",i)]
      parameters[paste0("FO",i)] <- param_converted[paste0("fo_rate_",i)]
      parameters[paste0("SQ",i)] <- param_converted[paste0("sq_rate_",i)]
    }
    parameters["dose"] <- param_converted["dose"]
    
    # Set flags for route of administration
    if(input$admin_route=="Oral") {
      parameters["G_Flag"] = 1
      parameters["U_Flag"] = 0
      parameters["V_Flag"] = 0
    }
    
    if(input$admin_route=="Uterine") {
      parameters["G_Flag"] = 0
      parameters["U_Flag"] = 1
      parameters["V_Flag"] = 0
    }
    
    if(input$admin_route=="Vaginal") {
      parameters["G_Flag"] = 0
      parameters["U_Flag"] = 0
      parameters["V_Flag"] = 1
    }
    
    # Set flag for simultaneous vs sequential dosing
    parameters["Simult"] <- switch(input$input_seq,
                                   "Simultaneous" = 1,
                                   "Sequential" = 0
    )
    return(parameters)
    })
    output$downloadData2 <- downloadHandler(
      filename = "parameters.csv",
      content = function(file) {
        write.csv(pmt(), file, row.names = FALSE)
      }
    )
}


# Run the application
shinyApp(ui = ui, server = server)
