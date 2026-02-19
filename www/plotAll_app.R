#=============================================================================
#           plotAll_app
#
#   R-code written to plot data in the shiny app only
#
#  created by : Katherine Kay
#  originally created : May 2018
#
#  last modified : Sep 9, 2019
#             by : Thomas Straubinger
#
#  last modified : Mar 14, 2024
#             by : Xinnong Li and Yifan yu
#=============================================================================
#
#
# 1 ng/mL = 0.001 mg/L
# 1 pg/mL = 1e-6 mg/L
# 
# Patient Data
#
# Densities of different body matter
# Blood - 1.0428 g/cm^3
# Bone - 1.75 g/cm^3
# Fat - 0.9094 g/cm^3
# Muscle - 1.0599 g/cm^3
#
# Converting ng/g to ng/ml should include a measure of the density
#   Currently know density of blood plasma = 1.06g/cm^3
#   Also know 1mL = 1cm^3
#   So ng/g * 1.06g/ml = ng/ml
#   Currently going to assume ng/g == ng/ml   1ng/ml = 0.001mg/L
#   pg/mg equivalent to ng/g which is equivalent to ng/ml

#"A10026" = plotColour[1] = "darkorange"
#"K06239" = plotColour[2] = "purple"
#"K02267" = plotColour[3] = "deepskyblue"
#"K05166" = plotColour[4] = "brown2"
#"A11261" = plotColour[5] = "blue"

#setwd("~/Bies Lab/FDA/PBPK_Simulator - Working Copy/www")

#=============================================================================
# ZimCHIC and CHIC2 Data

CHIC_comb <- read.csv("LNG_Data/LNG_CHIC.csv")


CHIC_cvl <- CHIC_comb %>% select("enroll_num", "contra", "cvl_conc", "time_days")
CHIC_plasma <- CHIC_comb %>% select("enroll_num", "contra", "plasma_conc", "time_days")

CHIC_cvl_COC <- CHIC_cvl %>% filter(contra=="COCs" & !is.na(cvl_conc))
CHIC_cvl_IUD <- CHIC_cvl %>% filter(contra=="LNG-IUD" & !is.na(cvl_conc))
CHIC_cvl_SUB <- CHIC_cvl %>% filter(contra=="LNG-I (Jadelle)" & !is.na(cvl_conc))

CHIC_plasma_COC <- CHIC_plasma %>% filter(contra=="COCs" & !is.na(plasma_conc))
CHIC_plasma_IUD <- CHIC_plasma %>% filter(contra=="LNG-IUD" & !is.na(plasma_conc))
CHIC_plasma_SUB <- CHIC_plasma %>% filter(contra=="LNG-I (Jadelle)" & !is.na(plasma_conc))

#=============================================================================
# Literature Data

Tremblay_2001_A <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Tremblay et al 2001", range = "A2:C17")
Tremblay_2001_B <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Tremblay et al 2001", range = "E2:G17")
Tremblay_2001_C <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Tremblay et al 2001", range = "I2:K17")
He_1990_HU <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "He C et al 1990", range = "A2:M8")
He_1990_CN <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "He C et al 1990", range = "A10:M16")
Kook_2002 <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Kook et al 2002", range = "A1:C19")
Landgren_1989 <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Landgren et al 1989", range = "A1:B16")
Johansson_2002_A <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Johansson et al 2002", range = "A2:C24")
Johansson_2002_B <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Johansson et al 2002", range = "D2:F25")
Johansson_2002_C <- read_excel("LNG_Data/LNG_Comb.xlsx", sheet = "Johansson et al 2002", range = "G2:I20")

Landgren_CI <- read.csv("LNG_Data/Landgren_CI_bars.csv")
Kook_error <- read.csv("LNG_Data/Kook_error_bars.csv")

# Note for Kyleena data - LCS 16 is Kyleena (19.5 mg LNG IUD), LCS 12 is Skyla (13.5 mg LNG IUD)
Mirena_FDA <- read.csv("LNG_Data/Mirena Comp C.csv")
Kyleena_FDA <- read.csv("LNG_Data/Kyleena Digitized QCed error bars.csv")
Skyla_FDA_1 <- read.csv("LNG_Data/Skyla Digitized QCed.csv")
Nilsson_1982_IUD <- read.csv("LNG_Data/Nilsson IUD.csv")
Nilsson_1982_oral <- read.csv("LNG_Data/Nilsson Oral.csv")
Kyleena_FDA_EB <- read.csv("LNG_Data/Kyleena Digitized QCed error bars.csv")

Skyla_FDA_2 <- Kyleena_FDA[1:12,-c(3,5,6)]
Kyleena_FDA <- Kyleena_FDA[,-c(4,7,8)]
Mirena_FDA <- Mirena_FDA[,-c(3,6)]

# Devoto Data
Devoto_endometrium <- read.csv("LNG_Data/Devoto 2005 endometrium nmol_mg.csv")
Devoto_plasma_oral <- read.csv("LNG_Data/Devoto 2005 oral nmol_L.csv")
Devoto_plasma_vaginal <- read.csv("LNG_Data/Devoto 2005 vaginal nmol_L.csv")

# Select columns and convert units: nmol/mg -> ng/mg = ug/g = ug/mL
Devoto_endo_vaginal <- Devoto_endometrium %>%
  select(Time_h, Vag_mean, Vag_sd) %>%
  mutate(Vag_mean = Vag_mean*312.446,
         Vag_sd = Vag_sd*312.446,
         Vag_upper = Vag_mean+Vag_sd,
         Vag_lower = Vag_mean-Vag_sd)
Devoto_endo_oral <- Devoto_endometrium %>%
  select(Time_h, Oral_mean, Oral_sd) %>%
  mutate(Oral_mean = Oral_mean*312.446,
         Oral_sd = Oral_sd*312.446,
         Oral_upper = Oral_mean + Oral_sd,
         Oral_lower = Oral_mean - Oral_sd)

# Convert from nmol/L to mg/L
Devoto_oral <- Devoto_plasma_oral %>%
  select(Time, Mean, SD) %>%
  mutate(Mean = Mean*312.446*1e-6,
         SD = SD*312.446*1e-6,
         Upper = Mean+SD,
         Lower = Mean-SD)
Devoto_vaginal <- Devoto_plasma_vaginal %>%
  select(Time=`Time..h.`, Mean, SD) %>%
  mutate(Mean = Mean*312.446*1e-6,
         SD = SD*312.446*1e-6,
         Upper = Mean+SD,
         Lower = Mean-SD)

  

# Change units from nmol/L to ng/mL
# Assume molecular weight of LNG is 312.446 g/mol
Landgren_1989 <- Landgren_1989 %>% mutate(`Conc (ng/mL)`=`Conc (nmol/L)`*312.446/1000)
Johansson_2002_A <- Johansson_2002_A %>% mutate(`Conc (ng/mL)` = `Conc (nmol/L)`*312.446/1000)
Johansson_2002_B <- Johansson_2002_B %>% mutate(`Conc (ng/mL)` = `Conc (nmol/L)`*312.446/1000)
Johansson_2002_C <- Johansson_2002_C %>% mutate(`Conc (ng/mL)` = `Conc (nmol/L)`*312.446/1000)
Tremblay_2001_A <- Tremblay_2001_A %>% mutate(`Mean (ng/mL)` = Mean*312.446/1000)
Tremblay_2001_B <- Tremblay_2001_B %>% mutate(`Mean (ng/mL)` = Mean*312.446/1000)
Tremblay_2001_C <- Tremblay_2001_C %>% mutate(`Mean (ng/mL)` = Mean*312.446/1000)
Landgren_CI <- Landgren_CI %>% mutate(Mean_ng_mL = Mean_nmol_L*312.446/1000,
                                      Upper_ng_mL = Upper*312.446/1000,
                                      Lower_ng_mL = Lower*312.446/1000)

# Fix column names
colnames(Mirena_FDA) <- c("Time_months", "Time_days", "Plas_mean_ng_L", "Plas_SD")
colnames(Kyleena_FDA) <- c("Time_months", "Time_days", "Plas_mean_ng_L", "upper", "lower")
colnames(Skyla_FDA_1) <- c("Time_days", "Plas_mean_ng_L", "upper", "lower")
colnames(Skyla_FDA_2) <- c("Time_months", "Time_days", "Plas_mean_ng_L", "upper", "lower")
colnames(Nilsson_1982_IUD) <- c("Time_days", "Plas_mean_ng_L", "Plas_SD", "Fat_mean_ng_g", "Fat_SD", "Myo_mean_ng_g", "Myo_SD", "Endo_mean_ng_g", "Endo_SD")
colnames(Nilsson_1982_oral) <- c("Time_days", "Plas_mean_ng_L", "Plas_SD", "Fat_mean_ng_g", "Fat_SD", "Myo_mean_ng_g", "Myo_SD", "Endo_mean_ng_g", "Endo_SD")

# Add columns for error bars
He_1990_CN <- He_1990_CN %>% mutate(upper = Mean + SD, lower = Mean - SD)
He_1990_HU <- He_1990_HU %>% mutate(upper = Mean + SD, lower = Mean - SD)
Tremblay_2001_A <- Tremblay_2001_A %>% mutate(upper = (Mean + SD)*312.446/1000,
                                              lower = (Mean - SD)*312.446/1000)
Tremblay_2001_B <- Tremblay_2001_B %>% mutate(upper = (Mean + SD)*312.446/1000,
                                              lower = (Mean - SD)*312.446/1000)
Tremblay_2001_C <- Tremblay_2001_C %>% mutate(upper = (Mean + SD)*312.446/1000,
                                              lower = (Mean - SD)*312.446/1000)
Mirena_FDA <- Mirena_FDA %>% mutate(upper = Plas_mean_ng_L + Plas_SD,
                                    lower = Plas_mean_ng_L - Plas_SD)
Nilsson_1982_IUD <- Nilsson_1982_IUD %>% mutate(Plas_upper = Plas_mean_ng_L + Plas_SD,
                                                Plas_lower = Plas_mean_ng_L - Plas_SD,
                                                Fat_upper = Fat_mean_ng_g + Fat_SD,
                                                Fat_lower = Fat_mean_ng_g - Fat_SD,
                                                Myo_upper = Myo_mean_ng_g + Myo_SD,
                                                Myo_lower = Myo_mean_ng_g - Myo_SD,
                                                Endo_upper = Endo_mean_ng_g + Endo_SD,
                                                Endo_lower = Endo_mean_ng_g - Endo_SD)
Nilsson_1982_oral <- Nilsson_1982_oral %>% mutate(Plas_upper = Plas_mean_ng_L + Plas_SD,
                                                  Plas_lower = Plas_mean_ng_L - Plas_SD,
                                                  Fat_lower = Fat_mean_ng_g - Fat_SD,
                                                  Myo_upper = Myo_mean_ng_g + Myo_SD,
                                                  Myo_lower = Myo_mean_ng_g - Myo_SD,
                                                  Endo_upper = Endo_mean_ng_g + Endo_SD,
                                                  Endo_lower = Endo_mean_ng_g - Endo_SD)




#===========================
# Plots
#===========================

############# Vaginal Luminal Fluid ###################

panelPlots_C_totLUMEN = function(simResults, yData_total, yData_solution, yData_unbound, yData_bound, yData_precip, xLabel, yLabel, CMT, 
                                 total, solution, unbound, bound, precip, logScale, CHIC_COC, CHIC_IUD, CHIC_SUB,
                                 xmin, xmax, ymin, ymax, num_ind)
{
    #options(scipen = 999)
    
    par(oma = c(5, 7, 0, 0))
    par(mar = c(1, 3, 6, 3))
    #par(fig= c(0, 1, 0, 0.8) )
    
    ## Common plot details
    plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                   "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                   "darkslateblue", "red")
    
    allYdata = c(yData_total, yData_solution, yData_bound, yData_unbound, yData_precip,
                 CHIC_cvl$cvl_conc*1e-6)
    
    #xRange_hrs = range(0, max(simResults$time))
    xRange_hrs = range(xmin, xmax)
    yRange=range(max(ymin,1e-12), ymax)
    
    # if (logScale==0) {
    #   yRange = range(ymin, ymax)
    #   plot(0, type="n", xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    # 
    # }
    # 
    # 
    # if (logScale==1) {
    #   yRange = range(min(ymin, 1e-12), ymax)
    #   plot(0, type="n", xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE,log="y")
    # 
    # }


    
    text(x=100, y=10, labels=as.character(max(allYdata,na.rm = TRUE)))
    
    ## Plot simulated results
    if(total==1){
      if(num_ind > 1) {
        if(logScale == FALSE) {
            plot(simResults$time[simResults$ID==1], yData_total[1:(length(yData_total)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, col = "black")
            for (i in 2:max(simResults$ID)){
              a<-(i-1)*(length(yData_total)/max(simResults$ID))+1
              b<-i*(length(yData_total)/max(simResults$ID))
              lines(simResults$time[simResults$ID==i], yData_total[a:b],lwd=2)
            }
          }
        if(logScale == TRUE) {
          plot(simResults$time, yData_total, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y", col = "black")
        }
      } else
      {
        if(logScale == 0) {
          plot(simResults$time, yData_total, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, col = "black")
        }
        if(logScale == 1) {
          plot(simResults$time, yData_total, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, col = "black", log = "y")

        }
      }
    }

    # 

   
# 
    if (solution == 1) {
      lines(simResults$time, yData_solution, type="l", lwd=2, col = "blue")
    }

    if (unbound == 1) {
      lines(simResults$time, yData_unbound, type="l", lwd=2, col = "orange")
    }

    if (bound == 1) {
      lines(simResults$time, yData_bound, type="l", lwd=2, col = "green")
    }

    if (precip == 1) {
      lines(simResults$time, yData_precip, type="l", lwd=2, col = "red")
    }
#     
# 
#     
    axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
    axis(side=2, las=1, cex.axis=1.4)
    box()

    legendLabels = c("Simulated", "In Solution", "Unbound", "Protein Bound", "In Suspension")
    legendCol     = c("black", "blue", "orange", "green", "red")
    lineType     = c(1, 1, 1, 1, 1)
    pointType     = c(NA)
    
    # Plot CHIC COC data
    if (CHIC_COC==1) {
        points(CHIC_cvl_COC$time_days, CHIC_cvl_COC$cvl_conc*1e-6, col=plotColour[1], pch=0, lwd=2)
        legendLabels = c(legendLabels, "CHIC Oral")
        legendCol     = c(legendCol, plotColour[1])
        lineType     = c(lineType, 1)
        pointType     = c(pointType, NA)
    }
    
    # Plot CHIC IUD data
    if (CHIC_IUD==1) {
      points(CHIC_cvl_IUD$time_days, CHIC_cvl_IUD$cvl_conc*1e-6, col=plotColour[2], pch=0, lwd=2)
      legendLabels = c(legendLabels, "CHIC IUD")
      legendCol     = c(legendCol, plotColour[2])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Mirena Data
    
    
    # # Plot CHIC Subdermal data
    # if (CHIC_SUB==1) {
    #   points(CHIC_cvl_SUB$time_days, CHIC_cvl_SUB$cvl_conc, col=plotColour[3], pch=0, lwd=2)
    #   legendLabels = c(legendLabels, "CHIC Subdermal")
    #   legendCol     = c(legendCol, plotColour[3])
    #   lineType     = c(lineType, 1)
    #   pointType     = c(pointType, NA)
    # }

    mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
    mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
    
     
    
    #par (fig= c(0, 1, 0.8, 1), new=TRUE )
    #par(mar = c(0, 0, 0, 0))
    par(xpd=TRUE)
    #plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
    
    legend("top", inset=c(0,-0.2), legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=5)
    
    #dev.set(p)
    
    options(scipen = 0)
    
}

################ Vagninal Stroma and  Epithelium ######################

panelPlots_C_tissue = function(simResults, yData, xLabel, yLabel, CMT, logScale,
                               xmin, xmax, ymin, ymax, num_ind)
{
    #options(scipen = 999)
    
    par(oma = c(5, 7, 0, 0))
    par(mar = c(1, 3, 1.5, 3))
    par(fig= c(0, 1, 0, 0.8) )
    
    allYdata = c(yData)

    #xRange_hrs = range(0, max(simResults$time))
    xRange_hrs = range(xmin, xmax)
    yRange=range(max(ymin,1e-12), ymax)
    
    if(num_ind > 1) {
      if(logScale == FALSE) {
        plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
        for (i in 2:max(simResults$ID)){
          a<-(i-1)*(length(yData)/max(simResults$ID))+1
          b<-i*(length(yData)/max(simResults$ID))
          lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
        }
      }
      if(logScale == TRUE) {
        plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      }
    } else
    {
      if(logScale == 0) {
        plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      }
      if(logScale == 1) {
        plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
        
      }
    }
    
    axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
    axis(side=2, las=1, cex.axis=1.4)
    box()
    
    legendLabels = c("Simulated")
    legendCol     = c("black")
    lineType     = c(1)
    pointType     = c(NA)

    mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
    mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
    
    
    par (fig= c(0, 1, 0.8, 1), new=TRUE )
    par(mar = c(0, 0, 0, 0))
    plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
    
    legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=6)
    
    options(scipen = 0)
    
}


###################### Venous Plasma ###############################

panelPlots_plasma = function(simResults, yData, xLabel, yLabel, CMT, logScale, selector, errorBars, xmin, xmax, ymin, ymax, num_ind)
{
    #options(scipen = 999)
    
    par(oma = c(5, 7, 0, 0))
    par(mar = c(1, 3, 1.5, 3))
    par(fig= c(0, 1, 0, 0.8) )
    
    #plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "forest green")
    plotColour = hcl.colors(30, palette = "Viridis")
    
    

    allYdata = c(yData, CHIC_plasma$plasma_conc*1e-6, Tremblay_2001_A$`Mean (ng/mL)`/1000, Tremblay_2001_B$`Mean (ng/mL)`/1000, Tremblay_2001_C$`Mean (ng/mL)`/1000,
                 He_1990_HU$Mean/1000, He_1990_CN$Mean/1000, Kook_2002$`Conc (ng/mL)`/1000, Landgren_1989$`Conc (ng/mL)`/1000,
                 Johansson_2002_A$`Conc (ng/mL)`/1000, Johansson_2002_B$`Conc (ng/mL)`/1000, Johansson_2002_C$`Conc (ng/mL)`/1000,
                 Mirena_FDA$Plas_mean_ng_L*1e-6, Kyleena_FDA$Plas_mean_ng_L*1e-6, Skyla_FDA_1$Plas_mean_ng_L*1e-6, Skyla_FDA_2$Plas_mean_ng_L*1e-6,
                 Nilsson_1982_IUD$Plas_upper*1e-6, Nilsson_1982_oral$Plas_upper*1e-6,
                 Devoto_oral$Upper, Devoto_vaginal$Upper,Landgren_CI$Upper_ng_mL/1000)
    
    #xRange_hrs = range(0, max(simResults$time))
    # Range is actually in Days, not hours
    xRange_hrs = range(xmin, xmax)
    
    # if(logScale==0) {
    #     yRange = range(0, max(allYdata, na.rm = TRUE))
    #     plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    # }
    # if(logScale==1) {
    #     yRange = range(min(allYdata[allYdata>0], na.rm=TRUE), (max(allYdata, na.rm=TRUE)*2))
    #     plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    # 
    # }
    
    yRange=range(max(ymin,1e-12), ymax)
    yData_summary <- simResults %>% group_by(ID)
    
    if(num_ind > 1) {
      yData_summary <- simResults %>%
        group_by(time) %>%
        summarize(
          med = median(simResults$VEINS),
          upper = quantile(simResults$VEINS, 0.95),
          lower = quantile(simResults$VEINS, 0.05)
        )

      if(logScale == FALSE) {
        plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
        for (i in 2:max(simResults$ID)){
          a<-(i-1)*(length(yData)/max(simResults$ID))+1
          b<-i*(length(yData)/max(simResults$ID))
          lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
        }
        # lines(simResults$time, yData_summary$med, lty=2, lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, col="red")
        #lines(yData_summary$time, yData_summary$upper, lty="dashed", lwd=2, col="red")
        #lines(yData_summary$time, yData_summary$lower, lty="dashed", lwd=2, col="red")
        #polygon(c(0,simResults$time),rev(c(0,simResults$time)), c(yData_summary$upper, rev(yData_summary$lower)), col = "grey" )
      }
      if(logScale == TRUE) {
        plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
        #lines(simResults$time, yData_summary$med, lty=2, lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, col="red")
        #lines(yData_summary$time, yData_summary$upper, lty="dashed", lwd=2, col="red")
        #lines(yData_summary$time, yData_summary$lower, lty="dashed", lwd=2, col="red")
        #polygon(c(0,simResults$time),rev(c(0,simResults$time)), c(yData_summary$upper, rev(yData_summary$lower)), col = "grey" )
      }
    } else
      {
      if(logScale == 0) {
        plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      }
      if(logScale == 1) {
        plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")

      }
    }
    
    
    
    axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
    axis(side=2, las=1, cex.axis=1.4)
    box()


    legendLabels = c("Simulated")
    legendCol     = c("black")
    lineType     = c(1)
    pointType     = c(NA)
    
    # Plot CHIC COC data
    if ("CHIC_COC" %in% selector) {
      points(CHIC_plasma_COC$time_days, CHIC_plasma_COC$plasma_conc*1e-6, col=plotColour[1], pch=0, lwd=2)
      legendLabels = c(legendLabels, "CHIC Oral")
      legendCol     = c(legendCol, plotColour[1])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot CHIC IUD data
    if ("CHIC_IUD" %in% selector) {
      points(CHIC_plasma_IUD$time_days, CHIC_plasma_IUD$plasma_conc*1e-6, col=plotColour[2], pch=0, lwd=2)
      legendLabels = c(legendLabels, "CHIC IUD")
      legendCol     = c(legendCol, plotColour[2])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # # Plot CHIC Subdermal data
    # if (CHIC_SUB==1) {
    #   points(CHIC_plasma_SUB$time_days, CHIC_plasma_SUB$plasma_conc, col=plotColour[3], pch=0, lwd=2)
    #   legendLabels = c(legendLabels, "CHIC Subdermal")
    #   legendCol     = c(legendCol, plotColour[3])
    #   lineType     = c(lineType, 1)
    #   pointType     = c(pointType, NA)
    # }
    
    # Plot Tremblay Group A
    if ("Tremblay_A" %in% selector) {
      points(Tremblay_2001_A$`Time (h)`/24, Tremblay_2001_A$`Mean (ng/mL)`/1000, col=plotColour[4], pch=0, lwd=2)
      if (errorBars){
        arrows(x0=Tremblay_2001_A$`Time (h)`/24, y0=Tremblay_2001_A$lower/1000, x1=Tremblay_2001_A$`Time (h)`/24, y1=Tremblay_2001_A$upper/1000,
               code=3, angle=90, length=0.05, col=plotColour[4],lwd=1)
      }
      legendLabels = c(legendLabels, "Tremblay A")
      legendCol     = c(legendCol, plotColour[4])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Tremblay Group B
    if ("Tremblay_B" %in% selector) {
      points(Tremblay_2001_B$`Time (h)`/24, Tremblay_2001_B$`Mean (ng/mL)`/1000, col=plotColour[5], pch=0, lwd=2)
      if (errorBars){
        arrows(x0=Tremblay_2001_B$`Time (h)`/24, y0=Tremblay_2001_B$lower/1000, x1=Tremblay_2001_B$`Time (h)`/24, y1=Tremblay_2001_B$upper/1000,
               code=3, angle=90, length=0.05, col=plotColour[5],lwd=1)
      }
      legendLabels = c(legendLabels, "Tremblay B")
      legendCol     = c(legendCol, plotColour[5])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Tremblay Group C
    if ("Tremblay_C" %in% selector) {
      points(Tremblay_2001_C$`Time (h)`/24, Tremblay_2001_C$`Mean (ng/mL)`/1000, col=plotColour[6], pch=0, lwd=2)
      if (errorBars){
        arrows(x0=Tremblay_2001_C$`Time (h)`/24, y0=Tremblay_2001_C$lower/1000, x1=Tremblay_2001_C$`Time (h)`/24, y1=Tremblay_2001_C$upper/1000,
               code=3, angle=90, length=0.05, col=plotColour[6],lwd=1)
      }
      legendLabels = c(legendLabels, "Tremblay C")
      legendCol     = c(legendCol, plotColour[6])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot He Hungarian Pill
    if ("He_HU" %in% selector) {
      points(He_1990_HU$`TSFD (h)`/24, He_1990_HU$Mean/1000, col="darkseagreen", pch=0, lwd=2)
      if (errorBars){
        arrows(x0=He_1990_HU$`TSFD (h)`/24, y0=He_1990_HU$lower/1000, x1=He_1990_HU$`TSFD (h)`/24, y1=He_1990_HU$upper/1000,
               code=3, angle=90, length=0.05, col="darkseagreen", lwd=1)
      }
      legendLabels = c(legendLabels, "He Hungarian Pill")
      legendCol     = c(legendCol, "darkseagreen")
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot He Chinese Pill
    if ("He_CN" %in% selector) {
      points(He_1990_CN$`TSFD (h)`/24, He_1990_CN$Mean/1000, col="darkslategray4", pch=0, lwd=2)
      if (errorBars){
        arrows(x0=He_1990_CN$`TSFD (h)`/24, y0=He_1990_CN$lower/1000, x1=He_1990_CN$`TSFD (h)`/24, y1=He_1990_CN$upper/1000,
               code=3, angle=90, length=0.05, col="darkslategray4",lwd=1)
      }
      legendLabels = c(legendLabels, "He Chinese Pill")
      legendCol     = c(legendCol, "darkslategray4")
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Kook data set
    # if ("Kook" %in% selector) {
    #   points(Kook_2002$`Digitized TIme (h)`/24, Kook_2002$`Conc (ng/mL)`/1000, col=plotColour[9], pch=0, lwd=2)
    #   legendLabels = c(legendLabels, "Kook")
    #   legendCol     = c(legendCol, plotColour[9])
    #   lineType     = c(lineType, 1)
    #   pointType     = c(pointType, NA)
    # }
    
    # Plot Kook data set
    if ("Kook" %in% selector) {
      points(Kook_error$Time_h/24, Kook_error$Mean_ng_mL/1000, col="darkgoldenrod2", pch=0, lwd=2)
      if (errorBars) {
        arrows(x0=Kook_error$Time_h/24, y0=Kook_error$lower/1000, x1=Kook_error$Time_h/24, y1=Kook_error$upper/1000,
               code=3, angle=90, length=0.05, col="darkgoldenrod2", lwd=1)
      }
      legendLabels = c(legendLabels, "Kook")
      legendCol     = c(legendCol, "darkgoldenrod2")
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Landgren data set
    # if ("Landgren" %in% selector) {
    #   points(Landgren_1989$`Time (h)`/24, Landgren_1989$`Conc (ng/mL)`/1000, col=plotColour[10], pch=0, lwd=2)
    #   legendLabels = c(legendLabels, "Landgren")
    #   legendCol     = c(legendCol, plotColour[10])
    #   lineType     = c(lineType, 1)
    #   pointType     = c(pointType, NA)
    # }
    
    if ("Landgren" %in% selector) {
      points(Landgren_CI$Time_h/24, Landgren_CI$Mean_ng_mL/1000, col="darkorange", pch=0, lwd=2)
      if (errorBars) {
        arrows(x0=Landgren_CI$Time_h/24, y0=Landgren_CI$Lower_ng_mL/1000, x1=Landgren_CI$Time_h/24, y1=Landgren_CI$Upper_ng_mL/1000,
               code=3, angle=90, length=0.05, col="darkorange", lwd=1)
      }
      legendLabels = c(legendLabels, "Landgren")
      legendCol     = c(legendCol, "darkorange")
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Johansson group A
    if ("Johansson_A" %in% selector) {
      points(Johansson_2002_A$`Dig Time (h)`/24, Johansson_2002_A$`Conc (ng/mL)`/1000, col=plotColour[11], pch=0, lwd=2)
      legendLabels = c(legendLabels, "Johansson A")
      legendCol     = c(legendCol, plotColour[11])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
      
    }
    
    # Plot Johansson group B
    if ("Johansson_B" %in% selector) {
      points(Johansson_2002_B$`Dig Time (h)`/24, Johansson_2002_B$`Conc (ng/mL)`/1000, col=plotColour[12], pch=0, lwd=2)
      legendLabels = c(legendLabels, "Johansson B")
      legendCol     = c(legendCol, plotColour[12])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Johansson group C
    if ("Johansson_C" %in% selector) {
      points(Johansson_2002_C$`Dig Time (h)`/24, Johansson_2002_C$`Conc (ng/mL)`/1000, col=plotColour[13], pch=0, lwd=2)
      legendLabels = c(legendLabels, "Johansson C")
      legendCol     = c(legendCol, plotColour[13])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot FDA Mirena data
    if ("FDA_Mirena" %in% selector) {
      points(Mirena_FDA$Time_days, Mirena_FDA$Plas_mean_ng_L*1e-6, col=plotColour[14], pch=0, lwd=2)
      if (errorBars){
        arrows(x0=Mirena_FDA$Time_days, y0=Mirena_FDA$lower*1e-6, x1=Mirena_FDA$Time_days, y1=Mirena_FDA$upper*1e-6,
               code=3, angle=90, length=0.05, col=plotColour[14], lwd=1)
      }
      legendLabels = c(legendLabels, "FDA Mirena")
      legendCol     = c(legendCol, plotColour[14])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot FDA Kyleena data
    if ("FDA_Kyleena" %in% selector) {
      points(Kyleena_FDA$Time_days, Kyleena_FDA$Plas_mean_ng_L*1e-6, col=plotColour[15], pch=0, lwd=2)
      if (errorBars){
        arrows(x0=Kyleena_FDA$Time_days, y0=Kyleena_FDA$lower*1e-6, x1=Kyleena_FDA$Time_days, y1=Kyleena_FDA$upper*1e-6,
               code=3, angle=90, length=0.05, col=plotColour[15], lwd=1)
      }
      legendLabels = c(legendLabels, "FDA Kyleena")
      legendCol     = c(legendCol, plotColour[15])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # # Plot FDA Skyla dataset 1
    # if ("FDA_Skyla_1" %in% selector) {
    #   points(Skyla_FDA_1$Time_days, Skyla_FDA_1$Plas_mean_ng_L*1e-6, col=plotColour[16], pch=0, lwd=2)
    #   legendLabels = c(legendLabels, "FDA Skyla 1")
    #   legendCol     = c(legendCol, plotColour[16])
    #   lineType     = c(lineType, 1)
    #   pointType     = c(pointType, NA)
    # }
    
    # Plot FDA Skyla dataset 1
    if ("FDA_Skyla_1" %in% selector) {
      points(Skyla_FDA_1$Time_days, Skyla_FDA_1$Plas_mean_ng_L*1e-6, col=plotColour[16], pch=0, lwd=2)
      if (errorBars){
        arrows(x0=Skyla_FDA_1$Time_days, y0=Skyla_FDA_1$lower*1e-6, x1=Skyla_FDA_1$Time_days, y1=Skyla_FDA_1$upper*1e-6,
               code=3, angle=90, length=0.05, col=plotColour[16], lwd=1)
      }
      legendLabels = c(legendLabels, "FDA Skyla 1")
      legendCol     = c(legendCol, plotColour[16])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot FDA Skyla dataset 2
    if ("FDA_Skyla_2" %in% selector) {
      points(Skyla_FDA_2$Time_days, Skyla_FDA_2$Plas_mean_ng_L*1e-6, col=plotColour[17], pch=0, lwd=2)
      if (errorBars){
        arrows(x0=Skyla_FDA_2$Time_days, y0=Skyla_FDA_2$lower*1e-6, x1=Skyla_FDA_2$Time_days, y1=Skyla_FDA_2$upper*1e-6,
               code=3, angle=90, length=0.05, col=plotColour[17], lwd=1)
      }
      legendLabels = c(legendLabels, "FDA Skyla 2")
      legendCol     = c(legendCol, plotColour[17])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Nilsson IUD data
    if ("Nilsson_IUD" %in% selector) {
      points(Nilsson_1982_IUD$Time_days, Nilsson_1982_IUD$Plas_mean_ng_L*1e-6, col=plotColour[18], pch=0, lwd=2)
      if (errorBars) {
        arrows(x0=Nilsson_1982_IUD$Time_days, y0=Nilsson_1982_IUD$Plas_lower*1e-6, x1=Nilsson_1982_IUD$Time_days, y1=Nilsson_1982_IUD$Plas_upper*1e-6,
               code=3, angle=90, length=0.05, col=plotColour[18], lwd=1)
      }
      legendLabels = c(legendLabels, "Nilsson IUD")
      legendCol     = c(legendCol, plotColour[18])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Nilsson oral data
    if ("Nilsson_Oral" %in% selector) {
      points(Nilsson_1982_oral$Time_days, Nilsson_1982_oral$Plas_mean_ng_L*1e-6, col=plotColour[19], pch=0, lwd=2)
      if (errorBars) {
        arrows(x0=Nilsson_1982_oral$Time_days, y0=Nilsson_1982_oral$Plas_lower*1e-6, x1=Nilsson_1982_oral$Time_days, y1=Nilsson_1982_oral$Plas_upper*1e-6,
               code=3, angle=90, length=0.05, col=plotColour[19], lwd=1)
      }
      legendLabels = c(legendLabels, "Nilsson Oral")
      legendCol     = c(legendCol, plotColour[19])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Devoto oral data
    if ("Devoto_Oral" %in% selector) {
      points(Devoto_oral$Time/24, Devoto_oral$Mean, col=plotColour[20], pch=0, lwd=2)
      if (errorBars) {
        arrows(x0=Devoto_oral$Time/24, y0=Devoto_oral$Lower, x1=Devoto_oral$Time/24, y1=Devoto_oral$Upper,
               code=3, angle=90, length=0.05, col=plotColour[20], lwd=1)
      }
      legendLabels = c(legendLabels, "Devoto Oral")
      legendCol     = c(legendCol, plotColour[20])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    # Plot Devoto vaginal data
    if ("Devoto_Vaginal" %in% selector) {
      points(Devoto_vaginal$Time/24, Devoto_vaginal$Mean, col=plotColour[21], pch=0, lwd=2)
      if (errorBars) {
        arrows(x0=Devoto_vaginal$Time/24, y0=Devoto_vaginal$Lower, x1=Devoto_vaginal$Time/24, y1=Devoto_vaginal$Upper,
               code=3, angle=90, length=0.05, col=plotColour[21], lwd=1)
      }
      legendLabels = c(legendLabels, "Devoto Vaginal")
      legendCol     = c(legendCol, plotColour[21])
      lineType     = c(lineType, 1)
      pointType     = c(pointType, NA)
    }
    
    mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
    mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
    
    
    par (fig= c(0, 1, 0.8, 1), new=TRUE )
    par(mar = c(0, 0, 0, 0))
    plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
    
    legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
    
    options(scipen = 0)
}

################### Cervical Fluid #############################

panelPlots_CF = function(simResults, yData, xLabel, yLabel, CMT, logScale,
                         num_ind, xmin, xmax, ymin, ymax)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}

################### Cervical Epithelium ########################

panelPlots_C_Epi = function(simResults, yData, xLabel, yLabel, CMT, logScale,
                            xmin, xmax, ymin, ymax, num_ind)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}

################### Cervical Myometrium ########################

panelPlots_C_Myo = function(simResults, yData, xLabel, yLabel, CMT, logScale,
                            xmin, xmax, ymin, ymax, num_ind)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}


################### Uterine Fluid ##############################

panelPlots_UF = function(simResults, yData, xLabel, yLabel, CMT, logScale, xmin, xmax, ymin, ymax, num_ind)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData)
  
  yRange=range(max(ymin,1e-12), ymax)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(0, xmax)
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}


################### Uterine Endometrium ########################

panelPlots_U_Endo = function(simResults, yData, xLabel, yLabel, CMT, logScale, errorBars, Nilsson_IUD, Nilsson_Oral,
                             Devoto_Oral, Devoto_Vaginal,
                             xmin, xmax, ymin, ymax, num_ind)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData, Nilsson_1982_IUD$Endo_upper*1e-3, Nilsson_1982_oral$Endo_upper*1e-3)#,
               #Devoto_endo_oral$Oral_upper, Devoto_endo_vaginal$Vag_upper)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  
  ## Note: Nilsson data is ng/g of wet tissue. Currently assuming wet tissue density of 1 g/mL
  # Plot Nilsson IUD data
  if (Nilsson_IUD==1) {
    points(Nilsson_1982_IUD$Time_days, Nilsson_1982_IUD$Endo_mean_ng_g*1e-3, col=plotColour[1], pch=0, lwd=2)
    if (errorBars) {
      arrows(x0=Nilsson_1982_IUD$Time_days, y0=Nilsson_1982_IUD$Endo_lower*1e-3, x1=Nilsson_1982_IUD$Time_days, y1=Nilsson_1982_IUD$Endo_upper*1e-3,
             code=3, angle=90, length=0.05, col=plotColour[1], lwd=1)
    }
    legendLabels = c(legendLabels, "Nilsson IUD")
    legendCol     = c(legendCol, plotColour[1])
    lineType     = c(lineType, 1)
    pointType     = c(pointType, NA)
  }
  
  # Plot Nilsson Oral data
  if (Nilsson_Oral==1) {
    points(Nilsson_1982_oral$Time_days, Nilsson_1982_oral$Endo_mean_ng_g*1e-3, col=plotColour[2], pch=0, lwd=2)
    # if (errorBars) {
    #   arrows(x0=Nilsson_1982_oral$Time_days, y0=Nilsson_1982_oral$Endo_lower*1e-3, x1=Nilsson_1982_oral$Time_days, y1=Nilsson_1982_oral$Endo_upper*1e-3,
    #          code=3, angle=90, length=0.05, col=plotColour[2], lwd=1)
    # }
    legendLabels = c(legendLabels, "Nilsson Oral")
    legendCol     = c(legendCol, plotColour[2])
    lineType     = c(lineType, 1)
    pointType     = c(pointType, NA)
  }
  
  
  # Plot Devoto Oral data
  if(Devoto_Oral==1) {
    points(Devoto_endo_oral$Time_h/24, Devoto_endo_oral$Oral_mean/1e6, col=plotColour[3], pch=0, lwd=2)
    if (errorBars) {
      arrows(x0=Devoto_endo_oral$Time_h/24, y0=Devoto_endo_oral$Oral_lower/1e6, x1=Devoto_endo_oral$Time_h/24, y1=Devoto_endo_oral$Oral_upper/1e6,
             code=3, angle=90, length=0.05, col=plotColour[3], lwd=1)
    }
    legendLabels = c(legendLabels, "Devoto Oral Dose")
    legendCol     = c(legendCol, plotColour[3])
    lineType     = c(lineType, 1)
    pointType     = c(pointType, NA)
  }
  
  # Plot Devoto Vaginal data
  if(Devoto_Vaginal==1) {
    points(Devoto_endo_vaginal$Time_h/24, Devoto_endo_vaginal$Vag_mean/1e6, col=plotColour[4], pch=0, lwd=2)
    if (errorBars) {
      arrows(x0=Devoto_endo_vaginal$Time_h/24, y0=Devoto_endo_vaginal$Vag_lower/1e6, x1=Devoto_endo_vaginal$Time_h/24, y1=Devoto_endo_vaginal$Vag_upper/1e6,
             code=3, angle=90, length=0.05, col=plotColour[4], lwd=1)
    }
    legendLabels = c(legendLabels, "Devoto Vaginal Dose")
    legendCol     = c(legendCol, plotColour[4])
    lineType     = c(lineType, 1)
    pointType     = c(pointType, NA)
  }
  
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}


################### Uterine Myometrium #########################

panelPlots_U_Myo = function(simResults, yData, xLabel, yLabel, CMT, logScale, errorBars, Nilsson_IUD, Nilsson_Oral,
                            xmin, xmax, ymin, ymax, num_ind)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData, Nilsson_1982_IUD$Myo_mean_ng_g*1e-3, Nilsson_1982_oral$Myo_mean_ng_g*1e-3)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  ## Note: Nilsson data is ng/g of wet tissue. Currently assuming wet tissue density of 1 g/mL
  # Plot Nilsson IUD data
  if (Nilsson_IUD==1) {
    points(Nilsson_1982_IUD$Time_days, Nilsson_1982_IUD$Myo_mean_ng_g*1e-3, col=plotColour[1], pch=0, lwd=2)
    if (errorBars) {
      arrows(x0=Nilsson_1982_IUD$Time_days, y0=Nilsson_1982_IUD$Myo_lower*1e-3, x1=Nilsson_1982_IUD$Time_days, y1=Nilsson_1982_IUD$Myo_upper*1e-3,
             code=3, angle=90, length=0.05, col=plotColour[1], lwd=1)
    }
    legendLabels = c(legendLabels, "Nilsson IUD")
    legendCol     = c(legendCol, plotColour[1])
    lineType     = c(lineType, 1)
    pointType     = c(pointType, NA)
  }
  
  # Plot Nilsson Oral data
  if (Nilsson_Oral==1) {
    points(Nilsson_1982_oral$Time_days, Nilsson_1982_oral$Myo_mean_ng_g*1e-3, col=plotColour[2], pch=0, lwd=2)
    if (errorBars) {
      arrows(x0=Nilsson_1982_oral$Time_days, y0=Nilsson_1982_oral$Myo_lower*1e-3, x1=Nilsson_1982_oral$Time_days, y1=Nilsson_1982_oral$Myo_upper*1e-3,
             code=3, angle=90, length=0.05, col=plotColour[2], lwd=1)
    }
    legendLabels = c(legendLabels, "Nilsson Oral")
    legendCol     = c(legendCol, plotColour[2])
    lineType     = c(lineType, 1)
    pointType     = c(pointType, NA)
  }
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}


################### Stroma Blood ###############################

panelPlots_stroma_blood = function(simResults, yData, xLabel, yLabel, CMT, logScale,
                                   xmin, xmax, ymin, ymax, num_ind)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}


################### Depot Compartment ###############################

panelPlots_depot = function(simResults, yData, xLabel, yLabel, CMT, logScale,
                            xmin, xmax, ymin, ymax, num_ind)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  if(num_ind > 1) {
    if(logScale == FALSE) {
      plot(simResults$time[simResults$ID==1], yData[1:(length(yData)/max(simResults$ID))], type="l",  lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
      for (i in 2:max(simResults$ID)){
        a<-(i-1)*(length(yData)/max(simResults$ID))+1
        b<-i*(length(yData)/max(simResults$ID))
        lines(simResults$time[simResults$ID==i], yData[a:b],lwd=2)
      }
    }
    if(logScale == TRUE) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
    }
  } else
  {
    if(logScale == 0) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
    }
    if(logScale == 1) {
      plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE, log="y")
      
    }
  }
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}

################### Mass Balance Check #############################

panelPlots_MB = function(simResults, yData, xLabel, yLabel, CMT, logScale,
                         xmin, xmax, ymin, ymax)
{
  #options(scipen = 999)
  
  par(oma = c(5, 7, 0, 0))
  par(mar = c(1, 3, 1.5, 3))
  par(fig= c(0, 1, 0, 0.8) )
  
  plotColour = c("darkorange", "deepskyblue", "firebrick", "purple", "darkgreen", "darkblue", "brown","chartreuse1","aquamarine1","aquamarine4","azure4",
                 "burlywood3", "burlywood4", "coral", "coral3", "darkkhaki", "darkolivegreen", "darkseagreen", "deeppink3", "darkorchid4", "darkslategray4", 
                 "darkslateblue", "red")
  
  allYdata = c(yData)
  
  #xRange_hrs = range(0, max(simResults$time))
  xRange_hrs = range(xmin, xmax)
  yRange=range(max(ymin,1e-12), ymax)
  
  if(logScale==0) {
    #yRange = range(0, max(allYdata), na.rm = TRUE)
    plot(simResults$time, yData, type="l", lwd=2, xlim=xRange_hrs, ylim=yRange, ann=FALSE, axes=FALSE)
  }
  
  axis(side=1, cex.axis=1.4, mgp=c(1, 2, 0))
  axis(side=2, las=1, cex.axis=1.4)
  box()
  
  
  legendLabels = c("Simulated")
  legendCol     = c("black")
  lineType     = c(1)
  pointType     = c(NA)
  
  mtext(side=1, xLabel, line=4, adj=0.4, cex=1.5, outer=FALSE)
  mtext(side=2, yLabel, line=7, cex=1.5, outer=FALSE)
  
  par (fig= c(0, 1, 0.8, 1), new=TRUE )
  par(mar = c(0, 0, 0, 0))
  plot( c(0,1), c(0,1), type ="l", pch=19, col="transparent", lwd=2, ann=FALSE, axes=FALSE)
  
  legend("center", legendLabels, col= legendCol, lty=lineType, pch=pointType, lwd=2, cex=1, ncol=4)
  
  options(scipen = 0)
}



