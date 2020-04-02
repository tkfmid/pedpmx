
library(shiny)
library(shinydashboard)
# install.packages("shinyMatrix", lib = "~/R_packages")
# library(shinyMatrix, lib.loc = "../R_packages")
# library(shinyMatrix)
library(tidyverse)
library(plotly)
library(ggpubr)
library(ggsci)
library(ggthemes)
library(foreign)
library(mrgsolve)
library(Hmisc)
library(DT)
theme_set(theme_pubr(base_size = 10))

layout_ggplotly <- function(gg, x = -0.02, y = -0.08){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
  gg
}

modelcode <- "
Model file:  pk2cmt.cpp 
$PROB
# Model: `pk2cmt`
  - Two-compartment PK model
  - Dual first-order absorption
  - Optional nonlinear clearance from `CENT`
  - Source: `mrgsolve` internal library
  - Date: `r Sys.Date()`
  - Version: `r packageVersion(mrgsolve)`
  
  $PARAM @annotated
  TVCL   :  1  : Clearance (volume/time)
  TVVC   : 20  : Central volume (volume)
  TVQ    :  2  : Inter-compartmental clearance (volume/time)
  TVVP   : 10  : Peripheral volume of distribution (volume)
  TVKA1  :  1  : Absorption rate constant 1 (1/time)
  KA2  :  1  : Absorption rate constant 2 (1/time)
  VMAX :  0  : Maximum velocity (mass/time)
  KM   :  2  : Michaelis Constant (mass/volume)
  WT   :  70  : Body weight
  CL_WT   :  0.75  : Power exponent
  VC_WT   :  0.75  : Power exponent
  WTref   :  70  : Reference WT
  
  $CMT  @annotated
  EV1    : First extravascular compartment (mass)
  CENT   : Central compartment (mass)
  PERIPH : Peripheral compartment (mass) 
  EV2    : Second extravascular compartment (mass)
  AUC    : Dummy AUC compartment
  
$GLOBAL 
#define CP (CENT/VC)
#define CT (PERIPH/VP)
#define CLNL (VMAX/(KM+CP))
  
$MAIN
double CL = TVCL * pow(WT / WTref, CL_WT) * exp(ECL);
double VC = TVVC * pow(WT / WTref, VC_WT) * exp(EVC);
double Q = TVQ * exp(EQ);
double VP = TVVP * exp(EVP);
double KA1 = TVKA1 * exp(EKA1);

$OMEGA @annotated @block
  ECL : 0.2: ETA on clearance
  EVC : 0 0.2 : ETA on volume
  EQ  : 0 0 0.2: ETA on volume
  EVP : 0 0 0 0.2: ETA on volume
  EKA1: 0 0 0 0 0.2: ETA on volume
  
$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA1*EV1 + KA2*EV2 - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH = Q*CP - Q*CT;
dxdt_AUC = CP;

$CAPTURE  @annotated
  CP : Plasma concentration (mass/time)
  AUC : AUC
"


# input = NULL
# input$cmt = "1cmt"
# input$route = "iv"
# input$dosetype = "WT-based"
# input$dose = 100
# input$adose = 100
# input$tinf = 0
# input$ii = 24
# input$agerange = c(0, 18)
# input$age = c(6, 12, 18)
# input$wt = choices = c(10, 20, 30)
# input$theta = NULL
# input$omegatype = "Block"
# input$omega = "0.2,0.2,0.2,0.2,0.2"
# input$omegab = "0.2, 0.6, 0.2, 0, 0, 0.2, 0, 0, 0.6, 0.2, 0, 0, 0, 0, 0.2"
# input$modelcode = modelcode
# input$cl = 1
# input$vc = 20
# input$q = 2
# input$vp = 10
# input$ka = 1
# input$log = FALSE


header <- dashboardHeader(title = "PEDEX Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("PK Dashboard", tabName = "pkdashboard", icon = icon("dashboard"))
    # menuItem("Widgets", icon = icon("th"), tabName = "widgets",
    #          badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pkdashboard",
            h2("PK Dashboard"),
            
            fluidRow(
              tabBox(
                title = "Input",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", width = 4,
                tabPanel("Dosing",
                         submitButton("Update", icon("refresh")),
                         radioButtons("route", label = "Route of administration", choices = c("iv", "sc/oral"), selected = "iv", inline = TRUE),
                         radioButtons("dosetype", label = "Dosing Type", choices = c("Fixed", "WT-based"), selected = "WT-based", inline = TRUE),
                         selectizeInput("adose", "Dose Amount for Adults", choices = 100, selected = 100, multiple = FALSE, options = list(create = TRUE)),
                         selectizeInput("dose", "Dose Amount for Pediatrics", choices = 100, selected = 100, multiple = TRUE, options = list(create = TRUE)),
                         sliderInput("agerange", label = "Age Range", min = 0, max = 18, value = c(2, 18)),
                         selectizeInput("age", "Age Cutpoint", choices = c(6, 12), selected = c(6, 12), multiple = TRUE, options = list(create = TRUE)),
                         selectizeInput("wt", "Weight Cutpoint", choices = c(20, 30, 40), selected = c(20, 30, 40), multiple = TRUE, options = list(create = TRUE)),
                         conditionalPanel(condition = "input.route == 'iv'",
                                          numericInput("tinf", label = "Infusion Time", value = 0)),
                         numericInput("ii", label = "Dosing Interval", value = 24)
                ),
                tabPanel("PK Parameter",
                         radioButtons("cmt", label = "Model", choices = c("1cmt", "2cmt"), selected = "1cmt", inline = TRUE),
                         numericInput("cl", "Theta (CL)", value = 1),
                         numericInput("vc", "Theta (VC)", value = 20),
                         conditionalPanel(condition = "input.cmt == '2cmt'",
                                          numericInput("q", "Theta (Q)", value = 2),
                                          numericInput("vp", "Theta (VP)", value = 10)),
                         conditionalPanel(condition = "input.route == 'sc/oral'",
                                          numericInput("ka", "Theta (KA)", value = 1)),
                         radioButtons("omegatype", label = "Omega Structure", choices = c("Diag", "Block", "Zero"), selected = "Block", inline = TRUE),
                         conditionalPanel(condition = "input.omegatype == 'Diag'",
                                          textInput("omega", "Omega (comma delimited)", value = "0.2, 0.2, 0.2, 0.2, 0.2")),
                         conditionalPanel(condition = "input.omegatype == 'Block'",
                                          textInput("omegab", "Omega (comma delimited)", value = "0.2, 0.6, 0.2, 0, 0, 0.2, 0, 0, 0.6, 0.2, 0, 0, 0, 0, 0.2"))
                ),
                tabPanel("PK Model Code",
                         textAreaInput("modelcode", "Mrgsolve model text", value = modelcode, width = "600px"))
              ),
              tabBox(
                width = 8,
                # tabPanel("Resampling", plotlyOutput("plot1", height = 700, width = "95%")),
                # tabPanel("Simulated Dataset", DTOutput("tbl")),
                tabPanel("PK Profile by Weight", 
                         checkboxInput("log", label = "Log?", value = FALSE),
                         plotlyOutput("pkwt", height = 700, width = "100%")),
                tabPanel("PK Profile by Age",
                         checkboxInput("log", label = "Log?", value = FALSE),
                         plotlyOutput("pkage", height = 700, width = "100%")),
                tabPanel("Exposure vs. Weight", plotlyOutput("pkbxpwt", height = 700, width = "100%")),
                tabPanel("Exposure vs. Age", plotlyOutput("pkbxpage", height = 700, width = "100%"))
              )
              )
            ),
    tabItem(
      tabName = "widgets",
      h2("Widgets tab content")
            )
  )
)


# Put them together into a dashboardPage
ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  
  # (https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015)
  
  # Read-in demographics
  nhanes_demo <- read.xport("./data/DEMO_I.XPT") %>% 
    tbl_df() %>% 
    dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDEXAGM) %>% 
    rename(SEX = RIAGENDR, AGE = RIDAGEYR, AGEM = RIDAGEMN, AGEM2 = RIDEXAGM)
  
  # Read-in body measurement
  nhanes_bmx <- read.xport("./data/BMX_I.XPT") %>% 
    tbl_df() %>% 
    dplyr::select(SEQN, BMXWT, BMXHT, BMXBMI) %>% 
    rename(WT = BMXWT, HT = BMXHT, BMI = BMXBMI)
  
  # Merge datasets
  nhanes <- left_join(nhanes_demo, nhanes_bmx) %>% 
    drop_na(WT, AGE)
  
  # output$plot1 <- renderPlotly({
  #   ggplot(nhanes)+
  #     geom_point(data = nhanes %>% filter(AGE >= input$agerange[1] & AGE <= input$agerange[2]), aes(AGE, WT, colour = "Selected age range"), alpha = 0.3)+
  #     geom_point(data = nhanes %>% filter(AGE < input$agerange[1] | AGE > input$agerange[2]), aes(AGE, WT), alpha = 0.1)+
  #     labs(x = "Age (year)", y = "Weight (kg)")+
  #     scale_color_npg()+
  #     theme(legend.title = element_blank())
  #   
  #   })
  
  re_simdf <- reactive({
    mod <- mcode("mod", input$modelcode)
    # mod <- mread("mod", file = "./model/pk2cmt2.cpp")
    
    Nsubj <- 500
    
    nhanes_filtered <- nhanes %>% 
      filter(AGE >= input$agerange[1], AGE <= input$agerange[2])
    nhanes_adults <- nhanes %>% 
      filter(AGE >= 18)
    
    data <- expand.ev(amt = as.numeric(input$dose)[1],
                      tinf = input$tinf,
                      ss = 0:1,#input$ss,
                      ii = input$ii,
                      cmt = ifelse(input$route == "iv", 2, 1),
                      addl = 1,
                      SEQN = rep(sample(nhanes_filtered$SEQN, Nsubj))) %>% 
      mutate(ss2 = rep(c(0, 1), times = Nsubj),
             id2 = rep(1:Nsubj, each = 2)) %>% 
      left_join(nhanes_filtered) %>% 
      mutate(WT2 = as.numeric(cut2(WT, cuts = as.numeric(input$wt)))) %>% 
      group_by(ID) %>% 
      mutate(amt = ifelse(length(input$dose) >= 2, as.numeric(input$dose)[WT2], amt))
    
    data_adults <- expand.ev(amt = as.numeric(input$adose),
                             tinf = input$tinf,
                             ss = 0:1,#input$ss,
                             ii = input$ii,
                             cmt = ifelse(input$route == "iv", 2, 1),
                             addl = 1,
                             SEQN = rep(sample(nhanes_adults$SEQN, Nsubj))) %>% 
      mutate(ss2 = rep(c(0, 1), times = Nsubj),
             id2 = rep(1:Nsubj, each = 2)) %>% 
      left_join(nhanes_adults)
    
    mod <- mod %>% param(TVCL = input$cl, TVVC = input$vc)
    
    if(input$cmt == "1cmt") mod <- mod %>% param(TVQ = 0)
    
    if(input$cmt == "2cmt") mod <- mod %>% param(TVQ = input$q, TVVP = input$vp)

    if(input$route == "sc/oral") mod <- mod %>% param(TVKA1 = input$ka)
    
    omega_vec <- as.numeric(unlist(strsplit(input$omega,",")))
    omegab_vec <- as.numeric(unlist(strsplit(input$omegab,",")))
    if(input$omegatype == "Zero") mod <- mod %>% zero_re()
    if(input$omegatype == "Diag") mod <- mod %>% omat(dmat(omega_vec))
    if(input$omegatype == "Block") mod <- mod %>% omat(cmat(omegab_vec))

      
    
    
    if(input$dosetype == "WT-based") {
      data <- data %>% mutate(amt = amt * WT)
      data_adults <- data_adults %>% mutate(amt = amt * WT)
    }
    
    
    simdf_peds <- mod %>%
      data_set(data) %>%
      Req(CP, AUC) %>%
      carry_out(WT, AGE, SEX, amt, evid, cmt, ss, ii, ss2, id2) %>% 
      mrgsim_df(end = input$ii * 2, tgrid = tgrid(0, input$ii * 2, 0.1), obsonly = F, tad = TRUE) %>% 
      tbl_df() %>% 
      mutate(ss2 = recode(ss2,
                          `0` = "First Dose",
                          `1` = "Steady-State"),
             pop = "peds")
    simdf_adults <- mod %>%
      data_set(data_adults) %>%
      Req(CP, AUC) %>%
      carry_out(WT, AGE, SEX, amt, evid, cmt, ss, ii, ss2, id2) %>% 
      mrgsim_df(end = input$ii * 2, tgrid = tgrid(0, input$ii * 2, 0.1), obsonly = F, tad = TRUE) %>% 
      tbl_df() %>% 
      mutate(ss2 = recode(ss2,
                          `0` = "First Dose",
                          `1` = "Steady-State"),
             pop = "adults")
      
    
    re_simdf <- bind_rows(simdf_peds, simdf_adults) %>% 
      mutate(WTC = ifelse(pop == "peds", as.character(cut2(WT[pop == "peds"], cuts = as.numeric(input$wt))), "Adults"),
             WTC = factor(WTC),
             WTC = factor(WTC, levels = c(levels(WTC)[levels(WTC) != "Adults"], "Adults")),
             AGEC = ifelse(pop == "peds", as.character(cut2(AGE[pop == "peds"], cuts = as.numeric(input$age))), "Adults"),
             AGEC = factor(AGEC),
             AGEC = factor(AGEC, levels = c(levels(AGEC)[levels(AGEC) != "Adults"], "Adults")))
    
    re_simdf
    
    })
  
  # output$tbl <- renderDT(
  #   re_simdf() %>% filter(pop == "peds"), options = list(lengthChange = FALSE)
  # )
  
  output$pkwt <- renderPlotly({
    
    conc_summary <- re_simdf() %>%
      # filter(pop == "peds") %>% 
      group_by(pop, ss2, time, WTC) %>% 
      summarise(conc_m = quantile(CP, probs = 0.5),
                conc_l = quantile(CP, probs = 0.05),
                conc_u = quantile(CP, probs = 0.95)) %>% 
      filter(!(ss2 == "Steady-State" & time == 0)) %>% 
      ungroup() %>% 
      arrange(WTC, ss2, time)
    
    conc_summary2 <- conc_summary %>% 
      filter(pop == "peds") %>%
      mutate(WTC2 = WTC) %>% 
      bind_rows(conc_summary %>% 
                  filter(pop == "adults") %>% 
                  slice(rep(1:n(), times = length(input$wt) + 1))) %>% 
      mutate(WTC2 = rep(WTC2[1:(n() / 2)], times = 2),
             WTC = factor(WTC, levels = c("Adults", levels(WTC)[levels(WTC) != "Adults"])))

    p <- ggplot(conc_summary2)+
      facet_grid(ss2~WTC2)+
      geom_ribbon(aes(x = time, ymax = conc_u, ymin = conc_l, colour = WTC, fill = WTC), alpha = 0.3)+
      geom_line(aes(time, conc_m, colour = WTC, fill = WTC), size = 1.1)+
      # geom_point(aes(time, conc_m, colour = "Median"), alpha = 0.5, size = 1.2)+
      labs(x = "Time", y = "Concentration", colour = "Body Weight", fill = "Body Weight")+
      scale_color_manual(values = c("darkgrey", get_palette("npg", 4)))+
      scale_fill_manual(values = c("darkgrey", get_palette("npg", 4)))
    if(input$log) p <- p + scale_y_log10()
    p <- ggplotly(p) %>% layout(margin = list(l = 100, b = 100))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  
  
  output$pkage <- renderPlotly({
    
    conc_summary <- re_simdf() %>%
      # filter(pop == "peds") %>% 
      group_by(pop, ss2, time, AGEC) %>% 
      summarise(conc_m = quantile(CP, probs = 0.5),
                conc_l = quantile(CP, probs = 0.05),
                conc_u = quantile(CP, probs = 0.95)) %>% 
      filter(!(ss2 == "Steady-State" & time == 0)) %>% 
      ungroup() %>% 
      arrange(AGEC, ss2, time)
    
    conc_summary2 <- conc_summary %>% 
      filter(pop == "peds") %>%
      mutate(AGEC2 = AGEC) %>% 
      bind_rows(conc_summary %>% 
                  filter(pop == "adults") %>% 
                  slice(rep(1:n(), times = length(input$age) + 1))) %>% 
      mutate(AGEC2 = rep(AGEC2[1:(n() / 2)], times = 2),
             AGEC = factor(AGEC, levels = c("Adults", levels(AGEC)[levels(AGEC) != "Adults"])))
    
    p <- ggplot(conc_summary2)+
      facet_grid(ss2~AGEC2)+
      geom_ribbon(aes(x = time, ymax = conc_u, ymin = conc_l, colour = AGEC, fill = AGEC), alpha = 0.3)+
      geom_line(aes(time, conc_m, colour = AGEC, fill = AGEC), size = 1.1)+
      # geom_point(aes(time, conc_m, colour = "Median"), alpha = 0.5, size = 1.2)+
      labs(x = "Time", y = "Concentration", colour = "Age", fill = "Age")+
      scale_color_manual(values = c("darkgrey", get_palette("npg", 4)))+
      scale_fill_manual(values = c("darkgrey", get_palette("npg", 4)))
    if(input$log) p <- p + scale_y_log10()
    p <- ggplotly(p) %>% layout(margin = list(l = 100, b = 100))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  
  re_sumpk <- reactive({
    re_sumpk <- re_simdf() %>% 
      filter(pop == "peds") %>% 
      filter(!((time ==  0 & evid == 0) | (time ==  0 & cmt == 1)) & time <= input$ii ) %>% 
      group_by(pop, ID, WT, AGE, WTC, AGEC, ss2) %>% 
      summarise(Cmax = max(CP),
                Cmin = min(CP),
                Cavg = (AUC[n()] - AUC[1]) / (time[n()] - time[1])) %>% 
      gather(exposure, value, Cmax, Cmin, Cavg) %>% 
      mutate(exposure = paste0(exposure, ": ", ss2))
    re_sumpk
  })

  
  re_sumpk_adults <- reactive({
    re_sumpk_adults <- re_simdf() %>% 
      filter(pop == "adults") %>% 
      filter(!((time ==  0 & evid == 0) | (time ==  0 & cmt == 1)) & time <= input$ii ) %>% 
      group_by(pop, ID, WT, AGE, WTC, AGEC, ss2) %>% 
      summarise(Cmax = max(CP),
                Cmin = min(CP),
                Cavg = (AUC[n()] - AUC[1]) / (time[n()] - time[1])) %>% 
      gather(exposure, value, Cmax, Cmin, Cavg) %>% 
      mutate(exposure = paste0(exposure, ": ", ss2))
      # group_by(exposure) %>% 
      # summarise(P05 = quantile(value, probs = 0.05),
      #           P50 = quantile(value, probs = 0.5),
      #           P95 = quantile(value, probs = 0.95))
    re_sumpk_adults
  })
  
  output$pkbxpwt <- renderPlotly({
    
    pkstats_adults <- re_sumpk_adults() %>% 
      group_by(exposure) %>%
      summarise(P05 = quantile(value, probs = 0.05),
                P50 = quantile(value, probs = 0.5),
                P95 = quantile(value, probs = 0.95))
      
    
    p <- re_sumpk() %>% 
      bind_rows(re_sumpk_adults()) %>% 
      ungroup() %>% 
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y")+
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed")+
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed")+
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed")+
      geom_violin(aes(WTC, value, colour = WTC, fill = WTC), width=1, alpha = 0.3)+
      geom_boxplot(aes(WTC, value, colour = WTC, fill = WTC), width=0.1, alpha = 0.3)+
      scale_color_npg()+
      scale_fill_npg()+
      labs(x = "Body Weight (kg)", y = "Exposure", colour = "Body Weight", fill = "Body Weight")
      # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  
  output$pkbxpage <- renderPlotly({
    
    pkstats_adults <- re_sumpk_adults() %>% 
      group_by(exposure) %>%
      summarise(P05 = quantile(value, probs = 0.05),
                P50 = quantile(value, probs = 0.5),
                P95 = quantile(value, probs = 0.95))
    
    p <- re_sumpk() %>% 
      bind_rows(re_sumpk_adults()) %>% 
      ungroup() %>% 
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y")+
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed")+
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed")+
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed")+
      geom_violin(aes(AGEC, value, colour = AGEC, fill = AGEC), width=1, alpha = 0.3)+
      geom_boxplot(aes(AGEC, value, colour = AGEC, fill = AGEC), width=0.1, alpha = 0.3)+
      scale_color_npg()+
      scale_fill_npg()+
      labs(x = "Age (year)", y = "Exposure", colour = "Age", fill = "Age")
      # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  
  
}

shinyApp(ui, server)
