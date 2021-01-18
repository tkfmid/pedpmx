
# install.packages("pammtool")
library(shiny)
library(shinydashboard)
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
library(furrr)
theme_set(theme_pubr(base_size = 10))

layout_ggplotly <- function(gg, x = -0.02, y = -0.08) {
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- x
  gg[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- y
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
  AUC1    : Dummy AUC compartment
  
$GLOBAL 
#define CP (CENT/VC)
#define CT (PERIPH/VP)
#define CLNL (VMAX/(KM+CP))
#define AUC (AUC1)
  
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
dxdt_AUC1 = CP;

$CAPTURE  @annotated
  CP  : Plasma concentration (mass/time)
  AUC : AUC
  CL  : Clearance
  
$SET
ss_cmt = 'CENT'
"

# {
# input <- NULL
# input$nsubj <- 50
# input$ntrial <- 5
# input$cmt <- "1cmt"
# input$aroute <- "iv"
# input$adosetype <- "WT-based" # "Fixed"
# input$proute <- "iv"
# input$pdosetype <- "BSA-Tiered Fixed" # "Fixed"
# input$pdose <- c(100, 200, 300, 400)
# input$adose <- 10
# input$atinf <- 0
# input$aii <- 12
# input$ptinf <- 0
# input$pii <- 12
# input$agerange <- c(0, 18)
# input$wtrange <- c(0, 80)
# input$bsarange <- c(0.5, 1, 1.5)
# input$age <- c(6, 12, 18)
# input$wt <- choices <- c(10, 20, 30)
# input$theta <- NULL
# input$omegatype <- "Block"
# input$omega <- "0.2,0.2,0.2,0.2,0.2"
# input$omegab <- "0.2, 0.6, 0.2, 0, 0, 0.2, 0, 0, 0.6, 0.2, 0, 0, 0, 0, 0.2"
# input$modelcode <- modelcode
# input$cl <- 1
# input$vc <- 20
# input$q <- 2
# input$vp <- 10
# input$ka <- 1
# input$log <- FALSE
# input$cl_wt <- 0.75
# input$vc_wt <- 0.75
# }

header <- dashboardHeader(title = "PEDEX Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("PK Dashboard", tabName = "pkdashboard", icon = icon("dashboard")),
    menuItem("Source code",
      icon = icon("file-code-o"),
      href = "https://github.com/tkfmid/pedpmx"
    )
    # menuItem("Widgets", icon = icon("th"), tabName = "widgets",
    #          badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "pkdashboard",
      h2("PK Dashboard"),

      fluidRow(
        tabBox(
          title = "Input",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", width = 4,
          tabPanel(
            "Adult Dosing",
            submitButton("Update", icon("refresh")),
            # actionButton("run_stochas", "Run Stochastic Simulation"),
            radioButtons("aroute", label = "Route of administration", choices = c("iv", "sc/oral"), selected = "sc/oral", inline = TRUE),
            radioButtons("adosetype", label = "Dosing Type", choices = c("Fixed", "WT-based", "BSA-based"), selected = "Fixed", inline = TRUE),
            selectizeInput("adose", "Dose Amount for Adults",
              choices = 600, selected = 600, multiple = FALSE, options = list(create = TRUE)
            ),
            conditionalPanel(
              condition = "input.aroute == 'iv'",
              numericInput("atinf", label = "Infusion Time", value = 0)
            ),
            numericInput("aii", label = "Dosing Interval", value = 12)
          ),
          tabPanel(
            "Pediatric Dosing",
            submitButton("Update", icon("refresh")),
            # actionButton("run_stochas", "Run Stochastic Simulation"),
            radioButtons("proute", label = "Route of administration", choices = c("iv", "sc/oral"), selected = "sc/oral", inline = TRUE),
            radioButtons("pdosetype", label = "Dosing Type", choices = c("WT-Tiered Fixed", "BSA-Tiered Fixed", "WT-based", "BSA-based"), selected = "BSA-based", inline = TRUE),
            selectizeInput("pdose", "Dose Amount for Pediatrics [Option 1. Single dose level, Option 2. Different dose levels for each WT/BSA tier (the number of dose levels should match the number of tiers)]",
              choices = 300, selected = 300, multiple = TRUE, options = list(create = TRUE)
            ),

            conditionalPanel(
              condition = "input.proute == 'iv'",
              numericInput("ptinf", label = "Infusion Time", value = 0)
            ),
            numericInput("pii", label = "Dosing Interval", value = 12),
            sliderInput("agerange", label = "Age Range of Pediatric Population", min = 0, max = 18, value = c(2, 18)),
            # sliderInput("wtrange", label = "Weight Range of Pediatric Population", min = 0, max = 120, value = c(0, 80)),
            selectizeInput("age", "Age Cutpoint", choices = c(6, 12), selected = c(6, 12), multiple = TRUE, options = list(create = TRUE)),
            selectizeInput("wt", "Weight Cutpoint", choices = c(20, 30, 40), selected = c(20, 30, 40), multiple = TRUE, options = list(create = TRUE)),
            selectizeInput("bsa", "BSA Cutpoint", choices = c(0.8, 1.1, 1.5), selected = c(0.8, 1.1, 1.5), multiple = TRUE, options = list(create = TRUE))
          ),
          tabPanel(
            "Simulation Setting",
            submitButton("Update", icon("refresh")),
            radioButtons("stat", label = "Summarise Pediatric Statistics for", choices = c("Population Mean / Variability", "Trial Mean / Uncertainty"), selected = "Population Mean / Variability", inline = TRUE),
            numericInput("nsubj", label = "N of Pediatric Subjects", value = 50),
            numericInput("ntrial", label = "N of Pediatric Trials", value = 1),
            radioButtons("stat2", label = "Summarise Adult Statistics for", 
                         choices = c("Population Mean / Variability (from 1000 Subjects)"), 
                         selected = "Population Mean / Variability (from 1000 Subjects)", inline = TRUE),
          ),
          tabPanel(
            "PK Parameters",
            submitButton("Update", icon("refresh")),
            radioButtons("cmt", label = "Model", choices = c("1cmt", "2cmt"), selected = "2cmt", inline = TRUE),
            numericInput("cl", "Theta (CL)", value = 1),
            numericInput("vc", "Theta (VC)", value = 20),
            conditionalPanel(
              condition = "input.cmt == '2cmt'",
              numericInput("q", "Theta (Q)", value = 2),
              numericInput("vp", "Theta (VP)", value = 10)
            ),
            numericInput("cl_wt", "WT on CL (Power Exponent)", value = 0.75),
            numericInput("vc_wt", "WT on VC (Power Exponent)", value = 0.75),
            conditionalPanel(
              condition = "input.aroute == 'sc/oral'| input.proute == 'sc/oral'",
              numericInput("ka", "Theta (KA)", value = 1)
            ),
            radioButtons("omegatype", label = "Omega Structure", choices = c("Diag", "Block", "Zero"), selected = "Block", inline = TRUE),
            conditionalPanel(
              condition = "input.omegatype == 'Diag'",
              textInput("omega", "Omega (comma delimited)", value = "0.2, 0.2, 0.2, 0.2, 0.2")
            ),
            conditionalPanel(
              condition = "input.omegatype == 'Block'",
              textInput("omegab", "Omega (comma delimited)", value = "0.2, 0.6, 0.2, 0, 0, 0.2, 0, 0, 0.6, 0.2, 0, 0, 0, 0, 0.2")
            )
          ),
          tabPanel(
            "PK Model Code",
            submitButton("Update", icon("refresh")),
            textAreaInput("modelcode", "Mrgsolve model text", value = modelcode)
          )
        ),
        tabBox(
          width = 8,
          # tabPanel("Resampling", plotlyOutput("plot1", height = 700, width = "95%")),
          # tabPanel("Simulated Dataset", DTOutput("tbl")),
          tabPanel(
            "PK Profile by Weight",
            checkboxInput("log", label = "Log?", value = FALSE),
            plotlyOutput("pkwt", height = 700, width = "100%")
          ),
          tabPanel(
            "PK Profile by BSA",
            checkboxInput("log", label = "Log?", value = FALSE),
            plotlyOutput("pkbsa", height = 700, width = "100%")
          ),
          tabPanel(
            "PK Profile by Age",
            checkboxInput("log", label = "Log?", value = FALSE),
            plotlyOutput("pkage", height = 700, width = "100%")
          ),
          tabPanel("Exposure vs. Weight", plotlyOutput("pkbxpwt", height = 700, width = "100%")),
          tabPanel("Exposure vs. BSA", plotlyOutput("pkbxpbsa", height = 700, width = "100%")),
          tabPanel("Exposure vs. Age", plotlyOutput("pkbxpage", height = 700, width = "100%")),
          tabPanel("Exposure vs. Weight (Continuous)", plotlyOutput("pkscatbw", height = 700, width = "100%")),
          tabPanel("Exposure vs. BSA (Continuous)", plotlyOutput("pkscatbsa", height = 700, width = "100%")),
          tabPanel("Exposure vs. Age (Continuous)", plotlyOutput("pkscatage", height = 700, width = "100%")),
          tabPanel("Proportion Matched by Weight", plotlyOutput("pmatchwt", height = 700, width = "100%")),
          tabPanel("Proportion Matched by BSA", plotlyOutput("pmatchbsa", height = 700, width = "100%")),
          tabPanel("Proportion Matched by Age", plotlyOutput("pmatchage", height = 700, width = "100%"))
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

  nseed <- 1234

  # Read-in demographics
  nhanes_demo <- read.xport("./data/DEMO_I.XPT") %>%
    tbl_df() %>%
    dplyr::select(SEQN, SEX = RIAGENDR, AGE = RIDAGEYR, AGEM = RIDAGEMN, AGEM2 = RIDEXAGM)

  # Read-in body measurement
  nhanes_bmx <- read.xport("./data/BMX_I.XPT") %>%
    tbl_df() %>%
    dplyr::select(SEQN, WT = BMXWT, HT = BMXHT, BMI = BMXBMI) %>%
    mutate(BSA = sqrt(WT * HT / 3600))

  # Merge datasets
  nhanes <- left_join(nhanes_demo, nhanes_bmx) %>%
    drop_na(WT, AGE, BSA)


  re_simdf <- reactive({
    mod <- mcode("mod", input$modelcode)
    # mod <- mread("mod", file = "./model/pk2cmt2.cpp")

    Nsubj <- input$nsubj
    Ntrial <- input$ntrial

    set.seed(nseed)
    pnhanes <- nhanes %>%
      filter(AGE >= input$agerange[1], AGE <= input$agerange[2]) %>%
      sample_n(size = Nsubj * Ntrial, replace = TRUE) %>%
      mutate(
        subj = rep(1:Nsubj, times = Ntrial),
        trial = rep(1:Ntrial, each = Nsubj),
        ID = 1:n()
      )

    set.seed(nseed)
    anhanes <- nhanes %>%
      filter(AGE >= 18) %>%
      sample_n(size = 1000, replace = TRUE) %>%
      mutate(
        subj = rep(1:1000, times = 1),
        trial = rep(1, each = 1000),
        ID = 1:n()
      )

    pdata <- expand.ev(
      # amt = as.numeric(input$pdose)[1],
      tinf = as.numeric(input$ptinf),
      ii = as.numeric(input$pii),
      cmt = ifelse(input$proute == "iv", 2, 1),
      addl = 1,
      SEQN = pnhanes$SEQN
    ) %>%
      left_join(pnhanes) %>%
      mutate(
        WT2 = as.numeric(cut2(WT, cuts = as.numeric(input$wt))),
        BSA2 = as.numeric(cut2(BSA, cuts = as.numeric(input$bsa))),
        ii2 = ii
      ) %>%
      group_by(ID) %>%
      mutate(
        amt = ifelse(length(input$pdose) >= 2 & (input$pdosetype == "WT-based" | input$pdosetype == "WT-Tiered Fixed"), as.numeric(input$pdose)[WT2], 
                     ifelse(length(input$pdose) >= 2 & (input$pdosetype == "BSA-based" | input$pdosetype == "BSA-Tiered Fixed"), as.numeric(input$pdose)[BSA2],
                            as.numeric(input$pdose))),
        amt = case_when(
          input$pdosetype == "WT-Tiered Fixed" ~ amt,
          input$pdosetype == "BSA-Tiered Fixed" ~ amt,
          input$pdosetype == "WT-based" ~ amt * WT,
          input$pdosetype == "BSA-based" ~ amt * BSA
        ),
        rate = ifelse(tinf == 0, rate, amt / tinf),
        dose = amt
      )


    adata <- expand.ev(
      amt = as.numeric(input$adose),
      tinf = as.numeric(input$atinf),
      ii = as.numeric(input$aii),
      cmt = ifelse(input$aroute == "iv", 2, 1),
      addl = 1,
      SEQN = anhanes$SEQN
    ) %>%
      left_join(anhanes) %>%
      mutate(
        ii2 = ii,
        amt = case_when(
          input$adosetype == "Fixed" ~ amt,
          input$adosetype == "WT-based" ~ amt * WT,
          input$adosetype == "BSA-based" ~ amt * BSA
        ),
        dose = amt
      )

    mod <- mod %>% param(TVCL = input$cl, TVVC = input$vc, CL_WT = input$cl_wt, VC_WT = input$vc_wt)

    if (input$cmt == "1cmt") mod <- mod %>% param(TVQ = 0)

    if (input$cmt == "2cmt") mod <- mod %>% param(TVQ = input$q, TVVP = input$vp)

    omega_vec <- as.numeric(unlist(strsplit(input$omega, ",")))
    omegab_vec <- as.numeric(unlist(strsplit(input$omegab, ",")))
    if (input$omegatype == "Zero") mod <- mod %>% zero_re()
    if (input$omegatype == "Diag") mod <- mod %>% omat(dmat(omega_vec))
    if (input$omegatype == "Block") mod <- mod %>% omat(cmat(omegab_vec))

    if (input$proute == "sc/oral") pmod <- mod %>% param(TVKA1 = input$ka) else pmod <- mod
    if (input$aroute == "sc/oral") amod <- mod %>% param(TVKA1 = input$ka) else amod <- mod




    nobs <- 50

    set.seed(nseed)

    psim_1 <- pmod %>%
      data_set(pdata %>%
        mutate(
          ss = 0,
          ss2 = ss
        )) %>%
      Req(CP, AUC, CL) %>%
      carry_out(WT, AGE, BSA, SEX, amt, evid, cmt, ss, ii, ss2, ii2, trial, subj, dose) %>%
      mrgsim_df(end = input$pii * 2, tgrid = tgrid(0, input$pii * 2, input$pii / nobs), obsonly = F, tad = TRUE) %>%
      tbl_df() %>%
      mutate(
        ss2 = recode(ss2,
          `0` = "First Dose",
          `1` = "Steady-State"
        ),
        pop = "peds"
      )

    set.seed(nseed)

    psim_ss <- pmod %>%
      data_set(pdata %>%
        mutate(
          ss = 1,
          ss2 = ss
        )) %>%
      Req(CP, AUC, CL) %>%
      carry_out(WT, AGE, BSA, SEX, amt, evid, cmt, ss, ii, ss2, ii2, trial, subj, dose) %>%
      mrgsim_df(end = input$pii * 2, tgrid = tgrid(0, input$pii * 2, input$pii / nobs), obsonly = F, tad = TRUE) %>%
      tbl_df() %>%
      mutate(
        ss2 = recode(ss2,
          `0` = "First Dose",
          `1` = "Steady-State"
        ),
        pop = "peds"
      )

    set.seed(nseed)

    asim_1 <- amod %>%
      data_set(adata %>%
        mutate(
          ss = 0,
          ss2 = ss
        )) %>%
      Req(CP, AUC, CL) %>%
      carry_out(WT, AGE, BSA, SEX, amt, evid, cmt, ss, ii, ss2, ii2, trial, subj, dose) %>%
      mrgsim_df(end = input$aii * 2, tgrid = tgrid(0, input$aii * 2, input$aii / nobs), obsonly = F, tad = TRUE) %>%
      tbl_df() %>%
      mutate(
        ss2 = recode(ss2,
          `0` = "First Dose",
          `1` = "Steady-State"
        ),
        pop = "adults"
      )

    set.seed(nseed)

    asim_ss <- amod %>%
      data_set(adata %>%
        mutate(
          ss = 1,
          ss2 = ss
        )) %>%
      Req(CP, AUC, CL) %>%
      carry_out(WT, AGE, BSA, SEX, amt, evid, cmt, ss, ii, ss2, ii2, trial, subj, dose) %>%
      mrgsim_df(end = input$aii * 2, tgrid = tgrid(0, input$aii * 2, input$aii / nobs), obsonly = F, tad = TRUE) %>%
      tbl_df() %>%
      mutate(
        ss2 = recode(ss2,
          `0` = "First Dose",
          `1` = "Steady-State"
        ),
        pop = "adults"
      )


    re_simdf <- bind_rows(psim_1, psim_ss, asim_1, asim_ss) %>%
      mutate(
        WTC = ifelse(pop == "peds", as.character(cut2(WT[pop == "peds"], cuts = as.numeric(input$wt))), "Adults"),
        WTC = factor(WTC),
        WTC = factor(WTC, levels = c(levels(WTC)[levels(WTC) != "Adults"], "Adults")),
        BSAC = ifelse(pop == "peds", as.character(cut2(BSA[pop == "peds"], cuts = as.numeric(input$bsa))), "Adults"),
        BSAC = factor(BSAC),
        BSAC = factor(BSAC, levels = c(levels(BSAC)[levels(BSAC) != "Adults"], "Adults")),
        AGEC = ifelse(pop == "peds", as.character(cut2(AGE[pop == "peds"], cuts = as.numeric(input$age))), "Adults"),
        AGEC = factor(AGEC),
        AGEC = factor(AGEC, levels = c(levels(AGEC)[levels(AGEC) != "Adults"], "Adults"))
      )

    re_simdf
  })


  re_sumpk_adults <- reactive({
    re_simdf <- re_simdf()

    re_sumpk_adults <- re_simdf %>%
      filter(pop == "adults") %>%
      filter(!((time == 0 & evid == 0) | (time == 0 & cmt == 1)) & time <= input$aii) %>%
      group_by(pop, trial, subj, ID, WT, AGE, WTC, AGEC, BSAC, ss2) %>%
      summarise(
        Cmax = max(CP),
        Cmin = min(CP),
        Cavg = (AUC[n()] - AUC[1]) / (time[n()] - time[1])
      ) %>%
      gather(exposure, value, Cmax, Cmin, Cavg) %>%
      mutate(exposure = paste0(exposure, ": ", ss2)) %>%
      arrange(pop, trial, subj, WT, AGE, WTC, AGEC, BSAC, ss2)
    re_sumpk_adults
  })

  re_sumpk <- reactive({
    re_simdf <- re_simdf()

    re_sumpk <- re_simdf %>%
      filter(pop == "peds") %>%
      filter(!((time == 0 & evid == 0) | (time == 0 & cmt == 1)) & time <= input$pii) %>%
      group_by(pop, trial, subj, ID, WT, AGE, BSA, WTC, AGEC, BSAC, ss2) %>%
      summarise(
        Cmax = max(CP),
        Cmin = min(CP),
        Cavg = (AUC[n()] - AUC[1]) / (time[n()] - time[1])
      ) %>%
      gather(exposure, value, Cmax, Cmin, Cavg) %>%
      ungroup() %>%
      mutate(
        exposure = paste0(exposure, ": ", ss2),
        # WTC1 = as.numeric(cut2(WT, cuts = seq(floor(min(WT)), ceiling(max(WT)), by = 5))),
        WTC1 = as.numeric(cut2(WT, g = 10)),
        # BSAC1 = as.numeric(cut2(BSA, cuts = seq(floor(min(BSA)), ceiling(max(BSA)), by = 0.1))),
        BSAC1 = as.numeric(cut2(BSA, g = 10))
      ) %>%
      mutate(AGEC1 = as.numeric(cut2(AGE, cuts = seq(floor(min(AGE)), ceiling(max(AGE)), by = 1)))) %>%
      group_by(WTC, WTC1) %>%
      mutate(WTC1M = median(WT, na.rm = TRUE)) %>%
      group_by(BSAC, BSAC1) %>%
      mutate(BSAC1M = median(BSA, na.rm = TRUE)) %>%
      group_by(AGEC, AGEC1) %>%
      mutate(AGEC1M = median(AGE, na.rm = TRUE)) %>%
      ungroup()

    re_sumpk
  })



  pkstats_adults <- reactive({
    re_sumpk_adults <- re_sumpk_adults()

    pkstats_adults <- re_sumpk_adults %>% 
      group_by(exposure) %>%
      summarise(
        P05 = quantile(value, probs = 0.05, na.rm = TRUE),
        P50 = quantile(value, probs = 0.5, na.rm = TRUE),
        P95 = quantile(value, probs = 0.95, na.rm = TRUE)
      ) %>%
      ungroup()

    pkstats_adults
  })


  output$pkwt <- renderPlotly({
    re_simdf <- re_simdf()
    
    conc_summary <- re_simdf
    
    if(input$stat == "Trial Mean / Uncertainty"){
      conc_summary <- conc_summary %>% 
        filter(pop == "peds") %>% 
        group_by(trial, pop, ss2, time, WTC) %>% 
        summarise(CP = mean(CP)) %>% 
        bind_rows(conc_summary %>% filter(pop == "adults"))
    }
    
    conc_summary <- conc_summary %>% 
      # filter(pop == "peds") %>%
      group_by(pop, ss2, time, WTC) %>%
      summarise(
        conc_m = quantile(CP, probs = 0.5),
        conc_l = quantile(CP, probs = 0.05),
        conc_u = quantile(CP, probs = 0.95)
      ) %>%
      # filter(!(ss2 == "Steady-State" & time == 0)) %>%
      ungroup() %>%
      arrange(WTC, ss2, time) %>%
      mutate(
        WTC = factor(WTC, levels = c("Adults", levels(WTC)[levels(WTC) != "Adults"])),
        WTC2 = WTC
      )
    
    p <- ggplot(conc_summary %>% filter(pop == "peds")) +
      facet_grid(ss2 ~ WTC2) +
      geom_ribbon(
        data = conc_summary %>% filter(pop == "adults") %>% dplyr::select(-WTC2),
        aes(x = time, ymax = conc_u, ymin = conc_l, colour = WTC, fill = WTC), alpha = 0.3
      ) +
      geom_ribbon(aes(x = time, ymax = conc_u, ymin = conc_l, colour = WTC, fill = WTC), alpha = 0.3) +
      geom_line(aes(time, conc_m, colour = WTC, fill = WTC), size = 1.1) +
      # geom_point(aes(time, conc_m, colour = "Median"), alpha = 0.5, size = 1.2)+
      labs(x = "Time", y = "Concentration", colour = "Body Weight", fill = "Body Weight") +
      scale_color_manual(values = c(get_palette("npg", length(levels(conc_summary$WTC)[levels(conc_summary$WTC) != "Adults"])), "darkgrey")) +
      scale_fill_manual(values = c(get_palette("npg", length(levels(conc_summary$WTC)[levels(conc_summary$WTC) != "Adults"])), "darkgrey"))
    if (input$log) p <- p + scale_y_log10()
    p <- ggplotly(p) %>% layout(margin = list(l = 100, b = 100))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  
  output$pkbsa <- renderPlotly({
    re_simdf <- re_simdf()

    conc_summary <- re_simdf
    
    if(input$stat == "Trial Mean / Uncertainty"){
      conc_summary <- conc_summary %>% 
        filter(pop == "peds") %>% 
        group_by(trial, pop, ss2, time, BSAC) %>% 
        summarise(CP = mean(CP)) %>% 
        bind_rows(conc_summary %>% filter(pop == "adults"))
    }
    
    conc_summary <- conc_summary %>% 
      # filter(pop == "peds") %>%
      group_by(pop, ss2, time, BSAC) %>%
      summarise(
        conc_m = quantile(CP, probs = 0.5),
        conc_l = quantile(CP, probs = 0.05),
        conc_u = quantile(CP, probs = 0.95)
      ) %>%
      # filter(!(ss2 == "Steady-State" & time == 0)) %>%
      ungroup() %>%
      arrange(BSAC, ss2, time) %>%
      mutate(
        BSAC = factor(BSAC, levels = c("Adults", levels(BSAC)[levels(BSAC) != "Adults"])),
        BSAC2 = BSAC
      )

    p <- ggplot(conc_summary %>% filter(pop == "peds")) +
      facet_grid(ss2 ~ BSAC2) +
      geom_ribbon(
        data = conc_summary %>% filter(pop == "adults") %>% dplyr::select(-BSAC2),
        aes(x = time, ymax = conc_u, ymin = conc_l, colour = BSAC, fill = BSAC), alpha = 0.3
      ) +
      geom_ribbon(aes(x = time, ymax = conc_u, ymin = conc_l, colour = BSAC, fill = BSAC), alpha = 0.3) +
      geom_line(aes(time, conc_m, colour = BSAC, fill = BSAC), size = 1.1) +
      # geom_point(aes(time, conc_m, colour = "Median"), alpha = 0.5, size = 1.2)+
      labs(x = "Time", y = "Concentration", colour = "BSA", fill = "BSA") +
      scale_color_manual(values = c(get_palette("npg", length(levels(conc_summary$BSAC)[levels(conc_summary$BSAC) != "Adults"])), "darkgrey")) +
      scale_fill_manual(values = c(get_palette("npg", length(levels(conc_summary$BSAC)[levels(conc_summary$BSAC) != "Adults"])), "darkgrey"))
    if (input$log) p <- p + scale_y_log10()
    p <- ggplotly(p) %>% layout(margin = list(l = 100, b = 100))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  

  


  output$pkage <- renderPlotly({
    re_simdf <- re_simdf()

    conc_summary <- re_simdf
    
    if(input$stat == "Trial Mean / Uncertainty"){
      conc_summary <- conc_summary %>% 
        filter(pop == "peds") %>% 
        group_by(trial, pop, ss2, time, AGEC) %>% 
        summarise(CP = mean(CP)) %>% 
        bind_rows(conc_summary %>% filter(pop == "adults"))
    }
    
    conc_summary <- conc_summary %>% 
      # filter(pop == "peds") %>%
      group_by(pop, ss2, time, AGEC) %>%
      summarise(
        conc_m = quantile(CP, probs = 0.5),
        conc_l = quantile(CP, probs = 0.05),
        conc_u = quantile(CP, probs = 0.95)
      ) %>%
      # filter(!(ss2 == "Steady-State" & time == 0)) %>%
      ungroup() %>%
      arrange(AGEC, ss2, time) %>%
      mutate(
        AGEC = factor(AGEC, levels = c("Adults", levels(AGEC)[levels(AGEC) != "Adults"])),
        AGEC2 = AGEC
      )

    p <- ggplot(conc_summary %>% filter(pop == "peds")) +
      facet_grid(ss2 ~ AGEC2) +
      geom_ribbon(
        data = conc_summary %>% filter(pop == "adults") %>% dplyr::select(-AGEC2),
        aes(x = time, ymax = conc_u, ymin = conc_l, colour = AGEC, fill = AGEC), alpha = 0.3
      ) +
      geom_ribbon(aes(x = time, ymax = conc_u, ymin = conc_l, colour = AGEC, fill = AGEC), alpha = 0.3) +
      geom_line(aes(time, conc_m, colour = AGEC, fill = AGEC), size = 1.1) +
      # geom_point(aes(time, conc_m, colour = "Median"), alpha = 0.5, size = 1.2)+
      labs(x = "Time", y = "Concentration", colour = "Body Weight", fill = "Body Weight") +
      scale_color_manual(values = c(get_palette("npg", length(levels(conc_summary$AGEC)[levels(conc_summary$AGEC) != "Adults"])), "darkgrey")) +
      scale_fill_manual(values = c(get_palette("npg", length(levels(conc_summary$AGEC)[levels(conc_summary$AGEC) != "Adults"])), "darkgrey"))
    if (input$log) p <- p + scale_y_log10()
    p <- ggplotly(p) %>% layout(margin = list(l = 100, b = 100))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })





  output$pkbxpwt <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()

    p <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      p <- p %>% 
        group_by(trial, exposure, WTC) %>% 
        summarise(value = mean(value))
    }
    
    p <- p %>%
      bind_rows(re_sumpk_adults) %>% 
      ungroup() %>%
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed") +
      geom_violin(aes(WTC, value, colour = WTC, fill = WTC), width = 1, alpha = 0.3) +
      geom_boxplot(aes(WTC, value, colour = WTC, fill = WTC), width = 0.1, alpha = 0.3) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(x = "Body Weight (kg)", y = "Exposure", colour = "Body Weight", fill = "Body Weight")
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  
  output$pkbxpbsa <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()
    
    p <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      p <- p %>% 
        group_by(trial, exposure, BSAC) %>% 
        summarise(value = mean(value))
    }
    
    p <- p %>%
      bind_rows(re_sumpk_adults) %>% 
      ungroup() %>%
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed") +
      geom_violin(aes(BSAC, value, colour = BSAC, fill = BSAC), width = 1, alpha = 0.3) +
      geom_boxplot(aes(BSAC, value, colour = BSAC, fill = BSAC), width = 0.1, alpha = 0.3) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(x = "Body Surface Area (m^2)", y = "Exposure", colour = "BSA", fill = "BSA")
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  }) 

  output$pkbxpage <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()

    p <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      p <- p %>% 
        group_by(trial, exposure, AGEC) %>% 
        summarise(value = mean(value))
    }
    
    p <- p %>%
      bind_rows(re_sumpk_adults) %>% 
      ungroup() %>%
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed") +
      geom_violin(aes(AGEC, value, colour = AGEC, fill = AGEC), width = 1, alpha = 0.3) +
      geom_boxplot(aes(AGEC, value, colour = AGEC, fill = AGEC), width = 0.1, alpha = 0.3) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(x = "Age (year)", y = "Exposure", colour = "Age", fill = "Age")
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })


  output$pkscatbw <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()

    re_sumpk_stat <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      re_sumpk_stat <- re_sumpk_stat %>% 
        group_by(trial, WTC1, WTC1M, WTC, ss2, exposure) %>% 
        summarise(value = mean(value))
    }
    
    re_sumpk_stat <- re_sumpk_stat %>% 
      group_by(WTC1, WTC1M, WTC, ss2, exposure) %>%
      summarise_at(
        vars(value),
        list(
          N = ~ n(),
          P05 = ~ quantile(., probs = 0.05, na.rm = TRUE),
          P50 = ~ quantile(., probs = 0.5, na.rm = TRUE),
          P95 = ~ quantile(., probs = 0.95, na.rm = TRUE)
        )
      ) %>%
      ungroup() %>%
      arrange(WTC1M)

    p <- re_sumpk %>%
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed") +
      geom_ribbon(data = re_sumpk_stat, aes(WTC1M, ymin = P05, ymax = P95), alpha = 0.3) +
      geom_line(data = re_sumpk_stat, aes(WTC1M, P50), alpha = 0.3, size = 1.2) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(x = "Body Weight (kg)", y = "Exposure", colour = "", fill = "")
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })

  output$pkscatbsa <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()
    
    re_sumpk_stat <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      re_sumpk_stat <- re_sumpk_stat %>% 
        group_by(trial, BSAC1, BSAC1M, BSAC, ss2, exposure) %>% 
        summarise(value = mean(value))
    }
    
    re_sumpk_stat <- re_sumpk_stat %>% 
      group_by(BSAC1, BSAC1M, BSAC, ss2, exposure) %>%
      summarise_at(
        vars(value),
        list(
          N = ~ n(),
          P05 = ~ quantile(., probs = 0.05, na.rm = TRUE),
          P50 = ~ quantile(., probs = 0.5, na.rm = TRUE),
          P95 = ~ quantile(., probs = 0.95, na.rm = TRUE)
        )
      ) %>%
      ungroup() %>%
      arrange(BSAC1M)
    
    p <- re_sumpk %>%
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed") +
      geom_ribbon(data = re_sumpk_stat, aes(BSAC1M, ymin = P05, ymax = P95), alpha = 0.3) +
      geom_line(data = re_sumpk_stat, aes(BSAC1M, P50), alpha = 0.3, size = 1.2) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(x = "Body Surface Area (m^2)", y = "Exposure", colour = "", fill = "")
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  
  output$pkscatage <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()

    re_sumpk_stat <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      re_sumpk_stat <- re_sumpk_stat %>% 
        group_by(trial, AGEC1, AGEC1M, AGEC, ss2, exposure) %>% 
        summarise(value = mean(value))
    }
    
    re_sumpk_stat <- re_sumpk_stat %>% 
      group_by(AGEC1, AGEC1M, AGEC, ss2, exposure) %>%
      summarise_at(
        vars(value),
        list(
          N = ~ n(),
          P05 = ~ quantile(., probs = 0.05, na.rm = TRUE),
          P50 = ~ quantile(., probs = 0.5, na.rm = TRUE),
          P95 = ~ quantile(., probs = 0.95, na.rm = TRUE)
        )
      ) %>%
      ungroup() %>%
      arrange(AGEC1M)

    p <- re_sumpk %>%
      ggplot() +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_hline(data = pkstats_adults, aes(yintercept = P50), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P95), colour = "darkgrey", linetype = "dashed") +
      geom_hline(data = pkstats_adults, aes(yintercept = P05), colour = "darkgrey", linetype = "dashed") +
      geom_ribbon(data = re_sumpk_stat, aes(AGEC1M, ymin = P05, ymax = P95), alpha = 0.3) +
      geom_line(data = re_sumpk_stat, aes(AGEC1M, P50), alpha = 0.3, size = 1.2) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(x = "Age (year)", y = "Exposure", colour = "", fill = "")
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })

  output$pmatchwt <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()

    p <- re_sumpk
      
    if(input$stat == "Trial Mean / Uncertainty"){
      p <- p %>% 
        group_by(trial, WTC, exposure) %>% 
        summarise(value = mean(value))
    }

    p <- p %>% 
      left_join(pkstats_adults) %>%
      mutate(Prop = value >= P05 & value <= P95) %>%
      group_by(WTC, exposure) %>%
      summarise(Prop = mean(Prop, na.rm = TRUE)) %>%
      ggplot(aes(x = WTC, y = Prop, fill = WTC)) +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_bar(stat = "identity") +
      geom_text(aes(
        y = Prop + 0.05,
        label = round(Prop, 3)
      ),
      size = 5
      ) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(
        title = "Proportion within Adult P05-P95 Exposure",
        x = "Weight Category",
        y = "Proportion",
        fill = "Weight Category"
      ) +
      scale_y_continuous(limits = c(0, 1.1), breaks = (0:10) / 10)

    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })

  output$pmatchbsa <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()
    
    p <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      p <- p %>% 
        group_by(trial, BSAC, exposure) %>% 
        summarise(value = mean(value))
    }
    
    p <- p %>% 
      left_join(pkstats_adults) %>%
      mutate(Prop = value >= P05 & value <= P95) %>%
      group_by(BSAC, exposure) %>%
      summarise(Prop = mean(Prop, na.rm = TRUE)) %>%
      ggplot(aes(x = BSAC, y = Prop, fill = BSAC)) +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_bar(stat = "identity") +
      geom_text(aes(
        y = Prop + 0.05,
        label = round(Prop, 3)
      ),
      size = 5
      ) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(
        title = "Proportion within Adult P05-P95 Exposure",
        x = "BSA Category",
        y = "Proportion",
        fill = "BSA Category"
      ) +
      scale_y_continuous(limits = c(0, 1.1), breaks = (0:10) / 10)
    
    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
  output$pmatchage <- renderPlotly({
    re_sumpk_adults <- re_sumpk_adults()
    re_sumpk <- re_sumpk()
    pkstats_adults <- pkstats_adults()

    p <- re_sumpk
    
    if(input$stat == "Trial Mean / Uncertainty"){
      p <- p %>% 
        group_by(trial, AGEC, exposure) %>% 
        summarise(value = mean(value))
    }
    
    p <- p %>% 
      left_join(pkstats_adults) %>%
      mutate(Prop = value >= P05 & value <= P95) %>%
      group_by(AGEC, exposure) %>%
      summarise(Prop = mean(Prop, na.rm = TRUE)) %>%
      ggplot(aes(x = AGEC, y = Prop, fill = AGEC)) +
      facet_wrap(~exposure, ncol = 2, scales = "free_y") +
      geom_bar(stat = "identity") +
      geom_text(aes(
        y = Prop + 0.05,
        label = round(Prop, 3)
      ),
      size = 5
      ) +
      scale_color_npg() +
      scale_fill_npg() +
      labs(
        title = "Proportion within Adult P05-P95 Exposure",
        x = "Age Category",
        y = "Proportion",
        fill = "Age Category"
      ) +
      scale_y_continuous(limits = c(0, 1.1), breaks = (0:10) / 10)

    p <- ggplotly(p) %>% layout(margin = list(l = 75, b = 75))
    layout_ggplotly(p, x = -0.05, y = -0.05)
  })
}

shinyApp(ui, server)
