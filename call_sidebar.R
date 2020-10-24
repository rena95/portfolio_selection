call_sidebar <- dashboardSidebar(  
                  tags$head(tags$style(HTML('   /* body */
                                .sidebar {
                                font-size: 10px;
                                }
                                .checkbox {
                                margin-bottom: 2px;
                                }
                                '))),
      checkboxGroupInput(inputId = "USA","USA",
                         c("AMAZON (AMZN)","BOEING (BA)","FACEBOOK (FB)","GOOGLE (GOOG)",
                           "MICROSOFT (MSFT)","NRG ENERGY (NRG)","WALMART (WMT)"),
                     selected = c("FACEBOOK (FB)")
      ),
      checkboxGroupInput(inputId = "EU","EU",
                         c("AXA SA (CS.PA)",
                           "DALMIER AG (DAI.DE)","ENEL MI","ENI MI","FERRARI NV (RACE)",
                           "NOKIA (NOK)","TOTAL (TOT)","VODAFONE PLC (VOD)","VOLKSWAGEN AG (WOW.DE)"),
                         selected = c("FERRARI NV (RACE)")
      ),
      checkboxGroupInput(inputId = "ETF","ETF",
                         c("ISHARE GLOBAL CLEAN ENERGY ETF (ICLN)","ISHARE LARGE CAP ETF (FXI)","KRANESHARES CSI CHINA INTERNET ETF (KWEB)","VANGUARD ITC INDEX ETF (VGT)" ,
                           "VANGUARD REAL ESTATE INDEX ETF (VNQ)","VANGAURD S&P 500 ETF (VOO)","XTRACKERS HARVEST CSI 300 CHINA A ETF (ASHR)"),
                         selected = c("ISHARE GLOBAL CLEAN ENERGY ETF (ICLN)")
   
      ),
      actionButton(inputId="BUTTON_1", label="OK")
)
