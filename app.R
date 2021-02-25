library(Quandl)
library(zoo)
library(dplyr)
library(plyr)
library(lubridate)
library(quantmod)
library(ggplot2)
library(tidyr)
library(GGally)
library(neuralnet)
library(shiny)
library(DT)
library(shinydashboard)
library(plotly)
library(stringr)

source("call_sidebar.R")
source("call_body.R")

n_sim <- 2000
###############UTILITIES####################


upload_stock <- function(stock, max_d){
  # NOTE:"app_string" and "value" must have the same order
  app_string <-  c("AMAZON (AMZN)","BOEING (BA)","FACEBOOK (FB)","GOOGLE (GOOG)",
                   "MICROSOFT (MSFT)","NRG ENERGY (NRG)","WALMART (WMT)","AXA SA (CS.PA)",
                   "DALMIER AG (DAI.DE)","ENEL MI","ENI MI","FERRARI NV (RACE)",
                   "NOKIA (NOK)","TOTAL (TOT)","VODAFONE PLC (VOD)","VOLKSWAGEN AG (WOW.DE)","ALIBABA (BABA)",
                   "ISHARE GLOBAL CLEAN ENERGY ETF (ICLN)","ISHARE LARGE CAP ETF (FXI)","KRANESHARES CSI CHINA INTERNET ETF (KWEB)","VANGUARD ITC INDEX ETF (VGT)",
                   "VANGUARD REAL ESTATE INDEX ETF (VNQ)", "VANGAURD S&P 500 ETF (VOO)","XTRACKERS HARVEST CSI 300 CHINA A ETF (ASHR)", "XTRACKERS MSCI WORLD HEALTH CARE (XDWH.MI)",
                   "ISHARES ELECTRIC VEHICLES AND DRIVING TEC (ECAR.MI)","WISDOMTREE PHYSICAL GOLD (PHAU.MI)", "ISHARES EURO STOXX (EXSI.DE)",
                   "ISHARES NASDAQ 100 (CSNDX.MI)", "ISHARES CORE MSCI WORLD (SWDA.MI)")
  pos        <- which(stock==app_string)
  value      <-    c("AMZN","BA","FB","GOOG",
                     "MSFT","NRG","WMT","CS.PA",
                     "DAI.DE","ENEL.MI","ENI.MI","RACE",
                     "NOK","TOT","VOD","VOW.DE","BABA",
                     "ICLN","FXI","KWEB","VGT",
                     "VNQ","VOO","ASHR","XDWH.MI",
                     "ECAR.MI","PHAU.MI","EXSI.DE","CSNDX.MI",
                     "SWDA.MI")[pos]
  
  # Download price
  getSymbols(value, src="yahoo")
  dat <- as.data.frame(get(value))
  dat <- cbind(row.names(dat),dat)
  names(dat)[1] <- "Date"
  dat <- from_day_to_month(dat)
  dat$Date <- as.Date(dat$Date)
  max_date <- as.Date(paste0(year(max_d),"-",month(max_d), "-01"))
  dat <- subset(dat, Date <= max_date)
  return(dat[,c(1,2)])
}


# Return the first day observation for each month
from_day_to_month <- function(df){
  df <- df %>%  dplyr::mutate(Date=lubridate::ymd(Date))
  df <- df %>%  dplyr::mutate(year = lubridate::year(Date), 
                              month= lubridate::month(Date)) %>%
    dplyr::group_by(year,month) %>%
    arrange(Date) %>%
    filter(row_number()==1)
  df <- df %>% mutate(Date=ymd(paste0(year,"-",month,"-01")))
  df
}



ui <- dashboardPage(
  dashboardHeader(title="Portfolio selection"),
  sidebar = call_sidebar,
  body    = call_body
)

percentage <- function(number){
  paste(round(100* number, 2), "%", sep="")
} 

server <- function(input, output){
  
  observeEvent(
    input$BUTTON_1,{
      withProgress(message = "Uploading...",{ 
        # Progress: 1/5
        incProgress(1/5)
        
        sel_stocks <- c(input$USA, input$EU, input$CHINA, input$ETF)
        series  <- lapply(sel_stocks, upload_stock,max_d = input$input_date )
        n_stock <- length(series)
        
        # Select the minimum span of time among the selected stocks, e.g:
        # Facebook: first trading month is May-2012 (102 months upon today)
        # Boeing: first trading month is October-1980 (492 months upon today)
        # The totale dataframe (i.e. data|Facebook|Boeing) considers stocks for the lasts 102 months!
        sel_min_month <- which.min(sapply(series,function(dat) {length(dat$Date)}))
        dates         <- series[[sel_min_month]]$Date
        first_month   <- length(dates)
        output$n_obs  <- renderText(paste0("Number of monthly observations: ",as.character(first_month)))
        
        # Rescale all the stocks to the same span of time
        series <- lapply(series,function(dat){
          if(nrow(dat)>first_month){
            dat <- dat[(nrow(dat)-first_month+1):nrow(dat),]
          }
          dat[,2]
        })
        series <- do.call("cbind",series)
        series <- cbind(dates,series)
        series <- na.omit(series)
        dates         <- series$dates
        
        names_stocks <- str_sub(names(series)[-1], end  = -6)
        
        
        # Compute the monthly returns
        yld     <-  (series[2:nrow(series),-1] - series[1:(nrow(series)-1),-1]) / series[1:(nrow(series)-1),-1]
        
        # Compute the yearly covariance
        cov_yld <-  cov(yld)*12
        
        yld     <-  cbind(dates[-1],yld)
        # Mean monthly returns for each stock 
        mean_yld <- colMeans(yld[,-1])
        
        # Build the efficient frontier 
        set.seed(20101995)
        
        # Start simulation
        ptf_sim     <- lapply(1:n_sim,function(sim){
          # Random weights
          w_i_abs   <- sample(1:1000,n_stock,replace = TRUE)
          # Rescale weights in (0,1)
          w_i       <-  w_i_abs/sum(w_i_abs)
          
          # Annualized simulated returns
          return_i  <- (sum(w_i*mean_yld)+1)^12 - 1
          
          # Annualized simulated covariances
          risk_yld  <- t(w_i) %*% cov_yld %*% w_i
          
          # Sharpe ratio
          sharpe_ratio <- return_i/risk_yld
          
          list(w_i=w_i, return_i=return_i, risk_yld=risk_yld, sharpe_ratio=sharpe_ratio)
        })
        
        
        # Progress: 2/5
        incProgress(2/5)
        
        # Put the list  in a dataframe
        df_ptf_sim <- data.frame(return       = sapply(ptf_sim,function(col){col$return_i}),
                                 risk_yld     = sapply(ptf_sim,function(col){col$risk_yld}),
                                 sharpe_ratio = sapply(ptf_sim,function(col){col$sharpe_ratio})
        )
        # Insert the weights in the dataframe
        for(i in 1:n_stock){
          df_ptf_sim <- cbind(df_ptf_sim, sapply(ptf_sim,function(col){col[[1]][i]}))
          
        }
        # Names the columns containing the weights
        names(df_ptf_sim)[(length(df_ptf_sim)-n_stock+1):length(df_ptf_sim)]   <- names_stocks
        
        # Build a summary column (whose value will appear when hovering with mouse on graph "a")
        tmp <-  apply(df_ptf_sim[,names_stocks], 1, function(stock){
          paste0(names_stocks,":", percentage(stock), "<br>")
        })
        df_ptf_sim$W <- do.call("paste", c(as.data.frame(t(tmp)),sep="  "))
        rm(tmp)
        
        # Select minimum risk portfolio 
        min_risk         <- df_ptf_sim[which.min(df_ptf_sim$risk_yld),]
        # Select maximum sharp ratio portfolio 
        max_sharpe_ratio <- df_ptf_sim[which.max(df_ptf_sim$sharpe_ratio),]
        
        # Build the minimum risk portfolio and maximum sharp ratio portfolio 
        extract_weight <- function(w){
          weights <- scan(text = w, what = "") %>% 
            str_remove_all(., "%<br>")
          weights <- as.numeric(substr(sub(".*:", "", weights),1,4))/100
          return(weights)
        }
        extract_weight_min_risk  <- extract_weight(min_risk$W)
        extract_max_sharpe_ratio  <- extract_weight(max_sharpe_ratio$W)
        
        ptf_max_sharpe <- apply(series[-1],1,function(row){extract_max_sharpe_ratio * row }) %>%
          colSums(.)
        ptf_min_risk         <- apply(series[-1],1,function(row){extract_weight_min_risk * row }) %>%
          colSums(.) 
        ptf                  <- rbind(ptf_max_sharpe, ptf_min_risk) %>% as.data.frame() %>%
          t() %>% round(.,2)
        series               <- cbind(series,ptf)
        yld_ptf              <- (ptf[2:nrow(ptf),] - ptf[1:(nrow(ptf)-1),]) / ptf[1:(nrow(ptf)-1),]
        # Summary Portfolio
        summary_ptf <- data.frame(Mean  = colMeans(yld_ptf), 
                                  Sd    = apply(yld_ptf,2,sd),
                                  Worst = apply(yld_ptf,2,min),
                                  Max   = apply(yld_ptf,2,max),
                                  median   = apply(yld_ptf,2, function(col) quantile(col, probs = 0.50 ))
        )
        
        
        
        # Summary
        summary_pos <- data.frame(Mean  = mean_yld, 
                                  Sd    = apply(yld[,-1],2,sd),
                                  Worst = apply(yld[,-1],2,min),
                                  Max   = apply(yld[,-1],2,max),
                                  median   = apply(yld[,-1],2, function(col) quantile(col, probs = 0.50 ))
        )
        summary     <- rbind(summary_pos, summary_ptf)
        
        summary           <- as.data.frame(apply(summary, 2, function(col) round(col,4)))
        rownames(summary) <- str_sub(rownames(summary),1 ,-6)
        output$summary    <- DT::renderDataTable(DT::datatable(summary,
                                                               options = list(searching = FALSE,
                                                                              paging    = FALSE)))
        
        # Progress: 3/5
        incProgress(3/5)
        
        
        a  <- ggplot(aes(x=risk_yld, y=return, color = sharpe_ratio, text=W), data =df_ptf_sim) +
          geom_point()+ 
          theme_classic() +
          scale_y_continuous(labels = scales::percent) +
          scale_x_continuous(labels = scales::percent) +
          labs(x = 'Annualized Risk',
               y = 'Annualized Returns',
               title = "Portfolio Optimization & Efficient Frontier") +
          geom_point(aes(x = risk_yld, y = return), data =min_risk , color = 'red') +
          geom_point(aes(x = risk_yld, y = return), data =max_sharpe_ratio, color = 'green') 
        
        max_sharpe_ratio_long  <-  gather(max_sharpe_ratio,"stock","weight",-c("return","risk_yld","sharpe_ratio","W"))
        b <- ggplot(max_sharpe_ratio_long,aes(x="",y=weight, fill=stock)) +
          geom_bar(stat="identity", width=1, color="white")+
          labs(x = '',
               y = 'Asset allocation',
               title = "Maximum sharpe ratio portfolio") + 
          theme_classic()
        
        min_risk_long  <-  gather(min_risk,"stock","weight",-c("return","risk_yld","sharpe_ratio","W"))
        c <- ggplot(min_risk_long,aes(x="",y=weight, fill=stock)) +
          geom_bar(stat="identity", width=1, color="white")+
          labs(x = '',
               y = 'Asset allocation',
               title = "Minimum risk portfolio") + 
          theme_classic()
        
        # Yld chart
        den <- bind_rows(replicate(nrow(series) - 1, series[1,-1], simplify = FALSE))
        num <- series[2:nrow(series),-1] 
        e   <- cbind(dates[-1] , (num - den) / den) %>% dplyr::rename(dates = 'dates[-1]')
        rm(den,num)
        
        e <-  e %>% gather(key = "Stock", value = "Price", -dates) %>%
          ggplot(., aes(x = dates , y = Price , color = Stock)) +
          geom_line() + 
          theme_bw() +
          labs(x = 'Date',
               y = '',
               title = "Performance") +
          scale_x_date(date_breaks = "3 month", date_labels = "%b-%y") + 
          theme(axis.text.x = element_text(angle = 90),
                plot.title  = element_text(color = "black", size = 25, face = "bold"),
                panel.border = element_blank()) +
          scale_y_continuous(labels = scales::percent)  
        
        
        a  <- ggplotly(a, tooltip = "text") %>% partial_bundle() 
        b  <- ggplotly(b)
        c  <- ggplotly(c)
        d  <- ggcorr(yld[,2:(n_stock+1)],label = TRUE) +
          theme(plot.title  = element_text(color = "black", size = 25, face = "bold"))
        e  <- ggplotly(e)
        
        # Progress: 4/5
        incProgress(4/5)
        
        output$graph   <- renderPlotly(a)
        output$graph_2 <- renderPlotly(b)
        output$graph_3 <- renderPlotly(c)
        output$graph_4 <- renderPlot(d)
        output$graph_5 <- renderPlotly(e)
        
        # Progress: 5/5
        incProgress(5/5)
      })
    })
}



shinyApp(ui = ui, server = server)

