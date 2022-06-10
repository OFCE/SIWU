# install.packages("tidyquant")
library(tidyquant)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ofce)
library(dygraphs)
library(xts)
library(quantmod)
library(TTR)
library(WDI)

# Yahoo finance price ----------------------------
# TTF est capricieux et a disparu de Yahoo Finance
# European Hub price (ttf)
dat <- tq_get("TTF=F",from = '2015-01-01',to = Sys.Date()) %>%
  mutate(TTF=open)%>%
  select(TTF,date)

#df_intc <- getSymbols('INTC',src='yahoo',auto.assign=FALSE)

dat1 <- tq_get("NG=F",from = '2015-01-01',to = Sys.Date()) %>%
  mutate(NGF=open)%>%
  select(NGF,date)

# Brent Oil

dat2 <- tq_get("BZ=F",from = '2015-01-01',to = Sys.Date())%>%
  mutate(BZ=open)%>%
  select(BZ,date)

# Euro
dat3 <- tq_get("EURUSD=X",from = '2015-01-01',to = Sys.Date()) %>%
  mutate(EUR=open)%>%
  select(EUR,date)

# Coal (API4 Richard Bay)

dat4 <- tq_get("MFF=F", from = '2015-01-01', to = Sys.Date()) %>%
  mutate(API4=open)%>%
  select(API4,date)

# Données en Euros

data<-dat |> 
  merge(dat1,by="date")%>%
  merge(dat2,by="date")%>%
  merge(dat3,by="date")%>%
  merge(dat4,by="date")%>%
  mutate(BZE=BZ/EUR,
         API4E=API4/EUR,
         NGF = NGF/EUR/0.293297)%>%
  select(date,TTF,BZE,API4E, NGF) |> 
  arrange(desc(date))

data_choc<-data%>%
  filter(date>"2022-02-01")%>%
  mutate(TTF=(TTF/TTF[date=="2022-02-23"]-1)*100,
         BZE=(BZE/BZE[date=="2022-02-23"]-1)*100,
         API4E=(API4E/API4E[date=="2022-02-23"]-1)*100)

data_choc_mean<-data_choc%>%
  filter(date>"2022-02-23")%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)%>%
  mutate(choc="Moyenne du choc depuis l'invasion")

data_choc<-data_choc%>%
  filter(row_number() == n())%>%
  mutate(choc="Dernière valeur")

data_choc_mean$date<-data_choc$date[1]  

Mesure_choc<-rbind(data_choc,data_choc_mean)%>%
  relocate(choc)%>%
  mutate_if(is.numeric, round, 1)

datal <- data |>  pivot_longer(cols=c(TTF, BZE, API4E)) |> 
  mutate(lib = case_when(name=="TTF" ~ "European Hub price (TTF)",
                         name=="BZE" ~ "Brent Oil (BZE)",
                         name=="API4E" ~ "Coal (API4 Richard Bay)"))


data.xts <- xts(data[-1], order.by = data$date)
# Plot Prix du GAZ

ggas <- (ggplot(data, aes(x=date, y=TTF)) +
           geom_vline(xintercept=as.Date("2022-02-23"), color="gray70", linetype="dashed", size=0.1) +
           geom_line() +
           ylab(NULL) + xlab(NULL) +
           ggtitle("Natural Gas European Hub Price (TTF)") +
           theme_ofce(base_size = 9, base_family="Nunito")) |> add_label_unit("€/MWh")

gcoal <- (ggplot(data, aes(x=date, y=API4E)) +
            geom_vline(xintercept=as.Date("2022-02-23"), color="#333388", linetype="dashed") +
            geom_line() +
            ylab(NULL) + xlab(NULL) +
            ggtitle("Coal (API4 Richard Bay)") +
            theme_ofce(base_size = 9, base_family="Nunito")) |> add_label_unit("€/ton")

gbrent <- (ggplot(data, aes(x=date, y=BZE)) +
             geom_vline(xintercept=as.Date("2022-02-23"), color="#333388", linetype="dashed") +
             geom_line() +
             ylab(NULL) + xlab(NULL) +
             ggtitle("Brent (BZ)") +
             theme_ofce(base_size = 9, base_family="Nunito")) |> add_label_unit("€/b")

library(patchwork)
br <- paletteer::paletteer_d("miscpalettes::pastel")
chocs <- (ggplot(datal, aes(x=date, y=value, col=lib)) +
            geom_vline(xintercept=as.Date("2022-02-23"), color="gray70", linetype="dashed", size=0.3) +
            geom_line(show.legend=FALSE) +
            scale_color_manual(values = c(br[4], "gray50", br[5]))+
            ylab(NULL) + xlab(NULL) +
            labs(title = "Commodities price increase",
                 subtitle = "Oil, Coal and Natural Gas",
                 caption = "Source: Yahoo Finance, ticker BZE, API4E, TTF")+
            facet_wrap(vars(lib), scales="free_y")+
            theme_ofce(base_size = 12, base_family="Nunito"))

graph2svg(chocs, width=25, ratio=16/9)
graph2jpg(chocs, width=25, ratio=16/9, dpi = 600)
graphs <- list(
  dygraph(data.xts[, c("TTF", "NGF")], main="Natural Gas (EU TTF, US NFG)", group="fossils") |>
    dyOptions(colors = c(br[4], br[8])) |> 
    dyAxis("y", logscale=TRUE),
  dygraph(data.xts$BZE, main="Oil (brent)", group="fossils") |> dyOptions(colors = "gray40") |> dyAxis("y", logscale=TRUE) ,
  dygraph(data.xts$API4E, main="Coal (API4E)", group="fossils") |> dyOptions(colors = br[5]) |> dyAxis("y", logscale=TRUE))

htmltools::browsable(htmltools::tagList(graphs))

