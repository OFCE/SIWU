
library(tidyverse)
library(readxl)
library(lubridate)
library(ofce)
library(dygraphs)
library(xts)
library(ggh4x)

daily_raw <- read_xlsx("data/gas datastream.xlsx", sheet="daily", skip=4)
codes <- read_xlsx("data/gas datastream.xlsx", sheet="daily", skip=3, n_max=1) |> as.list()
labels <- read_xlsx("data/gas datastream.xlsx", sheet="daily", skip=2, n_max=1) |> as.list()
daily <- daily_raw |> 
  mutate(date = as.Date(Code)) |> 
  relocate(date) |> 
  select(-Code) |> 
  mutate(across(-date, as.numeric)) |> 
  mutate(ttf = ETMCS00,
         iceng = NATBGAS * EURGBSR / 0.029307 / 100,
         usng = NGTXSNL * EUDOLLR / 0.293,
         brentmwh = EIAEBRT * EUDOLLR / 1.6282,
         brent = EIAEBRT * EUDOLLR,
         coalmwh = LMCYSPT * EUDOLLR / 8.13556,
         coal = LMCYSPT * EUDOLLR ) |> 
  select(-c(ETMCS00, TRNLTTD, NATBGAS, LNGINDX, NGTXSNL, EIAEBRT, LCNYSPT, LMCYSPT, EUDOLLR, EURGBSR)) |> 
  pivot_longer(cols = -date,
               names_to = "com", values_to = "price") |> 
  mutate(unit = case_when(
    com=="ttf" ~ "€/MWh",
    com=="iceng" ~ "€/MWh",
    com=="usng" ~ "€/MWh",
    com=="brentmwh" ~ "€/MWh",
    com=="brent" ~ "€/barrel",
    com=="coalmwh" ~ "€/MWh",
    com=="coal" ~ "€/mt"),
    label = case_when(
      com=="ttf" ~ "Natural Gas, Dutch TTF Endex",
      com=="iceng" ~ "Natural Gas, ICE, London",
      com=="usng" ~ "Natural Gas, US Henry Hub",
      com=="brentmwh" ~ "Brent, Europe spot price",
      com=="brent" ~ "Brent, Europe spot price",
      com=="coalmwh" ~ "Coal, ICE Newcastle",
      com=="coal" ~ "Coal, ICE Newcastle"),
    com = str_remove(com, "mwh"))

(comday <- ggplot(daily |> filter(unit == "€/MWh")) +
  geom_vline(xintercept=ymd("2022-02-24"), size = 0.2, col="gray25") +
  geom_line(aes(x=date, y=price, col=label)) + 
  scale_y_log10(
    breaks = c(5, 10, 25, 50, 75, 100),
    minor_breaks = scales::log_breaks(16), 
    guide = "axis_minor",
    sec.axis=sec_axis(
      trans = ~.x, 
      breaks = daily |> filter(unit == "€/MWh", date==max(date)) |> pull(price),
      labels = \(x) str_c(round(x, 1), " €/MWh")))+
  scale_x_date(labels=scales::label_date_short(), 
               limits = c(ymd("2021-01-01", NA)),
               expand = expansion(mult=0.01))+
  labs(color = "Commodity") + xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c(br[4], "gray50", br[5]))+
  theme_ofce()+
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c(0,1))) 


graph2svg(comday, width=25, ratio=16/9)
graph2png(comday, width=25, ratio=16/9, dpi = 600)

graphs <- list(
  dygraph(data.xts[, c("TTF", "NGF")], main="Natural Gas (EU TTF, US NFG)", group="fossils") |>
    dyOptions(colors = c(br[4], br[8])) |> 
    dyAxis("y", logscale=TRUE),
  dygraph(data.xts$BZE, main="Oil (brent)", group="fossils") |> dyOptions(colors = "gray40") |> dyAxis("y", logscale=TRUE) ,
  dygraph(data.xts$API4E, main="Coal (API4E)", group="fossils") |> dyOptions(colors = br[5]) |> dyAxis("y", logscale=TRUE))

htmltools::browsable(htmltools::tagList(graphs))

