library(tidyverse)
library(OECD)
library(WDI)
library(readxl)
library(tsibble)
library(tsbox)
# remotes::install_github("christophsax/tempdisagg")
# datapasta
library(tempdisagg)
library(ofce)
library(lubridate)
library(ggh4x)
library(showtext)

sysfonts::font_add_google('Nunito')
showtext::showtext_auto()

# la source des données de prix est la banque mondiale
# https://www.worldbank.org/en/research/commodity-markets
fs::dir_create("data")
download.file("https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx",
              destfile = "data/CMO-Historical-Data-Monthly.xlsx", mode="wb")

CMO <- readxl::read_xlsx("data/CMO-Historical-Data-Monthly.xlsx", sheet = "Monthly Prices", skip = 6) |> 
  rename(date = ...1) |> 
  mutate(date = paste0(substr(date, 1, 4), "-", substr(date, 6, 7), "-01")  |>  as.Date())  |> 
  gather(variable, value, -date) |> 
  mutate(value = as.numeric(value))  |> 
  filter(!is.na(value))

variable <- read_xlsx("data/CMO-Historical-Data-Monthly.xlsx", sheet = "Monthly Prices") |> 
  slice(4, 5, 6) |> 
  select(-1) |> 
  t() |> 
  as_tibble()  |> 
  select(variable = V3, Variable = V1, unit = V2)

ggplot(CMO)+geom_line(aes(x=date, y=value))+scale_y_log10()+facet_wrap(vars(variable))

# prix, données historiques (on utilisera l'INSEE pour les données récentes)
# 
oecd_q <- OECD::get_dataset(dataset="EO110_INTERNET", "USA+FRA+EA17.CPI+CPIH+PFDD+PCP+EXCHUD+EXCH.Q") |> 
  rename_with(tolower) |> 
  mutate(time = yq(time), v = as.numeric(obsvalue)) |>
  as_tsibble(index=time, key = c(location,variable)) 
pcp.fra <- oecd_q |> 
  filter(location=="FRA", variable=="PCP") |> 
  select(time, v) 
pcp.fra <- td(formula= pcp.fra ~ 1, to = "monthly", method="fast", conversion = "mean") |> 
  predict() |> 
  rename(pcp = value)
exch.fra <- oecd_q |> 
  filter(location=="FRA", variable=="EXCH") |> 
  select(time, v)
exch.fra <- td(formula= exch.fra ~ 1, to = "monthly", method="fast", conversion = "mean") |>
  predict() |>
  rename(exch = value)
# source INSEE
# 2. Comptes annuels, déflateur consommation des ménages
download.file("https://www.insee.fr/fr/statistiques/fichier/5354721/t_1103.xlsx", 
              dest = "data/t_1103.xlsx",
              mode="wb")
i_ca <- readxl::read_xlsx("data/t_1103.xlsx", skip=1) |>
  rename(var = `...1`) |>
  pivot_longer(cols=-var) |>
  drop_na(value) |>
  filter(var == "Ménages") |> 
  mutate(time = ym(str_c(name, "-01"))) |> 
  select(time, i_ca=value) |> 
  as_tsibble()
i_ca <- td(formula= i_ca ~ 1, to = "monthly", method="fast", conversion = "mean") |> 
  predict() |> 
  rename(dc = value)
# HICP eurostat
hicp <- sna_get("prc_hicp_midx", coicop="CP00", geo="FR", unit="I15") |> 
  full_join(pcp.fra, by="time") |> 
  full_join(i_ca, by="time") |> 
  full_join(exch.fra , by="time") |> 
  filter(time <= max(time[!is.na(I15_CP00)])) |> 
  arrange(desc(time)) |> 
  mutate(tmin = min(time[!is.na(I15_CP00)]),
         i = if_else(!is.na(I15_CP00),
                     I15_CP00, 
                     dc/dc[time==tmin] * I15_CP00[time==tmin])/100) |> 
  select(time, i, pcp, exch, dc)

# taux de change monthly
exch <- sna_get("ERT_BIL_EUR_M" |> tolower())

selected <- 
  tribble(
    ~variable, ~type,
    "ALUMINUM", "fer",
    "BEEF", "food",
    "COAL_AUS", "nrg",
    "COPPER", "fer",
    "CRUDE_BRENT", "nrg",
    "GOLD", "fer",
    "NGAS_EUR", "nrg",
    "WHEAT_US_HRW", "food",
    "PHOSROCK", "fer",
    "Zinc", "fer",
    "LEAD", "fer",
    "NICKEL", "fer",
    "MAIZE", "food",
    "PLATINIUM", "fer",
    "SILVER", "fer",
    "NGAS_US", "nrg",
    "SUNFLOWER_OIL", "food") |> 
  full_join(variable, by="variable") |> 
  arrange(type) |>  
  group_by(type) |> 
  mutate(typen = cur_group_id()) |> 
  ungroup()

CMOr <- CMO |> 
  rename(pcom = value, time = date) |>
  left_join(hicp, by="time") |> 
  group_by(variable) |> 
  mutate(
    pcom_real = pcom/exch/i,
    pcom_real = 100*pcom_real/mean(pcom_real[between(year(time), 2018, 2018)])) |> 
  arrange(desc(time)) |> 
  left_join(selected, by="variable") |> 
  mutate(
    variable = factor(variable, levels=selected$variable, labels=selected$Variable))

last_date <- max(CMOr$time)
ld_str <-  "{month(last_date, TRUE, FALSE, 'en_US.UTF-8')} {year(last_date)}" |> glue::glue()
eurdoll <- exch |> filter(currency == "USD", statinfo == "AVG") |> 
  filter(time == max(time)) |>
  pull(NAC)
cocol <- qs::qread("data/coicops.qs") |> pull(color, name = coicop)
CMOd  <- CMOr |> 
  group_by(variable) |>
  arrange(desc(time)) |> 
  summarize(dv = first(pcom),
            time = first(time),
            unit = first(unit),
            type  = first(type)) |> 
  ungroup() |> 
  mutate(
    unit = str_remove_all(unit, '[()]'),
    date = "{month(time, TRUE, FALSE, 'en_US.UTF-8')} {year(time)}" |> glue::glue(),
    dv = case_when(
      unit=="$/mmbtu" ~ dv/0.293297222222,
      TRUE ~ dv),
    unit = case_when(
      unit=="$/mmbtu" ~ "$/MWh",
      TRUE ~ unit),
    dv = dv/eurdoll,
    unit = str_replace(unit, "[$]", "€"),
    dv_str = prettyNum(dv, digits=3, format='fg', big.mark = ','),
    last_text = 
      "on {date}: {dv_str} {unit}" |> glue::glue())

gcom <- ggplot(CMOr |> filter(type!="unknown"))+
  geom_line(aes(x=time, y=pcom_real, color=type), show.legend = FALSE, size=0.25)+
  scale_y_log10(breaks = scales::log_breaks(10), minor_breaks = scales::log_breaks(16), guide = "axis_minor")+
  geom_text(data=CMOd |> filter(type!="unknown"), 
            aes(x=ymd("2022-01-01"), y=18, label=last_text, color= type), 
            hjust=1, vjust=1, size=2, show.legend=FALSE)+
  scale_color_manual(values = c(food = cocol[["CP01"]], nrg = cocol[["CP07"]], fer = cocol[["CP05"]]))+
  xlab(NULL)+ylab(NULL)+
  facet_wrap(vars(variable), ncol = 4)+
  labs(
    title = "Real price for a selection of commodities",
    subtitle = "France exchange and inflation rate (see note), 100 for year 2018, log scale (base 10)",
    caption = 
      str_c(
        "Last data point for {ld_str}" |> glue::glue(),
        "Sources : World Bank Commodity Markets (www.worldbank.org/en/research/commodity-markets)",
        "Eurostat HICP monthly since 1990, INSEE National accounts from 1960 to 1990 for price index for France",
        "OECD EO110 for exhange rate FRA/USD, EUR/USD", sep="\n"))+
  theme_ofce(base_size = 9, base_family = "Nunito")+
  scale_x_date(guide="axis_minor", date_minor_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.y = element_text(size=rel(0.75)))

goil <- ggplot(CMOr |> filter(variable=="Crude oil, Brent"))+
  geom_line(aes(x=time, y=pcom_real, color=type), show.legend = FALSE, size=0.25)+
  scale_y_log10(breaks = scales::log_breaks(10), minor_breaks = scales::log_breaks(16), guide = "axis_minor")+
  geom_text(data=CMOd |> filter(variable=="Crude oil, Brent"), 
            aes(x=ymd("2022-01-01"), y=18, label=last_text, color= type), 
            hjust=1, vjust=1, size=2, show.legend=FALSE)+
  scale_color_manual(values = c(food = cocol[["CP01"]], nrg = cocol[["CP07"]], fer = cocol[["CP05"]]))+
  xlab(NULL)+ylab(NULL)+
  facet_wrap(vars(variable), ncol = 4)+
  labs(
    title = "Real price for a selection of commodities",
    subtitle = "France exchange and inflation rate (see note), 100 for year 2018, log scale (base 10)",
    caption = 
      str_c(
        "Last data point for {ld_str}" |> glue::glue(),
        "Sources : World Bank Commodity Markets (www.worldbank.org/en/research/commodity-markets)",
        "Eurostat HICP monthly since 1990, INSEE National accounts from 1960 to 1990 for price index for France",
        "OECD EO110 for exhange rate FRA/USD, EUR/USD", sep="\n"))+
  theme_ofce(base_size = 9, base_family = "Nunito")+
  scale_x_date(guide="axis_minor", date_minor_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.y = element_text(size=rel(0.75)))+
  geom_hline(yintercept = 120+100, color= "orange", size = 0.5)
graph2png(goil)

graph2svg(gcom)
graph2png(gcom, width = 16, height = 23, dpi=600)
save(gcom, CMOr, CMOd, cocol, ld_str, file="data/gcom.rdata")
gcom2000 <- gcom %+% scale_x_date(limits = c(ymd("2000-01-01"), NA), guide="axis_minor", date_minor_breaks = "1 year", date_labels = "%Y")
graph2svg(gcom2000, ratio = 16/9, width = 16)
graph2png(gcom2000, width = 16, height = 23, dpi=600)
