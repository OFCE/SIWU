library(tidyverse)
library(eurostat)
# devtools::install_github("ofce/ofce")
library(ofce)
library(lubridate)
library(multidplyr)
list_eurostat_datasets <- c("prc_hicp_midx", "prc_hicp_inw",
                            "hbs_str_t223", "hbs_str_t226",
                            "icw_res_02", "hbs_exp_t121", "hbs_str_t211")
eucountries <- setdiff(eurostat::eu_countries$code, "UK")
# sna_clear_cache()
cache_state <- sna_check_cache()

walk(list_eurostat_datasets, ~{
  assign(.x, sna_get(.x), envir = .GlobalEnv)
})

# on donne un niveau à coicop et on crée CP00
# vérifier le panachage des années

hbs_str_t223m <- hbs_str_t223 |>
  mutate(coicop_digit = stringr::str_length(coicop)-3) |> 
  filter(str_starts(quantile, "QUINTILE")) |> 
  group_by(quantile, coicop, geo) |> 
  filter(year(time)==max(year(time))) |>
  ungroup()

hbs_str_t223l0 <- hbs_str_t223m |> 
  filter(coicop_digit==1) |>
  group_by(geo, quantile, time) |>
  summarize() |> 
  mutate(coicop_digit=0, coicop="CP00", PM = 1000) |> 
  ungroup()

hbs_str_t223m <- bind_rows(hbs_str_t223m, hbs_str_t223l0)

hbs_str_t211l3 <-  hbs_str_t211 |>
  mutate(coicop_digit = stringr::str_length(coicop)-3) |> 
  filter(coicop_digit==3) |> 
  mutate(coicop2= str_sub(coicop, 1, 5)) |> 
  group_by(coicop, geo) |> 
  filter(year(time)==max(year(time))) |> 
  ungroup() |> 
  arrange(geo)

#on fabrique le niveau 3 par quintile en appliquant les proportions du l2 par quintile aux coefficients l3 agrégés
hbs_str_t223l3 <- left_join(
  hbs_str_t223m |> filter(coicop_digit==2) |> mutate(coicop2=coicop),
  hbs_str_t211l3, by=c("coicop2", "geo"),
  suffix = c("", ".3")) |> 
  arrange(geo, coicop, quantile) |> 
  group_by(quantile, geo, coicop) |> 
  filter(coicop_digit.3 == 3) |> 
  mutate(PM.q3 = PM.3 / sum(PM.3) * first(PM)) |> 
  transmute(geo, coicop=coicop.3, time, time.3, coicop_digit=3, quantile, PM=PM.q3)

# le fichier modifié avec le niveau 3
hbs_str_t223m <- bind_rows(
  hbs_str_t223m |> filter(coicop_digit!=3),
  hbs_str_t223l3) 

all_coicop <- hbs_str_t223 |>
  distinct(coicop) |>
  mutate(coicop_digit=str_length(coicop)-3) |>
  filter(coicop_digit==3) |> 
  mutate(coicop1 = str_sub(coicop,1,4),
         coicop2 = str_sub(coicop,1,5),
         coicop3 = coicop,
         l1 = label_eurostat(coicop1,dic="coicop"),
         l2 = label_eurostat(coicop2,dic="coicop"),
         l3 = label_eurostat(coicop3,dic="coicop")) |> 
  select(-coicop, -coicop_digit)

housing <- all_coicop |> filter(coicop1=="CP04") |> pull(coicop2) |> unique()
# On caclule des part de quintiles dans le revenu total

inc_share <- icw_res_02 |>
  filter(str_starts(quant_inc, "QU"), quant_expn=="TOTAL", quant_wlth=="TOTAL", statinfo=="AVG", indic_il=="INC_DISP") |>
  group_by(geo, time) |>
  mutate(S = EUR/sum(EUR)) |>
  group_by(geo) |> 
  filter(year(time)==max(year(time))) |> 
  arrange(geo)

# On reprend les pondérations HICP par pays

hbs_hicp <- left_join(hbs_str_t223m,
                      hbs_str_t211 |> group_by(geo, coicop) |> filter(year(time)==max(year(time))), 
                      by=c("geo", "coicop"), suffix = c("", ".agg")) |>
  arrange(geo, desc(time), coicop, quantile) |> 
  left_join(prc_hicp_inw |> filter(time=="2022-01-01") |> select(-time) |> rename(PM=values),
            by=c("coicop", "geo"), suffix = c("", ".inw")) |> 
  filter(coicop!="CP00") |> 
  bind_rows(hbs_str_t223l0 |> mutate(PM.agg = 1000, PM.inw=1000)) |> 
  mutate(coicop_digit = str_length(coicop)-3,
         coicop_digit = if_else(coicop=="CP00", 0, coicop_digit),
         PM.inw = if_else(is.na(PM.inw), 0, PM.inw),
         PM.origin = PM,
         PM = PM.origin/PM.agg * PM.inw,
         PM = if_else(coicop=="CP00", 1000, PM)) 

coicop_colors <- tibble(
  label = c("Food and non-alcoholic beverages", "Alcoholic beverages and Tobacco",
            "Clothing and footwear", "Housing, water, gas, electricity and other fuels", 
            "Furnishing, household equipment and routine maintenance of the house",
            "Health", "Transport", "Communications", "Recreation and culture",
            "Education", "Restaurants and hotels", "Miscellaneous, goods and services"),
  short_label = c("Food", "Alcool and Tobacco",
                  "Clothing and footwear", "Sheltering", 
                  "Home equipment",
                  "Health", "Transport", "Communications", "Recreation and culture",
                  "Education", "Restaurants and hotels", "Others"),
  coicop = c("CP01", "CP02", "CP03", "CP04", "CP05", "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12"),
  color =    c("#d9524f", "#800020", "#FDA300", "#768299", "#947b4b", "#16c2d5", "#4a575d", "#3c1361", "#663a82", "#b491c8", "#bebada", "#babebe"),
  color_pm = c("#d9524f", "#800020", "#F9B000", "#768299", "#998D76", "#16c2d5", "#466963", "#3c1361", "#663a82", "#b491c8", "#bebada", "#C0087F"))

# Change in Prices: geo, coicop ---------
safe_stl <- purrr::safely(stl)
deseason <- function(data, start) {
  nas <- is.na(data)
  ts <- ts(data[!nas], start=c(lubridate::year(first(start)), lubridate::month(first(start))), deltat = 1/12)
  dts <- safe_stl(ts, 7)
  res <- 0
  res[nas] <- NA
  if(is.null(dts$error)) {
    dts2 <- dts$result$time.series[, "seasonal"]
    if(length(dts2)>0)
      res[!nas] <- dts2
  }
  return(res)
}

cluster <- multidplyr::new_cluster(8)
inf_1 <- prc_hicp_midx  |>
  filter(unit == "I15", str_starts(coicop, "CP"), geo %in% eucountries) |> # un filtre basique
  group_by(coicop, geo, unit) |> 
  arrange(time) |>
  mutate(values=if_else(values==0, NA_real_, values)) |> # on traite ici des valeurs manquantes pour la Grèce CP0732 et l'Irlande CP0441 
  mutate(values = zoo::na.locf(values, na.rm=FALSE)) |> # en utilisant les dernières valeurs observées
  mutate(coicop_digit = str_length(coicop)-3,
         coicop_digit = if_else(coicop=="CP00", 0, coicop_digit)) |> 
  filter(coicop_digit<=3) |> # on ne garde pas coicop l4
  group_by(unit, coicop, geo) |> 
  filter(max(time)>= ymd("2022-02-01")) |> # on enlève les données non disponibles en 2022
  group_by(geo, unit, coicop) |> 
  arrange(time) |> 
  #multidplyr::partition(cluster) |> # pour utiliser le multicore
  mutate(
    start = min(time),
    lp_nsa = log(values),
    # la désaisonalisation avec stl (rapide, ne traite pas les outliers)
    # avec seasonal on obtient à peu près la même chose, mais c'est très lent
    lp_sa = deseason(lp_nsa, start),
    price_sa = exp(lp_nsa-lp_sa), 
    price_nsa = values) |> 
  collect() |>
  ungroup() |> 
  complete(unit, coicop, geo, time, fill=list(lp_nsa=NA, price_sa=NA, price_nsa=NA, price_sa=NA)) |> 
  mutate(coicop_digit = str_length(coicop)-3,
         coicop_digit = if_else(coicop=="CP00", 0, coicop_digit)) |> 
  group_by(unit, coicop, geo, coicop_digit) |>
  arrange(unit, coicop, geo)

# on caclule la matrice des effets et des effets cumulés moyens
future::plan("multisession", workers = 8)
start <- ym("2020 01")
end <- max(inf_1$time)
inff <- furrr::future_map_dfr(1:(lubridate::interval(start,end)%/%months(1)), function(b) {
  base <- start + lubridate:::months.numeric(b-1)
  if(base==end) return(NULL)
  dmax <- lubridate::interval(base, end) %/% lubridate:::months.numeric(1)
  dates <- base + lubridate:::months.numeric(1:dmax)
  inf_1 |> 
    summarize(
      d = 1:dmax,
      i = purrr::map_dbl(dates, ~price_sa[time==.x]/price_sa[time==base]-1),
      i_nsa = purrr::map_dbl(dates, ~price_nsa[time==.x]/price_nsa[time==base]-1),
      # remplacer cumsum par cumprod(1+i)^(1/d)-1
      # ou les indices, hein?
      imd = cumsum(i)/d,
      imd_nsa = cumsum(i_nsa)/d,
      ref = base, 
      .groups = "drop") |>
    unnest(c(i, i_nsa))
})

# on joine avec les poids HICP
inflation_g_c <- inff |> 
  left_join(
    prc_hicp_inw |> filter(time=="2022-01-01") |> select(coicop, geo, pm = values),
    by=c("coicop", "geo")) |> 
  group_by(ref, d, geo, unit, coicop_digit) |>
  mutate( pm = pm / sum(pm)) |> 
  ungroup() |>  
  mutate(coicop1 = str_sub(coicop, 1,4),
         coicop2 = str_sub(coicop, 1,5)) |> 
  left_join(eurostat::eu_countries |> select(geo=code, geo_name = name), by="geo")

# save(inflation_g_c, file="data/inflation_g_c.rdata")

# quintile_shares: geo, quantile, coicop  ---------

quantileshares_g_q_c <- hbs_hicp %>%
  filter(quantile!="UNK") %>%
  group_by(geo, quantile, coicop) |> 
  filter(time == max(time)) |> 
  ungroup() |> 
  transmute(geo, quantile, coicop, quantileshares_g_q_c = PM/1000, year=year(time))

# conso_income: geo, quantile -------------

cons_income_g_q <- icw_res_02 %>%
  filter(quant_inc %in% c("QU1", "QU2", "QU3", "QU4", "QU5"),
         quant_expn == "TOTAL",
         quant_wlth == "TOTAL",
         statinfo == "AVG",
         indic_il %in% c("INC_DISP", "EXPN_CONS")) %>%
  group_by(geo, quant_inc, indic_il) |> 
  filter(time==max(time)) |> 
  ungroup() |> 
  spread(indic_il, EUR) %>%
  mutate(quantile = gsub("QU", "QUINTILE", quant_inc),
         year = year(time))  %>%
  select(geo, quantile, EXPNCONS_g_q = EXPN_CONS, INCDISP_g_q = INC_DISP, year)

# Final: outcome_g_q_c is shock in g, q, c ---------

# inflation_g_c_w <-  inflation_g_c |> 
#   mutate(name = case_when(
#     d==0&ref=="septembre 2021" ~ "sept21",
#     d==0&ref=="février 2022" ~ "wiu",
#     ref=="septembre 2021" ~ str_c("sept21", scales::label_number(style_positive = "plus", style_negative = "minus")(d)),
#     ref=="février 2022" ~ str_c("wiu", scales::label_number(style_positive = "plus", style_negative = "minus")(d)))) |> 
#   pivot_wider(id_cols=c(unit, coicop, geo), names_from = name, values_from=i)

# Attention on garde maintenant outcome en format long (ref/d)

outcome_g_q_c <- quantileshares_g_q_c |> 
  left_join(cons_income_g_q, by=c("geo", "quantile")) |> 
  left_join(inflation_g_c, by=c("geo", "coicop")) |> 
  mutate(
    quantile = factor(quantile, labels = str_c("Q", 1:5)),
    qs = quantileshares_g_q_c,
    outcome = qs * imd * EXPNCONS_g_q/INCDISP_g_q) |>  
  # drop_na(outcome) |> 
  select(-EXPNCONS_g_q, -INCDISP_g_q, -quantileshares_g_q_c)

outcome_l0s <- outcome_g_q_c |> 
  filter(coicop_digit==1) |> 
  group_by(geo, quantile, ref, d) |> 
  summarize(outcome = sum(outcome),  qs = 1) |>  
  mutate(coicop = "CP00s", coicop_digit = 0)

outcome_g_q_c <- bind_rows(outcome_g_q_c, outcome_l0s) |>
  arrange(geo, quantile, coicop) |> 
  transmute(geo, quantile, coicop, coicop_digit, i, d, ref, pm, qs, coicop1, coicop2, geo_name, outcome)

# save(outcome_g_q_c, file = "data/outcome_g_q_c.rdata")
# save(all_coicop, coicop_colors, file="data/coicop_info.rdata")

coicops <- coicop_colors |> 
  mutate(
    short_label2=str_c(coicop_colors$coicop,": ", coicop_colors$short_label))

# on trie les pays par ordre de consommation pps décroissante
conso_ppp <- sna_get("nama_10_pc", unit="CP_PPS_EU27_2020_HAB", na_item = "P41", name="indicateur") 

countries <- conso_ppp |>
  filter(time=="2021-01-01"& geo %in% eucountries) |> 
  arrange(desc(indicateur)) |> 
  pull(geo)
countries_l <- (eu_countries |> pull(name, name=code))[countries]

outcome_sorted <-  outcome_g_q_c |> 
  filter(geo %in% eucountries) |> 
  left_join(coicop_colors, by="coicop") |> 
  mutate(geo_f = factor(geo, levels = countries, labels = countries_l, ordered=TRUE),
         coicop1 = str_sub(coicop, 1,4),
         time = ref + lubridate:::months.numeric(d),
         coicop = factor(coicop),
         coicop1 = factor(coicop1),
         coicop2 = factor(coicop2),
         geo=factor(geo)) |> 
  select(-geo_name, -label) |> 
  ungroup() |> 
  # du coup on vire les coicop non présent dans l'hicp comme CP042 et sans doute d'autres
  drop_na(outcome)

inf_sorted <-  inflation_g_c |> 
  filter(geo %in% eucountries) |> 
  left_join(coicop_colors, by="coicop") |> 
  mutate(geo_f = factor(geo, levels = countries, labels = countries_l, ordered=TRUE)) |> 
  group_by(geo, d, ref, coicop_digit) |> 
  arrange(geo, coicop_digit, ref, d, desc(imd)) |> 
  mutate(xmax=cumsum(pm),
         xmin = lag(xmax, default=0),
         ri = rank(-imd),
         ripm = rank(-imd*pm),
         label_i = if_else(imd>=0.1 & ri<=5, coicop, ""),
         label_ipm = if_else(imd*pm>=0.001 & ripm<=5, coicop, ""),
         time = ref + lubridate:::months.numeric(d),
         coicop = factor(coicop),
         coicop1 = factor(coicop1),
         coicop2 = factor(coicop2),
         geo2c = countrycode::countrycode(geo, "eurostat", "iso2c") |> tolower() |> factor()) |>
  select(-unit, -geo_name, -color_pm) |> 
  ungroup() |> 
  drop_na(imd)

# ces fichiers sont utilisés ensuite dans graphiques.r et dans l'application shiny SIWU
# on peut vérifier les temps de chargement
# bench::mark()
fs::dir_create("data")
qs::qsave(inf_sorted, file = "data/inf_sorted.qs", preset="high", nthreads = 4)
qs::qsave(coicops, file = "data/coicops.qs", preset="fast", nthreads = 4)
qs::qsave(all_coicop, file = "data/all_coicop.qs", preset="fast", nthreads = 4)
qs::qsave(outcome_sorted, file = "data/outcome_sorted.qs", preset="high", nthreads = 4)

bench::mark(inf = qs::qread('data/inf_sorted.qs', nthreads=4),
            outcome = qs::qread('data/outcome_sorted.qs', nthreads=4), check=FALSE)

