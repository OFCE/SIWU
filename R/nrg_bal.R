library(eurostat)
library(tidyverse)
library(gt)
library(ofce)

#setwd("~/Dropbox/github/SIWU")

eu <- eurostat::eu_countries |> filter(code!="UK")

nrg_raw <- sna_get("nrg_bal_s", geo = eu$code)
# nrg_raw2 <- sna_get("nrg_bal_c", geo = eu$code)
# 
# sna_show(nrg_raw, lang="en")
# sna_show(nrg_raw2, lang="en")

pop_a <- sna_get("demo_gind", indic_de = "AVG", name="pop", geo=eu$code) |> 
  group_by(geo) |> filter(time == max(time))

nrg_yield <- tribble(~vector, ~coef,
                     "nuke", 0.333,
                     "coal", 1,
                     "gases", 1,
                     "oil", 1,
                     "rnw",  1,
                     "other", 1)

nrg <- nrg_raw |>
  filter(nrg_bal %in% c("GIC", "NRG_E", "TO_EHG_E", "TI_EHG_E", "NRG_EHG_E", "FC_E", "FC_OTH_E", "FC_OTH_HH_E", "FC_TRA_E", "FC_IND_E", "FC_NE")) |> 
  filter(siec != "E7000", siec !="H8000", siec != "TOTAL") |> 
  mutate(vector = case_when(
    siec%in%c("G3000", "C0350-0370") ~ "gases",
    siec%in%c("O4000XBIO", "S2000") ~ "oil", 
    siec%in%c("C0000X0350-0370") ~"coal",
    siec%in%c("RA000") ~"rnw",
    siec%in%c("N900H") ~"nuke",
    TRUE ~ "other")) |> 
  left_join(nrg_yield, by="vector") |> 
  group_by(vector, coef, unit, geo, time, nrg_bal) |> 
  summarize(values = sum(values, na.rm = TRUE)) |> 
  ungroup() |> 
  pivot_wider(id_cols = c(vector, unit, geo, time, coef),
              names_from = nrg_bal, values_from = values) |> 
  transmute(
    vector, unit, geo, time,
    total = TI_EHG_E*coef+FC_E+FC_NE,
    electricity = TI_EHG_E*coef,
    business = FC_IND_E + FC_OTH_E - FC_OTH_HH_E,
    residential = FC_OTH_E,
    transport = FC_TRA_E) |> 
  group_by(geo, unit, time) |> 
  mutate( across(c(total, electricity, business, residential, transport), ~.x/sum(.x), .names="{col}_ppc")) |> 
  left_join(pop_a |> select(-time), by="geo") |> 
  mutate(across(c(total, electricity, business, residential, transport), ~.x/pop, .names="{col}_pc")) |> 
  group_by(geo, unit, vector) |> 
  filter(unit=="GWH", time==max(time))|> 
  mutate(across(ends_with("_pc"), ~.x*1000)) |> 
  ungroup()

nrg |> select(geo, vector, ends_with("_pc")) |> 
  group_by(geo) |>
  gt() |> 
  fmt_number(where(is.numeric), decimals = 1) |> 
  table_ofce()

(tab1 <- nrg |> select(vector, ends_with("_pc"), geo) |> 
    pivot_longer(cols = ends_with("_pc"), names_to = "sector") |> 
    mutate(sector = str_remove(sector, "_pc")) |> 
    filter(sector=="total"|(sector %in% c("residential", "electricity") & vector=="gases")) |> 
    pivot_wider(names_from = c(vector, sector), values_from = value) |> 
    rename_with(~str_remove(.x,"_total")) |> 
    mutate(total = coal+gases+nuke+oil+other+rnw) |> 
    arrange(desc(gases)) |> 
    left_join(eu, by = c("geo"="code")) |>
    select(-geo, -label) |> 
    relocate(name, starts_with("gas"), oil, nuke, coal, rnw, other, total) |> 
    gt() |> 
    fmt_number(where(is.numeric), decimals = 1) |> 
    tab_spanner(label = "Gas", columns = c(gases, gases_electricity, gases_residential)) |> 
    cols_label(name="", gases = "total gas", gases_electricity = md("*of which* electricity&heat"),
               gases_residential = md("*of which* households"),
               oil = "Oil", nuke = "Nuclear", coal = "Coal", other= "Others", rnw="Renewables",
               total = "Total") |> 
    gt::tab_header(title = "Energy final consumption estimation by vector per capita", 
                   subtitle = md("*in megaWatt-hour per person*")) |> 
    tab_footnote("Gases (natural and produced, except biogases) input for electricity production, housing, industrial processes, commercial or public building (G3000&C0350-0370)", 
                 cells_column_labels(gases)) |> 
    tab_footnote("Oil and oil shale and oil sands (O4000XBIO&S2000)", 
                 cells_column_labels(oil)) |> 
    tab_footnote("Electricity and heat generation from nuclear production output (yield=33%) (N900H)", cells_column_labels(nuke)) |> 
    tab_footnote("Solid fossil fuels (C0000X0350-0370)", 
                 cells_column_labels(coal)) |>
    tab_footnote("Renewables and biofuels (RA000)", 
                 cells_column_labels(rnw)) |> 
    tab_footnote("Heat, Nuclear heat not used for electricity, peat, and non renewable waste", 
                 cells_column_labels(other)) |> 
    tab_footnote("Total final consumption", 
                 cells_column_labels(total)) |> 
    tab_source_note("Sources: Eurostat nrg_bal_s and demo_gind, code at github.com/OFCE/SIWU") |> 
    cols_width(starts_with("gas") ~ px(70),
               c(oil, nuke, coal, rnw, other, total) ~ pct(60),
               total ~ pct(70),
               name ~pct(80)) |> 
    gt::opt_row_striping() |> 
    table_ofce() |> 
    tab_options(table.width = px(650),
                table.font.size = 9) ) 

gtsave(tab1, "svg/tab1.png")
