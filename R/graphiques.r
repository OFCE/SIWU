
if(!"ggflags" %in% installed.packages()){devtools::install_github('rensa/ggflags')}

library(tidyverse)
library(eurostat)
library(ofce)
library(lubridate)
library(gganimate)
library(ggforce)
library(RColorBrewer)
library(colorspace)
library(ggflags)
library(ragg)
library(paletteer)
library(glue)
library(showtext)

sysfonts::font_add_google('Nunito')

outcome_sorted <- qs::qread("data/outcome_sorted.qs")
inf_sorted <- qs::qread("data/inf_sorted.qs")
coicops <- qs::qread("data/coicops.qs", nthreads = 4) |> 
  mutate(short_label2 = str_remove(short_label2, "CP"))
all_coicop <- qs::qread("data/all_coicop.qs", nthreads = 4)
eucountries <- setdiff(eurostat::eu_countries$code, "UK")

png_w <- 15
png_h <- 20

dmax <- inf_sorted |>
  group_by(geo, ref, coicop_digit, d) |> 
  mutate(n = n()) |> 
  group_by(geo, ref, coicop_digit) |> 
  mutate(dmax = max(d[n==max(n)])) |> 
  select(geo, ref, coicop_digit, coicop, d, dmax, time) |> 
  ungroup()
dmaxx <- dmax |>
  group_by(geo, ref, coicop_digit) |> 
  summarize(dmax = min(dmax)) |> 
  ungroup() |> 
  mutate(to_date = "{month(ref+months(dmax),TRUE,TRUE,'en_US.UTF-8')}. {year(ref+months(dmax))}" |> glue())

dmax_o <- outcome_sorted |>
  group_by(geo, ref, coicop_digit, d) |> 
  mutate(n = n()) |> 
  group_by(geo, ref, coicop_digit) |> 
  mutate(dmax = max(d[n==max(n)])) |> 
  select(geo, ref, coicop_digit, coicop, d, dmax, time) |> 
  ungroup() 
dmaxx_o <- dmax_o |>
  group_by(geo, ref, coicop_digit) |> 
  summarize(dmax = min(dmax)) |> 
  ungroup() |> 
  mutate(to_date = "{month(ref+months(dmax),TRUE,TRUE,'en_US.UTF-8')}. {year(ref+months(dmax))}" |> glue())

since_wiu <- ym("2022 02")
str_wiu <- "Feb. 2022"
since_1y <- max(inf_sorted |> filter(coicop_digit==1) |> pull(time))-years(1)
str_1y <- glue("{month(since_1y,TRUE,TRUE,'en_US.UTF-8')}. {year(since_1y)}")

to_date_1y <- max(inf_sorted |> filter(d==dmax$dmax,ref==since_1y, coicop_digit==1) |> pull(time))
to_date_wiu <- max(inf_sorted |> filter(d==dmax$dmax,ref==since_wiu, coicop_digit==1) |> pull(time))
to_date <- to_date_wiu
str_to <-  glue("{month(to_date,TRUE,TRUE,'en_US.UTF-8')}. {year(to_date)}")

# graphiques de diffusion par histogramme --------------------
gdis_feb <- map(1:3, ~{
  gg <- ggplot(inf_sorted |> filter(d==dmax$dmax, coicop_digit==3, ref==since_wiu), 
               aes(x=imd*pm)) +
    geom_vline(xintercept = 0, size=unit(0.1, "pt"), linetype="dashed") +
    geom_histogram(aes(weight=pm, fill = coicop1, group=coicop), col="white", size=0.05, binwidth=0.00025, show.legend=TRUE)+
    geom_hline(yintercept = 0, size=unit(0.1, "pt"), col="gray20") +
    geom_text(
      data = filter(dmaxx, coicop_digit==3, ref==since_wiu) , 
      aes(x=Inf, y=Inf, label="from {str_wiu} to {to_date}" |> glue()),
      hjust=1, vjust=1, size=rel(1)) +
    scale_x_continuous(labels = scales::label_percent(.1), limits = c(-0.005, 0.015), 
                       name = "Contribution to inflation", breaks = scales::breaks_width(0.005))+
    scale_y_continuous(name="weight in HICP", labels = scales::label_percent(1))+
    scale_fill_manual(labels = coicops |> pull(short_label2),
                      values = coicops |> pull(color, name = coicop),
                      aesthetics = c("color", "fill"), name="12 products classification") +
    theme_ofce(base_size = 9) +
    theme(plot.margin = margin(12,12,12,12,"pt"), panel.spacing = unit(6,"pt"),
          legend.position = "right", legend.key.size = unit(9, "pt"), legend.text = element_text(size=6),
          axis.text = element_text(size = rel(0.5))) +
    labs(title = "Contribution to inflation distribution",
         subtitle = "since Feb. 2022, subfig {.x}." |> glue::glue(),
         caption = str_c("Reading note: at 0 on the x axis are represented 3 digit COICOP (colored according to 1 digit COICOP)",
                         "with no increase in price, on the right side of the x axis are categories for which the contribtion",
                         "to inflation is positive, height of the bar is the weight in HICP, on the y axis, stacked if necessary.",
                         "Source: Eurostat prc_hicp_midx, prc_hicp_inw", sep="\n"))+
    ggrepel::geom_text_repel(aes(label=label_ipm, color = coicop1, y=pm),
                             size=rel(1), segment.size=0.1, min.segment.length = 0,
                             max.overlaps = 50, ylim=c(0.05, NA))+
    ggforce::facet_wrap_paginate(vars(geo_f), nrow=3, ncol=3, page = .x)
  graph2png(gg, file="inflation_coicop_wiu_{.x}" |> glue::glue(), width = png_w, height = png_h, dpi=600)})

gdis_1y <- map(1:3, ~{
  gg <- ggplot(inf_sorted |> filter(d==dmax$dmax, coicop_digit==3, ref==since_1y), 
               aes(x=imd*pm)) +
    geom_vline(xintercept = 0, size=unit(0.1, "pt"), linetype="dotted") +
    geom_histogram(aes(weight=pm, fill = coicop1, group=coicop), col="white", size=0.1, binwidth=0.00025, show.legend=TRUE)+
    geom_hline(yintercept = 0, size=unit(0.1, "pt"), col="gray20") +
    geom_text(
      data = filter(dmaxx, coicop_digit==3, ref==since_1y) , 
      aes(x=Inf, y=Inf, label="from {str_wiu} to {to_date}" |> glue()),
      hjust=1, vjust=1, size=rel(1)) +
    scale_x_continuous(labels = scales::label_percent(.1), limits = c(-0.005, 0.015), 
                       name = "Contribution to inflation", breaks = scales::breaks_width(0.005))+
    scale_y_continuous(name="weight in HICP", labels = scales::label_percent(1))+
    scale_fill_manual(labels = coicops |> pull(short_label2),
                      values = coicops |> pull(color, name = coicop),
                      aesthetics = c("color", "fill"), name="12 products classification") +
    theme_ofce(base_size = 9) +
    theme(plot.margin = margin(12,12,12,12,"pt"), panel.spacing = unit(6,"pt"),
          legend.position = "right", legend.key.size = unit(9, "pt"), legend.text = element_text(size=6),
          axis.text = element_text(size = rel(0.5))) +
    labs(title = "Contribution to inflation distribution per COICOP",
         subtitle = "from {str_1y} to {str_to}, subfig {.x}." |> glue::glue(),
         caption = str_c("Reading note: at 0 on the x axis are represented 3 digit COICOP (colored according to 1 digit COICOP)",
                         "with no increase in price, on the right side of the x axis are categories for which the contribtion",
                         "to inflation is positive, height of the bar is the weight in HICP, on the y axis, stacked if necessary.",
                         "Source: Eurostat prc_hicp_midx, prc_hicp_inw", sep="\n"))+
    ggrepel::geom_text_repel(aes(label=label_ipm, color = coicop1, y=pm),
                             size=rel(1), segment.size=0.1, min.segment.length = 0,
                             max.overlaps = 10, ylim=c(0.05, NA))+
    ggforce::facet_wrap_paginate(vars(geo_f), nrow=3, ncol=3, page = .x)
  graph2png(gg, file="inflation_coicop_1y_{.x}" |> glue::glue(), width = png_w, height = png_h, dpi=600)})

# contribution par coicop --------------------
g_1y <- map(1:3, ~{
  gg <- ggplot(inf_sorted |> filter(d==dmax$dmax, ref==since_1y)) +
    geom_rect(data = ~filter(.x, coicop_digit==0),
              aes(xmin=xmin, xmax=xmax, ymin=0, ymax=imd), 
              fill=NA, col="black", size=0.1, show.legend=FALSE)+
    geom_rect(data = ~filter(.x, coicop_digit==3),
              aes(xmin=xmin, xmax=xmax, ymin=0, ymax=imd, fill = coicop1), 
              size=0.01, col="white", show.legend=TRUE)+
    theme_ofce(base_size = 9)+
    theme(plot.margin = margin(12,12,12,12,"pt"), panel.spacing = unit(6,"pt"),
          legend.position = "bottom", legend.key.size = unit(8, "pt"), legend.text = element_text(size=rel(0.7)),
          axis.text = element_text(size = rel(0.5)), axis.line = element_line(size=unit(0.1, "pt")),
          axis.ticks.length = unit(0.5,"pt"), axis.ticks = element_line(size=unit(0.1, "pt")))+
    labs(title = "Price increases since {str_1y}" |> glue(),
         subtitle = "subfig {.x}." |> glue(),
         caption = str_c("Reading note: each 3 digit COICOP is represented in decreasing inflation order",
                         "Height of the bar is the increase in prices, width if the weight in HICP, hence the area is the contribution",
                         "Source: Eurostat prc_hicp_midx, prc_hicp_inw", sep="\n"))+
    scale_color_manual(labels = coicops$short_label2,
                       values = coicops |> pull(color, name = coicop), 
                       aesthetics = c("color","fill"), 
                       name=NULL) +
    guides(color=guide_legend(ncol=3, nrow=4))+
    scale_x_continuous(limits=c(0,1), name="Cumulative weight in HICP", labels=scales::label_percent(1))+
    scale_y_continuous(name="Increase in price since {str_1y}" |> glue(), oob=scales::oob_squish) +
    ggrepel::geom_text_repel(data=~filter(.x, coicop_digit==3),
                             aes(label=label_i, color = coicop1, x=(xmin+xmax)/2, y=0), 
                             size=rel(1), segment.size=0.1, min.segment.length = 0, 
                             max.overlaps = 100, ylim=c(NA, 0), show.legend=FALSE)+ 
    facet_wrap_paginate(vars(geo_f),  nrow=3, ncol=3, page = .x)
  graph2png(gg, file="depuis_1y_{.x}" |> glue(), width = png_w, height = png_h, dpi=600)})

g_wiu <- map(1:3, ~{
  gg <- ggplot(inf_sorted |> filter(d==dmax$dmax, ref==since_wiu)) +
    geom_rect(data = ~filter(.x, coicop_digit==0),
              aes(xmin=xmin, xmax=xmax, ymin=0, ymax=imd), 
              fill=NA, col="black", size=0.1,  show.legend=FALSE)+
    geom_rect(data = ~filter(.x, coicop_digit==3),
              aes(xmin=xmin, xmax=xmax, ymin=0, ymax=imd, fill = coicop1), 
              size=0.01, col="white", show.legend=TRUE)+
    theme_ofce(base_size = 9, base_family = "Nunito")+
    theme(plot.margin = margin(12,12,12,12,"pt"), panel.spacing = unit(6,"pt"),
          legend.position = "bottom", legend.key.size = unit(8, "pt"), legend.text = element_text(size=rel(0.7)),
          axis.text = element_text(size = rel(0.5)), axis.line = element_line(size=unit(0.1, "pt")),
          axis.ticks.length = unit(0.5,"pt"), axis.ticks = element_line(size=unit(0.1, "pt")))+
    guides(color=guide_legend(ncol=3, nrow=4))+
    scale_color_manual(labels = coicops$short_label2,
                       values = coicops |> pull(color, name = coicop), 
                       aesthetics = c("color","fill"), 
                       name=NULL) +
    labs(title = str_c("Price increases since war in Ukraine", 
                       "from February to {str_to}" |> glue(), sep="\n"),
         subtitle = "subfig {.x}." |> glue::glue(),
         caption = str_c("Reading note: each 3 digit COICOP is represented in decreasing inflation order",
                         "Height of the bar is the increase in prices, width if the weight in HICP, hence the area is the contribution",
                         "Source: Eurostat prc_hicp_midx, prc_hicp_inw", sep="\n"))+
    scale_x_continuous(limits=c(0,1),name="Cumulative weight in HICP", labels=scales::label_percent(1))+
    scale_y_continuous(limits=c(-0.1,0.45), name="Increase in price since Feb. 22", oob=scales::oob_squish) +
    ggrepel::geom_text_repel(
      aes(label=label_i, color = coicop1, x=(xmin+xmax)/2, y=0), 
      size=4/.pt, segment.size=0.1, min.segment.length = 0, 
      max.overlaps = 100, ylim=c(NA, 0), show.legend=FALSE)+ 
    facet_wrap_paginate(vars(geo_f),  nrow=3, ncol=3, page = .x)
  graph2png(gg, file="depuis_wiu_{.x}" |> glue::glue(), width = png_w, height = png_h, dpi=600)
  graph2png(gg, file="depuis_wiu_(pres)_{.x}" |> glue::glue(), width = png_w, height = png_h, dpi=600)})


g_wiu_NLD <- ggplot(inf_sorted |> filter(d==dmax$dmax, ref==since_wiu, geo=="NL")) +
  geom_rect(data = ~filter(.x, coicop_digit==0),
            aes(xmin=xmin, xmax=xmax, ymin=0, ymax=imd), 
            fill=NA, col="black", size=0.1,  show.legend=FALSE)+
  geom_rect(data = ~filter(.x, coicop_digit==3),
            aes(xmin=xmin, xmax=xmax, ymin=0, ymax=imd, fill = coicop1), 
            size=0.01, col="white", show.legend=TRUE)+
  theme_ofce(base_size = 9)+
  theme(legend.position = "bottom", legend.key.size = unit(8, "pt"))+
  guides(color=guide_legend(ncol=3, nrow=4))+
  scale_color_manual(labels = coicops$short_label2,
                     values = coicops |> pull(color, name = coicop), 
                     aesthetics = c("color","fill"), 
                     name=NULL) +
  labs(title = str_c("Price increases since war in Ukraine for The Netherlands", 
                     "from February to {str_to}" |> glue(), sep="\n"),
       subtitle = "subfig NL." |> glue::glue(),
       caption = str_c("Reading note: each 3 digit COICOP is represented in decreasing inflation order",
                       "Height of the bar is the increase in prices, width if the weight in HICP, hence the area is the contribution",
                       "Source: Eurostat prc_hicp_midx, prc_hicp_inw", sep="\n"))+
  scale_x_continuous(limits=c(0,1),name="Cumulative weight in HICP", labels=scales::label_percent(1))+
  scale_y_continuous(limits=c(-0.1,0.45), name="Increase in price since Feb. 22", oob=scales::oob_squish) +
  ggrepel::geom_text_repel(
    data = ~filter(.x, coicop_digit==3),
    aes(label=label_i, color = coicop1, x=(xmin+xmax)/2, y=0), 
    size=4/.pt, segment.size=0.1, min.segment.length = 0, 
    max.overlaps = 100, ylim=c(NA, 0), show.legend=FALSE)

graph2png(g_wiu_NLD, file="depuis_wiu_NLD" |> glue::glue(), width = png_w, height = png_h, dpi=600)

# gg <- ggplot(inf_radar_l1 |> filter(ref=="septembre 2021", d>0)) +
#   geom_text(aes(label=label, x = (xmin+xmax)/2, y=i+.05, hjust=0, vjust=0.5, color = coicop1), 
#             size=1, show.legend=FALSE)+
#   geom_rect(aes(xmin=xmin, xmax=xmax, ymin=0, ymax=i, fill = coicop1), show.legend=FALSE)+
#   # coord_flip()+
#   theme_ofce(base_size = 6)+
#   theme(plot.margin = margin(12,12,12,12, "pt"), panel.spacing = unit(6,"pt"),
#         legend.position = "right", legend.key.size = unit(9, "pt"), legend.text = element_text(size=6),
#         panel.grid.major.y = element_blank())+
#   scale_color_manual(labels = str_c(coicop_colors$coicop,": ", coicop_colors$short_label),
#                      values = coicop_colors |> pull(color, name = coicop), 
#                      aesthetics = c("color","fill"), 
#                      name="COICOP 12 products") +
#   scale_x_continuous(name=NULL, labels=NULL)+
#   scale_y_continuous(limits=c(-0.1,0.45), name=NULL) +
#   ggforce::facet_wrap_paginate(vars(geo_name), nrow = 3, ncol = 3, page = 1) +
#   gganimate::transition_time(d)+
#   labs(title = "{closest_state}") 
# gganimate::animate(gg, device="ragg_png", detail=10, renderer = gifski_renderer(),width=9,height=7, unit="cm", res=300)
# 
# 
# quintiles (impact sur le revenu) ---------------------------

gql1 <- imap(1:3, ~{
  gg <- ggplot(outcome_sorted |> filter(d==dmax_o$dmax, ref==since_wiu) ) +
    geom_col(data = ~filter(.x, coicop_digit==1), 
             aes(x=quantile, y=outcome, fill = coicop1), 
             col="white", size=0.1, width=0.6) +
    geom_col(data = ~filter(.x, coicop=="CP00"),
             aes(x=quantile, y=outcome),
             col="black", fill=NA, size=0.1, width=0.6) +
    scale_y_continuous(labels = scales::label_percent(1),
                       breaks=scales::breaks_width(0.02))+
    scale_fill_manual(labels = coicops$short_label2,
                      values = coicops |> pull(color, name = coicop),
                      name=NULL)+
    xlab(NULL)+ylab("impact as income share")+
    ggforce::facet_wrap_paginate(vars(geo_f), nrow=3, ncol=3, page=.x)+
    theme_ofce(base_size=9) +
    theme(legend.position = "bottom", legend.title =element_text(size=8), 
          legend.text=element_text(size=6), legend.key.size = unit(9, "pt"),
          axis.text = element_text(size = rel(0.5)), axis.line = element_line(size=unit(0.1, "pt")),
          axis.ticks.length = unit(0.5,"pt"), axis.ticks = element_line(size=unit(0.1, "pt")))+
    guides(fill=guide_legend(ncol=3, nrow=4))+
    labs(title="Impact on income per quintile of income since war in Ukraine",
         subtitle=str_c("% of quintile income", 
                        "from {month(since_wiu,TRUE, FALSE, 'en_US.UTF-8')} to {month(to_date_wiu,TRUE, FALSE, 'en_US.UTF-8')} {year(to_date_wiu)}",
                        "subfig {.y}.", sep="\n") |> glue(),
         caption=str_c("Sources: Eurostat HICP, consumption per quintile (hbs_str_t223), consumtion per cociop (hbs_str_t211),",
                       "income and expenditure per quintile (icw_res_02)", sep="\n"))
  graph2png(gg, file = "coicop_l1_wiu_{.x}" |> glue::glue(), width = png_w, height = png_h, dpi=600)
})

gql1_1y <- imap(1:3, ~{
  gg <- ggplot(outcome_sorted |> filter(d==dmax_o$dmax, ref==since_1y) ) +
    geom_col(data = ~filter(.x, coicop_digit==1), 
             aes(x=quantile, y=outcome, fill = coicop1), 
             col="white", size=0.1, width=0.6) +
    geom_col(data = ~filter(.x, coicop=="CP00"),
             aes(x=quantile, y=outcome),
             col="black", fill=NA, size=0.1, width=0.6) +
    scale_y_continuous(labels = scales::label_percent(1),
                       breaks=scales::breaks_width(0.02))+
    scale_fill_manual(labels = coicops$short_label2,
                      values = coicops |> pull(color, name = coicop),
                      name=NULL)+
    xlab(NULL)+ylab("impact as income share")+
    ggforce::facet_wrap_paginate(vars(geo_f), nrow=3, ncol=3, page=.x)+
    theme_ofce(base_size=9) +
    theme(legend.position = "bottom", legend.title =element_text(size=8), 
          legend.text=element_text(size=6), legend.key.size = unit(9, "pt"),
          axis.text = element_text(size = rel(0.5)), axis.line = element_line(size=unit(0.1, "pt")),
          axis.ticks.length = unit(0.5,"pt"), axis.ticks = element_line(size=unit(0.1, "pt")))+
    guides(fill=guide_legend(ncol=3, nrow=4))+
    labs(title="Impact on income per quintile of income since war in Ukraine",
         subtitle=str_c("% of quintile income", 
                        "from {month(since_1y,TRUE, FALSE, 'en_US.UTF-8')} to {month(to_date_wiu,TRUE, FALSE, 'en_US.UTF-8')} {year(to_date_wiu)}",
                        "subfig {.y}.", sep="\n") |> glue(),
         caption=str_c("Sources: Eurostat HICP, consumption per quintile (hbs_str_t223), consumtion per cociop (hbs_str_t211),",
                       "income and expenditure per quintile (icw_res_02)", sep="\n"))
  graph2png(gg, file = "coicop_l1_1y_{.x}" |> glue::glue(), width = png_w, height = png_h, dpi=600)
})


gql1_nld <- ggplot(outcome_sorted |> filter(d==dmax_o$dmax, ref==since_wiu, geo=="NL")) +
  geom_col(data = ~filter(.x, coicop_digit==1), 
           aes(x=quantile, y=outcome, fill = coicop1), 
           col="white", size=0.1, width=0.6) +
  geom_col(data = ~filter(.x, coicop=="CP00"),
           aes(x=quantile, y=outcome),
           col="black", fill=NA, size=0.1, width=0.6) +
  scale_y_continuous(labels = scales::label_percent(1),
                     breaks=scales::breaks_width(0.02))+
  scale_fill_manual(labels = coicops$short_label2,
                    values = coicops |> pull(color, name = coicop),
                    name=NULL)+
  xlab(NULL)+ylab("impact as income share")+
  theme_ofce(base_size=9) +
  theme(legend.position = "bottom", legend.title =element_text(size=8), 
        legend.text=element_text(size=6), legend.key.size = unit(9, "pt"),
        axis.text = element_text(size = rel(0.5)), axis.line = element_line(size=unit(0.1, "pt")),
        axis.ticks.length = unit(0.5,"pt"), axis.ticks = element_line(size=unit(0.1, "pt")))+
  guides(fill=guide_legend(ncol=3, nrow=4))+
  labs(title="Impact on income per quintile of income since war in Ukraine ofr The Netherlands",
       subtitle=str_c("% of quintile income", 
                      "from {month(since_wiu,TRUE, FALSE, 'en_US.UTF-8')} to {month(to_date_wiu,TRUE, FALSE, 'en_US.UTF-8')} {year(to_date_wiu)}",
                      "subfig NL.", sep="\n") |> glue(),
       caption=str_c("Sources: Eurostat HICP, consumption per quintile (hbs_str_t223), consumtion per cociop (hbs_str_t211),",
                     "income and expenditure per quintile (icw_res_02)", sep="\n"))
graph2png(gql1_nld, file = "coicop_l1_wiu_NLD" |> glue::glue(), width = png_w, height = png_h, dpi=600)

# quintiles simplifiés ------------------------
data <- outcome_sorted |> 
  filter(d==dmax_o$dmax, ref==since_wiu) |> 
  select(geo_f, coicop, outcome, quantile, coicop_digit, geo) |> 
  filter(quantile%in%c("Q1", "Q5")) |>
  pivot_wider(names_from = quantile, values_from = outcome) |> 
  filter(coicop %in% c("CP0111", "CP0115", "CP0451", "CP0452", "CP0453", "CP0454", "CP0722", "CP00")) |> 
  mutate(sub = if_else(coicop=="CP00", FALSE, TRUE)) |> 
  group_by(geo_f, sub) |>
  summarise(across(c(Q1,Q5), ~sum(.x, na.rm=TRUE)), geo=first(geo)) |> 
  ungroup() |> 
   mutate(geo = countrycode::countrycode(geo, "eurostat", "iso2c") |> tolower()) |> 
  ungroup() |> 
  pivot_wider(id_col = c(geo_f, geo), names_from = sub, values_from = c(Q1,Q5)) |> 
  rename_with(~str_replace(str_remove(.x, "_TRUE"), "_FALSE", "_t")) |> 
  mutate(geo_f = fct_reorder(geo_f, Q1_t))

quantiles1et5 <- ggplot(data , aes(y=geo_f)) + 
  geom_segment(aes(y=geo_f,x=Q1, yend=geo_f, xend=Q5), col="gray80", alpha=0.5, size=1.5)+
  geom_segment(aes(y=geo_f,x=Q1_t, yend=geo_f, xend=Q5_t), col="gray80", alpha=0.5, size=3.5)+
  geom_point(aes(x=Q1), size=1.5, col="steelblue1", alpha=0.75) +
  geom_point(aes(x=Q1_t), size=3.5, col="steelblue1") +
  geom_point(aes(x=Q5), size=1.5, col="steelblue4", alpha=0.75) +
  geom_point(aes(x=Q5_t), size=3.5, col="steelblue4") +
  geom_text(aes(x=Q1_t), label="Q1", size=5/.pt, col="white", fontface="bold" ) +
  geom_text(aes(x=Q5_t), label="Q5", size=5/.pt, col="white", fontface="bold") +
  scale_x_continuous(labels = scales::label_percent(1),
                     breaks = scales::breaks_width(0.01))+
  ylab(NULL) + xlab(NULL) +
  theme_ofce(base_size = 9) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour="gray70", size=0.1),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(margin=margin(0,10,0,0,"pt")))+
  ggflags::geom_flag(aes(x=-Inf, country=tolower(geo)), size=3) +
  labs(title = NULL,
       subtitle = str_c("from {str_wiu} to {month(to_date_wiu,TRUE, FALSE, 'en_US.UTF-8')} {year(to_date)}" |> glue(), sep="\n"),
       caption = str_c("Note: Impact on each quintile is as a share of income of the quintile for all products. Smaller dots are for a selection of COICOP items (CP0111, CP0115, CP0451, CP0452, CP0453, CP0454, CP0722)",
                       "Impact is the sum of mothly impacts divided by the sum of monthly income over the considered months.",
                       "Source: Eurostat HICP and income per quintile", sep="\n"))

quantiles1et5.fr <- ggplot(data |> mutate(geo_f = fct_reorder(countrycode::countrycode(data$geo, "iso2c", "un.name.fr"), Q1)) , aes(y=geo_f)) + 
  geom_segment(aes(y=geo_f,x=Q1, yend=geo_f, xend=Q5), col="steelblue")+
  geom_point(aes(x=Q1), size=3.5, col="steelblue1") +
  geom_point(aes(x=Q5), size=3.5, col="steelblue4") +
  geom_text(aes(x=Q1), label="Q1", size=6/.pt, col="white", fontface="bold" ) +
  geom_text(aes(x=Q5), label="Q5", size=6/.pt, col="white", fontface="bold") +
  scale_x_continuous(labels = scales::label_percent(1),
                     breaks = scales::breaks_width(0.01))+
  ylab(NULL) + xlab(NULL) +
  theme_ofce(base_size = 9) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour="gray70", size=0.1),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(margin=margin(0,10,0,0,"pt")))+
  ggflags::geom_flag(aes(x=-Inf, country=tolower(geo)), size=3) +
  labs(title = NULL,
       subtitle = NULL,
       caption = str_c("Note: L'impact sur chaque quintile est en % du revenu du quintinle  pour chque catégorie de produit (88 produits COICOP).",
                       "L'impact est la somme de l'impact chaque mois divisé par la somme des revenus mensuels sur la même période.",
                       "Huiles et matières grasses, céréales, combustibles pour le transport et le chauffage,",
                       "coicop CP0111, CP0115, CP0451, CP0452, CP0453, CP0454, CP0722.",
                       "De février 2022 à avril 2022.",
                       "Source: Eurostat HICP et revenus par quintile",
                       sep="\n"))

graph2svg(quantiles1et5)
graph2svg(quantiles1et5.fr)
graph2png(quantiles1et5, dpi=600)
graph2png(quantiles1et5, file="q1q5 pres", width = 25, height = 16, dpi=600)

# # animations ---------------------------
# library(gganimate)
# data2 <- outcome_sorted |> 
#   filter(d>=1, ref==since_wiu) |> 
#   select(geo_f, coicop, outcome, quantile, coicop_digit, geo, d) |> 
#   filter(quantile%in%c("Q1", "Q5")) |>
#   pivot_wider(names_from = quantile, values_from = outcome) |> 
#   filter(coicop %in% c("CP0111", "CP0115", "CP0451", "CP0452", "CP0453", "CP0454", "CP0722")) |> 
#   group_by(geo_f, d) |>
#   summarise(across(c(Q1,Q5), sum), geo=first(geo)) |> 
#   ungroup() |> 
#   mutate(geo_f = fct_reorder(geo_f, Q1, .fun=last)) |> 
#   mutate(geo = countrycode::countrycode(geo, "eurostat", "iso2c") |> tolower())
# 
# q15d <- ggplot(data2 , aes(y=geo_f)) + 
#   geom_segment(aes(y=geo_f,x=Q1, yend=geo_f, xend=Q5), col="steelblue")+
#   geom_point(aes(x=Q1), size=3, col="steelblue1") +
#   geom_point(aes(x=Q5), size=3, col="steelblue4") +
#   geom_text(aes(x=Q1), label="Q1", size=2, col="white", fontface="bold" ) +
#   geom_text(aes(x=Q5), label="Q5", size=2, col="white", fontface="bold") +
#   scale_x_continuous(labels = scales::label_percent(1),
#                      breaks = scales::breaks_width(0.01))+
#   ylab(NULL) + xlab(NULL) +
#   theme_ofce(base_size = 9) +
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(colour="gray70", size=0.1),
#         axis.line.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_text(margin=margin(0,10,0,0,"pt")))+
#   ggflags::geom_flag(aes(x=-Inf, country=tolower(geo)), size=3) +
#   # labs(title = str_c("Impact on quintile income of invasion of Ukraine {month(since,TRUE, FALSE,'en_US.UTF-8')} {year(since)}" |> glue(),
#   #                    "plus {closest_state} months"),
#   #      subtitle = str_c("Oils and fat, cereals, fuels for transportation and heating"),
#   #      caption = str_c("Note: Impact on each quintile is as a share of income of the quintile for the selected products.",
#   #                      "Slovenia has exempted from energy bill a part of the population in jan and feb 2022 (see companion text for details)",
#   #                      "Source: Eurostat HICP and income per quintile",
#   #                      "coicop CP0111, CP0115, CP0451, CP0452, CP0453, CP0454, CP0722", sep="\n")) +
#   facet_wrap(vars(d))
# animate(q15d, fps = 5,  device="ragg_png", res=200, width=1920, height=1080, unit="px") 
# 
# tableaux ------------------
# 

library(gt)
library(base64enc)
library(gtExtras)
flagAsIMG <- function(code) {
  codes <- paste0("./www/flags/",countrycode::countrycode(tolower(code),"eurostat","iso2c"), ".png")
  img <- paste0("data:image/png;base64,", map_chr(codes, ~base64enc::base64encode(.x)))
  paste0("<img src='", img, "'/>")
}

localflag <- function(code) {
  paste0("www/flags/",countrycode::countrycode(tolower(code),"eurostat","iso2c"), ".png") |> tolower()
}

# gt(data.frame(japan=1)) %>%
#   cols_label(japan=html("Japan", flagAsIMG("jp")))
ksplit <- function(x,k) {
  split(x, ceiling(seq_along(x)/k))
}

dmax_o_min <- dmax_o |> 
  group_by(geo, ref) |> 
  mutate(dmax = min(dmax))
dmax_min <- dmax |> 
  group_by(geo, ref) |> 
  mutate(dmax = min(dmax))

outcomeQ1Q5 <- outcome_sorted |>
  filter(d==dmax_o_min$dmax) |> 
  filter(ref %in% c(since_wiu, since_1y)) |>
  mutate(ref = case_when(ref==since_wiu ~ "wiu",
                         ref==since_1y ~ "1y")) |> 
  filter(quantile %in% c("Q1", "Q5")) |>
  pivot_wider(id_cols=c(geo, coicop), 
              names_from=c(quantile, ref),
              values_from=c(outcome, i, qs), 
              names_glue = "{str_sub(.value, 1,1)}_{quantile}_{ref}") |> 
  rename(qs_Q1 = q_Q1_1y,
         qs_Q5 = q_Q5_1y) |> 
  select(-starts_with("q_"))
table_index <- distinct(inf_sorted, geo) |> pull(geo) |> sort() |> ksplit(6)
cts <- imap(table_index, ~{
  ## data ----------------------
  countries_table <- inf_sorted |>
    filter(d==dmax_min$dmax, ref %in% c(since_wiu, since_1y)) |> 
    filter(geo %in% .x) |> 
    mutate(ref = case_when(ref==since_wiu ~ "wiu",
                           ref==since_1y ~ "1y")) |> 
    select(coicop, coicop_digit, i, d, ref, pm, geo_f, geo) |> 
    group_by(coicop, geo_f, coicop_digit) |> 
    filter((coicop_digit==3)|coicop_digit==0) |> 
    ungroup() |> 
    select(-coicop_digit) |> 
    mutate(ipm = i * pm) |> 
    pivot_wider(id_cols = c(coicop, geo_f, geo),
                names_from=c(d,ref),
                values_from = c(i, ipm, pm), 
                names_glue = "{.value}_{ref}") |>  
    rename(pm = pm_1y) |> select(-pm_wiu) |> 
    left_join(all_coicop |> select(coicop=coicop3, l3), by="coicop") |> 
    mutate(l3 = if_else(is.na(l3), "Total", l3)) |> 
    left_join(outcomeQ1Q5, by=c("geo", "coicop")) |> 
    arrange(geo, -ipm_wiu) |>
    mutate(flag = localflag(geo),
           l3 = str_c(coicop, ": ", l3)) |> 
    select(l3, geo_f,  i_1y, i_wiu, ipm_1y, ipm_wiu, o_Q1_wiu, o_Q5_wiu, pm, qs_Q1, qs_Q5) |> 
    filter(ipm_wiu >= 0.002 | rank(-ipm_wiu)<=5) |> 
    mutate(
      l3 = str_remove(l3, "CP"),
      l3 = if_else(str_length(l3)>32, str_c(str_sub(l3, 1,29), "..."), l3)) |> 
    ## gt ------------------------------------
  gt(groupname_col = "geo_f") |> 
    tab_header(title = md("**Price and Impacts ({.y}/{length(table_index)}**)" |> glue())) |> 
    tab_spanner(columns = c(i_1y, i_wiu), level=2, label = "Increase in price") |>
    tab_spanner(columns = c(ipm_1y, ipm_wiu, o_Q1_wiu, o_Q5_wiu), level=2, label = "Loss of purchase power") |>
    tab_spanner(columns = c(ipm_wiu, o_Q1_wiu, o_Q5_wiu), level = 1, label = "since feb. 2022") |>
    tab_spanner(columns = c(pm, qs_Q1, qs_Q5), label="weight (2015, HICP)", level=1) |> 
    cols_label(
      l3 ="",
      i_1y = glue("since {str_1y}"), i_wiu = glue("since {str_wiu}"), o_Q1_wiu = "20% poorest", o_Q5_wiu = "20% richest",
      ipm_1y = glue("since {str_1y}"), ipm_wiu = "all quintiles", 
      pm = "total", qs_Q1 = "20% poorest", qs_Q5 = "20% richest") |> 
    fmt_percent(columns= starts_with(c('i','o')), decimals=1) |> 
    fmt_percent(columns= c(pm, qs_Q1, qs_Q5), decimals=1) |> 
    tab_style(locations = cells_row_groups(),style = cell_text(weight="bold")) |> 
    tab_style(style = cell_text(indent = px(10)), locations = cells_body(columns = l3, rows = !str_detect(l3,"CP00"))) |> 
    ## tab options ----------------------------
  text_transform(locations = cells_body(), fn = \(x) if_else(x=="100.0%", "-", x)) |> 
  cols_width(where(is.numeric) ~ px(40)) |> 
    table_ofce() |> 
   tab_options(column_labels.padding.horizontal = px(5),
               data_row.padding = px(1),
               table.font.size = 8)
  gtsave(countries_table, "svg/annex_{.y}.png" |> glue::glue(), zoom=4)
  countries_table
})

save(gdis_feb, gdis_1y, g_1y, g_wiu, gql1, cts, 
     str_wiu, str_1y, dmax, dmax_o, dmaxx, dmaxx_o, str_to,
     since_1y, since_wiu, inf_sorted, outcome_sorted, outcomeQ1Q5,
     coicops, to_date_wiu, data,
     file="data/sorties.rdata")