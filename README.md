# Social Impact of the War in Ukraine

> [Guillaume Allègre](mailto:guillaume.allegre@sciencespo.fr), [François Geerolf](mailto:francois.geerolf@sciencespo.fr), [Xavier Timbeau](mailto:xavier.timbeau@sciencespo.fr)

## aim

This repo contains code, data and docs to reproduce a work done for the S&D group at the European Parliament. Using data from Eurostat HICP per coicop at the level 92 and data from Eurostat on consumption share (of income) per quintile and coicop (at the level 38) we evaluate what is the impact of recent price increases on income per quintile.

Data on HICP is updated every month (plus a flash estimate in between). Data on quintile consumption share is available for the year 2015 (except Italy, 2005).

## tranformation of data

A number of transformation are applied to data :

1.  we deseasonalize all prices from the HICP every month. For performance reasons, we use a loess algorithm (`stats::stl)` instead of the usual X11/X13. The deseasonalization is important for CP03 (clothing) products.

2.  we extrapolate consumption share per quintile from the level 38 (coicop 2) to to 92 (coicop 3) using a proportional rule

3.  we shift consumption share per quintile at the level 3 (extrapolated from level 2) from 2015 to 2021 using HICP weights. In the process, we keep only relative shares and align on HICP weights. This allow to break down aggregate effect by quintile. We have not dealt with chained indexes issues in the process. The effect we calculate per quintile per product is not summing to the aggregate effect. However, we checked the discrepancy which remains in tolerable margin.

We got this kind of results:

![](svg/depuis_wiu_1.png)

And a break down per quintile:

![](svg/coicop_l1_wiu_1.png)

summarized in thins comparison graph:

![](svg/quantiles1et5.png)

## other data

Using World Bank commodity prices we build a real price index starting in 1960 for a selection of commodity and build a graph panel of those real indexes. The scale is a log base 10, in order to provide meaningful analysis and all indexes are rebased as being on average 100 during the year 2018 (before the COVID, roughly). This produce the following graph:

<img src="svg/gcom2000.png" width="826"/>

## report

A report is presenting data and analysis. It is located in the `report` folder. The source for report is in `SIWU_brief.qmd` and relies on `{quarto}`. A pdf can be rendered ([SIWU_brief.pdf](report/SIWU_brief.pdf)) or an HTML accessible there: [SIWU_brief.html](report/SIWU_brief.html)

## structure of files and folders

R sources files are in the `R` folder.

`make ISGU data.r` download data from eurostat using some convenience function around the `{eurostat}` package. Install `{ofce}` from github (`devtools::install_github("ofce/ofce")`) and proceed to transformation of data. data is cached in `data` but is not on the github repo, so you need to run sources files to download everything. Results are on the repo.

A check is done each time you run `make ISGU data.r`, to assess if there is an update of Eurostat data and download the latest version of data. No history or versionning of datasets from Eurostat is kept.

`graphiques.r` is producing the graphs which are saved (mostly as `png` and `svg`) in the folder `svg` plus the annex tables. It produces the `sorties.Rdata` necessary to run the `SIWU_brief.qmd`.  

`nrg_bal.r` produces the table 1 of the report using data from Eurostat on energy balances.

`commodity prices.r` produces the real price graph above and downloads data from World Bank.

Once all dependencies are installed, data should be updated automatically when available.

`choc de prix.r` is an attempt to use daily data from Yahoo Finance, but some symbols are not always available so we dropped this method.

Sources are moderately commented and comments are usually in french.

## license

All code, texts and graph are under a [CECILL-B license](LICENSE.TXT) (a french licence in the spirit of MIT licence). Data is from (what we believe is) open data from various providers and may be subject to specific licenses.
