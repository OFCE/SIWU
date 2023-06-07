
# télécharge les données sur eurostat 
source("R/make ISGU data.r")
# fabrique la table des consommations finales en énergie
source("R/nrg_bal.R")
# télécharge le fichier de la banque mondiale et fabrique les données
# attention ces cochns peuvent changer le format du fichier excel
source("R/commodity prices.R")
# avec les données constituées fait les graphiques et les tableaux
source("R/graphiques.R")

 # penser à modifier readme.md et à renomer le pdf en fonction de la date
# penser à modifier le texte aussi de temps en temps

# 
# génére le html
quarto::quarto_render("report/SIWU_brief.qmd", output_format = "html")
# et le pdf
quarto::quarto_render("report/SIWU_brief.qmd", output_format = "pdf")
