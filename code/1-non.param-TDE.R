wants <- c('ggplot2','ggpubr','templates','PerformanceAnalytics','utils','randomcoloR')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(knitr)
library(templates)
library(markdown)


dat.filter = "gdat <- gdat[is.na(gdat$NECESSIDADE.DEFICIENCIA) & gdat$score.pre != 80 & gdat$score.pos != 80,]"

lpmarks <- list("group" = list(group = "GROUP",
                               dat.filter = dat.filter,
                               plot.filter = "",
                               val.group = c("WG", "St+WG"),
                               col.group = c("#bcbd22","#fd7f6f")),
                "stari" = list(group = "STARI.GROUP",
                               dat.filter = dat.filter, plot.filter = "",
                               val.group = c("Control","Experimental"),
                               col.group = c("#89ABE3","#F98866")))

for (key in names(lpmarks)) {
  tdeParams = lpmarks[[key]]

  ofile = paste0("code/non.param-TDE-",key,".Rmd")

  tfile = "templates/non-param-aov.Rmd"
  params = list(
    title = "for assess TDE score",
    dfile = "dat-TDE.xlsx",
    group = tdeParams$group, max.y = 1, plot.filter = tdeParams$plot.filter,
    val.group = tdeParams$val.group, col.group = tdeParams$col.group,
    dv = "score", dv.dif = "score.dif",
    dv.pre = "score.pre", dv.pos = "score.pos",
    fatores = c("SEXO","ZONA","COR.RACA","LOCAL","SERIE","ESCOLA"),
    dat.filter = dat.filter
  )
  txt <- do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))


  tfile =  "templates/non-param-aov-one.Rmd"
  params = c(params, list(
    title = paste0("One-way factor analysis for: *score ~ GROUP*"),
    iv = tdeParams$group, pivot.key = "time", pivot.value = "score",
    fig.width = 12, fig.height = 12,
    pfig.width = 14, pfig.height = 10
  ))
  txt <- paste0(
    txt, "\n",
    do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params)))

  for (iv2 in c("SEXO","ZONA","COR.RACA","LOCAL","SERIE","ESCOLA")) {
    tfile =  "templates/non-param-aov-two.Rmd"
    params = c(params, list(
      iv1 = tdeParams$group, iv2 = iv2,
      title = paste0("Two-way factor analysis for: *score ~ GROUP:",iv2,"*")
    ))
    txt <- paste0(txt, "\n", do.call(
      tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params)))
  }

  if (!file.exists(ofile)) {
    writeLines(txt, ofile, useBytes=T)
    #rmarkdown::render(
    #  ofile, output_dir = './results', clean = T,
    #  output_format = c("github_document","html_document"))
  }

}



