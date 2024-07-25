wants <- c('ggplot2','ggpubr','templates','PerformanceAnalytics','utils','randomcoloR')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])


library(knitr)
library(templates)
library(markdown)

dat.filter = "gdat <- gdat[is.na(gdat$NECESSIDADE.DEFICIENCIA) & gdat$vocab.norm.pre != 1 & gdat$vocab.norm.pos != 1,]"

lpmarks <- list("group" = list(group = "GROUP",
                               dat.filter = dat.filter,
                               plot.filter = "[pdat.long$GROUP %in% c(\"WG (teach)\",\"St+WG (teach)\"),]",
                               val.group = c("WG (base)", "WG (teach)", "St+WG (base)", "St+WG (teach)"),
                               col.group = c("#dedf4d","#7c7c16","#fec2ba","#fc3c24")),
                "stari" = list(group = "STARI.GROUP",
                               dat.filter = dat.filter,
                               plot.filter = "",
                               val.group = c("Control (base)","Control (teach)","Experimental (base)","Experimental (teach)"),
                               col.group = c("#b2c8ed","#4b7fd4","#fec2ba","#fc3c24")))




for (key in names(lpmarks)) {
  tdeParams = lpmarks[[key]]

  ofile = paste0("code/non.param-vocab-",key,".Rmd")

  tfile = "templates/non-param-aov.Rmd"
  params = list(
    title = "for assess TDE score",
    dfile = "dat-norm-vocab.xlsx",
    group = tdeParams$group, max.y = 1, plot.filter = tdeParams$plot.filter,
    val.group = tdeParams$val.group, col.group = tdeParams$col.group,
    dv = "vocab", dv.dif = "vocab.dif",
    dv.pre = "vocab.norm.pre", dv.pos = "vocab.norm.pos",
    fatores = c("SEXO","ZONA","COR.RACA","LOCAL","SERIE","ESCOLA"),
    dat.filter = tdeParams$dat.filter
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
    #rmarkdown::render(ofile, output_dir = './results', clean = T, output_format = c("github_document","html_document"))
  }

}



