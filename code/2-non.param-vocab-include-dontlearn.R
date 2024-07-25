wants <- c('ggplot2','ggpubr','templates','PerformanceAnalytics','utils','randomcoloR')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])


library(knitr)
library(templates)
library(markdown)

lpmarks <- list("group" = list(group = "GROUP",
                               dat.filter = "gdat <- gdat[is.na(gdat$NECESSIDADE.DEFICIENCIA),]",
                               val.group = c("WG (non.teach)", "WG (teach)", "St+WG (non.teach)", "St+WG (teach)"),
                               col.group = c("#dedf4d","#7c7c16","#fec2ba","#fc3c24")),
                "stari" = list(group = "STARI.GROUP",
                               dat.filter = "gdat <- gdat[is.na(gdat$NECESSIDADE.DEFICIENCIA),]",
                               val.group = c("Control (non.teach)","Control (teach)","Experimental (non.teach)","Experimental (teach)"),
                               col.group = c("#b2c8ed","#4b7fd4","#fec2ba","#fc3c24")),
                "wg" = list(group = "WG.GROUP",
                            dat.filter = "gdat <- gdat[is.na(gdat$NECESSIDADE.DEFICIENCIA),]",
                            val.group = c("WG (non.teach)", "WG (teach)", "St+WG (non.teach)", "St+WG (teach)"),
                            col.group = c("#dedf4d","#7c7c16","#fec2ba","#fc3c24")))

for (key in names(lpmarks)) {
  tdeParams = lpmarks[[key]]

  ofile = paste0("code/non.param-vocab-",key,"-include-dontlearn.Rmd")

  tfile = "templates/non-param-aov.Rmd"
  params = list(
    title = "for assess TDE score",
    dfile = "dat-norm-vocab.xlsx",
    group = tdeParams$group,
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
    fig.width = 8, fig.height = 8,
    pfig.width = 12, pfig.height = 8
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
    rmarkdown::render(
      ofile, output_dir = './results', clean = T,
      output_format = c("github_document","html_document"))
  }

}



