
library(readxl)

dat <- read_excel("data/dat-vocab.xlsx", sheet = "main")

df.vocab <- dat[,colnames(dat)[which(startsWith(colnames(dat),"vocab."))]]
df.names <- dat[,colnames(dat)[which(!startsWith(colnames(dat),"vocab."))]]

df1 <- df.vocab[,c("vocab.teach.norm.pre","vocab.teach.norm.pos")]
colnames(df1) <- c("vocab.norm.pre","vocab.norm.pos")

df1 <- cbind(df.names, df1)
df1$id <- paste0(df1$id, ".teach")
df1$GROUP <- paste0(df1$GROUP," (teach)")
df1$STARI.GROUP <- sapply(df1$STARI.GROUP, FUN = function(x) {
  ifelse(!is.na(x), paste0(x," (teach)"), NA)
})


df2 <- df.vocab[,c("vocab.non.teach.norm.pre","vocab.non.teach.norm.pos")]
colnames(df2) <- c("vocab.norm.pre","vocab.norm.pos")
df2 <- cbind(df.names, df2)
df2$id <- paste0(df2$id, ".non.teach")
df2$GROUP <- paste0(df2$GROUP, " (non.teach)")
df2$STARI.GROUP <- sapply(df2$STARI.GROUP, FUN = function(x) {
  ifelse(!is.na(x), paste0(x," (non.teach)"), NA)
})

writexl::write_xlsx(list("main" = rbind(df1, df2)), "data/dat-norm-vocab.xlsx")



