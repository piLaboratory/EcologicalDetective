library(dplyr)
library(reshape)
library(xtable)
aves <- read.csv("aves.csv", as.is=TRUE)
aves[is.na(aves)] <- 0
aves2 <- melt(aves)
f1 <- function(x) switch(x, A="Campo sujo", B= "Campo Cerrado", C= "Cerrado")
## Tudo de uma vez só
aves.summ <-
    aves %>%
        melt() %>%
            mutate(cod.fis=substr(variable,1,1), fisionomia=sapply(cod.fis,f1)) %>%
                group_by(sp,fisionomia) %>%
                    summarise(registros=sum(value), frequencia=sum(value>0))
## Tabela
tmp <- xtable(cast(melt(data.frame(aves.summ)), sp~variable+fisionomia, sum), digits=0)

print(tmp, include.rownames=FALSE, file="tabAves.tex")
