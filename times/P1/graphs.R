representa_datos <- function(datos, filename) {
    library(ggplot2)
    library(reshape2)
    d_dat <- melt(datos, id.vars="Primo")
    names(d_dat) <- c("Primo", "Version", "Tiempo")
    d_dat$Primo = as.character(d_dat$Primo)
    ggplot(d_dat, aes(x=Primo,y=Tiempo)) + geom_bar(aes(fill=Version), 
        position="dodge", stat="identity")
    ggsave(paste(filename, ".png", sep=""), width=17)
}

miller_rabin <- read.csv("miller_rabin.csv")
shanks <- read.csv("baby_s_giant_s.csv")
factorizar <- read.csv("factorizar.csv")

representa_datos(datos=miller_rabin, filename="miller_rabin")
representa_datos(datos=shanks, filename="shanks")
representa_datos(datos=factorizar, filename="factorizar")
