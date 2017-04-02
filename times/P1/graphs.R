representa_datos <- function(datos, filename) {
    library(ggplot2)
    library(reshape2)
    d_dat <- melt(datos, id.vars="Primo")
    names(d_dat) <- c("Primo", "Version", "Tiempo")
    ggplot(d_dat, aes(Primo,Tiempo)) + geom_line(aes(color=Version))
    ggsave(paste(filename, ".png", sep=""))
}

miller_rabin <- read.csv("miller_rabin.csv")
representa_datos(datos=miller_rabin, filename="miller_rabin.png")
