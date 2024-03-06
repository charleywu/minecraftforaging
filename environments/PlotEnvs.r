

setwd("~/GitLab/producerScrounger/environments")

smooth <- list.files(pattern = "smooth")
random <- list.files(pattern = "random")
files <- c(smooth[-c(14,15)], random[-c(14,15)])

graphics.off()
pdf("Enviroments.pdf", height = 8, width = 5)

par(mfrow = c(8,5),
    mar = c(0.5,0.5,0,0),
    oma = c(0,2,0.1,0.1))

for (file in files) {
  
  #Load feather file  
  d <- as.matrix(read.csv2(file))
  
  
image(d, col=0:1, xlim=c(-0.05,1.05), ylim=c(-0.05,1.05), xaxt = "n", yaxt = "n")

}

mtext("Random                                     Smooth", side = 2, line = 0, outer = TRUE, cex = 1.75)

dev.off()