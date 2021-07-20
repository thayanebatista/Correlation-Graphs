library(ggcorrplot)
library(dplyr)
#utilizando dados carregados do resultado do DESeq2 
genes <- genesFiltered$vsd #todos os valores deseq para cada amostra com valores NA de nomes nao pareaveis com a biblioteca do kegg
genes <- genes[complete.cases(genes),] # limpando os dados para somente linhas com valores
cellcorrelation <- as.data.frame(t(genes))
#nessa parte ele transforma todos os valores (eleva todos ao quadrado) e os arredonda pra um numero inteiro
cellcorrelation <- as.data.frame(apply(cellcorrelation, 1:2, function(x) as.integer(2^x)))
correlation <- cor(cellcorrelation)



res <- correlation
res <- as.data.frame(res)
res[] <- sapply(res,function(x) ifelse(x > 0.9
                                       & x <= 1,
                                       x , NA))

#
width = (ncol(res) * 3)
height = (nrow(res) * 3)
#salvando o arquivo do tipo png na pasta já criada graphics
filename = genename
filename = paste("graphics/",filename, ".png", sep = '')
png(filename = filename, width = width, height = height)

ggcorrplot(res,
           lab = TRUE,
           outline.col = "black",
           ggtheme = ggplot2::theme_gray(),
           type = "upper")
graphics.off()