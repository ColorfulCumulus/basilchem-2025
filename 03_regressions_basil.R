# Regressions for basil VOC data - all varieties
# originally created by SC / 4 April 2024
# last updated by SC / 4 April 2024

# 1 - SETUP ####
# opening packages, uploading data
library(ggplot2)
library(ggrepel)
library(ggcorrplot)
library(ggpubr)

# set wd, upload data
chems <- read.csv('regressions.csv', check.names = FALSE)

# changing names
chems[,1] <- c("AT", "CN", "GD", "GV", "LE", "LL", "LM", "MM", "DP")

#custom function for displaying R^2 values of regression lines
eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(R)^2 ==~r2,
                 list(r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

# extract floral values 
flowers <- chems[,2:72]
names(flowers) <- names(leaves)

# extract leaf values (even numbered columns)
leaves <- chems[,73:143]


# create correlation plot
corr.data <- ggcorrplot(cor(leaves,flowers))$data

# extract only values where same chemical is compared
  # between leaves and flowers
lvf <-corr.data[which(corr.data$Var1 == corr.data$Var2),]
  # note that not all 72 compounds are given
  # those where the compound is absent from all leaf samples
  # cannot be correlated (n = 11 compounds)

#order descending by absolute value of correlation
lvf<- lvf[order(-abs(lvf$value)), ]

# 2 - Getting the top 30 correlated compounds ####
lvf$Var1[1:30] 
  # note that many compounds are perfectly correlated
  # or close to so
  # because only one variety contains that compound

# for example, estragole:
  # which is only abundant in cv. Lime: 
if(TRUE){
summary(lm(`Estragole floral` ~ `Estragole`, data = chems)) #significant

estragole <- ggplot(chems, aes(x=`Estragole`, y = `Estragole floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE) +  
  geom_label(
    x = 6500, 
    y = 3500, 
    label = eq(chems$Estragole, chems$`Estragole floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = Estragole,
                                `Estragole floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 10,
                  size = 4)
estragole <- estragole + labs(x = "Estragole", 
                              y = "")
estragole <- estragole  + theme(axis.title = element_text(size = 12, face = "bold"))
estragole <- estragole  + theme(axis.text = element_text(size = 10))


estragole <- estragole  + theme(legend.position = "none")
estragole <- estragole   + theme(axis.line = element_line(colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.border = element_blank(),
                                 panel.background = element_blank())

estragole }
 
# 3 - Building graphs ####
# row 1: prominent compounds in basil ####
# eugenol
if(TRUE){
summary(lm(`Eugenol floral` ~ Eugenol, data = chems)) #ns

eugenol <- ggplot(chems, aes(x=Eugenol, y = `Eugenol floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 400, 
    y = 20000, 
    label = eq(chems$Eugenol, chems$`Eugenol floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = Eugenol,
                                y= `Eugenol floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
eugenol <- eugenol + labs(x = "Eugenol", 
                          y = "")
eugenol <- eugenol + theme(axis.title = element_text(size = 12, face = "bold"))
eugenol <- eugenol + theme(axis.text = element_text(size = 10))


eugenol <- eugenol + theme(legend.position = "none")
eugenol <- eugenol + theme(axis.line = element_line(colour = "black"),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank())

eugenol}

# methyl eugenol
if(TRUE){
summary(lm(`Methyl eugenol floral` ~ `Methyl eugenol`, data = chems)) #ns

M_eugenol <- ggplot(chems, aes(x=`Methyl eugenol`, y = `Methyl eugenol floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 290, 
    y = 10000, 
    label = eq(chems$`Methyl eugenol`, chems$`Methyl eugenol floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Methyl eugenol`,
                                `Methyl eugenol floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 10,
                  size = 4)
M_eugenol <- M_eugenol + labs(x = "Methyl eugenol", 
                              y = "")
M_eugenol <- M_eugenol + theme(axis.title = element_text(size = 12, face = "bold"))
M_eugenol <- M_eugenol + theme(axis.text = element_text(size = 10))


M_eugenol <- M_eugenol + theme(legend.position = "none")
M_eugenol <- M_eugenol + theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank())

M_eugenol}

# eucalyptol
if(TRUE){
  summary(lm(`Eucalyptol floral` ~ Eucalyptol, data = chems)) #ns
  
  eucalyptol <- ggplot(chems, aes(x=Eucalyptol, y = `Eucalyptol floral`))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE,
                linetype = "dashed") +  
    geom_label(
      x = 10100, 
      y = 5400, 
      label = eq(chems$Eucalyptol, chems$`Eucalyptol floral`), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = Eucalyptol,
                                  `Eucalyptol floral`,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  eucalyptol <- eucalyptol + labs(x = "Eucalyptol", 
                                  y = "")
  eucalyptol <- eucalyptol + theme(axis.title = element_text(size = 12, face = "bold"))
  eucalyptol <- eucalyptol + theme(axis.text = element_text(size = 10))
  
  
  eucalyptol <- eucalyptol + theme(legend.position = "none")
  eucalyptol <- eucalyptol + theme(axis.line = element_line(colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   panel.background = element_blank())
  eucalyptol}

# linalool
if(TRUE){
summary(lm(`Linalool floral` ~ Linalool, data = chems)) #ns

linalool <- ggplot(chems, aes(x=Linalool, y = `Linalool floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 7500, 
    y = 98000, 
    label = eq(chems$Linalool, chems$`Linalool floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = Linalool,
                                `Linalool floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
linalool <- linalool + labs(x = "Linalool", 
                            y = "")
linalool <- linalool + theme(axis.title = element_text(size = 12, face = "bold"))
linalool <- linalool + theme(axis.text = element_text(size = 10))


linalool <- linalool + theme(legend.position = "none")
linalool <- linalool + theme(axis.line = element_line(colour = "black"),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank())
linalool
}




# rows 2-4 excpt estragole: top 11 correlated compounds ####
# row 2: humulene, fenchyl acetate(X), (E-DMT), a-Terpineol, bornyl acetate
# humulene
if(TRUE){
summary(lm(`Humulene floral` ~ `Humulene`, data = chems)) #ns 

humulene <- ggplot(chems, aes(x=`Humulene`, y = `Humulene floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE) +  
  geom_label(
    x = 35, 
    y = 3200, 
    label = eq(chems$`Humulene`, chems$`Humulene floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Humulene`,
                                `Humulene floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
humulene <- humulene + labs(x = "Humulene", 
                                y = "")
humulene <- humulene   + theme(axis.title = element_text(size = 12, face = "bold"))
humulene <- humulene   + theme(axis.text = element_text(size = 10))


humulene <- humulene  + theme(legend.position = "none")
humulene <- humulene  + theme(axis.line = element_line(colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.border = element_blank(),
                                 panel.background = element_blank())
humulene}

# fenchyl acetate: no-go
if(TRUE){
summary(lm(`Fenchyl acetate` ~ `Fenchyl acetate`, data = chems)) #marginally sig

fenchyl_acetate <- ggplot(chems, aes(x=`Fenchyl acetate`, y = `Fenchyl acetate`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 170, 
    y = 200, 
    label = eq(chems$`Fenchyl acetate`, chems$`Fenchyl acetate`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Fenchyl acetate`,
                                `Fenchyl acetate`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
fenchyl_acetate <- fenchyl_acetate + labs(x = "Fenchyl acetate", 
                                            y = "")
fenchyl_acetate <- fenchyl_acetate  + theme(axis.title = element_text(size = 12, face = "bold"))
fenchyl_acetate <- fenchyl_acetate  + theme(axis.text = element_text(size = 10))

fenchyl_acetate <- fenchyl_acetate + theme(legend.position = "none")
fenchyl_acetate <- fenchyl_acetate + theme(axis.line = element_line(colour = "black"),
                                             panel.grid.major = element_blank(),
                                             panel.grid.minor = element_blank(),
                                             panel.border = element_blank(),
                                             panel.background = element_blank())

fenchyl_acetate}

# (E)- 4,8-dimethylnona-(1,3,7)-triene 
if(TRUE){
summary(lm(`(E)-4,8-Dimethylnona-1,3,7-triene floral` ~ `(E)-4,8-Dimethylnona-1,3,7-triene`, data = chems)) #ns

DMNT <- ggplot(chems, aes(x=`(E)-4,8-Dimethylnona-1,3,7-triene`, y = `(E)-4,8-Dimethylnona-1,3,7-triene floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 23, 
    y = 1600, 
    label = eq(chems$`(E)-4,8-Dimethylnona-1,3,7-triene`, chems$`(E)-4,8-Dimethylnona-1,3,7-triene floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `(E)-4,8-Dimethylnona-1,3,7-triene`,
                                `(E)-4,8-Dimethylnona-1,3,7-triene floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
DMNT <- DMNT + labs(x = "(E)-4,8-Dimethylnona-1,3,7-triene", 
                    y = "")
DMNT <- DMNT  + theme(axis.title = element_text(size = 8, face = "bold"))
DMNT <- DMNT  + theme(axis.text = element_text(size = 10))


DMNT <- DMNT  + theme(legend.position = "none")
DMNT <- DMNT  + theme(axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
DMNT 
}

# alpha-terpineol 
if(TRUE){
summary(lm(`alpha-Terpineol floral` ~ `alpha-Terpineol`, data = chems)) #ns

a_terpine <- ggplot(chems, aes(x=`alpha-Terpineol`, y = `alpha-Terpineol floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE) +  
  geom_label(
    x = 50, 
    y = 1350, 
    label = eq(chems$`alpha-Terpineol`, chems$`alpha-Terpineol floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `alpha-Terpineol`,
                                `alpha-Terpineol floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 10,
                  size = 4)
a_terpine <- a_terpine + labs(x = "alpha-Terpineol", 
                                  y = "")
a_terpine <- a_terpine + theme(axis.title = element_text(size = 12, face = "bold"))
a_terpine <- a_terpine  + theme(axis.text = element_text(size = 10))


a_terpine <- a_terpine + theme(legend.position = "none")
a_terpine <- a_terpine  + theme(axis.line = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank())
a_terpine
}

# bornyl acetate
if(TRUE){
summary(lm(`Bornyl acetate floral` ~ `Bornyl acetate`, data = chems)) #sig

bornyl_acetate <- ggplot(chems, aes(x=`Bornyl acetate`, y = `Bornyl acetate floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE) +  
  geom_label(
    x = 850, 
    y = 120, 
    label = eq(chems$`Bornyl acetate`, chems$`Bornyl acetate floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Bornyl acetate`,
                                `Bornyl acetate floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 10,
                  size = 4)
bornyl_acetate <- bornyl_acetate + labs(x = "Bornyl acetate", 
                                        y = "")
bornyl_acetate <- bornyl_acetate   + theme(axis.title = element_text(size = 10))
bornyl_acetate <- bornyl_acetate  + theme(axis.text = element_text(size = 10))


bornyl_acetate <- bornyl_acetate + theme(legend.position = "none")
bornyl_acetate <- bornyl_acetate + theme(axis.line = element_line(colour = "black"),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         panel.border = element_blank(),
                                         panel.background = element_blank())
bornyl_acetate}



# row 3: unk 43/81/105, a-Amorphene, Germacrene D, unk 43/105/204####
#unknown 43, 69, 55
if(TRUE){
summary(lm(`Unknown 43, 69, 55 floral` ~ `Unknown 43, 69, 55`, data = chems)) #ns

unknown43a <- ggplot(chems, aes(x=`Unknown 43, 69, 55`, y = `Unknown 43, 69, 55 floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 200, 
    y = 220, 
    label = eq(chems$`Unknown 43, 69, 55`, chems$`Unknown 43, 69, 55 floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Unknown 43, 69, 55`,
                                `Unknown 43, 69, 55 floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
unknown43a <- unknown43a + labs(x = "Unk. 43, 69, 55", 
                              y = "")
unknown43a <- unknown43a + theme(axis.title = element_text(size = 12, face = "bold"))
unknown43a <- unknown43a + theme(axis.text = element_text(size = 10))


unknown43a <- unknown43a + theme(legend.position = "none")
unknown43a <- unknown43a + theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank())
unknown43a
}

#amorphene
if(TRUE){
summary(lm(`alpha-Amorphene floral` ~ `alpha-Amorphene`, data = chems))# ns

amorphene <- ggplot(chems, aes(x=`alpha-Amorphene`, y = `alpha-Amorphene floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 26, 
    y = 670, 
    label = eq(chems$`alpha-Amorphene`, chems$`alpha-Amorphene floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `alpha-Amorphene`,
                                `alpha-Amorphene floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
amorphene <- amorphene + labs(x = "alpha-Amorphene", 
                            y = "")
amorphene <- amorphene   + theme(axis.title = element_text(size = 12, face = "bold"))
amorphene <- amorphene   + theme(axis.text = element_text(size = 10))


amorphene <- amorphene   + theme(legend.position = "none")
amorphene <- amorphene   + theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank())

amorphene}

# Germacrene D 
if(TRUE){
summary(lm(`Germacrene D floral` ~ `Germacrene D`, data = chems))# sig


germacrene <- ggplot(chems, aes(x=`Germacrene D`, y = `Germacrene D floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE) +  
  geom_label(
    x = 90, 
    y = 5400, 
    label = eq(chems$`Germacrene D`, chems$`Germacrene D floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Germacrene D`,
                                `Germacrene D floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
germacrene <- germacrene + labs(x = "Germacrene D", 
                                y = "")
germacrene <- germacrene + theme(axis.title = element_text(size = 12, face = "bold"))
germacrene <- germacrene + theme(axis.text = element_text(size = 10))


germacrene <- germacrene + theme(legend.position = "none")
germacrene <- germacrene + theme(axis.line = element_line(colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.border = element_blank(),
                                 panel.background = element_blank())
germacrene}

#unknown 43, 81, 105
if(TRUE){
summary(lm(`Unknown 43, 81, 105 floral` ~ `Unknown 43, 81, 105`, data = chems))# sig

unknown43b <- ggplot(chems, aes(x=`Unknown 43, 81, 105`, y = `Unknown 43, 81, 105 floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE) +  
  geom_label(
    x = 20, 
    y = 130, 
    label = eq(chems$`Unknown 43, 81, 105`, chems$`Unknown 43, 81, 105 floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Unknown 43, 81, 105`,
                                `Unknown 43, 81, 105 floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 10,
                  size = 4)
unknown43b <- unknown43b + labs(x = "Unk. 43, 81, 105",
                                y = "",
                                size = 6.5)
unknown43b <- unknown43b  + theme(axis.title = element_text(size = 10, face = "bold"))
unknown43b <- unknown43b + theme(axis.text = element_text(size = 10))


unknown43b <- unknown43b + theme(legend.position = "none")
unknown43b <- unknown43b + theme(axis.line = element_line(colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.border = element_blank(),
                                 panel.background = element_blank())


unknown43b}


# row 4:, aciphyllene, unk 95/161/107, valencene, estragole ####
# aciphyllene
if(TRUE){
summary(lm(`Aciphyllene floral` ~ `Aciphyllene`, data = chems))# ns
  

aciphyllene <- ggplot(chems, aes(x=Aciphyllene, y = `Aciphyllene floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE) +  
  geom_label(
    x = 320, 
    y = 100, 
    label = eq(chems$Aciphyllene, chems$`Aciphyllene floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = Aciphyllene,
                                `Aciphyllene floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 10,
                  size = 4)
aciphyllene <- aciphyllene + labs(x = "Aciphyllene", 
                                  y = "")
aciphyllene <- aciphyllene + theme(axis.title = element_text(size = 12, face = "bold"))
aciphyllene <- aciphyllene + theme(axis.text = element_text(size = 10))


aciphyllene <- aciphyllene + theme(legend.position = "none")
aciphyllene <- aciphyllene + theme(axis.line = element_line(colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   panel.background = element_blank())

aciphyllene}
#unknown 95, 161, 107
if(TRUE){
summary(lm(`Unknown 95, 161, 107 floral` ~ `Unknown 95, 161, 107`, data = chems))# ns

unknown95 <- ggplot(chems, aes(x=`Unknown 95, 161, 107`, y = `Unknown 95, 161, 107 floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 170, 
    y = 90, 
    label = eq(chems$`Unknown 95, 161, 107`, chems$`Unknown 95, 161, 107 floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = `Unknown 95, 161, 107`,
                                `Unknown 95, 161, 107 floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
unknown95 <- unknown95 + labs(x = "Unk. 95, 161, 107", 
                              y = "")
unknown95 <- unknown95 + theme(axis.title = element_text(size = 12, face = "bold"))
unknown95 <- unknown95 + theme(axis.text = element_text(size = 10))


unknown95 <- unknown95 + theme(legend.position = "none")
unknown95 <- unknown95 + theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank())

unknown95}


#valencene
if(TRUE){
summary(lm(`Valencene floral` ~ Valencene, data = chems))# ns

valencene <- ggplot(chems, aes(x=Valencene, y = `Valencene floral`))+
  geom_point() +
  geom_smooth(method = lm, color = "blue", se = TRUE,
              linetype = "dashed") +  
  geom_label(
    x = 21, 
    y = 95, 
    label = eq(chems$Valencene, chems$`Valencene floral`), 
    fontface = "bold",
    size = 4,
    parse = TRUE) +
  geom_text_repel(data = chems, 
                  mapping = aes(x = Valencene,
                                `Valencene floral`,
                                label = variety),
                  fontface = "bold",
                  min.segment.length = 1,
                  size = 4)
valencene <- valencene + labs(x = "Valencene", 
                                  y = "")
valencene <- valencene    + theme(axis.title = element_text(size = 12, face = "bold"))
valencene <- valencene   + theme(axis.text = element_text(size = 10))


valencene <- valencene   + theme(legend.position = "none")
valencene <- valencene    + theme(axis.line = element_line(colour = "black"),
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(),
                                      panel.border = element_blank(),
                                      panel.background = element_blank())

valencene}


# stitching it all together ####
# publishing the positive correlates
regressions <- ggarrange(eugenol, M_eugenol, eucalyptol, linalool,
                         humulene, DMNT, a_terpine, bornyl_acetate,
                         unknown43a, amorphene,germacrene, unknown43b, 
                         aciphyllene, unknown95, valencene, estragole,
                         labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
                                    "M", "N", "O", "P"),
                         font.label = list(size = 20, color = "black", face = "bold", family = NULL),
                         ncol = 4, nrow = 4)

ggsave(plot = regressions,
       filename = "Fig_2.pdf",
       width = 3000,
       height = 2700, 
       units = "px")

# unused species ####
if(FALSE){
  # 5: beta-caryophyllene ####
  summary(lm(b_Caryophyllene_floral ~ b_Caryophyllene, data = chems)) #sig
  
  caryophyllene <- ggplot(chems, aes(x=b_Caryophyllene, y = b_Caryophyllene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 300, 
      y = 28000, 
      label = eq(chems$b_Caryophyllene, chems$b_Caryophyllene_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = b_Caryophyllene,
                                  b_Caryophyllene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 0,
                    size = 4)
  caryophyllene <- caryophyllene + labs(x = "b-Caryophyllene", 
                                        y = "")
  caryophyllene <- caryophyllene + theme(axis.title = element_text(size = 12, face = "bold"))
  caryophyllene <- caryophyllene + theme(axis.text = element_text(size = 10))
  
  
  caryophyllene <- caryophyllene + theme(legend.position = "none")
  caryophyllene <- caryophyllene + theme(axis.line = element_line(colour = "black"),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         panel.border = element_blank(),
                                         panel.background = element_blank())
  
  # 6: a-bergamotene ####
  summary(lm(a_Bergamotene_floral ~ a_Bergamotene, data = chems)) #ns
  
  bergamotene <- ggplot(chems, aes(x=a_Bergamotene, y = a_Bergamotene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = FALSE,
                linetype = "dashed") +  
    geom_label(
      x = 9000, 
      y = 100, 
      label = eq(chems$a_Bergamotene, chems$a_Bergamotene_floral), 
      fontface = "bold",
      size = 6.5,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = a_Bergamotene,
                                  a_Bergamotene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 10,
                    size = 6.5)
  bergamotene <- bergamotene + labs(x = "Leaf concentration, ng/h/g", 
                                    y = "Floral concentration, ng/h/g")
  bergamotene <- bergamotene + theme(axis.title = element_text(size = 10))
  bergamotene <- bergamotene + theme(axis.text = element_text(size = 10))
  
  
  bergamotene <- bergamotene + theme(legend.position = "none")
  bergamotene <- bergamotene + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_blank(),
                                     panel.background = element_blank())
  # 7: 4-hexenyl acetate ####
  summary(lm(X4_hexen_yl_acetate_floral ~ X4_hexen_yl_acetate, data = chems)) #sig
  
  hxac <- ggplot(chems, aes(x=X4_hexen_yl_acetate, y = X4_hexen_yl_acetate_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 40, 
      y = 1100, 
      label = eq(chems$X4_hexen_yl_acetate, chems$X4_hexen_yl_acetate_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = X4_hexen_yl_acetate,
                                  X4_hexen_yl_acetate_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  hxac <- hxac + labs(x = "4-hexen-yl-acetate", 
                      y = "")
  hxac <- hxac + theme(axis.title = element_text(size = 12, face = "bold"))
  hxac <- hxac + theme(axis.text = element_text(size = 10))
  
  
  hxac <- hxac + theme(legend.position = "none")
  hxac <- hxac+ theme(axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
  
  # 9: Borneol ####
  summary(lm(Borneol_floral ~ Borneol, data = chems)) #ns
  
  borneol <- ggplot(chems, aes(x=Borneol, y = Borneol_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = FALSE) +  
    geom_label(
      x = 50, 
      y = 1000, 
      label = eq(chems$Borneol, chems$Borneol_floral), 
      fontface = "bold",
      size = 6.5,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = Borneol,
                                  Borneol_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 10,
                    size = 6.5)
  borneol <- borneol + labs(x = "Leaf concentration, ng/h/g", 
                            y = "Floral concentration, ng/h/g")
  borneol <- borneol  + theme(axis.text = element_text(size = 10))
  
  
  borneol <- borneol + theme(legend.position = "none")
  borneol <- borneol  + theme(axis.line = element_line(colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              panel.background = element_blank())
  
  # 12 Nerol & Citral ####
  summary(lm(Nerol_floral ~ Nerol, data = chems)) #significant
  
  nerol <- ggplot(chems, aes(x=Nerol, y = Nerol_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 25, 
      y = 8500, 
      label = eq(chems$Nerol, chems$Nerol_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = Nerol,
                                  Nerol_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  nerol <- nerol + labs(x = "Citral", 
                        y = "")
  nerol <- nerol  + theme(axis.title = element_text(size = 12, face = "bold"))
  nerol <- nerol + theme(axis.text = element_text(size = 10))
  
  
  nerol <- nerol  + theme(legend.position = "none")
  nerol <- nerol   + theme(axis.line = element_line(colour = "black"),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank())
  
  # citral will have an identical graph and R^2
  
  # 16: E-methyl cinnamate ####
  # R^2 almost 1. only found in cinnamon.
  
  # 17: cis bergamotene ####
  summary(lm(cis_Bergamotene_floral ~ cis_Bergamotene, data = chems)) #sig
  
  c_bergamotene <- ggplot(chems, aes(x=cis_Bergamotene, y = cis_Bergamotene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = FALSE) +  
    geom_label(
      x = 10, 
      y = 150, 
      label = eq(chems$cis_Bergamotene, chems$cis_Bergamotene_floral), 
      fontface = "bold",
      size = 6.5,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = cis_Bergamotene,
                                  cis_Bergamotene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 10,
                    size = 6.5)
  c_bergamotene <- c_bergamotene + labs(x = "Leaf concentration, ng/h/g", 
                                        y = "Floral concentration, ng/h/g")
  c_bergamotene <- c_bergamotene + theme(axis.title = element_text(size = 10))
  c_bergamotene <- c_bergamotene + theme(axis.text = element_text(size = 10))
  
  
  c_bergamotene <- c_bergamotene + theme(legend.position = "none")
  c_bergamotene <- c_bergamotene + theme(axis.line = element_line(colour = "black"),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         panel.border = element_blank(),
                                         panel.background = element_blank())
  
  # 18: beta gurjunene ####
  summary(lm(b_Gujurnene_floral ~ b_Gujurnene, data = chems)) #sig
  
  gurjunene <- ggplot(chems, aes(x=b_Gujurnene, y = b_Gujurnene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 40, 
      y = 4000, 
      label = eq(chems$b_Gujurnene, chems$b_Gujurnene_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = b_Gujurnene,
                                  b_Gujurnene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  gurjunene <- gurjunene + labs(x = "b-Gurjunene", 
                                y = "")
  gurjunene <- gurjunene + theme(axis.title = element_text(size = 12, face = "bold"))
  gurjunene <- gurjunene + theme(axis.text = element_text(size = 10))
  
  
  gurjunene <- gurjunene + theme(legend.position = "none")
  gurjunene <- gurjunene + theme(axis.line = element_line(colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.border = element_blank(),
                                 panel.background = element_blank())
  
  
  # 19: d-muurolene ####
  summary(lm(d_muurolene_floral ~ d_muurolene, data = chems)) #ns
  
  d_muurolene <- ggplot(chems, aes(x=d_muurolene, y = d_muurolene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = FALSE) +  
    geom_label(
      x = 100, 
      y = 6000, 
      label = eq(chems$d_muurolene, chems$d_muurolene_floral), 
      fontface = "bold",
      size = 6.5,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = d_muurolene,
                                  d_muurolene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 10,
                    size = 6.5)
  d_muurolene <- d_muurolene + labs(x = "Leaf concentration, ng/h/g", 
                                    y = "Floral concentration, ng/h/g")
  d_muurolene <- d_muurolene + theme(axis.title = element_text(size = 10))
  d_muurolene <- d_muurolene + theme(axis.text = element_text(size = 10))
  
  
  d_muurolene <- d_muurolene + theme(legend.position = "none")
  d_muurolene <- d_muurolene + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_blank(),
                                     panel.background = element_blank())
  
  # 20: a-muurolene ####
  summary(lm(a_muurolene_floral ~ a_muurolene, data = chems)) #ns
  
  a_muurolene <- ggplot(chems, aes(x=a_muurolene, y = a_muurolene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = FALSE) +  
    geom_label(
      x = -0.025, 
      y = 60, 
      label = eq(chems$a_muurolene, chems$a_muurolene_floral), 
      fontface = "bold",
      size = 6.5,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = a_muurolene,
                                  a_muurolene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 10,
                    size = 6.5)
  a_muurolene <- a_muurolene + labs(x = "Leaf concentration, ng/h/g", 
                                    y = "Floral concentration, ng/h/g")
  a_muurolene <- a_muurolene + theme(axis.title = element_text(size = 10))
  a_muurolene <- a_muurolene + theme(axis.text = element_text(size = 10))
  
  
  a_muurolene <- a_muurolene + theme(legend.position = "none")
  a_muurolene <- a_muurolene + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_blank(),
                                     panel.background = element_blank())
  
  
  # 22: g-muurolene ####
  # R^2 = 0, because all leaf values are 0. 
  
  # 25: d-cadinene ####
  summary(lm(d_Cadinene_floral ~ d_Cadinene, data = chems))# sig
  
  d_cadinene <- ggplot(chems, aes(x=d_Cadinene, y = d_Cadinene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 25, 
      y = 1600, 
      label = eq(chems$d_Cadinene, chems$d_Cadinene_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = d_Cadinene,
                                  d_Cadinene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 0,
                    size = 4)
  d_cadinene <- d_cadinene + labs(x = "d-Cadinene", 
                                  y = "")
  d_cadinene <- d_cadinene + theme(axis.title = element_text(size = 12, face = "bold"))
  d_cadinene <- d_cadinene + theme(axis.text = element_text(size = 10))
  
  
  d_cadinene <- d_cadinene + theme(legend.position = "none")
  d_cadinene <- d_cadinene + theme(axis.line = element_line(colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   panel.background = element_blank())
  
  # 26: 1,4-cadinadiene ####
  summary(lm(X1_4_Cadinadiene_floral ~ X1_4_Cadinadiene, data = chems))# sig
  
  cadinadiene <- ggplot(chems, aes(x=X1_4_Cadinadiene, y = X1_4_Cadinadiene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 100, 
      y = 5000, 
      label = eq(chems$X1_4_Cadinadiene, chems$X1_4_Cadinadiene_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = X1_4_Cadinadiene,
                                  X1_4_Cadinadiene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  cadinadiene <- cadinadiene + labs(x = "1,4-Cadinadiene", 
                                    y = "")
  cadinadiene <- cadinadiene + theme(axis.title = element_text(size = 12, face = "bold"))
  cadinadiene <- cadinadiene + theme(axis.text = element_text(size = 10))
  
  
  cadinadiene <- cadinadiene + theme(legend.position = "none")
  cadinadiene <- cadinadiene + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_blank(),
                                     panel.background = element_blank())
  
  
  
  # 28: spathulenol ####
  summary(lm(Spathulenol_floral ~ Spathulenol, data = chems))# sig
  
  spathulenol <- ggplot(chems, aes(x=Spathulenol, y = Spathulenol_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 10, 
      y = 180, 
      label = eq(chems$Spathulenol, chems$Spathulenol_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = Spathulenol,
                                  Spathulenol_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  spathulenol <- spathulenol + labs(x = "Spathulenol", 
                                    y = "")
  spathulenol <- spathulenol + theme(axis.title = element_text(size = 12, face = "bold"))
  spathulenol <- spathulenol + theme(axis.text = element_text(size = 10))
  
  
  spathulenol <- spathulenol + theme(legend.position = "none")
  spathulenol <- spathulenol + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_blank(),
                                     panel.background = element_blank())
  # 29: b-Ocimene ####
  summary(lm(beta_Ocimene_floral ~ beta_Ocimene, data = chems))# ns
  
  b_ocimene <- ggplot(chems, aes(x=beta_Ocimene, y = beta_Ocimene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = FALSE,
                linetype = "dashed") +  
    geom_label(
      x = 3800, 
      y = 110, 
      label = eq(chems$beta_Ocimene, chems$beta_Ocimene_floral), 
      fontface = "bold",
      size = 6.5,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = beta_Ocimene,
                                  beta_Ocimene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 10,
                    size = 6.5)
  b_ocimene <- b_ocimene + labs(x = "Leaf concentration, ng/h/g", 
                                y = "Floral concentration, ng/h/g")
  b_ocimene <- b_ocimene + theme(axis.title = element_text(size = 10))
  b_ocimene <- b_ocimene + theme(axis.text = element_text(size = 10))
  
  
  b_ocimene <- b_ocimene + theme(legend.position = "none")
  b_ocimene <- b_ocimene + theme(axis.line = element_line(colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.border = element_blank(),
                                 panel.background = element_blank())
  
  
  # 30: epicubenol ####
  summary(lm(Epicubenol_floral ~ Epicubenol, data = chems))# ns
  
  epicubenol <- ggplot(chems, aes(x=Epicubenol, y = Epicubenol_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE,
                linetype = "dashed") +  
    geom_label(
      x = 500, 
      y = 2700, 
      label = eq(chems$Epicubenol, chems$Epicubenol_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = Epicubenol,
                                  Epicubenol_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  epicubenol <- epicubenol + labs(x = "Epicubenol", 
                                  y = "")
  epicubenol <- epicubenol  + theme(axis.title = element_text(size = 12, face = "bold"))
  epicubenol <- epicubenol  + theme(axis.text = element_text(size = 10))
  
  
  epicubenol <- epicubenol  + theme(legend.position = "none")
  epicubenol <- epicubenol  + theme(axis.line = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank())
  
  # 33: 2-(1-phenylethyl)-phenol ####
  summary(lm(X2_1_phenylethyl_Phenol_floral ~ X2_1_phenylethyl_Phenol, data = chems))# ns
  
  TPEP <- ggplot(chems, aes(x=X2_1_phenylethyl_Phenol, y = X2_1_phenylethyl_Phenol_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE,
                linetype = "dashed") +  
    geom_label(
      x = 1100, 
      y = 170, 
      label = eq(chems$X2_1_phenylethyl_Phenol, chems$X2_1_phenylethyl_Phenol_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = X2_1_phenylethyl_Phenol,
                                  X2_1_phenylethyl_Phenol_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  TPEP <- TPEP + labs(x = "2-(1-phenylethyl)-phenol", 
                      y = "")
  TPEP <- TPEP    + theme(axis.title = element_text(size = 12, face = "bold"))
  TPEP <- TPEP   + theme(axis.text = element_text(size = 10))
  
  
  TPEP <- TPEP   + theme(legend.position = "none")
  TPEP <- TPEP   + theme(axis.line = element_line(colour = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         panel.background = element_blank())
  
  # 34: longipinene ####
  summary(lm(a_Longipinene_floral ~ a_Longipinene, data = chems))# ns
  
  longipinene <- ggplot(chems, aes(x=a_Longipinene, y = a_Longipinene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE,
                linetype = "dashed") +  
    geom_label(
      x = 20, 
      y = 520, 
      label = eq(chems$a_Longipinene, chems$a_Longipinene_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = a_Longipinene,
                                  a_Longipinene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  longipinene <- longipinene + labs(x = "Longipinene", 
                                    y = "")
  longipinene <- longipinene    + theme(axis.title = element_text(size = 12, face = "bold"))
  longipinene <- longipinene    + theme(axis.text = element_text(size = 10))
  
  
  longipinene <- longipinene   + theme(legend.position = "none")
  longipinene <- longipinene    + theme(axis.line = element_line(colour = "black"),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        panel.border = element_blank(),
                                        panel.background = element_blank())
  
  # 36: aromadendrene ####
  summary(lm(Aromadendrene_floral ~ Aromadendrene, data = chems))# ns
  
  aromadendrene <- ggplot(chems, aes(x=Aromadendrene, y = Aromadendrene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE,
                linetype = "dashed") +  
    geom_label(
      x = 470, 
      y = 50, 
      label = eq(chems$Aromadendrene, chems$Aromadendrene_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = Aromadendrene,
                                  Aromadendrene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  aromadendrene <- aromadendrene + labs(x = "Aromadendrene",
                                        y = "")
  aromadendrene <- aromadendrene + theme(axis.title = element_text(size = 12, face = "bold"))
  aromadendrene <- aromadendrene + theme(axis.text = element_text(size = 10))
  
  
  aromadendrene <- aromadendrene+ theme(legend.position = "none")
  aromadendrene <- aromadendrene + theme(axis.line = element_line(colour = "black"),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         panel.border = element_blank(),
                                         panel.background = element_blank())
  
  # 38: a-cadinene ####
  summary(lm(a_Cadinene_floral ~ a_Cadinene, data = chems))# ns
  
  a_cadinene <- ggplot(chems, aes(x=a_Cadinene, y = a_Cadinene_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE,
                linetype = "dashed") +  
    geom_label(
      x = 10, 
      y = 75, 
      label = eq(chems$a_Cadinene, chems$a_Cadinene_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = a_Cadinene,
                                  a_Cadinene_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  a_cadinene <- a_cadinene + labs(x = "a-Cadinene", 
                                  y = "")
  a_cadinene <- a_cadinene + theme(axis.title = element_text(size = 12, face = "bold"))
  a_cadinene <- a_cadinene + theme(axis.text = element_text(size = 10))
  
  
  a_cadinene <- a_cadinene + theme(legend.position = "none")
  a_cadinene <- a_cadinene + theme(axis.line = element_line(colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   panel.background = element_blank())
  
  # 39: trans-geraniol ####
  geraniol <- ggplot(chems, aes(x=trans_Geraniol, y = trans_Geraniol_floral))+
    geom_point() +
    geom_smooth(method = lm, color = "blue", se = TRUE) +  
    geom_label(
      x = 10, 
      y = 75, 
      label = eq(chems$trans_Geraniol, chems$trans_Geraniol_floral), 
      fontface = "bold",
      size = 4,
      parse = TRUE) +
    geom_text_repel(data = chems, 
                    mapping = aes(x = trans_Geraniol,
                                  trans_Geraniol_floral,
                                  label = variety),
                    fontface = "bold",
                    min.segment.length = 1,
                    size = 4)
  geraniol <- geraniol + labs(x = "trans-Geraniol", 
                              y = "")
  geraniol <- geraniol + theme(axis.title = element_text(size = 10, face = "bold"))
  geraniol <- geraniol + theme(axis.text = element_text(size = 10))
  
  
  geraniol <- geraniol + theme(legend.position = "none")
  geraniol <- geraniol + theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank())
  # need 2 do (>-0.1 PC2)
  # gernaniol (probs R^2 = 0.999)
  
  
  
  # SUMMARY: highly correlated targets / PC2: ####
  # 4-hexenyl acetate
  # lemon-D, R^2 = 0.649
  # (E)-4,8-Dimethylnona-(1,3,7)-triene
  # lime-D, R^2 = 0.322
  # Borneol
  # good spread, R^2 = 0.0569
  # a-terpineol
  # lemon-D, R^2 = 0.202
  # Estragole
  # lime-D, R^2 = 0.956
  # Nerol
  # only in lemon.  R^2 = 1
  # Citral
  # only in lemon. R^2 = 1
  # Bornyl acetate
  # good spread. R^2 = 0.536
  # Terpinyl acetate
  # good spread. R^2 = 0.373
  # a-cubebene
  # good spread. R^2 = 0.139
  # Methyl cinnamate 
  # only in cinnamon. R^2 = 1
  # cis-bergamotene
  # lemon-D, R^2 = 0.729
  # b-caryophyllene
  # lemon-D, R^2
  # a-bergamotene
  # decent spread. R^2 = 0.301
  # b-gurjunene 
  # lemon-D, R^2 = 0.841
  # d-muurolene
  # good spread. R^2 = 0.196
  # a-muurolene 
  # all x = 0. so R^2 = 0.
  # unknown 93_80_121
  # good spread. R^2 = 0.196
  # g-muurolene
  # all x = 0. so R^2 = 0.
  # Germacrene D
  # decent spread. 
  # Aciphyllene
  # decent spread. R^2 = 0.560
  # d-cadinene
  # lemon-D, R^2  = 0.927
  # 1,4-cadinadiene
  # lemon-D, R^2 = 0.996
  # Unknown 43, 81, 105
  # greek dwarf-D, R^2 = 0.833
  # Spathulenol
  # lemon-D, R^2 = 0.776
  # Epicubenol
  # R^2 = 0.019
  # alpha Guaiene
  # R^2 = 0.0699
  # Valencene
  # R^2 = 0.023
  # 2-(1-phenylethyl)-phenol
  # R^2 = 0.0307
  # Longipinene
  # R^2 = 0.00101
  # unknown 95,161, 107
  # R^2 = 0.0383
  # aromadendrene
  # R^2 = 0.700
  # isoValencene
  # 0.042
  # alpha Cadinene
  # R^2 = 0.256, lettuce-D
  # trans-geraniol
  # R^2 = 1, lemon-D
  
  
  
  # Unused code: PCA correlations
  if(FALSE){
    pcs <- pcs[-c(3,13),]
    pc.f <- pcs[c(1:9),]
    pc.l <- pcs[c(10:18),]
    pc.1 <- data.frame(pc.l$PC1_average, pc.l$PC2_average, 
                       pc.f$PC1_average, pc.f$PC2_average)
    names(pc.1) <- c("LPC1", "LPC2", "FPC1", "FPC2")
    pc.1$variety <- c("AT", "CN", "GD", "LE", "LL",
                      "LM", "MM", "DP", "NF")
    
    # first principal component
    summary(lm(FPC1 ~ LPC1, data = pc.1)) #ns
    
    PC1 <- ggplot(pc.1, aes(x=LPC1, y = FPC1))+
      geom_point() +
      geom_smooth(method = lm, color = "blue", se = TRUE,
                  linetype = "dashed") +  
      geom_label(
        x = -6, 
        y = -8.7, 
        label = eq(pc.1$LPC1, pc.1$FPC1), 
        fontface = "bold",
        size = 6.5,
        parse = TRUE) +
      geom_text_repel(data = pc.1, 
                      mapping = aes(x = LPC1,
                                    FPC1,
                                    label = variety),
                      fontface = "bold",
                      min.segment.length = 10,
                      size = 6.5)
    PC1 <- PC1 + labs(x = "Leaf PC 1", 
                      y = "Floral PC 1")
    PC1 <- PC1 + theme(axis.title = element_text(size = 10))
    PC1 <- PC1 + theme(axis.text = element_text(size = 10))
    
    
    PC1 <- PC1 + theme(legend.position = "none")
    PC1 <- PC1 + theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
    
    PC1 <- PC1 + theme(axis.text = element_text(size = 20))
    PC1 <- PC1 + theme(axis.title.x = element_text(size = 20),
                       axis.title.y = element_text(size = 20))
    
    
    # second principal component
    summary(lm(FPC2 ~ LPC2, data = pc.1)) # sig
    
    PC2 <- ggplot(pc.1, aes(x=LPC2, y = FPC2))+
      geom_point() +
      geom_smooth(method = lm, color = "blue", se = TRUE,) +  
      geom_label(
        x = 2.5, 
        y = -15, 
        label = eq(pc.1$LPC2, pc.1$FPC2), 
        fontface = "bold",
        size = 6.5,
        parse = TRUE) +
      geom_text_repel(data = pc.1, 
                      mapping = aes(x = LPC2,
                                    FPC2,
                                    label = variety),
                      fontface = "bold",
                      min.segment.length = 10,
                      size = 6.5)
    PC2 <- PC2 + labs(x = "Leaf PC 2", 
                      y = "Floral PC 2")
    PC2 <- PC2 + theme(axis.title = element_text(size = 10))
    PC2 <- PC2 + theme(axis.text = element_text(size = 10))
    
    
    PC2 <- PC2 + theme(legend.position = "none")
    PC2 <- PC2 + theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
    
    PC2 <- PC2 + theme(axis.text = element_text(size = 20))
    PC2 <- PC2 + theme(axis.title.x = element_text(size = 20),
                       axis.title.y = element_text(size = 20))
    
    # second principal component, no cv. lemon
    pc.2 <- pc.1[-4,]
    summary(lm(FPC2 ~ LPC2, data = pc.2)) #ns
    
    
    PC2b <- ggplot(pc.2, aes(x=LPC2, y = FPC2))+
      geom_point() +
      geom_smooth(method = lm, color = "blue", se = TRUE,
                  linetype = "dashed") +  
      geom_label(
        x = 4.3, 
        y = -6, 
        label = eq(pc.2$LPC2, pc.2$FPC2), 
        fontface = "bold",
        size = 6.5,
        parse = TRUE) +
      geom_text_repel(data = pc.2, 
                      mapping = aes(x = LPC2,
                                    FPC2,
                                    label = variety),
                      fontface = "bold",
                      min.segment.length = 10,
                      size = 6.5)
    PC2b <- PC2b + labs(x = "Leaf PC 2", 
                        y = "Floral PC 2")
    PC2b <- PC2b + theme(axis.title = element_text(size = 10))
    PC2b <- PC2b + theme(axis.text = element_text(size = 10))
    
    
    PC2b <- PC2b + theme(legend.position = "none")
    PC2b <- PC2b + theme(axis.line = element_line(colour = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         panel.background = element_blank())
    
    PC2b <- PC2b + theme(axis.text = element_text(size = 20))
    PC2b <- PC2b + theme(axis.title.x = element_text(size = 20),
                         axis.title.y = element_text(size = 20))
    
    PCboth <- ggarrange(PC1, PC2, PC2b,
                        labels = c("A", "B", "C"),
                        font.label = list(size = 20, color = "black", face = "bold", family = NULL),
                        ncol = 3, nrow = 1)
    #export @ width: 1200, height: 400
    
  }
  
}

