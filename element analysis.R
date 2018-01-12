## packages -----
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(plot3D)
library(factoextra)

## colors ----
myColors7 <- brewer.pal(7,"Dark2")
myColors6 <- brewer.pal(6,"Dark2")
myColors8 <- brewer.pal(8, "Dark2")
myColors12 <- brewer.pal(12, "Paired")
myColors4 <- c("#e66101", "#fdb863", "#5e3c99", "#b2abd2")


## my functions -----

# seed.univscatter() to create univariate scatter plot OBJECT
# for SEED soluble and protein-incorporated DATA
# df = df
# varY = df$varY
# varGroup = df$varGroup; the grouping variable on the X axis
# varColor = df$varColor
# varShape = df$varShape
# metN = "", for graph title
# type = "", NULL, for graph title
# units = "", for y axis title
# jw = jitter width
# jh = jitter height
# dw = dodge width
# return is ggplot OBJECT


seed.univscatter2 <- function (df, varY, varGroup, metN, type, units, jw, jh){
  pj <- position_jitter(width = jw, 
                        height = jh)
  title <- paste(type, metN)
  yLabel <- units
  
  usp <- ggplot(df, aes(x = varGroup, y = varY)) +
    geom_point(size = 3, shape = 1, stroke = 0.3, alpha = 0.9, position = pj) +
    xlab("") +
    ylab(yLabel) +
    labs(color = NULL, shape = NULL) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) +
    theme(axis.title.x=element_text(size=8)) +
    theme(axis.title.y=element_text(size=8)) +
    theme(axis.text.x=element_text(color="black", size=10,angle = 90, hjust = 0.8, vjust = 0.5)) +
    theme(axis.text.y=element_text(color="black", size=8)) +
    theme(panel.background = element_rect(fill = "white", color = "black")) +
    theme(panel.border = element_rect(colour="black", fill=NA)) +
    scale_y_continuous(limits = c(0, max(varY)*1.2))
  return(usp)
}



seed.univscatter4 <- function (df, varY, varGroup, varColor, varShape, metN, type, units, jw, jh){
  pj <- position_jitter(width = jw, 
                        height = jh)
  title <- paste(type, metN)
  yLabel <- units
  
  usp <- ggplot(df, aes(x = varGroup, y = varY)) +
    geom_point(aes(color = varColor, shape = varShape), size = 3, alpha = 0.8, position = pj) +
    xlab("") +
    ylab(yLabel) +
    labs(color = NULL, shape = NULL) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(axis.title.x=element_text(size=7)) +
    theme(axis.title.y=element_text(size=7)) +
    theme(axis.text.x=element_text(color="black", size=10,angle = 90, hjust = 0.8, vjust = 0.5)) +
    theme(axis.text.y=element_text(color="black", size=8)) +
    theme(panel.background = element_rect(fill = "white", color = "black")) +
    theme(panel.border = element_rect(colour="black", fill=NA)) +
    scale_y_continuous(limits = c(0, NA))
  return(usp)
}

seed.univscatter4_noLegend <- function (df, varY, varGroup, varColor, varShape, metN, type, units, jw, jh){
  pj <- position_jitter(width = jw, 
                        height = jh)
  title <- paste(type, metN)
  yLabel <- units
  
  usp <- ggplot(df, aes(x = varGroup, y = varY)) +
    geom_point(aes(color = varColor, shape = varShape), size = 3, alpha = 0.8, position = pj) +
    xlab("") +
    ylab(yLabel) +
    labs(color = NULL, shape = NULL) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(axis.title.x=element_text(size=7)) +
    theme(axis.title.y=element_text(size=7)) +
    theme(axis.text.x=element_text(color="black", size=10,angle = 90, hjust = 0.8, vjust = 0.5)) +
    theme(axis.text.y=element_text(color="black", size=8)) +
    theme(panel.background = element_rect(fill = "white", color = "black")) +
    theme(panel.border = element_rect(colour="black", fill=NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    theme(legend.position="none")
  return(usp)
}


equalize <- function(plots) {
  g <- lapply(plots, ggplotGrob) # Convert to gtables
  g.widths = lapply(g, function(x) grid:::unit.list(x$widths)) # Apply the un-exported unit.list() from grid to each plot
  g3.widths <- lapply(g.widths, function(x) x[1:3]) # Get first three widths from each plot
  g3max.widths <- do.call(unit.pmax, g3.widths) # Get maximum widths for first three widths across the plots
  for(i in 1:length(plots)) g[[i]]$widths[1:3] = g3max.widths # Apply the maximum widths to each plot
  return(g)
}

# Baptiste Augui?, on ggplot2 wiki
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mean_ci95 <- function(x) {
  n <- sum(!is.na(x))
  se <- sd(x)/sqrt(n)
  me <- se*qt(0.975, n-1)
  m <- mean(x)
  ymin <- m-me
  ymax <- m+me
  return(c(y=m,ymin=ymin,ymax=ymax))
}

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "red", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}


panel.scatter <-function(x, y){
  points(x,y, pch = 19, col = myColors7[df$lineSimple])
}


eigenvalue_plot = function(ev, standardization = TRUE)
{
  # Broken stick model (MacArthur 1957)
  n = length(ev)
  bsm = data.frame(j = seq(1:n), p=0) # Broken Stick Model
  bsm$p[1] = 1/n
  for (i in 2:n) {
    bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
  } 
  bsm$p = 100*bsm$p/n
  
  # Plot eigenvalues and % of variance for each axis
  op = par(mfrow = c(2,1))
  barplot(ev, main = "Eigenvalues", col = "bisque", las = 2)
  if(standardization == TRUE) {
    abline(h = 1, col = "red")
  }
  else {
    abline(h = mean(ev), col = "red")
  }
  
  legend("topright", "Kaiser Criterion", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main = "Variance [%]", col=c("bisque",2), las=2)
  legend("topright", c("Variance [%]", "Broken Stick Model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}




## data ------
all <- read.delim("Exp summary for msPrep.txt", header = TRUE)

## order factor levels ------

# set order for lineComplete
all$lineComplete <- factor(all$lineComplete, levels = c("WT", 
                                                        "SAT47", 
                                                        "CGSx4",
                                                        "SSA", 
                                                        "SAT47_SSA_4-8", 
                                                        "SAT47_SSA_5-5",
                                                        "CGSx4_SSA_1-1", 
                                                        "CGSx4_SSA_1-7", 
                                                        "IR64"))
table(all$lineComplete)
sum(table(all$lineComplete))

# set order for lineSimple
all$lineSimple <- factor(all$lineSimple, levels = c("WT", 
                                                    "SAT", 
                                                    "CGS", 
                                                    "SSA", 
                                                    "SAT_SSA", 
                                                    "CGS_SSA", 
                                                    "IR64"))
table(all$lineSimple)
sum(table(all$lineSimple))

# set order for genotypePresumed1
#(i.e, CGS+/+; SSA-/- segregant coded as CGS+/+, just like parental line)
all$genotypePresumed1 <- factor(all$genotypePresumed1, levels = c("WT", 
                                                                  "SAT+/+", 
                                                                  "CGS+/+", 
                                                                  "SSA+/+", 
                                                                  "SAT+/+ ; SSA+/+", 
                                                                  "SAT+/+ ; SSA+/-", 
                                                                  "SAT+/- ; SSA+/+", 
                                                                  "SAT? ; SSA+/+", 
                                                                  "CGS+/+ ; SSA+/+", 
                                                                  "CGS+/+ ; SSA+/-", 
                                                                  "CGS+/- ; SSA+/+", 
                                                                  "CGS? ; SSA+/+", 
                                                                  "IR64"))
table(all$genotypePresumed1)
sum(table(all$genotypePresumed1))

# set order for genotypePresumed2
#(i.e, CGS+/+; SSA-/- segregant coded as CGS+/+; SSA-/-, NOT like parental line, can still see that they are segregants)
all$genotypePresumed2 <- factor(all$genotypePresumed2, levels = c("WT", 
                                                                  "SAT+/+", 
                                                                  "CGS+/+", 
                                                                  "SSA+/+", 
                                                                  "SAT+/+ ; SSA+/+", 
                                                                  "SAT+/+ ; SSA+/-", 
                                                                  "SAT+/- ; SSA+/+", 
                                                                  "SAT? ; SSA+/+",
                                                                  "CGS+/+ ; SSA+/+", 
                                                                  "CGS+/+ ; SSA+/-", 
                                                                  "CGS+/- ; SSA+/+", 
                                                                  "CGS? ; SSA+/+",
                                                                  "SAT+/+ ; SSA-/-",
                                                                  "CGS+/+ ; SSA-/-",
                                                                  "SAT-/- ; SSA+/+",
                                                                  "CGS-/- ; SSA+/+",
                                                                  "IR64"))
table(all$genotypePresumed2)
sum(table(all$genotypePresumed2))

## smaller objects for element analyses --------------

# create a smaller df with only those plants that have a ICP data
e7 <- all[is.na(all$ICPpool) == 0, ] # all genotypes
e6 <- e7[e7$lineSimple != "IR64", ]

# create separate df for looking at SATpush+Pull and for CGSpush+Pull
e.setSpp <- e7 %>%
  filter(lineSimple %in% c("WT", "SAT", "SSA", "SAT_SSA"))
table(e.setSpp$lineSimple)

e.setCpp <- e7 %>%
  filter(lineSimple %in% c("WT", "CGS", "SSA", "CGS_SSA"))
table(e.setCpp$lineSimple)



# identify column indices for elements -----------------

# create vector of element variable names
eVars <- tail(colnames(e7), 16) ### this will stop working if I add more data columns!!
indicies <- which(names(e6) %in% eVars)
startI <- min(indicies)
endI <- max(indicies)
# confirm that can use startI and endI in iterative graphing function!
ifelse((sum(startI:endI != indicies)) == 0,"inidices are in a sequence, OK proceed","check indices")



## combine data from Exp1 and Exp2? ------

# independent 2-group Mann-Whitney U Test 
# wilcox.test(y~A), where y is numeric dependent variable and A is a *binary explanatory variable
# apply wilcox.test to list of variables in e6 df
# startI and endI defined previously
outWT <- lapply(startI:endI, function(x) wilcox.test(e6[[x]] ~ e6$ExpName)) # don't worry about it not being possible to calculate exact p-values
names(outWT) <- names(e6)[startI:endI]
pval.WT <- sapply(outWT, function(x) {
  p <- x$p.value
  p
})

which(pval.WT < 0.05) # for which variables should I avoid combining Exp1 and Exp2 data? based on wilcox test?
# Cr and Fe


# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a *binary explanatory variable

# apply t.test to list of variables in e6 df
# startI and endI defined previously
outTT <- lapply(startI:endI, function(x) t.test(e6[[x]] ~ e6$ExpName))
names(outTT) <- names(e6)[startI:endI]
pval.TT <- sapply(outTT, function(x) {
  p <- x$p.value
  p
})

which(pval.TT < 0.05) # for which variables should I avoid combining Exp1 and Exp2 data?
# none, though Cr and especially Fe close to 0.05 cut off





## One way ANOVA ----
# Element data ~ lineSimple
# aov(y ~ A, data=mydataframe) where y is numeric dependent variable and A is a categorical explanatory variable
# NOTE!!! provides Type I sequential SS (NOT Type III marginal SS, default in SAS and SPSS)
# does this matter if I only have 1 explanatory variable on right side of tilda?? I don't think so

# apply ANOVA by lineSimple to list of variables in e6 df
# startI and endI defined previously
outAOV <- lapply(startI:endI, function(x) summary((aov(e6[[x]] ~ e6$lineSimple))))
names(outAOV) <- names(e6)[startI:endI]
pval.AOV <- sapply(outAOV, function(x) {
  p <- x[[1]][[5]][1]
  p
})

which(pval.AOV < 0.05) # for which variables is ANOVA x lineSimple significant? (pvalue < 0.05)
which(!pval.AOV < 0.05) # all significant except K and Na

which(pval.AOV < 0.05/16) # for which variables is ANOVA x lineSimple significant? (pvalue < 0.05/16)
which(!pval.AOV < 0.05/16) # even with crude FDR adjustment, all significant except K and Na



# apply ANOVA by lineSimple to list of variables in e.setCpp df
# startI and endI defined previously
outAOV <- lapply(startI:endI, function(x) summary((aov(e.setCpp[[x]] ~ e.setCpp$lineSimple))))
names(outAOV) <- names(e.setCpp)[startI:endI]
pval.AOV <- sapply(outAOV, function(x) {
  p <- x[[1]][[5]][1]
  p
})
pval.AOV.setCpp <- sort(pval.AOV)


which(pval.AOV < 0.05) # for which variables is ANOVA x lineSimple significant? (pvalue < 0.05) IN CGS push+pull set!
which(!pval.AOV < 0.05) # all significant except Cu, and Na

which(pval.AOV < 0.05/16) # for which variables is ANOVA x lineSimple significant? (pvalue < 0.05/16)  IN CGS push+pull set!
which(!pval.AOV < 0.05/16) # even with crude FDR adjustment, all significant except Cu, K, and Na



# apply ANOVA by lineSimple to list of variables in e.setSpp df
# startI and endI defined previously
outAOV <- lapply(startI:endI, function(x) summary((aov(e.setSpp[[x]] ~ e.setSpp$lineSimple))))
names(outAOV) <- names(e.setSpp)[startI:endI]
pval.AOV <- sapply(outAOV, function(x) {
  p <- x[[1]][[5]][1]
  p
})
pval.AOV.setSpp <- sort(pval.AOV)


which(pval.AOV < 0.05) # for which variables is ANOVA x lineSimple significant? (pvalue < 0.05) IN SAT push+pull set!
which(!pval.AOV < 0.05) # all except Ca, Fe, K, Mg, Mn, Na, and P 

which(pval.AOV < 0.05/16) # for which variables is ANOVA x lineSimple significant? (pvalue < 0.05/16)  IN SAT push+pull set!
# ONLY Al, Cu, Mo, S, Ti



## Two way ANOVA -------------

# most interested in elements that show non-additive behavoir in the double mutants
# looking for a statistically significant interaction between PushTg genotype and PullTg genotype

## on all 6 indica lines
## var1 = CGS Tg status; var2 = SSA Tg status
factor(e6$CGS.Tg_presumed)
factor(e6$SSA.Tg_presumed)

out2xANOVA <- lapply(startI:endI, function(x) anova(lm(e6[[x]] ~ e6$CGS.Tg_presumed*e6$SSA.Tg_presumed)))
names(out2xANOVA) <- names(e6)[startI:endI]

pval.AOV.CGSxSSAinteraction <- sapply(out2xANOVA, function(x) {
  p <- x[[5]][3]
  p
})

which(pval.AOV.CGSxSSAinteraction <0.05) # Al, 0.015
which(pval.AOV.CGSxSSAinteraction <0.05/length(startI:endI)) # none


## on CGS push pull set only
## var1 = CGS Tg status; var2 = SSA Tg status
factor(e.setCpp$CGS.Tg_presumed)
factor(e.setCpp$SSA.Tg_presumed)

out2xANOVA <- lapply(startI:endI, function(x) anova(lm(e.setCpp[[x]] ~ e.setCpp$CGS.Tg_presumed*e.setCpp$SSA.Tg_presumed)))
names(out2xANOVA) <- names(e.setCpp)[startI:endI]

pval.AOV.CGSxSSAinteraction <- sapply(out2xANOVA, function(x) {
  p <- x[[5]][3]
  p
})

which(pval.AOV.CGSxSSAinteraction <0.05) # none


## on all 6 indica lines
## var1 = SAT Tg status; var2 = SSA Tg status
factor(e6$SAT.Tg_presumed)
factor(e6$SSA.Tg_presumed)

out2xANOVA <- lapply(startI:endI, function(x) anova(lm(e6[[x]] ~ e6$SAT.Tg_presumed*e6$SSA.Tg_presumed)))
names(out2xANOVA) <- names(e6)[startI:endI]

pval.AOV.SATxSSAinteraction <- sapply(out2xANOVA, function(x) {
  p <- x[[5]][3]
  p
})

which(pval.AOV.SATxSSAinteraction <0.05) # K, Zn
which(pval.AOV.SATxSSAinteraction <0.05/length(startI:endI)) # K


## on SAT push pull set only
## var1 = SAT Tg status; var2 = SSA Tg status
factor(e.setSpp$SAT.Tg_presumed)
factor(e.setSpp$SSA.Tg_presumed)

out2xANOVA <- lapply(startI:endI, function(x) anova(lm(e.setSpp[[x]] ~ e.setSpp$SAT.Tg_presumed*e.setSpp$SSA.Tg_presumed)))
names(out2xANOVA) <- names(e.setSpp)[startI:endI]

pval.AOV.SATxSSAinteraction <- sapply(out2xANOVA, function(x) {
  p <- x[[5]][3]
  p
})

which(pval.AOV.SATxSSAinteraction <0.05) # K and Zn
which(pval.AOV.SATxSSAinteraction <0.05/length(startI:endI)) # none




## iteratively graph all element analysis variables (group, color, shape)-----

# create TIFF files of all individual graphs with legends
for(i in startI:endI) {
  element <- names(e6[i])
  saveName <-paste("univariate scatter", element, "legend.tiff", sep = "_")
  usp <- seed.univscatter4(e6, e6[i], e6$lineSimple, e6$lineComplete, e6$ExpName, element, "total", "umol g-1 DW", 0.1, 0)
  ggsave(saveName, width= 5, height=5, unit="in", dpi=300)
}



# create ggplot objects without legends to arrange in a grid, and save as PDF

output <- vector(mode = "list", length = length(e6))
names(output) <- names(e6)

for(i in startI:endI) {
  what <- names(e6[i])
  plot <- seed.univscatter4_noLegend(e6, e6[i], e6$lineSimple, e6$lineComplete, e6$ExpName, what, "total", "umol g-1 DW", 0.1, 0)
  print(plot)
  output[[i]] <- plot
}

objects <- output[startI:endI] ## remove empty elements of the output list
alph <- objects[order(names(objects))] ## alphabetize the objects

# graph all on same page, NO LEGEND
pdf("total element analysis_mature seeds.pdf", width = 2.25*4, height= 3.25*4)
do.call(grid.arrange, c(alph, ncol = 4)) # Draw it, note, grid will be filled by row
dev.off()


# graph all on same page, WITH LEGEND on the bottom

# extracts and draws legend
forLegend <- seed.univscatter4(e6, e6$Al_seed_mature, e6$lineSimple, e6$lineComplete, e6$ExpName,"for legend" , "total", "umol g-1 DW", 0.1, 0)
l <- g_legend(forLegend + theme(legend.position = "bottom"))
grid.newpage()
grid.draw(l)

# add legend to alph list of graphs
length(alph)
alph[[17]] <- l

# draw them
w <- 2.25*4
h <- 3.25*4 + 1.5
pdf("total element analysis_mature seeds_with legend.pdf", width = w, height= h)
grid.arrange(grobs = alph,
             widths = c(1, 1, 1, 1),
             layout_matrix = rbind(1:4, 5:8, 9:12, 13:16, 17))
dev.off()




## iteratively graph all element analysis variables with 95% confidence intervals for the mean-----


## SAT push pull set
# create TIFF files of all individual graphs
for(i in startI:endI) {
  element <- names(e.setSpp[i])
  saveName <-paste("usp", element, "setSpp", "lineSimple.group_95CI.tiff", sep = "_")
  usp <- seed.univscatter2(e.setSpp, e.setSpp[i], e.setSpp$lineSimple, element, "total", "umol g-1 DW", 0, 0)
  usp + stat_summary(fun.data = mean_ci95, geom = "errorbar", width = 0.25, color = "red", size = 0.75)
  ggsave(saveName, width= 4, height=5, unit="in", dpi=300)
}

# create ggplot objects without legends to arrange in a grid, and save as PDF
output <- vector(mode = "list", length = length(e.setSpp))
names(output) <- names(e.setSpp)

for(i in startI:endI) {
  what <- names(e.setSpp[i])
  plot <- seed.univscatter2(e.setSpp, e.setSpp[i], e.setSpp$lineSimple, what, "total", "umol g-1 DW", 0, 0)
  plot <- plot + stat_summary(fun.data = mean_ci95, geom = "errorbar", width = 0.25, color = "red", size = 0.75)
  print(plot)
  output[[i]] <- plot
}

objects <- output[startI:endI] ## remove empty elements of the output list
order.sign <- objects[names(pval.AOV.setSpp)] ## graph in ascending pvalue of ANOVA x lineSimple

pdf("total element analysis_mature seeds_SAT push pull.pdf", width = 2.5*4, height= 3.25*4)
do.call(grid.arrange, c(order.sign, ncol = 4)) # Draw it, note, grid will be filled by row
dev.off()


## CGS push pull set
# create TIFF files of all individual graphs
for(i in startI:endI) {
  element <- names(e.setCpp[i])
  saveName <-paste("usp", element, "setCpp", "lineSimple.group_95CI.tiff", sep = "_")
  usp <- seed.univscatter2(e.setCpp, e.setCpp[i], e.setCpp$lineSimple, element, "total", "umol g-1 DW", 0, 0)
  usp + stat_summary(fun.data = mean_ci95, geom = "errorbar", width = 0.25, color = "red", size = 0.75)
  ggsave(saveName, width= 4, height=5, unit="in", dpi=300)
}

# create ggplot objects without legends to arrange in a grid, and save as PDF
output <- vector(mode = "list", length = length(e.setCpp))
names(output) <- names(e.setCpp)

for(i in startI:endI) {
  what <- names(e.setCpp[i])
  plot <- seed.univscatter2(e.setCpp, e.setCpp[i], e.setCpp$lineSimple, what, "total", "umol g-1 DW", 0, 0)
  plot <- plot + stat_summary(fun.data = mean_ci95, geom = "errorbar", width = 0.25, color = "red", size = 0.75)
  print(plot)
  output[[i]] <- plot
}

objects <- output[startI:endI] ## remove empty elements of the output list
order.sign <- objects[names(pval.AOV.setCpp)] ## graph in ascending pvalue of ANOVA x lineSimple

pdf("total element analysis_mature seeds_CGS push pull.pdf", width = 2.5*4, height= 3.25*4)
do.call(grid.arrange, c(order.sign, ncol = 4)) # Draw it, note, grid will be filled by row
dev.off()





## pairwise scatter plot matrix ----
short.eVars <- sub("_.*$","",eVars) # simplify variable names

# all indica plants (e6)
df <- e6[,startI:endI] # trim data
names(df) <- short.eVars
df$lineSimple <-e6$lineSimple
df$SAT.Tg_presumed <- e6$SAT.Tg_presumed
df$SSA.Tg_presumed <- e6$SSA.Tg_presumed
df$CGS.Tg_presumed <- e6$CGS.Tg_presumed
df$ExpName <- e6$ExpName

pdf("pairwise scatter_R_element analysis_mature seeds.pdf", width = 20, height= 20)
pairs(df[,1:16], 
      lower.panel = panel.cor)
dev.off()

pdf("pairwise scatter_R_color.lineSimple_element analysis_mature seeds.pdf", width = 20, height= 20)
pairs(df[,1:16],
      upper.panel = panel.scatter,
      lower.panel = panel.cor)
dev.off()

pdf("pairwise scatter_element analysis_mature seeds.pdf", width = 20, height= 20)
pairs(df[,1:16], pch = 19, 
      lower.panel = NULL)
dev.off()

pdf("pairwise scatter_color.lineSimple_element analysis_mature seeds.pdf", width = 20, height= 20)
pairs(df[,1:16], pch = 19,
      col = myColors6[df$lineSimple],
      lower.panel = NULL)
dev.off()

pdf("pairwise scatter_color.SAT.Tgpresumed_element analysis_mature seeds.pdf", width = 20, height= 20)
pairs(df[,1:16], pch = 19,
      col = myColors4[df$SAT.Tg_presumed],
      lower.panel = NULL)
dev.off()


# notice that for some pairs of variables, there are two clouds of points

ggplot(df, aes(x = S, y = Ca, color = SAT.Tg_presumed, shape = ExpName)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = myColors4) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  theme(panel.border = element_rect(colour="black", fill=NA))
ggsave("usp_S x Ca_mature seeds.tiff", width= 4, height=3, unit="in", dpi=300)

ggplot(df, aes(x = S, y = Cu, color = SAT.Tg_presumed, shape = ExpName)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = myColors4) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  theme(panel.border = element_rect(colour="black", fill=NA))
ggsave("usp_S x Cu_mature seeds.tiff", width= 4, height=3, unit="in", dpi=300)

ggplot(df, aes(x = S, y = Mg, color = SAT.Tg_presumed, shape = ExpName)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = myColors4) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  theme(panel.border = element_rect(colour="black", fill=NA))
ggsave("usp_S x Mg_mature seeds.tiff", width= 4, height=3, unit="in", dpi=300)

ggplot(df, aes(x = S, y = Mn, color = SAT.Tg_presumed, shape = ExpName)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = myColors4) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  theme(panel.border = element_rect(colour="black", fill=NA))
ggsave("usp_S x Mn_mature seeds.tiff", width= 4, height=3, unit="in", dpi=300)

ggplot(df, aes(x = S, y = P, color = SAT.Tg_presumed, shape = ExpName)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = myColors4) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  theme(panel.border = element_rect(colour="black", fill=NA))
ggsave("usp_S x P_mature seeds.tiff", width= 4, height=3, unit="in", dpi=300)

ggplot(df, aes(x = S, y = Zn, color = SAT.Tg_presumed, shape = ExpName)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = myColors4) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  theme(panel.border = element_rect(colour="black", fill=NA))
ggsave("usp_S x Zn_mature seeds.tiff", width= 4, height=3, unit="in", dpi=300)

ggplot(df, aes(x = S, y = Ti, color = SAT.Tg_presumed, shape = ExpName)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = myColors4) +
  theme(panel.background = element_rect(fill = "white", color = "black")) +
  theme(panel.border = element_rect(colour="black", fill=NA))
ggsave("usp_S x Ti_mature seeds.tiff", width= 4, height=3, unit="in", dpi=300)




## PCA -----------

## on all 7 genotypes, including IR64
df7 <- e7[,startI:endI] # only element variables

short.eVars <- sub("_.*$","",eVars) # simplify variable names
if(length(names(df7)) == length(short.eVars)) {names(df7) <- short.eVars}
names(df7)

#prcomp{stats}
# center: if TRUE, varaibles are zero centered (column means are subtracted from the values in those columns (ignoring NAs))
# scale: if TRUE (highly advisable, note default is FALSE), variables sclaed to have unit variance
# scale: if TRUE, the centered column values are divided by the column's standard deviation

e7.PCA <- prcomp(df7, center = TRUE, scale = TRUE)
summary(e7.PCA) # what are the stdev, proportion of variance, and cumulative proportion of each PC

loadings <- e7.PCA$rotation # loading of each variable in each PC (a measure of how much a variable contributes to the PC)

sort(abs(loadings[,"PC1"]), decreasing = TRUE) # which variables are most strongly weighted in PC1
sort(loadings[,"PC1"], decreasing = TRUE)   


ev <- e7.PCA$sdev^2 # Eigenvalues = Variance of the standard variation of each PC
variance <- ev*100/sum(ev) # % total variance explained but each PC

# Barplot of the variances of the Principal Components
barplot(variance, names.arg = colnames(e7.PCA$x),
        xlab = "Principal Components",
        ylab = "Variance [%]",
        col ="steelblue")



# Eigenvalue plot
# This plot shows the eigenvalues of its PCs
# The Kaiser-Guttmann criterion: only those PCs with an eigenvalue > 1 needed for further analysis
# The Broken Stick Model: only those PCs where the variance bar is greater than the bar of the Broken Stick Model 

eigenvalue_plot(ev)


# Biplot - each observation plotted based on score for the PCs that are plotted
# Source R-files
source("ggbiplot.r")

# choices: which PCs to plot
# groups: how observations should be grouped for color and elipses
# obs.scale: scale factor to apply to observations
# var.scale: scale factor to apply to variables
# ellipse: ellipse around the data points by group
# arrow (vectors) for each variable, indicate strength and direction of correlation between variable and the PCs as well as (indirectly) correlations between varaibles
# circle = TRUE
# ellipse.prob = 0.68)
# g = g + scale_color_discrete(name = '')
# g + theme(legend.direction = 'horizontal', legend.position = 'top')


g <- ggbiplot(e7.PCA,
              choices = c(1,2),
              groups = e7$lineSimple,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              ellipse.prob = 0.80)

print(g)


## on all 6 Indica genotypes, excluding IR64
df6 <- e6[,startI:endI] # only element variables

short.eVars <- sub("_.*$","",eVars) # simplify variable names
if(length(names(df6)) == length(short.eVars)) {names(df6) <- short.eVars}
names(df6)

#prcomp{stats}
# center: if TRUE, varaibles are zero centered (column means are subtracted from the values in those columns (ignoring NAs))
# scale: if TRUE (highly advisable, note default is FALSE), variables sclaed to have unit variance
# scale: if TRUE, the centered column values are divided by the column's standard deviation

e6.PCA <- prcomp(df6, center = TRUE, scale = TRUE)
summary(e6.PCA) # what are the stdev, proportion of variance, and cumulative proportion of each PC

loadings <- e6.PCA$rotation # loading of each variable in each PC (a measure of how much a variable contributes to the PC)

sort(abs(loadings[,"PC1"]), decreasing = TRUE) # which variables are most strongly weighted in PC1
sort(loadings[,"PC1"], decreasing = TRUE)   


ev <- e6.PCA$sdev^2 # Eigenvalues = Variance of the standard variation of each PC
variance <- ev*100/sum(ev) # % total variance explained but each PC

# Barplot of the variances of the Principal Components
barplot(variance, names.arg = colnames(e6.PCA$x),
        xlab = "Principal Components",
        ylab = "Variance [%]",
        col ="steelblue")



# Eigenvalue plot
# This plot shows the eigenvalues of its PCs
# The Kaiser-Guttmann criterion: only those PCs with an eigenvalue > 1 needed for further analysis
# The Broken Stick Model: only those PCs where the variance bar is greater than the bar of the Broken Stick Model 

eigenvalue_plot(ev)


# Biplot - each observation plotted based on score for the PCs that are plotted
# Source R-files
source("ggbiplot.r")

# choices: which PCs to plot
# groups: how observations should be grouped for color and elipses
# obs.scale: scale factor to apply to observations
# var.scale: scale factor to apply to variables
# ellipse: ellipse around the data points by group
# arrow (vectors) for each variable, indicate strength and direction of correlation between variable and the PCs as well as (indirectly) correlations between varaibles
# circle = TRUE
# ellipse.prob = 0.68)
# g = g + scale_color_discrete(name = '')
# g + theme(legend.direction = 'horizontal', legend.position = 'top')


g <- ggbiplot(e6.PCA,
              choices = c(1,2),
              groups = e6$lineSimple,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              ellipse.prob = 0.6)

print(g)
ggsave("biplot_total elements seed_indica6.tiff", width= 5, height=5, unit="in", dpi=300)


## on the 4 lines in the CGS push pull set
df.Cpp <- e.setCpp[,startI:endI] # only element variables

short.eVars <- sub("_.*$","",eVars) # simplify variable names
if(length(names(df.Cpp)) == length(short.eVars)) {names(df.Cpp) <- short.eVars}
names(df.Cpp)

#prcomp{stats}
# center: if TRUE, varaibles are zero centered (column means are subtracted from the values in those columns (ignoring NAs))
# scale: if TRUE (highly advisable, note default is FALSE), variables sclaed to have unit variance
# scale: if TRUE, the centered column values are divided by the column's standard deviation

e.setCpp.PCA <- prcomp(df.Cpp, center = TRUE, scale = TRUE)
summary(e.setCpp.PCA) # what are the stdev, proportion of variance, and cumulative proportion of each PC

loadings <- e.setCpp.PCA$rotation # loading of each variable in each PC (a measure of how much a variable contributes to the PC)

sort(abs(loadings[,"PC1"]), decreasing = TRUE) # which variables are most strongly weighted in PC1
sort(loadings[,"PC1"], decreasing = TRUE)   


ev <- e.setCpp.PCA$sdev^2 # Eigenvalues = Variance of the standard variation of each PC
variance <- ev*100/sum(ev) # % total variance explained but each PC

# Barplot of the variances of the Principal Components
barplot(variance, names.arg = colnames(e.setCpp.PCA$x),
        xlab = "Principal Components",
        ylab = "Variance [%]",
        col ="steelblue")



# Eigenvalue plot
# This plot shows the eigenvalues of its PCs
# The Kaiser-Guttmann criterion: only those PCs with an eigenvalue > 1 needed for further analysis
# The Broken Stick Model: only those PCs where the variance bar is greater than the bar of the Broken Stick Model 

eigenvalue_plot(ev)


# Biplot - each observation plotted based on score for the PCs that are plotted
# Source R-files
source("ggbiplot.r")

# choices: which PCs to plot
# groups: how observations should be grouped for color and elipses
# obs.scale: scale factor to apply to observations
# var.scale: scale factor to apply to variables
# ellipse: ellipse around the data points by group
# arrow (vectors) for each variable, indicate strength and direction of correlation between variable and the PCs as well as (indirectly) correlations between varaibles
# circle = TRUE
# ellipse.prob = 0.68)
# g = g + scale_color_discrete(name = '')
# g + theme(legend.direction = 'horizontal', legend.position = 'top')


g <- ggbiplot(e.setCpp.PCA,
              choices = c(1,2),
              groups = e.setCpp$lineSimple,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              ellipse.prob = 0.6)

print(g)
ggsave("biplot_total elements seed_CGSpp.tiff", width= 5, height=5, unit="in", dpi=300)

## on the 4 lines in the SAT push pull set
df.Spp <- e.setSpp[,startI:endI] # only element variables

short.eVars <- sub("_.*$","",eVars) # simplify variable names
if(length(names(df.Spp)) == length(short.eVars)) {names(df.Spp) <- short.eVars}
names(df.Spp)

#prcomp{stats}
# center: if TRUE, varaibles are zero centered (column means are subtracted from the values in those columns (ignoring NAs))
# scale: if TRUE (highly advisable, note default is FALSE), variables sclaed to have unit variance
# scale: if TRUE, the centered column values are divided by the column's standard deviation

e.setSpp.PCA <- prcomp(df.Spp, center = TRUE, scale = TRUE)
summary(e.setSpp.PCA) # what are the stdev, proportion of variance, and cumulative proportion of each PC

loadings <- e.setSpp.PCA$rotation # loading of each variable in each PC (a measure of how much a variable contributes to the PC)

sort(abs(loadings[,"PC1"]), decreasing = TRUE) # which variables are most strongly weighted in PC1
sort(loadings[,"PC1"], decreasing = TRUE)   


ev <- e.setSpp.PCA$sdev^2 # Eigenvalues = Variance of the standard variation of each PC
variance <- ev*100/sum(ev) # % total variance explained but each PC

# Barplot of the variances of the Principal Components
barplot(variance, names.arg = colnames(e.setSpp.PCA$x),
        xlab = "Principal Components",
        ylab = "Variance [%]",
        col ="steelblue")



# Eigenvalue plot
# This plot shows the eigenvalues of its PCs
# The Kaiser-Guttmann criterion: only those PCs with an eigenvalue > 1 needed for further analysis
# The Broken Stick Model: only those PCs where the variance bar is greater than the bar of the Broken Stick Model 

eigenvalue_plot(ev)


# Biplot - each observation plotted based on score for the PCs that are plotted
# Source R-files
source("ggbiplot.r")

# choices: which PCs to plot
# groups: how observations should be grouped for color and elipses
# obs.scale: scale factor to apply to observations
# var.scale: scale factor to apply to variables
# ellipse: ellipse around the data points by group
# arrow (vectors) for each variable, indicate strength and direction of correlation between variable and the PCs as well as (indirectly) correlations between varaibles
# circle = TRUE
# ellipse.prob = 0.68)
# g = g + scale_color_discrete(name = '')
# g + theme(legend.direction = 'horizontal', legend.position = 'top')


g <- ggbiplot(e.setSpp.PCA,
              choices = c(1,2),
              groups = e.setSpp$lineSimple,
              obs.scale = 1,
              var.scale = 1,
              ellipse = TRUE,
              ellipse.prob = 0.6)

print(g)
ggsave("biplot_total elements seed_SATpp.tiff", width= 5, height=5, unit="in", dpi=300)



