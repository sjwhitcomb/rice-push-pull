## packages -----
library(ggplot2)
library(dplyr)


## functions ----

# convert umol/g DW --> g/kg DW
convert1 <- function (umol, MW) {
  umol*MW/10^3
}


# convert g/kg DW --> umol/g DW
convert2 <- function (g, MW) {
  g/MW*10^3
}



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
  title <- paste(type, metN, "in seeds")
  yLabel <- units
  
  usp <- ggplot(df, aes(x = varGroup, y = varY)) +
    geom_point(size = 3, stroke = 0.25, alpha = 0.9, position = pj) +
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



seed.univscatter4 <- function (df, varY, varGroup, varColor, varShape, metN, type, units, jw, jh){
  pj <- position_jitter(width = jw, 
                        height = jh)
  title <- paste(type, metN, "in seeds")
  yLabel <- units
  
  usp <- ggplot(df, aes(x = varGroup, y = varY)) +
    geom_point(aes(color = varColor, shape = varShape), size = 3, stroke = 0.25, alpha = 0.9, position = pj) +
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



## smaller df for specific analyses --------------

# create a smaller df with only those plants that have a value for Met.total_seed_mature OR Cys.total_seed_mature
mcTot <- all[is.na(all$Met.total_seed_mature) == 0 | is.na(all$Cys.total_seed_mature) == 0, ]

# add variables ------

#total_seed_mature units: g / kg DW
#totalMolarDensity_seed_mature units: umol/ g DW
mcTot <- mcTot %>%
  mutate(Met.totalMolarDensity_seed_mature = convert2(Met.total_seed_mature, 149.2))
mcTot <- mcTot %>%
  mutate(Cys.totalMolarDensity_seed_mature = convert2(Cys.total_seed_mature, 121.2))

mcTot <- mcTot %>%
  mutate(MetCys.totalMassDensity_seed_mature = Met.total_seed_mature + Cys.total_seed_mature)
mcTot <- mcTot %>%
  mutate(MetCys.totalMolarDensity_seed_mature = Met.totalMolarDensity_seed_mature + Cys.totalMolarDensity_seed_mature)

  

# create separate df for looking at SATpush+Pull and for CGSpush+Pull
mcTot.setSpp <- mcTot %>%
  filter(lineSimple %in% c("WT", "SAT", "SSA", "SAT_SSA"))
table(mcTot.setSpp$lineSimple)

mcTot.setCpp <- mcTot %>%
  filter(lineSimple %in% c("WT", "CGS", "SSA", "CGS_SSA"))
table(mcTot.setCpp$lineSimple)




## graphs ------

# total Met in mature seeds
# group by lineSimple
# color by lineComplete
# shape by ExpName
seed.univscatter4(mcTot, mcTot$Met.total_seed_mature, mcTot$lineSimple, mcTot$lineComplete, mcTot$ExpName, "Met", "total", "g kg-1 DW", 0.1, 0.05)

# total Met in mature seeds
# group by genotypePresumed1
# color by lineComplete
# shape by ExpName
seed.univscatter4(mcTot, mcTot$Met.total_seed_mature, mcTot$genotypePresumed1, mcTot$lineComplete, mcTot$ExpName, "Met", "total", "g kg-1 DW", 0, 0.05)

# total Met in mature seeds
# group by lineComplete
# color by lineSimple
# shape by ExpName
png("usp_Met.total_seed_lineComplete.group_lineSimple.color_ExpName.shape.png", height = 600, width = 600)
seed.univscatter4(mcTot, mcTot$Met.total_seed_mature, mcTot$lineComplete, mcTot$lineSimple, mcTot$ExpName, "Met", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Met in mature seeds
# group by genotypePresumed1
# color by lineComplete
# shape by ExpName
png("usp_Met.total_seed_genotypePresumed1.group_lineComplete.color_ExpName.shape.png", height = 600, width = 600)
seed.univscatter4(mcTot, mcTot$Met.total_seed_mature, mcTot$genotypePresumed1, mcTot$lineComplete, mcTot$ExpName, "Met", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Met in mature seeds
# group by lineSimple
png("usp_Met.total_seed_lineSimple.group.png", height = 600, width = 600)
seed.univscatter2(mcTot, mcTot$Met.total_seed_mature, mcTot$lineSimple, "Met", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Met in mature seeds
# SAT push pull set ONLY
# group by lineSimple
png("usp_Met.total_seed_setSpp_lineSimple.group.png", height = 600, width = 350)
seed.univscatter2(mcTot.setSpp, mcTot.setSpp$Met.total_seed_mature, mcTot.setSpp$lineSimple, "Met", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Met in mature seeds
# CGS push pull set ONLY
# group by lineSimple
png("usp_Met.total_seed_setCpp_lineSimple.group.png", height = 600, width = 350)
seed.univscatter2(mcTot.setCpp, mcTot.setCpp$Met.total_seed_mature, mcTot.setCpp$lineSimple, "Met", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

#-


# total Cys in mature seeds
# group by lineSimple
# color by lineComplete
# shape by ExpName
seed.univscatter4(mcTot, mcTot$Cys.total_seed_mature, mcTot$lineSimple, mcTot$lineComplete, mcTot$ExpName, "Cys", "total", "g kg-1 DW", 0.1, 0.05)

# total Cys in mature seeds
# group by genotypePresumed1
# color by lineComplete
# shape by ExpName
seed.univscatter4(mcTot, mcTot$Cys.total_seed_mature, mcTot$genotypePresumed1, mcTot$lineComplete, mcTot$ExpName, "Cys", "total", "g kg-1 DW", 0, 0.05)

# total Cys in mature seeds
# group by lineComplete
# color by lineSimple
# shape by ExpName
png("usp_Cys.total_seed_lineComplete.group_lineSimple.color_ExpName.shape.png", height = 600, width = 600)
seed.univscatter4(mcTot, mcTot$Cys.total_seed_mature, mcTot$lineComplete, mcTot$lineSimple, mcTot$ExpName, "Cys", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Cys in mature seeds
# group by genotypePresumed1
# color by lineComplete
# shape by ExpName
png("usp_Cys.total_seed_genotypePresumed1.group_lineComplete.color_ExpName.shape.png", height = 600, width = 600)
seed.univscatter4(mcTot, mcTot$Cys.total_seed_mature, mcTot$genotypePresumed1, mcTot$lineComplete, mcTot$ExpName, "Cys", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Cys in mature seeds
# group by lineSimple
png("usp_Cys.total_seed_lineSimple.group.png", height = 600, width = 600)
seed.univscatter2(mcTot, mcTot$Cys.total_seed_mature, mcTot$lineSimple, "Cys", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Cys in mature seeds
# SAT push pull set ONLY
# group by lineSimple
png("usp_Cys.total_seed_setSpp_lineSimple.group.png", height = 600, width = 350)
seed.univscatter2(mcTot.setSpp, mcTot.setSpp$Cys.total_seed_mature, mcTot.setSpp$lineSimple, "Cys", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()

# total Cys in mature seeds
# CGS push pull set ONLY
# group by lineSimple
png("usp_Cys.total_seed_setCpp_lineSimple.group.png", height = 600, width = 350)
seed.univscatter2(mcTot.setCpp, mcTot.setCpp$Cys.total_seed_mature, mcTot.setCpp$lineSimple, "Cys", "total", "g kg-1 DW", 0.1, 0.05)
dev.off()


# --
# total Met + Cys in mature seeds
# group by lineSimple
# color by lineComplete
# shape by ExpName
seed.univscatter4(mcTot, mcTot$MetCys.totalMolarDensity_seed_mature, mcTot$lineSimple, mcTot$lineComplete, mcTot$ExpName, "Met + Cys", "total", "umol g-1 DW", 0.1, 0.05)
seed.univscatter4(mcTot, mcTot$MetCys.totalMassDensity_seed_mature, mcTot$lineSimple, mcTot$lineComplete, mcTot$ExpName, "Met + Cys", "total", "g kg-1 DW", 0.1, 0.05)

# total Met + Cys in mature seeds
# group by genotypePresumed1
# color by lineComplete
# shape by ExpName
seed.univscatter4(mcTot, mcTot$MetCys.totalMolarDensity_seed_mature, mcTot$genotypePresumed1, mcTot$lineComplete, mcTot$ExpName, "Met + Cys", "total", "umol g-1 DW", 0, 0.05)

# total Met + Cys in mature seeds
# group by lineComplete
# color by lineSimple
# shape by ExpName
png("usp_MetCys.totalMolarDensity_seed_lineComplete.group_lineSimple.color_ExpName.shape.png", height = 600, width = 600)
seed.univscatter4(mcTot, mcTot$MetCys.totalMolarDensity_seed_mature, mcTot$lineComplete, mcTot$lineSimple, mcTot$ExpName, "Met + Cys", "total", "umol g-1 DW", 0.1, 0.05)
dev.off()

# total Met + Cys in mature seeds
# group by genotypePresumed1
# color by lineComplete
# shape by ExpName
png("usp_MetCys.totalMolarDensity_seed_genotypePresumed1.group_lineComplete.color_ExpName.shape.png", height = 600, width = 600)
seed.univscatter4(mcTot, mcTot$MetCys.totalMolarDensity_seed_mature, mcTot$genotypePresumed1, mcTot$lineComplete, mcTot$ExpName, "Met + Cys", "total", "umol g-1 DW", 0.1, 0.05)
dev.off()

# total Met + Cys in mature seeds
# group by lineSimple
png("usp_MetCys.totalMolarDensity_seed_lineSimple.group.png", height = 600, width = 600)
seed.univscatter2(mcTot, mcTot$MetCys.totalMolarDensity, mcTot$lineSimple, "Met + Cys", "total", "umol g-1 DW", 0.1, 0.05)
dev.off()

# total Met + Cys in mature seeds
# SAT push pull set ONLY
# group by lineSimple
png("usp_MetCys.totalMolarDensity_seed_setSpp_lineSimple.group.png", height = 600, width = 350)
seed.univscatter2(mcTot.setSpp, mcTot.setSpp$MetCys.totalMolarDensity, mcTot.setSpp$lineSimple, "Met + Cys", "total", "umol g-1 DW", 0.1, 0.05)
dev.off()

# total Met + Cys in mature seeds
# CGS push pull set ONLY
# group by lineSimple
png("usp_MetCys.totalMolarDensity_seed_setCpp_lineSimple.group.png", height = 600, width = 350)
seed.univscatter2(mcTot.setCpp, mcTot.setCpp$MetCys.totalMolarDensity, mcTot.setCpp$lineSimple, "Met + Cys", "total", "umol g-1 DW", 0.1, 0.05)
dev.off()




# calculate mid-parent heterosis for total seed Methioine ---------------
# MPH = (PushPull - MP)/MP
# MP = mean(push, pull parental plants)


# trait = total seed Met

# in all CGS_SSA plants
parents <- mcTot.setCpp %>%
  filter(lineSimple %in% c("CGS", "SSA"))
MP <- mean(parents$Met.total_seed_mature)

double.CGS <- mcTot.setCpp %>%
  filter(lineSimple == "CGS_SSA") %>%
  mutate(MPH = (Met.total_seed_mature-MP)/MP)

t.test(double.CGS$MPH, mu =0)

# in all SAT_SSA plants
parents <- mcTot.setSpp %>%
  filter(lineSimple %in% c("SAT", "SSA"))
MP <- mean(parents$Met.total_seed_mature)

double.SAT <- mcTot.setSpp %>%
  filter(lineSimple == "SAT_SSA") %>%
  mutate(MPH = (Met.total_seed_mature-MP)/MP)

t.test(double.SAT$MPH, mu =0)


# trait = total seed Cys

# in all CGS_SSA plants
parents <- mcTot.setCpp %>%
  filter(lineSimple %in% c("CGS", "SSA"))
MP <- mean(parents$Cys.total_seed_mature)

double.CGS <- mcTot.setCpp %>%
  filter(lineSimple == "CGS_SSA") %>%
  mutate(MPH = (Cys.total_seed_mature-MP)/MP)

t.test(double.CGS$MPH, mu =0)

# in all SAT_SSA plants
parents <- mcTot.setSpp %>%
  filter(lineSimple %in% c("SAT", "SSA"))
MP <- mean(parents$Cys.total_seed_mature)

double.SAT <- mcTot.setSpp %>%
  filter(lineSimple == "SAT_SSA") %>%
  mutate(MPH = (Cys.total_seed_mature-MP)/MP)

t.test(double.SAT$MPH, mu =0)


# trait = total seed Met + Cys (molar density)

# in all CGS_SSA plants
parents <- mcTot.setCpp %>%
  filter(lineSimple %in% c("CGS", "SSA"))
MP <- mean(parents$MetCys.totalMolarDensity_seed_mature)

double.CGS <- mcTot.setCpp %>%
  filter(lineSimple == "CGS_SSA") %>%
  mutate(MPH = (MetCys.totalMolarDensity_seed_mature-MP)/MP)

t.test(double.CGS$MPH, mu =0)

# in all SAT_SSA plants
parents <- mcTot.setSpp %>%
  filter(lineSimple %in% c("SAT", "SSA"))
MP <- mean(parents$MetCys.totalMolarDensity_seed_mature)

double.SAT <- mcTot.setSpp %>%
  filter(lineSimple == "SAT_SSA") %>%
  mutate(MPH = (MetCys.totalMolarDensity_seed_mature-MP)/MP)

t.test(double.SAT$MPH, mu =0)



# summary statistics -----------
mcTot %>%
  group_by(lineSimple) %>%
  summarize(mean = mean(Met.total_seed_mature), sd = sd(Met.total_seed_mature), n = n())

mcTot %>%
  group_by(lineSimple) %>%
  summarize(mean = mean(Cys.total_seed_mature), sd = sd(Cys.total_seed_mature), n = n())

# t-testing -----------
df <- mcTot %>%
  select(lineSimple, Met.total_seed_mature, Cys.total_seed_mature, MetCys.totalMolarDensity_seed_mature)

SAT <- df %>%
  filter(lineSimple == "SAT")
CGS <- df %>%
  filter(lineSimple == "CGS")
SSA <- df %>%
  filter(lineSimple == "SSA")
CGS_SSA <- df %>%
  filter(lineSimple == "CGS_SSA")
SAT_SSA <- df %>%
  filter(lineSimple == "SAT_SSA")


t.test(CGS$Met.total_seed_mature, CGS_SSA$Met.total_seed_mature)
t.test(SSA$Met.total_seed_mature, CGS_SSA$Met.total_seed_mature)

t.test(SAT$Cys.total_seed_mature, SAT_SSA$Cys.total_seed_mature)
t.test(CGS$Cys.total_seed_mature, CGS_SSA$Cys.total_seed_mature)

t.test(CGS$MetCys.totalMolarDensity_seed_mature, CGS_SSA$MetCys.totalMolarDensity_seed_mature)
t.test(SSA$MetCys.totalMolarDensity_seed_mature, CGS_SSA$MetCys.totalMolarDensity_seed_mature)
