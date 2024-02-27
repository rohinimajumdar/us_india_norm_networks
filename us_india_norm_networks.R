library(psych)
library(effsize)
library(reshape2)
library(lme4)
library(lmerTest)
library(effects)
library(rmeta)
library(ggplot2)
library(igraph)
library(qgraph)
library(GGally)
library(data.table)
library(ggcorrplot)
library(dplyr)
library(smacof)
library(wordcloud)
library(tidyverse)
library(apaTables)
library(cluster)
library(factoextra)
library(amap)
library(xlsx)
library(ade4)
library(EGAnet)
library(vegan)
library(networktools)
library(openxlsx)
library(corrplot)

# Read in raw data

us1 <- read.csv(file = 'us_norm_networks_data.csv')
ind1 <- read.csv(file = 'india_norm_networks_data.csv')

# Remove top row

us2 <- us1[-c(1),]
ind2 <- ind1[-c(1),]

# Keep only common items

common_us <- c("sdacown", "sdacee", "sdacee2x", "sdacnotnec", "sdacwant", "sdacss", "sdacnotworth",
               "sdacqol", "sdacnotbad", "siwastee", "sidivest", "sirenslow", "sirenair", "siglobal",
               "siccdrastic", "sisoc", "sicccit", "siccgov", "siccrich", "siccus", "sicclux", "sieeworth",
               "siwestcut", "siev", "sisolar", "sisolarenv", "sifoodwaste", "simeat", "sidevair", "siairpol",
               "sinewbldg", "sirenter", "sirichstd", "sisuccess", "sicomfort", "sisaving", "siwaste", "dacown",
               "dacee", "dacee2x", "dacnotnec", "dacss", "dacnotworth", "dacqol", "dacnotbad", "pacee2x",
               "pacnotnec", "pacss", "pacnotworth", "pacqol", "pacnotbad", "bacvsother", "bpayeeac", "bpubtrans",
               "age", "edu", "income", "monthlyexp", "n4wheel", "n2wheel", "state", "gender", "employ",
               "marital", "home")

common_ind <- c("sdacown", "sdacee", "sdacee2x", "sdacnotnec", "sdacwant", "sdacss", "sdacnotworth",
                "sdacqol", "sdacnotbad", "siwastee", "sidivest", "sirenslow", "sirenair", "siglobal",
                "siccdrastic", "sisoc", "sicccit", "siccgov", "siccrich", "siccind", "sicclux", "sieeworth",
                "siwestcut", "siev", "sisolar", "sisolarenv", "sifoodwaste", "simeat", "sidevair", "siairpol",
                "sinewbldg", "sirenter", "sirichstd", "sisuccess", "sicomfort", "sisaving", "siwaste", "dacown",
                "dacee", "dacee2x", "dacnotnec", "dacss", "dacnotworth", "dacqol", "dacnotbad", "pacee2x",
                "pacnotnec", "pacss", "pacnotworth", "pacqol", "pacnotbad", "bacvsother", "bpayeeac", "bpubtrans",
                "age", "edu", "income", "monthlyexp", "n4wheel", "n2wheel", "state", "gender", "employ",
                "marital", "home")

us <- us2[common_us]
ind <- ind2[common_ind]

# Save data files

write.csv(us, "us_norm_networks_data.csv", row.names = FALSE)
write.csv(ind, "india_norm_networks_data.csv", row.names = FALSE)

# Convert to numeric
i <- c(1:60)
us[ , i] <- apply(us[ , i], 2, function(x) as.numeric(as.character(x)))
ind[ , i] <- apply(ind[ , i], 2, function(x) as.numeric(as.character(x)))

# Convert to factor

factor_columns <- c("state", "gender", "employ", "marital", "home")
us[factor_columns] <- lapply(us[factor_columns], as.factor)
ind[factor_columns] <- lapply(ind[factor_columns], as.factor)

# Recode states

us$state <- factor(us$state, levels = c("33", "45"), labels = c("NY", "TX"))
ind$state <- factor(ind$state, levels = c("14", "32"), labels = c("MH", "DL"))

# Replace missing values with 0 in n4wheel
us$n4wheel[is.na(us$n4wheel)] <- 0
ind$n4wheel[is.na(ind$n4wheel)] <- 0

# Replace missing values with 0 in n2wheel
us$n2wheel[is.na(us$n2wheel)] <- 0
ind$n2wheel[is.na(ind$n2wheel)] <- 0

# Reassign employment levels
us$employ <- factor(us$employ, levels = c("private","self","gov","student","unemploy","retire","housewife","handi","other"))
ind$employ <- factor(ind$employ, levels = c("private","self","gov","student","unemploy","retire","housewife","handi","other"))

# Difference in demographic composition
demo <- c("male", "female", "age_18-25", "age_26-35", "age_36-45", "age_46-55", "age_56-65", "age_66-up")
demographics <- data.frame(demo)
demographics$usa <- 0
demographics$india <- 0
demographics$usa[demographics$demo == "male"] <- sum(us$gender == 1)
demographics$india[demographics$demo == "male"] <- sum(ind$gender == 1)
demographics$usa[demographics$demo == "female"] <- sum(us$gender == 2)
demographics$india[demographics$demo == "female"] <- sum(ind$gender == 2)
demographics$usa[demographics$demo == "age_18-25"] <- sum(us$age >= 18 & us$age <= 25)
demographics$india[demographics$demo == "age_18-25"] <- sum(ind$age >= 18 & ind$age <= 25)
demographics$usa[demographics$demo == "age_26-35"] <- sum(us$age >= 26 & us$age <= 35)
demographics$india[demographics$demo == "age_26-35"] <- sum(ind$age >= 26 & ind$age <= 35)
demographics$usa[demographics$demo == "age_36-45"] <- sum(us$age >= 36 & us$age <= 45)
demographics$india[demographics$demo == "age_36-45"] <- sum(ind$age >= 36 & ind$age <= 45)
demographics$usa[demographics$demo == "age_46-55"] <- sum(us$age >= 46 & us$age <= 55)
demographics$india[demographics$demo == "age_46-55"] <- sum(ind$age >= 46 & ind$age <= 55)
demographics$usa[demographics$demo == "age_56-65"] <- sum(us$age >= 56 & us$age <= 65)
demographics$india[demographics$demo == "age_56-65"] <- sum(ind$age >= 56 & ind$age <= 65)
demographics$usa[demographics$demo == "age_66-up"] <- sum(us$age >= 66)
demographics$india[demographics$demo == "age_66-up"] <- sum(ind$age >= 66)
contingency_table <- as.table(matrix(c(demographics$usa, demographics$india), nrow = 8))
chi_squared_result <- chisq.test(contingency_table)
print(chi_squared_result)

# REGRESSIONS

# Demographics

# US
us.d.bacvsother <- lm(bacvsother~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.d.bacvsother)
us.d.bpayeeac <- lm(bpayeeac~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.d.bpayeeac)
us.d.bpubtrans <- lm(bpubtrans~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.d.bpubtrans)

# India
ind.d.bacvsother <- lm(bacvsother~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.d.bacvsother)
ind.d.bpayeeac <- lm(bpayeeac~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.d.bpayeeac)
ind.d.bpubtrans <- lm(bpubtrans~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.d.bpubtrans)

# Attitudes

# US
us.a.bacvsother <- lm(bacvsother~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad,data=us)
summary(us.a.bacvsother)
us.a.bpayeeac <- lm(bpayeeac~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad,data=us)
summary(us.a.bpayeeac)
us.a.bpubtrans <- lm(bpubtrans~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad,data=us)
summary(us.a.bpubtrans)

# India
ind.a.bacvsother <- lm(bacvsother~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad,data=ind)
summary(ind.a.bacvsother)
ind.a.bpayeeac <- lm(bpayeeac~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad,data=ind)
summary(ind.a.bpayeeac)
ind.a.bpubtrans <- lm(bpubtrans~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad,data=ind)
summary(ind.a.bpubtrans)

# Norms

# US
us.n.bacvsother <- lm(bacvsother~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                      +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccus+sicclux
                      +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                        +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                      +dacqol+dacnotbad,data=us) 
summary(us.n.bacvsother)
us.n.bpayeeac <- lm(bpayeeac~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                    +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccus+sicclux
                    +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                      +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                    +dacqol+dacnotbad,data=us)
summary(us.n.bpayeeac)
us.n.bpubtrans <- lm(bpubtrans~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                     +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccus+sicclux
                     +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                       +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                     +dacqol+dacnotbad,data=us)
summary(us.n.bpubtrans)

# India
ind.n.bacvsother <- lm(bacvsother~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                       +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                       +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                         +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                       +dacqol+dacnotbad,data=ind)
summary(ind.n.bacvsother)
ind.n.bpayeeac <- lm(bpayeeac~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                     +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                     +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                       +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                     +dacqol+dacnotbad,data=ind)
summary(ind.n.bpayeeac)
ind.n.bpubtrans <- lm(bpubtrans~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                      +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                      +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                        +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                      +dacqol+dacnotbad,data=ind)
summary(ind.n.bpubtrans)

# Demographics + Attitudes

# US
us.da.bacvsother <- lm(bacvsother~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                       +n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.da.bacvsother)
us.da.bpayeeac <- lm(bpayeeac~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                     +n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.da.bpayeeac)
us.da.bpubtrans <- lm(bpubtrans~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                      +n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.da.bpubtrans)

# India
ind.da.bacvsother <- lm(bacvsother~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                        +n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.da.bacvsother)
ind.da.bpayeeac <- lm(bpayeeac~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                      +n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.da.bpayeeac)
ind.da.bpubtrans <- lm(bpubtrans~pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                       +n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.da.bpubtrans)

# Demographics + Attitudes + Norms

# US
us.dan.bacvsother <- lm(bacvsother~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                        +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccus+sicclux
                        +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                          +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                        +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                        +n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.dan.bacvsother)
us.dan.bpayeeac <- lm(bpayeeac~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                      +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccus+sicclux
                      +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                        +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                      +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                      +n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.dan.bpayeeac)
us.dan.bpubtrans <- lm(bpubtrans~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                       +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccus+sicclux
                       +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                         +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                       +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                       +n4wheel+n2wheel+state+gender+employ+marital+home,data=us)
summary(us.dan.bpubtrans)

# India
ind.dan.bacvsother <- lm(bacvsother~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                         +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                         +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                           +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                         +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                         +n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.dan.bacvsother)
ind.dan.bpayeeac <- lm(bpayeeac~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                       +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                       +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                         +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                       +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                       +n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.dan.bpayeeac)
ind.dan.bpubtrans <- lm(bpubtrans~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                        +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                        +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                          +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                        +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                        +n4wheel+n2wheel+state+gender+employ+marital+home,data=ind)
summary(ind.dan.bpubtrans)

# Descriptive statistics to explain low norms adjusted R-squared
describe(us$bacvsother)
describe(ind$bacvsother)
describe(us$bpubtrans)
describe(ind$bpubtrans)

# Correlations
us.nab.corr <- corr.test(us[1:54])
cor_matrix <- us.nab.corr$r
p_values_matrix <- us.nab.corr$p
us.nab.r.p <- data.frame(Correlation = as.vector(cor_matrix), P_Value = as.vector(p_values_matrix))
us.nab.r.p <- us.nab.r.p[!(us.nab.r.p$Correlation == 1 | us.nab.r.p$P_Value > 0.05), ]

ind.nab.corr <- corr.test(ind[1:54])
cor_matrix <- ind.nab.corr$r
p_values_matrix <- ind.nab.corr$p
ind.nab.r.p <- data.frame(Correlation = as.vector(cor_matrix), P_Value = as.vector(p_values_matrix))
ind.nab.r.p <- ind.nab.r.p[!(ind.nab.r.p$Correlation == 1 | ind.nab.r.p$P_Value > 0.05), ]

# Plots

# US

egaUS <- EGA(data = us[1:54], labels=c(names), plot.type = "GGally")
#write.xlsx(egaUS$wc, "new_usa_ega.xlsx", sheetName = "Sheet1", append = TRUE)
new_usa_key <- read.csv("new_usa_key.csv")
us.nab.corr <- corr.test(us[1:54])
us.dissim <- sim2diss(abs(us.nab.corr$r))
USnabDV_MDS <- mds(us.dissim)
USnabDV_MDS_mspline <- mds(us.dissim, type="mspline")
#plot(USnabDV_MDS_mspline, plot.type = "Shepard", main="Spline")
qgraph(us.nab.corr$r, layout=USnabDV_MDS_mspline$conf, diag=FALSE, color=new_usa_key$color, vsize = 5, minimum=0.0, labels=c(1:54), legend=FALSE, GLratio = 5)
text(-1,-1, paste("Stress=", round(USnabDV_MDS_mspline$stress,2)))
text(-0,+1.2, paste("Clusters of energy-related norms, attitudes, and behaviors in the US"))

# India

egaIND <- EGA(data = ind[1:54], labels=c(names), plot.type = "GGally")
#write.xlsx(egaIND$wc, "new_india_ega.xlsx", sheetName = "Sheet1", append = TRUE)
new_india_key <- read.csv("new_india_key.csv")
ind.nab.corr <- corr.test(ind[1:54])
ind.dissim <- sim2diss(abs(ind.nab.corr$r))
INDnabDV_MDS <- mds(ind.dissim)
INDnabDV_MDS_mspline <- mds(ind.dissim, type="mspline")
#plot(INDnabDV_MDS_mspline, plot.type = "Shepard", main="Spline")
qgraph(ind.nab.corr$r, layout=INDnabDV_MDS_mspline$conf, diag=FALSE, color=new_india_key$color, vsize = 5, minimum=0.0, palette = "rainbow", labels=c(1:54), legend=FALSE, GLratio = 5)
text(-1,-1, paste("Stress=", round(INDnabDV_MDS_mspline$stress,2)))
text(-0,+1.2, paste("Clusters of energy-related norms, attitudes, and behaviors in India"))

# Procrustes

fit_procrustes <- Procrustes(USnabDV_MDS_mspline$conf, INDnabDV_MDS_mspline$conf)
qgraph(us.nab.corr$r, layout=fit_procrustes$X, diag=FALSE, color=new_usa_key$color, shape=new_usa_key$shape, vsize = 5, minimum=0.0, palette = "rainbow", labels=c(1:54), legend=FALSE, GLratio = 5)
text(-1,-1, paste("Stress=", round(USnabDV_MDS_mspline$stress,2)))
qgraph(ind.nab.corr$r, layout=fit_procrustes$Yhat, diag=FALSE, color=new_india_key$color, shape=new_india_key$shape, vsize = 5, minimum=0.0, palette = "rainbow", labels=c(1:54), legend=FALSE, GLratio = 5)
text(-1,-1, paste("Stress=", round(INDnabDV_MDS_mspline$stress,2)))

# Jennrich

cortest.jennrich(cor(ind[1:54]),cor(us[1:54]),n1=2134, n2=2076) # sig different (more so than within state; 8k vs 7k)

# Mantel
INDdist <- as.dist(sim2diss(abs(ind.nab.corr$r)))
USdist <- as.dist(sim2diss(abs(us.nab.corr$r)))
mantel.rtest(INDdist, USdist, nrepet = 9999) # sig similar structure (less so than within state; ~0.6 vs ~0.8)

ttest.sdacown <- t.test(us$sdacown, ind$sdacown)
print(ttest.sdacown)
wilcox.sdacown <- wilcox.test(us$sdacown, ind$sdacown)
print(wilcox.sdacown)

ttest.sdacss <- t.test(us$sdacss, ind$sdacss)
print(ttest.sdacss)
wilcox.sdacss <- wilcox.test(us$sdacown, ind$sdacown)
print(wilcox.sdacss)

# India language comparison
ind_nums <- c("sub", "lang", "sdacown", "sdacee", "sdacee2x", "sdacnotnec", "sdacwant", "sdacss", "sdacnotworth",
              "sdacqol", "sdacnotbad", "siwastee", "sidivest", "sirenslow", "sirenair", "siglobal",
              "siccdrastic", "sisoc", "sicccit", "siccgov", "siccrich", "siccind", "sicclux", "sieeworth",
              "siwestcut", "siev", "sisolar", "sisolarenv", "sifoodwaste", "simeat", "sidevair", "siairpol",
              "sinewbldg", "sirenter", "sirichstd", "sisuccess", "sicomfort", "sisaving", "siwaste", "dacown",
              "dacee", "dacee2x", "dacnotnec", "dacss", "dacnotworth", "dacqol", "dacnotbad", "pacee2x",
              "pacnotnec", "pacss", "pacnotworth", "pacqol", "pacnotbad", "bacvsother", "bpayeeac", "bpubtrans")
ind_lang <- ind2[ind_nums]
ind.en <- ind_lang[ind_lang$lang == "EN", ] 
ind.hi <- ind_lang[ind_lang$lang == "HI", ]
ind.en[, -c(1, 2)] <- apply(ind.en[, -c(1, 2)], 2, as.numeric)
ind.hi[, -c(1, 2)] <- apply(ind.hi[, -c(1, 2)], 2, as.numeric)

# Get the common numeric columns
common_numeric_cols <- intersect(names(ind.en)[-(1:2)], names(ind.hi)[-(1:2)])

# Initialize a list to store the significant results
significant_results <- list()

# Loop through each common numeric column
for (col_name in common_numeric_cols) {
  # Perform the Welch's t-test
  result <- t.test(ind.en[[col_name]], ind.hi[[col_name]], var.equal = FALSE)
  
  # Check if p-value is less than 0.05
  if (result$p.value < 0.05) {
    # Store the significant result with column names as identifier
    significant_results[[paste(col_name, "_vs_", col_name, sep = "")]] <- result
  }
}

# Create a table of significant results
significant_table <- data.frame(
  Comparison = names(significant_results),
  p_value = sapply(significant_results, function(x) x$p.value)
)

# Get the common numeric columns
common_numeric_cols <- intersect(names(ind.en)[-(1:2)], names(ind.hi)[-(1:2)])

# Initialize lists to store the significant results, means, and directions
significant_results <- list()
means <- list()
directions <- list()

# Loop through each common numeric column
for (col_name in common_numeric_cols) {
  # Perform the Welch's t-test
  result <- t.test(ind.en[[col_name]], ind.hi[[col_name]], var.equal = FALSE)
  
  # Check if p-value is less than 0.05
  if (result$p.value < 0.05) {
    # Store the significant result with column names as identifier
    significant_results[[paste(col_name, "_vs_", col_name, sep = "")]] <- result
    
    # Calculate and store means
    en_mean <- mean(ind.en[[col_name]])
    hi_mean <- mean(ind.hi[[col_name]])
    means[[paste(col_name, "_mean_en", sep = "")]] <- en_mean
    means[[paste(col_name, "_mean_hi", sep = "")]] <- hi_mean
    
    # Determine the direction
    direction <- en_mean > hi_mean
    directions[[paste(col_name, "_direction", sep = "")]] <- direction
  }
}

# Combine significant results, means, and directions into a single dataframe
significant_table <- data.frame(
  Comparison = names(significant_results),
  p_value = sapply(significant_results, function(x) x$p.value),
  en_mean = unlist(means[grep("_mean_en", names(means))]),
  hi_mean = unlist(means[grep("_mean_hi", names(means))]),
  direction = unlist(directions)
)

# HI mean > EN mean for: siccgov siev sisolar siairpol pacnotbad 

# Bonferroni corrections

# Get the common numeric columns
common_numeric_cols <- intersect(names(ind.en)[-(1:2)], names(ind.hi)[-(1:2)])

# Set the desired alpha level
alpha <- 0.05

# Initialize lists to store the significant results, means, and directions
significant_results <- list()
means <- list()
directions <- list()

# Loop through each common numeric column
for (col_name in common_numeric_cols) {
  # Perform the Welch's t-test
  result <- t.test(ind.en[[col_name]], ind.hi[[col_name]], var.equal = FALSE)
  
  # Check if p-value after Bonferroni correction is less than alpha
  if (result$p.value < alpha / length(common_numeric_cols)) {
    # Store the significant result with column names as identifier
    significant_results[[paste(col_name, "_vs_", col_name, sep = "")]] <- result
    
    # Calculate and store means
    en_mean <- mean(ind.en[[col_name]])
    hi_mean <- mean(ind.hi[[col_name]])
    means[[paste(col_name, "_mean_en", sep = "")]] <- en_mean
    means[[paste(col_name, "_mean_hi", sep = "")]] <- hi_mean
    
    # Determine the direction
    direction <- en_mean > hi_mean
    directions[[paste(col_name, "_direction", sep = "")]] <- direction
  }
}

# Combine significant results, means, and directions into a single dataframe
significant_table <- data.frame(
  Comparison = names(significant_results),
  p_value = sapply(significant_results, function(x) x$p.value),
  en_mean = unlist(means[grep("_mean_en", names(means))]),
  hi_mean = unlist(means[grep("_mean_hi", names(means))]),
  direction = unlist(directions)
)

# Regressions with language as a predictor

ind_extra <- c("sdacown", "sdacee", "sdacee2x", "sdacnotnec", "sdacwant", "sdacss", "sdacnotworth",
               "sdacqol", "sdacnotbad", "siwastee", "sidivest", "sirenslow", "sirenair", "siglobal",
               "siccdrastic", "sisoc", "sicccit", "siccgov", "siccrich", "siccind", "sicclux", "sieeworth",
               "siwestcut", "siev", "sisolar", "sisolarenv", "sifoodwaste", "simeat", "sidevair", "siairpol",
               "sinewbldg", "sirenter", "sirichstd", "sisuccess", "sicomfort", "sisaving", "siwaste", "dacown",
               "dacee", "dacee2x", "dacnotnec", "dacss", "dacnotworth", "dacqol", "dacnotbad", "pacee2x",
               "pacnotnec", "pacss", "pacnotworth", "pacqol", "pacnotbad", "bacvsother", "bpayeeac", "bpubtrans",
               "age", "edu", "income", "monthlyexp", "n4wheel", "n2wheel", "state", "gender", "employ",
               "marital", "home", "lang")
ind_lang <- ind2[ind_extra]

# Convert to numeric
i <- c(1:60)
ind_lang[ , i] <- apply(ind[ , i], 2, function(x) as.numeric(as.character(x)))

# Convert to factor

lang_factor_columns <- c("state", "gender", "employ", "marital", "home", "lang")
ind_lang[lang_factor_columns] <- lapply(ind_lang[lang_factor_columns], as.factor)

# Replace missing values with 0 in n4wheel
ind_lang$n4wheel[is.na(ind_lang$n4wheel)] <- 0

# Replace missing values with 0 in n2wheel
ind_lang$n2wheel[is.na(ind_lang$n2wheel)] <- 0

# Reassign employment levels
ind_lang$employ <- factor(ind_lang$employ, levels = c("private","self","gov","student","unemploy","retire","housewife","handi","other"))

# Regressions with language added

# Demographics

ind.d.bacvsother <- lm(bacvsother~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home+lang,data=ind_lang)
summary(ind.d.bacvsother)
ind.d.bpayeeac <- lm(bpayeeac~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home+lang,data=ind_lang)
summary(ind.d.bpayeeac)
ind.d.bpubtrans <- lm(bpubtrans~age+edu+income+monthlyexp+n4wheel+n2wheel+state+gender+employ+marital+home+lang,data=ind_lang)
summary(ind.d.bpubtrans)

# Demographics + attitudes + norms
ind.dan.bacvsother <- lm(bacvsother~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                         +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                         +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                           +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                         +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                         +n4wheel+n2wheel+state+gender+employ+marital+home+lang,data=ind_lang)
summary(ind.dan.bacvsother)
ind.dan.bpayeeac <- lm(bpayeeac~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                       +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                       +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                         +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                       +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                       +n4wheel+n2wheel+state+gender+employ+marital+home+lang,data=ind_lang)
summary(ind.dan.bpayeeac)
ind.dan.bpubtrans <- lm(bpubtrans~sdacown+sdacee+sdacee2x+sdacnotnec+sdacwant+sdacss+sdacnotworth+sdacqol+sdacnotbad+siwastee
                        +sidivest+sirenslow+sirenair+siglobal+siccdrastic+sisoc+sicccit+siccgov+siccrich+siccind+sicclux
                        +sieeworth+siwestcut+siev+sisolar+sisolarenv+sifoodwaste+simeat+sidevair+siairpol+sinewbldg+sirenter+
                          +sirichstd+sisuccess+sicomfort+sisaving+siwaste+dacown+dacee+dacee2x+dacnotnec+dacss+dacnotworth
                        +dacqol+dacnotbad+pacee2x+pacnotnec+pacss+pacnotworth+pacqol+pacnotbad+age+edu+income+monthlyexp
                        +n4wheel+n2wheel+state+gender+employ+marital+home+lang,data=ind_lang)
summary(ind.dan.bpubtrans)

# Pluralistic ignorance: US

# attitudes: pacee2x pacnotnec pacss pacnotworth pacqol pacnotbad

mean(us$pacee2x > 4) * 100 # 54%
mean(us$pacnotnec > 4) * 100 # 19%
mean(us$pacss > 4) * 100 # 35%
mean(us$pacnotworth > 4) * 100 # 16%
mean(us$pacqol > 4) * 100 # 79%
mean(us$pacnotbad > 4) * 100 # 47%

# sd norms: sdacee2x sdacnotnec sdacss sdacnotworth sdacqol sdacnotbad

describe(us$sdacee2x) # 3.72 = 33.33-50%
describe(us$sdacnotnec) # 2.49 = 16.67-33.33%
describe(us$sdacss) # 3.90 = 33.33-50%
describe(us$sdacnotworth) # 2.74 = 16.67-33.33%
describe(us$sdacqol) # 5.44 = 66.67-83.33%
describe(us$sdacnotbad) # 4.72 = 50-66.67%

# Pluralistic ignorance: India

# attitudes: pacee2x pacnotnec pacss pacnotworth pacqol pacnotbad

mean(ind$pacee2x > 4) * 100 # 72%
mean(ind$pacnotnec > 4) * 100 # 46%
mean(ind$pacss > 4) * 100 # 63%
mean(ind$pacnotworth > 4) * 100 # 39%
mean(ind$pacqol > 4) * 100 # 71%
mean(ind$pacnotbad > 4) * 100 # 41%

# sd norms: sdee2x sdacnotnec sdacss sdacnotworth sdacqol sdacnotbad

describe(ind$sdacee2x) # 4.21 = 50-66.67%
describe(ind$sdacnotnec) # 3.42 = 33.33-50%
describe(ind$sdacss) # 5.1 = 66.67-83.33%
describe(ind$sdacnotworth) # 3.64 = 33.33-50%
describe(ind$sdacqol) # 4.91 = 50-66.67%
describe(ind$sdacnotbad) # 4.07 = 50-66.67%

