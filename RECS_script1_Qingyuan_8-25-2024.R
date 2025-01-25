#Read in dataset
df = read.csv("D:/recs2020_public_v7.csv",header=TRUE)

#check the dimension
dim(df)

#clean the dataset, excluding NA and values other than 0 & 1
df_clean <- df[!is.na(df$MEDICALDEV), ]
df_clean1 <- df_clean[df_clean$MEDICALDEV<2, ]

#creating independent dataset for medical device households/ non-medical-device households
df_medicaldev_1 <- df_clean[df_clean$MEDICALDEV == 1, ]
df_medicaldev_0 <- df_clean[df_clean$MEDICALDEV == 0, ]

#box plot of household numbers by medical device households/ non-medical-device households
boxplot(NHSLDMEM~MEDICALDEV,
        data=df_clean1,
        main="Number of household member by medical device used or not",
        xlab="Medical device used/not used",
        ylab="Number of household member",
        col="steelblue",
        border="black"
)

# Kruskal test for the number of households
kruskal.test(NHSLDMEM~MEDICALDEV, data = df_clean1)

# summary table of household numbers by different Census region
table_region1<-table(df_medicaldev_1$REGIONC)
table_region1
table_region0<-table(df_medicaldev_0$REGIONC)
table_region0

#cross table by Census regions
sjt.xtab(var.row=df_clean1$REGIONC, 
         var.col=df_clean1$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

# bar plot of HH numbers by Census regions
df_region <- data.frame(Region=rep(c('MIDWEST', 'NORTHEAST','SOUTH','WEST'), each=2),
                 medicaldevice=rep(c('1', '0'), times=4),
                 hhnumber=c(536,3250,468,3143,971,5368,637,3892))
library(ggplot2)
ggplot(df_region, aes(fill=medicaldevice, y=hhnumber, x=Region)) +
  geom_bar(position='dodge', stat='identity')+
  ggtitle('Household numbers by Census region') +
  xlab('Census region') +
  ylab('Household numbers')

# Chi-square test of Census regions and states
chisq.test(table(df_clean1$REGIONC, df_clean1$MEDICALDEV))
chisq.test(table(df_clean1$state_postal, df_clean1$MEDICALDEV))

install.packages("sjPlot")
library(sjPlot)

#cross table by different states
sjt.xtab(var.row=df_clean1$state_postal, 
         var.col=df_clean1$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#box plot of Heating Degree Days(HDD)
boxplot(HDD65~MEDICALDEV,
        data=df_clean1,
        main="Heating Degree Days by medical device used or not",
        xlab="Medical device used/not used",
        ylab="Heating Degree Days",
        col="steelblue",
        border="black"
)

#box plot of Cooling Degree Days(CDD)
boxplot(CDD65~MEDICALDEV,
        data=df_clean1,
        main="Cooling Degree Days by medical device used or not",
        xlab="Medical device used/not used",
        ylab="Cooling Degree Days",
        col="steelblue",
        border="black"
)

#Histogram of HDD and CDD
hist(df_medicaldev_1$HDD65,        
     main="Distribution of HDD in medical device user households",
     xlab="HDD",
     ylab="Frequency",)

hist(df_medicaldev_1$CDD65,        
     main="Distribution of CDD in medical device user households",
     xlab="CDD",
     ylab="Frequency",)

hist(df_medicaldev_0$HDD65,        
     main="Distribution of HDD in non-medical-device households",
     xlab="HDD",
     ylab="Frequency",)

hist(df_medicaldev_0$CDD65,        
     main="Distribution of CDD in non-medical-device households",
     xlab="CDD",
     ylab="Frequency",)

#Shapiro's normality test for HDD and CDD
shapiro.test(df_clean1$HDD65[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$CDD65[df_clean1$MEDICALDEV == 1])

install.packages("rstatix")

#Wilcoxon test for HDD and CDD
wilcox_test_result <- wilcox.test(HDD65 ~ MEDICALDEV, data = df_clean1)
print(wilcox_test_result)

wilcox_test_result <- wilcox.test(CDD65 ~ MEDICALDEV, data = df_clean1)
print(wilcox_test_result)


#Shapiro's normality test of electricity consumption in KWH for different appliances
shapiro.test(df_clean1$KWHSPH[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHCOL[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHWTH[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHRFG[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHFRZ[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHCOK[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHMICRO[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHCW[df_clean1$MEDICALDEV == 1])

shapiro.test(df_clean1$KWHCDR[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHDWH[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHLGT[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHTVREL[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHHUM[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHDHUM[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHHTBPMP[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHHTBHEAT[df_clean1$MEDICALDEV == 1])
shapiro.test(df_clean1$KWHEVCHRG[df_clean1$MEDICALDEV == 1])


#Wilcoxon test of electricity consumption in KWH for different appliances 
wilcox.test(KWHSPH~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHCOL~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHWTH~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHRFG~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHFRZ~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHCOK~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHMICRO~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHCW~ MEDICALDEV, data = df_clean1)

wilcox.test(KWHCDR~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHDWH~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHLGT~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHTVREL~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHHUM~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHDHUM~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHHTBPMP~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHHTBHEAT~ MEDICALDEV, data = df_clean1)
wilcox.test(KWHEVCHRG~ MEDICALDEV, data = df_clean1)
