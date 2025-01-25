#Read in dataset
#setwd("G:/My Drive/Teaching/CEE298 - Energy Data and Modeling/Week 2 - Energy Systems & Electricity Markets (Data)")
df = read.csv("recs2020_public_v7.csv",header=TRUE)

#confirm dimensions of the dataset
dim(df)

#We are primarily interested about disparities faced by households (HHs) with medical devices.
#The 2020 RECS survey asked HHs if they had a medical device:
#Question: Are any medical devices used in your home, such as nebulizers, CPAP machines, or ventilators? 
#<https://www.eia.gov/survey/form/eia_457/RECS%20457%20A_Questionnaire%20Content.pdf>


#Remove any HHs with no data for this variable of interest
#remove NAs from df$MEDICALDEV
df_clean <- df[!is.na(df$MEDICALDEV), ]


#Create separate dfs
df_medicaldev_1 <- df_clean[df_clean$MEDICALDEV == 1, ]
df_medicaldev_0 <- df_clean[df_clean$MEDICALDEV == 0, ]

#Percent of HHs with medical devices = 14.1436%
sum(df_medicaldev_1$MEDICALDEV)/dim(df)[1]*100

#statistical tests
#We want to compare medical device HHs to the general population. 
#There are a few statistical tests to use, depending on the cross-variable type.

#A 2x2 chi-square test is used when there are two levels of the independent variable & two levels of the dependent variable. 
#Chi-square test is useful in cases with more than two categories.
#A Fisher's exact tests is used when you wish to conduct a chi-square test but one or more of the cells (in the cross-tab matrix) has an expected frequency of five or less.
#One-way ANOVA is used when you have a categorical independent variable (with two or more categories) and a normally distributed interval dependent variable.
## An example could be to test whether the mean of KWH differs between the two or more categories...
#Kruskal Wallis test is when you have one independent variable with two or more levels and an ordinal dependent variable.
#Wilcoxon signed rank sum test (Mann-Whitney U test) is a non-parametric version of a paired sample t-test (requires continuous data)
#McNemar's test is used if you are interested in the marginal frequencies of two binary outcomes. The null hypothesis is that the cross-tabs table is symmetric.


#TOTAL ELECTRICITY USE (KWH)
#see if there's a statistically significant difference in electricity consumption between HHs with medical devices and the general population.

#Shapiro-Wilk normality test
#The W statistic is a measure of how well the data fits a normal distribution. A value of 1 indicates perfect normality.
#If the the p-value is significantly less than 0.05, you reject the null hypothesis that the data comes from a normal distribution.
shapiro.test(df_clean$KWH[df_clean$MEDICALDEV == 1])
#P-value was very low for medical device HHs: We should use a non-parametric test to compare the two groups because the assumption of normality is violated.

#Mann-Whitney U test (also known as the Wilcoxon rank-sum test) to compare two groups
#KWH = continuous variable (dependent)
#MEDICALDEV = categorical variable with two levels (independent)
#t.test(KWH ~ MEDICALDEV, data = df_clean)
#wilcox.test(DV~IV, df)
wilcox_test_result <- wilcox.test(KWH ~ MEDICALDEV, data = df_clean)
print(wilcox_test_result)
#P-value was less than 0.05: There is a statistically significant difference in KWH use between the two groups


#Plot density plots to show differences in KWH per year and total dollars spent on electricity
library(ggplot2)
ggplot(df_clean, aes(x = KWH, color = factor(MEDICALDEV), fill = factor(MEDICALDEV))) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot of Annual Electricity Consumption",
       x = "Annual Electricity Consumption (in kWh)",
       fill = "Medical Device?",
       color = "Medical Device?") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"), labels = c("0" = "No", "1" = "Yes")) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("0" = "No", "1" = "Yes")) +
  xlim(0,45000)+
  theme_minimal()

ggplot(df_clean, aes(x = DOLLAREL, color = factor(MEDICALDEV), fill = factor(MEDICALDEV))) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot of Annual Electricity Expenditures",
       x = "Annual Electricity Expenditures (in 2020$)",
       fill = "Medical Device?",
       color = "Medical Device?") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"), labels = c("0" = "No", "1" = "Yes")) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("0" = "No", "1" = "Yes")) +
  xlim(0,6000)+
  theme_minimal()


library(sjmisc)
library(sjPlot)        

#Chi-square test for backup generator

#cross-tabs for backup generator
chisq.test(table(df$BACKUP, df$MEDICALDEV))
sjt.xtab(var.row=df$BACKUP,
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#P-value was less than 0.05: HHs with medical devices were statistically more likely to have back-up power options than non-medical device HHs (20.6% vs 14.8%).



#Perhaps those with medical devices had a higher likelihood of power outages (i.e., lived in areas with more power outages)?

#cross-tabs for power outage over 24 hours
chisq.test(table(df$POWEROUT, df$MEDICALDEV))
sjt.xtab(var.row=df$POWEROUT,
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#P-value was less than 0.05: There is a statistically significant difference in power outages between the two groups (19.9% vs 17.8%)

#cross-tabs for why power outage
#1 - Natural disaster/weather event: Slightly higher with medical device HHs (17% vs 15.4%).
#2 - Unable to pay bills: same %
#3 - Utility had a planned or unplanned blackout: Slightly higher with medical device HHs (2.3% vs 1.9%).
#99 - Other: same %
#-2 - No outage

#chisq.test(table(df$WHYPOWEROUT, df$MEDICALDEV))
#can't do chi-square because some cells had very few responses
fisher.test(table(df$WHYPOWEROUT, df$MEDICALDEV))
#p-value = 0.09 (some association between reason for power outage & medical device HH status)

sjt.xtab(var.row=df$WHYPOWEROUT,
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#all relative to -2 = no outage
df_clean_0_1_whypowerout <- df_clean[df_clean$WHYPOWEROUT %in% c(-2, 1), ]
df_clean_0_2_whypowerout <- df_clean[df_clean$WHYPOWEROUT %in% c(-2, 2), ]
df_clean_0_3_whypowerout <- df_clean[df_clean$WHYPOWEROUT %in% c(-2, 3), ]

# Compare WHYPOWEROUT = 1 (Natural disasters/weather)
fisher.test(table(df_clean_0_1_whypowerout$WHYPOWEROUT, df_clean_0_1_whypowerout$MEDICALDEV))
#p-value = 0.024: There is a statistically significant difference in cause of outage (Nat dis/weather event vs. none) between the two groups

# Compare WHYPOWEROUT = 2 (unable to pay bills)
fisher.test(table(df_clean_0_2_whypowerout$WHYPOWEROUT, df_clean_0_2_whypowerout$MEDICALDEV))
#p-value = 0.464: There is NO statistically significant difference in cause of outage (unable to pay bills vs. none) between the two groups

# Compare WHYPOWEROUT = 3 (utility had planned or unplanned outage)
fisher.test(table(df_clean_0_3_whypowerout$WHYPOWEROUT, df_clean_0_3_whypowerout$MEDICALDEV))
#p-value = 0.1464: There is NO statistically significant difference in cause of outage (utility outage event vs. none) between the two groups

# In conclusion: Medical device HHs are more likely to live in areas where the electric grid has severe (>24 hour outages) due to weather/disaster than the general population.
# This is a health equity issue!

#cross-tabs for type of house
chisq.test(table(df$TYPEHUQ, df$MEDICALDEV))
sjt.xtab(var.row=df$TYPEHUQ, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = T,
         show.cell.prc = F,
         show.summary  = T)
#statistically significant difference in groups by housing type: medical device HHs are more likely to live in mobile home and detached SFDs, 
#and less likely to live in attached homes, and apartment (of any size)
#1 Mobile home
#2 Single-family house detached from any other house 
#3 Single-family house attached to one or more other houses (for example: duplex, row house, or townhome)
#4 Apartment in a building with 2 to 4 units
#5 Apartment in a building with 5 or more units

#cross-tabs for solar PV
#chisq.test(table(df$SOLAR, df$MEDICALDEV))
sjt.xtab(var.row=df$SOLAR, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = T,
         show.cell.prc = F,
         show.summary  = T)


#-2 = NA (apartments with 2 or more units): medical device HHs are less likely to reside in apartments
#0 = no PV: medical device HHs are more likely to not have PV (solar)... due to less N/A?
#1 = yes PV: medical device HHs are more likely to have PV (solar)... due to less N/A?

df_clean_0_1_PV <- df_clean[df_clean$SOLAR %in% c(0, 1), ]

# Compare SOLAR = 1 (to no solar & not in an apartment)
fisher.test(table(df_clean_0_1_PV$SOLAR, df_clean_0_1_PV$MEDICALDEV))
#p-value = 0.0132: There is a statistically significant difference in PV adoption between the two groups



#cross-tabs for annual gross income
sjt.xtab(var.row=df$MONEYPY, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)


#Annual gross HH income last year shows a statistically significant difference at 10% level (p-value = 0.081).
#more work needed here!
#Medical Device HHs are over-represented in 3-8 ($7,500 - $29,999)
#1 Less than $5,000
#2 $5,000 - $7,499
#3 $7,500 - $9,999
#4 $10,000 - $12,499
#5 $12,500 - $14,999
#6 $15,000 - $19,999
#7 $20,000 - $24,999
#8 $25,000 - $29,999
#9 $30,000 - $34,999
#10 $35,000 - $39,999
#11 $40,000 - $49,999
#12 $50,000 - $59,999
#13 $60,000 - $74,999
#14 $75,000 - $99,999
#15 $100,000 - $149,999
#16 $150,000 or more


#cross-tabs for race
sjt.xtab(var.row=df$HOUSEHOLDER_RACE, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#P-value was less than 0.05: There is a statistically significant difference in HH income between the two groups

df_clean_1_2_WhiteBlack <- df_clean[df_clean$HOUSEHOLDER_RACE %in% c(1, 2), ]
#Fisher test for race: White vs Black
fisher.test(table(df_clean_1_2_WhiteBlack$HOUSEHOLDER_RACE, df_clean_1_2_WhiteBlack$MEDICALDEV))
#P-value was greater than 0.05: There is NO statistically significant difference for White and Black races between the two groups

df_clean_1_3_WhiteNative <- df_clean[df_clean$HOUSEHOLDER_RACE %in% c(1, 3), ]
#Fisher test for race: White vs Native American
fisher.test(table(df_clean_1_3_WhiteNative$HOUSEHOLDER_RACE, df_clean_1_3_WhiteNative$MEDICALDEV))
#P-value was greater than 0.05: There is NO statistically significant difference for White and Native American races between the two groups

df_clean_1_4_WhiteAsian <- df_clean[df_clean$HOUSEHOLDER_RACE %in% c(1, 4), ]
#Fisher test for race: White vs Asian American
fisher.test(table(df_clean_1_4_WhiteAsian$HOUSEHOLDER_RACE, df_clean_1_4_WhiteAsian$MEDICALDEV))
#P-value was less than 0.05: There is a statistically significant difference for White and Asian American races between the two groups
#We should compare Asian to other groups too.

df_clean_2_4_BlackAsian <- df_clean[df_clean$HOUSEHOLDER_RACE %in% c(2, 4), ]
#Fisher test for race: Black vs Asian American
fisher.test(table(df_clean_2_4_BlackAsian$HOUSEHOLDER_RACE, df_clean_2_4_BlackAsian$MEDICALDEV))
#P-value was less than 0.05: There is a statistically significant difference for Black and Asian American races between the two groups

df_clean_1_5_WhiteHPI <- df_clean[df_clean$HOUSEHOLDER_RACE %in% c(1, 5), ]
#Fisher test for race: White vs HPI
fisher.test(table(df_clean_1_5_WhiteHPI$HOUSEHOLDER_RACE, df_clean_1_5_WhiteHPI$MEDICALDEV))
#P-value was greater than 0.05: There is NO statistically significant difference for White and HPI American races between the two groups

df_clean_1_6_WhiteMulti <- df_clean[df_clean$HOUSEHOLDER_RACE %in% c(1, 6), ]
#Fisher test for race: White vs Multiracial
fisher.test(table(df_clean_1_6_WhiteMulti$HOUSEHOLDER_RACE, df_clean_1_6_WhiteMulti$MEDICALDEV))
#P-value was greater than 0.05: There is NO statistically significant difference for White and Multiracial races between the two groups

#Non-parametric test for elderly count
#cross-tabs for race
sjt.xtab(var.row=df$NUMADULT2, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

wilcox.test(NUMADULT2 ~ MEDICALDEV, data = df_clean)
#P-value was less than 0.05: There is a statistically significant difference in #of adults over 65 between the two groups

#medical device HHs are more likely to have 0 adults older than 65 in the house than non-medical device HHs (49.9% vs 62.6%).
#they are also more likely to have two adults older than 65 (22.9% vs 13.4%).
#this is representative of medical device holders not following the usual population curve.


#Frequency of reducing or forgoing basic necessities due to home energy bill
#1 Almost every month
#2 Some months
#3 1 or 2 months
#0 Never

#medical device HHs are statistically more likely to forgo basic necessities due to home energy bill (regardless of monthly frequency).
sjt.xtab(var.row=df$SCALEB, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)


#Frequency of keeping home at unhealthy temperature
#1 Almost every month
#2 Some months
#3 1 or 2 months
#0 Never

#medical device HHs are statistically more likely to keep home at an unhealthy temperature (regardless of monthly frequency).
sjt.xtab(var.row=df$SCALEG, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)


#Frequency of receiving disconnection notice
#1 Almost every month
#2 Some months
#3 1 or 2 months
#0 Never

#medical device HHs are statistically more likely to receive a disconnection notice (regardless of monthly frequency).
sjt.xtab(var.row=df$SCALEE, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)


#Received energy assistance to help pay energy bills after disconnection notice
#1 - Yes
#0 - No
#-2 - NA

#medical device HHs are statistically more likely to receive energy assistance after a disconnection notice.
#this shows that our systems are catching these vulnerable HHs at a higher rate (even if not everyone is helped).
df_payhelp <- df_clean[df_clean$PAYHELP %in% c(0,1), ]
sjt.xtab(var.row=df_payhelp$PAYHELP, 
         var.col=df_payhelp$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)



#Unable to use heating equipment in the past year because equipment was broken and could not afford repair or replacement
sjt.xtab(var.row=df_payhelp$NOHEATBROKE, 
         var.col=df_payhelp$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#not statistically significant


#Unable to use heating equipment in the past year because could not afford electricity and it was disconnected
sjt.xtab(var.row=df_payhelp$NOHEATEL, 
         var.col=df_payhelp$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#not statistically significant

#Unable to use heating equipment in the past year because could not afford natural gas and it was disconnected
sjt.xtab(var.row=df_payhelp$NOHEATNG, 
         var.col=df_payhelp$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#not statistically significant






#Unable to use air conditioning equipment in the past year because equipment was broken and could not afford repair or replacement
sjt.xtab(var.row=df_payhelp$NOACBROKE, 
         var.col=df_payhelp$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#not statistically significant


#Unable to use air conditioning equipment in the past year because could not afford electricity and it was disconnected
sjt.xtab(var.row=df_payhelp$NOACEL, 
         var.col=df_payhelp$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#not statistically significant

#Number of days unable to use air conditioning equipment in the past year
df_clean_no_ac_days <- df_clean[df_clean$NOACDAYS!=-2, ]
wilcox.test(NOACDAYS ~ MEDICALDEV, data = df_clean_no_ac_days)
#not statistically significant


#Medical attention needed because home was too hot
#1 - Yes
#0 - No

#medical device HHs are statistically more likely to have needed medical attention because home was too hot.
sjt.xtab(var.row=df$HOTMA, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#Medical attention needed because home was too cold
#1 - Yes
#0 - No

#medical device HHs are statistically more likely to have needed medical attention because home was too cold.
sjt.xtab(var.row=df$COLDMA, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)


#ever participated in an energy assistance program?
sjt.xtab(var.row=df_payhelp$ENERGYASST, 
         var.col=df_payhelp$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
# statistically significant: HHs were more likely to have ever participated!


#even though there are some differences in public health field regarding DBT....

#Dry Bulb Design Temperature (F) - temp expected to be exceeded 1% of the time
wilcox.test(DBT1 ~ MEDICALDEV, data = df_clean)
#significant!

#Dry Bulb Design Temperature (F) - temp expected to be exceeded 99% of the time
wilcox.test(DBT99 ~ MEDICALDEV, data = df_clean)
#NOT significant!



#we could probably map HDD data to point data from https://hprcc.unl.edu/maps.php?map=ACISClimateMaps#
#https://hprcc.unl.edu/gis/archive.php (look up point data in 2020 and accumulate).
#we map each RECS datapoint to each point by state and then closest point. Or we do the contour to get a random location???
#need GIS assistance for this!


#Statistically significant difference in home's building america climate zone
#looks like less hot-dry, more mixed-dry, more mixed-humid
sjt.xtab(var.row=df$BA_climate, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

df_clean_Mixed_Humid_Hot_Dry <- df_clean[df_clean$BA_climate %in% c("Mixed-Humid", "Hot-Dry", "Mixed-Dry"), ]
#Fisher test for race:Mixed Humid, Hot Dry, Mixed Dry
fisher.test(table(df_clean_Mixed_Humid_Hot_Dry$BA_climate, df_clean_Mixed_Humid_Hot_Dry$MEDICALDEV))
#P-value was less than 0.05: There is a statistically significant difference for home's climate zone between the two groups

#we don't know if this is a result of residential self-selection (i.e., those with medical devices choose to live in these climate zones)
#or these zones are more prevalent
#or other environmental or confounding factors causing more health conditions requiring medical devices are found in these climate zones

#cross-tabs for urban type
sjt.xtab(var.row=df$UATYP10, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#statistically significant difference in where medical device HHs live (higher rural: 25% vs 21.6%, higher urban cluster: 12.8% vs 10.5%)

#C = urban CLUSTER (2010-era census urban type code)
#R = rural
#U = urban area (includes suburban areas)



#Mann-Whitney U test (also known as the Wilcoxon rank-sum test) to compare two groups
wilcox.test(HDD65 ~ MEDICALDEV, data = df_clean)
#No statistically significant difference between groups in heating degree days

wilcox.test(CDD65 ~ MEDICALDEV, data = df_clean)
#No statistically significant difference between groups in cooling degree days
#Interesting finding: medical device HHs were significantly more likely to keep home at an unhealthy temperature, yet they are not more likely to have higher heating or cooling demands.


#cross-tabs for rent or own
sjt.xtab(var.row=df$KOWNRENT, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#statistically significant: medical device HHs more likely to own (77.3% vs 72%) and less likely to rent (21.8% vs 27%)
#1 = own
#2 = rent
#3 = occupy without payment of rent

#cross-tabs for level of insulation (self-reported)
sjt.xtab(var.row=df$ADQINSUL, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#1 Well insulated
#2 Adequately insulated
#3 Poorly insulated
#4 Not insulated

#cross-tabs for how often the home is drafty
sjt.xtab(var.row=df$DRAFTY, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#statistically significant (more often than not drafty)

#1 All the time
#2 Most of the time
#3 Some of the time
#4 Never


#we know that gas ranges are bad for human health... so what's the association here?

#cross-tabs for range fuel type
sjt.xtab(var.row=df$RANGEFUEL, 
         var.col=df$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#-2 Not applicable
#1 Natural gas from underground pipes
#2 Propane (bottled gas)
#5 Electricity
#13 Electricity and gas (dual-fuel range) 

#cross-tabs: NG used for cooking?
sjt.xtab(var.row=df_clean$UGCOOK, 
         var.col=df_clean$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#Statistically significant differences (less likely to have NG, more likely to have electricity).

#cross-tabs for those who have NG range: frequency of using cooktop per week
#attempt to understand exposure risk....
df_NG_range <- df_clean[df_clean$RANGEFUEL %in% c(1), ]
sjt.xtab(var.row=df_NG_range$RCOOKUSE, 
         var.col=df_NG_range$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#statistically significant difference

#density plot suggests higher exposure risk for medical device HHs using NG cooktop... but caution with small sample size (n=607)
ggplot(df_NG_range, aes(x = RCOOKUSE, color = factor(MEDICALDEV), fill = factor(MEDICALDEV))) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot of Cooktop Frequency of Use of Range per Week",
       x = "Use per Week",
       fill = "Medical Device?",
       color = "Medical Device?") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"), labels = c("0" = "No", "1" = "Yes")) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("0" = "No", "1" = "Yes")) +
  xlim(0,25)+
  theme_minimal()


#cross-tabs: type of main air conditioning equipment type
sjt.xtab(var.row=df_clean$ACEQUIPM_PUB, 
         var.col=df_clean$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#1 Central air conditioner (includes central heat pump)
#3 Ductless heat pump, also known as a “mini-split”
#4 Window or wall air conditioner
#5 Portable air conditioner
#6 Evaporative or swamp cooler
#-2 Not applicable

#statistically significant: more likely to have central AC, less likely to have window/wall Ac, more likely to have evaporation or swamp cooler.

#cross-tabs: air conditioning age
df_AC <- df_clean[df_clean$ACEQUIPAGE %in% c(1,2,3,4,5,6), ]
sjt.xtab(var.row=df_AC$ACEQUIPAGE, 
         var.col=df_AC$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#1 Less than 2 years old
#2 2 to 4 years old
#3 5 to 9 years old
#4 10 to 14 years old
#5 15 to 19 years old
#6 20 or more years old
#-2 Not applicable

#NO statistically significant difference in age


#cross-tabs: employment
sjt.xtab(var.row=df_clean$EMPLOYHH, 
         var.col=df_clean$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#1 Employed full-time
#2 Employed part-time
#3 Retired
#4 Not employed

#statistically significant: less likely to be FT worker, PT worker, more likely retired


#cross-tabs: education
sjt.xtab(var.row=df_clean$EDUCATION, 
         var.col=df_clean$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)
#1 Less than high school diploma or GED
#2 High school diploma or GED
#3 Some college or Associate’s degree
#4 Bachelor’s degree
#5 Master’s, Professional, or Doctoral degree 

#statistically significant: more likely to be HS or less. Less likely to have BS, MS and PhD.



#cross-tabs: Hispanic/Latino
sjt.xtab(var.row=df_clean$SDESCENT, 
         var.col=df_clean$MEDICALDEV, 
         show.col.prc  = T,
         show.row.prc  = F,
         show.cell.prc = F,
         show.summary  = T)

#statistically significant: less likely