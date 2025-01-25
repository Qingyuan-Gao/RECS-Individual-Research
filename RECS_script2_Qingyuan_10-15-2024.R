
#Read in dataset
df = read.csv("D:/recs2020_public_v7.csv",header=TRUE)

#check the dimension
dim(df)

#clean the dataset, excluding NA and values other than 0 & 1
df_clean <- df[!is.na(df$MEDICALDEV), ]
df_clean1<- df_clean[!is.na(df_clean$KWH),]
df_clean2 <- df_clean1[df_clean1$MEDICALDEV<2, ]

#creating independent dataset for medical device households/ non-medical-device households
df_medicaldev_1 <- df_clean2[df_clean2$MEDICALDEV == 1, ]
df_medicaldev_0 <- df_clean2[df_clean2$MEDICALDEV == 0, ]


#data analysis 2
#medical households by demographic variables

#create nominal variables
#REGIONC
nominal_regionc1<-as.integer(factor(df_medicaldev_1$REGIONC))
#UATYP10
nominal_uatyp101<-as.integer(factor(df_medicaldev_1$UATYP10))
#TYPEHUQ
nominal_typehuq1<-as.integer(factor(df_medicaldev_1$TYPEHUQ))
#EMPLOYHH
nominal_employhh1<-as.integer(factor(df_medicaldev_1$EMPLOYHH))
#WHYPOWEROUT
nominal_whypowerout1<-as.integer(factor(df_medicaldev_1$WHYPOWEROUT))

nominal_backup1<-as.integer(factor(df_medicaldev_1$BACKUP))
nominal_powerout1<-as.integer(factor(df_medicaldev_1$POWEROUT))
nominal_energyasst1<-as.integer(factor(df_medicaldev_1$ENERGYASST))
nominal_hotma1<-as.integer(factor(df_medicaldev_1$HOTMA))
nominal_coldma1<-as.integer(factor(df_medicaldev_1$COLDMA))
nominal_medicaldev1<-as.integer(factor(df_medicaldev_1$MEDICALDEV))

#create ordinal variables

ordinal_education1<-as.integer(factor(df_medicaldev_1$EDUCATION))
ordinal_moneypy1<-as.integer(factor(df_medicaldev_1$MONEYPY))
ordinal_scaleb1<- as.integer(factor(df_medicaldev_1$SCALEB))
ordinal_scaleg1<-as.integer(factor(df_medicaldev_1$SCALEG))
ordinal_scalee1<-as.integer(factor(df_medicaldev_1$SCALEE))

#create count variables

count_nhsldmem1<-df_medicaldev_1$NHSLDMEM
count_numadult21<-as.integer(df_medicaldev_1$NUMADULT2+1)

#create continuous variable
continuous_kwh1<-as.integer(log(df_medicaldev_1$KWH)-4)

min(continuous_kwh1)

#join dataset

lcm1data<-data.frame(nominal_regionc1,
                    nominal_uatyp101,
                    nominal_typehuq1,
                    nominal_employhh1,
                    nominal_whypowerout1,
                    
                   nominal_backup1,
                   nominal_powerout1,
                   nominal_energyasst1,
                   nominal_hotma1,
                   nominal_coldma1,
                   nominal_medicaldev1,
                   ordinal_education1,
                   ordinal_moneypy1,
                   ordinal_scaleb1,
                   ordinal_scaleg1,
                   ordinal_scalee1,
                   count_nhsldmem1,
                   count_numadult21,
                   continuous_kwh1
                   )

#POLCA packages

library(poLCA)

#LCM

lcm1<-cbind(nominal_regionc1,
            nominal_uatyp101,
            nominal_typehuq1,

            
            nominal_backup1,
            nominal_powerout1,
            nominal_energyasst1,
            nominal_hotma1,
            nominal_coldma1,
            ordinal_scaleb1,
            ordinal_scaleg1,
            ordinal_scalee1,
            
            continuous_kwh1)~nominal_employhh1+ordinal_education1+ordinal_moneypy1+count_nhsldmem1+count_numadult21



fitlcm11<-poLCA(lcm1,lcm1data,nclass=2)

fitlcm12<-poLCA(lcm1,lcm1data,nclass=3)

fitlcm13<-poLCA(lcm1,lcm1data,nclass=4)#4 classes

fitlcm14<-poLCA(lcm1,lcm1data,nclass=5)


#non medical device HHs by demographic variables

#create nominal variables
#REGIONC
nominal_regionc0<-as.integer(factor(df_medicaldev_0$REGIONC))
#UATYP10
nominal_uatyp100<-as.integer(factor(df_medicaldev_0$UATYP10))
#TYPEHUQ
nominal_typehuq0<-as.integer(factor(df_medicaldev_0$TYPEHUQ))
#EMPLOYHH
nominal_employhh0<-as.integer(factor(df_medicaldev_0$EMPLOYHH))
#WHYPOWEROUT
nominal_whypowerout0<-as.integer(factor(df_medicaldev_0$WHYPOWEROUT))

nominal_backup0<-as.integer(factor(df_medicaldev_0$BACKUP))
nominal_powerout0<-as.integer(factor(df_medicaldev_0$POWEROUT))
nominal_energyasst0<-as.integer(factor(df_medicaldev_0$ENERGYASST))
nominal_hotma0<-as.integer(factor(df_medicaldev_0$HOTMA))
nominal_coldma0<-as.integer(factor(df_medicaldev_0$COLDMA))
nominal_medicaldev0<-as.integer(factor(df_medicaldev_0$MEDICALDEV))

#create ordinal variables

ordinal_education0<-as.integer(factor(df_medicaldev_0$EDUCATION))
ordinal_moneypy0<-as.integer(factor(df_medicaldev_0$MONEYPY))
ordinal_scaleb0<- as.integer(factor(df_medicaldev_0$SCALEB))
ordinal_scaleg0<-as.integer(factor(df_medicaldev_0$SCALEG))
ordinal_scalee0<-as.integer(factor(df_medicaldev_0$SCALEE))

#create count variables

count_nhsldmem0<-df_medicaldev_0$NHSLDMEM
count_numadult20<-as.integer(df_medicaldev_0$NUMADULT2+1)
min(count_numadult20)

#create continuous variable
continuous_kwh0<-as.integer(log(df_medicaldev_0$KWH+1)+1)

min(continuous_kwh0)

#join dataset

lcm2data<-data.frame(nominal_regionc0,
                     nominal_uatyp100,
                     nominal_typehuq0,
                     nominal_employhh0,
                     nominal_whypowerout0,
                     
                     nominal_backup0,
                     nominal_powerout0,
                     nominal_energyasst0,
                     nominal_hotma0,
                     nominal_coldma0,
                     nominal_medicaldev0,
                     ordinal_education0,
                     ordinal_moneypy0,
                     ordinal_scaleb0,
                     ordinal_scaleg0,
                     ordinal_scalee0,
                     count_nhsldmem0,
                     count_numadult20,
                     continuous_kwh0
)

#POLCA packages

library(poLCA)

#LCM

lcm2<-cbind(nominal_regionc0,
            nominal_uatyp100,
            nominal_typehuq0,
            
            
            nominal_backup0,
            nominal_powerout0,
            nominal_energyasst0,
            nominal_hotma0,
            nominal_coldma0,
            ordinal_scaleb0,
            ordinal_scaleg0,
            ordinal_scalee0,
            continuous_kwh0)~nominal_employhh0+ordinal_education0+ordinal_moneypy0+
            count_nhsldmem0+count_numadult20



fitlcm21<-poLCA(lcm2,lcm2data,nclass=2)

fitlcm22<-poLCA(lcm2,lcm2data,nclass=3)

fitlcm23<-poLCA(lcm2,lcm2data,nclass=4)#4 classes

fitlcm24<-poLCA(lcm2,lcm2data,nclass=5)


#all households data input


#create nominal variables
#REGIONC
nominal_regionc<-as.integer(factor(df_clean2$REGIONC))
#UATYP10
nominal_uatyp10<-as.integer(factor(df_clean2$UATYP10))
#TYPEHUQ
nominal_typehuq<-as.integer(factor(df_clean2$TYPEHUQ))
#EMPLOYHH
nominal_employhh<-as.integer(factor(df_clean2$EMPLOYHH))
#WHYPOWEROUT
nominal_whypowerout<-as.integer(factor(df_clean2$WHYPOWEROUT))

nominal_backup<-as.integer(factor(df_clean2$BACKUP))
nominal_powerout<-as.integer(factor(df_clean2$POWEROUT))
nominal_energyasst<-as.integer(factor(df_clean2$ENERGYASST))
nominal_hotma<-as.integer(factor(df_clean2$HOTMA))
nominal_coldma<-as.integer(factor(df_clean2$COLDMA))
nominal_medicaldev<-as.integer(factor(df_clean2$MEDICALDEV))

#create ordinal variables

ordinal_education<-as.integer(factor(df_clean2$EDUCATION))
ordinal_moneypy<-as.integer(factor(df_clean2$MONEYPY))
ordinal_scaleb<- as.integer(factor(df_clean2$SCALEB))
ordinal_scaleg<-as.integer(factor(df_clean2$SCALEG))
ordinal_scalee<-as.integer(factor(df_clean2$SCALEE))

#create count variables

count_nhsldmem<-as.integer(df_clean2$NHSLDMEM)
count_numadult2<-as.integer(df_clean2$NUMADULT2+1)
min(count_numadult2)

#create continuous variable
continuous_kwh<-as.integer(log(df_clean2$KWH+3)+1)

min(continuous_kwh)

#join the dataset
lcm3data<-data.frame(nominal_regionc,
                     nominal_uatyp10,
                     nominal_typehuq,
                     nominal_employhh,
                     nominal_whypowerout,
                     
                     nominal_backup,
                     nominal_powerout,
                     nominal_energyasst,
                     nominal_hotma,
                     nominal_coldma,
                     nominal_medicaldev,
                     ordinal_education,
                     ordinal_moneypy,
                     ordinal_scaleb,
                     ordinal_scaleg,
                     ordinal_scalee,
                     count_nhsldmem,
                     count_numadult2,
                     continuous_kwh
)

lcm3<-cbind(nominal_regionc,
            nominal_uatyp10,
            nominal_typehuq,
            
            
            nominal_backup,
            nominal_powerout,
            nominal_energyasst,
            nominal_hotma,
            nominal_coldma,
            ordinal_scaleb,
            ordinal_scaleg,
            ordinal_scalee,
            continuous_kwh,
            
            nominal_employhh,
            ordinal_education,
            ordinal_moneypy,
            count_nhsldmem,
            count_numadult2
            )~nominal_medicaldev

# LCM of medical device HHs, with Y only the demographic variable

lcm3<-cbind(nominal_employhh1,
            ordinal_education1,
            ordinal_moneypy1,
            count_nhsldmem1,
            count_numadult21
)~1

fitlcm31<-poLCA(lcm3,lcm1data,nclass=2)
fitlcm32<-poLCA(lcm3,lcm1data,nclass=3)# 3 classes
fitlcm33<-poLCA(lcm3,lcm1data,nclass=4)


# LCM of medical device HHs, with Y only the geographic& house type variable

lcm4<-cbind(nominal_regionc1,
            nominal_uatyp101,
            nominal_typehuq1
)~1

fitlcm41<-poLCA(lcm4,lcm1data,nclass=2)
fitlcm42<-poLCA(lcm4,lcm1data,nclass=3)# 3 classes
fitlcm43<-poLCA(lcm4,lcm1data,nclass=4)

# LCM of medical device HHs, with Y only the energy assistance variables

lcm5<-cbind(nominal_backup1,
            nominal_powerout1,
            nominal_energyasst1,
            ordinal_scaleb1,
            ordinal_scalee1,
            continuous_kwh1
)~1

fitlcm51<-poLCA(lcm5,lcm1data,nclass=2)# 2 classes
fitlcm52<-poLCA(lcm5,lcm1data,nclass=3)
fitlcm53<-poLCA(lcm5,lcm1data,nclass=4)

# LCM of medical device HHs, with Y only the energy vulnerability variables

lcm6<-cbind(    nominal_hotma1,
                nominal_coldma1,
                ordinal_scaleg1,
                continuous_kwh1
)~1

fitlcm61<-poLCA(lcm6,lcm1data,nclass=2)
fitlcm62<-poLCA(lcm6,lcm1data,nclass=3)# 3 classes
fitlcm63<-poLCA(lcm6,lcm1data,nclass=4)

