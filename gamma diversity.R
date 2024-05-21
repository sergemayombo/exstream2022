library(readr)
library(vegan)

exstream2022_IMP_microscopy <- read_csv("exstream2022_IMP_microscopy.csv")



exstream2021_algae <- read_csv2("exstream2021_algae_t.csv")
df<-t(exstream2021_algae)
metadata_IMP2022 <- read_csv("metadata_IMP2022.csv")

names(exstream2021_algae_t)
colnames(exstream2021_algae_t)[1] ="Channel"


df_merge <- merge(metadata,exstream2021_algae_t,by="Channel") 
exstream2021_algae_t$Treatment <- metadata_IMP2022$treatments

names(df_merge)
print ("Second DataFrame") 
print (df2) 
diatoms <- diat5 %>% rownames_to_column(var = "Channel")
df_merge <- merge(meta2, diatoms,by="Channel") 
df_merge <- df_merge %>% column_to_rownames(var = "Channel")

#Control Total = 115, str = 92, rec = 75
#Control OTU = 859, str = 697, rec = 597
nbc<-subset(df_merge, treatments == "NBN")
gamma_total_nbc<-specnumber(colSums(nbc[,11:1864]))

nbc_str<-subset(nbc, Phase == "stress")
gamma_str_nbc<-specnumber(colSums(nbc_str[,11:1864]))

nbc_rec<-subset(nbc, Phase == "recovery")
gamma_str_rec<-specnumber(colSums(nbc_rec[,11:1864]))

presabs_nbc<-ifelse(nbc>0,1,0)
beta.multi(presabs_nbc,index.family = "sorensen" )

presabs_nbc_rec<-ifelse(nbc_rec>0,1,0)
beta.multi(presabs_nbc_rec,index.family = "sorensen" )

presabs_nbc_str<-ifelse(nbc_str>0,1,0)
beta.multi(presabs_nbc_str,index.family = "sorensen" )

#Temperature mic Total = 133, str =85, rec = 92
#Temperature OTU Total = 890, str =712, rec = 620
nbi<-subset(df_merge, treatments == "NBI")
gamma_total_nbi<-specnumber(colSums(nbi[,11:1864])) #11:1864

nbi_str<-subset(nbi, Phase == "stress")
gamma_str_nbi<-specnumber(colSums(nbi_str[,11:1864]))

nbi_rec<-subset(nbi, Phase == "recovery")
gamma_rec_nbi<-specnumber(colSums(nbi_rec[,11:1864]))

presabs_nbi<-ifelse(nbi>0,1,0)
beta.multi(presabs_nbi,index.family = "sorensen" )

presabs_nbi_rec<-ifelse(nbi_rec>0,1,0)
beta.multi(presabs_nbi_rec,index.family = "sorensen" )

presabs_nbi_str<-ifelse(nbi_str>0,1,0)
beta.multi(presabs_nbi_str,index.family = "sorensen" )

#Salinity Total = 103, str = 85, rec = 67
#Salinity OTU Total = 833, str = 670, rec = 558
nsn<-subset(df_merge, treatments == "NSN")
gamma_total_nsn<-specnumber(colSums(nsn[,11:1864]))

nsn_str<-subset(nsn, Phase == "stress")
gamma_str_nsn<-specnumber(colSums(nsn_str[,11:1864]))

nsn_rec_nsn<-subset(nsn, Phase == "recovery")
gamma_rec_nsn<-specnumber(colSums(nsn_rec_nsn[,11:1864]))

presabs_nsn<-ifelse(nsn>0,1,0)
beta.multi(presabs_nsn,index.family = "sorensen" )

presabs_nsn_rec<-ifelse(nsn_rec_nsn>0,1,0)
beta.multi(presabs_nsn_rec,index.family = "sorensen" )

presabs_nsn_str<-ifelse(nsn_str>0,1,0)
beta.multi(presabs_nsn_str,index.family = "sorensen" )

#Reduced Flow Total = 106, str = 85, rec = 71
#Reduced Flow OTU Total = 851, str = 719, rec = 575

rbn<-subset(df_merge, treatments == "RBN")
gamma_total_rbn<-specnumber(colSums(rbn[,11:1864]))

rbn_str<-subset(rbn, Phase == "stress")
gamma_str_rbn<-specnumber(colSums(rbn_str[,11:1864]))

rbn_rec_rbn<-subset(rbn, Phase == "recovery")
gamma_rec_rbn<-specnumber(colSums(rbn_rec_rbn[,11:1864]))

presabs_rbn<-ifelse(rbn>0,1,0)
beta.multi(presabs_rbn,index.family = "sorensen" )

presabs_rbn_rec<-ifelse(rbn_rec_rbn>0,1,0)
beta.multi(presabs_rbn_rec,index.family = "sorensen" )

presabs_rbn_str<-ifelse(rbn_str>0,1,0)
beta.multi(presabs_rbn_str,index.family = "sorensen" )


# R + S + T Total = 134, str = 110, rec = 76
# R + S + T OTU Total = 906, str = 722, rec = 596
rsi<-subset(df_merge, treatments == "RSI")
gamma_total_rsi<-specnumber(colSums(rsi[,11:1864]))

rsi_str<-subset(rsi, Phase == "stress")
gamma_str_rsi<-specnumber(colSums(rsi_str[,11:1864]))

rsi_rec_rsi<-subset(rsi, Phase == "recovery")
gamma_rec_rsi<-specnumber(colSums(rsi_rec_rsi[,11:1864]))

presabs_rsi<-ifelse(rsi>0,1,0)
beta.multi(presabs_rsi,index.family = "sorensen" )

presabs_rsi_rec<-ifelse(rsi_rec_rsi>0,1,0)
beta.multi(presabs_rsi_rec,index.family = "sorensen" )

presabs_rsi_str<-ifelse(rsi_str>0,1,0)
beta.multi(presabs_rsi_str,index.family = "sorensen" )


# R + S  Total = 111, str = 88, rec = 76
# R + S  OTU Total =  894, str = 746, rec = 576
rsn<-subset(df_merge, treatments == "RSN")
gamma_total_rsn<-specnumber(colSums(rsn[,11:1864]))

rsn_str<-subset(rsn, Phase == "stress")
gamma_str_rsn<-specnumber(colSums(rsn_str[,11:1864]))

rsn_rec_rsn<-subset(rsn, Phase == "recovery")
gamma_rec_rsn<-specnumber(colSums(rsn_rec_rsn[,11:1864]))

presabs_rsn<-ifelse(rsn>0,1,0)
beta.multi(presabs_rsn,index.family = "sorensen" )

presabs_rsn_rec<-ifelse(rsn_rec_rbi>0,1,0)
beta.multi(presabs_rsn_rec,index.family = "sorensen" )

presabs_rsn_str<-ifelse(rsn_str>0,1,0)
beta.multi(presabs_rsn_str,index.family = "sorensen" )

# S + T Total = 130, str = 93, rec = 91
# S + T OTU Total = 887, str = 743, rec = 534
nsi<-subset(df_merge, treatments == "NSI")
gamma_total_nsi<-specnumber(colSums(nsi[,11:1864]))

nsi_str<-subset(nsi, Phase == "stress")
gamma_str_nsi<-specnumber(colSums(nsi_str[,11:1864]))

nsi_rec_nsi<-subset(nsi, Phase == "recovery")
gamma_rec_nsi<-specnumber(colSums(nsi_rec_nsi[,11:1864]))

presabs_nsi<-ifelse(nsi>0,1,0)
beta.multi(presabs_nsi,index.family = "sorensen" )

presabs_nsi_rec<-ifelse(nsi_rec_nsi>0,1,0)
beta.multi(presabs_nsi_rec,index.family = "sorensen" )

presabs_nsi_str<-ifelse(nsi_str>0,1,0)
beta.multi(presabs_nsi_str,index.family = "sorensen" )

# R + T Total = 124, str = 92, rec = 86
# R + T Total = 920, str = 748, rec = 589

rbi<-subset(df_merge, treatments == "RBI")
gamma_total_rbi<-specnumber(colSums(rbi[,11:1864]))

rbi_str<-subset(rbi, Phase == "stress")
gamma_str_rbi<-specnumber(colSums(rbi_str[,11:1864]))

rbi_rec_rbi<-subset(rbi, Phase == "recovery")
gamma_rec_rbi<-specnumber(colSums(rbi_rec_rbi[,11:1864]))

presabs_rbi<-ifelse(rbi>0,1,0)
beta.multi(presabs_rbi,index.family = "sorensen" )

presabs_rbi_rec<-ifelse(rbi_rec_rbi>0,1,0)
beta.multi(presabs_rbi_rec,index.family = "sorensen" )

presabs_rbi_str<-ifelse(rbi_str>0,1,0)
beta.multi(presabs_rbi_str,index.family = "sorensen" )




#Control Total = 115, str = 92, rec = 75
#Control OTU = 859, str = 697, rec = 597
nbc<-subset(df_merge, treatments == "NBC")
gamma_total_nbc<-specnumber(colSums(nbc[,11:84]))

nbc_str<-subset(nbc, Phase == "stress")
gamma_str_nbc<-specnumber(colSums(nbc_str[,11:84]))

nbc_rec<-subset(nbc, Phase == "recovery")
gamma_str_rec<-specnumber(colSums(nbc_rec[,11:84]))

presabs_nbc<-ifelse(nbc[,11:84]>0,1,0)
beta.multi(presabs_nbc,index.family = "sorensen" )

presabs_nbc_rec<-ifelse(nbc_rec>0,1,0)
beta.multi(presabs_nbc_rec,index.family = "sorensen" )

presabs_nbc_str<-ifelse(nbc_str>0,1,0)
beta.multi(presabs_nbc_str,index.family = "sorensen" )

#Temperature mic Total = 133, str =85, rec = 92
#Temperature OTU Total = 890, str =712, rec = 620
nbi<-subset(df_merge, treatments == "NBI")
gamma_total_nbi<-specnumber(colSums(nbi[,11:267])) #11:267

nbi_str<-subset(nbi, Phase == "stress")
gamma_str_nbi<-specnumber(colSums(nbi_str[,11:267]))

nbi_rec<-subset(nbi, Phase == "recovery")
gamma_rec_nbi<-specnumber(colSums(nbi_rec[,11:267]))

presabs_nbi<-ifelse(nbi>0,1,0)
beta.multi(presabs_nbi,index.family = "sorensen" )

presabs_nbi_rec<-ifelse(nbi_rec>0,1,0)
beta.multi(presabs_nbi_rec,index.family = "sorensen" )

presabs_nbi_str<-ifelse(nbi_str>0,1,0)
beta.multi(presabs_nbi_str,index.family = "sorensen" )
 
#Salinity Total = 103, str = 85, rec = 67
#Salinity OTU Total = 833, str = 670, rec = 558
nsn<-subset(df_merge, treatments == "NSN")
gamma_total_nsn<-specnumber(colSums(nsn[,11:267]))

nsn_str<-subset(nsn, Phase == "stress")
gamma_str_nsn<-specnumber(colSums(nsn_str[,11:267]))

nsn_rec_nsn<-subset(nsn, Phase == "recovery")
gamma_rec_nsn<-specnumber(colSums(nsn_rec_nsn[,11:267]))

presabs_nsn<-ifelse(nsn>0,1,0)
beta.multi(presabs_nsn,index.family = "sorensen" )

presabs_nsn_rec<-ifelse(nsn_rec_nsn>0,1,0)
beta.multi(presabs_nsn_rec,index.family = "sorensen" )

presabs_nsn_str<-ifelse(nsn_str>0,1,0)
beta.multi(presabs_nsn_str,index.family = "sorensen" )

#Reduced Flow Total = 106, str = 85, rec = 71
#Reduced Flow OTU Total = 851, str = 719, rec = 575

rbn<-subset(df_merge, treatments == "RBN")
gamma_total_rbn<-specnumber(colSums(rbn[,11:267]))

rbn_str<-subset(rbn, Phase == "stress")
gamma_str_rbn<-specnumber(colSums(rbn_str[,11:267]))

rbn_rec_rbn<-subset(rbn, Phase == "recovery")
gamma_rec_rbn<-specnumber(colSums(rbn_rec_rbn[,11:267]))

presabs_rbn<-ifelse(rbn>0,1,0)
beta.multi(presabs_rbn,index.family = "sorensen" )

presabs_rbn_rec<-ifelse(rbn_rec_rbn>0,1,0)
beta.multi(presabs_rbn_rec,index.family = "sorensen" )

presabs_rbn_str<-ifelse(rbn_str>0,1,0)
beta.multi(presabs_rbn_str,index.family = "sorensen" )


# R + S + T Total = 134, str = 110, rec = 76
# R + S + T OTU Total = 906, str = 722, rec = 596
rsi<-subset(df_merge, treatments == "RSI")
gamma_total_rsi<-specnumber(colSums(rsi[,11:267]))

rsi_str<-subset(rsi, Phase == "stress")
gamma_str_rsi<-specnumber(colSums(rsi_str[,11:267]))

rsi_rec_rsi<-subset(rsi, Phase == "recovery")
gamma_rec_rsi<-specnumber(colSums(rsi_rec_rsi[,11:267]))

presabs_rsi<-ifelse(rsi>0,1,0)
beta.multi(presabs_rsi,index.family = "sorensen" )

presabs_rsi_rec<-ifelse(rsi_rec_rsi>0,1,0)
beta.multi(presabs_rsi_rec,index.family = "sorensen" )

presabs_rsi_str<-ifelse(rsi_str>0,1,0)
beta.multi(presabs_rsi_str,index.family = "sorensen" )


# R + S  Total = 111, str = 88, rec = 76
# R + S  OTU Total =  894, str = 746, rec = 576
rsn<-subset(df_merge, treatments == "RSN")
gamma_total_rsn<-specnumber(colSums(rsn[,11:267]))

rsn_str<-subset(rsn, Phase == "stress")
gamma_str_rsn<-specnumber(colSums(rsn_str[,11:267]))

rsn_rec_rsn<-subset(rsn, Phase == "recovery")
gamma_rec_rsn<-specnumber(colSums(rsn_rec_rsn[,11:267]))

presabs_rsn<-ifelse(rsn>0,1,0)
beta.multi(presabs_rsn,index.family = "sorensen" )

presabs_rsn_rec<-ifelse(rsn_rec_rsn>0,1,0)
beta.multi(presabs_rsn_rec,index.family = "sorensen" )

presabs_rsn_str<-ifelse(rsn_str>0,1,0)
beta.multi(presabs_rsn_str,index.family = "sorensen" )

# S + T Total = 130, str = 93, rec = 91
# S + T OTU Total = 887, str = 743, rec = 534
nsi<-subset(df_merge, treatments == "NSI")
gamma_total_nsi<-specnumber(colSums(nsi[,11:267]))

nsi_str<-subset(nsi, Phase == "stress")
gamma_str_nsi<-specnumber(colSums(nsi_str[,11:267]))

nsi_rec_nsi<-subset(nsi, Phase == "recovery")
gamma_rec_nsi<-specnumber(colSums(nsi_rec_nsi[,11:267]))

presabs_nsi<-ifelse(nsi>0,1,0)
beta.multi(presabs_nsi,index.family = "sorensen" )

presabs_nsi_rec<-ifelse(nsi_rec_nsi>0,1,0)
beta.multi(presabs_nsi_rec,index.family = "sorensen" )

presabs_nsi_str<-ifelse(nsi_str>0,1,0)
beta.multi(presabs_nsi_str,index.family = "sorensen" )

# R + T Total = 124, str = 92, rec = 86
# R + T Total = 920, str = 748, rec = 589

rbi<-subset(df_merge, treatments == "RBI")
gamma_total_rbi<-specnumber(colSums(rbi[,11:267]))

rbi_str<-subset(rbi, Phase == "stress")
gamma_str_rbi<-specnumber(colSums(rbi_str[,11:267]))

rbi_rec_rbi<-subset(rbi, Phase == "recovery")
gamma_rec_rbi<-specnumber(colSums(rbi_rec_rbi[,11:267]))

presabs_rbi<-ifelse(rbi>0,1,0)
beta.multi(presabs_rbi,index.family = "sorensen" )

presabs_rbi_rec<-ifelse(rbi_rec_rbi>0,1,0)
beta.multi(presabs_rbi_rec,index.family = "sorensen" )

presabs_rbi_str<-ifelse(rbi_str>0,1,0)
beta.multi(presabs_rbi_str,index.family = "sorensen" )
