                                                           
######################################################### Chick Growth  ################################################################################
################################# By R.B. Sherley, Environment and Sustainability Institute, University of Exeter ##############################################
#
#       This script will calculate growth co-efficents for measurements of mass, which can be read in automatically from
#       a file (the data should be formatted as in the example file). The code can deal with missing values (specified as 'NA').
#
#       This version of the code can only deal with up to 12 measurement events per bird and the code must be run once for each bird in your dataset. 
#       Implementing a flexible version that checks the number of measurements per bird each time and runs all the birds within a loop is on the to do list.
#    
#       The "Set up data" and "User defined data" must be loaded into the R workspace the first time and then should be commented out. 
#
#       On each run of the code, the median Gompertz and logistic growth coefficients (k values) for each bird are stored in a matrix called "Kvalues_temp".
#       A summary of the last 10 lines of this matrix is printed at the end of each run, so the user can check that the values stored look reasonable.
#       At the very end of the code, there is a section of code (usually commented out) that will print the data to a CSV file.
#       
#       Each run through the script will also produce two files containing the gompertz values and logistic values for all measurement pairs for that chick (stored to the working 
#       directory entered in the "User specified data" section. If these are not needed, the files can be ignored and deleted. 
#       For the logisitic values, only the k-values are stored (the mass values and days are also stored for the Gompertz values).
#
#       The equation for the number of estimates (p) of k generated for each bird is p=n(n-1)/2, where n is number of measurements per bird
#
################################################ START - User defined data #########################################################
############################## Run this section only the first time you run the code for each dataset ##############################
# setwd("~/Google Drive/Main File Store/Papers, Reports, Theses etc/My Papers/In progress/Swallows") ## Set your working directory
# data.growth<-read.csv("Growth.csv",header=T)  ## Enter the name of the file containing your data
# n.chicks <- 200      ## Increase these values if you have data from more than 200 chicks
# variables <- 8      ## or want more than 8 variables listed in the final output
# MassAss <- 26.5      ## Set the desired Asymptotic mass
############################################ END - User defined data ##########################################################

################################################ START - Set up data #########################################################
########### Do not change these values and run this section only the first time you run the code for each dataset ############
# Count <- 1
# p <- 0
# d <- 1
# DataLengthStop <- 1
# DataLengthStop_Head  <- 1
# Kvalues_temp<-matrix(rep(0, (n.chicks*variables)),nrow=n.chicks)
# ByDate_temp<-matrix(rep(0, ((n.chicks*variables)*7)),nrow=(n.chicks*variables))
############################################ END - Set up data ##########################################################


################################################ START - Code to run one for each bird  #########################################################
p <- p+1
########################################## Reads in the data for Mass and Days for up to 12 measurements ##########
###############################################################################################################################
Chick <- data.growth[p,1]
Nest <- data.growth[p,4]  ## The user can add other identifiers here, like bird ID, nest ID, number of siblings etc to be carried through and linked with the k-values at the end -- but "variables" must be changed in the user defined data 
Date1 <- data.growth[p,2]
Ring <- data.growth[p,3]
d <- p 
while (p > 0 & data.growth[p,1] == data.growth[d,1]){p<-p+1}                                                    
Date2 <- data.growth[(p-1),2]
TotalDays <- Date2-Date1 
datapoints <- p-d
datapointsMass <- 0

if(d > 0 & d < p & data.growth[d,5] > 0) Mass1 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days1 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1
                     
if(d > 0 & d < p & data.growth[d,5] > 0) Mass2 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days2 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass3 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days3 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass4 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days4 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass5 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days5 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass6 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days6 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass7 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days7 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass8 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days8 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass9 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days9 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass10 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days10 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass11 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days11 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass12 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days12 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass13 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days13 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass14 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days14 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass15 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days15 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass16 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days16 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass17 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days17 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass18 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days18 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass19 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days19 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass20 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days20 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass21 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days21 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

if(d > 0 & d < p & data.growth[d,5] > 0) Mass22 <- data.growth[d,5] else next
if(d > 0 & d < p & data.growth[d,5] > 0) datapointsMass <- datapointsMass+1 else next
if(d > 0 & d < p) Days22 <- data.growth[d,2] else next
if(d > 0 & d < p) d<-d+1

#################################################################################################################################
###################################################################################################################################
###################################################################################################################################
################################### Works out the gompertz k for Mass ############################################################
###################################################################################################################################
i<-1

Data<-matrix(rep(0,(231*5)),nrow=231)

kgom1_2<-(log(-log(Mass1/MassAss))-log(-log(Mass2/MassAss)))/(Days2-Days1)
Data[i,1]<-kgom1_2;
Data[i,4]<-Date1;
Data[i,5]<-(Date1+Days2);
Data[i,2]<-Mass1;
Data[i,3]<-Mass2; if(Data[i] != 0) i<-i+1

kgom1_3<-(log(-log(Mass1/MassAss))-log(-log(Mass3/MassAss)))/(Days3-Days1)
Data[i,1]<-kgom1_3;
Data[i,2]<-Mass1;
Data[i,3]<-Mass3; if(Data[i] != 0) i<-i+1

kgom1_4<-(log(-log(Mass1/MassAss))-log(-log(Mass4/MassAss)))/(Days4-Days1)
Data[i,1]<-kgom1_4;
Data[i,2]<-Mass1;
Data[i,3]<-Mass4; if(Data[i] != 0) i<-i+1

kgom1_5<-(log(-log(Mass1/MassAss))-log(-log(Mass5/MassAss)))/(Days5-Days1)
Data[i,1]<-kgom1_5;
Data[i,2]<-Mass1;
Data[i,3]<-Mass5; if(Data[i] != 0) i<-i+1

kgom1_6<-(log(-log(Mass1/MassAss))-log(-log(Mass6/MassAss)))/(Days6-Days1)
Data[i,1]<-kgom1_6;
Data[i,2]<-Mass1;
Data[i,3]<-Mass6; if(Data[i] != 0) i<-i+1

kgom1_7<-(log(-log(Mass1/MassAss))-log(-log(Mass7/MassAss)))/(Days7-Days1)
Data[i,1]<-kgom1_7;
Data[i,2]<-Mass1;
Data[i,3]<-Mass7; if(Data[i] != 0) i<-i+1

kgom1_8<-(log(-log(Mass1/MassAss))-log(-log(Mass8/MassAss)))/(Days8-Days1)
Data[i,1]<-kgom1_8;
Data[i,2]<-Mass1;
Data[i,3]<-Mass8; if(Data[i] != 0) i<-i+1

kgom1_9<-(log(-log(Mass1/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days1)
Data[i,1]<-kgom1_9;
Data[i,2]<-Mass1;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom1_10<-(log(-log(Mass1/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days1)
Data[i,1]<-kgom1_10;
Data[i,2]<-Mass1;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom1_11<-(log(-log(Mass1/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days1)
Data[i,1]<-kgom1_11;
Data[i,2]<-Mass1;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom1_12<-(log(-log(Mass1/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days1)
Data[i,1]<-kgom1_12;
Data[i,2]<-Mass1;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom1_13<-(log(-log(Mass1/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days1)
Data[i,1]<-kgom1_13;
Data[i,2]<-Mass1;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom1_14<-(log(-log(Mass1/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days1)
Data[i,1]<-kgom1_14;
Data[i,2]<-Mass1;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom1_15<-(log(-log(Mass1/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days1)
Data[i,1]<-kgom1_15;
Data[i,2]<-Mass1;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom1_16<-(log(-log(Mass1/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days1)
Data[i,1]<-kgom1_16;
Data[i,2]<-Mass1;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom1_17<-(log(-log(Mass1/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days1)
Data[i,1]<-kgom1_17;
Data[i,2]<-Mass1;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom1_18<-(log(-log(Mass1/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days1)
Data[i,1]<-kgom1_18;
Data[i,2]<-Mass1;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom1_19<-(log(-log(Mass1/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days1)
Data[i,1]<-kgom1_19;
Data[i,2]<-Mass1;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom1_20<-(log(-log(Mass1/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days1)
Data[i,1]<-kgom1_20;
Data[i,2]<-Mass1;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom1_21<-(log(-log(Mass1/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days1)
Data[i,1]<-kgom1_21;
Data[i,2]<-Mass1;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom1_22<-(log(-log(Mass1/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days1)
Data[i,1]<-kgom1_22;
Data[i,2]<-Mass1;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom2_3<-(log(-log(Mass2/MassAss))-log(-log(Mass3/MassAss)))/(Days3-Days2)
Data[i,1]<-kgom2_3;
Data[i,4]<-(Date1+Days2);
Data[i,5]<-(Date1+Days3);
Data[i,2]<-Mass2;
Data[i,3]<-Mass3; if(Data[i] != 0) i<-i+1

kgom2_4<-(log(-log(Mass2/MassAss))-log(-log(Mass4/MassAss)))/(Days4-Days2)
Data[i,1]<-kgom2_4;
Data[i,2]<-Mass2;
Data[i,3]<-Mass4; if(Data[i] != 0) i<-i+1

kgom2_5<-(log(-log(Mass2/MassAss))-log(-log(Mass5/MassAss)))/(Days5-Days2)
Data[i,1]<-kgom2_5;
Data[i,2]<-Mass2;
Data[i,3]<-Mass5; if(Data[i] != 0) i<-i+1

kgom2_6<-(log(-log(Mass2/MassAss))-log(-log(Mass6/MassAss)))/(Days6-Days2)
Data[i,1]<-kgom2_6;
Data[i,2]<-Mass2;
Data[i,3]<-Mass6; if(Data[i] != 0) i<-i+1

kgom2_7<-(log(-log(Mass2/MassAss))-log(-log(Mass7/MassAss)))/(Days7-Days2)
Data[i,1]<-kgom2_7;
Data[i,2]<-Mass2;
Data[i,3]<-Mass7; if(Data[i] != 0) i<-i+1

kgom2_8<-(log(-log(Mass2/MassAss))-log(-log(Mass8/MassAss)))/(Days8-Days2)
Data[i,1]<-kgom2_8;
Data[i,2]<-Mass2;
Data[i,3]<-Mass8; if(Data[i] != 0) i<-i+1

kgom2_9<-(log(-log(Mass2/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days2)
Data[i,1]<-kgom2_9;
Data[i,2]<-Mass2;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom2_10<-(log(-log(Mass2/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days2)
Data[i,1]<-kgom2_10;
Data[i,2]<-Mass2;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom2_11<-(log(-log(Mass2/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days2)
Data[i,1]<-kgom2_11;
Data[i,2]<-Mass2;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom2_12<-(log(-log(Mass2/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days2)
Data[i,1]<-kgom2_12;
Data[i,2]<-Mass2;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom2_13<-(log(-log(Mass2/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days2)
Data[i,1]<-kgom2_13;
Data[i,2]<-Mass2;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom2_14<-(log(-log(Mass2/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days2)
Data[i,1]<-kgom2_14;
Data[i,2]<-Mass2;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom2_15<-(log(-log(Mass2/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days2)
Data[i,1]<-kgom2_15;
Data[i,2]<-Mass2;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom2_16<-(log(-log(Mass2/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days2)
Data[i,1]<-kgom2_16;
Data[i,2]<-Mass2;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom2_17<-(log(-log(Mass2/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days2)
Data[i,1]<-kgom2_17;
Data[i,2]<-Mass2;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom2_18<-(log(-log(Mass2/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days2)
Data[i,1]<-kgom2_18;
Data[i,2]<-Mass2;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom2_19<-(log(-log(Mass2/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days2)
Data[i,1]<-kgom2_19;
Data[i,2]<-Mass2;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom2_20<-(log(-log(Mass2/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days2)
Data[i,1]<-kgom2_20;
Data[i,2]<-Mass2;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom2_21<-(log(-log(Mass2/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days2)
Data[i,1]<-kgom2_21;
Data[i,2]<-Mass2;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom2_22<-(log(-log(Mass2/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days2)
Data[i,1]<-kgom2_22;
Data[i,2]<-Mass2;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom3_4<-(log(-log(Mass3/MassAss))-log(-log(Mass4/MassAss)))/(Days4-Days3)
Data[i,1]<-kgom3_4;
Data[i,4]<-(Date1+Days3);
Data[i,5]<-(Date1+Days4);
Data[i,2]<-Mass3;
Data[i,3]<-Mass4; if(Data[i] != 0) i<-i+1

kgom3_5<-(log(-log(Mass3/MassAss))-log(-log(Mass5/MassAss)))/(Days5-Days3)
Data[i,1]<-kgom3_5;
Data[i,2]<-Mass3;
Data[i,3]<-Mass5; if(Data[i] != 0) i<-i+1

kgom3_6<-(log(-log(Mass3/MassAss))-log(-log(Mass6/MassAss)))/(Days6-Days3)
Data[i,1]<-kgom3_6;
Data[i,2]<-Mass3;
Data[i,3]<-Mass6; if(Data[i] != 0) i<-i+1

kgom3_7<-(log(-log(Mass3/MassAss))-log(-log(Mass7/MassAss)))/(Days7-Days3)
Data[i,1]<-kgom3_7;
Data[i,2]<-Mass3;
Data[i,3]<-Mass7; if(Data[i] != 0) i<-i+1

kgom3_8<-(log(-log(Mass3/MassAss))-log(-log(Mass8/MassAss)))/(Days8-Days3)
Data[i,1]<-kgom3_8;
Data[i,2]<-Mass3;
Data[i,3]<-Mass8; if(Data[i] != 0) i<-i+1

kgom3_9<-(log(-log(Mass3/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days3)
Data[i,1]<-kgom3_9;
Data[i,2]<-Mass3;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom3_10<-(log(-log(Mass3/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days3)
Data[i,1]<-kgom3_10;
Data[i,2]<-Mass3;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom3_11<-(log(-log(Mass3/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days3)
Data[i,1]<-kgom3_11;
Data[i,2]<-Mass3;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom3_12<-(log(-log(Mass3/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days3)
Data[i,1]<-kgom3_12;
Data[i,2]<-Mass3;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom3_13<-(log(-log(Mass3/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days3)
Data[i,1]<-kgom3_13;
Data[i,2]<-Mass3;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom3_14<-(log(-log(Mass3/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days3)
Data[i,1]<-kgom3_14;
Data[i,2]<-Mass3;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom3_15<-(log(-log(Mass3/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days3)
Data[i,1]<-kgom3_15;
Data[i,2]<-Mass3;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom3_16<-(log(-log(Mass3/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days3)
Data[i,1]<-kgom3_16;
Data[i,2]<-Mass3;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom3_17<-(log(-log(Mass3/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days3)
Data[i,1]<-kgom3_17;
Data[i,2]<-Mass3;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom3_18<-(log(-log(Mass3/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days3)
Data[i,1]<-kgom3_18;
Data[i,2]<-Mass3;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom3_19<-(log(-log(Mass3/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days3)
Data[i,1]<-kgom3_19;
Data[i,2]<-Mass3;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom3_20<-(log(-log(Mass3/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days3)
Data[i,1]<-kgom3_20;
Data[i,2]<-Mass3;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom3_21<-(log(-log(Mass3/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days3)
Data[i,1]<-kgom3_21;
Data[i,2]<-Mass3;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom3_22<-(log(-log(Mass3/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days3)
Data[i,1]<-kgom3_22;
Data[i,2]<-Mass3;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom4_5<-(log(-log(Mass4/MassAss))-log(-log(Mass5/MassAss)))/(Days5-Days4)
Data[i,1]<-kgom4_5;
Data[i,4]<-(Date1+Days4);
Data[i,5]<-(Date1+Days5);
Data[i,2]<-Mass4;
Data[i,3]<-Mass5; if(Data[i] != 0) i<-i+1

kgom4_6<-(log(-log(Mass4/MassAss))-log(-log(Mass6/MassAss)))/(Days6-Days4)
Data[i,1]<-kgom4_6;
Data[i,2]<-Mass4;
Data[i,3]<-Mass6; if(Data[i] != 0) i<-i+1

kgom4_7<-(log(-log(Mass4/MassAss))-log(-log(Mass7/MassAss)))/(Days7-Days4)
Data[i,1]<-kgom4_7;
Data[i,2]<-Mass4;
Data[i,3]<-Mass7; if(Data[i] != 0) i<-i+1

kgom4_8<-(log(-log(Mass4/MassAss))-log(-log(Mass8/MassAss)))/(Days8-Days4)
Data[i,1]<-kgom4_8;
Data[i,2]<-Mass4;
Data[i,3]<-Mass8; if(Data[i] != 0) i<-i+1

kgom4_9<-(log(-log(Mass4/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days4)
Data[i,1]<-kgom4_9;
Data[i,2]<-Mass4;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom4_10<-(log(-log(Mass4/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days4)
Data[i,1]<-kgom4_10;
Data[i,2]<-Mass4;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom4_11<-(log(-log(Mass4/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days4)
Data[i,1]<-kgom4_11;
Data[i,2]<-Mass4;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom4_12<-(log(-log(Mass4/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days4)
Data[i,1]<-kgom4_12;
Data[i,2]<-Mass4;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom4_13<-(log(-log(Mass4/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days4)
Data[i,1]<-kgom4_13;
Data[i,2]<-Mass4;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom4_14<-(log(-log(Mass4/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days4)
Data[i,1]<-kgom4_14;
Data[i,2]<-Mass4;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom4_15<-(log(-log(Mass4/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days4)
Data[i,1]<-kgom4_15;
Data[i,2]<-Mass4;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom4_16<-(log(-log(Mass4/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days4)
Data[i,1]<-kgom4_16;
Data[i,2]<-Mass4;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom4_17<-(log(-log(Mass4/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days4)
Data[i,1]<-kgom4_17;
Data[i,2]<-Mass4;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom4_18<-(log(-log(Mass4/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days4)
Data[i,1]<-kgom4_18;
Data[i,2]<-Mass4;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom4_19<-(log(-log(Mass4/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days4)
Data[i,1]<-kgom4_19;
Data[i,2]<-Mass4;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom4_20<-(log(-log(Mass4/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days4)
Data[i,1]<-kgom4_20;
Data[i,2]<-Mass4;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom4_21<-(log(-log(Mass4/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days4)
Data[i,1]<-kgom4_21;
Data[i,2]<-Mass4;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom4_22<-(log(-log(Mass4/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days4)
Data[i,1]<-kgom4_22;
Data[i,2]<-Mass4;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom5_6<-(log(-log(Mass5/MassAss))-log(-log(Mass6/MassAss)))/(Days6-Days5)
Data[i,1]<-kgom5_6;
Data[i,4]<-(Date1+Days5);
Data[i,5]<-(Date1+Days6);
Data[i,2]<-Mass5;
Data[i,3]<-Mass6; if(Data[i] != 0) i<-i+1

kgom5_7<-(log(-log(Mass5/MassAss))-log(-log(Mass7/MassAss)))/(Days7-Days5)
Data[i,1]<-kgom5_7;
Data[i,2]<-Mass5;
Data[i,3]<-Mass7; if(Data[i] != 0) i<-i+1

kgom5_8<-(log(-log(Mass5/MassAss))-log(-log(Mass8/MassAss)))/(Days8-Days5)
Data[i,1]<-kgom5_8;
Data[i,2]<-Mass5;
Data[i,3]<-Mass8; if(Data[i] != 0) i<-i+1

kgom5_9<-(log(-log(Mass5/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days5)
Data[i,1]<-kgom5_9;
Data[i,2]<-Mass5;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom5_10<-(log(-log(Mass5/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days5)
Data[i,1]<-kgom5_10;
Data[i,2]<-Mass5;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom5_11<-(log(-log(Mass5/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days5)
Data[i,1]<-kgom5_11;
Data[i,2]<-Mass5;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom5_12<-(log(-log(Mass5/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days5)
Data[i,1]<-kgom5_12;
Data[i,2]<-Mass5;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom5_13<-(log(-log(Mass5/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days5)
Data[i,1]<-kgom5_13;
Data[i,2]<-Mass5;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom5_14<-(log(-log(Mass5/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days5)
Data[i,1]<-kgom5_14;
Data[i,2]<-Mass5;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom5_15<-(log(-log(Mass5/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days5)
Data[i,1]<-kgom5_15;
Data[i,2]<-Mass5;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom5_16<-(log(-log(Mass5/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days5)
Data[i,1]<-kgom5_16;
Data[i,2]<-Mass5;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom5_17<-(log(-log(Mass5/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days5)
Data[i,1]<-kgom5_17;
Data[i,2]<-Mass5;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom5_18<-(log(-log(Mass5/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days5)
Data[i,1]<-kgom5_18;
Data[i,2]<-Mass5;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom5_19<-(log(-log(Mass5/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days5)
Data[i,1]<-kgom5_19;
Data[i,2]<-Mass5;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom5_20<-(log(-log(Mass5/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days5)
Data[i,1]<-kgom5_20;
Data[i,2]<-Mass5;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom5_21<-(log(-log(Mass5/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days5)
Data[i,1]<-kgom5_21;
Data[i,2]<-Mass5;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom5_22<-(log(-log(Mass5/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days5)
Data[i,1]<-kgom5_22;
Data[i,2]<-Mass5;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom6_7<-(log(-log(Mass6/MassAss))-log(-log(Mass7/MassAss)))/(Days7-Days6)
Data[i,1]<-kgom6_7;
Data[i,4]<-(Date1+Days6);
Data[i,5]<-(Date1+Days7);
Data[i,2]<-Mass6;
Data[i,3]<-Mass7; if(Data[i] != 0) i<-i+1

kgom6_8<-(log(-log(Mass6/MassAss))-log(-log(Mass8/MassAss)))/(Days8-Days6)
Data[i,1]<-kgom6_8;
Data[i,2]<-Mass6;
Data[i,3]<-Mass8; if(Data[i] != 0) i<-i+1

kgom6_9<-(log(-log(Mass6/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days6)
Data[i,1]<-kgom6_9;
Data[i,2]<-Mass6;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom6_10<-(log(-log(Mass6/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days6)
Data[i,1]<-kgom6_10;
Data[i,2]<-Mass6;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom6_11<-(log(-log(Mass6/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days6)
Data[i,1]<-kgom6_11;
Data[i,2]<-Mass6;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom6_12<-(log(-log(Mass6/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days6)
Data[i,1]<-kgom6_12;
Data[i,2]<-Mass6;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom6_13<-(log(-log(Mass6/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days6)
Data[i,1]<-kgom6_13;
Data[i,2]<-Mass6;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom6_14<-(log(-log(Mass6/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days6)
Data[i,1]<-kgom6_14;
Data[i,2]<-Mass6;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom6_15<-(log(-log(Mass6/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days6)
Data[i,1]<-kgom6_15;
Data[i,2]<-Mass6;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom6_16<-(log(-log(Mass6/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days6)
Data[i,1]<-kgom6_16;
Data[i,2]<-Mass6;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom6_17<-(log(-log(Mass6/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days6)
Data[i,1]<-kgom6_17;
Data[i,2]<-Mass6;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom6_18<-(log(-log(Mass6/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days6)
Data[i,1]<-kgom6_18;
Data[i,2]<-Mass6;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom6_19<-(log(-log(Mass6/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days6)
Data[i,1]<-kgom6_19;
Data[i,2]<-Mass6;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom6_20<-(log(-log(Mass6/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days6)
Data[i,1]<-kgom6_20;
Data[i,2]<-Mass6;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom6_21<-(log(-log(Mass6/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days6)
Data[i,1]<-kgom6_21;
Data[i,2]<-Mass6;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom6_22<-(log(-log(Mass6/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days6)
Data[i,1]<-kgom6_22;
Data[i,2]<-Mass6;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom7_8<-(log(-log(Mass7/MassAss))-log(-log(Mass8/MassAss)))/(Days8-Days7)
Data[i,1]<-kgom7_8;
Data[i,4]<-(Date1+Days7);
Data[i,5]<-(Date1+Days8);
Data[i,2]<-Mass7;
Data[i,3]<-Mass8; if(Data[i] != 0) i<-i+1

kgom7_9<-(log(-log(Mass7/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days7)
Data[i,1]<-kgom7_9;
Data[i,2]<-Mass7;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom7_10<-(log(-log(Mass7/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days7)
Data[i,1]<-kgom7_10;
Data[i,2]<-Mass7;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom7_11<-(log(-log(Mass7/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days7)
Data[i,1]<-kgom7_11;
Data[i,2]<-Mass7;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom7_12<-(log(-log(Mass7/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days7)
Data[i,1]<-kgom7_12;
Data[i,2]<-Mass7;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom7_13<-(log(-log(Mass7/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days7)
Data[i,1]<-kgom7_13;
Data[i,2]<-Mass7;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom7_14<-(log(-log(Mass7/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days7)
Data[i,1]<-kgom7_14;
Data[i,2]<-Mass7;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom7_15<-(log(-log(Mass7/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days7)
Data[i,1]<-kgom7_15;
Data[i,2]<-Mass7;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom7_16<-(log(-log(Mass7/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days7)
Data[i,1]<-kgom7_16;
Data[i,2]<-Mass7;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom7_17<-(log(-log(Mass7/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days7)
Data[i,1]<-kgom7_17;
Data[i,2]<-Mass7;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom7_18<-(log(-log(Mass7/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days7)
Data[i,1]<-kgom7_18;
Data[i,2]<-Mass7;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom7_19<-(log(-log(Mass7/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days7)
Data[i,1]<-kgom7_19;
Data[i,2]<-Mass7;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom7_20<-(log(-log(Mass7/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days7)
Data[i,1]<-kgom7_20;
Data[i,2]<-Mass7;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom7_21<-(log(-log(Mass7/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days7)
Data[i,1]<-kgom7_21;
Data[i,2]<-Mass7;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom7_22<-(log(-log(Mass7/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days7)
Data[i,1]<-kgom7_22;
Data[i,2]<-Mass7;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom8_9<-(log(-log(Mass8/MassAss))-log(-log(Mass9/MassAss)))/(Days9-Days8)
Data[i,1]<-kgom8_9;
Data[i,4]<-(Date1+Days8);
Data[i,5]<-(Date1+Days9);
Data[i,2]<-Mass8;
Data[i,3]<-Mass9; if(Data[i] != 0) i<-i+1

kgom8_10<-(log(-log(Mass8/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days8)
Data[i,1]<-kgom8_10;
Data[i,2]<-Mass8;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom8_11<-(log(-log(Mass8/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days8)
Data[i,1]<-kgom8_11;
Data[i,2]<-Mass8;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom8_12<-(log(-log(Mass8/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days8)
Data[i,1]<-kgom8_12;
Data[i,2]<-Mass8;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom8_13<-(log(-log(Mass8/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days8)
Data[i,1]<-kgom8_13;
Data[i,2]<-Mass8;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom8_14<-(log(-log(Mass8/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days8)
Data[i,1]<-kgom8_14;
Data[i,2]<-Mass8;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom8_15<-(log(-log(Mass8/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days8)
Data[i,1]<-kgom8_15;
Data[i,2]<-Mass8;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom8_16<-(log(-log(Mass8/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days8)
Data[i,1]<-kgom8_16;
Data[i,2]<-Mass8;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom8_17<-(log(-log(Mass8/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days8)
Data[i,1]<-kgom8_17;
Data[i,2]<-Mass8;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom8_18<-(log(-log(Mass8/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days8)
Data[i,1]<-kgom8_18;
Data[i,2]<-Mass8;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom8_19<-(log(-log(Mass8/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days8)
Data[i,1]<-kgom8_19;
Data[i,2]<-Mass8;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom8_20<-(log(-log(Mass8/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days8)
Data[i,1]<-kgom8_20;
Data[i,2]<-Mass8;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom8_21<-(log(-log(Mass8/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days8)
Data[i,1]<-kgom8_21;
Data[i,2]<-Mass8;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom8_22<-(log(-log(Mass8/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days8)
Data[i,1]<-kgom8_22;
Data[i,2]<-Mass8;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom9_10<-(log(-log(Mass9/MassAss))-log(-log(Mass10/MassAss)))/(Days10-Days9)
Data[i,1]<-kgom9_10;
Data[i,4]<-(Date1+Days9);
Data[i,5]<-(Date1+Days10);
Data[i,2]<-Mass9;
Data[i,3]<-Mass10; if(Data[i] != 0) i<-i+1

kgom9_11<-(log(-log(Mass9/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days9)
Data[i,1]<-kgom9_11;
Data[i,2]<-Mass9;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom9_12<-(log(-log(Mass9/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days9)
Data[i,1]<-kgom9_12;
Data[i,2]<-Mass9;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom9_13<-(log(-log(Mass9/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days9)
Data[i,1]<-kgom9_13;
Data[i,2]<-Mass9;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom9_14<-(log(-log(Mass9/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days9)
Data[i,1]<-kgom9_14;
Data[i,2]<-Mass9;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom9_15<-(log(-log(Mass9/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days9)
Data[i,1]<-kgom9_15;
Data[i,2]<-Mass9;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom9_16<-(log(-log(Mass9/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days9)
Data[i,1]<-kgom9_16;
Data[i,2]<-Mass9;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom9_17<-(log(-log(Mass9/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days9)
Data[i,1]<-kgom9_17;
Data[i,2]<-Mass9;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom9_18<-(log(-log(Mass9/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days9)
Data[i,1]<-kgom9_18;
Data[i,2]<-Mass9;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom9_19<-(log(-log(Mass9/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days9)
Data[i,1]<-kgom9_19;
Data[i,2]<-Mass9;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom9_20<-(log(-log(Mass9/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days9)
Data[i,1]<-kgom9_20;
Data[i,2]<-Mass9;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom9_21<-(log(-log(Mass9/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days9)
Data[i,1]<-kgom9_21;
Data[i,2]<-Mass9;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom9_22<-(log(-log(Mass9/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days9)
Data[i,1]<-kgom9_22;
Data[i,2]<-Mass9;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom10_11<-(log(-log(Mass10/MassAss))-log(-log(Mass11/MassAss)))/(Days11-Days10)
Data[i,1]<-kgom10_11;
Data[i,4]<-(Date1+Days10);
Data[i,5]<-(Date1+Days11);
Data[i,2]<-Mass10;
Data[i,3]<-Mass11; if(Data[i] != 0) i<-i+1

kgom10_12<-(log(-log(Mass10/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days10)
Data[i,1]<-kgom10_12;
Data[i,2]<-Mass10;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom10_13<-(log(-log(Mass10/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days10)
Data[i,1]<-kgom10_13;
Data[i,2]<-Mass10;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom10_14<-(log(-log(Mass10/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days10)
Data[i,1]<-kgom10_14;
Data[i,2]<-Mass10;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom10_15<-(log(-log(Mass10/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days10)
Data[i,1]<-kgom10_15;
Data[i,2]<-Mass10;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom10_16<-(log(-log(Mass10/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days10)
Data[i,1]<-kgom10_16;
Data[i,2]<-Mass10;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom10_17<-(log(-log(Mass10/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days10)
Data[i,1]<-kgom10_17;
Data[i,2]<-Mass10;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom10_18<-(log(-log(Mass10/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days10)
Data[i,1]<-kgom10_18;
Data[i,2]<-Mass10;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom10_19<-(log(-log(Mass10/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days10)
Data[i,1]<-kgom10_19;
Data[i,2]<-Mass10;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom10_20<-(log(-log(Mass10/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days10)
Data[i,1]<-kgom10_20;
Data[i,2]<-Mass10;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom10_21<-(log(-log(Mass10/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days10)
Data[i,1]<-kgom10_21;
Data[i,2]<-Mass10;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom10_22<-(log(-log(Mass10/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days10)
Data[i,1]<-kgom10_22;
Data[i,2]<-Mass10;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom11_12<-(log(-log(Mass11/MassAss))-log(-log(Mass12/MassAss)))/(Days12-Days11)
Data[i,1]<-kgom11_12;
Data[i,4]<-(Date1+Days11);
Data[i,5]<-(Date1+Days12);
Data[i,2]<-Mass11;
Data[i,3]<-Mass12; if(Data[i] != 0) i<-i+1

kgom11_13<-(log(-log(Mass11/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days11)
Data[i,1]<-kgom11_13;
Data[i,2]<-Mass11;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom11_14<-(log(-log(Mass11/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days11)
Data[i,1]<-kgom11_14;
Data[i,2]<-Mass11;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom11_15<-(log(-log(Mass11/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days11)
Data[i,1]<-kgom11_15;
Data[i,2]<-Mass11;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom11_16<-(log(-log(Mass11/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days11)
Data[i,1]<-kgom11_16;
Data[i,2]<-Mass11;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom11_17<-(log(-log(Mass11/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days11)
Data[i,1]<-kgom11_17;
Data[i,2]<-Mass11;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom11_18<-(log(-log(Mass11/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days11)
Data[i,1]<-kgom11_18;
Data[i,2]<-Mass11;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom11_19<-(log(-log(Mass11/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days11)
Data[i,1]<-kgom11_19;
Data[i,2]<-Mass11;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom11_20<-(log(-log(Mass11/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days11)
Data[i,1]<-kgom11_20;
Data[i,2]<-Mass11;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom11_21<-(log(-log(Mass11/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days11)
Data[i,1]<-kgom11_21;
Data[i,2]<-Mass11;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom11_22<-(log(-log(Mass11/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days11)
Data[i,1]<-kgom11_22;
Data[i,2]<-Mass11;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom12_13<-(log(-log(Mass12/MassAss))-log(-log(Mass13/MassAss)))/(Days13-Days12)
Data[i,1]<-kgom12_13;
Data[i,4]<-(Date1+Days12);
Data[i,5]<-(Date1+Days13);
Data[i,2]<-Mass12;
Data[i,3]<-Mass13; if(Data[i] != 0) i<-i+1

kgom12_14<-(log(-log(Mass12/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days12)
Data[i,1]<-kgom12_14;
Data[i,2]<-Mass12;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom12_15<-(log(-log(Mass12/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days12)
Data[i,1]<-kgom12_15;
Data[i,2]<-Mass12;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom12_16<-(log(-log(Mass12/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days12)
Data[i,1]<-kgom12_16;
Data[i,2]<-Mass12;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom12_17<-(log(-log(Mass12/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days12)
Data[i,1]<-kgom12_17;
Data[i,2]<-Mass12;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom12_18<-(log(-log(Mass12/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days12)
Data[i,1]<-kgom12_18;
Data[i,2]<-Mass12;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom12_19<-(log(-log(Mass12/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days12)
Data[i,1]<-kgom12_19;
Data[i,2]<-Mass12;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom12_20<-(log(-log(Mass12/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days12)
Data[i,1]<-kgom12_20;
Data[i,2]<-Mass12;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom12_21<-(log(-log(Mass12/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days12)
Data[i,1]<-kgom12_21;
Data[i,2]<-Mass12;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom12_22<-(log(-log(Mass12/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days12)
Data[i,1]<-kgom12_22;
Data[i,2]<-Mass12;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom13_14<-(log(-log(Mass13/MassAss))-log(-log(Mass14/MassAss)))/(Days14-Days13)
Data[i,1]<-kgom13_14;
Data[i,4]<-(Date1+Days13);
Data[i,5]<-(Date1+Days14);
Data[i,2]<-Mass13;
Data[i,3]<-Mass14; if(Data[i] != 0) i<-i+1

kgom13_15<-(log(-log(Mass13/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days13)
Data[i,1]<-kgom13_15;
Data[i,2]<-Mass13;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom13_16<-(log(-log(Mass13/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days13)
Data[i,1]<-kgom13_16;
Data[i,2]<-Mass13;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom13_17<-(log(-log(Mass13/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days13)
Data[i,1]<-kgom13_17;
Data[i,2]<-Mass13;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom13_18<-(log(-log(Mass13/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days13)
Data[i,1]<-kgom13_18;
Data[i,2]<-Mass13;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom13_19<-(log(-log(Mass13/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days13)
Data[i,1]<-kgom13_19;
Data[i,2]<-Mass13;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom13_20<-(log(-log(Mass13/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days13)
Data[i,1]<-kgom13_20;
Data[i,2]<-Mass13;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom13_21<-(log(-log(Mass13/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days13)
Data[i,1]<-kgom13_21;
Data[i,2]<-Mass13;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom13_22<-(log(-log(Mass13/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days13)
Data[i,1]<-kgom13_22;
Data[i,2]<-Mass13;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom14_15<-(log(-log(Mass14/MassAss))-log(-log(Mass15/MassAss)))/(Days15-Days14)
Data[i,1]<-kgom14_15;
Data[i,4]<-(Date1+Days14);
Data[i,5]<-(Date1+Days15);
Data[i,2]<-Mass14;
Data[i,3]<-Mass15; if(Data[i] != 0) i<-i+1

kgom14_16<-(log(-log(Mass14/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days14)
Data[i,1]<-kgom14_16;
Data[i,2]<-Mass14;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom14_17<-(log(-log(Mass14/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days14)
Data[i,1]<-kgom14_17;
Data[i,2]<-Mass14;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom14_18<-(log(-log(Mass14/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days14)
Data[i,1]<-kgom14_18;
Data[i,2]<-Mass14;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom14_19<-(log(-log(Mass14/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days14)
Data[i,1]<-kgom14_19;
Data[i,2]<-Mass14;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom14_20<-(log(-log(Mass14/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days14)
Data[i,1]<-kgom14_20;
Data[i,2]<-Mass14;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom14_21<-(log(-log(Mass14/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days14)
Data[i,1]<-kgom14_21;
Data[i,2]<-Mass14;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom14_22<-(log(-log(Mass14/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days14)
Data[i,1]<-kgom14_22;
Data[i,2]<-Mass14;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom15_16<-(log(-log(Mass15/MassAss))-log(-log(Mass16/MassAss)))/(Days16-Days15)
Data[i,1]<-kgom15_16;
Data[i,4]<-(Date1+Days15);
Data[i,5]<-(Date1+Days16);
Data[i,2]<-Mass15;
Data[i,3]<-Mass16; if(Data[i] != 0) i<-i+1

kgom15_17<-(log(-log(Mass15/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days15)
Data[i,1]<-kgom15_17;
Data[i,2]<-Mass15;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom15_18<-(log(-log(Mass15/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days15)
Data[i,1]<-kgom15_18;
Data[i,2]<-Mass15;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom15_19<-(log(-log(Mass15/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days15)
Data[i,1]<-kgom15_19;
Data[i,2]<-Mass15;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom15_20<-(log(-log(Mass15/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days15)
Data[i,1]<-kgom15_20;
Data[i,2]<-Mass15;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom15_21<-(log(-log(Mass15/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days15)
Data[i,1]<-kgom15_21;
Data[i,2]<-Mass15;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom15_22<-(log(-log(Mass15/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days15)
Data[i,1]<-kgom15_22;
Data[i,2]<-Mass15;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom16_17<-(log(-log(Mass16/MassAss))-log(-log(Mass17/MassAss)))/(Days17-Days16)
Data[i,1]<-kgom16_17;
Data[i,4]<-(Date1+Days16);
Data[i,5]<-(Date1+Days17);
Data[i,2]<-Mass16;
Data[i,3]<-Mass17; if(Data[i] != 0) i<-i+1

kgom16_18<-(log(-log(Mass16/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days16)
Data[i,1]<-kgom16_18;
Data[i,2]<-Mass16;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom16_19<-(log(-log(Mass16/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days16)
Data[i,1]<-kgom16_19;
Data[i,2]<-Mass16;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom16_20<-(log(-log(Mass16/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days16)
Data[i,1]<-kgom16_20;
Data[i,2]<-Mass16;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom16_21<-(log(-log(Mass16/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days16)
Data[i,1]<-kgom16_21;
Data[i,2]<-Mass16;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom16_22<-(log(-log(Mass16/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days16)
Data[i,1]<-kgom16_22;
Data[i,2]<-Mass16;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom17_18<-(log(-log(Mass17/MassAss))-log(-log(Mass18/MassAss)))/(Days18-Days17)
Data[i,1]<-kgom17_18;
Data[i,4]<-(Date1+Days17);
Data[i,5]<-(Date1+Days18);
Data[i,2]<-Mass17;
Data[i,3]<-Mass18; if(Data[i] != 0) i<-i+1

kgom17_19<-(log(-log(Mass17/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days17)
Data[i,1]<-kgom17_19;
Data[i,2]<-Mass17;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom17_20<-(log(-log(Mass17/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days17)
Data[i,1]<-kgom17_20;
Data[i,2]<-Mass17;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom17_21<-(log(-log(Mass17/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days17)
Data[i,1]<-kgom17_21;
Data[i,2]<-Mass17;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom17_22<-(log(-log(Mass17/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days17)
Data[i,1]<-kgom17_22;
Data[i,2]<-Mass17;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom18_19<-(log(-log(Mass18/MassAss))-log(-log(Mass19/MassAss)))/(Days19-Days18)
Data[i,1]<-kgom18_19;
Data[i,4]<-(Date1+Days18);
Data[i,5]<-(Date1+Days19);
Data[i,2]<-Mass18;
Data[i,3]<-Mass19; if(Data[i] != 0) i<-i+1

kgom18_20<-(log(-log(Mass18/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days18)
Data[i,1]<-kgom18_20;
Data[i,2]<-Mass18;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom18_21<-(log(-log(Mass18/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days18)
Data[i,1]<-kgom18_21;
Data[i,2]<-Mass18;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom18_22<-(log(-log(Mass18/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days18)
Data[i,1]<-kgom18_22;
Data[i,2]<-Mass18;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom19_20<-(log(-log(Mass19/MassAss))-log(-log(Mass20/MassAss)))/(Days20-Days19)
Data[i,1]<-kgom19_20;
Data[i,4]<-(Date1+Days19);
Data[i,5]<-(Date1+Days20);
Data[i,2]<-Mass19;
Data[i,3]<-Mass20; if(Data[i] != 0) i<-i+1

kgom19_21<-(log(-log(Mass19/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days19)
Data[i,1]<-kgom19_21;
Data[i,2]<-Mass19;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom19_22<-(log(-log(Mass19/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days19)
Data[i,1]<-kgom19_22;
Data[i,2]<-Mass19;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom20_21<-(log(-log(Mass20/MassAss))-log(-log(Mass21/MassAss)))/(Days21-Days20)
Data[i,1]<-kgom20_21;
Data[i,4]<-(Date1+Days20);
Data[i,5]<-(Date1+Days21);
Data[i,2]<-Mass20;
Data[i,3]<-Mass21; if(Data[i] != 0) i<-i+1

kgom20_22<-(log(-log(Mass20/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days20)
Data[i,1]<-kgom20_22;
Data[i,2]<-Mass20;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1

kgom21_22<-(log(-log(Mass21/MassAss))-log(-log(Mass22/MassAss)))/(Days22-Days21)
Data[i,1]<-kgom21_22;
Data[i,4]<-(Date1+Days21);
Data[i,5]<-(Date1+Days22);
Data[i,2]<-Mass21;
Data[i,3]<-Mass22; if(Data[i] != 0) i<-i+1
                           
ifelse(i > 1, k_temps_mass <-matrix(rep(0,(i-1)*(1)),nrow=(i-1)), k_temps_mass <- 0)

j<-1
while(j > 0 & j < i){
k_temps_mass[j,1] <- Data[j,1]; j<-j+1}
k_median_mass <- median(k_temps_mass)

ka.o<-cbind(Data[Data[,1]>0,])
write.table(ka.o, paste('./',as.character(Chick),'KGomMass.csv',sep=''),sep=",",row.names=F,col.names=F,quote=F)

######################################################################################################################################
 
                                                                                                                                                           
                                      
                                      
################################### Works out the logistic k for Mass ############################################################
######################################################################################################################################

i<-1
Data<-matrix(rep(0,length(231)*(1)),nrow=231)

klog1_2<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass2 - 1))/(Days2-Days1)
Data[i,]<-klog1_2; i<-i+1

klog1_3<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass3 - 1))/(Days3-Days1)
Data[i,]<-klog1_3; i<-i+1

klog1_4<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass4 - 1))/(Days4-Days1)
Data[i,]<-klog1_4; i<-i+1

klog1_5<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass5 - 1))/(Days5-Days1)
Data[i,]<-klog1_5; i<-i+1

klog1_6<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass6 - 1))/(Days6-Days1)
Data[i,]<-klog1_6; i<-i+1

klog1_7<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass7 - 1))/(Days7-Days1)
Data[i,]<-klog1_7; i<-i+1

klog1_8<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass8 - 1))/(Days8-Days1)
Data[i,]<-klog1_8; i<-i+1

klog1_9<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days1)
Data[i,]<-klog1_9; i<-i+1

klog1_10<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days1)
Data[i,]<-klog1_10; i<-i+1

klog1_11<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days1)
Data[i,]<-klog1_11; i<-i+1

klog1_12<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days1)
Data[i,]<-klog1_12; i<-i+1

klog1_13<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days1)
Data[i,]<-klog1_13; i<-i+1

klog1_14<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days1)
Data[i,]<-klog1_14; i<-i+1

klog1_15<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days1)
Data[i,]<-klog1_15; i<-i+1

klog1_16<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days1)
Data[i,]<-klog1_16; i<-i+1

klog1_17<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days1)
Data[i,]<-klog1_17; i<-i+1

klog1_18<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days1)
Data[i,]<-klog1_18; i<-i+1

klog1_19<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days1)
Data[i,]<-klog1_19; i<-i+1

klog1_20<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days1)
Data[i,]<-klog1_20; i<-i+1

klog1_21<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days1)
Data[i,]<-klog1_21; i<-i+1

klog1_22<-(log(MassAss/Mass1 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days1)
Data[i,]<-klog1_22; i<-i+1

klog2_3<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass3 - 1))/(Days3-Days2)
Data[i,]<-klog2_3; i<-i+1

klog2_4<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass4 - 1))/(Days4-Days2)
Data[i,]<-klog2_4; i<-i+1

klog2_5<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass5 - 1))/(Days5-Days2)
Data[i,]<-klog2_5; i<-i+1

klog2_6<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass6 - 1))/(Days6-Days2)
Data[i,]<-klog2_6; i<-i+1

klog2_7<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass7 - 1))/(Days7-Days2)
Data[i,]<-klog2_7; i<-i+1

klog2_8<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass8 - 1))/(Days8-Days2)
Data[i,]<-klog2_8; i<-i+1

klog2_9<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days2)
Data[i,]<-klog2_9; i<-i+1

klog2_10<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days2)
Data[i,]<-klog2_10; i<-i+1

klog2_11<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days2)
Data[i,]<-klog2_11; i<-i+1

klog2_12<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days2)
Data[i,]<-klog2_12; i<-i+1

klog2_13<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days2)
Data[i,]<-klog2_13; i<-i+1

klog2_14<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days2)
Data[i,]<-klog2_14; i<-i+1

klog2_15<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days2)
Data[i,]<-klog2_15; i<-i+1

klog2_16<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days2)
Data[i,]<-klog2_16; i<-i+1

klog2_17<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days2)
Data[i,]<-klog2_17; i<-i+1

klog2_18<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days2)
Data[i,]<-klog2_18; i<-i+1

klog2_19<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days2)
Data[i,]<-klog2_19; i<-i+1

klog2_20<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days2)
Data[i,]<-klog2_20; i<-i+1

klog2_21<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days2)
Data[i,]<-klog2_21; i<-i+1

klog2_22<-(log(MassAss/Mass2 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days2)
Data[i,]<-klog2_22; i<-i+1

klog3_4<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass4 - 1))/(Days4-Days3)
Data[i,]<-klog3_4; i<-i+1

klog3_5<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass5 - 1))/(Days5-Days3)
Data[i,]<-klog3_5; i<-i+1

klog3_6<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass6 - 1))/(Days6-Days3)
Data[i,]<-klog3_6; i<-i+1

klog3_7<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass7 - 1))/(Days7-Days3)
Data[i,]<-klog3_7; i<-i+1

klog3_8<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass8 - 1))/(Days8-Days3)
Data[i,]<-klog3_8; i<-i+1

klog3_9<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days3)
Data[i,]<-klog3_9; i<-i+1

klog3_10<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days3)
Data[i,]<-klog3_10; i<-i+1

klog3_11<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days3)
Data[i,]<-klog3_11; i<-i+1

klog3_12<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days3)
Data[i,]<-klog3_12; i<-i+1

klog3_13<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days3)
Data[i,]<-klog3_13; i<-i+1

klog3_14<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days3)
Data[i,]<-klog3_14; i<-i+1

klog3_15<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days3)
Data[i,]<-klog3_15; i<-i+1

klog3_16<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days3)
Data[i,]<-klog3_16; i<-i+1

klog3_17<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days3)
Data[i,]<-klog3_17; i<-i+1

klog3_18<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days3)
Data[i,]<-klog3_18; i<-i+1

klog3_19<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days3)
Data[i,]<-klog3_19; i<-i+1

klog3_20<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days3)
Data[i,]<-klog3_20; i<-i+1

klog3_21<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days3)
Data[i,]<-klog3_21; i<-i+1

klog3_22<-(log(MassAss/Mass3 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days3)
Data[i,]<-klog3_22; i<-i+1

klog4_5<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass5 - 1))/(Days5-Days4)
Data[i,]<-klog4_5; i<-i+1

klog4_6<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass6 - 1))/(Days6-Days4)
Data[i,]<-klog4_6; i<-i+1

klog4_7<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass7 - 1))/(Days7-Days4)
Data[i,]<-klog4_7; i<-i+1

klog4_8<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass8 - 1))/(Days8-Days4)
Data[i,]<-klog4_8; i<-i+1

klog4_9<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days4)
Data[i,]<-klog4_9; i<-i+1

klog4_10<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days4)
Data[i,]<-klog4_10; i<-i+1

klog4_11<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days4)
Data[i,]<-klog4_11; i<-i+1

klog4_12<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days4)
Data[i,]<-klog4_12; i<-i+1

klog4_13<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days4)
Data[i,]<-klog4_13; i<-i+1

klog4_14<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days4)
Data[i,]<-klog4_14; i<-i+1

klog4_15<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days4)
Data[i,]<-klog4_15; i<-i+1

klog4_16<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days4)
Data[i,]<-klog4_16; i<-i+1

klog4_17<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days4)
Data[i,]<-klog4_17; i<-i+1

klog4_18<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days4)
Data[i,]<-klog4_18; i<-i+1

klog4_19<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days4)
Data[i,]<-klog4_19; i<-i+1

klog4_20<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days4)
Data[i,]<-klog4_20; i<-i+1

klog4_21<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days4)
Data[i,]<-klog4_21; i<-i+1

klog4_22<-(log(MassAss/Mass4 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days4)
Data[i,]<-klog4_22; i<-i+1

klog5_6<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass6 - 1))/(Days6-Days5)
Data[i,]<-klog5_6; i<-i+1

klog5_7<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass7 - 1))/(Days7-Days5)
Data[i,]<-klog5_7; i<-i+1

klog5_8<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass8 - 1))/(Days8-Days5)
Data[i,]<-klog5_8; i<-i+1

klog5_9<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days5)
Data[i,]<-klog5_9; i<-i+1

klog5_10<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days5)
Data[i,]<-klog5_10; i<-i+1

klog5_11<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days5)
Data[i,]<-klog5_11; i<-i+1

klog5_12<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days5)
Data[i,]<-klog5_12; i<-i+1

klog5_13<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days5)
Data[i,]<-klog5_13; i<-i+1

klog5_14<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days5)
Data[i,]<-klog5_14; i<-i+1

klog5_15<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days5)
Data[i,]<-klog5_15; i<-i+1

klog5_16<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days5)
Data[i,]<-klog5_16; i<-i+1

klog5_17<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days5)
Data[i,]<-klog5_17; i<-i+1

klog5_18<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days5)
Data[i,]<-klog5_18; i<-i+1

klog5_19<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days5)
Data[i,]<-klog5_19; i<-i+1

klog5_20<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days5)
Data[i,]<-klog5_20; i<-i+1

klog5_21<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days5)
Data[i,]<-klog5_21; i<-i+1

klog5_22<-(log(MassAss/Mass5 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days5)
Data[i,]<-klog5_22; i<-i+1

klog6_7<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass7 - 1))/(Days7-Days6)
Data[i,]<-klog6_7; i<-i+1

klog6_8<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass8 - 1))/(Days8-Days6)
Data[i,]<-klog6_8; i<-i+1

klog6_9<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days6)
Data[i,]<-klog6_9; i<-i+1

klog6_10<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days6)
Data[i,]<-klog6_10; i<-i+1

klog6_11<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days6)
Data[i,]<-klog6_11; i<-i+1

klog6_12<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days6)
Data[i,]<-klog6_12; i<-i+1

klog6_13<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days6)
Data[i,]<-klog6_13; i<-i+1

klog6_14<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days6)
Data[i,]<-klog6_14; i<-i+1

klog6_15<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days6)
Data[i,]<-klog6_15; i<-i+1

klog6_16<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days6)
Data[i,]<-klog6_16; i<-i+1

klog6_17<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days6)
Data[i,]<-klog6_17; i<-i+1

klog6_18<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days6)
Data[i,]<-klog6_18; i<-i+1

klog6_19<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days6)
Data[i,]<-klog6_19; i<-i+1

klog6_20<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days6)
Data[i,]<-klog6_20; i<-i+1

klog6_21<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days6)
Data[i,]<-klog6_21; i<-i+1

klog6_22<-(log(MassAss/Mass6 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days6)
Data[i,]<-klog6_22; i<-i+1

klog7_8<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass8 - 1))/(Days8-Days7)
Data[i,]<-klog7_8; i<-i+1

klog7_9<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days7)
Data[i,]<-klog7_9; i<-i+1

klog7_10<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days7)
Data[i,]<-klog7_10; i<-i+1

klog7_11<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days7)
Data[i,]<-klog7_11; i<-i+1

klog7_12<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days7)
Data[i,]<-klog7_12; i<-i+1

klog7_13<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days7)
Data[i,]<-klog7_13; i<-i+1

klog7_14<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days7)
Data[i,]<-klog7_14; i<-i+1

klog7_15<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days7)
Data[i,]<-klog7_15; i<-i+1

klog7_16<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days7)
Data[i,]<-klog7_16; i<-i+1

klog7_17<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days7)
Data[i,]<-klog7_17; i<-i+1

klog7_18<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days7)
Data[i,]<-klog7_18; i<-i+1

klog7_19<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days7)
Data[i,]<-klog7_19; i<-i+1

klog7_20<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days7)
Data[i,]<-klog7_20; i<-i+1

klog7_21<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days7)
Data[i,]<-klog7_21; i<-i+1

klog7_22<-(log(MassAss/Mass7 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days7)
Data[i,]<-klog7_22; i<-i+1

klog8_9<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass9 - 1))/(Days9-Days8)
Data[i,]<-klog8_9; i<-i+1

klog8_10<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days8)
Data[i,]<-klog8_10; i<-i+1

klog8_11<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days8)
Data[i,]<-klog8_11; i<-i+1

klog8_12<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days8)
Data[i,]<-klog8_12; i<-i+1

klog8_13<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days8)
Data[i,]<-klog8_13; i<-i+1

klog8_14<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days8)
Data[i,]<-klog8_14; i<-i+1

klog8_15<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days8)
Data[i,]<-klog8_15; i<-i+1

klog8_16<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days8)
Data[i,]<-klog8_16; i<-i+1

klog8_17<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days8)
Data[i,]<-klog8_17; i<-i+1

klog8_18<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days8)
Data[i,]<-klog8_18; i<-i+1

klog8_19<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days8)
Data[i,]<-klog8_19; i<-i+1

klog8_20<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days8)
Data[i,]<-klog8_20; i<-i+1

klog8_21<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days8)
Data[i,]<-klog8_21; i<-i+1

klog8_22<-(log(MassAss/Mass8 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days8)
Data[i,]<-klog8_22; i<-i+1
                          
klog9_10<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass10 - 1))/(Days10-Days9)
Data[i,]<-klog9_10; i<-i+1

klog9_11<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days9)
Data[i,]<-klog9_11; i<-i+1

klog9_12<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days9)
Data[i,]<-klog9_12; i<-i+1

klog9_13<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days9)
Data[i,]<-klog9_13; i<-i+1

klog9_14<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days9)
Data[i,]<-klog9_14; i<-i+1

klog9_15<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days9)
Data[i,]<-klog9_15; i<-i+1

klog9_16<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days9)
Data[i,]<-klog9_16; i<-i+1

klog9_17<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days9)
Data[i,]<-klog9_17; i<-i+1

klog9_18<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days9)
Data[i,]<-klog9_18; i<-i+1

klog9_19<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days9)
Data[i,]<-klog9_19; i<-i+1

klog9_20<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days9)
Data[i,]<-klog9_20; i<-i+1

klog9_21<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days9)
Data[i,]<-klog9_21; i<-i+1

klog9_22<-(log(MassAss/Mass9 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days9)
Data[i,]<-klog9_22; i<-i+1

klog10_11<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass11 - 1))/(Days11-Days10)
Data[i,]<-klog10_11; i<-i+1

klog10_12<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days10)
Data[i,]<-klog10_12; i<-i+1

klog10_13<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days10)
Data[i,]<-klog10_13; i<-i+1

klog10_14<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days10)
Data[i,]<-klog10_14; i<-i+1

klog10_15<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days10)
Data[i,]<-klog10_15; i<-i+1

klog10_16<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days10)
Data[i,]<-klog10_16; i<-i+1

klog10_17<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days10)
Data[i,]<-klog10_17; i<-i+1

klog10_18<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days10)
Data[i,]<-klog10_18; i<-i+1

klog10_19<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days10)
Data[i,]<-klog10_19; i<-i+1

klog10_20<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days10)
Data[i,]<-klog10_20; i<-i+1

klog10_21<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days10)
Data[i,]<-klog10_21; i<-i+1

klog10_22<-(log(MassAss/Mass10 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days10)
Data[i,]<-klog10_22; i<-i+1

klog11_12<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass12 - 1))/(Days12-Days11)
Data[i,]<-klog11_12; i<-i+1

klog11_13<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days11)
Data[i,]<-klog11_13; i<-i+1

klog11_14<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days11)
Data[i,]<-klog11_14; i<-i+1

klog11_15<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days11)
Data[i,]<-klog11_15; i<-i+1

klog11_16<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days11)
Data[i,]<-klog11_16; i<-i+1

klog11_17<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days11)
Data[i,]<-klog11_17; i<-i+1

klog11_18<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days11)
Data[i,]<-klog11_18; i<-i+1

klog11_19<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days11)
Data[i,]<-klog11_19; i<-i+1

klog11_20<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days11)
Data[i,]<-klog11_20; i<-i+1

klog11_21<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days11)
Data[i,]<-klog11_21; i<-i+1

klog11_22<-(log(MassAss/Mass11 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days11)
Data[i,]<-klog11_22; i<-i+1

klog12_13<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass13 - 1))/(Days13-Days12)
Data[i,]<-klog12_13; i<-i+1

klog12_14<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days12)
Data[i,]<-klog12_14; i<-i+1

klog12_15<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days12)
Data[i,]<-klog12_15; i<-i+1

klog12_16<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days12)
Data[i,]<-klog12_16; i<-i+1

klog12_17<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days12)
Data[i,]<-klog12_17; i<-i+1

klog12_18<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days12)
Data[i,]<-klog12_18; i<-i+1

klog12_19<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days12)
Data[i,]<-klog12_19; i<-i+1

klog12_20<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days12)
Data[i,]<-klog12_20; i<-i+1

klog12_21<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days12)
Data[i,]<-klog12_21; i<-i+1

klog12_22<-(log(MassAss/Mass12 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days12)
Data[i,]<-klog12_22; i<-i+1

klog13_14<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass14 - 1))/(Days14-Days13)
Data[i,]<-klog13_14; i<-i+1

klog13_15<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days13)
Data[i,]<-klog13_15; i<-i+1

klog13_16<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days13)
Data[i,]<-klog13_16; i<-i+1

klog13_17<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days13)
Data[i,]<-klog13_17; i<-i+1

klog13_18<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days13)
Data[i,]<-klog13_18; i<-i+1

klog13_19<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days13)
Data[i,]<-klog13_19; i<-i+1

klog13_20<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days13)
Data[i,]<-klog13_20; i<-i+1

klog13_21<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days13)
Data[i,]<-klog13_21; i<-i+1

klog13_22<-(log(MassAss/Mass13 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days13)
Data[i,]<-klog13_22; i<-i+1

klog14_15<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass15 - 1))/(Days15-Days14)
Data[i,]<-klog14_15; i<-i+1

klog14_16<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days14)
Data[i,]<-klog14_16; i<-i+1

klog14_17<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days14)
Data[i,]<-klog14_17; i<-i+1

klog14_18<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days14)
Data[i,]<-klog14_18; i<-i+1

klog14_19<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days14)
Data[i,]<-klog14_19; i<-i+1

klog14_20<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days14)
Data[i,]<-klog14_20; i<-i+1

klog14_21<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days14)
Data[i,]<-klog14_21; i<-i+1

klog14_22<-(log(MassAss/Mass14 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days14)
Data[i,]<-klog14_22; i<-i+1

klog15_16<-(log(MassAss/Mass15 - 1)-log(MassAss/Mass16 - 1))/(Days16-Days15)
Data[i,]<-klog15_16; i<-i+1

klog15_17<-(log(MassAss/Mass15 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days15)
Data[i,]<-klog15_17; i<-i+1

klog15_18<-(log(MassAss/Mass15 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days15)
Data[i,]<-klog15_18; i<-i+1

klog15_19<-(log(MassAss/Mass15 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days15)
Data[i,]<-klog15_19; i<-i+1

klog15_20<-(log(MassAss/Mass15 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days15)
Data[i,]<-klog15_20; i<-i+1

klog15_21<-(log(MassAss/Mass15 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days15)
Data[i,]<-klog15_21; i<-i+1

klog15_22<-(log(MassAss/Mass15 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days15)
Data[i,]<-klog15_22; i<-i+1

klog16_17<-(log(MassAss/Mass16 - 1)-log(MassAss/Mass17 - 1))/(Days17-Days16)
Data[i,]<-klog16_17; i<-i+1

klog16_18<-(log(MassAss/Mass16 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days16)
Data[i,]<-klog16_18; i<-i+1

klog16_19<-(log(MassAss/Mass16 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days16)
Data[i,]<-klog16_19; i<-i+1

klog16_20<-(log(MassAss/Mass16 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days16)
Data[i,]<-klog16_20; i<-i+1

klog16_21<-(log(MassAss/Mass16 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days16)
Data[i,]<-klog16_21; i<-i+1

klog16_22<-(log(MassAss/Mass16 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days16)
Data[i,]<-klog16_22; i<-i+1

klog17_18<-(log(MassAss/Mass17 - 1)-log(MassAss/Mass18 - 1))/(Days18-Days17)
Data[i,]<-klog17_18; i<-i+1

klog17_19<-(log(MassAss/Mass17 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days17)
Data[i,]<-klog17_19; i<-i+1

klog17_20<-(log(MassAss/Mass17 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days17)
Data[i,]<-klog17_20; i<-i+1

klog17_21<-(log(MassAss/Mass17 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days17)
Data[i,]<-klog17_21; i<-i+1

klog17_22<-(log(MassAss/Mass17 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days17)
Data[i,]<-klog17_22; i<-i+1

klog18_19<-(log(MassAss/Mass18 - 1)-log(MassAss/Mass19 - 1))/(Days19-Days18)
Data[i,]<-klog18_19; i<-i+1

klog18_20<-(log(MassAss/Mass18 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days18)
Data[i,]<-klog18_20; i<-i+1

klog18_21<-(log(MassAss/Mass18 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days18)
Data[i,]<-klog18_21; i<-i+1

klog18_22<-(log(MassAss/Mass18 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days18)
Data[i,]<-klog18_22; i<-i+1

klog19_20<-(log(MassAss/Mass19 - 1)-log(MassAss/Mass20 - 1))/(Days20-Days19)
Data[i,]<-klog19_20; i<-i+1

klog19_21<-(log(MassAss/Mass19 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days19)
Data[i,]<-klog19_21; i<-i+1

klog19_22<-(log(MassAss/Mass19 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days19)
Data[i,]<-klog19_22; i<-i+1

klog20_21<-(log(MassAss/Mass20 - 1)-log(MassAss/Mass21 - 1))/(Days21-Days20)
Data[i,]<-klog20_21; i<-i+1

klog20_22<-(log(MassAss/Mass20 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days20)
Data[i,]<-klog20_22; i<-i+1

klog21_22<-(log(MassAss/Mass21 - 1)-log(MassAss/Mass22 - 1))/(Days22-Days21)
Data[i,]<-klog21_22; i<-i+1
                          
ifelse(i > 1, k_temps_mass <-matrix(rep(0,(i-1)*(1)),nrow=(i-1)), k_temps_mass <- 0)

j<-1
while(j > 0 & j<i){
k_temps_mass[j,1] <- Data[j,1]; j<-j+1}
k_median_mass_l <- median(k_temps_mass)

ka.o<-cbind(Data[Data[,1]>0,])
write.table(ka.o, paste('./',as.character(Chick),'KLogMass.csv',sep=''),sep=",",row.names=F,col.names=F,quote=F)

######################################################################################################################################
######################################################################################################################################
############################################### Writes the chick number and k values to Kvalues_temp #######################################
         ##  Chick no.; start date; end date; k gompertz mass; blank; blank; k logistic mass; blank; blank; Total measurement period; Nest ID; chick ring ID; blank ##

Kvalues_temp[Count,1] <-Chick
Kvalues_temp[Count,2] <-Date1
Kvalues_temp[Count,3] <-Date2
Kvalues_temp[Count,4] <-(k_median_mass)
Kvalues_temp[Count,5] <-(k_median_mass_l)
Kvalues_temp[Count,6] <-TotalDays
Kvalues_temp[Count,7] <-as.character(Nest)
Kvalues_temp[Count,8] <-as.character(Ring)
Kvalues_temp[Count,9] <-0
Kvalues_temp[Count,10] <- 0
Kvalues_temp[Count,11] <- 0
Kvalues_temp[Count,12] <- 0
Kvalues_temp[Count,13] <- 0
Count <- Count + 1
          
################################# Clears the variables that need to be used again #########################################################

rm(Mass1,Mass2,Mass3,Mass4,Mass5,Mass6,Mass7,Mass8,Mass9,Mass10,Mass11,Mass12,Mass13,Mass14,Mass15,Mass16,Mass17,Mass18,Mass19,Mass20,Mass21,Mass22,
Days1,Days2,Days3,Days4,Days5,Days6,Days7,Days8,Days9,Days10,Days11,Days12,Days13,Days14,Days15,Days16,Days17,Days18,Days19,Days20,Days21,Days22)
################
################
rm(Chick,Data,datapoints,datapointsMass,Date1,Date2,TotalDays,Ring,d,i,j,k_median_mass,k_median_mass_l,k_temps_mass,ka.o,kb.o,Nest)
################
################
rm(kgom1_2,kgom1_3,kgom1_4,kgom1_5,kgom1_6,kgom1_7,kgom1_8,kgom1_9,kgom1_10,kgom1_11,kgom1_12,kgom1_13,kgom1_14,kgom1_15,kgom1_16,kgom1_17,kgom1_18,kgom1_19,kgom1_20,kgom1_21,kgom1_22,
kgom2_3,kgom2_4,kgom2_5,kgom2_6,kgom2_7,kgom2_8,kgom2_9,kgom2_10,kgom2_11,kgom2_12,kgom2_13,kgom2_14,kgom2_15,kgom2_16,kgom2_17,kgom2_18,kgom2_19,kgom2_20,kgom2_21,kgom2_22,
kgom3_4,kgom3_5,kgom3_6,kgom3_7,kgom3_8,kgom3_9,kgom3_10,kgom3_11,kgom3_12,kgom3_13,kgom3_14,kgom3_15,kgom3_16,kgom3_17,kgom3_18,kgom3_19,kgom3_20,kgom3_21,kgom3_22,
kgom4_5,kgom4_6,kgom4_7,kgom4_8,kgom4_9,kgom4_10,kgom4_11,kgom4_12,kgom4_13,kgom4_14,kgom4_15,kgom4_16,kgom4_17,kgom4_18,kgom4_19,kgom4_20,kgom4_21,kgom4_22,
kgom5_6,kgom5_7,kgom5_8,kgom5_9,kgom5_10,kgom5_11,kgom5_12,kgom5_13,kgom5_14,kgom5_15,kgom5_16,kgom5_17,kgom5_18,kgom5_19,kgom5_20,kgom5_21,kgom5_22,
kgom6_7,kgom6_8,kgom6_9,kgom6_10,kgom6_11,kgom6_12,kgom6_13,kgom6_14,kgom6_15,kgom6_16,kgom6_17,kgom6_18,kgom6_19,kgom6_20,kgom6_21,kgom6_22,
kgom7_8,kgom7_9,kgom7_10,kgom7_11,kgom7_12,kgom7_13,kgom7_14,kgom7_15,kgom7_16,kgom7_17,kgom7_18,kgom7_19,kgom7_20,kgom7_21,kgom7_22,
kgom8_9,kgom8_10,kgom8_11,kgom8_12,kgom8_13,kgom8_14,kgom8_15,kgom8_16,kgom8_17,kgom8_18,kgom8_19,kgom8_20,kgom8_21,kgom8_22,
kgom9_10,kgom9_11,kgom9_12,kgom9_13,kgom9_14,kgom9_15,kgom9_16,kgom9_17,kgom9_18,kgom9_19,kgom9_20,kgom9_21,kgom9_22,
kgom10_11,kgom10_12,kgom10_13,kgom10_14,kgom10_15,kgom10_16,kgom10_17,kgom10_18,kgom10_19,kgom10_20,kgom10_21,kgom10_22,
kgom11_12,kgom11_13,kgom11_14,kgom11_15,kgom11_16,kgom11_17,kgom11_18,kgom11_19,kgom11_20,kgom11_21,kgom11_22,
kgom12_13,kgom12_14,kgom12_15,kgom12_16,kgom12_17,kgom12_18,kgom12_19,kgom12_20,kgom12_21,kgom12_22,
kgom13_14,kgom13_15,kgom13_16,kgom13_17,kgom13_18,kgom13_19,kgom13_20,kgom13_21,kgom13_22,
kgom14_15,kgom14_16,kgom14_17,kgom14_18,kgom14_19,kgom14_20,kgom14_21,kgom14_22,
kgom15_16,kgom15_17,kgom15_18,kgom15_19,kgom15_20,kgom15_21,kgom15_22,
kgom16_17,kgom16_18,kgom16_19,kgom16_20,kgom16_21,kgom16_22,
kgom17_18,kgom17_19,kgom17_20,kgom17_21,kgom17_22,
kgom18_19,kgom18_20,kgom18_21,kgom18_22,
kgom19_20,kgom19_21,kgom19_22,
kgom20_21,kgom20_22,
kgom21_22)
################
################
rm(klog1_2,klog1_3,klog1_4,klog1_5,klog1_6,klog1_7,klog1_8,klog1_9,klog1_10,klog1_11,klog1_12,klog1_13,klog1_14,klog1_15,klog1_16,klog1_17,klog1_18,klog1_19,klog1_20,klog1_21,klog1_22,
klog2_3,klog2_4,klog2_5,klog2_6,klog2_7,klog2_8,klog2_9,klog2_10,klog2_11,klog2_12,klog2_13,klog2_14,klog2_15,klog2_16,klog2_17,klog2_18,klog2_19,klog2_20,klog2_21,klog2_22,
klog3_4,klog3_5,klog3_6,klog3_7,klog3_8,klog3_9,klog3_10,klog3_11,klog3_12,klog3_13,klog3_14,klog3_15,klog3_16,klog3_17,klog3_18,klog3_19,klog3_20,klog3_21,klog3_22,
klog4_5,klog4_6,klog4_7,klog4_8,klog4_9,klog4_10,klog4_11,klog4_12,klog4_13,klog4_14,klog4_15,klog4_16,klog4_17,klog4_18,klog4_19,klog4_20,klog4_21,klog4_22,
klog5_6,klog5_7,klog5_8,klog5_9,klog5_10,klog5_11,klog5_12,klog5_13,klog5_14,klog5_15,klog5_16,klog5_17,klog5_18,klog5_19,klog5_20,klog5_21,klog5_22,
klog6_7,klog6_8,klog6_9,klog6_10,klog6_11,klog6_12,klog6_13,klog6_14,klog6_15,klog6_16,klog6_17,klog6_18,klog6_19,klog6_20,klog6_21,klog6_22,
klog7_8,klog7_9,klog7_10,klog7_11,klog7_12,klog7_13,klog7_14,klog7_15,klog7_16,klog7_17,klog7_18,klog7_19,klog7_20,klog7_21,klog7_22,
klog8_9,klog8_10,klog8_11,klog8_12,klog8_13,klog8_14,klog8_15,klog8_16,klog8_17,klog8_18,klog8_19,klog8_20,klog8_21,klog8_22,
klog9_10,klog9_11,klog9_12,klog9_13,klog9_14,klog9_15,klog9_16,klog9_17,klog9_18,klog9_19,klog9_20,klog9_21,klog9_22,
klog10_11,klog10_12,klog10_13,klog10_14,klog10_15,klog10_16,klog10_17,klog10_18,klog10_19,klog10_20,klog10_21,klog10_22,
klog11_12,klog11_13,klog11_14,klog11_15,klog11_16,klog11_17,klog11_18,klog11_19,klog11_20,klog11_21,klog11_22,
klog12_13,klog12_14,klog12_15,klog12_16,klog12_17,klog12_18,klog12_19,klog12_20,klog12_21,klog12_22,
klog13_14,klog13_15,klog13_16,klog13_17,klog13_18,klog13_19,klog13_20,klog13_21,klog13_22,
klog14_15,klog14_16,klog14_17,klog14_18,klog14_19,klog14_20,klog14_21,klog14_22,
klog15_16,klog15_17,klog15_18,klog15_19,klog15_20,klog15_21,klog15_22,
klog16_17,klog16_18,klog16_19,klog16_20,klog16_21,klog16_22,
klog17_18,klog17_19,klog17_20,klog17_21,klog17_22,
klog18_19,klog18_20,klog18_21,klog18_22,
klog19_20,klog19_21,klog19_22,
klog20_21,klog20_22,
klog21_22)
################
################


## Displays the last 10 rows of the matrix containing the data -- for visual checks 
if(Count>10){
  Kvalues_temp[(Count-10):Count,]
} else {
  Kvalues_temp[1:Count,]
}

#######################################################################################################################################
############################################## Writes the data matrix to a final table ###############################################

#### Do only at the end, once you have run all your birds through the code - it produces a csv file of all the data ##########
# 
# K_medians_final<-matrix(rep(0,(Count)*length(Kvalues_temp[1,])),nrow=(Count))
# 
# K_medians_final[1,1] <- "Chick"
# K_medians_final[1,2] <- "Date Start"
# K_medians_final[1,3] <- "Date End"
# K_medians_final[1,4] <- "g-k Mass"
# K_medians_final[1,5] <- "l-k Mass"
# K_medians_final[1,6] <- "Total Days"
# K_medians_final[1,7] <- "Nest"
# K_medians_final[1,8] <- "RingNo"
# ###K_medians_final[1,9] <- "NotUsed"
# ###K_medians_final[1,10] <- "NotUsed"
# ###K_medians_final[1,11] <- "NotUsed"
# ###K_medians_final[1,12] <- "NotUsed"
# ###K_medians_final[1,13] <- "NotUsed"
# 
# m <- 2
# n <- 1
# while(m > 0 & m < Count+1){
# K_medians_final[m,] <- Kvalues_temp[n,]; m<-m+1; n<-n+1}
# 
# kv.o<-cbind(K_medians_final)
# write.table(kv.o,"SWALLOWS_TEST.csv",sep=",",row.names=F,col.names=F,quote=F)

############################################################################################################################################

#### EOF ####