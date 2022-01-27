#D10/13 collected 300+ mammal samples from 4 different sites stored in 6 sample-specific inventoried cryovial boxes. 
#In order to ship all these samples to 3 different labs: CCDB, LMZ and ASU, I was originally comparing sample barcodes listed in the
#RITM0041457 mammal sample destination lists, to Fulcrum inventory app data. It was time consuming to copy a barcode
#and search for samples within our inventory data, so I decided to join both tables. This will simplify the organization process by 
#generating one master spreadsheet that includes all these data. Below are the steps I took to do this.
#Shoutout to Ricky Lehrter for helping with this code!

#Load packages
library(neonUtilities)
library(ggplot2)
library(tidyverse)
library (dplyr)
library(tidyr)


#Set working directory to where you saved the Fulcrum inventory data, and the mammal sample list given to your domain by Science.
#E.g. I saved the D10 and D13 lists, and inventory data in a folder named 'Inventory_MAMsample_join'. This is what I set my working 
#directory to since this is where all the files I will be manipulating live.
setwd('~/Inventory_MAMsample_join')


#use the read.csv() function to instruct where R pulls the data from in .csv format.
#important to remember to add '.csv' to the end of your spreadsheet file.
##Name df_1 whatever you want, however YOUR_FILE needs to be an exact match of your local dataset title 

#syntax: df_1 <- read.csv("C:\Users\usually_your_last_name\Documents\Folder\YOUR_FILE.csv")

ear_inventory_boxes <- read.csv("C:\\Users\\mccahill\\Documents\\Inventory_MAMsample_join\\inventory_all_ear_boxes.csv")
#opens dataset in a separate window to review
View(ear_inventory_boxes)


mam_d10_sampleList <- read.csv("C:\\Users\\mccahill\\Documents\\Inventory_MAMsample_join\\D10sampledestinationsupdated.csv")
View(mam_d10_sample_lab_list)


mam_d13_sampleList <- read.csv("C:\\Users\\mccahill\\Documents\\Inventory_MAMsample_join\\D13sampledestinationsupdated.csv")
View(mam_d13_sample_lab_list)


#IMPORTANT: the two columns with matching values in both tables are the sample barcode and ID.
#Notice that in 'DXXsampledestinationsudpated.csv' the column titles are "sampleBarcode" and "sampleID", however
#in 'ear_inventory_boxes' column titles are listed as "samplebarcode" and "sampleid".
#In order to successfully join tables, column headers in both datasets need to be exactly the same. 
#This means you need to go into your File Explorer, navigate to the inventory dataset and 
#manually change the title of which ever column you pick to join on.

#A Closer Look: Joining on the "sampleBarcode" column is effective to reduce any potential transcription errors.
#However, using the "sampleID" column to join acts as a form of inventory app QC.
#For example, I used the sampleID and noticed there were only 19 samples listed in the final dataset instead of the correct 20. 
#This is because one sample was saved as "Rxxxx" not "NRxxxx" in the inventory app. 
#I was able to go into the app, find the record, delete & re enter the barcode number, to update the 
#inventory child record with the trap collection data. I then had to redownload the inventory data and re read
#it into R.

# 99% of the time an inner join will be what you want to use. 
# This will keep any observations, or rows, where there are matching values in 
# both df1 and df2. A full join will keep everything from both, regardless of if 
# they match. Left join will keep everything from df1, as well as any other rows 
# that match from df2. A right join does the opposite- keeps everything from df2 
# and adds any rows from df1 that match. 

#Use the dplyr package's join functions instead of the base R merge() function since it's more user friendly. 

#syntax: newdf <- df1 %>% inner_join(df2, by = "column with matching values in both tables")

inventory_d10_sampleList_join <- ear_inventory_boxes %>% inner_join(mam_d10_sampleList, by = "sampleID") 

inventory_d13_sampleList_join <- ear_inventory_boxes %>% inner_join(mam_d13_sampleList, by = "sampleID")

#Note: you cannot join d10 and d13 together since there are no matching values between the two!


#Any column that is a duplicate name as another column is 
#automatically assigned the .x by R to differentiate them
View(inventory_d10_sampleList_join)

View(inventory_d13_sampleList_join)


#use the select() function to display your columns of choice in a new dataset. 
#I selected all the columns from the 'XXsampledestinationsupdated' and any columns I needed from the inventory data.
mam_d10_sample_destinations <- inventory_d10_sampleList_join %>% select(taxonID, identificationQualifier, sex, lifeStage, tagID, sampleID, 
                                                                    sampleBarcode.y, remarks.y, fulcrumID, field_domain_id, tissueType, tissueUse, 
                                                                    destination, storagecontainercode_child, X_title, wellcoordinates, collectdate)
#final dataset has 174 rows (matches # of ear samples in D10sampledestinationsupdated.csv)
View(mam_d10_sample_destinations)



mam_d13_sample_destinations <- inventory_d13_sampleList_join %>% select(taxonID, identificationQualifier, sex, lifeStage, tagID, sampleID, 
                                                                        sampleBarcode.y, remarks.y, fulcrumID, field_domain_id, tissueType, tissueUse, 
                                                                        destination, storagecontainercode_child, X_title, wellcoordinates, collectdate)
#final dataset has 20 rows (matches # of ear samples in D13sampledestinationsudpated.csv)
View(mam_d13_sample_destinations)


#If your tables don't join correctly, data within an observation could be incorrect.
#Always triple check the values are correct within a couple rows once you've joined data tables.
#I crossed checked ~3 samples from destination lists against inventory app to ensure accurate merging. 

#Save these .csv files locally to your working directory using the write.csv() command.
#You can now open up your file(s) in Excel, filter columns to your liking, 
#and manually organize samples without bouncing around to different windows! 

write.csv(mam_d10_sample_destinations, file = 'd10_sampledestinations_inventory_joined.csv')

write.csv(mam_d13_sample_destinations, file = 'd13_sampledestinations_inventory_joined.csv')






#___________________________

#To join 3 data tables syntax:

  # jnd.tbl <- df1 %>%
  #   left_join(df2, by='b') %>%
  #   left_join(df3, by='d')

#In the first left join they are saying to join df1 and df2 by column "b" 
#in df2, then in the second join they're saying to join df2 and df3 by column "d" in df3.

