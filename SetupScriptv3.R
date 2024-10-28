# Hoang Luu - 2023 Feb
# Script to redo iniital simulations done last year
# corrected to ensure assumptions such as where germination rate is not 100%

# next create the setup files 
# note that we are using the same site files as the previous simulations 

library(readr)
library(XML)


setwd("C:/Users/hl000018/OneDrive - Ohio University/Desktop/Feb2023v3")

# path add on for where the site/file data will be saved/retrieved
site.path <- "Data/Sites/"

file.path <- "C:/Users/hl000018/OneDrive - Ohio University/Desktop/Feb2023v3/"


# name added to the file 
# ForClim_Setup_(out_name)xxxxxxxxxxxxxx
# Data for sites is (outname)(site)
# change the output names 
out_name <- "HL"   

# loop to create a site files in the 'sites' list 

# list of model 'variants'

# will need to change based on the desired model type 

# x4: variant to help shade tolerant species and for "natural" undisturbed forest dynamics 
# 1x: use the unchanged reproduction method 

# 3x: seed = DBH + weather
# 4x: seed = DBH
# 5x: seed = weather 
# 6x: seed = constant; - Survival equations only

# will only use the most updated model 

variants <- c(14,34,44,54,64)

# get the unique nat keys from the 210 temps 
temp210 <- read_csv("temp210.csv")

# filter sites to ones that are in the PRISM data (the random selected)
uniqueSites <- unique(temp210$nat.key2)

sites <- uniqueSites # 210 unique sites 


for (variant in variants) {
  
  for (site in sites){
    
    out<-NULL
    
    # use base setup file for current site
    # base file is "ForClim_Setup.xml 
    # located in ..... file.path
    
    setupxml <- xmlTreeParse(paste(file.path,"ForClim_Setup_HLTest.xml",sep=""))
    
    # use xmlRoot
    # provides easy access to the top-level XMLNode object 
    #resulting from parsing an XML document.
    
    r <- xmlRoot(setupxml)
    
    # function to create xml output within R
    
    setup <- xmlOutputDOM("configData")
    
    #copy the first parse
    setup$addNode(xmlChildren(r)[[1]]) #<version>
    
    #modify siteParam node
    setup$addTag("siteParam", close=FALSE)
    setup$addTag("name", paste0(out_name,site,".xml"))
    setup$addTag("path", "Data/Sites/")
    setup$closeTag()
    
    # speciesParam
    #setup$addNode(xmlChildren(r)[[3]]) #<speciesParam>
    setup$addTag("speciesParam", close=FALSE)
    setup$addTag("name", "Species_PNW_HNL_v4.xml")
    setup$addTag("path", "Data/Species/")
    setup$closeTag()
    
    #copy - no Climate change
    setup$addNode(xmlChildren(r)[[4]]) #<climateParam>
    
    # snow param - snow module is on
    setup$addNode(xmlChildren(r)[[5]]) #<snowParam>
    
    
    # setup$addNode(xmlChildren(r)[[6]]) #<modelStructureParam> code to copy the model structure box 
    
    # will need to change based on the desired model type 
    
    setup$addTag("modelStructure", close=FALSE)
    setup$addTag("variant", paste(variant)) # change here for new simulation name
    setup$addTag("path", "Data/Species/")
    setup$addTag("GerminationRate", paste(0.001)) # change based on Latin Hyper cube input sample
    setup$addTag("TimeSeedProductionStart", paste(100)) # change based on Latin Hyper cube input sample
    setup$addTag("SaplingGrowthTime", paste(7)) # change based on Latin Hyper cube input sample
    
    setup$closeTag()
    
    
    # weather data is used here - set to false 
    
    setup$addNode(xmlChildren(r)[[7]]) #<weatherParam>
    
    # no management - set to false 
    
    setup$addNode(xmlChildren(r)[[8]]) #<managementParam>
    
    # stateParam node - use if initialized we want to initialize the stands
    #setup$addTag("states", close=FALSE)
    #setup$addTag("name", paste("StandData_pinupina_test_Out.xml",sep=""))
    #setup$addTag("path", "Data/States/")
    #setup$addTag("standDataIn", "False")
    #setup$addTag("stateIn", "False")
    #setup$addTag("stateOut", "False")
    #setup$closeTag()
    
    setup$addNode(xmlChildren(r)[[9]]) #<statesParam>
    
    #modify result node
    setup$addTag("results", close=FALSE)
    setup$addTag("name", paste0("MyResults",sep=""))
    setup$addTag("path", "Results/")
    setup$addTag("cohortOut","False")        
    setup$addTag("logDIncOut","False")
    setup$addTag("limFactOut","True")
    setup$addTag("cohortsSQLiteOut","True")
    setup$closeTag()
    
    #modify simulation node
    setup$addTag("simulation", close=FALSE)
    setup$addTag("name", paste(out_name,"_",site,"_",variant,sep="")) # change here for new simulation name 
    setup$addTag("initTime","0")
    setup$addTag("endTime","1000")    #hey! endtime here!!
    setup$addTag("runNumber","200")
    setup$addTag("viewStep","100")
    setup$addTag("viewStepSQL","1000")
    setup$addTag("startOutput","0")
    setup$addTag("seedValue","1")
    setup$closeTag()
    
    #modify output node
    setup$addTag("output", close=FALSE)
    setup$addTag("disable","false")
    setup$closeTag()
    
    # add the PNW species 
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","00")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","01")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","02")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","03")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","04")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","05")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","06")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","07")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","08")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","09")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","10")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","11")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","12")
    setup$closeTag()
    setup$addTag("actualSpecies",close=FALSE)
    setup$addTag("kID","13")
    setup$closeTag()
    
    setupfile <-  paste("ForClim_Setup_",out_name,site,"_", variant, ".xml", sep="")  #change here site name
    
    #saveXML(setup$value(), file=paste(file.path,"SetupFiles/",setupfile,sep=""))
    saveXML(setup$value(), file=paste(file.path,setupfile,sep=""))
    
  }
  
}



#### script to create the batch file

# Creates batch file

#Inputs

ForClimExeLocation <- "C:/Users/hl000018/OneDrive - Ohio University/Desktop/Build/Debug/netcoreapp3.1/ForCLim.exe"

# get list of setup xml files 

setwd("C:/Users/hl000018/OneDrive - Ohio University/Desktop/Feb2023v3")

SetupFileLocation <- "C:/Users/hl000018/OneDrive - Ohio University/Desktop/Feb2023v3"

MyList <- list.files(path = SetupFileLocation ,
                     pattern = ".xml")

# echo off in the batch file heading 
write( paste("@echo off"),
       file="myfile.bat",
       append=TRUE)

TotalSimulations <- length(MyList)

#for (i in 1:length(sites)

for (i in 1:length(MyList)){   
  
  write( paste("\"", ForClimExeLocation, "\" \"",MyList[i] , "\"", sep = ""),
         file="myfile.bat",
         append=TRUE)
  
  write( paste("echo",i, "/" , TotalSimulations,  " Simulations Done - Hoang Luu"),
         file="myfile.bat",
         append=TRUE)
  
}

## add message and pause when finished 

write( paste("echo o7734 It's finished! - Hoang Luu"),
       file="myfile.bat",
       append=TRUE)

write( paste("pause"),
       file="myfile.bat",
       append=TRUE)

# save the 100 input values used for SA 

write.csv(inputValues, "MySAInputValuesHL.csv")

