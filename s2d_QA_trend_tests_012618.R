##S2D QA function to test expected trends in output variables on changing input variables
##Raga Avanasi 01/19/18 QAed by Jeanne Luh, George Agyeman-Badu and Graham Glen, ICF

s2d_QA_trend_tests = function(s2d_path = 'C:/Users/37700/Desktop/HEM/S2D_QA/S2D_112017.R')
{
  #source the S2D code that needs to be tested for various trends
  source(s2d_path) 
  s2d("base.txt")
  
  #Trend test 1: to test impact of Air exchange rate
   s2d("testrun1_25.txt")
   s2d("testrun1_75.txt")
  annual_base_file <-paste0("output/S2D/base/Annual/All_houses_annual.csv") #file with path
  if(file.exists(annual_base_file))   annual_base<- fread(annual_base_file) else paste("Annual output file does not exist for the base run")
  
   annual_testrun1_25_file <-paste0("output/S2D/testrun1_25/Annual/All_houses_annual.csv") #file with path
   if(file.exists(annual_testrun1_25_file))   annual_testrun1_25<- fread(annual_testrun1_25_file) else paste("Annual output file does not exist for the testrun1_25 ")
   
   
   annual_testrun1_75_file <-paste0("output/S2D/testrun1_75/Annual/All_houses_annual.csv") #file with path
   if(file.exists(annual_testrun1_75_file))   annual_testrun1_75<- fread(annual_testrun1_75_file) else paste("Annual output file does not exist for the testrun1_75 ")
   
   annual_test1_data <-cbind(annual_base,annual_testrun1_25, annual_testrun1_75)
   
   for(i in 1:nrow(annual_base)){
     
     if(annual_base$out.air[i] < annual_testrun1_25$out.air[i] || annual_base$out.air[i] > annual_testrun1_75$out.air[i] || annual_base$ind.inhal.exp[i] > annual_testrun1_25$ind.inhal.exp[i] || annual_base$ind.inhal.exp[i] < annual_testrun1_75$ind.inhal.exp[i] || annual_base$ind.inhal.max[i] > annual_testrun1_25$ind.inhal.max[i] || annual_base$ind.inhal.max[i] < annual_testrun1_75$ind.inhal.max[i] || annual_base$ind.inhal.mass[i] > annual_testrun1_25$ind.inhal.mass[i] || annual_base$ind.inhal.mass[i] < annual_testrun1_75$ind.inhal.mass[i] || annual_base$ind.inhal.abs[i] > annual_testrun1_25$ind.inhal.abs[i] || annual_base$ind.inhal.abs[i] < annual_testrun1_75$ind.inhal.abs[i])
       print(paste("Trend test 1 fails for house", annual_base$household[i], "chemical", annual_base$dtxsid[i], sep = " " ))
     
   }
   
  # #Trend test 2: To test the impact of changing room height
   s2d("testrun2.txt")
   annual_testrun2_file <-paste0("output/S2D/testrun2/Annual/All_houses_annual.csv")
   if(file.exists(annual_testrun2_file))   annual_testrun2<- fread(annual_testrun2_file) else paste("Annual output file does not exist for the testrun2 ")
   
   annual_test2_data <-cbind(annual_base,annual_testrun2)
   
   for(i in 1:nrow(annual_base)){
     
     if( annual_base$ind.inhal.exp[i] < annual_testrun2$ind.inhal.exp[i] ||  annual_base$ind.inhal.max[i] < annual_testrun2$ind.inhal.max[i] ||  annual_base$ind.inhal.mass[i] < annual_testrun2$ind.inhal.mass[i] ||  annual_base$ind.inhal.abs[i] < annual_testrun2$ind.inhal.abs[i]) 
       print(paste("Trend test 2 fails for house", annual_base$household[i], "chemical", annual_base$dtxsid[i], sep = " " ))
     
   }  
  # 
   #Trend test 3:  Chemical properties Vp and Kp
   s2d("testrun3.txt")
   annual_testrun3_file <-paste0("output/S2D/testrun3/Annual/All_houses_annual.csv")
   if(file.exists(annual_testrun3_file))   annual_testrun3<- fread(annual_testrun3_file) else paste("Annual output file does not exist for the testrun3 ")
   
   annual_test3_data <-cbind(annual_base,annual_testrun3)
   
   
   for(i in 1:nrow(annual_base)){
     
     if( annual_base$dir.inhal.exp[i] > annual_testrun3$dir.inhal.exp[i] ||  annual_base$dir.inhal.max[i] > annual_testrun3$dir.inhal.max[i] ||  annual_base$dir.inhal.mass[i] > annual_testrun3$dir.inhal.mass[i] ||  annual_base$dir.inhal.abs[i] > annual_testrun3$dir.inhal.abs[i] || annual_base$ind.inhal.exp[i] > annual_testrun3$ind.inhal.exp[i] ||  annual_base$ind.inhal.max[i] > annual_testrun3$ind.inhal.max[i] ||  annual_base$ind.inhal.mass[i] > annual_testrun3$ind.inhal.mass[i] ||  annual_base$ind.inhal.abs[i] > annual_testrun3$ind.inhal.abs[i] 
         || annual_base$dir.derm.abs[i] > annual_testrun3$dir.derm.abs[i] || annual_base$ind.derm.abs[i] > annual_testrun3$ind.derm.abs[i]) 
       print(paste("Trend test 3 fails for house", annual_base$household[i], "chemical", annual_base$dtxsid[i], sep = " " ))
   }  
   
  # #Trend test 4: Chemical properties Solubility and Fabs   
   s2d("testrun4.txt")
   annual_testrun4_file <-paste0("output/S2D/testrun4/Annual/All_houses_annual.csv")
   if(file.exists(annual_testrun4_file))   annual_testrun4<- fread(annual_testrun4_file) else paste("Annual output file does not exist for the testrun4 ")
   
   annual_test4_data <-cbind(annual_base,annual_testrun4)
   
   
   for(i in 1:nrow(annual_base)){
     
     if (annual_base$dir.ingest.abs[i] > annual_testrun4$dir.ingest.abs[i] || annual_base$ind.ingest.abs[i] > annual_testrun4$ind.ingest.abs[i])
       print(paste("Trend test 4 fails for house", annual_base$household[i], "chemical", annual_base$dtxsid[i], sep = " " ))
   } 
   
   #Trend test 5: Handling time   
   s2d("testrun5.txt")
   annual_testrun5_file <-paste0("output/S2D/testrun5/Annual/All_houses_annual.csv")
   if(file.exists(annual_testrun5_file))   annual_testrun5<- fread(annual_testrun5_file) else paste("Annual output file does not exist for the testrun5 ")
   
   annual_test5_data <-cbind(annual_base,annual_testrun5)
   
  # 
   for(i in 1:nrow(annual_base)){
     
     if(annual_base$dir.derm.exp[i] > annual_testrun5$dir.derm.exp[i] || annual_base$dir.derm.max[i] > annual_testrun5$dir.derm.max[i] || annual_base$dir.derm.abs[i] > annual_testrun5$dir.derm.abs[i] || annual_base$dir.inhal.exp[i] > annual_testrun5$dir.inhal.exp[i] || annual_base$dir.inhal.max[i] > annual_testrun5$dir.inhal.max[i] || annual_base$dir.inhal.mass[i] > annual_testrun5$dir.inhal.mass[i] || annual_base$dir.inhal.abs[i] > annual_testrun5$dir.inhal.abs[i] || annual_base$dir.ingest.exp[i] > annual_testrun5$dir.ingest.exp[i] || annual_base$dir.ingest.abs[i] > annual_testrun5$dir.ingest.abs[i]  )
       print(paste("Trend test 5 fails for house", annual_base$household[i], "chemical", annual_base$dtxsid[i], sep = " " ))
     
   }
  
  #Trend test 6: mass   
  s2d("testrun6.txt")
  annual_testrun6_file <-paste0("output/S2D/testrun6/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun6_file))   annual_testrun6<- fread(annual_testrun6_file) else paste("Annual output file does not exist for the testrun6 ")
  
  annual_test6_data <-cbind(annual_base,annual_testrun6)
  
  
  for(i in 1:nrow(annual_base)){
    
    if(annual_base$dir.derm.exp[i] > annual_testrun6$dir.derm.exp[i] || annual_base$dir.derm.max[i] > annual_testrun6$dir.derm.max[i] || annual_base$dir.derm.abs[i] > annual_testrun6$dir.derm.abs[i] || annual_base$dir.inhal.exp[i] > annual_testrun6$dir.inhal.exp[i] || annual_base$dir.inhal.max[i] > annual_testrun6$dir.inhal.max[i] || annual_base$dir.inhal.mass[i] > annual_testrun6$dir.inhal.mass[i] || annual_base$dir.inhal.abs[i] > annual_testrun6$dir.inhal.abs[i] || annual_base$dir.ingest.exp[i] > annual_testrun6$dir.ingest.exp[i] || annual_base$dir.ingest.abs[i] > annual_testrun6$dir.ingest.abs[i] ||  
       annual_base$ind.derm.exp[i] > annual_testrun6$ind.derm.exp[i] || annual_base$ind.derm.max[i] > annual_testrun6$ind.derm.max[i] || annual_base$ind.derm.abs[i] > annual_testrun6$ind.derm.abs[i] || annual_base$ind.inhal.exp[i] > annual_testrun6$ind.inhal.exp[i] || annual_base$ind.inhal.max[i] > annual_testrun6$ind.inhal.max[i] || annual_base$ind.inhal.mass[i] > annual_testrun6$ind.inhal.mass[i] || annual_base$ind.inhal.abs[i] > annual_testrun6$ind.inhal.abs[i] || annual_base$ind.ingest.exp[i] > annual_testrun6$ind.ingest.exp[i] || annual_base$ind.ingest.abs[i] > annual_testrun6$ind.ingest.abs[i]
    )
      
      print(paste("Trend test 6 fails for house", annual_base$household[i], "chemical", annual_base$dtxsid[i], sep = " " ))
  }  
  
  trend_test_outputs<-list(annual_test1_data,annual_test2_data,annual_test3_data,annual_test4_data,annual_test5_data,annual_test6_data)
  return(trend_test_outputs)
}