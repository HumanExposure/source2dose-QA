s2d_QA_trend_tests = function(s2d_path="C:/Users/39285/Desktop/HEM/S2D_QA") {

  #source the code that needs to be tested for various trends
  source(paste0(s2d_path,"/S2D_06112018.R")) 
  s2d("base.txt")

  
  # #Trend test 1: to test impact of Air exchange rate
  s2d("testrun1_25.txt")
  s2d("testrun1_75.txt")
  annual_base_file <-paste0("output/S2D/base/Annual/All_houses_annual.csv") #file with path
  if(file.exists(annual_base_file))   annual_base<- fread(annual_base_file) else paste("Annual output file does not exist for the base run")
  annual_testrun1_25_file <-paste0("output/S2D/testrun1_25/Annual/All_houses_annual.csv") #file with path
  if(file.exists(annual_testrun1_25_file))   annual_testrun1_25<- fread(annual_testrun1_25_file) else paste("Annual output file does not exist for the testrun1_25")
  annual_testrun1_75_file <-paste0("output/S2D/testrun1_75/Annual/All_houses_annual.csv") #file with path
  if(file.exists(annual_testrun1_75_file))   annual_testrun1_75<- fread(annual_testrun1_75_file) else paste("Annual output file does not exist for the testrun1_75")
  annual_test1_data <-cbind(annual_base,annual_testrun1_25, annual_testrun1_75)
  write.file(annual_testrun1_25,annual_base,s2d_path,"testrun1_25")
  write.file(annual_testrun1_75,annual_base,s2d_path,"testrun1_75")
  
 
  #Trend test 2: To test the impact of changing floor area and room height
  s2d("testrun2.txt")
  annual_testrun2_file <-paste0("output/S2D/testrun2/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun2_file))   annual_testrun2<- fread(annual_testrun2_file) else paste("Annual output file does not exist for the testrun2")
  annual_test2_data <-cbind(annual_base,annual_testrun2)
  write.file(annual_testrun2,annual_base,s2d_path,"testrun2")

 
  #Trend test 3:  Chemical properties VP 
  s2d("testrun3.txt")
  annual_testrun3_file <-paste0("output/S2D/testrun3/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun3_file))   annual_testrun3<- fread(annual_testrun3_file) else paste("Annual output file does not exist for the testrun3")
  annual_test3_data <-cbind(annual_base,annual_testrun3)
  write.file(annual_testrun3,annual_base,s2d_path,"testrun3")
  
  #Trend test 4:  Chemical properties Kp 
  s2d("testrun4.txt")
  annual_testrun4_file <-paste0("output/S2D/testrun4/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun4_file))   annual_testrun4<- fread(annual_testrun4_file) else paste("Annual output file does not exist for the testrun4")
  annual_test4_data <-cbind(annual_base,annual_testrun4)
  write.file(annual_testrun4,annual_base,s2d_path,"testrun4")
  
 
  #Trend test 5: Chemical properties Solubility
  s2d("testrun5.txt")
  annual_testrun5_file <-paste0("output/S2D/testrun5/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun5_file))   annual_testrun5<- fread(annual_testrun5_file) else paste("Annual output file does not exist for the testrun5")
  annual_test5_data <-cbind(annual_base,annual_testrun5)
  write.file(annual_testrun5,annual_base,s2d_path,"testrun5")

  
  #Trend test 6: Chemical properties Fabs
  s2d("testrun6.txt")
  annual_testrun6_file <-paste0("output/S2D/testrun6/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun6_file))   annual_testrun6<- fread(annual_testrun6_file) else paste("Annual output file does not exist for the testrun6")
  annual_test6_data <-cbind(annual_base,annual_testrun6)
  write.file(annual_testrun6,annual_base,s2d_path,"testrun6")


  #Trend test 7: Handling time
  s2d("testrun7.txt")
  annual_testrun7_file <-paste0("output/S2D/testrun7/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun7_file))   annual_testrun7<- fread(annual_testrun7_file) else paste("Annual output file does not exist for the testrun7")
  annual_test7_data <-cbind(annual_base,annual_testrun7)
  write.file(annual_testrun7,annual_base,s2d_path,"testrun7")

 
  #Trend test 8: mass
  s2d("testrun8.txt")
  annual_testrun8_file <-paste0("output/S2D/testrun8/Annual/All_houses_annual.csv")
  if(file.exists(annual_testrun8_file))   annual_testrun8<- fread(annual_testrun8_file) else paste("Annual output file does not exist for the testrun8")
  annual_test8_data <-cbind(annual_base,annual_testrun8)
  write.file(annual_testrun8,annual_base,s2d_path,"testrun8")
 
  trend_test_outputs<-list(annual_test1_data,annual_test2_data,annual_test3_data,annual_test4_data,annual_test5_data,annual_test6_data,annual_test7_data,annual_test8_data)
  return(trend_test_outputs)
}

ratios = function(a,b) {
  h <- a$household
  c <- a$dtxsid
  a$household <- NULL
  b$household <- NULL
  a$dtxsid    <- NULL
  b$dtxsid   <- NULL
  rows <- nrow(a)
  cols <- ncol(a)
  r <- matrix(0,nrow=rows,ncol=cols)
  for (i in 1:rows) {
    for (j in 1:cols) {
      x <- a[[i,j]]
      y <- b[[i,j]]
      if (y>0)  r[[i,j]] <- round(x/y,3)
      if (x==0) r[[i,j]] <- NA
    }  
  }
  return(data.frame(h,c,r))
}

write.file = function(a,b,path,name) {
  da <- as.data.table(a)
  db <- as.data.table(ratios(a,b))
  dz <- as.data.table(matrix(NA,nrow=3,ncol=ncol(a)))
  setnames(db,names(da))
  setnames(dz,names(da))
  all <- rbind(da,dz,db)
  write.csv(all,paste0(path,"/Output/S2D/Base/Temp/",name,".csv"),na="",row.names=FALSE)
}
