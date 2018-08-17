## QA script for S2D
## Raga Avanasi 02/09/2018 
## QAed by George Agyeman Badu and Graham Glen
## Last modified on 06/11/2018

## Instructions:
 # After the S2D run finishes, Call s2d_QA()  with optional argument naming the run 
 # If no argument, then all runs in the "output/S2D" folder are processed


QA_read.control.file = function(control.file) {
  if (is.null(control.file)) control.file <- "S2D_control_file.txt"
  lc <- str_length(control.file)
  if(!substr(control.file,lc-3,lc)=='.txt') control.file <- paste0(control.file,".txt")
  x <- fread(paste0("input/",control.file),header=FALSE)
  setnames(x,c("key","setting"))
  x$key          <- tolower(x$key)
  chem.list      <- list(c(x$setting[x$key=="chem"]))
  puc.list       <- list(c(x$setting[x$key=="puc"]))
  fug.file       <- x$setting[x$key=="fug.file"] 
  chem.file      <- x$setting[x$key=="chem.file"]
  chem.frac.file <- x$setting[x$key=="chem.frac.file"]
  puc.type.file  <- x$setting[x$key=="puc.type.file"]
  compart.file   <- x$setting[x$key=="compart.file"]
  puc.met.file   <- x$setting[x$key=="puc.met.file"]
  skin.area.file <- x$setting[x$key=="skin.area.file"]
  removal.file   <- x$setting[x$key=="removal.file"]
  vent.file      <- x$setting[x$key=="vent.file"]
  diary.prefix   <- x$setting[x$key=="diary.prefix"]
  run.name       <- x$setting[x$key=="run.name"]
  house.seed     <- x$setting[x$key=="house.seed"]
  puc.seed       <- x$setting[x$key=="puc.seed"]
  chem.seed      <- x$setting[x$key=="chem.seed"]
  init.seed      <- x$setting[x$key=="init.seed"]
  if (length(init.seed)>0) { 
    house.seed   <- init.seed
    puc.seed     <- init.seed
    chem.seed    <- init.seed
  } else init.seed <- 0
  mode(house.seed)  <- "integer"
  mode(puc.seed)    <- "integer"
  mode(chem.seed)   <- "integer"
  first.house       <- x$setting[x$key=="first.house"]
  if (length(first.house)==0) first.house <- 1
  mode(first.house) <- "integer"
  last.house        <- x$setting[x$key=="last.house"]
  if (length(last.house)==0) last.house <- NA
  mode(last.house)  <- "integer"
  n.houses          <- x$setting[x$key=="n.houses"]
  if (length(n.houses)==0)   n.houses   <- NA
  mode(n.houses)    <- "integer"
  if (is.na(last.house) & !is.na(n.houses)) last.house <- first.house+n.houses-1
  if (is.na(n.houses) & !is.na(last.house)) n.houses   <- last.house-first.house+1
  comp.method       <- x$setting[x$key=="comp.method"]
  if (length(comp.method)==0) comp.method <- 1
  mode(comp.method) <- "integer" 
  puc.offset        <- x$setting[x$key=="puc.offset"]
  if (length(puc.offset)==0) puc.offset <- 0
  mode(puc.offset)  <- "integer"
  prog              <- substr(x$setting[x$key=="show.progress"],1,1)
  if (length(prog)==0)  prog <- "n"                     # default is no progress messages
  parallel          <- substr(x$setting[x$key=="parallel"],1,1)
  if (length(parallel)==0) parallel <- "n"              # default is not parallel
  save.r.objects    <- substr(x$setting[x$key=="save.r.objects"],1,1)
  if (length(save.r.objects)==0) save.r.objects <- "n"  # default is not to save
  out <- paste0("output/S2D/",run.name)
  g <- as.data.table(list(chem.list,puc.list,fug.file,chem.file,chem.frac.file,puc.type.file,compart.file,puc.met.file,
                          skin.area.file,removal.file,vent.file,diary.prefix,run.name,n.houses,init.seed,prog,parallel,
                          puc.offset,first.house,last.house,comp.method,save.r.objects,house.seed,puc.seed,chem.seed,out))
  setnames(g,c("chem.list","puc.list","fug.file","chem.file","chem.frac.file","puc.type.file","compart.file",
               "puc.met.file","skin.area.file","removal.file","vent.file","diary.prefix","run.name","n.houses",
               "init.seed","prog","parallel","puc.offset","first.house","last.house","comp.method","save.r.objects",
               "house.seed","puc.seed","chem.seed","out"))
  return(g)
}


# read.chem.props reads the .csv file of chemical properties 

QA_read.chem.props = function(chem.file,chem.list) {
  if (is.null(chem.file)) chem.file <- g$chem.file
  # Read chemical properties input file
  dt <- as.data.table(fread(paste0("input/",chem.file)))
  setnames(dt,names(dt),tolower(str_replace_all(names(dt),"_",".")))
  mode(dt$cas) <- "character"
  mode(dt$kp) <- "numeric"
  dt$cas <- QA_trimzero(str_replace_all(dt$cas,"-","_"))
  dt$cas <- ifelse(substr(dt$cas,1,1)=="_",paste0("0",dt$cas),dt$cas)
  if(!exists("x",dt))               dt$x       <- 1:nrow(dt)
  if(is.na(dt[1]$x))                dt[1]$x    <- 1
  y <- lag(dt$x) + 1
  dt$x[is.na(dt$x)] <- y[is.na(dt$x)]
  if (length(chem.list)>0)  dt <- dt[dt$dtxsid %in% chem.list]
  dt <- unique(dt,by="dtxsid",fromLast=TRUE)
  if(nrow(dt)==0) cat("\n  No chemicals left in fugacity calcs \n")
  if(exists("name",dt))             setnames(dt,"name","chem.name")
  if(exists("vp.pa",dt))            setnames(dt,"vp.pa","vapor")
  if(exists("mw",dt))               setnames(dt,"mw","molwt")
  if(exists("water.sol.mg.l",dt))   dt$solub   <- dt$water.sol.mg.l/dt$molwt
  if(exists("log.kow",dt))          dt$kow     <- 10^dt$log.kow
  if(exists("half.sediment.hr",dt)) dt$decay.s <- 24*log(2)/dt$half.sediment.hr 
  if(exists("half.air.hr",dt))      dt$decay.a <- 24*log(2)/dt$half.air.hr 
  dt <- dt[order(dt$dtxsid,chem.list)]
  return(dt) 
}

# read.puc.types reads the .csv file listing the product type for each PUC

QA_read.puc.types = function(puc.type.file,puc.list) {
  if (is.null(puc.type.file)) puc.type.file <- g$puc.type.file
  x <- fread(paste0("input/",puc.type.file))
  setnames(x,tolower(names(x)))
  if(exists("new_source.id",x)) setnames(x,"new_source.id","source.id")
  if(length(puc.list>0)) {
    x <- x[x$source.id %in% puc.list] 
    for (i in 1:length(puc.list)) {
      if (!puc.list[i] %in% x$source.id) cat("  PUC ",puc.list[i]," not on PUC types file")
    }
  }
  x$code    <- toupper(x$code)
  puc.types <- x[x$code!="XXX"]
  return(puc.types)
}

# read.chem.fracs loads the .csv file of chemical fractions in each product

QA_read.chem.fracs = function(chem.list,puc.list,chem.frac.file, g) {
  if (is.null(chem.frac.file)) chem.frac.file <- chem.frac.file
  x <- fread(paste0("input/",chem.frac.file))
  setnames(x,tolower(names(x)))
  if(exists("id",x))  setnames(x,"id","source.id")
  if(exists("shedsid",x)) setnames(x,"shedsid","source.id")
  x$weight_fraction[is.na(x$weight_fraction)] <- 0
  if(length(puc.list)>0) x <- x[x$source.id %in% puc.list] 
  if(g$comp.method==2) x <- x[x$dtxsid %in% chem.list]
  if(g$comp.method==2) x <- x[x$weight_fraction>0]           # for comp.method=1, do this after making brand.list
  chem.fracs <- unique(x,by=c("product_id","formulation_id","dtxsid"))
  if (nrow(chem.fracs)==0) cat ("\n No product-chemical fractions left in run \n")
  return(chem.fracs)
}

# eval.brand.list evaluates the products (brands) and formulations to be retained

QA_eval.brand.list = function(chem.list,puc.list,chem.fracs) {
  keep.puc <- rep(TRUE,length(puc.list))
  for (i in 1:length(puc.list)) {
    pchem <- unique(chem.fracs$dtxsid[chem.fracs$source.id==puc.list[i]])
    if (length(pchem %in% chem.list)==0) {
      cat ("\n  PUC ",puc.list[i]," contains no modeled chemicals")
      keep.puc[i] <- FALSE
    }
  }  
  puc       <- puc.list[keep.puc]
  brands    <- vector("list",length(puc))
  for (i in 1:length(puc)) {
    brands[[i]] <- c(unique(chem.fracs$product_id[chem.fracs$source.id==puc[i]]))
  }
  brand.list  <- data.table(puc,brands)
  setorder(brand.list,puc)
  return(brand.list)
}


# eval.chem.list evaluates the chemicals to be retained

QA_eval.chem.list = function(chem.list,puc.list,chem.fracs) {
  keep.chem <- rep(TRUE,length(chem.list))
  chems     <- unique(chem.fracs$dtxsid)
  for (i in 1:length(chem.list)) {
    chem <- chem.list[i]
    if (!chem %in% chems) {
      cat("\n  Chem ",chem," not in any modeled PUC")
      keep.chem[i] <- FALSE
    } else {
      y <- chem.fracs[chem.fracs$dtxsid==chem]
      if (max(y$weight_fraction)==0) {
        cat ("\n  Chem ",chem," always zero")
        keep.chem[i] <- FALSE
      }  
    }      
  }  
  chem.list <- unique(chem.list[keep.chem])
  return(chem.list)
}


# read.fie.list reads the list of available run folders 

QA_read.file.list = function() {
  x <- list.dirs("output/S2D",recursive=FALSE)
  if (length(x)==0) {
    cat("No output folders found")
    stop()
  }  
  if (length(x)>0) {
    y <- lapply(x,substr,12,17)
  }
  return(y)
}



# trimzero removes leading zeroes from CAS numbers

QA_trimzero = function(x) {
  y <- x
  for (j in 1:length(x)) {
    i <- 1
    while(substr(x[j],i,i)=="0" & substr(x[j],i+1,i+1)!="_") {
      substr(y[j],i,i)<- " "
      i <- i+1
    }  
  }
  return(str_trim(y))
}


#############
########
##Test criteria 1 to 6
#######
#############

s2d_QA = function(filelist="") {
  
  library(data.table)
  library(stringr)
  library(plyr)
  library(dplyr)
  library(dtplyr)
  library(ggplot2)
  library(bit64)
  library(foreach)
  library(doParallel)
  library(reshape2)
 
  print.pass.msg <- 1
  if(filelist=="") filelist<- QA_read.file.list() 
  if (length(filelist)==0) {
    cat("\n No files found")
    stop()
  }  
  for (num in 1:length(filelist)) {
  control.file  <- filelist[[num]]  
  cat(paste0("\nStarting run ",control.file,": "))  
  g             <- QA_read.control.file(control.file)
  run.name      <- g$run.name
  n.houses      <- g$last.house-g$first.house +1
  pass.count    <- 0
  fail.count    <- 0
  invalid.count <- 0
  
  annual_file <-paste0("output/S2D/",g$run.name,"/Annual/All_houses_annual.csv") 
  if(file.exists(annual_file))   {
    annual <- fread(annual_file) 
    houses_actually_run <-unique(annual$household)
    ## Added from S2D for S2D_QA_general_tests() to work as a separate function
    ##START:
    chem.list      <- unlist(g$chem.list)
    puc.list       <- unlist(g$puc.list)
    if (length(chem.list)==0) chem.list <- NULL
    if (length(puc.list)==0)   puc.list <- NULL
    fug.cvars      <- QA_read.chem.props(g$chem.file,chem.list)
    if(length(chem.list)==0 | is.null(chem.list)) chem.list <- unique(fug.cvars$dtxsid)
    puc.types      <- QA_read.puc.types(g$puc.type.file,puc.list)
    puc.list       <- as.vector(puc.types$source.id)
    chem.frac.file <- g$chem.frac.file
    chem.fracs     <- QA_read.chem.fracs(chem.list,puc.list,chem.frac.file, g)
    brand.list     <- QA_eval.brand.list(chem.list,puc.list,chem.fracs)
    puc.list       <- brand.list$puc
    chem.fracs     <- chem.fracs[chem.fracs$source.id %in% puc.list]
    chem.list      <- QA_eval.chem.list(chem.list,puc.list,chem.fracs)
    #END:
    ###Added from S2D for S2D_QA_general_tests() to work as a separate function 
  
    sink(paste0("output/S2D/",g$run.name,"_QA_Criteria_tests_Report.txt"), append = FALSE)
   
    cat("#################################################################################################\n")
    cat("THE FOLLOWING ARE THE QA TEST RESULTS FOR THE HOUSEHOLDS FOR WHICH THE S2D RUN PRODUCED AN OUTPUT\n")
    cat("#################################################################################################\n")
  
    mass_intake   <- NULL
    systemic_dose <- NULL  
  
    ###Test criteria 1: Mass ingested + Mass inspired + Mass on skin - total systemic dose >=  0
    for(i in 1: nrow(annual)) {
      mass_intake[i]   <-  annual$dir.derm.exp[i] + annual$dir.ingest.exp[i] + annual$dir.inhal.mass[i] + annual$ind.derm.exp[i] 
                          + annual$ind.ingest.exp[i] + annual$ind.inhal.mass[i] 
      systemic_dose[i] <- annual$dir.derm.abs[i] + annual$dir.ingest.abs[i] + annual$dir.inhal.abs[i] + annual$ind.derm.abs[i] 
                         + annual$ind.ingest.abs[i] + annual$ind.inhal.abs[i] 
      #annual$test1[i] <-  mass_intake[i]-systemic_dose[i]
      if ((mass_intake[i]-systemic_dose[i]) < 0) {
        print(paste("Test 1 fails for house", annual$household[i], "chemical",annual$dtxsid[i], sep = " " ))
        fail.count <- fail.count +1
      } else  { 
        if(print.pass.msg == 1) {
          print(paste("Test 1 passes for house", annual$household[i], "chemical",annual$dtxsid[i], sep = " " ))
          pass.count <- pass.count + 1 
        } else {
          pass.count <- pass.count + 1
        }
      }
    }   
  
    ###Test criteria 2: Mass of chemical used in products - mass out in trash - mass out in air - mass down the drain > 0 
    for(i in 1: nrow(annual)) {
      if ( (annual$total.used[i] - annual$out.air[i]- annual$drain[i] - annual$waste[i] - annual$out.sur[i]) < 0) {
        print(paste("Test 2 fails for house", annual$household[i], "chemical", annual$dtxsid[i], sep = " " ))
        fail.count <- fail.count +1
      } else {
        if(print.pass.msg == 1) {
          print(paste("Test 2 passes for house", annual$household[i], "chemical",annual$dtxsid[i], sep = " " ))
          pass.count <- pass.count + 1
        } else {
          pass.count <- pass.count + 1
        }
      }
    } 
  

    ###Test criteria 3: Test Dose Calcs: Chemical that is used in NO diaries results in NO chemical release
      ##identify unused chemicals in each house using the prod_chem output files.
      for (h in 1: length(houses_actually_run)) {
        prod_chem_file <- paste0("output/S2D/",g$run.name,"/Prod_chem/Prod_chem_",houses_actually_run[h],".csv")
        if(file.exists(prod_chem_file))   prod_chem <- fread(prod_chem_file) else 
           print(paste("prod_chem file does not exist in the output/S2D/",g$run.name,"/Prod_chem folder for house",h))
        chemicals_used <- unique(prod_chem$chemical)
        chemicals_not_used <- setdiff(chem.list, chemicals_used)
        house_specific_annual <- annual[annual$household == houses_actually_run[h] ]
        if (length(chemicals_not_used) !=0) {
          for (chem in 1:length(chemicals_not_used)) {
            if(house_specific_annual[house_specific_annual$dtxsid == chemicals_not_used[chem]]$total.used >0) {
              print(paste("Test 3 fails for house", houses_actually_run[h], "chemical", chemicals_not_used[chem] )) 
              fail.count <- fail.count +1
            } else {
              if(print.pass.msg == 1) {
                print(paste("Test 3 passes for house", houses_actually_run[h], "chemical",chemicals_not_used[chem], sep = " " ))
                pass.count <- pass.count + 1
              } else {
                pass.count <- pass.count + 1
              }
            }
          }
        } else {
          print(paste("Test 3 for house", houses_actually_run[h], 
                      "is not valid since each chemical is used by at least one individual in the house", sep = " " ))
          invalid.count <- invalid.count +1
        }
      }
  
    ##Test criteria 6: If primary individual did not use any PUC being tested, NO direct exposure should be seen.  
    for (d in 1: length(houses_actually_run)) { 
      diary_file <-paste0("output/ABM/household_", houses_actually_run[d], ".csv") #file with path
      if(file.exists(diary_file))   diary <- fread(diary_file) 
      setnames(diary,tolower(names(diary)))
      diary_prim <-diary[diary$primary.person==1,]
      diary_prim_pucs <-unique(diary_prim$sheds.id.refined)
      if(length(intersect(diary_prim_pucs, puc.list)) != 0) {
        print(paste("Primary individual in house", houses_actually_run[d],
                    "uses at least 1 PUC, so Test 6 is invalid")); invalid.count <- invalid.count +1
      } else {
        annual_house <-annual[annual$household == houses_actually_run[d],] 
        for(i in 1: nrow(annual_house)) {
          if(annual_house$dir.derm.exp[i] >0 || annual_house$dir.inhal.exp[i] >0 || annual_house$dir.ingest.exp[i] >0) {
            print(paste("Test 6 fails for house", annual_house$household[i], "chemical", annual_house$dtxsid[i], sep = " " )) 
            fail.count <- fail.count +1 
          } else {
            if(print.pass.msg == 1) {
              print(paste("Test 6 passes for house", annual_house$household[i], "chemical", annual_house$dtxsid[i], sep = " " ))
              pass.count <- pass.count + 1
            } else {
              pass.count <- pass.count + 1
            }
          }
        }
      }  
    }
    
    
    
    ##Tests that can be conducted when only a single PUC is evaluated in the S2D run:
      # Create a PUC list with Indoor-Outdoor flag
      Compartment_fractions <- fread(paste0("input/",g$compart.file))
      PUC_product_codes     <- fread(paste0("input/",g$puc.type.file))
      setnames(PUC_product_codes,"new_source.id","source.id")
      Indoor_outdoor        <- merge(PUC_product_codes, Compartment_fractions, by= "code" )

      ##If a single PUC is being evaluated
      if (length(puc.list)==1) {
   
        ### Test criteria 4: Chemical with NO dermal application has NO direct dermal exposure. 
        skin.area.file <- g$skin.area.file  
        x <- fread(paste0("input/",skin.area.file))
        setnames(x,tolower(names(x)))
        all.dermal.pucs <- x$new_puc
        if(puc.list %in%  all.dermal.pucs) {
          print(paste("The PUC", puc.list,"is a dermal application-based PUC and Test 4 is invalid")); invalid.count <- invalid.count +1
        } else {
          for(i in 1: nrow(annual)) {
            if(annual$dir.derm.exp[i] >0) {
              print(paste("Test 4 fails for house", annual$household[i], "chemical", annual$dtxsid[i], sep = " " ))
              fail.count <- fail.count +1
            }  else {
              if(print.pass.msg == 1) {
                print(paste("Test 4 passes for house", annual$household[i], "chemical", annual$dtxsid[i], sep = " " ))
                pass.count <- pass.count + 1
              } else {
                pass.count <- pass.count + 1
              }
            }
          }
        }
        
        ## Test criteria 5: Chemical used outdoors causes no indirect exposure
        if(puc.list %in% Indoor_outdoor$source.id ) {
          if(Indoor_outdoor$indoor[Indoor_outdoor$source.id==puc.list] ==1) {
            print(paste("The PUC", puc.list, "is an indoor PUC and Test 5 is invalid for this PUC"))
            invalid.count <- invalid.count +1
          } else {
            for(i in 1: nrow(annual)) {
              if(annual$ind.derm.exp[i] >0 || annual$ind.inhal.exp[i] >0 || annual$ind.ingest.exp[i] >0) {
                print(paste("Test 5 fails for house", annual$household[i], "chemical", annual$dtxsid[i], sep = " " ))
              } else {
                if(print.pass.msg == 1) {
                  print(paste("Test 5 passes for house", annual$household[i], "chemical", annual$dtxsid[i], sep = " " ))
                  pass.count <- pass.count + 1
                } else {
                  pass.count <- pass.count + 1
                } 
              }
            } 
          } 
        } else {
          print(paste("The PUC", puc.list," currently does not have a 3 letter 'product code' and Test 5 is invalid for it"))
          invalid.count <- invalid.count +1
        }
      } else {
        print("The S2D run is evaluating more than 1 PUC and so Tests 4 and 5 are invalid")
        invalid.count <- invalid.count +2 
      }
      cat("######################################################\n")
      cat("          SUMMARY OF QA TEST RESULTS   \n")
      cat("######################################################\n")
      print(paste("The number of 'pass' messages is:",pass.count))
      print(paste("The number of 'fail' messages is:",fail.count))
      print(paste("The number of 'invalid test' messages is:",invalid.count))
      if(pass.count == 6*n.houses*length(chem.list)) print("ALL TESTS PASSED")
   } else cat(paste0("\n  File output/S2D/",g$run.name,"/Annual/All_houses_annual.csv not found"))
   if(file.exists(annual_file)) sink()
  } 
  # Repeat with next file in filelist
}

