library(dplyr)
library(oro.nifti)
library(ggplot2)
library(gridExtra)
library(tidyr)

#load all data

#load c1045 functional time series data (preprocessed, but no trend removed)
c1045 <- readNIfTI("~/Desktop/Autism/c1045/bold/001/funct_data_mc_tempfilt.nii.gz",
                   reorient = FALSE)
#/media/mike/UNTITLED/Autism/freesurfer_functional/c1045/bold/001/funct_data_mc_tempfilt.nii.gz
#load brian mask
c1045.mask <- readNIfTI("~/Desktop/Autism/c1045/bold/001/funct_data_mc_tempfilt_Capstone++.feat/mask.nii.gz",
                        reorient = FALSE)

#load c1045 z statistic map for biological > random
c1045.zstat <- readNIfTI("~/Desktop/Autism/c1045/bold/001/funct_data_mc_tempfilt_Capstone++.feat/stats/zstat1.nii.gz",
                         reorient = FALSE)

#load c1045 cluster mask
c1045.cluster.mask <- readNIfTI("~/Desktop/Autism/c1045/bold/001/funct_data_mc_tempfilt_Capstone++.feat/cluster_mask_zstat1.nii.gz",
                                reorient = FALSE)

#pull info into one data frame, one column each 
c1045.as.column <- data.frame( array(c1045.mask@.Data, dim = c(64*64*34,1)),
                               array(c1045.zstat@.Data, dim = c(64*64*34,1)),
                               array(c1045.cluster.mask@.Data, dim = c(64*64*34,1)))

names(c1045.as.column) <- c("BrainMask", "ZStat", "ClusterMask")

#organize fMRI time series data as columns over time
c1045.as.column <- 
  aperm(c1045@.Data, perm = c(4, 1, 2, 3)) %>%
  matrix(nrow = 159, ncol = 64*64*34) %>% 
  t() %>%
  data.frame() %>% 
  cbind(c1045.as.column, .)





#load group activation region mask
group.cluster.mask <- readNIfTI("~/Documents/Foundations_Course/Capstone/Capstone.gfeat/cope1.feat/cluster_mask_zstat1_reg2_c1045func.nii.gz",
                                reorient = FALSE)

#load group z statistic map for mean group biological > random
group.zstat <- readNIfTI("~/Documents/Foundations_Course/Capstone/Capstone.gfeat/cope1.feat/stats/zstat1_reg2_c1045func.nii.gz",
                         reorient = FALSE)


group.as.column <- data.frame( array(group.zstat@.Data, dim = c(64*64*34,1)),
                               array(group.cluster.mask@.Data, dim = c(64*64*34,1)))

names(group.as.column) <- c("ZStat", "ClusterMask")



###repeat the above, but using CENTERED fMRI data (mean centered at 0)
c1045.as.column.centered <- c1045.as.column
c1045.as.column.centered[,4:ncol(c1045.as.column.centered)] <-
  sweep( c1045.as.column.centered[,4:ncol(c1045.as.column.centered)], 1, 
         apply(c1045.as.column.centered[,4:ncol(c1045.as.column.centered)], 1, mean),
         "-"  )


#Read in biological and random event paradigms
paradigm <- read.table("~/Desktop/Autism/BioPoint_Data_Config_Files/biological_3DD_1column_2.txt") %>%
  cbind(read.table("~/Desktop/Autism/BioPoint_Data_Config_Files/scrambled_3DD_1column.txt"))
names(paradigm) <- c("Biological", "Random")
paradigm <- paradigm[-c(nrow(paradigm)-1, nrow(paradigm)),] 
paradigm <- transform(paradigm, Biological = as.logical(Biological), Random = as.logical(Random))




#Alternate Dataset: Fewer runs of data

num.runs <- 4
temp <- subset(c1045.as.column.centered, subset = BrainMask != 0)
#temp[,c(4:9,(num.runs*12+9):(ncol(c1045.as.column)))] <- list(NULL)

skip <- 1
#skip the first 2 runs, get the next "num.runs" runs
temp[,c(4:9,(10):(12*skip+9),((num.runs+skip)*12+9+1):(ncol(c1045.as.column.centered)))] <- list(NULL)



#skip the first 2 runs, get the next "num.runs" runs
paradigm.2runs <- paradigm[-c(1:6,7:(skip*12+6),((num.runs+skip)*12+6+1):nrow(paradigm)),]
c1045.as.column.centered.2runs <- temp

#chosen.time.pts <- strtoi(sapply(grep("X", names(model.data), value = TRUE), function(s) strsplit(s, #"X[:alnum:]*")[[1]][2]))
#model.paradigm <- paradigm[chosen.time.pts,1]

