library(dplyr)
library(oro.nifti)
library(ggplot2)
library(gridExtra)
library(tidyr)

#load all data

#load c1045 functional time series data (preprocessed, but no trend removed)
c1045 <- readNIfTI("/media/mike/UNTITLED/Autism/freesurfer_functional/c1045/bold/001/funct_data_mc_tempfilt.nii.gz",
                   reorient = FALSE)

#load brian mask
c1045.mask <- readNIfTI("/media/mike/UNTITLED/Autism/freesurfer_functional/c1045/bold/001/funct_data_mc_tempfilt_Capstone++.feat/mask.nii.gz",
                        reorient = FALSE)

#load c1045 z statistic map for biological > random
c1045.zstat <- readNIfTI("/media/mike/UNTITLED/Autism/freesurfer_functional/c1045/bold/001/funct_data_mc_tempfilt_Capstone++.feat/stats/zstat1.nii.gz",
                         reorient = FALSE)

#load c1045 cluster mask
c1045.cluster.mask <- readNIfTI("/media/mike/UNTITLED/Autism/freesurfer_functional/c1045/bold/001/funct_data_mc_tempfilt_Capstone++.feat/cluster_mask_zstat1.nii.gz",
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
group.cluster.mask <- readNIfTI("/media/mike/UNTITLED/Foundations_Course/Capstone/Capstone.gfeat/cope1.feat/cluster_mask_zstat1_reg2_c1045func.nii.gz",
                                reorient = FALSE)

#load group z statistic map for mean group biological > random
group.zstat <- readNIfTI("/media/mike/UNTITLED/Foundations_Course/Capstone/Capstone.gfeat/cope1.feat/stats/zstat1_reg2_c1045func.nii.gz",
                         reorient = FALSE)


group.as.column <- data.frame( array(group.zstat@.Data, dim = c(64*64*34,1)),
                               array(group.cluster.mask@.Data, dim = c(64*64*34,1)))

names(group.as.column) <- c("ZStat", "ClusterMask")




#Read in biological and random event paradigms
paradigm <- read.table("/media/mike/UNTITLED/Autism/BioPoint_Data_Config_Files/biological_3DD_1column_2.txt") %>%
  cbind(read.table("/media/mike/UNTITLED/Autism/BioPoint_Data_Config_Files/scrambled_3DD_1column.txt"))
names(paradigm) <- c("Biological", "Random")
paradigm <- paradigm[-c(nrow(paradigm)-1, nrow(paradigm)),] 
paradigm <- transform(paradigm, Biological = as.logical(Biological), Random = as.logical(Random))


