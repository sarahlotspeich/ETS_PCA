# Load libraries
library(haven) ## to read in .xpt data 
library(dplyr) ## for data wrangling
library(MASS) ## to simulate from multivariate normal distribution (note: this will cause conflict with dplyr::select)

# //////////////////////////////////////////////////////////////////////////////
# Read in data from NHANES /////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////
## Outcomes 
VID_L = read_xpt(file = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/VID_L.xpt") ## Vitamin D
BPXO_L = read_xpt(file = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/BPXO_L.xpt") ## Heart Rate
HDL_L = read_xpt(file = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/HDL_L.xpt") ## Lab Values
INS_L = read_xpt(file = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/INS_L.xpt") ## Insulin Levels
FOLATE_L = read_xpt(file = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/FOLATE_L.xpt") ## Ferritin
## Covariates
DR1TOT_L = read_xpt(file = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DR1TOT_L.xpt")
## Demographics
DEMO_L = read_xpt(file = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DEMO_L.xpt")

# Combine relevant data 
nhanes = 
  full_join(x = 
              full_join(x = 
                          full_join(x = 
                                      full_join(x = 
                                                  full_join(x = VID_L |> dplyr::select(SEQN, LBXVIDMS),
                                                            y = BPXO_L |> dplyr::select(SEQN, BPXOPLS1),
                                                            by = join_by(SEQN == SEQN) ),
                                                y = HDL_L |> dplyr::select(SEQN, LBDHDD), 
                                                by = join_by(SEQN == SEQN) ),
                                    y = INS_L |> dplyr::select(SEQN, LBXIN) , 
                                    by = join_by(SEQN == SEQN) ),
                        y = FOLATE_L |> dplyr::select(SEQN, LBDRFO),
                        by = join_by(SEQN == SEQN) ),
            y = DR1TOT_L |> dplyr::select(SEQN, DR1TCALC, DR1TCAFF, DR1TSFAT, DR1TALCO, DR1TFF),
            by = join_by(SEQN == SEQN) )

# Merge NHANES into demographics 
demos = left_join(x = nhanes, 
                  y = DEMO_L, 
                  by = join_by(SEQN==SEQN))

# Define useFlag based on complete data for Y, X, and Z 
demos[,"useFlag"] = ifelse(test = rowSums(is.na(demos[c(2:11, 14:15, 17, 24)])) == 0, 
                           yes = "Y",
                           no = "N")

# Subset to complete cases 
analysis_data = demos |> 
  filter(useFlag == "Y") 

# Rename columns to match notation in paper
colnames(analysis_data)[2:11] = c(paste0("Y", 1:5), paste0("X", 1:5))

## Save data 
analysis_data |> 
  write.csv("NHANES-Analysis/analysis_data_orig.csv", 
            row.names = FALSE)

# //////////////////////////////////////////////////////////////////////////////
# Add simulated covariate measurement error ////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////
## Simulate error-prone continuous covariates Xj*|Xj
### For reproducibility 
set.seed(918) 
### Simulate random errors (with variance relative to the variance of Xs)
varXs = as.numeric(apply(X = analysis_data[, c("X1", "X2", "X3", "X4", "X5")], ### calculate Var(X1), ..., Var(X5)
                         MARGIN = 2, 
                         FUN = var))
U = mvrnorm(n = nrow(analysis_data), 
            mu = rep(0, 5), ### mean vector
            Sigma = diag(varXs / 4, ### variance-covariance matrix
                         nrow = 5)) ### assuming independent errors with Var(Uj) = Var(Xj) / 4
### Add random errors to covariates to create error-prone covariates 
Xstar = analysis_data[, paste0("X", 1:5)] + U 
colnames(Xstar) = paste0("Xstar", 1:5)
analysis_data = analysis_data |> 
  bind_cols(Xstar)

## Save data 
analysis_data |> 
  write.csv("NHANES-Analysis/analysis_data_with_errors.csv", 
            row.names = FALSE)