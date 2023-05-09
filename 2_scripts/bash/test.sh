#!/bin/bash
#SBATCH --job-name=XGB
#SBATCH --output=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=1:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen-yang.su@mail.mcgill.ca  
#SBATCH --array=1-7


# XGB jobs to launch
# sbatch train_xgb.sh data_hgb_only predict_hgb
# sbatch train_xgb.sh data_hgb_only predict_ferr

# sbatch train_xgb.sh data_hgb_ferr predict_hgb
# sbatch test.sh data_hgb_ferr predict_ferr

cd $HOME/iron-ml-ext-val

# for XGB, 10 hyperparams per job array (1800 hyperparams = 180 job arrays * 10)
start=$((( ${SLURM_ARRAY_TASK_ID} - 1) * 10 + 1154))

# Rscript ./2_scripts/train_models.R mod_name train_biomarkers predict_biomarkers
# Rscript ./2_scripts/train_models.R {RF|EN|XGB} {data_hgb_only|data_hgb_ferr} {predict_hgb|predict_ferr}
echo ${SLURM_ARRAY_TASK_ID}
echo $1 $2 $start $start


# 1154 1155 1156 1157 1158 1159 1160 
# 1169 1170 
# 1177 1178 1179 1180 
# 1193 1194 1195 1196 1197 1198 1199 1200

