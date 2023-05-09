#!/bin/bash
#SBATCH --job-name=ensemble
#SBATCH --output=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen-yang.su@mail.mcgill.ca  


# Ensemble jobs to launch
# sbatch 04_run_ensemble.sh 1
# sbatch 04_run_ensemble.sh 2
# sbatch 04_run_ensemble.sh 3
# sbatch 04_run_ensemble.sh 4

# 1 = "hgb_ferr_predict_hgb", 
# 2 = "hgb_only_predict_hgb",
# 3 = "hgb_ferr_predict_ferr",
# 4 = "hgb_only_predict_ferr"


cd $HOME/iron-ml-ext-val

mkdir -p ./3_intermediate/ensemble

# Rscript ./2_scripts/train_main_models.R mod_name predict_biomarkers train_biomarkers
# Rscript ./2_scripts/train__mainmodels.R {RF|EN|XGB} {predict_hgb|predict_ferr} {data_hgb_only|data_hgb_ferr} 
Rscript ./2_scripts/04_run_ensemble.R $1  # 1 | 2 | 3 | 4


