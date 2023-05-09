#!/bin/bash
#SBATCH --job-name=feature_importance
#SBATCH --output=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen-yang.su@mail.mcgill.ca  


# Ensemble jobs to launch
# sbatch 09_feature_importance.sh 1
# sbatch 09_feature_importance.sh 2
# sbatch 09_feature_importance.sh 3
# sbatch 09_feature_importance.sh 4

# 1 = "hgb_ferr_predict_hgb", 
# 2 = "hgb_only_predict_hgb",
# 3 = "hgb_ferr_predict_ferr",
# 4 = "hgb_only_predict_ferr"


cd $HOME/iron-ml-ext-val

Rscript ./2_scripts/09_feature_importance.R $1  # 1 | 2 | 3 | 4


