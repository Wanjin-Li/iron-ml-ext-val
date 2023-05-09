#!/bin/bash
#SBATCH --job-name=train_top_model
#SBATCH --output=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen-yang.su@mail.mcgill.ca  


# Ensemble jobs to launch
# sbatch 07_train_top_model.sh 1
# sbatch 07_train_top_model.sh 2
# sbatch 07_train_top_model.sh 3
# sbatch 07_train_top_model.sh 4

# 1 = "hgb_ferr_predict_hgb", 
# 2 = "hgb_only_predict_hgb",
# 3 = "hgb_ferr_predict_ferr",
# 4 = "hgb_only_predict_ferr"


cd $HOME/iron-ml-ext-val

mkdir -p ./3_intermediate/trained_models

Rscript ./2_scripts/07_train_top_model.R $1  # 1 | 2 | 3 | 4


