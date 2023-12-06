#!/bin/bash
#SBATCH --job-name=RandomForest
#SBATCH --output=/home/wanjinli/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/wanjinli/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=wanjin.li@mail.mcgill.ca  
#SBATCH --array=1-448


# Random forest jobs to launch
# sbatch 02_train_rf.sh predict_hgb data_hgb_only 
# sbatch 02_train_rf.sh predict_ferr data_hgb_only

# sbatch 02_train_rf.sh predict_hgb data_hgb_ferr 
# sbatch 02_train_rf.sh predict_ferr data_hgb_ferr 

cd $HOME/iron-ml-ext-val

mkdir -p ./3_intermediate/tune_results/main_model/{predict_hgb,predict_ferr}/{data_hgb_only,data_hgb_ferr}

# for RF, 1 hyperparams per job array 

start=$(( ${SLURM_ARRAY_TASK_ID} ))

# Rscript ./2_scripts/train_main_models.R mod_name predict_biomarkers train_biomarkers
# Rscript ./2_scripts/train_main_models.R {RF|EN|XGB} {predict_hgb|predict_ferr} {data_hgb_only|data_hgb_ferr} 
Rscript ./2_scripts/train_main_models.R RF $1 $2 $start $start