#!/bin/bash
#SBATCH --job-name=XGB
#SBATCH --output=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen-yang.su@mail.mcgill.ca  
#SBATCH --array=1-480


# XGB jobs to launch
# sbatch train_xgb.sh predict_hgb data_hgb_only
# sbatch train_xgb.sh predict_ferr data_hgb_only

# sbatch train_xgb.sh predict_hgb data_hgb_ferr 
# sbatch train_xgb.sh predict_ferr data_hgb_ferr 

cd $HOME/iron-ml-ext-val

mkdir -p ./3_intermediate/tune_results/main_model/{predict_hgb,predict_ferr}/{data_hgb_only,data_hgb_ferr}

# for XGB, 10 hyperparams per job array (1800 hyperparams = 180 job arrays * 10)
start=$((( ${SLURM_ARRAY_TASK_ID} - 1) * 10 + 1))
end=$(( ${SLURM_ARRAY_TASK_ID} * 10 ))

# Rscript ./2_scripts/train_main_models.R mod_name predict_biomarkers train_biomarkers 
# Rscript ./2_scripts/train_main_models.R {RF|EN|XGB} {predict_hgb|predict_ferr} {data_hgb_only|data_hgb_ferr} 
Rscript ./2_scripts/train_main_models.R XGB $1 $2 $start $end

