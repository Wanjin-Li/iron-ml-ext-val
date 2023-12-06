#!/bin/bash
#SBATCH --job-name=ElasticNet
#SBATCH --output=/home/wanjinli/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/wanjinli/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=wanjin.li@mail.mcgill.ca  


# Elastic net jobs to launch
# sbatch 02_train_en.sh predict_hgb data_hgb_only 
# sbatch 02_train_en.sh predict_ferr data_hgb_only 

# sbatch 02_train_en.sh predict_hgb data_hgb_ferr 
# sbatch 02_train_en.sh predict_ferr data_hgb_ferr 

cd $HOME/iron-ml-ext-val

mkdir -p ./3_intermediate/tune_results/main_model/{predict_hgb,predict_ferr}/{data_hgb_only,data_hgb_ferr}

# Rscript ./2_scripts/train_main_models.R mod_name predict_biomarkers train_biomarkers
# Rscript ./2_scripts/train__mainmodels.R {RF|EN|XGB} {predict_hgb|predict_ferr} {data_hgb_only|data_hgb_ferr} 
Rscript ./2_scripts/train_main_models.R EN $1 $2



