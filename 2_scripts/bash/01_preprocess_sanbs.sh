#!/bin/bash
#SBATCH --job-name=01_preprocess_sanbs
#SBATCH --output=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.out  # make sure directory slurm_output exists first or file will not write 
#SBATCH --error=/home/chenyangsu/iron-ml-ext-val/3_intermediate/slurm_output/%j.err
#SBATCH --time=1:00:00
#SBATCH --mem=24GB                  
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2          
#SBATCH --mail-type=ALL
#SBATCH --mail-user=chen-yang.su@mail.mcgill.ca  


cd $HOME/iron-ml-ext-val

Rscript ./2_scripts/sanbs/01_preprocess_sanbs.R
