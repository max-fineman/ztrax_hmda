#!/bin/bash
#SBATCH --job-name=ztrax_unzip
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=8G
#SBATCH --time=2:00:00
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-type=fail
#SBATCH --mail-user=mfineman@princeton.edu
#SBATCH --output=ztrax_unzip.out

Rscript ~/code/ztrax_hmda/ztrax/ztrax_unzip.R
