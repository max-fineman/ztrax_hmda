#!/bin/bash
#SBATCH --job-name=ztrax_unzip_CA
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=32G
#SBATCH --time=8:00:00
#SBATCH --mail-type=end
#SBATCH --mail-type=fail
#SBATCH --mail-user=mfineman@princeton.edu
#SBATCH --output=ztrax_unzip_CA.out

Rscript ~/code/ztrax_hmda/ztrax/ztrax_unzip_CA.R