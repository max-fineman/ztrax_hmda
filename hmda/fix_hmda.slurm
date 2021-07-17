#!/bin/bash
#SBATCH --job-name=fix_hmda
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=16G
#SBATCH --time=2:00:00
#SBATCH --mail-type=end
#SBATCH --mail-type=fail
#SBATCH --mail-user=mfineman@princeton.edu
#SBATCH --output=fix_hmda.out

Rscript ~/code/ztrax_hmda/hmda/fix_hmda.R