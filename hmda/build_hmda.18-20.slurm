#!/bin/bash
#SBATCH --job-name=build_hmda.18-20
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=32G
#SBATCH --time=2:00:00
#SBATCH --mail-type=end
#SBATCH --mail-type=fail
#SBATCH --mail-user=mfineman@princeton.edu
#SBATCH --output=build_hmda.18-20.out

Rscript ~/code/ztrax_hmda/hmda/build_hmda.18-20.R