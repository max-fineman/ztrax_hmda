#!/bin/bash
#SBATCH --job-name=post_merge_build
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=100G
#SBATCH --time=3:00:00
#SBATCH --mail-type=end
#SBATCH --mail-type=fail
#SBATCH --mail-user=mfineman@princeton.edu
#SBATCH --output=post_merge_build.out

Rscript ~/code/ztrax_hmda/merge/post_merge_build.R
