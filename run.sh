#!/bin/bash
#SBATCH --job-name=bioclim_run_targets
#SBATCH --mail-user=rdinnage@fiu.edu
#SBATCH --mail-type=FAIL,END
#SBATCH --account=rdinnage.fiu
#SBATCH --qos=rdinnage.fiu
#SBATCH --partition=gpu
#SBATCH --gres=gpu:a100:1

# Where to put the outputs: %j expands into the job number (a unique identifier for this job)
#SBATCH --output logs/my_job%j.out
#SBATCH --error logs/my_job%j.err

# Number of nodes to use
#SBATCH --nodes=1

# Number of tasks (usually translate to processor cores) to use: important! this means the number of mpi ranks used, useless if you are not using Rmpi)
#SBATCH --ntasks=1

#number of cores to parallelize with:
#SBATCH --cpus-per-task=8
##SBATCH --mem=64G
# Memory per cpu core. Default is megabytes, but units can be specified with M
# or G for megabytes or Gigabytes.
#SBATCH --mem-per-cpu=8G

# Job run time in [DAYS]
# HOURS:MINUTES:SECONDS
# [DAYS] are optional, use when it is convenient
#SBATCH --time=96:00:00

## activate conda
export PATH=/blue/rdinnage.fiu/rdinnage.fiu/.conda/envs/rstudio-gpu/bin:$PATH

# Save some useful information to the "output" file
date;hostname;pwd

# Load R and run a command
Rscript -e "targets::tar_make(c(model_runs_2.5_lgneg3.5, model_runs_2.5_lgneg4.5))"
