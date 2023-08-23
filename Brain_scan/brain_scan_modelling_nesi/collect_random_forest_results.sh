#!/bin/bash -e
#SBATCH --account=uoo03493
#SBATCH --job-name    random_forest_collate
#SBATCH --cpus-per-task 1	
#SBATCH --mem 20480MB
#SBATCH --time        01:30:00
#SBATCH --output=/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/slurm_out/collect_random_forest.%J.out # Include the job ID in the names of
#SBATCH --error=/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/slurm_error/collect_random_forest.%J.err # the output and error files

module load R/4.1.0-gimkl-2020a
DATA=$1

# Help R to flush errors and show overall job progress by printing
# "executing" and "finished" statements.
echo "Executing R ..."
srun Rscript collect_random_forest_results.R $DATA
echo "R finished."