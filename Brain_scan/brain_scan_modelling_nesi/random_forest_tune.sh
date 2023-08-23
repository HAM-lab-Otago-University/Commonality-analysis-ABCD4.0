#!/bin/bash -e
#SBATCH --account=uoo03493
#SBATCH --job-name    random_forest
#SBATCH --cpus-per-task 20	
#SBATCH --mem 30720MB
#SBATCH --time        60:00:00
#SBATCH --output=/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/slurm_out/rf.%J.out # Include the job ID in the names of
#SBATCH --error=/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/slurm_error/rf.%J.err # the output and error files

module load R/4.1.0-gimkl-2020a
DATA=$1

# Help R to flush errors and show overall job progress by printing
# "executing" and "finished" statements.
echo "Executing R ..."
srun Rscript random_forest_tune.R $DATA
echo "R finished."