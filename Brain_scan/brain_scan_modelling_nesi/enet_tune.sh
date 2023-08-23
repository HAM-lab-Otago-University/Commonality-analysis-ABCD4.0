#!/bin/bash -e
#SBATCH --account=uoo03493
#SBATCH --job-name    enet
#SBATCH --cpus-per-task 8	
#SBATCH --mem 10240MB
#SBATCH --time        02:00:00
#SBATCH --output=/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/slurm_out/enet.%J.out # Include the job ID in the names of
#SBATCH --error=/nesi/nobackup/uoo03493/Yue/stacking_gfactor_modelling/slurm_error/enet.%J.err # the output and error files

module load R/4.1.0-gimkl-2020a
DATA=$1

# Help R to flush errors and show overall job progress by printing
# "executing" and "finished" statements.
echo "Executing R ..."
srun Rscript enet_tune.R $DATA
echo "R finished."