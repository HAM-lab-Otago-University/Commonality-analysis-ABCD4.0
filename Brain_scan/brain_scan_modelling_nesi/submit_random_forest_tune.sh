# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in random_forest_baseline.RDS random_forest_followup.RDS
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J random_forest_tune.${DATANAME} --array=1-${N_BATCHES} random_forest_tune.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collect_random_forest_results.sh ${DATASET}
done