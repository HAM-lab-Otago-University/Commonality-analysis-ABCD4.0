# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in random_forest_baseline_results.RDS random_forest_followup_results.RDS
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J rf_shap.${DATANAME} --array=1-${N_BATCHES} rf_shap.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collect_rf_shap.sh ${DATASET}
done