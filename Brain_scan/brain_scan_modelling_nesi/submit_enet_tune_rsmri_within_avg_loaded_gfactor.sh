# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in rsmri_within_avg_data.RDS
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J enet_tune_loaded_gfactor_rsmri.${DATANAME} --array=1-${N_BATCHES} enet_tune_loaded_gfactor_rsmri.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collate_enet_tune_loaded_gfactor_rsmri.sh ${DATASET}
done