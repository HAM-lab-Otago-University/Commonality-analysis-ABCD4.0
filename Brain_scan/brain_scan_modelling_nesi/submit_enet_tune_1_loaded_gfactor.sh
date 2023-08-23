# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in smri_T2_mean_total_data.RDS smri_T1_mean_total_data.RDS Normalised_T2_.RDS Avg_T2_Gray_.RDS Avg_T2_White_.RDS Normalised_T1_.RDS Avg_T1_Gray_.RDS Avg_T1_White_.RDS Dest_Sulcal_Depth_.RDS Dest_Vol_.RDS
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J enet_tune_loaded_gfactor.${DATANAME} --array=1-${N_BATCHES} enet_tune_loaded_gfactor.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collate_enet_tune_loaded_gfactor.sh ${DATASET}
done