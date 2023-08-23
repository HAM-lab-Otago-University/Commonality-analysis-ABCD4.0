# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in Dest_Area_.RDS Dest_Thick_.RDS Vol_ASEG_.RDS Avg_T2_ASEG_.RDS Avg_T1_ASEG_Vol_.RDS incorrectgovsincorrectstop_ROI_.RDS incorrectgovscorrectgo_ROI_.RDS correctstopvsincorrectstop_ROI_.RDS anystopvscorrectgo_ROI_.RDS incorrectstopvscorrectgo_ROI_.RDS 
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J enet_tune.${DATANAME} --array=1-${N_BATCHES} enet_tune_rsmri.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collate_enet_tune_rsmri.sh ${DATASET}
done