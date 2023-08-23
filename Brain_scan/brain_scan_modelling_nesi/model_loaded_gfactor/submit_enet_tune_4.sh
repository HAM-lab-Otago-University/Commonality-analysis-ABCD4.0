# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in antiLosVsNeu_ROI_.RDS antiRewVsNeu_ROI_.RDS posfacevsneutface_ROI_.RDS negfacevsneutface_ROI_.RDS facevsplace_ROI_.RDS emotionvsneutface_ROI_.RDS X2backvs0back_ROI_.RDS emotion_ROI_.RDS place_ROI_.RDS X2back_ROI_.RDS X0back_ROI_.RDS
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J enet_tune.${DATANAME} --array=1-${N_BATCHES} enet_tune_rsmri.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collate_enet_tune_rsmri.sh ${DATASET}
done