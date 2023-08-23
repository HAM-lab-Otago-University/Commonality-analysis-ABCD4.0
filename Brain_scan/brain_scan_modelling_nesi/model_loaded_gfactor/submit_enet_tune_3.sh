# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in correctstopvscorrectgo_ROI_.RDS correctgovsfixation_ROI_.RDS antiLargeLossVsSmallLoss_ROI_.RDS antiSmallLossVsNeu_ROI_.RDS antiLargeLossVsNeu_ROI_.RDS antiLargeRewVsSmallRew_ROI_.RDS antiSmallRewVsNeu_ROI_.RDS antiLargeRewVsNeu_ROI_.RDS feedPunPosVsNeg_ROI_.RDS feedRewPosVsNeg_ROI_.RDS 
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J enet_tune.${DATANAME} --array=1-${N_BATCHES} enet_tune_rsmri.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collate_enet_tune_rsmri.sh ${DATASET}
done