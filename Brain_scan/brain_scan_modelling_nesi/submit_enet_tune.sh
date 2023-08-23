# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in DTI_data.RDS smri_T2_mean_total_data.RDS smri_T1_mean_total_data.RDS Normalised_T2_.RDS Avg_T2_Gray_.RDS Avg_T2_White_.RDS Normalised_T1_.RDS Avg_T1_Gray_.RDS Avg_T1_White_.RDS Dest_Sulcal_Depth_.RDS Dest_Vol_.RDS Dest_Area_.RDS Dest_Thick_.RDS Vol_ASEG_.RDS Avg_T2_ASEG_.RDS Avg_T1_ASEG_Vol_.RDS rsmri_gordon_aseg_data.RDS rsmri_within_avg_data.RDS incorrectgovsincorrectstop_ROI_.RDS incorrectgovscorrectgo_ROI_.RDS correctstopvsincorrectstop_ROI_.RDS anystopvscorrectgo_ROI_.RDS incorrectstopvscorrectgo_ROI_.RDS correctstopvscorrectgo_ROI_.RDS correctgovsfixation_ROI_.RDS antiLargeLossVsSmallLoss_ROI_.RDS antiSmallLossVsNeu_ROI_.RDS antiLargeLossVsNeu_ROI_.RDS antiLargeRewVsSmallRew_ROI_.RDS antiSmallRewVsNeu_ROI_.RDS antiLargeRewVsNeu_ROI_.RDS feedPunPosVsNeg_ROI_.RDS feedRewPosVsNeg_ROI_.RDS antiLosVsNeu_ROI_.RDS antiRewVsNeu_ROI_.RDS posfacevsneutface_ROI_.RDS negfacevsneutface_ROI_.RDS facevsplace_ROI_.RDS emotionvsneutface_ROI_.RDS X2backvs0back_ROI_.RDS emotion_ROI_.RDS place_ROI_.RDS X2back_ROI_.RDS X0back_ROI_.RDS
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J enet_tune.${DATANAME} --array=1-${N_BATCHES} enet_tune.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"
  sbatch -d afterok:${JOBID} collate_enet_tune.sh ${DATASET}
done