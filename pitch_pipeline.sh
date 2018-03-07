#############################################
# CLEAR IVH
#############################################
cd ~/CLEAR_PITCH; 

Rnosave process.R -N PROC \
	-l mem_free=8G,h_vmem=10G -t 1-23

Rnosave combine_df.R -N MODEL \
	-l mem_free=100G,h_vmem=101G -hold_jid PROC

Rnosave run_predict.R -N PRED \
	-l mem_free=40G,h_vmem=41G -hold_jid MODEL

