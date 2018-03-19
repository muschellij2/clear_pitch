#############################################
# CLEAR IVH
#############################################
cd ~/CLEAR_PITCH; 

Rnosave process.R -N PROC \
	-l mem_free=8G,h_vmem=10G -t 1-23

Rnosave combine_df.R -N MODEL -t 5-8 \
	-l mem_free=72G,h_vmem=74G -hold_jid PROC

Rnosave run_predict.R -N PRED -t 1-4 \
	-l mem_free=50G,h_vmem=51G -hold_jid MODEL

Rnosave run_predict.R -N TEST_PRED -t 5-8 \
	-l mem_free=50G,h_vmem=51G -hold_jid PRED
