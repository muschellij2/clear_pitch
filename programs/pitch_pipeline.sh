#############################################
# CLEAR IVH
#############################################
cd ${dex}/CLEAR_PITCH/programs;

Rnosave make_df_first_batch.R -N DF \
	-l mem_free=1G,h_vmem=2G 

# n_ids=23
# n_ids=
Rnosave process.R -N PROC \
	-l mem_free=22G,h_vmem=23G -t 76-84

# Rnosave make_df_test_set.R -N TESTDF \
# 	-l mem_free=1G,h_vmem=2G 

# Rnosave process_test_set.R -N TESTPROC \
# 	-l mem_free=10G,h_vmem=12G -t 1-282

Rnosave fit_model.R -N MODEL -t 5-8 \
	-l mem_free=140G,h_vmem=141G -hold_jid PROC

Rnosave run_predict.R -N PRED -t 13 \
	-l mem_free=60G,h_vmem=61G -hold_jid MODEL \
	-hold_jid PROC

# Rnosave run_predict.R -N TEST_PRED -t 5-8 \
# 	-l mem_free=50G,h_vmem=51G -hold_jid PRED

# dice tasks 25-36 are when validation set 
# in there
Rnosave compute_dice.R -N DICE -t 40-78 \
	-hold_jid_ad PRED

Rnosave plot_results.R -N PLOTTER -t 40-78 \
	-hold_jid_ad DICE


Rnosave dice_results.R -N DPLOT
