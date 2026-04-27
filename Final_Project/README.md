# Joshua's Final Project Pipeline & Instructions

Starting input Files: 
1. List_of_4854_insects&10_outgroups.xlsx
2. insectgs.csv
3. 10outgroups_4854insects_824genes_MLbest_rooted.tree

Step 1: Finding direct matches between our two data files. 
Run Part_1_TipSearch.R
Output: Matched_Tips.csv

Step 2: Prune the Maximum Likelihood Tree
Run Part_2_CleanTree.R
Output: ML_pruned.tree

Step 3: Setting the BAAM Parameters
Run Part_3_SettingBAAMPriors
Output: insect_gs_bamm.txt
