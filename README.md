# betaseries_bevel

Analysis Pipeline for Betaseries Connectivity Analysis for the Beverage Learning Study

Pipeline
1. Level 1
- Run level_1/onset_parser_forEVs.py to get onset files by run (output: onsets folder)
- Run level_1/reformat_EVs.R to format onsets for betaseries analysis (output: onsets_reformatted) 
- Fill onsets into level 1 .fsf files & run on computing cluster (script: level_1/make_fsf_file.py)

2. Concatenate PE.nii files 
- Run concatenate/make_fsl_merge_by_run.ipynb to make fsl merge commands to combine all the PE files of the same condition for participants

-- Warp ROIs into MNI_Asymmetrical Space --- 
*This can be done at any point, I did it here. 
- On computing cluster, warp MNILin152 template into MNIAsym152 BOLD space
- Use the naming.xlsx template to make flirt commands
- Apply the flirt warp to all the ROIs from the chocolate decoding analysis (warp_rois.job)

(scripts in the /scripts/betaseries/job_files folder)

3. Pull Timeseries
- Run the make_commands_timeseries.ipynb script to make the fslmeants commands
- Copy command into .sh file and run

4. Combine Timeseries into Matrix
- Run the combine_timeseries.ipynb script to combine timeseries into one file per condition per participant
