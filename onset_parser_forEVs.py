#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on 

@author: jennysadler

This script is used to convert the log files from the probabilistic reward learning task in BeveL to txt files formatted for betaseries analysis via FSL FEAT level1.
Following Mumford et al 2012, each trial will be modeled in a unique GLM where the trial of interest is 1 EV, and all other trials are included as a nusiance EV

"""
import os
import glob
import numpy as numpy

handles=[]

basepath='/Users/jennygilbert/Documents/betaseries_bevel/task_logs'
os.chdir(basepath)

ignore = ['DATA 	Keypress: o','Level post injecting via pump at address']

#get the global info about the run. 
for file in glob.glob(os.path.join(basepath,'bevel*.log')):
    print(file)

    sub=file.split('/')[6].split('_')[1]
    run=file.split('/')[6].split('_')[2]
    print([sub,run])
    
#   open the script and read in log data
    with open(file,'r') as infile:
        trials=[]
        rinse=[]
        start_time=None
        
        for x in infile.readlines():
#            if x.find('Keypress: q'):
#                continue
            
            if not x.find(ignore[0])>-1 or x.find(ignore[1])>-1:
                
                l_s=x.strip().split()
#                print l_s
                
                if x.find('Level start key press')>-1:#find the start
                    l_s=x.strip().split()
                    start_time=float(l_s[0])
                    
                if x.find('Level injecting via pump at address ')>-1:#find the tasty image
                    l_s=x.strip().split()
#                    print(l_s)
                    trials.append(l_s[0])
#                    print(trials)
#                   rinse
#                    if x.find('Level RINSE 	25')>-1:
#                        rinse.append(l_s[0])
                
        trials1=(numpy.asarray(trials,dtype=float))-start_time 
#        rinse=(numpy.asarray(rinse,dtype=float))-start_time 
        print(trials1)
#        print(rinse)
        
        #files2make=['trials','rinse']
        #mydict={}
 
    path='%s_%s_.txt'%(sub,run)
    print(path)
    print(sub)
    
    f_make=open(path, 'w')
    for a in trials1:
        f_make.write(str(a)+'\t'+'5'+'\t'+'1'+'\n')
    f_make.close()
               
#        f_neu=open(mydict['rinse'], 'w')
#        for t in range(len(rinse)):
#            f_neu.write('%f\t3\t1\n'%(rinse[t]))
#        f_neu.close()
            
#    f_trial=open(mydict['trials'], 'w')
#    for t in range(len(trials)):
#        f_trial.write('%f\t5\t1\n' %(trials[t]))
#    f_trial.close()