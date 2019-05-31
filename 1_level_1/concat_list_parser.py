#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on 

@author: jennysadler

This script is used to make the fsl merge command for beta series analysis.

"""
import os
import glob
import numpy as numpy

handles=[]

basepath='/Users/jennygilbert/Documents/betaseries_bevel/level_1/task_logs'
os.chdir(basepath)

ignore = ['DATA 	Keypress: o','Level post injecting via pump at address']


#get the global info about the run. 
for file in glob.glob(os.path.join(basepath,'bevel*.log')):
    print(file)

    sub=file.split('/')[7].split('_')[1]
    run=file.split('/')[7].split('_')[2]
    print([sub,run])
    
#   open the script and read in log data
    with open(file,'r') as infile:
        trials=[]
        reinforcer=[]
        start_time=None
        count=0

        for x in infile.readlines():    
            if not x.find(ignore[0])>-1 or x.find(ignore[1])>-1:
                    
                if x.find('Level injecting via pump at address ')>-1:
                    count = count + 1
                    trials.append(count)

                if x.find('Level injecting via pump at address 1')>-1:
                    reinforcer.append('reward')
                    print(reinforcer)
                
                if x.find('Level injecting via pump at address 2')>-1:
                    reinforcer.append('punishment')
                    print(reinforcer)       
        

    path='/Users/jennygilbert/Documents/betaseries_bevel/concatenate/%s_%s.txt'%(sub,run)
    print(path)
    print(sub)
    
    f_make=open(path, 'w')
    for a,b in zip(trials,reinforcer):
        f_make.write(str(a)+'\t'+str(b)+'\n')
    f_make.close()