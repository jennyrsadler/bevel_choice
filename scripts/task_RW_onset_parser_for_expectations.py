#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 13 14:21:45 2018

@author: jennygilbert

This script is used to conver the log files from the probabilistic reward learning task in BeveL to csv files formatted for Rescorla Wagner modeling in hBayesDM r package.

- pyndl requires a single file for all participants with 4 columns: 'subjID", "type", "choice", "reward"    
Type: AB = option1; CD= option2; EF; option3  
Choice: A=1, B=0, C=1, D=4, E=5, F=6
    #1 if "left"; 0 if "right
Reward: sweet taste = 1; bitter taste = 0
***Needs headers

"""
import os
import glob
import pandas as pd

handles=[]

basepath='/Users/jennygilbert/Google Drive/NIBL/Projects/18-0417 BeveL/data/task_behavior/task_logs'
os.chdir(basepath)


ignore = ['DATA 	Keypress: o','Level post injecting via pump at address']

for file in glob.glob(os.path.join(basepath,'bevel_*.log')):
    print(file)

    sub=file.split('/')[10].split('_')[1].split('l')[1]
    run=file.split('/')[10].split('_')[2]
    print([sub,run])
    

    with open(file,'r') as infile:
        subjID=[]
        runID=[]
        pair=[]
        choice=[]
        outcome=[]
        matched=[]
        RT=[]
        start_time=None
        
        for x in infile.readlines():    
            if not x.find(ignore[0])>-1 or x.find(ignore[1])>-1:
                l_s=x.strip().split()
                
                
 #               if x.find('at time=')>-1:#find the response time
 #                   l_s=x.strip().split()
 #                   reaction_time=(l_s[5])
                if x.find('at time= ')>1:
                    l_s=x.strip().split()
                    RT.append(l_s[5])
                    
                if x.find('Level injecting via pump at address ')>-1:#find the tasty image
                    l_s=x.strip().split()
                    #print(l_s)
                    if l_s[7] == '1' and l_s[16] == 'a.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        pair.append('AB')
                        choice.append('A')
                        outcome.append('reward')
                        matched.append('matched')
                    if l_s[7] == '1' and l_s[16] == 'c.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('CD')
                        reward.append('C')
                        congruency.append('matched')
                    if l_s[7] == '1' and l_s[16] == 'e.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('EF')
                        choice.append('E')
                        reward.append('reward')
                        congruency.append('matched')
                    if l_s[7] == '1' and l_s[16] == 'b.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('AB')
                        choice.append('B')
                        reward.append('reward')
                        congruency.append('mismatched')
                    if l_s[7] == '1' and l_s[16] == 'd.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('CD')
                        choice.append('D')
                        reward.append('reward')
                        congruency.append('mismatched')
                    if l_s[7] == '1' and l_s[16] == 'f.jpg': 
                        subjID.append(sub)
                        type.append('EF')
                        runID.append(run)
                        choice.append('incorrect')
                        reward.append('reward')
                        congruency.append('mismatched')
                    if l_s[7] == '2' and l_s[16] == 'b.jpg':
                        subjID.append(sub)
                        type.append('12')
                        runID.append(run)
                        choice.append('incorrect')
                        reward.append('punish')
                        congruency.append('congruent')
                    if l_s[7] == '2' and l_s[16] == 'd.jpg':  
                        subjID.append(sub)
                        runID.append(run)
                        type.append('34')
                        choice.append('incorrect')
                        reward.append('punish')
                        congruency.append('congruent')
                    if l_s[7] == '2' and l_s[16] == 'f.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('56')
                        choice.append('incorrect')
                        reward.append('punish')
                        congruency.append('congruent')
                    if l_s[7] == '2' and l_s[16] == 'a.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('12')
                        choice.append('correct')
                        reward.append('punish')
                        congruency.append('incongruent')
                    if l_s[7] == '2' and l_s[16] == 'c.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('34')
                        choice.append('correct')
                        reward.append('punish')
                        congruency.append('incongruent')
                    if l_s[7] == '2' and l_s[16] == 'e.jpg':
                        subjID.append(sub)
                        runID.append(run)
                        type.append('56')
                        choice.append('correct')
                        reward.append('punish')
                        congruency.append('incongruent')
                        RT.append()

                if x.find('Key Press Missed!')>-1:
                    l_s=x.strip().split()
                    print(l_s)
                    subjID.append(sub)
                    runID.append(run)
                    pair.append('Miss')
                    choice.append('Miss')
                    outcome.append('Miss')
                    matched.append('Miss')
                

        #files2make=['task_log.csv']
        #mydict={}
        #try:
      #      for files in files2make:
    path='%s_%s.txt'%(sub,run)
    print(path)
    print(sub)
                #if os.path.exists(path) == True:
                    #print ('exists')
                    #break
     #           else:
      #              mydict[files] = path
           
    f_make=open(path, 'w')
    for a,b,c,d,e,f,g in zip(subjID,runID,pair,choice,outcome,congruency,RT):
        f_make.write(str(a)+'\t'+str(b)+'\t'+str(c)+'\t'+str(d)+'\t'+str(e)+'\t'+str(f)+'\t'+str(g)+'\n')
    f_make.close()
