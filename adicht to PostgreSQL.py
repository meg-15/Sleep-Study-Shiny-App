#Packages-----------------------------------------------------------------------
import adi
  #adi file reader
import sqlite3
  #to create SQLite database
import random
import pandas as pd
import numpy as np
  
#Functions Bank-----------------------------------------------------------------

iter_len = lambda a: range(len(a))

#Loading in Files---------------------------------------------------------------

path = 'C:/' #insert file download path here

#CHANGE - This is the file you downloaded from AWS. Change file path and name accordingly.
files = ['C:/'] #insert downloaded AWS file path here

iter_fil = iter_len(files)
t = [adi.read_file(files[f]) for f in iter_fil]

#CHANGE -  Change the values below accordingly. 
record_id = 5 #check 'ACS summary' for section #
subjectID = [#] #check 'Notes on AWS files' for preset SubjectIDs
age = [#] #check 'ACS per breath 2024-01-04' for age and bmi for each patient
bmi = [#]

#Channel info
iter_chan = iter_len(t[0].channels)
sample_rate = [t[0].channels[c].fs[record_id-1] for c in iter_chan]

#Extracting Channel Metadata----------------------------------------------------
  #I have already run this and uploaded the file once...don't upload again!
def gen_chan_id():
  ChanID = [str(list(iter_chan)[c] + 1) for c in iter_chan]
  return ChanID

ChannelID = gen_chan_id()

record_id = [t[0].records[0].id] 

sample_rate = [t[0].channels[c].fs[record_id-1] for c in iter_chan]
units = [t[0].channels[c].units[record_id-1] for c in iter_chan]

def get_names():
  names = [t[0].channels[c].name for c in iter_chan]
  return [names[c].replace(' ', '_') for c in iter_chan]

names = get_names()

meta_data = [[int(ChannelID[c]), sample_rate[c], units[c], names[c]] for c in iter_chan]

meta_df = pd.DataFrame(meta_data)
meta_csv = meta_df.to_csv(path+'meta_data.csv', index = False, header = False)

#Extracting Participant Characteristics-----------------------------------------
  #run this section for each file
  
  #get_info = pulls initials and date from file name
  #f = file #

def get_info(f):
  return files[f].split('/')[-1].replace('.adicht', '').split('-')

initials = [get_info(f)[0] for f in iter_fil]
year = [get_info(f)[1] for f in iter_fil]
month = [get_info(f)[2] for f in iter_fil]

pt_char = sum([[[subjectID[f], initials[f], int(year[f]), int(month[f]), age[f], bmi[f]] for f in iter_fil]], [])

#Download pt_char.csv with characteristics of patient
pt_char_df = pd.DataFrame(pt_char)
pt_char_csv = pt_char_df.to_csv(path+'pt_char.csv', index = False, header = False)

#Extracting Channel Data--------------------------------------------------------
  #run this section for each file
  
  #ChanID_data = number ids for each channel
  #t_dat  = values of data for each channel per file
  #f, c, s = file #, channel #, and sample #
  #n_samples = number of sample for each channel per file
  #reformat(f,c,s) = changes format of individual value from numpy.float32 to float
  #get_time = gets time for value by dividing each sample # by sample_rate for that channel
  #format_data = function for setting up table so that data is never actually stored within R (data takes up a lot of space)
  #subjectID_Data = subject ID for patient
  #ID = number id (1-total # of data points) for each data point

ChanID_data = [[str(list(iter_chan)[c] + 1) for c in iter_chan]]

t_dat = [[t[f].channels[c].get_data(record_id) for c in iter_chan] for f in iter_fil]

def reformat(f, c, s):
  return float(t_dat[f][c][s])

n_samples = [[t[f].channels[c].n_samples[record_id-1] for c in iter_chan] for f in iter_fil]

def get_time(c, s): #time is in seconds...could convert...but there is a lot of overlap in times...probably convert in R Shiny app
  return s/sample_rate[c]

subjectID_data = [[subjectID[f]] * len(iter_chan) for f in iter_fil]

#If file is smaller, the code below will work to generate data ID for all data points.
total_samps_f = [sum(n_samples[f]) for f in iter_fil]
total_samps = sum(total_samps_f)
ID_test = [n for n in range(total_samps)]
myNum = tuple(n_samples[0])
prev = 0
ID = []
for i in myNum:
    ID.append(tuple(ID_test[prev:prev+1]))
    prev = prev+i 
ID = [ID]

#If you get a memory error when running the code above, the file is too large. 
# You will have to manually generate the IDs separately for each channel.
chan = 1 #change to channel number you are working on
chan_samp = [n_samples[0][c] for c in range(chan-1)]
chan_start = sum(chan_samp)+1
chan_end = chan_start + n_samples[0][chan-1]

ID_test = [n for n in range(chan_start, chan_end)]
ID = []
prev = 0
ID.append(tuple(ID_test[prev:prev+n_samples[0][chan-1]]))
ID = [ID]
ID_test = 0


#formatting data
def format_data2(c):
  return [[ID[0][c][s], ChanID_data[0][c], subjectID_data[0][c], get_time(c, s), reformat(0, c, s)] for s in range(n_samples[0][c])]
          #If using the second method to generate IDs, change ID[0][c][s] to ID[0][0][s]
          
dat0 = format_data2(0)
data_df = pd.DataFrame(dat0).to_csv(path+'data0.csv', index = False, header = False)
dat1 = format_data2(1)
data_df = pd.DataFrame(dat1).to_csv(path+'data1.csv', index = False, header = False)
dat2 = format_data2(2)
data_df = pd.DataFrame(dat2).to_csv(path+'data2.csv', index = False, header = False)
dat3 = format_data2(3)
data_df = pd.DataFrame(dat3).to_csv(path+'data3.csv', index = False, header = False)
dat4 = format_data2(4)
data_df = pd.DataFrame(dat4).to_csv(path+'data4.csv', index = False, header = False)
dat5 = format_data2(5)
data_df = pd.DataFrame(dat5).to_csv(path+'data5.csv', index = False, header = False)
dat6 = format_data2(6)
data_df = pd.DataFrame(dat6).to_csv(path+'data6.csv', index = False, header = False)
dat7 = format_data2(7)
data_df = pd.DataFrame(dat7).to_csv(path+'data7.csv', index = False, header = False)

#if files are large, run and download each file individually. Delete the files after downloading
dat7 = 0
data_df = 0

#Annotations--------------------------------------------------------------------
  #run this section for each file
  
  #comments = list of data for annotations per file
  #n_annot = total number of annots per file (for iterations)
  #channel for each annotation per file
  #annotID = list of annotation numbers per file
  #time = time for each annotation per file
  #annot = each annotation per file
  #subjectID_annot = repeated subjectID for every annot per file
  #annotations = data for annotations table

comments = [t[f].records[record_id-1].comments for f in iter_fil] 
n_annot = [len(comments[f]) for f in iter_fil]
chans = sum([[str(comments[f][a].channel_) for a in range(n_annot[f])] for f in iter_fil], [])
annotID = sum([[iter_len(comments[f])[a] + 1 for a in range(n_annot[f])] for f in iter_fil], [])
time = sum([[comments[f][a].time for a in range(n_annot[f])] for f in iter_fil], [])
time2 = list(np.around(np.array(time),4))
annot = sum([[comments[f][a].text for a in range(n_annot[f])] for f in iter_fil], [])
subjectID_annot = sum([[subjectID[f]] * n_annot[f] for f in iter_fil], [])

iter_an = iter_len(annot)
annotations = [[annotID[a], subjectID_annot[a], int(chans[a]), time2[a], annot[a]] for a in iter_an]

annot_df = pd.DataFrame(annotations)
annot_csv = annot_df.to_csv(path+'annotations2.csv', index = False, header = False)


