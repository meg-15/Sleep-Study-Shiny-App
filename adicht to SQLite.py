#Packages-----------------------------------------------------------------------
import adi
  #adi file reader
import sqlite3
  #to create SQLite database
  
#Functions Bank-----------------------------------------------------------------

iter_len = lambda a: range(len(a))

#Loading in Files---------------------------------------------------------------

files = ##Enter file locations; should have form similar too: 'MH 14-06.adicht'##

iter_fil = iter_len(files)
t = [adi.read_file(files[f]) for f in iter_fil]

#Extracting Participant Characteristics-----------------------------------------
def get_info(f):
  return files[f].split('/')[-1].replace('.adicht', '').split(' ')

pt_char = [get_info(f) for f in iter_fil]
  #2 columns: SubjectID (Initials), DateOfStudy

#Extracting Channel Metadata----------------------------------------------------

iter_chan = iter_len(t[0].channels)

def gen_chan_id():
  ChanID = [[str(list(iter_chan)[c] + 1) for c in iter_chan]]*len(files)
  return [[ChanID[f][c] + pt_char[f][0] for c in iter_chan] for f in iter_fil]

ChannelID = gen_chan_id()
  #Channel number + SubjectID

record_id = [t[f].records[0].id for f in iter_fil] 

sample_rate = [[t[f].channels[c].fs[record_id[f]-1] for c in iter_chan] for f in iter_fil]
units = [[t[f].channels[c].units[record_id[f]-1] for c in iter_chan] for f in iter_fil]

def get_names():
  names = [[t[f].channels[c].name for c in iter_chan] for f in iter_fil]
  return [[names[f][c].replace(' ', '_') for c in iter_chan] for f in iter_fil]

names = get_names()

meta_data = sum([[[ChannelID[f][c], sample_rate[f][c], units[f][c], names[f][c]] for c in iter_chan] for f in iter_fil], [])
  #4 columns: ChannelID + SampeRate + Units + ChannelName

#Extracting Channel Data--------------------------------------------------------

t_dat = [[t[f].channels[c].get_data(1) for c in iter_chan] for f in iter_fil]

def reformat(f, c, s):
  return float(t_dat[f][c][s])

n_samples = [[t[f].channels[c].n_samples[record_id[f]-1] for c in iter_chan] for f in iter_fil]
#dt = [[t[f].channels[c].dt[record_id[f]-1] for c in iter_chan] for f in iter_fil]

def get_time(f, c, s): #time is in seconds...could convert...but there is a lot of overlap in times...probably convert in R Shiny app
  return s/sample_rate[f][c]

def format_data():
  return sum([sum([[[ChannelID[f][c], get_time(f, c, s), reformat(f, c, s)] for s in range(n_samples[f][c])] for c in iter_chan], []) for f in iter_fil], [])
  #Function called below to fill channeldata table (speeds up process due to large amount of data)
  #3 columns: ChannelID, Time, Value

#Annotations--------------------------------------------------------------------

comments = [t[f].records[0].comments for f in iter_fil]
n_annot = [len(comments[f]) for f in iter_fil]
chans = [[str(comments[f][a].channel_) for a in range(n_annot[f])] for f in iter_fil]
ChannelID3 = sum([[chans[f][a] + pt_char[f][0] for a in range(n_annot[f])] for f in iter_fil], [])
AnnotID = sum([[comments[f][a].id for a in range(n_annot[f])] for f in iter_fil], [])
row_id = [ChannelID3[a] + str(AnnotID[a]) for a in range(len(ChannelID3))]
Time = sum([[comments[f][a].time for a in range(n_annot[f])] for f in iter_fil], [])
Annot = sum([[comments[f][a].text for a in range(n_annot[f])] for f in iter_fil], [])

annotations = [[row_id[a], ChannelID3[a], AnnotID[a], Time[a], Annot[a]] for a in range(sum(n_annot))]
  #5 columns: row_id, ChannelID, AnnotationID, Time, Annotation

#Inputing Data into SQLite------------------------------------------------------

db = sqlite3.connect(##name of database##) 
c = db.cursor()

#Patient Characteristics
c.execute("CREATE TABLE IF NOT EXISTS pt_char ('SubjectID' TEXT, 'DateofStudy' TEXT)")
c.executemany("INSERT INTO pt_char VALUES(?, ?)", pt_char)

#Channel Meta Data
c.execute("CREATE TABLE IF NOT EXISTS channelmeta ('ChannelID' TEXT, 'SampleRate' FLOAT, 'Units' TEXT, 'ChannelName' TEXT)")
c.executemany("INSERT INTO channelmeta VALUES(?, ?, ?, ?)", meta_data) 

#Channel Data
c.execute("CREATE TABLE IF NOT EXISTS channeldata ('ChannelID' TEXT, 'Time' FLOAT, 'Value' FLOAT)")
c.executemany("INSERT INTO channeldata VALUES(?, ?, ?)", format_data()) 

#Annotations
c.execute("CREATE TABLE IF NOT EXISTS annots ('row_id' TEXT, 'ChannelID' TEXT, 'AnnotationID' INT, 'Time' FLOAT, 'Annotation' TEXT)")
c.executemany("INSERT INTO annots VALUES(?, ?, ?, ?, ?)", annotations) 

db.commit()
db.close()


