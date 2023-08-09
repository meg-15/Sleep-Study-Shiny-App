import numpy as np
from scipy.signal import find_peaks, butter, lfilter #find_peaks_cwt, general_gaussian, fftconvolve
from scipy import integrate, stats, interpolate
from pybaselines.polynomial import imodpoly
from itertools import groupby
import pandas as pd
import matplotlib.pyplot as plt

import tensorflow as tf
from tensorflow.keras.applications.inception_v3 import InceptionV3
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Dense, GlobalAveragePooling2D
from tensorflow.keras.layers import Input

def low_pass_filter(wavedata, rate, cutoff=1): # stim signal
    
    # Filter requirements.   
    nyq = 0.5 * float(rate)  # Nyquist Frequency
    order = 2       # sin wave can be approx represented as quadratic   
    normal_cutoff = cutoff / nyq
    # Get the filter coefficients 
    b, a = butter(order, normal_cutoff, btype='low', analog=False)
    y = lfilter(b, a, wavedata)
    return y

def down_sample(sig_stim_lfilt, sample_rate):
    
    f = interpolate.interp1d(x = np.arange(0, len(sig_stim_lfilt)), y = sig_stim_lfilt)
    xnew = np.arange(0, len(sig_stim_lfilt), sample_rate)
    return f(xnew)
  
def get_seq_len(c, thresh = 10000): # get the length of an inspiration effort (consecutive 1s)
    seqs = [(key, len(list(val))) for key, val in groupby(c)]
    seqs = [(key, sum(s[1] for s in seqs[:i]), len) for i, (key, len) in enumerate(seqs)]
    # seqs = [(key, start, length), ...]
    return [[s[1], s[1] + s[2] - 1] for s in seqs if s[0] == 1 and s[2] > thresh]

def analyze_section(wave, regular_imodpoly, sample_rate, minimun_breath_dur, rise_rate = 0.05, draw = False): # define an inspiration effort by upward movement of the rip belt signal
    
    b = wave - regular_imodpoly # diff
    c = (b >= np.std(b)*rise_rate) # bool arrray
    c = c.astype(int)     
    if draw:
        max_time = len(b)/sample_rate
        time_steps = np.linspace(0, max_time, len(b)) 
        plt.style.use('default')
        plt.plot(time_steps, wave, 'g')
        plt.plot(time_steps, regular_imodpoly, 'r')
        plt.show()
        # plt.plot(b)
        # plt.axhline(np.std(b)*0.05, c = 'r')
        plt.show()
    d = get_seq_len(c, thresh = int(0.4*sample_rate*minimun_breath_dur))
    return d

def visualize_inspiration(wave, start, end, sample_rate, minimun_breath_dur, rise_rate = 0.05, poly_order=3, plot_fig = False):

    a = wave[int(start): int(end)]
    regular_imodpoly = imodpoly(a, poly_order=poly_order, num_std=0.5)[0]
    d =  analyze_section(a, regular_imodpoly, sample_rate, minimun_breath_dur, rise_rate, draw = False)
            
    if plot_fig:
        
        max_time = len(a)/sample_rate
        time_steps = np.linspace(0, max_time, len(a)) 
     
        plt.style.use('default')
        
        plt.plot(time_steps, a, 'b')
        for i in range(len(d)):    
            time_range = list(range(d[i][0], d[i][1] + 1))
            time_steps_temp = np.linspace(d[i][0]/sample_rate, d[i][1]/sample_rate, len(time_range)) 
            plt.plot(time_steps_temp,  a[d[i][0]: d[i][1] + 1], 'g')
        plt.plot(time_steps, regular_imodpoly, 'r--')
        plt.show()
    
    for i in range(len(d)):
        for j in [0, 1]:
            d[i][j] += int(start)
    
    return d

def get_cycles(sig_rip_abd, sig_rip_chst, sig_epi, sig_flow, minimun_breath_dur, sample_rate, signal_type = 'both', poly_order=3, min_cycle = 5, rise_rate= 0.05):
    
    if signal_type == 'both':
        wave_temp = sig_rip_abd + sig_rip_chst
    elif signal_type == 'chest':
        wave_temp = sig_rip_chst
    elif signal_type == 'epi':
        wave_temp = sig_rip_abd + sig_rip_chst - sig_epi
    else:
        wave_temp = sig_rip_abd
        
    start_idx = 0
    window_size = int(minimun_breath_dur * min_cycle * sample_rate) # per 10 breath cycles
    d_list = []
    start_list, end_list = [], []
    while start_idx * 1/2 * window_size  < len(sig_flow):
        start_temp = int(start_idx * 1/2 * window_size)
        
        if start_temp + window_size <= len(sig_flow):
            end_temp = start_temp + window_size
        else:
            end_temp = len(sig_flow) - 1
        
        #print('Analyzing {}s - {}s of totally {}s'.format(int(start_temp/sample_rate), int(end_temp/sample_rate), int(len(sig_flow)/sample_rate)))
        d = visualize_inspiration(wave_temp, start_temp, end_temp, sample_rate, minimun_breath_dur, rise_rate = rise_rate, poly_order=poly_order, plot_fig = False)
        d_list.append(d)
        start_list.append(start_temp)
        end_list.append(end_temp)
        start_idx +=1
    
    df_breath = pd.DataFrame(list(zip(start_list,end_list, d_list)), columns=['Start', 'End', 'Positions'])
    
    # get start and end of each cycle
    start_p, end_p = [], []
    for i in range(len(df_breath)):
        for p in df_breath['Positions'][i]:
            start_p.append(p[0])
            end_p.append(p[1])
    
    df_p =  pd.DataFrame(list(zip(start_p, end_p)), columns=['Start', 'End'])
    df_p.sort_values(by = 'Start', inplace=True, ignore_index=True)

    start_p, end_p = [], []
    i = 0
    while i+1< len(df_p):
       
        a_u = range(min(df_p['Start'][i], df_p['Start'][i+1]), max(df_p['End'][i], df_p['End'][i+1]))
        a_i = range(max(df_p['Start'][i], df_p['Start'][i+1]), min(df_p['End'][i], df_p['End'][i+1]))
        
        # if bool(set(a_u) & set(a_i)):
            
        if len(a_i)/len(a_u) > 0:
            start_temp = min(df_p['Start'][i], df_p['Start'][i+1])
            end_temp = max(df_p['End'][i], df_p['End'][i+1])

            start_p.append(start_temp)
            end_p.append(end_temp)
            i += 2

        else:
            start_p.append(df_p['Start'][i])
            end_p.append(df_p['End'][i])
            i += 1
    if not df_p['Start'].iloc[-1] < end_p[-1]:
        start_p.append(df_p['Start'].iloc[-1])
        end_p.append(df_p['End'].iloc[-1])        
    df_p_filter = pd.DataFrame(list(zip(start_p, end_p)), columns=['Ins start', 'Ins end'])    
    
    return df_p_filter 

def separate_double_cycles(df_p_filter, sample_rate):
    
    mean_dur = df_p_filter['Dur'].mean()
    
    df_temp = df_p_filter.loc[df_p_filter['Dur'] >= 2 * mean_dur]
    
    start_p_list, end_p_list = [], []
    for i in range(len(df_temp)):
        start_p_list.append(df_temp.iloc[i]['Ins start'])
        end_p_list.append(df_temp.iloc[i]['Ins start'] + np.int64(mean_dur * sample_rate))
        start_p_list.append(df_temp.iloc[i]['Ins end'] - np.int64(mean_dur * sample_rate))
        end_p_list.append(df_temp.iloc[i]['Ins end'])
    
    df_filtered = pd.DataFrame(list(zip(start_p_list, end_p_list)), columns=['Ins start', 'Ins end'])  
    
    df_filtered['Dur'] = (df_filtered['Ins end'] - df_filtered['Ins start'])/sample_rate
    
    return df_filtered

def recheck_cycles(df_p_filter, sample_rate, minimun_rate = 0.7):
    
    #1st
    
    df_p_filter['Dur']= (df_p_filter['Ins end'] - df_p_filter['Ins start'])/sample_rate
    
    mean_dur = df_p_filter['Dur'].mean()
    
    df_filtered = separate_double_cycles(df_p_filter, sample_rate)
        
    #separate double cycles
    
    df_final = pd.concat([df_p_filter.loc[(df_p_filter['Dur'] < 2 * mean_dur) & (df_p_filter['Dur'] > minimun_rate * mean_dur)], df_filtered], axis=0, ignore_index=True)

    df_final['Ins start'] = df_final['Ins start'].astype('int64')
    df_final['Ins end'] = df_final['Ins end'].astype('int64')
    
    df_final = df_final.sort_values('Ins start', ignore_index=True)
    
    #2nd check
    mean_dur2 = df_final['Dur'].mean()
    for i in range(len(df_final)-1):
        if df_final.iloc[i]['Ins end'] > df_final.iloc[i+1]['Ins start'] :
            if int(mean_dur2 * sample_rate) + df_final.iloc[i]['Ins start'] < df_final.iloc[i+1]['Ins start']:
                df_final.at[i, 'Ins end'] = df_final.iloc[i]['Ins start'] + int(mean_dur2 * sample_rate)
            elif df_final.iloc[i+1]['Ins end'] - int(mean_dur2 * sample_rate) > df_final.iloc[i]['Ins end']:
                df_final.at[i+1, 'Ins start'] = df_final.iloc[i+1]['Ins end'] - int(mean_dur2 * sample_rate)
            else:
                df_final.at[i, 'Ins end'] = df_final.iloc[i+1]['Ins end']
                df_final.at[i+1, 'Ins start'] = df_final.iloc[i]['Ins start']
                               
    df_final['Dur']= (df_final['Ins end'] - df_final['Ins start'])/sample_rate
    
    return  df_final.drop_duplicates(ignore_index = True)

def get_baseline_portions(df_cycle, sig_flow):
    
    df = df_cycle.copy()
    
    df['Exp start'] = df['Ins end'] + 1
    
    df['Exp end'] = ''

    for i in range(len(df)-1):
        df.at[i, 'Exp end'] = int(df.iloc[i+1]['Ins start'] - 1)
    
    df.at[len(df)-1, 'Exp end'] = len(sig_flow)-1
    
    df['Exp end'] = df['Exp end'].astype(int)
    
    return df

def get_measurement(df_breaths, sig_flow, sig_rip_chst, sig_rip_abd, sig_stim_lfilt, sig_epi, sample_rate, t_start, sig_cpap):
    
    for i in range(1, len(df_breaths)):

        df_breaths.at[i, 'Exp.flow.mean.prev'] = df_breaths.iloc[i-1]['Exp.flow.mean']
        df_breaths.at[i, 'Exp.chst.mean.prev'] = df_breaths.iloc[i-1]['Exp.chst.mean']
        df_breaths.at[i, 'Exp.abd.mean.prev'] = df_breaths.iloc[i-1]['Exp.abd.mean']
        
    end = int(df_breaths['Ins start'][0] - 1)    
    df_breaths.at[0, 'Exp.flow mean.prev'] = np.mean(sig_flow[0:end])
    df_breaths.at[0, 'Exp.chst mean.prev'] = np.mean(sig_rip_chst[0:end])    
    df_breaths.at[0, 'Exp.abd mean.prev'] = np.mean(sig_rip_abd[0:end])
    
    stim_mean = []
    stim_max = []
    pepi_min_time = []
    flow_max_time = []
    flow_max = []
    flow_mean = []
    flow_std = []
    cpap_mean = []
    stim_flow_max_time = []
    pepi_flow_max_time = []
    stim_pepi_min_time = []
    flow_pepi_min_time = []
    flow_median = []
    
    for i in range(len(df_breaths)):
                    
        start_t = int(df_breaths['Ins start'][i])     
        end_t = int(df_breaths['Exp end'][i] + 1)
        
        stim_mean.append(np.nanmean(sig_stim_lfilt[start_t : end_t]))
        stim_max.append(np.amax(sig_stim_lfilt[start_t : end_t]))

        pepi_min_time_temp = start_t + np.argmin(sig_epi[start_t : end_t]) 
        pepi_min_time.append(pepi_min_time_temp/sample_rate + t_start)
        
        flow_max_time_temp = start_t + np.argmax(sig_flow[start_t : end_t])
        flow_max_time.append(flow_max_time_temp/sample_rate + t_start)
        flow_max.append(np.amax(sig_flow[start_t : end_t]))
        flow_mean.append(np.mean(sig_flow[start_t : end_t]))
        flow_std.append(np.std(sig_flow[start_t : end_t]))
        
        cpap_mean.append(np.mean(sig_cpap[start_t : end_t]))
        
        stim_flow_max_time.append(sig_stim_lfilt[flow_max_time_temp])
        pepi_flow_max_time.append(sig_epi[flow_max_time_temp])
        stim_pepi_min_time.append(sig_stim_lfilt[pepi_min_time_temp])
        flow_pepi_min_time.append(sig_flow[pepi_min_time_temp])
        
        flow_median.append(np.median(sig_flow[start_t : end_t]))
        
    df_measure = pd.DataFrame(list(zip(stim_mean, stim_max, pepi_min_time, flow_max_time, flow_max, flow_mean, flow_std, cpap_mean, 
                               stim_flow_max_time, pepi_flow_max_time, stim_pepi_min_time, flow_pepi_min_time, flow_median)),
              columns=['Stim.mean','Stim.max', 'Pepi.min.time', 'Flow.max.time', 'Flow.max.unadj', 'Flow.mean', 'Flow.std', 'CPAP.mean',
                       'Stim.at.max.flow', 'Pepi.at.max.flow', 'Stim.at.min.pepi', 'Flow.at.min.pepi', 'Flow.median'])
        
    return pd.concat([df_breaths, df_measure], axis=1)

def stim_mode(row):
    if row['Stim.at.max.flow'] >= 0.002:
        val = 'Combined'
    elif row['Stim.at.max.flow'] >= 0.001: 
        val = 'ACS'
    else:
        val = 'No stim' 
    return val
  
def airway_status(row):
    if row['Time diff'] >= -1000 and row['Time diff'] <= -100 and row['Flow.std'] > 1:
        val = 'FL'
    elif abs(row['Flow.max.unadj'] - row['Flow.mean']) <= 1.5 and row['Flow.std'] < 1:
        val = 'Apnea'
        
    elif row['Time diff'] > 0 and row['Flow.std'] > 1:
        val = 'NFL'
    else:
        val = 'Other'
    return val
  
def data_analysis(df):
    
    z = np.polyfit(df['CPAP.mean'], df['Flow.median'], 1)
    p = np.poly1d(z) 
        
    df['Time diff'] = (df['Flow.max.time'] - df['Pepi.min.time']) * 1000
    
    df['Stim.mode'] = df.apply(stim_mode, axis=1)
    df['Stim.mode.review'] = ''
    df['Stim.mode.final'] = ''

    df['Airway.status'] = df.apply(airway_status, axis=1)
    df['Airway.status.review'] = ''
    df['Airway.status.final'] = ''
    
    df['Fit.flow.by.CPAP'] = p(df['CPAP.mean'])
    
    df['Flow.max.adj'] = df['Flow.max.unadj'] -  df['Fit.flow.by.CPAP']
  
    df['Flow.max.adj1'] = df['Flow.max.unadj'] - 0.5 * df['Exp.flow.mean'] - 0.5 * df['Exp.flow.mean.prev']
    
    df['CPAP.ceil'] = np.ceil(df['CPAP.mean'])

    return df
  
def normalized_plot(breath_sig):
    my_dpi = 96
    f = plt.figure(figsize=(299/my_dpi, 299/my_dpi), dpi=my_dpi)
    ax = f.add_subplot(111)
    ax.plot(breath_sig, c = 'k')   
    ax.axis('off')    
    f.tight_layout(pad=0)
    # To remove the huge white borders
    # ax.margins(0)      
    f.canvas.draw( )
    # Get the RGBA buffer from the figure
    w,h = f.canvas.get_width_height()
    buf = np.frombuffer(f.canvas.tostring_rgb(), dtype=np.uint8)
    buf.shape = (w, h, 3)
 
    # # canvas.tostring_argb give pixmap in ARGB mode. Roll the ALPHA channel to have it in RGBA mode
    # buf = np.roll(buf, 3, axis = 2 )
    plt.close()
    return buf/255.  
  
def build_model_inception(input_shape, num_classes):
    tf.keras.backend.clear_session()
    
    input_shape = tuple(int(num) for num in input_shape)

    input_tensor = Input(shape = input_shape)
    base_model = InceptionV3(input_tensor=input_tensor, weights='imagenet', include_top=False)
   
    x = base_model.output
    x = GlobalAveragePooling2D()(x)
   
    x = Dense(1024, activation='relu')(x)
    predictions = Dense(num_classes, activation='softmax')(x)
    
    model = Model(inputs=base_model.input, outputs=predictions)
    
    for layer in base_model.layers:
        layer.trainable = False
    
    optimizer = tf.keras.optimizers.Adam(learning_rate=1e-4)

    model.compile(optimizer = optimizer, #'rmsprop', 
                  loss='categorical_crossentropy',
                  metrics=["accuracy"])
    
    return model
  
def detect_airway_status_dl(sig_flow, df, model):
    
    cycle_div = df['Ins start'].tolist()
    arr_list = []
    for i in range(len(cycle_div)):    
        try:
            arr_list.append(normalized_plot(sig_flow[int(cycle_div[i]): int(cycle_div[i+1])]))
        except:
            arr_list.append(normalized_plot(sig_flow[int(cycle_div[i]): ]))

    img_array = np.stack(arr_list, axis=0, out=None)
    predictions = model.predict(img_array)
    df['Airway_DL'] = np.argmax(predictions, axis = 1)
    df['Airway.status.DL'] = df['Airway_DL'].apply(lambda x: 'NFL' if x == 2 else 'FL' if x == 1 else 'Apnea')  
    return df
  

