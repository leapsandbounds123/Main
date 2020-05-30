#Install the netCDF4 library first.
from netCDF4 import Dataset
import pandas as pd
from datetime import timedelta, date
import math

# Open the dataset:

# data = Dataset(fname) #'fname' is the path of the original datasets.



'''
The function is to make a table with latitude, longitude, date and the dust value for 2 years of the 50km data.
The data should be the 50km data.
For dust, name is 'DUEXTTAU'.

An example of calling this function:
fname='E:\\files\\biostatistics\\thesis\\data\\MERRA2_aerosol_variables_over_MiddleEast_daily_20000516-20180515.nc'
data50 = Dataset(fname)
table_50 = make_table_50(data50, 'DUEXTTAU', to_csv = False)
'''
def make_table_50(data, name, to_csv = False): 
    
    lat = data.variables['lat'][:]
    lon = data.variables['lon'][:]
    values_with_time = data.variables[name][:,:]
    
    strat_date = date(2005, 5, 16)
    
    final = pd.DataFrame()

    for n in range(1825, 2555): #The day 1825 is the date(2005, 5, 16).
        values = pd.DataFrame(values_with_time.data[n,:,:]).set_index(pd.Series(lat))
        values.columns = pd.Series(lon)
        values = values.stack().reset_index()
        values.columns = ['latitude', 'longitude', name]
        values['time'] = strat_date + timedelta(n-1825)
        final = final.append(values, ignore_index=True)
            
    if to_csv:
        final.to_csv(name + '_' + '50km' + '.csv',index=False)
    return final


'''
The function is to make a table with latitude, longitude, date and the dust value for N days of the 7km data matched with same-day 50km data.
The data should be the 7km data.
For dust, name is 'DUEXTTAU'.
The start_date is the first day of the period you want to match. It should be the Python datetime object. It can be date(2005, 5, 16), the start date of the dataset. It can also be other date.
N is the number of days you want to match.
table_50 is the table you made using the make_table_50 function. It should be pandas dataframe.
out_csv is the path of the output csv file.

An example of calling this function:
fname='E:\\files\\biostatistics\\thesis\\data\\G5NR_aerosol_variables_over_MiddleEast_daily_20050516-20060515.nc'
data7 = Dataset(fname)
out_csv = 'E:\\files\\biostatistics\\thesis\\data\\G5NR+MERR2_dust_over_MiddleEast_daily_20050516-20060515.csv'
make_table_7(data7, 'DUEXTTAU', date(2005, 5, 16), 365, table_50, out_csv)
'''
def make_table_7(data, name, start_date, N, table_50, out_csv):
    lat = data.variables['lat'][:]
    lon = data.variables['lon'][:]
    values_with_time = data.variables[name][:,:]
    
    def round_to_half (x):
        return round(x*2)/2
    
    def round_to_0625 (x):
        x = round(x*1.6)/1.6
        if x < 26.25:
            x = 26.25
        elif x > 74.375:
            x = 74.375
        return x
        
    first_date = date(2005, 5, 16)
    days_past = (start_date - first_date).days
    final = pd.DataFrame()
    
    for n in range(0, N):
        current_time = start_date + timedelta(n)
        current_days = days_past + n
        values = pd.DataFrame(values_with_time.data[current_days,:,:]).set_index(pd.Series(lat))
        values.columns = pd.Series(lon)
        values = values.stack().reset_index()
        values.columns = ['latitude', 'longitude', name]
        
        values['lat_round'] = values['latitude'].apply(round_to_half)
        values['lon_round'] = values['longitude'].apply(round_to_0625)
        current_table_50 = table_50.loc[table_50['time'] == current_time]
        merged_values = pd.merge(values, current_table_50,  how='left', 
                     left_on=['lat_round','lon_round'], right_on = ['latitude','longitude'])
        merged_values['date'] = pd.to_datetime(merged_values['time'])
        merged_values.drop(columns=['lat_round', 'lon_round', 'latitude_y', 'longitude_y', 'time'], inplace = True)
        merged_values.rename(columns={'latitude_x': 'latitude', 'longitude_x': 'longitude',
                                         'DUEXTTAU_x': 'DUEXTTAU_7', 'DUEXTTAU_y': 'DUEXTTAU_50'}, inplace = True)
        
        if n == 0:
            merged_values.to_csv(out_csv,index=False)
        else:
            merged_values.to_csv(out_csv,index=False, mode='a',header=False)
            
        if n%10 == 0:
            print('day ' + str(n) + ' finished')
        
    print('all' + str(N) + 'days finished')


fname='//Users//dulichen//PycharmProjects//cisi567//venv//lib//PA_copy//thesis//MERRA2_aerosol_variables_over_MiddleEast_daily_20000516-20180515_copy.nc'
data50 = Dataset(fname)
table_50 = make_table_50(data50, 'DUEXTTAU', to_csv = False)
#
# fname1='//Users//dulichen//PycharmProjects//cisi567//venv//lib//PA_copy//thesis//G5NR_aerosol_variables_over_MiddleEast_daily_20050516-20060515_copy.nc'
# data7 = Dataset(fname1)
# out_csv = '//Users//dulichen//PycharmProjects//cisi567//venv//lib//PA_copy//thesis//G5NR+MERR2_dust_over_MiddleEast_daily_20050516-20060515.csv'
# make_table_7(data7, 'DUEXTTAU', date(2005, 5, 16), 365, table_50, out_csv)

fname1='//Users//dulichen//PycharmProjects//cisi567//venv//lib//PA_copy//thesis//G5NR_aerosol_variables_over_MiddleEast_daily_20060516-20070515_copy.nc'
data72 = Dataset(fname1)
out_csv = '//Users//dulichen//PycharmProjects//cisi567//venv//lib//PA_copy//thesis//G5NR+MERR2_dust_over_MiddleEast_daily_20060516-20070515.csv'
make_table_7(data72, 'DUEXTTAU', date(2005, 5, 16), 365, table_50, out_csv)