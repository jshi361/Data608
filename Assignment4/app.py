
import pandas as pd
import numpy as np
import json
import plotly.offline as py
import plotly.graph_objs as go
from plotly import subplots
import dash
from dash import dcc
from dash import html
pd.options.mode.chained_assignment = None  # default='warn'

url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json'

#load data
trees = pd.read_json(url)
#gather data for question 1
trees_q1 = trees[['spc_common','health','boroname']]
#convert nans to 'Uknown'
trees_q1['spc_common'].fillna('Unknown',inplace = True)
#drop remaining nans
trees_q1.dropna(inplace = True)

#identify different health conditions
statuses = list(set(trees_q1['health']))
#create colors for different health conditions
colors = ['rgb(49,130,189)','rgb(204,204,204)','rgba(222,45,38,0.8)']

#create columns that specify tree health conditions
for status in set(trees_q1['health']):
    trees_q1[status] = np.where(trees_q1['health']==status,1,0)
    
trees_q1 = pd.DataFrame(trees_q1.groupby(['boroname','spc_common']).sum())

#find out boroughs
boroughs = list(set(trees['boroname']))

#calculate proportion of trees in different conditions
trees_q1['total'] = trees_q1.sum(axis=1)
for column in list(trees_q1.columns):
    trees_q1[column] = (trees_q1[column]/trees_q1['total'])*100
trees_q1.head()

#create list to store data for each borough
trace_list = []

#create plot titles
borough_list = list(map(lambda x: str(x), boroughs))
row = 1
col = len(boroughs)

fig = subplots.make_subplots(rows=row, cols=col, subplot_titles=tuple(borough_list))

#iterate through boroughs
for borough in boroughs:
        for i in range(0,len(statuses)):
            trace = go.Bar(
            x = list(trees_q1.loc[borough].index),
            y = list(trees_q1.loc[borough][statuses[i]]),
            name = statuses[i],
            marker=dict(color=colors[i])
            )
            trace_list += [trace]

row_i = []
col_j = []
for i in range(1,row+1):
    for j in range (1,col+1):
        for n in range (1,4):
            row_i.append(i)
            col_j.append(j)

for i in range(0,len(trace_list)):        
     fig.append_trace(trace_list[i], row_i[i],col_j[i]) 
 
        
fig['layout'].update(showlegend=False,height=400, width=1400, title='Proportion of Trees in Good, Fair and Poor Conditions', barmode='stack')





#gather data for question 2
trees_q2 = trees[['spc_common','health','boroname','steward']]
#convert nans to 'Uknown'
trees_q2['spc_common'].fillna('Unknown',inplace = True)
#drop remaining nans
trees_q2.dropna(inplace = True)
#factorize categorical variables 'steward' and 'health'
trees_q2[['steward','health']] = trees_q2[['steward','health']].apply(lambda x : pd.factorize(x)[0])
##group by borough and plant name and calculate correlation
trees_q2_cor = pd.DataFrame(trees_q2.groupby(['boroname','spc_common']).corr())

#create grids for subplots
fig_q2 = subplots.make_subplots(rows=1, cols=len(boroughs), subplot_titles=tuple(borough_list))

#create list that stores graphs data
trace_list_q2 =[]

#identify trees
plants = list(set(trees_q2['spc_common']))

#iterate through boroughs
for borough in boroughs:
    trace = go.Bar(
            #list of trees
            x = list(trees_q1.loc[borough].index),
            #list of correlation coefficients
            y = list(trees_q2_cor.loc[borough]['steward'][::2])
            )
    #add individual plot data to the list
    trace_list_q2 += [trace]

#add subplots 
for i in range(len(boroughs)):
    fig_q2.append_trace(trace_list_q2[i], 1, i+1) 


fig_q2['layout'].update(showlegend=False,height=500, width=1400, title='Proportion of Trees in Good, Fair and Poor Conditions')


app = dash.Dash()

colors = {
    'background': '#ffffff',
    'text': '#111111'
}



app.layout = html.Div(style={'backgroundColor': colors['background']}, children=[
    html.H1(
        children='Question #1',
        style={
            'textAlign': 'center',
            'color': colors['text']
        }
    ),
    html.Div(children='Proportion of trees in Good, Fair and Poor conditions', style={
        'textAlign': 'center',
        'color': colors['text']
    }),
    
    html.Div([
        dcc.Graph(figure=fig, id='my-figure-q1')
    ]),

    html.H1(
        children='Question #2',
        style={
            'textAlign': 'center',
            'color': colors['text']
        }
    ),
    
    html.Div(children='Correlation between stewardsand health of trees', style={
        'textAlign': 'center',
        'color': colors['text']
    }),
    
    html.Div([
        dcc.Graph(figure=fig_q2, id='my-figure-q2')
    ])

    ])



if __name__ == '__main__':
    app.run_server(debug=True)