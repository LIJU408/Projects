# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.

import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
from dash.exceptions import PreventUpdate

import plotly.express as px

import pandas as pd

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

df = pd.read_csv('https://raw.githubusercontent.com/wadefagen/datasets/master/gpa/uiuc-gpa-dataset.csv')

df = df[df['Primary Instructor'].notnull()]

groups = df.groupby(['Subject','Number','Primary Instructor'])

pro_df = groups[['A+','A','A-','B+','B','B-','C+','C','C-','D+','D','D-','F']].sum()

pro_df['num_grades'] = pro_df['A+'] + pro_df['A'] + pro_df['A-'] +  pro_df['B+'] + pro_df['B'] + pro_df['B-'] + pro_df['C+'] + pro_df['C'] + pro_df['C-'] + pro_df['D+'] + pro_df['D']+ pro_df['D-'] + pro_df['F']

pro_df['A_per'] = (pro_df['A+']+ pro_df['A'] + pro_df['A-'])/pro_df['num_grades']
pro_df['B_per'] = (pro_df['B+']+ pro_df['B'] + pro_df['B-'])/pro_df['num_grades']
pro_df['C_per'] = (pro_df['C+']+ pro_df['C'] + pro_df['C-'])/pro_df['num_grades']
pro_df['D_per'] = (pro_df['D+']+ pro_df['D'] + pro_df['D-'])/pro_df['num_grades']
pro_df['F_per'] = (pro_df['F'])/pro_df['num_grades']

pro_df['GPA'] = (4.00*pro_df['A+'] + 4.00*pro_df['A'] + 3.67*pro_df['A-'] + 3.33*pro_df['B+'] + 3.00*pro_df['B'] + 2.67 * pro_df['B-'] + 2.33*pro_df['C+']
            + 2.00 * pro_df['C'] + 1.67*pro_df['C-'] + 1.33*pro_df['D+'] + 1*pro_df['D'] + 0.67*pro_df['D-'] + 0 *pro_df['F'])/pro_df['num_grades']

pro_df = pro_df.reset_index()

available_subjects = df['Subject'].unique()


app.layout = html.Div([
    dcc.Markdown('#### How will your professor grade you for a course? Check the past statistics!'),
    html.Div([
        dcc.Markdown('###### Please choose subject here'),
        html.Div([
            dcc.Dropdown(
                id='subject',
                options=[{'label': i, 'value': i} for i in available_subjects],
                value='STAT'
            ),
        ],
        style={'width': '48%', 'display': 'inline-block'}),
  
    ]),
    html.Hr(),
    html.Div([
        dcc.Markdown('###### Please choose course number here'),
        html.Div([
            dcc.Dropdown(
                id='course_number'
            ),
        ],
        style={'width': '48%', 'display': 'inline-block'}),
    html.Hr(),
  
    ]),

    dcc.Graph(id='gpa-distribution-graphic')

])

@app.callback(
    Output('course_number', 'options'),
    Input('subject', 'value'))
def set_course_number_options(selected_subject):
    num_list = df.loc[df['Subject'] == selected_subject,'Number'].unique()
    return [{'label': i, 'value': i} for i in num_list]


@app.callback(
    Output('course_number', 'value'),
    Input('course_number', 'options'))
def set_course_number_value(available_options):
    return available_options[0]['value']

@app.callback(
    Output('gpa-distribution-graphic', 'figure'),
    Input('subject', 'value'),
    Input('course_number', 'value'))
def visualization_figure(selected_subject, selected_course_number):
    selected_df = pro_df[(pro_df['Subject'] == selected_subject) & (pro_df['Number'] == selected_course_number)]
    if selected_df.empty:
        raise PreventUpdate
    else:
        fig = px.bar(selected_df, y="Primary Instructor", x=["A_per", "B_per", "C_per","D_per","F_per"], hover_data = ['GPA','num_grades'],title = '{} {} Grades'.format(selected_subject,selected_course_number))
        fig.update_layout(margin={'l': 40, 'b': 40, 't': 40, 'r': 0}, hovermode='closest')
        return fig

if __name__ == '__main__':
    app.run_server(debug=True, host = '127.0.0.1')