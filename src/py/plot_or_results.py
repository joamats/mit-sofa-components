import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.ticker import AutoMinorLocator, FixedLocator
import numpy as np

import matplotlib
#matplotlib.use('TKAgg')

plot_name = "Forest Plot"
df = pd.read_csv(f"results/sofa_plot_data.csv")

# based on
# https://medium.com/@ginoasuncion/visualizing-logistic-regression-results-using-a-forest-plot-in-python-bc7ba65b55bb


"""
plt.figure(figsize=(6, 4), dpi=150)
ci = [df.iloc[::-1]['or'] - df.iloc[::-1]['lci'].values, 
df.iloc[::-1]['uci'].values - df.iloc[::-1]['or']]

plt.errorbar(x=df.iloc[::-1]['or'], y=df.iloc[::-1].index.values, xerr=ci,
            color='black',  capsize=3, linestyle='None', linewidth=1,
            marker="o", markersize=5, mfc="black", mec="black")
plt.axvline(x=1, linewidth=0.8, linestyle='--', color='black')
plt.tick_params(axis='both', which='major', labelsize=8)
plt.xlabel('Odds Ratio and 95% Confidence Interval', fontsize=8)
plt.tight_layout()
# plt.savefig('raw_forest_plot.png')
plt.show()
"""

# first components
# second day
# third cohort

components = df.component.unique()
days = df.day.unique()
cohorts = df.cohort.unique()
colors = ["tab:red", "tab:blue"]
markerstyles = ['o', 'v']
yy = range(len(components), 0, -1)
dayoffset = [-.25, .25]
cohortoffset = [-.04, .04]

fig, ax = plt.subplots(nrows=1, sharex=True, sharey=True, figsize=(6, 4), dpi=200)

for c,y in zip(components, yy): 
    
    for i,k in enumerate(cohorts):

        for j,d in enumerate(days):
            new_df = df[(df.component == c) & (df.day == d)]
            row = new_df[new_df.cohort == k]
            ci = [row['or'] - row['lci'], row['uci'] - row['or'] ]
            plt.errorbar(x=row['or'], y=y+dayoffset[j]+cohortoffset[i], xerr=ci, \
                         ecolor=colors[i], capsize=3, \
                         label = k, \
                         linewidth=1, marker=markerstyles[j], \
                         markersize=5, mfc=colors[i], mec=colors[i])

ax.yaxis.grid(True, which='minor')  
ax.set_yticks([1.5, 2.5, 3.5 , 4.5 , 5.5], minor=True)
for tick in ax.yaxis.get_minor_ticks():
    tick.tick1line.set_visible(False)
    tick.tick2line.set_visible(False)
    tick.label1.set_visible(False)
    tick.label2.set_visible(False)

plt.yticks(ticks= yy, \
           labels=components)
plt.ylabel(ylabel='SOFA components')
plt.axvline(x=1, linewidth=0.8, linestyle='--', color='black')
plt.tick_params(axis='both', which='major', labelsize=8)
plt.xlabel('Odds Ratio and 95% Confidence Interval', fontsize=8)
plt.tight_layout()
plt.savefig('results/forest_plot.png')



"""


    axes[i].errorbar(x=df_temp1.sofa_start, y=df_temp1.psi, 
                     yerr=((df_temp1.psi- df_temp1.i_ci), (df_temp1.s_ci-df_temp1.psi)),
                     fmt='-o', c='tab:gray', ecolor='tab:gray',
                     elinewidth=.4, linewidth=1.5, capsize=4, markeredgewidth=.4,
                     label="MIMIC")
"""

"""
sofas_start = [0., 4., 7., 11.]
sofas_end = [3., 6., 10., 100.]

# initialize column
df['sofa_start'] = np.nan

# create column mapping sofa ranges
for s,e in zip(sofas_start, sofas_end):
    df['sofa_start'] = df.apply(lambda row: s \
                                     if ((row.SOFA >= s) & (row.SOFA <= e)) \
                                     else row.sofa_start,
                                     axis=1)

t_dict = dict(zip(["ventilation_bin", "rrt", "pressor"],
                  ["Mechanical Ventilation", "RRT", "Vasopressor(s)"]))

r_dict = dict(zip(range(2), ["Non-White", "White"]))

fig, axes = plt.subplots(1, 3,
                         sharex=True, sharey=True,
                         figsize=(8.25,3),
                         constrained_layout=True)

fig.suptitle('Distribution of Patients, across SOFA ranges and treatment\n')

w = [-.7, .7] 

colors1 = ['dimgray', 'firebrick']
colors2 = ['silver', 'salmon']


for i, t in enumerate(t_dict.keys()):

    # iterating over races
    for j, r in r_dict.items():

        df_temp = df[df.ethnicity_white == j]


        axes[i].bar(x=[i + w[j]/2 for i in sofas_start],
                    height=df_temp[df_temp[t]==0].groupby('sofa_start')[t].count()/1000,
                    width=w[1],
                    label=f"{r}\nNot Treated",
                    color=colors1[j],
                    edgecolor='white'
                    )

        axes[i].bar(x=[i + w[j]/2 for i in sofas_start],
                    height=df_temp[df_temp[t]==1].groupby('sofa_start')[t].count()/1000,
                    width=w[1],
                    label=f"{r}\nTreated",
                    bottom=df_temp[df_temp[t]==0].groupby('sofa_start')[t].count()/1000,
                    color=colors2[j],
                    hatch="//",
                    edgecolor='white'
                    )
            
    axes[i].set(xlabel=None)
    axes[i].set(ylabel=None)

    axes[i].set_title(t_dict[t])
    axes[i].set_xticklabels(["0-3", "4-6", "7-10", ">10"])
    axes[i].set_xticks([0., 4., 7., 11.])
    axes[0].set(ylabel="Number of Patients\n(thousands)")
    axes[2].legend(bbox_to_anchor=(1.05, 1.02), loc='upper left')

fig.supxlabel('\nSOFA Range              ')

fig.savefig(f"results/paper/fig4_TMLE_dist.png", dpi=200)
"""