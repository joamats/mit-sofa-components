import pandas as pd
from matplotlib import pyplot as plt
import numpy as np

import matplotlib
matplotlib.use('TKAgg')

days = {24: 1, 168: 7}
comps = dict()

for h in days.keys():
    comps[h] = {f"cns_{h}Abnormal": "Neurological",
                f"resp_{h}Abnormal": "Respiratory",
                f"coag_{h}Abnormal": "Coagulation",
                f"liver_{h}Abnormal": "Liver",
                f"cv_{h}Abnormal": "Cardiovascular",
                f"renal_{h}Abnormal": "Renal"
               }

sens_analy = {"all": ["All Patients",],
              "cirrhosis": ["No Cirrhosis Patients", "Liver"],
              "ckd": ["No Patients with CKD over 2nd stage", "Renal"],
              "copd_asthma": ["No COPD Nor Asthma Patients", "Respiratory"],
              "heart_failure": ["No CHF Patients", "Cardiovascular"]
             }

cohorts = {"MIMIC": "MIMIC-IV",
          "eICU": "eICU-CRD"
          }

colors = ["tab:blue", "tab:red"]
markerstyles = ['o', 'v']
daysoffset = [.1,-.1]

yy = range(6, 0, -1)

for s, s_name in sens_analy.items():

    fig, ax = plt.subplots(1, 2, sharex=True, sharey=False,
                           figsize=(10.5, 4), dpi=400)
                           

    fig.suptitle(f"    SOFA Components Forest Plot, with {s_name}")
        
    for i, (c, c_name) in enumerate(cohorts.items()):

        for j, (h, d) in enumerate(days.items()):

            df = pd.read_csv(f"results/glm/{c}_{h}_{s}.csv")
            df = df.rename(columns={"Unnamed: 0": "comp"})

            for (comp, comp_name), y in zip(comps[h].items(), yy): 

                row = df[df.comp == comp]

                ci = [row['OR'] - row['2.5 %'], row['97.5 %'] - row['OR'] ]

                if y == 6:
                    lbl = d
                else:
                    lbl = None

                ax[i].errorbar(x=row['OR'], y=y+daysoffset[j], xerr=ci, \
                               ecolor=colors[j], color=colors[j], marker=markerstyles[j], \
                               capsize=3, \
                               label = lbl, \
                               linewidth=1, \
                               markersize=5, mfc=colors[j], mec=colors[j])

        ax[i].set_title(c_name)
        ax[i].axvline(x=1, linewidth=0.8, linestyle='--', color='black')
        ax[i].set_xlabel('Odds Ratio and 95% Confidence Interval')
        ax[i].set_xlim([0.2, 5.8])
        ax[0].set_ylabel(ylabel='SOFA Components')
        ax[i].set_yticks(ticks= yy, labels=comps[h].values())
        ax[1].legend(title="Day", bbox_to_anchor=(1.05, 0.6), loc='upper left')

        plt.tight_layout()

    fig.savefig(f'results/{s_name}.png')
