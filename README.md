# kwb
Cleaning and assembling KWB (Kerncijfers Wijken en Buurten) Dataset

The (Excel) files for the KWB can be dowloaded here: https://www.cbs.nl/nl-nl/reeksen/publicatie/kerncijfers-wijken-en-buurten

The output of the scripts is a clean and aggregated files with some key variables per pc4, and gemeente. 
Note that the definition of buurten and wijken changes every year and is therefore not suitable for a panle analysis. However, postcodes are relativley stable over time. 
When aggregating KWB data per gemeente, I use the macthing form 2022 for a stable relation between gemeente codes and 4-digit postcodes. 
