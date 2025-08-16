
# ABOUT
Data files associated with Clemente et al (202x), "Characterization of floral volatile organic compounds in ten cultivars of basil (Ocimum basilicum L.)" 

# CREDITS
## Study design: Seanne R. Clemente, Lynn S. Adler, Nina Theis
## Data collection: Seanne R. Clemente
## Data processing: Joesph (Joe) Zeno, Nina Theis, Seanne R. Clemente
## Data analysis: Seanne R. Clemente



# METADATA

##FILENAME: basil_chemicals_all.csv

## list of variables: 
### datafile: original filename for sample, from GCMS
### sampname: sample ID, given during data collection
### variety: name of cultivar from which sample was taken
### type: plant tissue from which sample was taken
### dry wt: dry weight of plant tissue from which sample was taken
### columns 6-91 are quantities of identified VOCS in nanogram VOC per gram of plant tissue per hour 	of VOC collection (ng/g/hr)  
### row 2 of datasheet. retention indices of compounds, blank if column does not have a VOC.

##FILENAME: regressions.csv

### origin: This datafile was created using "02_multivar_basil.R" Running the entirety of the .R file 	will create this .csv file anew.

## list of variables:
### columns 1-71 are the average quantities of VOCS collected from floral tissue per cultivar, in 	ng/g/hr 
### columns 72-142 are the average quantities of VOCS collected from leaf tissue per cultivar, in 	ng/g/hr 

