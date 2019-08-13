# SedStatChange
Explanation of what code is doing:
- Load in polygons (PIZ, SIZ, Region)
- Add column in PIZ attributes table for 'sub-region'
- Bring in monitoring data and add column to identify record as 'monitoring' 
  (RSMP station code, % of mjr sed fractions, Lat, Long)
- Take only monitoring data falling within PIZ
- Bring in baseline data and add column to identify record as 'baseline'
- Take only baseline data falling within PIZ
- Match Baseline and Monitoring samples and join DFs (object: data)
- Create 2 copies of object 'data' and rbind to 'data'. area_numbe pop with area_numbe (1), sub_region (2) and All (3). (Object: data3)
- Take only required columns
- Concatenate area and treatment
- Identify mean sed comp by site at baseline and monitoring (object: sumdata)
- Perform Wilcoxen Rank tests (p-values by site) object: pmatrix2
- Split sumdata into baseline and monitoring subsets (objects: sumdata_b sumdata_m)
- Join results of Wilcoxen tests with mean sed at baseline and monitoring
