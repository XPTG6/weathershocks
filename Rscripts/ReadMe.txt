Thesis by Andrea C. Caflisch

Some notes on the scripts contained here:

1. The main results are actually produced in scripts starting with Adm2_75
2. 'Man' are no longer main, but Main_Prep.R does build some tables on which Adm2_75_Prep.R relies
3. Prep scripts should not be called in 'source': saving and loading shapefiles can alter column names (e.g. shortened, change '.' to '-'), so attention needs to be paid to this when running them (if need be, just use setnames to change them)
