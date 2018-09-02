# In Main_Prep, some NAs appear in Panel for variable 'status' after merging Panel and EPR-Core using foverlaps.
# This diagnostic code was run just after the foverlaps line. 
# Note that one fix enters the code before that, and needs to be commented out to reproduce results.

## Diagnostic for NAs in status: ----
if(pre.length!=nrow(Panel))stop("Output of foverlaps has different number of rows")
ProportionNA(Panel)
Panel[is.na(status), .(min(year), max(year)), by = .(group, NAME_0)] # 3 groups
EPR_raw <- as.data.table(read.csv("/data/Data/EPR-2018.csv"))

# Group 1: Myene (Gabon) ----
EPR_raw[group == "Myene"] # checked on Grow documentation: 
# Myene split into Orungu and Nkomi, which are appropriately coded;
# but some areas in Geo-EPR are not assigned to either subfaction and remain coded as Myene.
# Note in following after 2005 Orungu opposition disappears and Myene return together.
# Therefore, code Myene as 'JUNIOR PARTNER' like Nkomi. ***

# Group 2: Africans (Zimbabwe) ----
EPR_raw[group == "Africans"] # checked on Grow documentation: 
# 'Africans' only relevant in post-Independence white-black conflict; 
# unclear why polygon remains later and what ethnic groups it includes, or their status
# Therefore, remove from Panel. ***

# Group 3: Shona (Zimbabwe) ----
EPR_raw[group == "Shona"]
# Appears in EPR as following two modifications, not reflected in Geo-EPR
EPR_raw[group == "Shona (minus Manyika & Ndau)"]; EPR_raw[group == "Shona (minus Ndau)"]
Panel[group == "Shona (minus Manyika & Ndau)"]; Panel[group == "Shona (minus Ndau)"]
# Manyika & Ndau are subgroups of Shona listed in dataset, but not discussed in country history doc.
# Therefore, code Shona variants as simply Shona in EPR Core, before merging above. ***
####