use "/Users/johnmuschelli/Dropbox/CTR/DHanley/MISTIE/MISTIE DSMB Analysis/statacalc/Enrolled_Subject_CT_Scans_Merged.dta", clear
keep patientName Date_Time_CT Dataset Date_Time_CT_RC ichvolumeabc ///
site_abc2 L_lat Rt_lat IIIrd IVth ICHVolume cathetertractvol ///
cathetertractvol2 othervol IVHVolume time time_RC CTTime ///
entered_ICS calculated_ICS
outsheet using "/Users/johnmuschelli/Dropbox/CTR/DHanley/CT_Registration/Segmentation/Segmentation_Paper/All_ABC2_Data.csv", comma names replace


use "/Users/johnmuschelli/Dropbox/CTR/DHanley/MISTIE/MISTIE DSMB Analysis/statacalc/Enrolled_Subject_CT_Scans_Merged.dta", clear

sort patientName Date_Time_CT_RC 
by patientName : keep if _n == 1
order Date_Time_CT time 
outsheet using "/Users/johnmuschelli/Dropbox/CTR/DHanley/CT_Registration/Segmentation/Segmentation_Paper/ABC2_Data.csv", comma names replace
