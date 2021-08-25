#==========================================================================================
#C-O2-P box model 
#Study: Papadomanolaki, N.M., Sluijs, A. and Slomp, C.P. (2021). Eutrophication and deoxygenation forcing of marginal marine organic carbon burial during the PETM. Paleoceanography and Paleoclimatology.
#==========================================================================================

#KEY PUBLICATIONS

#Walker, J. C., & Kasting, J. F. (1992). Effects of fuel and forest conservation on future levels of atmospheric carbon dioxide. Palaeogeography, Palaeoclimatology, Palaeoecology, 97(3), 151-189.
#Zeebe, R. E. (2012). LOSCAR: Long-term ocean-atmosphere-sediment carbon cycle reservoir model v2. 0.4. Geoscientific Model Development, 5(1), 149-166.
#Zeebe, R. E., Zachos, J. C., & Dickens, G. R. (2009). Carbon dioxide forcing alone insufficient to explain Palaeocene-Eocene Thermal Maximum warming. Nature Geoscience, 2(8), 576-580.
#Komar, N., & Zeebe, R. E. (2017). Redox-controlled carbon and phosphorus burial: A mechanism for enhanced organic carbon sequestration during the PETM. Earth and Planetary Science Letters, 479, 71-82.
#Slomp, C. P., & Van Cappellen, P. (2006). The global marine phosphorus cycle: sensitivity to oceanic circulation. Biogeosciences Discussions, 3(5), 1587-1629.
#==========================================================================================

#Install ReacTran first
install.packages(ReacTran)
require(ReacTran)
library(beepr)
options(digits = 16)

#==========================================================================================
####SETTING UP SIMULATION####
#CO2 FORCING:Set one of the given scenarios to TRUE (lines 341 - 406) or set new (time-dependent) values for fpCO2
#STRATIFICATION: For a run with stratification set stratonoff to 1 (line 477)
#TEMPERATURE EFFECT: For simulations with constant temperature set Tonoff to 0
#OXYGEN EFFECT: For simulations with constant surface oxygen set fO2S to 0; for constant deep oxygen set fO2D to 0; for constant DOA set fDOA to 0
#PRODUCTIVITY EFFECT: For simulations with constant productivity set fPP to 0
#P WEATHERING STRENGTH: To test the effect of the strength of P weathering, adjust np (line 138)

#NB: Timestamps ta and tb correspond to the "end of plateau" and "end of PETM" phases, respectively
#==========================================================================================
#SIMULATION NAME
pCO2sc <- "Z09_mainrun_np04_straton_S101_S304_fOPB075_fCaPB04" #Change for each different (pCO2) run
#==========================================================================================
#MODEL GEOMETRY
#==========================================================================================
#S1: Arctic surface; S2: open ocean cont. shelf; S3: Epicontinental seaway surface;
#S4: low latitudes open ocean; S5: Southern Ocean; IM: thermocline
#D1: Arctic deep; D2: Atlantic deep; D3: Indotethys deep; D4: Pacific deep
#D5: Epicontinental seaway deep
#==========================================================================================
#Pre-PETM
#Water reservoirs

#Reservoir Area (Tm2)
S1a <- 3.0    #Arctic surface box area 
S2a <- 27.0   #Shelf box area
S3a <- 13.5   #Epicontintental seaway box area 
S4a <- 314.1  #Low latitudes box area 
S5a <- 34.9   #Southern ocean box area 

IMa <- 314.1  #Thermocline box area 
D1a <- 3.0    #Arctic deep box area
D2a <- 52.35  #Atlantic deep box area 
D3a <- 80.27  #Indotethys deep box area
D4a <- 181.48 #Pacific deep box area
D5a <- 13.5   #Epicontinental seaway deep box area

OCa <- 392.5  #Total ocean area 
SRa <- 43.5   #Area of surface boxes with river inflow (S1-3) 
DOa <- 314.1  #Total deep 

#Reservoir volume (Tm3)
S1v <- 600    #Arctic surface box volume 
S2v <- 4050   #Shelf box volume 
S3v <- 2025   #Epi. seaway box volume 
S4v <- 47115  #Low lat. box volume 
S5v <- 6980   #South. oc. box volume 

IMv <- 282690 #Thermocline box volume 
D1v <- 3114   #Arctic deep box volume 
D2v <- 154000 #Atlantic deep box volume 
D3v <- 160300 #Indotethys deep box volume 
D4v <- 654700 #Pacific deep box volume 
D5v <- 10125  #Epi. seaway deep box volume

OCv <- 1325360  #Total ocean volume 
DOv <- 969000   #Deep open ocean volume

#Dissolved P Reservoirs (Tmol)
DP_S1 <- 0.5263366637529890 #Dissolved P content Arctic surface
DP_S2 <- 4.6046737811227500 #Dissolved P content shelf
DP_S3 <- 1.8006655639512700 #Dissolved P content Epi. seaway surface
DP_S4 <- 37.040094339622600 #Dissolved P content low lat.
DP_S5 <- 3.7639237955987400 #Dissolved P content South. oc.
DP_IM <- 369.06069736169500 #Dissolved P content thermocline
DP_D1 <- 5.7199017032666800 #Dissolved P content Arctic deep
DP_D2 <- 141.82170337838900 #Dissolved P content Atlantic deep
DP_D3 <- 161.94919905538000 #Dissolved P content Indo. deep
DP_D4 <- 706.83829621254000 #Dissolved P content Pacific deep
DP_D5 <- 17.605387179225800 #Dissolved P content Epi. seaway deep

#Dissolved O2 Reservoirs (Tmol)
DO2_IM <- 38221.973162267000 #Dissolved oxygen content thermocline
DO2_D1 <- 313.71321396804400 #Dissolved oxygen content Arc. deep
DO2_D2 <- 31459.468903559900 #Dissolved oxygen content Atl. deep
DO2_D3 <- 30719.501586246700 #Dissolved oxygen content Indo. deep		
DO2_D4 <- 119041.09042832800 #Dissolved oxygen content Pac. deep
DO2_D5 <- 862.66511456352000 #Dissolved oxygen content Epi. deep
			
#Temperature (deg. Celcius)
S1_T <- 17 #Water temperature Arctic surface
S2_T <- 25 #Water temperature shelf
S3_T <- 25 #Water temperature Epi. surface
S4_T <- 25 #Water temperature Low lat.
S5_T <- 12 #Water temperature South. oc.
IM_T <- 16 #Water temperature thermocline
D1_T <- 12 #Water temperature Arctic deep
D2_T <- 12 #Water temperature Atlantic deep
D3_T <- 12 #Water temperature Indotethys deep
D4_T <- 12 #Water temperature Pacific deep
D5_T <- 12 #Water temperature Epi. deep

#==========================================================================================
#PARAMETERS
#==========================================================================================
#Atmosphere
Catm <- 183333.333333333   #Atmospheric carbon dioxide mass - pre-PETM (Tmol) 
q0 <- (2200)/12            #Conversion factor (Tmol/uatm)
pCO2_0 <- Catm/q0          #Initial pCO2 (uatm)
kas <- 0.06                #air-sea gas exchange factor (Tmol (uatm Tm2 yr)-1)
Fs_0 <- 6                  #Initial silicate weathering (Tmol/yr)
Fcc_0 <- 16                #Initial carbonate weathering (Tmol/yr)
ns <- 0.2                  #Silicate weathering scaling factor (-)
ncc <- 0.4                 #Carbonate weathering scaling factor (-) 

#Temperature factors
tlags  <- 20   #surface waters temperature relaxation time 
tlagim <- 200  #intermediate waters temperature relaxation time
tlagd  <- 1000 #deep waters temperature relaxation time 
s      <- 3    #Temperature sensitivity to doubling of CO2 

#Carbon
tcpfract <- 0.78  #Org. C decay ratio thermocline

fExp_S1D1  <- 0.15 #Corg export fraction Arc. surface to Arc. deep
fExp_S2S4  <- 0.03 #Corg export fraction shelf to low lat.
fExp_S3D5  <- 0.04 #Corg export fraction Epi. surface to Epi. deep
fExp_S4IM  <- 0.08 #Corg export fraction low lat. to thermocline
fExp_S5Dx  <- 0.10 #Corg export fraction South. oc. to deep open ocean
fExp_IMDx  <- 0.22 #Corg export fraction thermocline to deep open ocean

#Phosphorus
np <- 0.4 #Phosphorus weathering exponent

fR_S1 <- 0.10 #Fraction of riverine input to Arctic surface
fR_S2 <- 0.55 #Fraction of riverine input to shelf
fR_S3 <- 0.35 #Fraction of riverine input to Epi. seaway surface

fExp_P_S1D1  <- 0.15 #Porg export fraction Arc. surface to Arc. deep
fExp_P_S2S4  <- 0.03 #Porg export fraction shelf to low lat.
fExp_P_S3D5  <- 0.04 #Porg export fraction Epi. surface to Epi. deep
fExp_P_S4IM  <- 0.08 #Porg export fraction low lat. to thermocline
fExp_P_S5Dx  <- 0.10 #Porg export fraction South. oc. to deep open ocean
fExp_P_IMDx  <- 0.22 #Porg export fraction thermocline to deep open ocean

kbio_S1 <- 0.8603432363050410 #Productivity rate constant Ar. surface
kbio_S2 <- 0.9403898918957640 #Productivity rate constant shelf
kbio_S3 <- 0.9548356213736300 #Productivity rate constant Epi. surface
kbio_S4 <- 0.8000000000000000 #Productivity rate constant low lat.
kbio_S5 <- 0.4135587908036520 #Productivity rate constant South. oc.    

kOPB_S1 <- 0.0039494645593869700 #OrgP burial rate constant Arc. surface
kOPB_S2 <- 0.0029737144917737200 #OrgP burial rate constant shelf
kOPB_S3 <- 0.0046808468851993800 #OrgP burial rate constant Epi. surface
kOPB_D1 <- 0.0040769230769230800 #OrgP burial rate constant Arc. deep
kOPB_D2 <- 0.0006794871794871800 #OrgP burial rate constant Atl. deep
kOPB_D3 <- 0.0010418803418803400 #OrgP burial rate constant Indo. deep
kOPB_D4 <- 0.0023555555555555600 #OrgP burial rate constant Pac. deep
kOPB_D5 <- 0.0040769230769230800 #OrgP burial rate constant Epi. deep

kauth_S1 <- 0.009336237952571330 #CaP burial rate constant Arc. surface
kauth_S2 <- 0.006150224738122540 #CaP burial rate constant shelf
kauth_S3 <- 0.009799545774702750 #CaP burial rate constant Epi. surface
kauth_D1 <- 0.008187224839731210 #CaP burial rate constant Arc. deep
kauth_D2 <- 0.031165048254745100 #CaP burial rate constant Atl. deep
kauth_D3 <- 0.031165048254745100 #CaP burial rate constant Indo. deep
kauth_D4 <- 0.031165048254745100 #CaP burial rate constant Pac. deep
kauth_D5 <- 0.008187224839731210 #CaP burial rate constant Epi. deep

fOPB <- 0.75 #Redox sensitivity OrgP burial
fCaPB <- 0.4 #Redox sensitivity CaP burial

#Ratios
RedfR <- 106         #Redfield C:Porg ratio
RedfR_CO2 <- 106/138 #Redfield C:O2 ratio
RedfR_anox <- 1500   #Anoxic C:Porg ratio

S1_BR <- 400  #Arc. surface oxic burial C:Porg ratio
S2_BR <- 500  #Shelf oxic burial C:Porg ratio
S3_BR <- 400  #Epi. surface oxic burial C:Porg ratio
D1_BR <- 260  #Arc. deep oxic burial C:Porg ratio 
D2_BR <- 260  #Atl. deep oxic burial C:Porg ratio 
D3_BR <- 260  #Indo. deep oxic burial C:Porg ratio 
D4_BR <- 260  #Pac. deep oxic burial C:Porg ratio 
D5_BR <- 260  #Epi. deep oxic burial C:Porg ratio 

#Salinity

Sal <- 34.72 #Ocean salinity

#Oxygen
 
A <- c(-177.7888, 255.5907, 146.4813, -22.2040) #Gravimetric constant [Weiss, 1970]
B <- c(-0.037362, 0.016504, -0.0020564)         #Gravimetric constant [Weiss, 1970]

#=========================================================================================

# Assembling the 'parameters' vector

parameters <- c(S1a      =S1a,
                S2a      =S2a,
                S3a      =S3a,
                S4a      =S4a,
                S5a      =S5a,
                IMa      =IMa,
                D1a      =D1a,
                D2a      =D2a,
                D3a      =D3a,
                D4a      =D4a,
                D5a      =D5a,
                OCa      =OCa,
                SRa      =SRa,
                S1v      =S1v,
                S2v      =S2v,
                S3v      =S3v,
                S4v      =S4v,
                S5v      =S5v,
                IMv      =IMv,
                D1v      =D1v,
                D2v      =D2v,
                D3v      =D3v,
                D4v      =D4v,
                D5v      =D5v,
                OCv      =OCv,
                DOv      =DOv,
                DP_S1   =DP_S1,
                DP_S3   =DP_S3,
                DP_S4   =DP_S4,
                DP_S5   =DP_S5,
                DP_IM   =DP_IM,
                DP_D1   =DP_D1,
                DP_D2   =DP_D2,
                DP_D3   =DP_D3,
                DP_D4   =DP_D4,
                DP_D5   =DP_D5,
                DO2_IM  =DO2_IM,
                DO2_D1  =DO2_D1,
                DO2_D2  =DO2_D2,
                DO2_D3  =DO2_D3,
                DO2_D4  =DO2_D4,
                DO2_D5  =DO2_D5,
                S1_T    =S1_T,
                S2_T    =S2_T,
                S3_T    =S3_T,
                S4_T    =S4_T,
                S5_T    =S5_T,
                IM_T    =IM_T,
                D1_T    =D1_T,
                D2_T    =D2_T,
                D3_T    =D3_T,
                D4_T    =D4_T,
                D5_T    =D5_T,
                Catm    =Catm,
                q0      =q0,
                pCO2_0  =pCO2_0,
                kas     =kas,
                Fs_0    =Fs_0,
                Fcc_0   =Fcc_0,
                ns      =ns,
                ncc     =ncc,
                tlags   =tlags,
                tlagim  =tlagim,
                tlagd   =tlagd,
                s       =s,
                fExp_S1D1 =fExp_S1D1,
                fExp_S2S4 =fExp_S2S4,
                fExp_S3D5 =fExp_S3D5,
                fExp_S4IM =fExp_S4IM,
                fExp_S5Dx =fExp_S5Dx,
                fExp_IMDx =fExp_IMDx,
                np        =np,
                fR_S1    =fR_S1,
                fR_S2    =fR_S2,
                fR_S3    =fR_S3,
                fExp_P_S1D1 =fExp_P_S1D1,
                fExp_P_S2S4 =fExp_P_S2S4,
                fExp_P_S3D5 =fExp_P_S3D5,
                fExp_P_S4IM =fExp_P_S4IM,
                fExp_P_S5Dx =fExp_P_S5Dx,
                fExp_P_IMDx =fExp_P_IMDx,
                kbio_S1    =kbio_S1,
                kbio_S2    =kbio_S2,
                kbio_S3    =kbio_S3,
                kbio_S4    =kbio_S4,
                kbio_S5    =kbio_S5,
                kOPB_S1    =kOPB_S1,
                kOPB_S2    =kOPB_S2,
                kOPB_S3    =kOPB_S3,
                kOPB_D1    =kOPB_D1,
                kOPB_D2    =kOPB_D2,
                kOPB_D3    =kOPB_D3,
                kOPB_D4    =kOPB_D4,
                kOPB_D5    =kOPB_D5,   
                kauth_S1   =kauth_S1,
                kauth_S2   =kauth_S2,
                kauth_S3   =kauth_S3,
                kauth_D1   =kauth_D1,
                kauth_D2   =kauth_D2,
                kauth_D3   =kauth_D3,
                kauth_D4   =kauth_D4,
                kauth_D5   =kauth_D5,
                fOPB       =fOPB,
                fCaPB      =fCaPB,
                RedfR      =RedfR,
                RedfR_CO2  =RedfR_CO2,
                RedfR_anox =RedfR_anox,
                S1_BR      =S1_BR,
                S2_BR      =S2_BR,
                S3_BR      =S3_BR,
                D1_BR      =D1_BR,
                D2_BR      =D2_BR,
                D3_BR      =D3_BR,
                D4_BR      =D4_BR,
                D5_BR      =D5_BR,
                Sal        =Sal,
                A          =A,
                B          =B
                )

#=============================================================================
# MODEL FORMULATION
#=============================================================================

model<-function(t,state,parameters)
{
  with(as.list(c(state,parameters)),{
    
    #PROBLEM-SPECIFIC CODE STARTS HERE
    
#=============================================================================
# SIMULATION SETUP
#=============================================================================

#ATMOSPHERIC pCO2 CONTROL [ONE MUST BE TRUE]
    
  #No CO2 forcing [LINE464]
  #[SET LINE 490 TO 0 if TRUE]
    if(FALSE){ 
      fpCO2 <- 1
    }
    
    #Frieling et al. 2016 - F16 [LINE398]
    if(FALSE){
      if (t<=20000) fpCO2 <- 1
      else if (t>20000 & t<=25868.661027) fpCO2 <- 0.00014126687927769000*t - 1.82533758555381
      else if (t>25868.661027 & t<=32529.46654) fpCO2 <- -0.00002137591833074730*t + 2.38201381426185
      else if (t>32529.46654 & t<=33676.4557) fpCO2 <- 0.00003949112675415410*t + 0.402041307784129
      else if (t>33676.4557 & t<=39876.78599) fpCO2 <- -0.00001274473715622840*t + 2.16116006471306
      else if (t>39876.78599 & t<=41173.68112) fpCO2 <- 0.00003779161510905200*t + 0.145932760715575
      else if (t>41173.68112 & t<=47270.16346) fpCO2 <-  -0.00001048282225149610*t + 2.13356905084582
      else if (t>47270.16346 & t<=48510.51864) fpCO2 <-  0.00004320945127166570*t - 0.404473495133505
      else if (t>48510.51864 & t<=54991.80281) fpCO2 <- -0.00000920766995808447*t + 2.13830824133782
      else if (t>54991.80281 & t<=55999.4763) fpCO2 <- 0.00005604566232720010*t - 1.45009014039013
      else if (t>55999.4763 & t<=62463.80807) fpCO2 <- -0.00000834852581185191*t + 2.1559506721607
      else if (t>62463.80807 & t<=63460.41349) fpCO2 <- 0.00005504307157770560*t - 1.80372990042901
      else if (t>63460.41349 & t<=69996.37781) fpCO2 <- -0.00000742846124930970*t + 2.16073940412507
      else if (t>69996.37781 & t<=70989.7123) fpCO2 <- 0.00005742703049339260*t - 2.37891009894962
      else if (t>70989.7123 & t<=77338.5043) fpCO2 <- -0.00000738429420884804*t + 2.22202719544362
      else if (t>77338.5043 & t<=78620.42513) fpCO2 <-  0.00004298751539871220*t - 1.67365321848986
      else if (t>78620.42513 & t<=91796.43204) fpCO2 <- -0.00000529155404134969*t + 2.12206774576891
      else if (t>91796.43204 & t<=92613.7617) fpCO2 <-  0.00004905360623071200*t - 2.86662406584854
      else if (t>92613.7617 & t<=220000) fpCO2 <- -0.00000210256086596512*t + 1.87114100312857
      else if (t>220000) fpCO2 <- 1
    }
   
    #Gutjahr et al. 2017 Median - G17 [LINE414]
    if(FALSE){ 
      if (t<=20000) fpCO2 <- 1
      else if (t>20000 & t<= 26000) fpCO2 <- 0.00008333333333333230*t - 0.666666666666632
      else if (t>26000 & t<= 39000) fpCO2 <- 0.00003461538461538460*t + 0.6
      else if (t>39000 & t<=52000) fpCO2 <- 0.00001538461538461540*t + 1.35
      else if (t>52000 & t<=66000) fpCO2 <- 2.15
      else if (t>66000 & t<= 77000) fpCO2 <- -0.00001363636363636150*t + 3.04999999999986
      else if (t>77000 & t<=220000) fpCO2 <- -0.00000699300699300713*t + 2.53846153846154
      else if (t>220000) fpCO2 <- 1
      }
   
    #Zeebe et al. 2019 3000+1480 Pg - Pfeed=0 - Z09 [LINE430]
    if(TRUE){
      if (t<=20000) fpCO2 <- 1
      else if (t>20000 & t<= 25036.8588010057) fpCO2 <- 0.00015299875615684300*t - 2.05997512314368
      else if (t>25036.8588010057 & t<= 34565.8265916157) fpCO2 <- -0.00002461296853543960*t + 2.38686454938011
      else if (t>34565.8265916157 & t<= 40047.5606840014) fpCO2 <- 0.00000204365304312974*t + 1.465456390377
      else if (t>40047.5606840014 & t<= 45977.0677089587) fpCO2 <- -0.00000436383147776813*t + 1.72206051555943
      else if (t>45977.0677089587 & t<= 52498.0125248455) fpCO2 <- -0.00000166559115181067*t + 1.59800333739778
      else if (t>52498.0125248455 & t<= 61923.7918451049) fpCO2 <- 0.00000034487981579566*t + 1.49245760735959
      else if (t>61923.7918451049 & t<= 81893.4064129795) fpCO2 <- 0.00000207376552009990*t + 1.38539844888227
      else if (t>81893.4064129795 & t<= 97228.4546068324) fpCO2 <- -0.00000332381931709524*t + 1.82742505760326
      else if (t>97228.4546068324 & t<= 220000) fpCO2 <- -0.00000082694625307134*t + 1.58465794823875
      else if (t>220000) fpCO2 <- 1
    }
    
    #LOSCAR new (Zeebe et al. 2009 + Zeebe & Lourens, 2019) Pf0 170kyr 3000+2100 Pg C - K170 [LINE446]
    if(FALSE){
      if (t<=20000) fpCO2 <- 1
      else if (t>20000 & t<= 25036.8588010057) fpCO2 <- 0.00015299875615684300*t - 2.05997512314368
      else if (t>25036.8588010057 & t<= 40121.6796409417) fpCO2 <- -0.00002019529864650300*t + 2.27625997214134
      else if (t>40121.6796409417 & t<= 58686.867178197) fpCO2 <- -0.00000308550086310506*t + 1.58978614675455
      else if (t>58686.867178197 & t<= 190000) fpCO2 <- 0.00000094849292340436*t + 1.353043689208
      else if (t>190000 & t<= 310000) fpCO2 <- -0.00000111652799651526*t + 1.74533313243434000000
      else if (t>310000) fpCO2 <- 1
    }

#SIMULATION SPECIFIC VALUES [ONE MUST BE TRUE]
    
#No CO2 forcing   
if(FALSE){
    ta <- 0
    tb <- 20000
    }
    
#F16
if(FALSE){
  ta <- 92613.7617
  tb <- 220000
  
  S1str_a <- 1.88551453454105000000
  S1str_b <- -0.0000317100373136852
  S1str_c <- 0.00000838096216282121
  S1str_d <- 0.67619243256424000000
  
  S3str_a <- 1.28707712174184000000
  S3str_b <- -0.0000126186925235985
  S3str_c <- 0.00000558730810854747
  S3str_d <- 0.11746162170949300000
}
    
#G17
if(FALSE){
  ta <- 66000
  tb <- 220000
  
  S1str_a <- 2.72133876837531000000
  S1str_b <- -0.0000500561976737836
  S1str_c <- 0.00000584415584415584
  S1str_d <- 0.28571428571428500000
  
  S3str_a <- 1.48942073323794000000
  S3str_b <- -0.0000199193637363947
  S3str_c <- 0.00000389610389610390
  S3str_d <- 0.14285714285714300000
}
    
#Z09
if(TRUE){
  ta <- 81893.4064129795
  tb <- 220000
  
  S1str_a <- 2.10443830158048000000
  S1str_b <- -0.0000372024295710953
  S1str_c <- 0.00000651670551437439
  S1str_d <- 0.43367521316236600000
  
  S3str_a <- 1.34458673137794000000
  S3str_b <- -0.0000148043351461425
  S3str_c <- 0.00000434447034291624
  S3str_d <- 0.04421652455842450000
}
    
#K170
if(FALSE){
  ta <- 190000
  tb <- 310000
  
  S1str_a <- 1.31113393742156000
  S1str_b <- -0.0000135446181941
  S1str_c <- 0.00000750000000000
  S1str_d <- 1.32500000000000000
  
  S3str_a <- 1.11382374363871000
  S3str_b <- -0.0000053899454816
  S3str_c <- 0.00000500000000000
  S3str_d <- -0.55000000000000000
}
    
    
#STRATIFICATION CONTROL (S1: 0.1; S2: 1; S3: 0.4)
    
stratonoff <- 1 #1:on, 0:off [SET TO 0 if fpCO2 <- 1]

  if (t<=20000) fstrat_S1 <- 1
  else if (t>20000 & t<=ta) fstrat_S1 <- S1str_a*exp(S1str_b*t)*stratonoff+1*(1-stratonoff)  
  else if (t>ta & t<=tb)  fstrat_S1 <- (S1str_c*t - S1str_d)*stratonoff+1*(1-stratonoff)
  else if (t>tb) fstrat_S1 <- 1
    
  if (t<=20000) fstrat_S2 <- 1
  else if (t>20000 & t<=ta) fstrat_S2 <- 1
  else if (t>ta & t<=tb)  fstrat_S2 <- 1
  else if (t>tb) fstrat_S2 <- 1
    
  if (t<=20000) fstrat_S3 <- 1
  else if (t>20000 & t<=ta) fstrat_S3 <- S3str_a*exp(S3str_b*t)*stratonoff+1*(1-stratonoff)
  else if (t>ta & t<=tb)  fstrat_S3 <- (S3str_c*t + S3str_d)*stratonoff+1*(1-stratonoff)
  else if (t>tb) fstrat_S3 <- 1
    
#OXYGEN CONTROL [1: variable [O2]; 0: constant [O2]]    
#Surface
  if (t<=20000) fO2S <- 1
  else if (t>20000 & t<=ta) fO2S <- 1
  else if (t>ta & t<=tb)  fO2S <- 1
  else if (t>tb) fO2S <- 1  
    
#Deep
  if (t<=20000) fO2D <- 1
  else if (t>20000 & t<=ta) fO2D <- 1
  else if (t>ta & t<=tb)  fO2D <- 1
  else if (t>tb) fO2D <- 1

#SURFACE DOA CONTROL
  if (t<=20000) fDOA <- 1
  else if (t>20000 & t<=ta) fDOA <- 1
  else if (t>ta & t<=tb)  fDOA <- 1
  else if (t>tb) fDOA <- 1

#MARGINAL PRODUCTIVITY CONTROL [1: variable PP; 0: constant PP]    
if (t<=20000) fPP <- 1
else if (t>20000 & t<=ta) fPP <- 1
else if (t>ta & t<=tb)  fPP <- 1
else if (t>tb) fPP <- 1

#OPEN OCEAN PRODUCTIVITY CONTROL [1: variable PP; 0: constant PP]    
  if (t<=20000) fPP_OO <- 1
  else if (t>20000 & t<=ta) fPP_OO <- 1
  else if (t>ta & t<=tb)  fPP_OO <- 1
  else if (t>tb) fPP_OO <- 1

#TEMPERATURE CONTROL [1:on, 0:off]    
Tonoff<-1 

#RIVERINE P CONTROL [1:on, 0:off]
  if (t<=20000) frivP <- 1
  else if (t>20000 & t<=ta) frivP <- 1
  else if (t>ta & t<=tb)  frivP <- 1
  else if (t>tb) frivP <- 1

#=============================================================================
# CALCULATIONS/FLUXES
#=============================================================================

pCO2 <- (Catm/q0)*fpCO2         #partial CO2 pressure - pre-PETM (uatm)

#TEMPERATURE CALCULATIONS
  Teq <-s*log(pCO2/pCO2_0)/log(2) #Equilibrium temperature component
    
  S1_T_eq <- S1_T_0+Teq*Tonoff  #Equilibrium temperature S1
  S2_T_eq <- S2_T_0+Teq*Tonoff  #Equilibrium temperature S2
  S3_T_eq <- S3_T_0+Teq*Tonoff  #Equilibrium temperature S3
  S4_T_eq <- S4_T_0+Teq*Tonoff  #Equilibrium temperature S4
  S5_T_eq <- S5_T_0+Teq*Tonoff  #Equilibrium temperature S5
  IM_T_eq <- IM_T_0+Teq*Tonoff  #Equilibrium temperature IM
  D1_T_eq <- D1_T_0+Teq*Tonoff  #Equilibrium temperature D1
  D2_T_eq <- D2_T_0+Teq*Tonoff  #Equilibrium temperature D2
  D3_T_eq <- D3_T_0+Teq*Tonoff  #Equilibrium temperature D3
  D4_T_eq <- D4_T_0+Teq*Tonoff  #Equilibrium temperature D4
  D5_T_eq <- D5_T_0+Teq*Tonoff  #Equilibrium temperature D5
    
  TS1 <- S1_T #Temperature in Celcius - Arc. surface
  TS2 <- S2_T #Temperature in Celcius - shelf
  TS3 <- S3_T #Temperature in Celcius - Epi. surface
  TS4 <- S4_T #Temperature in Celcius - low lat.
  TS5 <- S5_T #Temperature in Celcius - South. oc.
  TIM <- IM_T #Temperature in Celcius - thermocline
  TD1 <- D1_T #Temperature in Celcius - Arc. deep
  TD2 <- D2_T #Temperature in Celcius - Atl. deep
  TD3 <- D3_T #Temperature in Celcius - Indo. deep
  TD4 <- D4_T #Temperature in Celcius - Pac. deep
  TD5 <- D5_T #Temperature in Celcius - Epi. deep
    
#Kelvin - for O2
  TK_S1 <- TS1 + 274.15 #Temperature in Kelvin - Arc. surface
  TK_S2 <- TS2 + 274.15 #Temperature in Kelvin - shelf
  TK_S3 <- TS3 + 274.15 #Temperature in Kelvin - Epi. surface
  TK_S4 <- TS4 + 274.15 #Temperature in Kelvin - low lat.
  TK_S5 <- TS5 + 274.15 #Temperature in Kelvin - South. oc.
  TK_IM <- TIM + 274.15 #Temperature in Kelvin - thermocline
  TK_D1 <- TD1 + 274.15 #Temperature in Kelvin - Arc. deep
  TK_D2 <- TD2 + 274.15 #Temperature in Kelvin - Atl. deep
  TK_D3 <- TD3 + 274.15 #Temperature in Kelvin - Indo. deep
  TK_D4 <- TD4 + 274.15 #Temperature in Kelvin - Pac. deep
  TK_D5 <- TD5 + 274.15 #Temperature in Kelvin - Epi. deep
    
#CONCENTRATIONS

#Dissolved P (Tmol/Tm3)
  S1_DP_conc <- DP_S1/S1v  #Dissolved P concentr. Arc. surface 
  S2_DP_conc <- DP_S2/S2v  #Dissolved P concentr. shelf 
  S3_DP_conc <- DP_S3/S3v  #Dissolved P concentr. Epi. surface 
  S4_DP_conc <- DP_S4/S4v  #Dissolved P concentr. low lat. 
  S5_DP_conc <- DP_S5/S5v  #Dissolved P concentr. South. oc. 
  IM_DP_conc <- DP_IM/IMv  #Dissolved P concentr. thermocline 
  D1_DP_conc <- DP_D1/D1v  #Dissolved P concentr. Arc. deep 
  D2_DP_conc <- DP_D2/D2v  #Dissolved P concentr. Atl. deep 
  D3_DP_conc <- DP_D3/D3v  #Dissolved P concentr. Indo. deep 
  D4_DP_conc <- DP_D4/D4v  #Dissolved P concentr. Pac. deep 
  D5_DP_conc <- DP_D5/D5v  #Dissolved P concentr. Epi. deep 
    
#Steady state Dissolved O2
  S1_DO2_conc_0 <- 0.23401767854745400 #Steady state dissolved [O2] - Arc. surface
  S2_DO2_conc_0 <- 0.20319616794307600 #Steady state dissolved [O2] - shelf
  S3_DO2_conc_0 <- 0.20319616794307600 #Steady state dissolved [O2] - Epi. surface
  S4_DO2_conc_0 <- 0.20319616794307600 #Steady state dissolved [O2] - low lat.
  S5_DO2_conc_0 <- 0.25828567667029600 #Steady state dissolved [O2] - South. oc.
  IM_DO2_conc_0 <- 0.13520330089234900 #Steady state dissolved [O2] - thermocline
  D1_DO2_conc_0 <- 0.10074284327811300 #Steady state dissolved [O2] - Arc. deep
  D2_DO2_conc_0 <- 0.20428226560753200 #Steady state dissolved [O2] - Atl. deep
  D3_DO2_conc_0 <- 0.19163756448064100 #Steady state dissolved [O2] - Indo. deep
  D4_DO2_conc_0 <- 0.18182540160123400 #Steady state dissolved [O2] - Pac. deep
  D5_DO2_conc_0 <- 0.08520149279639700 #Steady state dissolved [O2] - Epi. deep			

#Dissolved surface O2 calculations [Weiss, 1970]  
    
  lno2_S1 <- A[1]+A[2]*100/TK_S1+A[3]*log(TK_S1/100)+A[4]*(TK_S1/100)+Sal*(B[1]+B[2]*TK_S1/100+B[3]*(TK_S1/100)^2) #Arc. surface
  lno2_S2 <- A[1]+A[2]*100/TK_S2+A[3]*log(TK_S2/100)+A[4]*(TK_S2/100)+Sal*(B[1]+B[2]*TK_S2/100+B[3]*(TK_S2/100)^2) #Shelf
  lno2_S3 <- A[1]+A[2]*100/TK_S3+A[3]*log(TK_S3/100)+A[4]*(TK_S3/100)+Sal*(B[1]+B[2]*TK_S3/100+B[3]*(TK_S3/100)^2) #Epi. surface
  lno2_S4 <- A[1]+A[2]*100/TK_S4+A[3]*log(TK_S4/100)+A[4]*(TK_S4/100)+Sal*(B[1]+B[2]*TK_S4/100+B[3]*(TK_S4/100)^2) #Low lat.
  lno2_S5 <- A[1]+A[2]*100/TK_S5+A[3]*log(TK_S5/100)+A[4]*(TK_S5/100)+Sal*(B[1]+B[2]*TK_S5/100+B[3]*(TK_S5/100)^2) #South. oc.
    
  S1_DO2_conc <- S1_DO2_conc_0*(1-fO2S)+fO2S*fstrat_S1*exp(lno2_S1)/22.4 #Arc. surface
  S2_DO2_conc <- S2_DO2_conc_0*(1-fO2S)+fO2S*fstrat_S2*exp(lno2_S2)/22.4 #Shelf
  S3_DO2_conc <- S3_DO2_conc_0*(1-fO2S)+fO2S*fstrat_S3*exp(lno2_S3)/22.4 #Epi. surface
  S4_DO2_conc <- S4_DO2_conc_0*(1-fO2S)+fO2S*exp(lno2_S4)/22.4           #Low lat.
  S5_DO2_conc <- S5_DO2_conc_0*(1-fO2S)+fO2S*exp(lno2_S5)/22.4           #South. oc.

#Dissolved deep O2 calculations   
  
  IM_DO2_conc <- IM_DO2_conc_0*(1-fO2D)+fO2D*DO2_IM/IMv #Thermocline
  D1_DO2_conc <- D1_DO2_conc_0*(1-fO2D)+fO2D*DO2_D1/D1v #Arc. deep 
  D2_DO2_conc <- D2_DO2_conc_0*(1-fO2D)+fO2D*DO2_D2/D2v #Atl. deep
  D3_DO2_conc <- D3_DO2_conc_0*(1-fO2D)+fO2D*DO2_D3/D3v #Indo. deep
  D4_DO2_conc <- D4_DO2_conc_0*(1-fO2D)+fO2D*DO2_D4/D4v #Pac. deep
  D5_DO2_conc <- D5_DO2_conc_0*(1-fO2D)+fO2D*DO2_D5/D5v #Epi. deep
    
#FLUXES 
    
#Water Cycle (Tm3/yr) 
    F1 <- 69.62941682013500000 #Arc. surface to Arc. deep
    F2 <- 69.62941682013500000 #Arc. deep to Arc. surface
    F3 <- 65.32000000000000000 #Arc. surface to Epi. surface
    F4 <- 62.82000000000000000 #Epi. surface to Arc. surface
    F5 <- 43.24869596080100000 #Arc. surface to low lat.
    F6 <- 43.24869596080100000 #Low lat. to Arc. surface
    F7 <- 153.9531261363630000 #Low lat. to Epi. surface
    F8 <- 153.9531261363630000 #Epi. surface to low lat.
    F9 <- 79.62941682013500000 #Epi. deep to Epi. surface
    F10 <- 79.6294168201350000 #Epi. surface to Epi. deep
    F11 <- 631.138520000000000 #Shelf to low lat.
    F12 <- 631.138520000000000 #Thermocline to shelf
    F13 <- 157.784630000000000 #Thermocline to low lat.
    F14 <- 3345.03415600000000 #Low lat. to thermocline
    F15 <- 3345.03415600000000 #Thermocline to low lat.
    F16 <- 157.784630000000000 #South. oc. to Atl. deep
    F17 <- 157.784630000000000 #Atl. deep to South. oc.
    F18 <- 157.784630000000000 #South. oc. to Indo. deep
    F19 <- 157.784630000000000 #Indo. deep to South. oc.
    F20 <- 252.455408000000000 #South. oc. to Pac. deep
    F21 <- 252.455408000000000 #Pac. deep to South. oc.
    F22 <- 131.487191666667000 #South. oc. to Atl. deep
    F23 <- 201.613693888889000 #South. oc. to Indo. deep
    F24 <- 455.822264444444000 #South. oc. to Pac. deep
    F25 <- 131.487191666667000 #Atl. deep to thermocline
    F26 <- 201.613693888889000 #Indo. deep to thermocline
    F27 <- 455.822264444444000 #Pac. deep to thermocline
    F28 <- 394.461575000000000 #Low lat. to South. oc.
    
    RF1 <- 5.000 #Arc. surface river influx
    RF2 <- 17.50 #Epi. surface river influx
    RF3 <- 27.50 #Shelf river influx
    
    EV1 <- 2.500000000000000 #Arc. surface evaporation
    EV2 <- 20.00000000000000 #Epi. surface evaporation
    EV3 <- 27.50000000000000 #Shelf evaporation
    EV4 <- 394.4615750000000 #Low lat. evaporation
    
    PR1 <- 394.4615750000000 #South. oc. precipitation
    
#Atmosphere (Tmol/yr)
    Fv <- Fs_0                      #Volcanic outgassing flux 
    Fs <- Fs_0*(pCO2/pCO2_0)^ns     #Silicate weathering flux 
    Fcc <- Fcc_0*(pCO2/pCO2_0)^ncc  #Carbonate weathering flux
    Fk <- 20.8333333333333          #Kerogen oxidation

#Primary Productivity (Tmol/yr)
    PPP_S1_0 <- 0.452830188679245000  #POP production Arc. surface [steady state]
    PPP_S2_0 <- 4.330188679245280000  #POP production shelf [steady state]
    PPP_S3_0 <- 1.719339622641510000  #POP production Epi. surface [steady state]
    PPP_S4_0 <- 29.63207547169810000  #POP production low lat. [steady state]
    PPP_S5_0 <- 1.556603773584910000  #POP production South. oc. [steady state]
    
    PPP_S1 <- PPP_S1_0*(1-fPP)+fPP*DP_S1*kbio_S1  #POP production Arc. surface
    PPP_S2 <- PPP_S2_0*(1-fPP)+fPP*DP_S2*kbio_S2  #POP production shelf 
    PPP_S3 <- PPP_S3_0*(1-fPP)+fPP*DP_S3*kbio_S3  #POP production Epi. surface 
    PPP_S4 <- PPP_S4_0*(1-fPP_OO)+fPP_OO*DP_S4*kbio_S4  #POP production low lat. 
    PPP_S5 <- PPP_S5_0*(1-fPP_OO)+fPP_OO*DP_S5*kbio_S5  #POP production South. oc. 
    
    OPP_S1 <- PPP_S1*RedfR  #POC production rate Arc. surface 
    OPP_S2 <- PPP_S2*RedfR  #POC production rate shelf
    OPP_S3 <- PPP_S3*RedfR  #POC production rate Epi. surface 
    OPP_S4 <- PPP_S4*RedfR  #POC production rate low lat. 
    OPP_S5 <- PPP_S5*RedfR  #POC production rate South. oc. 

#Export
    Exp_S1D1 <- OPP_S1*fExp_S1D1              #POC export Arc. surface to Arc. deep
    Exp_S2S4 <- OPP_S2*fExp_S2S4              #POC export shelf to low lat.
    Exp_S3D5 <- OPP_S3*fExp_S3D5              #POC export Epi. surface to Epi. deep
    Exp_S4IM <- OPP_S4*fExp_S4IM              #POC export low lat. to thermocline
    Exp_S5Dx <- OPP_S5*fExp_S5Dx              #POC export South. oc. to deep open ocean
    Exp_IMDx <- (Exp_S2S4+Exp_S4IM)*fExp_IMDx #POC export thermocline to deep open ocean
    
    Exp_P_S1D1 <- PPP_S1*fExp_P_S1D1              #POP export Arc. surface to Arc. deep
    Exp_P_S2S4 <- PPP_S2*fExp_P_S2S4              #POP export shelf to low lat.
    Exp_P_S3D5 <- PPP_S3*fExp_P_S3D5              #POP export Epi. surface to Epi. deep
    Exp_P_S4IM <- PPP_S4*fExp_P_S4IM              #POP export low lat. to thermocline
    Exp_P_S5Dx <- PPP_S5*fExp_P_S5Dx              #POP export South. oc. to deep open ocean
    Exp_P_IMDx <- (Exp_P_S2S4+Exp_P_S4IM)*fExp_P_IMDx #POP export thermocline to deep open ocean
    
#Surface Degree of Anoxia 
    DOA_S1 <- (1-(S1_DO2_conc/S1_DO2_conc_0)*(PPP_S1_0/PPP_S1))*fDOA #Arc. surface
    if (DOA_S1 <=0) DOA_S1 <- 0 
    else if (DOA_S1 >0) DOA_S1 <- DOA_S1
    
    DOA_S2 <- (1-(S2_DO2_conc/S2_DO2_conc_0)*(PPP_S2_0/PPP_S2))*fDOA #Shelf
    if (DOA_S2 <=0) DOA_S2 <- 0 
    else if(DOA_S2 >0) DOA_S2 <- DOA_S2
    
    DOA_S3 <- (1-(S3_DO2_conc/S3_DO2_conc_0)*(PPP_S3_0/PPP_S3))*fDOA #Epi. surface
    if (DOA_S3 <=0) DOA_S3 <- 0 
    else if (DOA_S3 >0) DOA_S3 <- DOA_S3

#Deep ocean oxygenation    
    DOO_D1 <- (D1_DO2_conc/D1_DO2_conc_0)           #Arc. deep
    if (DOO_D1 <=0) DOO_D1 <- 0 
    else if (DOO_D1 >0 & DOO_D1<=1) DOO_D1 <- DOO_D1 
    else if (DOO_D1 >1) DOO_D1 <- 1
    
    DOO_D2 <- (D2_DO2_conc/D2_DO2_conc_0)           #Atl. deep
    if (DOO_D2 <=0) DOO_D2 <- 0 
    else if (DOO_D2 >0 & DOO_D2<=1) DOO_D2 <- DOO_D2 
    else if (DOO_D2 >1) DOO_D2 <- 1
    
    DOO_D3 <- (D3_DO2_conc/D3_DO2_conc_0)           #Indo. deep
    if (DOO_D3 <=0) DOO_D3 <- 0 
    else if (DOO_D3 >0 & DOO_D3<=1) DOO_D3 <- DOO_D3 
    else if (DOO_D3 >1) DOO_D3 <- 1
    
    DOO_D4 <- (D4_DO2_conc/D4_DO2_conc_0)           #Pac. deep
    if (DOO_D4 <=0) DOO_D4 <- 0 
    else if (DOO_D4 >0 & DOO_D4<=1) DOO_D4 <- DOO_D4 
    else if (DOO_D4 >1) DOO_D4 <- 1
    
    DOO_D5 <- (D5_DO2_conc/D5_DO2_conc_0)           #Epi. deep
    if (DOO_D5 <=0) DOO_D5 <- 0 
    else if (DOO_D5 >0 & DOO_D5<=1) DOO_D5 <- DOO_D5 
    else if (DOO_D5 >1) DOO_D5 <- 1
    
#Burial   
    OPB_S1 <- PPP_S1*kOPB_S1*((1-fOPB)+fOPB*(1-DOA_S1))                         #POP burial Arc. surface 
    OPB_S2 <- PPP_S2*kOPB_S2*((1-fOPB)+fOPB*(1-DOA_S2))                         #POP burial shelf 
    OPB_S3 <- PPP_S3*kOPB_S3*((1-fOPB)+fOPB*(1-DOA_S3))                         #POP burial Epi. surface 
    OPB_D1 <- Exp_P_S1D1*kOPB_D1*((1-fOPB)+fOPB*DOO_D1)                         #POP burial Arc. deep 
    OPB_D2 <- (Exp_P_S5Dx+Exp_P_S2S4+Exp_P_S4IM)*kOPB_D2*((1-fOPB)+fOPB*DOO_D2) #POP burial Atl. deep 
    OPB_D3 <- (Exp_P_S5Dx+Exp_P_S2S4+Exp_P_S4IM)*kOPB_D3*((1-fOPB)+fOPB*DOO_D3) #POP burial Indo. deep 
    OPB_D4 <- (Exp_P_S5Dx+Exp_P_S2S4+Exp_P_S4IM)*kOPB_D4*((1-fOPB)+fOPB*DOO_D4) #POP burial Pac. deep 
    OPB_D5 <- Exp_P_S3D5*kOPB_D5*((1-fOPB)+fOPB*DOO_D5)                         #POP burial Epi. deep 
    
    OB_S1 <- OPB_S1*((S1_BR*RedfR_anox)/(RedfR_anox*(1-DOA_S1)+S1_BR*DOA_S1))   #POC burial Arc. surface 
    OB_S2 <- OPB_S2*((S2_BR*RedfR_anox)/(RedfR_anox*(1-DOA_S2)+S2_BR*DOA_S2))   #POC burial shelf 
    OB_S3 <- OPB_S3*((S3_BR*RedfR_anox)/(RedfR_anox*(1-DOA_S3)+S3_BR*DOA_S3))   #POC burial Epi. surface 
    OB_D1 <- OPB_D1*((D1_BR*RedfR_anox)/(RedfR_anox*(DOO_D1)+D1_BR*(1-DOO_D1))) #POC burial Arc. deep 
    OB_D2 <- OPB_D2*((D2_BR*RedfR_anox)/(RedfR_anox*(DOO_D2)+D2_BR*(1-DOO_D2))) #POC burial Atl. deep 
    OB_D3 <- OPB_D3*((D3_BR*RedfR_anox)/(RedfR_anox*(DOO_D3)+D3_BR*(1-DOO_D3))) #POC burial Indo. deep 
    OB_D4 <- OPB_D4*((D4_BR*RedfR_anox)/(RedfR_anox*(DOO_D4)+D4_BR*(1-DOO_D4))) #POC burial Pac. deep 
    OB_D5 <- OPB_D5*((D5_BR*RedfR_anox)/(RedfR_anox*(DOO_D5)+D5_BR*(1-DOO_D5))) #POC burial Epi. deep 
  
#Decay
    Dec_S1 <- OPP_S1-OB_S1-Exp_S1D1                #POC decay Arc. surface
    Dec_S2 <- OPP_S2-OB_S2-Exp_S2S4                #POC decay shelf
    Dec_S3 <- OPP_S3-OB_S3-Exp_S3D5                #POC decay Epi. surface
    Dec_S4 <- OPP_S4-Exp_S4IM                      #POC decay low lat.
    Dec_S5 <- OPP_S5-Exp_S5Dx                      #POC decay South. oc.
    Dec_IM <- (Exp_S4IM+Exp_S2S4)*tcpfract         #POC decay thermocline
    Dec_D1 <- Exp_S1D1-OB_D1                       #POC decay Arc. deep
    Dec_D2 <- (Exp_IMDx+Exp_S5Dx)*(D2a/DOa)-OB_D2  #POC decay Atl. deep
    Dec_D3 <- (Exp_IMDx+Exp_S5Dx)*(D3a/DOa)-OB_D3  #POC decay Indo. deep
    Dec_D4 <- (Exp_IMDx+Exp_S5Dx)*(D4a/DOa)-OB_D4  #POC decay Pac. deep
    Dec_D5 <- Exp_S3D5-OB_D5                       #POC decay Epi. deep
    
    Dec_P_S1 <- PPP_S1-OPB_S1-Exp_P_S1D1                 #POP decay Arc. surface
    Dec_P_S2 <- PPP_S2-OPB_S2-Exp_P_S2S4                 #POP decay shelf
    Dec_P_S3 <- PPP_S3-OPB_S3-Exp_P_S3D5                 #POP decay Epi. surface
    Dec_P_S4 <- PPP_S4-Exp_P_S4IM                        #POP decay low lat.
    Dec_P_S5 <- PPP_S5-Exp_P_S5Dx                        #POP decay South. oc.
    Dec_P_IM <- (Exp_P_S4IM+Exp_P_S2S4)*tcpfract         #POP decay thermocline
    Dec_P_D1 <- Exp_P_S1D1-OPB_D1                        #POP decay Arc. deep
    Dec_P_D2 <- (Exp_P_IMDx+Exp_P_S5Dx)*(D2a/DOa)-OPB_D2 #POP decay Atl. deep
    Dec_P_D3 <- (Exp_P_IMDx+Exp_P_S5Dx)*(D3a/DOa)-OPB_D3 #POP decay Indo. deep
    Dec_P_D4 <- (Exp_P_IMDx+Exp_P_S5Dx)*(D4a/DOa)-OPB_D4 #POP decay Pac. deep
    Dec_P_D5 <- Exp_P_S3D5-OPB_D5                        #POP decay Epi. deep
    
    Dec_O2_IM <- Dec_IM/RedfR_CO2 #POC decay O2 consumption thermocline
    Dec_O2_D1 <- Dec_D1/RedfR_CO2 #POC decay O2 consumption thermocline
    Dec_O2_D2 <- Dec_D2/RedfR_CO2 #POC decay O2 consumption thermocline
    Dec_O2_D3 <- Dec_D3/RedfR_CO2 #POC decay O2 consumption thermocline
    Dec_O2_D4 <- Dec_D4/RedfR_CO2 #POC decay O2 consumption thermocline
    Dec_O2_D5 <- Dec_D5/RedfR_CO2 #POC decay O2 consumption thermocline
    
#Inorganic Burial
    CaPB_S1 <- kauth_S1*Dec_P_S1*((1-fCaPB)+fCaPB*(1-DOA_S1)) #CaP burial Arc. surface 
    CaPB_S2 <- kauth_S2*Dec_P_S2*((1-fCaPB)+fCaPB*(1-DOA_S2)) #CaP burial shelf
    CaPB_S3 <- kauth_S3*Dec_P_S3*((1-fCaPB)+fCaPB*(1-DOA_S3)) #CaP burial Epi. surface 
    CaPB_D1 <- kauth_D1*Dec_P_D1*((1-fCaPB)+fCaPB*(DOO_D1))   #CaP burial Arc. deep 
    CaPB_D2 <- kauth_D2*Dec_P_D2*((1-fCaPB)+fCaPB*(DOO_D2))   #CaP burial Atl. deep 
    CaPB_D3 <- kauth_D3*Dec_P_D3*((1-fCaPB)+fCaPB*(DOO_D3))   #CaP burial Indo. deep 
    CaPB_D4 <- kauth_D4*Dec_P_D4*((1-fCaPB)+fCaPB*(DOO_D4))   #CaP burial Pac. deep 
    CaPB_D5 <- kauth_D5*Dec_P_D5*((1-fCaPB)+fCaPB*(DOO_D5))   #CaP burial Epi. deep 
    
    FePB_S1_0 <- 0.0017884367816092000 #Steady state FeP burial Arc. surface 
    FePB_S2_0 <- 0.0128767448275862000 #Steady state FeP burial shelf 
    FePB_S3_0 <- 0.0080479655172413800 #Steady state FeP burial Epi. surface 
    FePB_D1_0 <- 0.0002769230769230770 #Steady state FeP burial Arc. deep 
    FePB_D2_0 <- 0.0018048076923076900 #Steady state FeP burial Atl. deep 
    FePB_D3_0 <- 0.0027673717948717900 #Steady state FeP burial Indo. deep 
    FePB_D4_0 <- 0.0062566666666666700 #Steady state FeP burial Pac. deep 
    FePB_D5_0 <- 0.0002803846153846150 #Steady state FeP burial Epi. deep 
    
    FePB_S1 <- FePB_S1_0*(1-DOA_S1) #FeP burial Arc. surface 
    FePB_S2 <- FePB_S2_0*(1-DOA_S2) #FeP burial shelf
    FePB_S3 <- FePB_S3_0*(1-DOA_S3) #FeP burial Epi. surface 
    FePB_D1 <- FePB_D1_0*(DOO_D1)   #FeP burial Arc. deep 
    FePB_D2 <- FePB_D2_0*(DOO_D2)   #FeP burial Atl. deep 
    FePB_D3 <- FePB_D3_0*(DOO_D3)   #FeP burial Indo. deep 
    FePB_D4 <- FePB_D4_0*(DOO_D4)   #FeP burial Pac. deep 
    FePB_D5 <- FePB_D5_0*(DOO_D5)   #FeP burial Epi. deep 
   
#Dissolved exchange
    S1_wfp <- -S1_DP_conc*(F1+F3+F5)+F2*D1_DP_conc+F4*S3_DP_conc+F6*S4_DP_conc                                     #Dissolved P exchange - Arc. surface
    S2_wfp <- -S2_DP_conc*(F11)+IM_DP_conc*F12                                                                     #Dissolved P exchange - shelf
    S3_wfp <- -S3_DP_conc*(F4+F8+F10)+F3*S1_DP_conc+F7*S4_DP_conc+F9*D5_DP_conc                                    #Dissolved P exchange - Epi. surface
    S4_wfp <- -S4_DP_conc*(F6+F7+F14+F28)+F5*S1_DP_conc+F8*S3_DP_conc+F15*IM_DP_conc+F11*S2_DP_conc+IM_DP_conc*F13 #Dissolved P exchange - low lat.
    S5_wfp <- -S5_DP_conc*(F16+F18+F20+F22+F23+F24)+F17*D2_DP_conc+F19*D3_DP_conc+F21*D4_DP_conc+S4_DP_conc*F28    #Dissolved P exchange - South. oc.
    IM_wfp <- -IM_DP_conc*(F12+F13+F15)+F14*S4_DP_conc+F25*D2_DP_conc+F26*D3_DP_conc+F27*D4_DP_conc                #Dissolved P exchange - thermocline
    D1_wfp <- -D1_DP_conc*F2+F1*S1_DP_conc                                                                         #Dissolved P exchange - Arc. deep
    D2_wfp <- -D2_DP_conc*(F17+F25)+S5_DP_conc*(F16+F22)                                                           #Dissolved P exchange - Atl. deep
    D3_wfp <- -D3_DP_conc*(F19+F26)+S5_DP_conc*(F18+F23)                                                           #Dissolved P exchange - Indo. deep
    D4_wfp <- -D4_DP_conc*(F21+F27)+S5_DP_conc*(F20+F24)                                                           #Dissolved P exchange - Pac. deep
    D5_wfp <- -D5_DP_conc*F9+F10*S3_DP_conc                                                                        #Dissolved P exchange - Epi. deep
    
    IM_wfo <- -IM_DO2_conc*(F12+F13+F15)+F14*S4_DO2_conc+F25*D2_DO2_conc+F26*D3_DO2_conc+F27*D4_DO2_conc #Dissolved O2 exchange - thermocline
    D1_wfo <- -D1_DO2_conc*F2+F1*S1_DO2_conc                                                             #Dissolved O2 exchange - Arc. deep
    D2_wfo <- -D2_DO2_conc*(F17+F25)+S5_DO2_conc*(F16+F22)                                               #Dissolved O2 exchange - Atl. deep
    D3_wfo <- -D3_DO2_conc*(F19+F26)+S5_DO2_conc*(F18+F23)                                               #Dissolved O2 exchange - Indo. deep
    D4_wfo <- -D4_DO2_conc*(F21+F27)+S5_DO2_conc*(F20+F24)                                               #Dissolved O2 exchange - Pac. deep
    D5_wfo <- -D5_DO2_conc*F9+F10*S3_DO2_conc                                                            #Dissolved O2 exchange - Epi. deep 

#River input
    fpw_0 <- 0.136397203890363000                                   #Initial total P weathering and riverine input 
    fpw <- fpw_0*(((Fcc+Fs)/(Fcc_0+Fs_0))^np)*frivP+(1-frivP)*fpw_0 #pCO2 dependent P weathering and riverine input

    rP_S1 <- fpw*fR_S1  #Dissolved P input Arc. surface 
    rP_S2 <- fpw*fR_S2  #Dissolved P input shelf 
    rP_S3 <- fpw*fR_S3  #Dissolved P input Epi. surface.
                               
#Totals    

    PB_S1 <- OPB_S1 + CaPB_S1 + FePB_S1 #Total P burial - Arc. surf.
    PB_S2 <- OPB_S2 + CaPB_S2 + FePB_S2 #Total P burial - shelf
    PB_S3 <- OPB_S3 + CaPB_S3 + FePB_S3 #Total P burial - Epi. surf.
    PB_D1 <- OPB_D1 + CaPB_D1 + FePB_D1 #Total P burial - Arc. deep
    PB_D2 <- OPB_D2 + CaPB_D2 + FePB_D2 #Total P burial - Atl. deep
    PB_D3 <- OPB_D3 + CaPB_D3 + FePB_D3 #Total P burial - Indo. deep
    PB_D4 <- OPB_D4 + CaPB_D4 + FePB_D4 #Total P burial - Pac. deep
    PB_D5 <- OPB_D5 + CaPB_D5 + FePB_D5 #Total P burial - Epi. deep
    
    
    PPP_tot <- PPP_S1+PPP_S2+PPP_S3+PPP_S4+PPP_S5             #Total Prim. Prod. (Tmol P)
    OPP_tot <- OPP_S1+OPP_S2+PPP_S3+OPP_S4+OPP_S5             #Total Prim. Prod. (Tmol C)
    OB_tot <- OB_S1+OB_S2+OB_S3+OB_D1+OB_D2+OB_D3+OB_D4+OB_D5 #Total Corg burial   
    PB_tot <- PB_S1+PB_S2+PB_S3+PB_D1+PB_D2+PB_D3+PB_D4+PB_D5 #Total P burial
    
#Burial C/P ratios 
    
    CP_S1 <- OB_S1/OPB_S1 #CPorg - Arc. surf
    CP_S2 <- OB_S2/OPB_S2 #CPorg - shelf
    CP_S3 <- OB_S3/OPB_S3 #CPorg - Epi. surf
    CP_D1 <- OB_D1/OPB_D1 #CPorg - Arc. deep
    CP_D2 <- OB_D2/OPB_D2 #CPorg - Atl. deep
    CP_D3 <- OB_D3/OPB_D3 #CPorg - Indo. deep
    CP_D4 <- OB_D4/OPB_D4 #CPorg - Pac. deep
    CP_D5 <- OB_D5/OPB_D5 #CPorg - Epi. deep
    
    CP_S1_t <- OB_S1/PB_S1 #CPtot - Arc. surf
    CP_S2_t <- OB_S2/PB_S2 #CPtot - shelf
    CP_S3_t <- OB_S3/PB_S3 #CPtot - Epi. surf.
    CP_D1_t <- OB_D1/PB_D1 #CPtot - Arc. deep
    CP_D2_t <- OB_D2/PB_D2 #CPtot - Atl. deep
    CP_D3_t <- OB_D3/PB_D3 #CPtot - Indo. deep
    CP_D4_t <- OB_D4/PB_D4 #CPtot - Pac. deep
    CP_D5_t <- OB_D5/PB_D5 #CPtot - Epi. deep
    
    #-------------------------------------------------------------------------------
    
    #Mass Balances
    
    #Water cycle
  
    dS1v <- -(F1+F3+F5+EV1)+(F2+F4+F6+RF1)
    dS2v <- -(F11+EV3)+(F12+RF3)
    dS3v <- -(F4+F8+F10+EV2)+(F3+F7+F9+RF2)
    dS4v <- -(F6+F7+F14+F28+EV4)+(F5+F8+F15+F11+F13)
    dS5v <- -(F16+F18+F20+F22+F23+F24)+(F17+F19+F21+F28+PR1)
    dIMv <- -(F12+F13+F15)+(F14+F25+F26+F27)
    dD1v <- -(F2)+(F1)
    dD2v <- -(F17+F25)+(F16+F22)
    dD3v <- -(F19+F26)+(F18+F23)
    dD4v <- -(F21+F27)+(F20+F24)
    dD5v <- -(F9)+(F10)

    
    #Dissolved P cycle
    
    dDP_S1 <- S1_wfp+rP_S1+Dec_P_S1-PPP_S1-CaPB_S1-FePB_S1
    dDP_S2 <- S2_wfp+rP_S2+Dec_P_S2-PPP_S2-CaPB_S2-FePB_S2
    dDP_S3 <- S3_wfp+rP_S3+Dec_P_S3-PPP_S3-CaPB_S3-FePB_S3 
    dDP_S4 <- S4_wfp+Dec_P_S4-PPP_S4
    dDP_S5 <- S5_wfp+Dec_P_S5-PPP_S5
    dDP_IM <- IM_wfp+Dec_P_IM
    dDP_D1 <- D1_wfp+Dec_P_D1-CaPB_D1-FePB_D1
    dDP_D2 <- D2_wfp+Dec_P_D2-CaPB_D2-FePB_D2
    dDP_D3 <- D3_wfp+Dec_P_D3-CaPB_D3-FePB_D3
    dDP_D4 <- D4_wfp+Dec_P_D4-CaPB_D4-FePB_D4
    dDP_D5 <- D5_wfp+Dec_P_D5-CaPB_D5-FePB_D5
    
    
    #Dissolved O2 cycle
    
    dDO2_IM <- IM_wfo - Dec_O2_IM
    dDO2_D1 <- D1_wfo - Dec_O2_D1
    dDO2_D2 <- D2_wfo - Dec_O2_D2
    dDO2_D3 <- D3_wfo - Dec_O2_D3
    dDO2_D4 <- D4_wfo - Dec_O2_D4
    dDO2_D5 <- D5_wfo - Dec_O2_D5
    
    #Temperature
    
    dS1_T <- (S1_T_eq-TS1)/tlags
    dS2_T <- (S2_T_eq-TS2)/tlags
    dS3_T <- (S3_T_eq-TS3)/tlags
    dS4_T <- (S4_T_eq-TS4)/tlags
    dS5_T <- (S5_T_eq-TS5)/tlags
    dIM_T <- (IM_T_eq-TIM)/tlagim
    dD1_T <- (D1_T_eq-TD1)/tlagd
    dD2_T <- (D2_T_eq-TD2)/tlagd
    dD3_T <- (D3_T_eq-TD3)/tlagd
    dD4_T <- (D4_T_eq-TD4)/tlagd
    dD5_T <- (D5_T_eq-TD5)/tlagd
   
    
    
    # Output list with rates of change
    
    #TEST
    
      list(c(dS1v, dS2v, dS3v, dS4v, 
             dS5v, dIMv, dD1v, dD2v, dD3v,  dD4v, dD5v,
             dDP_S1, dDP_S2, dDP_S3, dDP_S4, dDP_S5, dDP_IM, dDP_D1, dDP_D2, dDP_D3, dDP_D4, dDP_D5, 
             dDO2_IM, dDO2_D1, dDO2_D2, dDO2_D3, dDO2_D4, dDO2_D5,
             dS1_T, dS2_T, dS3_T, dS4_T, dS5_T, dIM_T, dD1_T, dD2_T, dD3_T, dD4_T, dD5_T),
           c(DP_S1=DP_S1, DP_S2=DP_S2, DP_S3=DP_S3, DP_S4=DP_S4, DP_S5=DP_S5, #Surface P reservoir 
             DP_IM=DP_IM, #Thermocline P reservoir
             DP_D1=DP_D1, DP_D2=DP_D2, DP_D3=DP_D3, DP_D4=DP_D4, DP_D5=DP_D5, #Deep P reservoir
             S1_DP_conc=S1_DP_conc, S2_DP_conc=S2_DP_conc, S3_DP_conc=S3_DP_conc, S4_DP_conc=S4_DP_conc, S5_DP_conc=S5_DP_conc, #Surface P concent.
             IM_DP_conc=IM_DP_conc, #Thermocline P concent.
             D1_DP_conc=D1_DP_conc, D2_DP_conc=D2_DP_conc, D3_DP_conc=D3_DP_conc, D4_DP_conc=D4_DP_conc, D5_DP_conc=D5_DP_conc, #Deep P concent.
             PPP_S1=PPP_S1, PPP_S2=PPP_S2,PPP_S3=PPP_S3, PPP_S4=PPP_S4, PPP_S5=PPP_S5, #Prim. Prod. P 
             OPP_S1=OPP_S1, OPP_S2=OPP_S2, OPP_S3=OPP_S3, OPP_S4=OPP_S4, OPP_S5=OPP_S5,#Prim. Prod. C 
             PPP_tot=PPP_tot, OPP_tot=OPP_tot, #Prim. Prod. totals
             Exp_S1D1=Exp_S1D1, Exp_S2S4a=Exp_S2S4, Exp_S3D5a=Exp_S3D5,  Exp_S4IM=Exp_S4IM, Exp_IMDx=Exp_IMDx, Exp_S5Dx=Exp_S5Dx, #Export C
             Exp_P_S1D1=Exp_P_S1D1, Exp_P_S2S4=Exp_P_S2S4, Exp_P_S3D5=Exp_P_S3D5, Exp_P_S4IM=Exp_P_S4IM, Exp_P_IMDx=Exp_P_IMDx, Exp_P_S5Dx=Exp_P_S5Dx, #Export P
             Dec_S1=Dec_S1, Dec_S2=Dec_S2, Dec_S3=Dec_S3, Dec_S4=Dec_S4, Dec_S5=Dec_S5, #Surface decay C
             Dec_IM=Dec_IM, #Thermocline decay C
             Dec_D1=Dec_D1, Dec_D2=Dec_D2, Dec_D3=Dec_D3, Dec_D4=Dec_D4, Dec_D5=Dec_D5, #Deep decay C
             Dec_P_S1=Dec_P_S1, Dec_P_S2=Dec_P_S2, Dec_P_S3=Dec_P_S3, Dec_P_S4=Dec_P_S4, Dec_P_S5=Dec_P_S5, #Surface decay P
             Dec_P_IM=Dec_P_IM, #Thermocline decay P
             Dec_P_D1=Dec_P_D1, Dec_P_D2=Dec_P_D2, Dec_P_D3=Dec_P_D3, Dec_P_D4=Dec_P_D4, Dec_P_D5=Dec_P_D5, #Deep decay P
             Dec_O2_IM=Dec_O2_IM,Dec_O2_D2=Dec_O2_D2, Dec_O2_D3=Dec_O2_D3, Dec_O2_D4=Dec_O2_D4, #O2 consumption decay
             DOA_S1=DOA_S1, DOA_S2=DOA_S2, DOA_S3=DOA_S3, #Degree of Anoxia surface
             OPB_S1=OPB_S1, FePB_S1=FePB_S1, CaPB_S1=CaPB_S1, #P burial Arctic surface
             OPB_S2=OPB_S2, FePB_S2=FePB_S2, CaPB_S2=CaPB_S2, #P burial shelf
             OPB_S3=OPB_S3, FePB_S3=FePB_S3, CaPB_S3=CaPB_S3, #P burial Epi. seaway surface
             OPB_D1=OPB_D1, FePB_D1=FePB_D1, CaPB_D1=CaPB_D1, #P burial Arctic deep
             OPB_D2=OPB_D2, FePB_D2=FePB_D2, CaPB_D2=CaPB_D2, #P burial Atlantic
             OPB_D3=OPB_D3, FePB_D3=FePB_D3, CaPB_D3=CaPB_D3, #P burial Indotethys
             OPB_D4=OPB_D4, FePB_D4=FePB_D4, CaPB_D4=CaPB_D4, #P burial Pacific
             OPB_D5=OPB_D5, FePB_D5=FePB_D5, CaPB_D5=CaPB_D5, #P burial Epi. seaway deep
             OB_S1=OB_S1, OB_S2=OB_S2, OB_S3=OB_S3, OB_D1=OB_D1, OB_D2=OB_D2, OB_D3=OB_D3, OB_D4=OB_D4, OB_D5=OB_D5, #C burial
             OB_tot=OB_tot, PB_tot=PB_tot, #Burial totals
             CP_S3=CP_S3, CP_S1=CP_S1, CP_S2=CP_S2, CP_D5=CP_D5, CP_D1=CP_D1, CP_D2=CP_D2, CP_D3=CP_D3, CP_D4=CP_D4, #C/Porg ratios
             CP_S1_t=CP_S1_t, CP_S2_t=CP_S2_t, CP_S3_t=CP_S3_t, CP_D1_t=CP_D1_t,CP_D2_t=CP_D2_t,CP_D3_t=CP_D3_t,CP_D4_t=CP_D4_t,CP_D5_t=CP_D5_t, #C/Ptotal ratios
             rP_S1=rP_S1, rP_S2=rP_S2, rP_S3=rP_S3, #P riverine influx
             pCO2=pCO2, #pCO2 change
             TS1=TS1, TS2=TS2, TS3=TS3, TS4=TS4, TS5=TS5, TIM=TIM, TD1=TD1, TD2=TD2, TD3=TD3, TD4=TD4, TD5=TD5 #Temperature
             ))
   
    
  
    # PROBLEM-SPECIFIC CODE ENDS HERE#
    
    
  })
}  # end of model equations


#=============================================================================
# Model solution
#=============================================================================

#Reservoir volume (Tm3) at t=0

S1v_0 <- 600   #Arctic surface box volume 
S2v_0 <- 4050   #Shelf box volume 
S3v_0 <- 2025   #Epicontinental seaway box volume 
S4v_0 <- 47115  #Low lat. box volume 
S5v_0 <- 6980   #South. oc. box volume 

IMv_0 <- 282690     #Thermocline box volume 
D1v_0 <- 3114  #Arctic deep box volume 
D2v_0 <- 154000     #Atlantic deep box volume 
D3v_0 <- 160300     #Indotethys deep box volume 
D4v_0 <- 654700     #Pacific deep box volume 
D5v_0 <- 10125      #Epicontintental seaway deep box volume

OCv_0 <- 1325360  #Total ocean volume 
DOv_0 <- 969000      #Deep open ocean volume

#Dissolved P Reservoirs (Tmol) at t=0

DP_S1_0 <- 0.5263366637529890 #Dissolved P content Arctic surface
DP_S2_0 <- 4.6046737811227500 #Dissolved P content shelf
DP_S3_0 <- 1.8006655639512700 #Dissolved P content Epi. seaway surface
DP_S4_0 <- 37.040094339622600 #Dissolved P content low lat.
DP_S5_0 <- 3.7639237955987400 #Dissolved P content South. oc.
DP_IM_0 <- 369.06069736169500 #Dissolved P content thermocline
DP_D1_0 <- 5.7199017032666800 #Dissolved P content Arctic deep
DP_D2_0 <- 141.82170337838900 #Dissolved P content Atlantic deep
DP_D3_0 <- 161.94919905538000 #Dissolved P content Indo. deep
DP_D4_0 <- 706.83829621254000 #Dissolved P content Pacific deep
DP_D5_0 <- 17.605387179225800 #Dissolved P content Epi. seaway deep

#Dissolved O2 Reservoirs (Tmol) at t=0

DO2_IM_0 <- 38221.973162267000 #Dissolved O2 thermocline
DO2_D1_0 <- 313.71321396804400 #Dissolved O2 Arc. deep
DO2_D2_0 <- 31459.468903559900 #Dissolved O2 Atl. deep
DO2_D3_0 <- 30719.501586246700 #Dissolved O2 Indo. deep
DO2_D4_0 <- 119041.09042832800 #Dissolved O2 Pac. deep
DO2_D5_0 <- 862.66511456352000 #Dissolved O2 Epi. deep

#Water temperature (degrees C) at t=0
S1_T_0 <- 17 #Arc. surface
S2_T_0 <- 25 #Shelf
S3_T_0 <- 25 #Epi. surface
S4_T_0 <- 25 #Low lat.
S5_T_0 <- 12 #South. oc.
IM_T_0 <- 16 #Thermocline
D1_T_0 <- 12 #Arc. deep
D2_T_0 <- 12 #Atl. deep
D3_T_0 <- 12 #Indo. deep
D4_T_0 <- 12 #Pac. deep
D5_T_0 <- 12 #Epi. deep

state <- c(S1v      =S1v_0,
           S2v      =S2v_0,
           S3v      =S3v_0,
           S4v      =S4v_0,
           S5v      =S5v_0,
           IMv      =IMv_0,
           D1v      =D1v_0,
           D2v      =D2v_0,
           D3v      =D3v_0,
           D4v      =D4v_0,
           D5v      =D5v_0,
           DP_S1   =DP_S1_0,
           DP_S2   =DP_S2_0,
           DP_S3   =DP_S3_0,
           DP_S4   =DP_S4_0,
           DP_S5   =DP_S5_0,
           DP_IM   =DP_IM_0,
           DP_D1   =DP_D1_0,
           DP_D2   =DP_D2_0,
           DP_D3   =DP_D3_0,
           DP_D4   =DP_D4_0,
           DP_D5   =DP_D5_0,
           DO2_IM  =DO2_IM_0,
           DO2_D1  =DO2_D1_0,
           DO2_D2  =DO2_D2_0,
           DO2_D3  =DO2_D3_0,
           DO2_D4  =DO2_D4_0,
           DO2_D5  =DO2_D5_0,
           S1_T    =S1_T_0,
           S2_T    =S2_T_0,
           S3_T    =S3_T_0,
           S4_T    =S4_T_0,
           S5_T    =S5_T_0,
           IM_T    =IM_T_0,
           D1_T    =D1_T_0,
           D2_T    =D2_T_0,
           D3_T    =D3_T_0,
           D4_T    =D4_T_0,
           D5_T    =D5_T_0)




# Time sequence
t_start  <- 0         # year start
t_stop   <- 700000    # year stop
n_output <- 15000     # number of output times



time_seq <- seq(from = t_start, to = t_stop,len = n_output)

out <- as.data.frame(ode(y=state,times=time_seq,func=model,parms=parameters))



#=================================================================================
# Post-model excess cumulative Corg burial calculations (Ignore warnings)
#=================================================================================
time_a <- time_seq[430:15000]
time_b <- time_seq[431:15000]
time_c <- 21.3347556504

time_sub <- (time_b-time_a)

TOT_a <- out$OB_tot[430:15000]
TOT_b <- out$OB_tot[431:15000]
TOT_c <- out$OB_tot[429]

S1_a <- out$OB_S1[430:15000]
S1_b <- out$OB_S1[431:15000]
S1_c <- out$OB_S1[429]

S2_a <- out$OB_S2[430:15000]
S2_b <- out$OB_S2[431:15000]
S2_c <- out$OB_S2[429]

S3_a <- out$OB_S3[430:15000]
S3_b <- out$OB_S3[431:15000]
S3_c <- out$OB_S3[429]

D1_a <- out$OB_D1[430:15000]
D1_b <- out$OB_D1[431:15000]
D1_c <- out$OB_D1[429]

D2_a <- out$OB_D2[430:15000]
D2_b <- out$OB_D2[431:15000]
D2_c <- out$OB_D2[429]

D3_a <- out$OB_D3[430:15000]
D3_b <- out$OB_D3[431:15000]
D3_c <- out$OB_D3[429]

D4_a <- out$OB_D4[430:15000]
D4_b <- out$OB_D4[431:15000]
D4_c <- out$OB_D4[429]

D5_a <- out$OB_D5[430:15000]
D5_b <- out$OB_D5[431:15000]
D5_c <- out$OB_D5[429]

TOT_start <-time_c*(out$OB_tot[430]-TOT_c)
S1_start <-time_c*(out$OB_S1[430]-S1_c)
S2_start <-time_c*(out$OB_S2[430]-S2_c)
S3_start <-time_c*(out$OB_S3[430]-S3_c)
D1_start <-time_c*(out$OB_D1[430]-D1_c)
D2_start <-time_c*(out$OB_D2[430]-D2_c)
D3_start <-time_c*(out$OB_D3[430]-D3_c)
D4_start <-time_c*(out$OB_D4[430]-D4_c)
D5_start <-time_c*(out$OB_D5[430]-D5_c)

OB_sub_TOT <- (TOT_b-TOT_c)
OB_sub_S1 <- (S1_b-S1_c)
OB_sub_S2 <- (S2_b-S2_c)
OB_sub_S3 <- (S3_b-S3_c)
OB_sub_D1 <- (D1_b-D1_c)
OB_sub_D2 <- (D2_b-D2_c)
OB_sub_D3 <- (D3_b-D3_c)
OB_sub_D4 <- (D4_b-D4_c)
OB_sub_D5 <- (D5_b-D5_c)

time_OB_TOT <- time_sub*OB_sub_TOT
time_OB_S1 <- time_sub*OB_sub_S1
time_OB_S2 <- time_sub*OB_sub_S2
time_OB_S3 <- time_sub*OB_sub_S3
time_OB_D1 <- time_sub*OB_sub_D1
time_OB_D2 <- time_sub*OB_sub_D2
time_OB_D3 <- time_sub*OB_sub_D3
time_OB_D4 <- time_sub*OB_sub_D4
time_OB_D5 <- time_sub*OB_sub_D5


TOT_tot <- c(TOT_start, time_OB_TOT)
S1_tot <- c(S1_start, time_OB_S1)
S2_tot <- c(S2_start, time_OB_S2)
S3_tot <- c(S3_start, time_OB_S3)
D1_tot <- c(D1_start, time_OB_D1)
D2_tot <- c(D2_start, time_OB_D2)
D3_tot <- c(D3_start, time_OB_D3)
D4_tot <- c(D4_start, time_OB_D4)
D5_tot <- c(D5_start, time_OB_D5)


ExcCS_TOT <- cumsum(TOT_tot)
ExcCS_S1 <- cumsum(S1_tot)
ExcCS_S2 <- cumsum(S2_tot)
ExcCS_S3 <- cumsum(S3_tot)
ExcCS_D1 <- cumsum(D1_tot)
ExcCS_D2 <- cumsum(D2_tot)
ExcCS_D3 <- cumsum(D3_tot)
ExcCS_D4 <- cumsum(D4_tot)
ExcCS_D5 <- cumsum(D5_tot)

#=================================================================================
# PLOTS
#=================================================================================
if (TRUE){
  par(mfrow=c(2,3), oma=c(0,0,0,0))  
  plot (time_seq,out$DOA_S1 ,type="l",lwd=2,main="DOA_S1",xlim=c(t_start,2.2e5), xlab="time (year)", ylab="DOA")
  plot (time_seq,out$DOA_S2 ,type="l",lwd=2,main="DOA_S2",xlim=c(t_start,2.2e5), xlab="time (year)", ylab="DOA")
  plot (time_seq,out$DOA_S3 ,type="l",lwd=2,main="DOA_S3",xlim=c(t_start,2.2e5), xlab="time (year)", ylab="DOA")
  plot (time_seq,(out$OPP_tot*12*1e12/1e15) ,type="l",lwd=2,main="OPP_tot",xlim=c(t_start,2.2e5), xlab="time (year)", ylab="Total Productivity (Pg C yr-1)")
  plot (time_seq,(out$OB_tot*12*1e12/1e15) ,type="l",lwd=2,main="OB_tot",xlim=c(t_start,2.2e5), xlab="time (year)", ylab="Total Corg Burial (Pg C yr-1)")
  plot (time_seq,out$PB_tot ,type="l",lwd=2,main="PB_tot",xlim=c(t_start,2.2e5), xlab="time (year)", ylab="Total P burial (Tmol P yr-1)")
}

#EXCS out ===================
#F16
if (FALSE){
  t_main <- 1558
  t_40 <- 2415
  t_full <- 4286
}

#G17
if (FALSE){
  t_main <- 987
  t_40 <- 1844
  t_full <- 4286
}

#Z09
if (TRUE){
  t_main <- 1327
  t_40 <- 2184
  t_full <- 4286
}

#K170
if (FALSE){
  t_main <- 3644
  t_40 <- 4501
  t_full <- 6215
}

EXCSTOT_main <- ExcCS_TOT[t_main]*12*1e12/1e15
EXCSTOT_40 <- (ExcCS_TOT[t_40]*12*1e12/1e15)-EXCSTOT_main
EXCSTOT_full <- ExcCS_TOT[t_full]*12*1e12/1e15
relCPS1 <- max(out$CP_S1_t)/out$CP_S1_t[1]
relCPS2 <- max(out$CP_S2_t)/out$CP_S2_t[1] 
relCPS3 <- max(out$CP_S3_t)/out$CP_S3_t[1]

print(c(EXCSTOT_40, relCPS1, relCPS2, relCPS3))

#=================================================================================
# FILE SAVE [CHANGE DESTINATION FOLDER]
#=================================================================================
if(TRUE){
  Data_PP <- cbind(time_seq, 
                   out$DP_S1, out$DP_S2, out$DP_S3, out$DP_S4, out$DP_S5, out$DP_IM, out$DP_D1, out$DP_D2, out$DP_D3, out$DP_D4, out$DP_D5,
                   out$PPP_tot, out$PPP_S1, out$PPP_S2, out$PPP_S3, out$PPP_S4, out$PPP_S5,
                   out$OPP_tot, out$OPP_S1, out$OPP_S2, out$OPP_S3, out$OPP_S4, out$OPP_S5,
                   out$S1_DP_conc, out$S2_DP_conc, out$S3_DP_conc, out$S4_DP_conc, out$S5_DP_conc,
                   out$IM_DP_conc, out$D1_DP_conc, out$D2_DP_conc, out$D3_DP_conc, out$D4_DP_conc, out$D5_DP_conc
  )
  myfile_PP <- file.path("G:/WINTER-SPRING 2020 Modelling/OUTPUT/PP/mainrun", paste0("E", "_",  pCO2sc, "_", "PP", ".csv"))
  write.table(Data_PP,file=myfile_PP,sep=",",append=F,row.names=F,col.names=T)#Export data to file
  #====================================================================================================================================
  Data_Bur <- cbind(time_seq,
                    out$OPB_S1, out$OPB_S2, out$OPB_S3, out$OPB_D1, out$OPB_D2, out$OPB_D3, out$OPB_D4, out$OPB_D5, 
                    out$CaPB_S1, out$CaPB_S2, out$CaPB_S3, out$CaPB_D1, out$CaPB_D2, out$CaPB_D3, out$CaPB_D4, out$CaPB_D5, 
                    out$FePB_S1, out$FePB_S2, out$FePB_S3, out$FePB_D1, out$FePB_D2, out$FePB_D3, out$FePB_D4, out$FePB_D5, 
                    out$OB_S1, out$OB_S2, out$OB_S3, out$OB_D1, out$OB_D2, out$OB_D3, out$OB_D4, out$OB_D5, 
                    out$rP_S1, out$rP_S2, out$rP_S3
  )
  myfile_Bur <- file.path("G:/WINTER-SPRING 2020 Modelling/OUTPUT/Burial/mainrun", paste0("E", "_",  pCO2sc,"_", "Burial_Input",  ".csv"))
  write.table(Data_Bur,file=myfile_Bur,sep=",",append=F,row.names=F,col.names=T)#Export data to file
  #====================================================================================================================================
  Data_Dec <- cbind(time_seq,
                    out$Dec_S1, out$Dec_S2, out$Dec_S3, out$Dec_S4, out$Dec_S5, out$Dec_IM, out$Dec_D1, out$Dec_D2, out$Dec_D3, out$Dec_D4, out$Dec_D5,
                    out$Dec_P_S1, out$Dec_P_S2, out$Dec_P_S3, out$Dec_P_S4, out$Dec_P_S5, out$Dec_P_IM, out$Dec_P_D1, out$Dec_P_D2, out$Dec_P_D3, out$Dec_P_D4, out$Dec_P_D5,
                    out$Exp_S1D1, out$Exp_P_S1D1, out$Exp_S2S4a, out$Exp_P_S2S4, out$Exp_S3D5a, out$Exp_P_S3D5,
                    out$Exp_S4IM, out$Exp_P_S4IM, out$Exp_S5Dx, out$Exp_P_S5Dx, out$Exp_IMDx, out$Exp_P_IMDx
  )
  myfile_Dec <- file.path("G:/WINTER-SPRING 2020 Modelling/OUTPUT/Decay/mainrun", paste0("E", "_",  pCO2sc,"_", "Decay",  ".csv"))
  write.table(Data_Dec,file=myfile_Dec,sep=",",append=F,row.names=F,col.names=T)#Export data to file
  #====================================================================================================================================
  EXCS <- cbind(time_a, 
                out$CP_S1, out$CP_S2, out$CP_S3, out$CP_D1, out$CP_D2, out$CP_D3, out$CP_D4, out$CP_D5, #The CP data is plotted from time=0, delete rows with constant values
                ExcCS_TOT, ExcCS_S1, ExcCS_S2, ExcCS_S3, ExcCS_D1,ExcCS_D2, ExcCS_D3, ExcCS_D4, ExcCS_D5
  )
  myfilePE_check <- file.path("G:/WINTER-SPRING 2020 Modelling/OUTPUT/EXCS/mainrun", paste0("E", "_",  pCO2sc, "_", "Excs", ".csv"))
  write.table(EXCS,file=myfilePE_check,sep=",",append=F,row.names=F,col.names=T)#Export data to file 
  #====================================================================================================================================
  DOA <- cbind(time_seq, out$fRP,
               out$DOA_S1, out$DOA_S2, out$DOA_S3, out$DOO_D1,
               out$DO2_IM,  out$DO2_D1, out$DO2_D2, out$DO2_D3, out$DO2_D4, out$DO2_D5,
               out$S1_DO2_conc, out$S2_DO2_conc, out$S3_DO2_conc, out$S4_DO2_conc, out$S5_DO2_conc,
               out$IM_DO2_conc,
               out$D1_DO2_conc, out$D2_DO2_conc, out$D3_DO2_conc, out$D4_DO2_conc, out$D5_DO2_conc
  )
  myfilePE_check <- file.path("G:/WINTER-SPRING 2020 Modelling/OUTPUT/DOA/mainrun", paste0("E", "_",  pCO2sc, "_", "DOA", ".csv"))
  write.table(DOA,file=myfilePE_check,sep=",",append=F,row.names=F,col.names=T)#Export data to file
  #====================================================================================================================================
  Info <- cbind(EXCSTOT_40, relCPS1, relCPS2, relCPS3)
  myfilePE_check <- file.path("G:/WINTER-SPRING 2020 Modelling/OUTPUT/EXCS/mainrun", paste0("E", "_",  pCO2sc, "_", "DOA", ".txt"))
  write.table(Info,file=myfilePE_check,sep=",",append=F,row.names=F,col.names=T)#Export data to file
  #====================================================================================================================================
  Info2 <- cbind(time_seq, out$fstrat_S1)
  myfilePE_check <- file.path("G:/WINTER-SPRING 2020 Modelling/OUTPUT/EXCS/mainrun", paste0("E", "_",  pCO2sc, "_", "fstrat", ".txt"))
  write.table(Info2,file=myfilePE_check,sep=",",append=F,row.names=F,col.names=T)#Export data to file
  
}
#=================================================================================
# FIN
#=================================================================================
#Simulation is finished:
beep()