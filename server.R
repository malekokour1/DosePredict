
############ DosePredict

##############################################################################
########################################################Global variables  ####
##############################################################################
rm(list=ls())

##### Load Packages
library(mrgsolve) 
library(shiny)
library(deSolve)
library(magrittr) 
library(ggplot2)
library(PKNCA)
library(data.table) 
library(dplyr)
library(RColorBrewer)
library(shinythemes)
library(rmarkdown)
library(knitr)
library(plotly)
library(reshape2)
library(shinyBS)
#Misc
set.seed(1234)
cbPalette <- c(brewer.pal(8,"Set2"),brewer.pal(9,"Set1"),brewer.pal(12,"Set3"))


# please note in the blow models, PD indirect response model (with logistic growth model) is also incorporated for future development
# in this indirect response model, parameters  includes as TVIC50, TVEMAX, TVKG, and Nmax.
# In this indirect response model, the PK is assumed to affect (inhibit) the PD output.
# under the current version of the software, the indirect response model is not outputed.
# However, the user can modify the model or output to utilize the PD indirect response model.


####################################################################################################

# 1 comp PK-PD 
code1PK <- '
$PARAM TVCL = 17, TVVC = 4.12, TVKA = 0.5,
TVFBIO = 0.5,TVBP= 1,SCA= 1,
TVIC50=0.05, TVEMAX=.4, TVKG=0.076, Nmax=10000000

$CMT GUT CENT PD
$OMEGA  @labels ECL EVC EKA EBV EIC EKmax
0.09 0.09 0.09 0.09 0.09 0.09

$MAIN
double CL = TVCL*exp(ECL);
double V  =  TVVC*exp(EVC);
double KA =  TVKA*exp(EKA);;

double FBIO   =  TVFBIO*exp(EBV);
double IC50   =  TVIC50*exp(EIC);
double EMAX   =  TVEMAX*exp(EKmax);
double KG     =  TVKG;

double S2 = V/SCA;
double BP = TVBP;

F_GUT     = FBIO;
double KE = CL/V;

PD_0   = 100000000;

$ODE
double CP = CENT/S2;
double CPLASMA = CENT/S2;
double CBLOOD = BP*(CENT/S2);

double INH = (EMAX*CPLASMA)/(IC50+CPLASMA);

dxdt_GUT=-KA*GUT;
dxdt_CENT = KA*GUT -KE*CENT;
dxdt_PD = KG*PD*(1-PD/Nmax) - INH*PD;

$TABLE
double conc = CENT/V;
$CAPTURE CL V KA  FBIO ECL EVC EKA EBV CP CPLASMA CBLOOD INH IC50
'

# 2 comp PKPD
code2PK <- '
$PARAM TVCL = 17, TVVC = 4.12, TVKA = 0.5,
TVQ1 = 2.9 , TVVP1  = 56.4,
TVFBIO = 0.5,TVBP= 1,SCA= 1,
TVIC50=0.05, TVEMAX=.4, TVKG=0.076, Nmax=10000000

$CMT GUT CENT PERI1 PD
$OMEGA  @labels ECL EVC EKA EBV EIC EKmax
0.09 0.09 0.09 0.09 0.09 0.09

$MAIN
double CL  =  TVCL*exp(ECL);
double V   =  TVVC*exp(EVC);
double KA  =  TVKA*exp(EKA);
double Q1  =  TVQ1;
double VP1 =  TVVP1;

double FBIO  =  TVFBIO*exp(EBV);
double IC50  =  TVIC50*exp(EIC);
double EMAX  =  TVEMAX*exp(EKmax);
double KG    =  TVKG;

double S2    =  V/SCA;
double BP    =  TVBP;

F_GUT      = FBIO;
double KE  = CL/V;
double K23 = Q1/V;
double K32 = Q1/VP1;

PD_0=100000000;

$ODE
double CP = CENT/S2;
double CPLASMA = CENT/S2;
double CBLOOD = BP*(CENT/S2);

double INH = (EMAX*CPLASMA)/(IC50+CPLASMA);

dxdt_GUT=-KA*GUT;
dxdt_CENT = KA*GUT -KE*CENT+ K32*PERI1-K23*CENT; 
dxdt_PERI1= - K32*PERI1+K23*CENT;
dxdt_PD = KG*PD*(1-PD/Nmax) - INH*PD;

$TABLE
double conc = CENT/V;
$CAPTURE CL V KA Q1 VP1 FBIO ECL EVC EKA EBV CP CPLASMA CBLOOD INH IC50
'

# 3 comp PDPK 
code3PK <- '
$PARAM TVCL = 17, TVVC = 4.12, TVKA = 0.5,
TVQ1 = 2.9 , TVVP1  = 56.4,
TVQ2  = 3.52, TVVP2 = 9.9,
TVFBIO = 0.5,TVBP= 1,SCA= 1,
TVIC50=0.05, TVEMAX=.4, TVKG=0.076, Nmax=10000000

$CMT GUT CENT PERI1 PERI2 PD
$OMEGA @labels ECL EVC EKA EBV EIC EKmax
0.09 0.09 0.09 0.09 0.09 0.09

$MAIN
double CL  = TVCL*exp(ECL);
double V   =  TVVC*exp(EVC);
double KA  =  TVKA*exp(EKA);
double Q1  = TVQ1;
double VP1 =  TVVP1;
double Q2  = TVQ2;
double VP2 =  TVVP2;

double FBIO  =  TVFBIO*exp(EBV);
double IC50  =  TVIC50*exp(EIC);
double EMAX  =  TVEMAX*exp(EKmax);
double KG    =  TVKG;

double S2 = V/SCA;
double BP = TVBP;

F_GUT      = FBIO;
double KE  = CL/V;
double K23 = Q1/V;
double K32 = Q1/VP1;
double K24 = Q2/V;
double K42 = Q2/VP2;

PD_0=100000000;


$ODE
double CP = CENT/S2;
double CPLASMA = CENT/S2;
double CBLOOD = BP*(CENT/S2);

double INH = (EMAX*CPLASMA)/(IC50+CPLASMA);

dxdt_GUT=-KA*GUT;
dxdt_CENT = KA*GUT -KE*CENT+ K32*PERI1-K23*CENT + K42*PERI2-K24*CENT;
dxdt_PERI1= - K32*PERI1+K23*CENT;
dxdt_PERI2= - K42*PERI2+K24*CENT;
dxdt_PD = KG*PD*(1-PD/Nmax) - INH*PD;

$TABLE
double conc = CENT/V;
$CAPTURE CL V KA Q1 VP1 Q2 VP2 FBIO ECL EVC EKA EBV CP CPLASMA CBLOOD INH IC50
'

#####################################################################################################################


server = function(input, output) {
  
##############################################################################
################################################################## 1-Simulate ##
##############################################################################

sim.data <- eventReactive(input$go,{
  
  # TEST input
  IDMAXIN<-3 
  SimTime<-72
  DOSEIN<- c(10,50,300)
  IIIN<-12
  ADDLIN<-2
  CLIN <- 10
  VCIN<- 50
  KAIN<-0.5
  FBIOIN<-80
  FBIOIN <- (FBIOIN)/100
  SCAIN<-1
  BPIN<-1
  IC50<-0.02
  Kmax<-0.4
  KG<-0.2
  ECL<-0.24
  EVC<-0.04
  EKA<-0.24
  EBV<-0.04
  EIC<-0.04
  EKmax<-0.04
  Q1IN<-4
  V2IN<-50
  Q2IN<-2
  V3IN<-11
  IIIN<-24 
  ADDLIN<-2
  
  
################# SHINY INPUT:
  # GENERAL:
  IDMAXIN<-input$IDmax 
  SimTime<-(input$SimTime)
  #DOSING:
  DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
  IIIN<-input$II 
  ADDLIN<-input$ADDL 
  #PK:
  CLIN<-input$CL 
  VCIN<- input$VC
  KAIN<-input$KA
  FBIOIN<-input$FBIO
  FBIOIN <- (FBIOIN)/100
  SCAIN<-input$SCALE
  BPIN<-input$BP
  #PD: fixed for now, in next version of the software is planned to incude PD models
  IC50<-0.05
  Kmax<-0.6
  KG<-0.02
  #EIC<-input$EIC
  EIC<-0
  #EKmax<-input$EKmax
  EKmax<-0

#selected model:
selectPKmod <- input$PKmodel  

# Simulations # KEEP amt the first column
if (selectPKmod == 1) {
  #PK:
  CLIN<-input$CL1 
  VCIN<- input$VC1
  KAIN<-input$KA1
  FBIOIN<-input$FBIO1
  FBIOIN <- (FBIOIN)/100
  SCAIN<-input$SCALE1
  BPIN<-input$BP1
  ECL<-input$ECL1
  ECL<-(ECL/100)^2
  EVC<-input$EVC1
  EVC<-(EVC/100)^2
  EKA<-input$EKA1
  EKA<-(EKA/100)^2
  EBV<-input$EBV1
  EBV<-(EBV/100)^2
  
  
  metadata <- expand.ev(amt=DOSEIN,ID=1:IDMAXIN, 
                        TVCL = CLIN,TVVC=VCIN,TVKA=KAIN,
                        TVFBIO = FBIOIN,
                        TVIC50=IC50, TVEMAX=Kmax, TVKG=KG,
                        SCA =SCAIN,TVBP=BPIN,
                        ii=IIIN, addl=ADDLIN)  
 
  mod1PK <- mcode("pk1", code1PK)
  
  out <- 
    mod1PK %>%
    #drop.re %>% 
    omat(dmat(ECL,EVC,EKA,EBV,EIC,EKmax))%>% 
    data_set(metadata) %>%
    carry.out(amt)%>%
    mrgsim(end=SimTime,delta=.2)
  plot(out)
}

if (selectPKmod == 2) {
  #PK:
  CLIN<-input$CL2
  VCIN<- input$VC2
  KAIN<-input$KA2
  FBIOIN<-input$FBIO2
  FBIOIN <- (FBIOIN)/100
  SCAIN<-input$SCALE2
  BPIN<-input$BP2
  ECL<-input$ECL2
  ECL<-(ECL/100)^2
  EVC<-input$EVC2
  EVC<-(EVC/100)^2
  EKA<-input$EKA2
  EKA<-(EKA/100)^2
  EBV<-input$EBV2
  EBV<-(EBV/100)^2
  Q1IN<-input$Q12
  V2IN<-input$V22
  metadata <- expand.ev(amt=DOSEIN,ID=1:IDMAXIN, 
                        TVCL = CLIN,TVVC=VCIN,TVKA=KAIN,
                        TVQ1 = Q1IN , TVVP1  = V2IN,
                        TVFBIO = FBIOIN,
                        TVIC50=IC50, TVEMAX=Kmax, TVKG=KG,
                        SCA =SCAIN,TVBP=BPIN,
                        ii=IIIN, addl=ADDLIN)  
  mod2PK <- mcode("pk2", code2PK)
  out <- 
    mod2PK %>%
    #drop.re %>% 
    omat(dmat(ECL,EVC,EKA,EBV,EIC,EKmax))%>% 
    data_set(metadata) %>%
    carry.out(amt)%>%
    mrgsim(end=SimTime,delta=.2)
}

if (selectPKmod == 3) {
  #PK:
  CLIN<-input$CL3 
  VCIN<- input$VC3
  KAIN<-input$KA3
  FBIOIN<-input$FBIO3
  FBIOIN <- (FBIOIN)/100
  SCAIN<-input$SCALE3
  BPIN<-input$BP3
  ECL<-input$ECL3
  ECL<-(ECL/100)^2
  EVC<-input$EVC3
  EVC<-(EVC/100)^2
  EKA<-input$EKA3
  EKA<-(EKA/100)^2
  EBV<-input$EBV3
  EBV<-(EBV/100)^2
  Q1IN<-input$Q13
  V2IN<-input$V23
  Q2IN<-input$Q23
  V3IN<-input$V33
  
  
  metadata <- expand.ev(amt=DOSEIN,ID=1:IDMAXIN, 
                        TVCL = CLIN,TVVC=VCIN,TVKA=KAIN,
                        TVQ1 = Q1IN , TVVP1  = V2IN,
                        TVQ2  = Q2IN, TVVP2 = V3IN,
                        TVFBIO = FBIOIN,
                        TVIC50=IC50, TVEMAX=Kmax, TVKG=KG,
                        SCA =SCAIN,TVBP=BPIN,
                        ii=IIIN, addl=ADDLIN)  
  mod3PK <- mcode("pk3", code3PK)
   out <- 
    mod3PK %>%
    #drop.re %>% 
    omat(dmat(ECL,EVC,EKA,EBV,EIC,EKmax))%>% 
    data_set(metadata) %>%
    carry.out(amt)%>%
    mrgsim(end=SimTime,delta=.2)
}

if (selectPKmod == 4) {
  #PK:
  CLIN<-input$CL4 
  VCIN<- input$VC4
  KAIN<-input$KA1
  FBIOIN<-input$FBIO1
  FBIOIN <- (FBIOIN)/100
  SCAIN<-input$SCALE4
  BPIN<-input$BP4
  ECL<-input$ECL4
  ECL<-(ECL/100)^2
  EVC<-input$EVC4
  EVC<-(EVC/100)^2
  EKA<-input$EKA1
  EKA<-(EKA/100)^2
  EBV<-input$EBV1
  EBV<-(EBV/100)^2
  
  metadata <- expand.ev(amt=DOSEIN,ID=1:IDMAXIN, 
                        TVCL = CLIN,TVVC=VCIN,TVKA=KAIN,
                        TVFBIO = FBIOIN,
                        TVIC50=IC50, TVEMAX=Kmax, TVKG=KG,
                        SCA =SCAIN,TVBP=BPIN,
                        ii=IIIN, addl=ADDLIN,cmt=2)  

  mod1PK <- mcode("pk1", code1PK)
  
  out <- 
    mod1PK %>%
    #drop.re %>% 
    omat(dmat(ECL,EVC,EKA,EBV,EIC,EKmax))%>% 
    data_set(metadata) %>%
    carry.out(amt)%>%
    mrgsim(end=SimTime,delta=.2)
}

if (selectPKmod == 5) {
  #PK:
  CLIN<-input$CL5
  VCIN<- input$VC5
  KAIN<-input$KA2
  FBIOIN<-input$FBIO2
  FBIOIN <- (FBIOIN)/100
  SCAIN<-input$SCALE5
  BPIN<-input$BP5
  ECL<-input$ECL5
  ECL<-(ECL/100)^2
  EVC<-input$EVC5
  EVC<-(EVC/100)^2
  EKA<-input$EKA2
  EKA<-(EKA/100)^2
  EBV<-input$EBV2
  EBV<-(EBV/100)^2
  Q1IN<-input$Q15
  V2IN<-input$V25
  metadata <- expand.ev(amt=DOSEIN,ID=1:IDMAXIN, 
                        TVCL = CLIN,TVVC=VCIN,TVKA=KAIN,
                        TVQ1 = Q1IN , TVVP1  = V2IN,
                        TVFBIO = FBIOIN,
                        TVIC50=IC50, TVEMAX=Kmax, TVKG=KG,
                        SCA =SCAIN,TVBP=BPIN,
                        ii=IIIN, addl=ADDLIN,cmt=2)  
  mod2PK <- mcode("pk2", code2PK)
  out <- 
    mod2PK %>%
    #drop.re %>% 
    omat(dmat(ECL,EVC,EKA,EBV,EIC,EKmax))%>% 
    data_set(metadata) %>%
    carry.out(amt)%>%
    mrgsim(end=SimTime,delta=.2)
}

if (selectPKmod == 6) {
  #PK:
  CLIN<-input$CL6 
  VCIN<- input$VC6
  KAIN<-input$KA3
  FBIOIN<-input$FBIO3
  FBIOIN <- (FBIOIN)/100
  SCAIN<-input$SCALE6
  BPIN<-input$BP6
  ECL<-input$ECL6
  ECL<-(ECL/100)^2
  EVC<-input$EVC6
  EVC<-(EVC/100)^2
  EKA<-input$EKA3
  EKA<-(EKA/100)^2
  EBV<-input$EBV3
  EBV<-(EBV/100)^2
  Q1IN<-input$Q16
  V2IN<-input$V26
  Q2IN<-input$Q26
  V3IN<-input$V36
  
  
  metadata <- expand.ev(amt=DOSEIN,ID=1:IDMAXIN, 
                        TVCL = CLIN,TVVC=VCIN,TVKA=KAIN,
                        TVQ1 = Q1IN , TVVP1  = V2IN,
                        TVQ2  = Q2IN, TVVP2 = V3IN,
                        TVFBIO = FBIOIN,
                        TVIC50=IC50, TVEMAX=Kmax, TVKG=KG,
                        SCA =SCAIN,TVBP=BPIN,
                        ii=IIIN, addl=ADDLIN,cmt=2)  
  mod3PK <- mcode("pk3", code3PK)
  out <- 
    mod3PK %>%
    #drop.re %>% 
    omat(dmat(ECL,EVC,EKA,EBV,EIC,EKmax))%>% 
    data_set(metadata) %>%
    carry.out(amt)%>%
    mrgsim(end=SimTime,delta=.2)
}




#test plot
plot(out, CPLASMA+ CBLOOD~.)

# sim dataset
dataset<-as.data.frame(out)

#dataset<- dataset[ which(dataset$CPLASMA >=10E-7 ),] # to exclude very small simuliations
#dataset$CPLASMA[ which(dataset$CPLASMA <=10E-7 )] <-NA
})


############################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### ######### 
#### #### 5555 #### #### ####  PLOT: Generate the  PK  spaghetti plot #### #### #### ###### 
#### #### #### #### #### ####                             #### #### #### #### #### ######### 
############################################################################################
PLOTConc <- eventReactive(input$go, {
  
  # SHINY INPUT:
  # GENERAL:
  IDMAXIN<-input$IDmax 
  SimTime<-(input$SimTime)
  DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
  Xaxis<-input$Xaxis 
  effLimit<-input$effConcLim
  CmaxLimit<-input$cmaxSafinput
  
  

  # dataset PREP:
  DoseData <- expand.ev(amt=DOSEIN, ID=1:IDMAXIN)
  DoseData<-DoseData[,c(1,2)]
  
  plot_data<-sim.data()
  #plot_data<-dataset
  plot_data<-plot_data[-3]
  AllPlotData<-merge(plot_data,DoseData,by="ID",all.x=T)
  
  # clean a bit for IV models
  #selected model:
  selectPKmod <- input$PKmodel  
  if (selectPKmod == 4|selectPKmod == 5|selectPKmod == 6) {
    
    AllPlotData<-AllPlotData[-which(AllPlotData$time==0),]  
        }
  
  
  # Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
  #probs <- c(0.025,0.05, 0.1, 0.25, 0.5, 0.75, 0.9,0.95,0.975)
  probs <- c(0.05,0.95)
  lowerLim<-probs[1]
  upperLim<-probs[2]

  
  
  AllPlotData%>%
    group_by(amt,time)%>%
    summarise(
      Mean = mean(CPLASMA),
      lowerQuant=quantile(CPLASMA,probs =lowerLim),
      upperQuant=quantile(CPLASMA,probs =upperLim),
      GeoMeanCP = gm_mean(CPLASMA),
      meanCB = mean(CBLOOD),
      GeoMeanCB = gm_mean(CBLOOD)
    ) -> SummaryConcData

  SummaryConcData = SummaryConcData %>% rename( Dose=amt , Time=time, q5PI = lowerQuant ,q95PI = upperQuant )
  SummaryConcData$Dose<-as.factor(SummaryConcData$Dose)
  
  plotobj<-ggplot (SummaryConcData) +
    geom_line (aes(x=Time, y=Mean, color=Dose),alpha = 1,size=1.5)+
    xlab('Time') +
    ylab('Concentrations')+
    theme_bw()+
    ggtitle("Means Only PK Plot") + 
    scale_color_manual(name="Doses:",values=cbPalette)+
    theme( axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           # panel.border = element_blank(),
           panel.background = element_blank(),
           axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"),
           legend.position="top")+ 
    guides(colour = guide_legend(override.aes = list(size=3)))+
    scale_x_continuous(breaks=seq(0, SimTime, Xaxis))
    
  
  plotobj

 
  if (input$logPlot) {
   plotobj<-plotobj+scale_y_log10()
   plotobj
  }

  if (input$IndLines) {
     AllPlotData2 = AllPlotData %>% rename( Time=time, Conc = CPLASMA , Dose=amt  )
     AllPlotData2$ID<-as.factor(AllPlotData2$ID)
     AllPlotData2$Dose<-as.factor(AllPlotData2$Dose)
     plotobj<-plotobj+
       geom_line (data=AllPlotData2,aes(x=Time, y=Conc, group=ID,color=Dose),alpha = .2,size=1) +
       ggtitle("Means and Individual Profiles PK Plot")
     plotobj
   }

   if (input$PredInterval) {
     plotobj<-plotobj+
       geom_ribbon(data=SummaryConcData, aes(x=Time, ymin=q5PI, ymax=q95PI,color=Dose), linetype=2, alpha=0.1)+
       ggtitle("Means and Prediction Intervals PK Plot")
     plotobj
   }
  
  
  if (input$LimitsLines) {
    plotobj<-plotobj+
      geom_hline(aes(yintercept= effLimit), color="green", linetype="dashed",size=1 )+
      geom_hline(aes(yintercept=CmaxLimit ), color="red",size=1.3  )+
      annotate("text", x = (SimTime- (1/5)*SimTime), y = effLimit , label = paste0("Efficacy limit=",effLimit), colour = "black", size = 5)+
      annotate("text", x = (SimTime- (1/5)*SimTime), y = CmaxLimit , label = paste0("Safety Cmax=",CmaxLimit), colour = "black", size = 5)
    plotobj
  }
  
  plotobj
  
})

 output$PLOTConc2 <- 
   renderPlot({
     PLOTConc()
  
   })

output$trendPlot <- renderPlotly({
  ggplotly(PLOTConc()) %>% 
    layout(autosize=TRUE)
})

########################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### #### 
#### #### #### #### #### ####   Generate the NCA data     #### #### #### #### #### #### 
#### #### #### #### #### ####                             #### #### #### #### #### #### 
########################################################################################
NCA.data <- eventReactive(input$go, {
  IDMAXIN<-input$IDmax 
  DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
  SimTime<-(input$SimTime)
  
  
  
  NCAfrom<-input$NCAfrom
  #NCAfrom<-0
  NCAto<-input$NCAto
  #NCAto<-12
  
  
NCAdata<-sim.data()

#1 Subset the concentration-time data
  #NCAdata<-dataset
  d.conc<-NCAdata
  d.conc<-NCAdata[which(NCAdata$amt==0),]
  
  # to remove all negative values!
  d.conc<- d.conc[ which(d.conc$CPLASMA >=0 ),]

#2 create the dose data
  IDMAXIN<-input$IDmax 
  #DOSEIN<- c(10,50,300)
  DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
  
  
  d.dose<-expand.ev(Dose=DOSEIN, ID=1:IDMAXIN, time=0 )
  
  
#3 Put your concentration data into a PKNCAconc object
  myconc <- PKNCAconc(data=d.conc,
                      formula=CP~time|ID)
#4 Put your dose data into a PKNCAdose object
  mydose <- PKNCAdose(data=d.dose,
                      formula=Dose~time|ID)
  
  

  
#5 Specify the NCA params. start and end are for the general NCA, to the end of Simtime!!
  m.intervals<-data.frame(
                  start=c(NCAfrom),
                  end= c(NCAto),
                  auclast=T,
                  cmax=T,
                  aucinf.obs=T,
                  tmax=T,
                  half.life=T)
  
# 6 calculate the NCA  
  my.data.manual<-PKNCAdata(myconc, mydose, intervals=m.intervals) 
  m.results<-pk.nca(my.data.manual)
  
  results<-as.data.frame(m.results$result)

  
  
  
######7 cast results # may need them later
 # casted <- cast(results, ID+start+end~PPTESTCD, mean)
  

#7 Merge with dosing
  doseMerge<-d.dose[,c(1,2)]
  all2<-merge(results,doseMerge,all=T)
  
  sim.data.df <- all2
  #ncadataset<-sim.data.df
})


########################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### #### 
#### #### #### #### #### ####     Plot  all the NCA data      #### #### #### #### #### #### 
#### #### #### #### #### ####                             #### #### #### #### #### #### 
########################################################################################
plotallNCA1 <- eventReactive(input$go, {
  

  
# Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
#NCAdata<-all2
NCAdata<-NCA.data()   
  

  #The plot
  plotobj<-ggplot (NCAdata, aes(x=PPORRES)) +
    geom_histogram (aes(group=as.factor(ID),fill=as.factor(Dose)),color="black") +
    facet_wrap(~PPTESTCD, scales = "free") +
    theme_bw()+
    ylab("Count")+xlab("PK Parameter")+
    scale_fill_manual(name="Doses",values=cbPalette)+
    theme( axis.line = element_line(colour = "black"),
           #panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
           #panel.border = element_blank(),
           #panel.background = element_blank(),
           axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"))
  
  plotobj
  
})

output$plotallNCA <- 
  renderPlot({
    plotallNCA1()
    
  })


########################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### #### 
#### #### #### #### #### ####     Plot  all the safety NCA data  AUCinf+cmax    #### #### #### #### #### #### 
#### #### #### #### #### ####                             #### #### #### #### #### #### 
########################################################################################
plotSAFNCA1 <- eventReactive(input$go, {
  
  # input
  effLimit<-input$efficacyLine
  CmaxLimit<-input$cmaxSafinput
  AUCinfSaf<- input$AUCinfSaf
  #effLimit<-0.5
  #CmaxLimit<-3.0
  #AUCinfSaf<-4.0
  
  
  # Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
  #NCAdata<-all2
  NCAdata<-NCA.data()   
  

  
  # just keep AUC and Cmax:
  NCAdata<-NCAdata[NCAdata$PPTESTCD %in% c("aucinf.obs","cmax" ,"auclast"),]
  
  #summary of the NCA data
  NCAdata%>%
    group_by(PPTESTCD,ID,start,end,Dose)%>%
    summarise(min = min(PPORRES),
              max = max(PPORRES),
              median=median(PPORRES),
              mean=round(mean(PPORRES),2),
              GeoMean=round(gm_mean(PPORRES),8))-> SumNCA
  
  # Summaries related to safety AUC and Cmax:
  AUC<-  NCAdata[NCAdata$PPTESTCD %in% "aucinf.obs",]
  Cmax<-  NCAdata[NCAdata$PPTESTCD %in% "cmax",]
  AUClast<-  NCAdata[NCAdata$PPTESTCD %in% "auclast",]
  
   AUC%>%
    group_by(PPTESTCD)%>%
    summarise(
      Limitsafe = AUCinfSaf,
      Nallsubjects = length(ID),
      NsafeSubjects = length(PPORRES[PPORRES<AUCinfSaf ]),
      Psafe=round(100*(NsafeSubjects/Nallsubjects),2),
      GeoMean=round(gm_mean(PPORRES),4),
      FoldCover=round((Limitsafe /GeoMean),1)
    )-> SafAUC
  
  
  Cmax%>%
    group_by(PPTESTCD)%>%
    summarise(
      Limitsafe = CmaxLimit,
      Nallsubjects = length(ID),
      NsafeSubjects = length(PPORRES[PPORRES<CmaxLimit ]),
      Psafe=round(100*(NsafeSubjects/Nallsubjects),2),
      GeoMean=round(gm_mean(PPORRES),4),
      FoldCover=round((Limitsafe /GeoMean),1))-> SafCmax
  
  AUClast%>%
    group_by(PPTESTCD)%>%
    summarise(
      Limitsafe = AUCinfSaf,
      Nallsubjects = length(ID),
      NsafeSubjects = length(PPORRES[PPORRES<AUCinfSaf ]),
      Psafe=round(100*(NsafeSubjects/Nallsubjects),2),
      GeoMean=round(gm_mean(PPORRES),4),
      FoldCover=round((Limitsafe /GeoMean),1)
    )-> SafAUClast 
  
  
  
  
#SafSum<-rbind(SafAUC,SafCmax)    
  
  
#Finding the Max count of the Histogram for plotting purposes
  # AUC plot
  plotobj<-ggplot (AUC, aes(x=PPORRES)) +
  geom_histogram(colour="black")+ facet_wrap(~PPTESTCD, scales = "free")
  HistoData<-ggplot_build(plotobj)$data[[1]]
  maxCoountHisto<-max(ggplot_build(plotobj)$data[[1]]$count)
  SafAUC<-cbind(SafAUC,maxCoountHisto)
  
  # Cmax plot
  plotobj<-ggplot (Cmax, aes(x=PPORRES)) +
    geom_histogram(colour="black")+ facet_wrap(~PPTESTCD, scales = "free")
  HistoData<-ggplot_build(plotobj)$data[[1]]
  maxCoountHisto<-max(ggplot_build(plotobj)$data[[1]]$count)
  SafCmax<-cbind(SafCmax,maxCoountHisto)
  
  #auc last:
  plotobj<-ggplot (AUClast, aes(x=PPORRES)) +
    geom_histogram(colour="black")+ facet_wrap(~PPTESTCD, scales = "free")
  HistoData<-ggplot_build(plotobj)$data[[1]]
  maxCoountHisto<-max(ggplot_build(plotobj)$data[[1]]$count)
  SafAUClast<-cbind(SafAUClast,maxCoountHisto)
  
  
  # combine the Summary data with the histo  count data:
  AllSums<-rbind(SafAUC,SafCmax,SafAUClast)
  

  #The plot
  plotobj<-ggplot (NCAdata, aes(x=PPORRES)) +
    geom_histogram (aes(group=as.factor(ID),fill=as.factor(Dose)),color="black") +
    
    facet_wrap(~PPTESTCD, scales = "free", ncol=1) +
    ylab("Count")+xlab("PK Parameter")+
    theme_bw()+
    scale_fill_manual(name="Doses",values=cbPalette)+
    theme( axis.line = element_line(colour = "black"),
           axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"),
           strip.text = element_text(size=20))
  plotobj
  
  if (input$LimitsLines) {
    plotobj<-plotobj+
      geom_vline(data= AllSums, aes(xintercept=Limitsafe ), linetype="dashed", size=1.5, colour="red")+
      geom_label(data=AllSums, aes(x=Limitsafe,y=-0.15, 
                                   label=paste0("Safety Limit=",Limitsafe))) 
    plotobj
  }
  
  plotobj
  
  
  
  })

output$plotSAFNCA <- 
  renderPlot({
    plotSAFNCA1()
    
  })

##############################################################################################
#### #### #### #### ####                                               #### #### #### #### #### 
#### #### #### #### ####  ANALYSIS 3: Generate  Time Above Efficacy limit #### #### #### #### ####  
#### #### #### #### ####                                               #### #### #### #### ####  
##############################################################################################
TimeAboveefficacyLine <- eventReactive(input$go, {
  
  data<-sim.data()
  #data<-dataset
  
  #effConcLimit<-1
  effConcLimit<-input$effConcLim
  
  #effTimeLimit<-24
  SimTime<-(input$SimTime)
  # Load your concentration-time data
  d.conc<-data
  d.conc<-data[which(data$amt==0),]
  d.conc$ID<-as.factor(d.conc$ID)

# to cut time in pieces of 3 hours so that we can evaluate Time above Effline:
  y<- seq(0,SimTime,3)
  #y[1]
  for(i in 1:length(y)) {
    # i-th element of `u1` squared into `i`-th position of `usq`
   
     d.conc$Seq[d.conc$time>=y[i] &d.conc$time<=y[i+1]] <-  i
      }

  #Time Above per Seq#
  TimeAbove<-setDT(d.conc)[CP > effConcLimit, diff((time)), by = .(ID,Seq)]
  
  SumAllTime<-summarise(group_by(TimeAbove, ID),
                    sum=sum(V1))  
  IDMAXIN<-input$IDmax 
  DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
  
  
  d.dose<-expand.ev(Dose=DOSEIN, ID=1:IDMAXIN, time=0 )
  
  # merge with dosing
  doseMerge<-d.dose[,c(1,2)]
  SumAllTime2<-merge(doseMerge, SumAllTime,all=T)
  
  
  sim.data.df <- SumAllTime2 
  
})


##############################################################################################
#### #### #### #### ####                                              #### #### #### #### #### 
#### #### #### #### ####       Plot Time Above Efficacy limit    #### #### #### #### ####  
#### #### #### #### ####                                              #### #### #### #### ####  
##############################################################################################
plotTimeAbove1<- eventReactive(input$go, {	
  
  # Efficacy Time limit
  effTimeLimit<-input$effTimeLim
  #effTimeLimit<-24
  
  effConcLimit<-input$effConcLim
  #effConcLimit<-2
  SimTime<-(input$SimTime)
  # Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
  
  TimeAboveefficacyLine() %>%
    #SumAllTime2 %>%
    group_by(Dose)%>%
    summarise(min = min(sum),
              max = max(sum),
              nSubjects=length(unique(ID)),
              GeoMean=round(gm_mean(sum),4),
              median=median(sum),
              mean=round(mean(sum),2))-> SummaryTimeAboveEffiacy
  
  
  
  #finding the Max count of the Histogram / it will be approximate as the bis width change when we add vline below
  plotobj<-ggplot (TimeAboveefficacyLine(), aes(x=sum)) +
    #plotobj<-ggplot (SumAllTime2, aes(x=sum)) +
    geom_histogram(colour="black") 
  HistoData<-ggplot_build(plotobj)$data[[1]]
  maxCoountHisto<-max(ggplot_build(plotobj)$data[[1]]$count)
  
  # combine the Summary data with the histo  count data:
  AllSums<-cbind(SummaryTimeAboveEffiacy,maxCoountHisto)
  
  
  plotobj<-ggplot (TimeAboveefficacyLine(), aes(x=sum)) +
    #plotobj<-ggplot (SumAllTime2, aes(x=sum)) +
    geom_histogram (aes(group=as.factor(ID),fill=as.factor(Dose)),color="black")+
    theme_bw()+
    scale_fill_manual(name="Doses",values=cbPalette)+
    theme( axis.line = element_line(colour = "black"),
           axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"),
           strip.text = element_text(size=20))+
    ggtitle(paste0("Histogram of Cumulative Time above PK Efficacy Target =",effConcLimit)) +
    xlab("Cumulative Time above PK Efficacy Target")+ylab("Count")
  print(plotobj)
  
  
  
  if (input$LimitsLines) {
    plotobj<-plotobj+
      geom_vline(data= AllSums, aes(xintercept=effTimeLimit ), linetype="dashed", size=1.5, colour="red")+
      geom_label(data=AllSums, aes(x=effTimeLimit,y=-0.15, 
                                   label=paste0("Efficacy Time Limit=",effTimeLimit)))
    plotobj
  }
  
  plotobj
  
})

output$plotTimeAbove<- 
  renderPlot({
    plotTimeAbove1()
    
  })
##############################################################################################
#### #### #### #### ####                                               #### #### #### #### #### 
#### #### #### #### ####  ANALYSIS 3: Generate  Conc Efficacy limit #### #### #### #### ####  
#### #### #### #### ####                                               #### #### #### #### ####  
##############################################################################################
ConcefficacyLimit <- eventReactive(input$go, {
  
  data<-sim.data()
  #data<-dataset
  

  effTimeLimit<-input$effTimeLim
  #effTimeLimit<-12
  
  
  # Load your concentration-time data
  d.conc<-data
  d.conc<-data[which(data$amt==0),]
  d.conc$ID<-as.factor(d.conc$ID)
  
  #filter conc only at the effTimelimit:
  Conceffi <-subset(d.conc, time==effTimeLimit) 
  
  
  IDMAXIN<-input$IDmax 
  DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
  
  
  d.dose<-expand.ev(Dose=DOSEIN, ID=1:IDMAXIN, time=0 )
  
  # merge with dosing
  doseMerge<-d.dose[,c(1,2)]
  SumAllConc<-merge(doseMerge, Conceffi,all=T)
  
  
  sim.data.df <- SumAllConc 
  
})


##############################################################################################
#### #### #### #### ####                                              #### #### #### #### #### 
#### #### #### #### ####       Plot Conc Efficacy limit    #### #### #### #### ####  
#### #### #### #### ####                                              #### #### #### #### ####  
##############################################################################################
plotConcEff1<- eventReactive(input$go, {
  
  # Efficacy Time limit
  effTimeLimit<-input$effTimeLim
  #effTimeLimit<-24
  
  effConcLimit<-input$effConcLim
  #effTimeLimit<-2
  
  # Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
  
  ConcefficacyLimit() %>%
    #SumAllConc %>%
    group_by(Dose)%>%
    summarise(min = min(CPLASMA),
              max = max(CPLASMA),
              nSubjects=length(unique(ID)),
              GeoMean=round(gm_mean(CPLASMA),4),
              median=median(CPLASMA),
              mean=round(mean(CPLASMA),2))-> SummaryConcEffiacy
  
  
  
  #finding the Max count of the Histogram / it will be approximate as the bis width change when we add vline below
  plotobj<-ggplot (ConcefficacyLimit(), aes(x=CPLASMA)) +
    #ggplot (SumAllConc, aes(x=CPLASMA)) +
    geom_histogram(colour="black") 
  HistoData<-ggplot_build(plotobj)$data[[1]]
  maxCoountHisto<-max(ggplot_build(plotobj)$data[[1]]$count)
  
  # combine the Summary data with the histo  count data:
  AllSums<-cbind(SummaryConcEffiacy,maxCoountHisto)
  
  
  plotobj<-ggplot (ConcefficacyLimit(), aes(x=CPLASMA)) +
    #ggplot (SumAllConc, aes(x=CPLASMA)) +
    geom_histogram (aes(group=as.factor(ID),fill=as.factor(Dose)),color="black")+
    theme_bw()+
    scale_fill_manual(name="Doses",values=cbPalette)+
    theme( axis.line = element_line(colour = "black"),
           axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"),
           strip.text = element_text(size=20))+
    ggtitle(paste0("Histogram of Conc at Target time =",effTimeLimit)) +
    xlab("Histogram of Conc at Target time")+ylab("Count")
  print(plotobj)
  
 
  if (input$LimitsLines) {
    plotobj<-plotobj+
      geom_vline(data= AllSums, aes(xintercept=effConcLimit ), linetype="dashed", size=1.5, colour="red")+
      geom_label(data=AllSums, aes(x=effConcLimit,y=-0.15, 
                                   label=paste0("Efficacy Conc Limit=",effConcLimit)))
    plotobj
  }
  
  plotobj 
  
})

output$plotConcEff<- 
  renderPlot({
    plotConcEff1()
    
  })


##############################################################################################
#### #### #### #### ####                                              #### #### #### #### #### 
#### #### #### #### ####       Table:  Efficacy table    #### #### #### #### ####  
#### #### #### #### ####                                              #### #### #### #### ####  
##############################################################################################
# create the table
Efftable1<-reactive({
# Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  # input
  # Efficacy Time limit
  effTimeLimit<-input$effTimeLim
  #effTimeLimit<-24
  
  effConcLimit<-input$effConcLim
  #effConcLimit<-2
  
 

EffTimeData<-TimeAboveefficacyLine()
# EffTimeData<- SumAllTime2 
EffConcData<-ConcefficacyLimit()
# EffConcData<-  SumAllConc


#IDMAXIN<-3
IDMAXIN<-input$IDmax
# frame data template:
EffTimeData%>%
  group_by(Dose)%>%
  summarise(NSubjects= IDMAXIN,
            nSubjects=NSubjects-sum(is.na(sum)),
            NASubjects = sum(is.na(sum))
            )->  EffTimeTemplate

EffConcData%>%
  group_by(Dose)%>%
  summarise(NSubjects= IDMAXIN,
            nSubjects=NSubjects-sum(is.na(CPLASMA)),
            NASubjects = sum(is.na(CPLASMA))
  )->  EffConcTemplate





#4  remove raws with NA in  conc column 
removeNA <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

EffTimeData<-removeNA(EffTimeData, "sum")

EffConcData<-removeNA(EffConcData, "CPLASMA")

#EffConcData
EffTimeData%>%
  group_by(Dose)%>%
  summarise(NSubjects= IDMAXIN,
            nSubjects=length(ID),
            NASubjects = NSubjects-length(ID),
            Min = min(sum),
            Max = max(sum),
            Median=median(sum),
            Mean=round(mean(sum),2),
            GeoMean=round(gm_mean(sum),8),
            EffLimit= effTimeLimit,
            NsubjectsLessThan= NASubjects+length(ID[sum<effTimeLimit]),
            Problessthan=round(100*(NsubjectsLessThan/NSubjects),2),
            NsubjectsMoreThan= length(ID[sum>=effTimeLimit]),
            ProbMorethan=round(100*(NsubjectsMoreThan/NSubjects),2))->  SumEffTime

SumEffTime<-merge(EffTimeTemplate,SumEffTime,all=T)

SumEffTime$Target<-"Time Target"
SumEffTime = SumEffTime[order(SumEffTime$Dose),]

#EffConcData
EffConcData%>%
  group_by(Dose)%>%
  summarise(NSubjects= IDMAXIN,
            nSubjects=length(ID),
            NASubjects = NSubjects-length(ID),
            Min = min(CPLASMA),
            Max = max(CPLASMA),
            Median=median(CPLASMA),
            Mean=round(mean(CPLASMA),2),
            GeoMean=round(gm_mean(CPLASMA),8),
            EffLimit= effConcLimit,
            NsubjectsLessThan= NASubjects+length(ID[CPLASMA<effConcLimit]),
            Problessthan=round(100*(NsubjectsLessThan/NSubjects),2),
            NsubjectsMoreThan= length(ID[CPLASMA>=effConcLimit]),
            ProbMorethan=round(100*(NsubjectsMoreThan/NSubjects),2))->  SumEffConc
SumEffConc<-merge(EffConcTemplate,SumEffConc,all=T)

SumEffConc$Target<-"Concentration Target"
SumEffConc = SumEffConc[order(SumEffConc$Dose),]



EffAllData<-rbind(SumEffTime, SumEffConc)
EffAllData<-EffAllData[,c(15,1:14)]
first <- !duplicated(EffAllData[[1]])
EffAllData[[1]][!first] <- ""


EffAllData = EffAllData %>% rename( N=NSubjects, "N<Target"= NsubjectsLessThan,
                                    "P%<Target"=Problessthan, "N>=Target"=NsubjectsMoreThan,
                                    "P%>=Target"=ProbMorethan, "Efficacy Target"=EffLimit)



EffAllData
})

output$Efftable2 <- renderTable({ 
  if(is.null(Efftable1())){return ()} 
  Efftable1()
}) 

# view the table:
output$Efftb<-renderUI({
  if(is.null(Efftable1()))
    h5(tags$img(src = "http://www.jobimu.com/wp-content/uploads/2016/09/GSK_Logo.jpg", width = "500", height = "500"))
  #h5(tags$video(src = "https://www.youtube.com/watch?v=66zvj3m5Wag"))
  #h5(tags$video(src = "https://www.youtube.com/watch?v=66zvj3m5Wag", type = "video", controls = NA, width = "100%"))
  
  else
    tabPanel("Efficacy Tables", tableOutput("Efftable2"))
})

########################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### #### 
#### #### #### #### #### ####   TABLE:  SAFETY Table            #### #### #### #### #### #### 
#### #### #### #### #### ####                             #### #### #### #### #### #### 
########################################################################################

# create the table
SAFtable1<-reactive({
  # Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
  # input
  CmaxLimit<-input$cmaxSafinput
  AUCinfSaf<- input$AUCinfSaf

  
  #NCAdata<-all2
  NCAdata<-NCA.data()  
  
  castedData<-dcast(NCAdata, ID+Dose ~ PPTESTCD,value.var="PPORRES")
  
  castedData%>%
    group_by(Dose)%>%
    summarise(NSubjects = length(ID),
              Min = min(cmax),
              Max = max(cmax),
              Median=median(cmax),
              Mean=round(mean(cmax),2),
              GeoMean=round(gm_mean(cmax),8),
              SafLimit= CmaxLimit,
              NsubjectsLessThan= length(ID[cmax<CmaxLimit]),
              Problessthan=round(100*(NsubjectsLessThan/NSubjects),2),
              NsubjectsMoreThan= length(ID[cmax>=CmaxLimit]),
              ProbMorethan=round(100*(NsubjectsMoreThan/NSubjects),2))->  SumCmax
  SumCmax$Parameter<-"Cmax"
  
  castedData%>%
    group_by(Dose)%>%
    summarise(NSubjects = length(ID),
              Min = min(aucinf.obs),
              Max = max(aucinf.obs),
              Median=median(aucinf.obs),
              Mean=round(mean(aucinf.obs),2),
              GeoMean=round(gm_mean(aucinf.obs),8),
              SafLimit= AUCinfSaf,
              NsubjectsLessThan= length(ID[aucinf.obs<AUCinfSaf]),
              Problessthan=round(100*(NsubjectsLessThan/NSubjects),2),
              NsubjectsMoreThan= length(ID[aucinf.obs>=AUCinfSaf]),
              ProbMorethan=round(100*(NsubjectsMoreThan/NSubjects),2))->  SumAUCinf
  SumAUCinf$Parameter<-"aucinf.obs"
  castedData%>%
    group_by(Dose)%>%
    summarise(NSubjects = length(ID),
              Min = min(auclast),
              Max = max(auclast),
              Median=median(auclast),
              Mean=round(mean(auclast),2),
              GeoMean=round(gm_mean(auclast),8),
              SafLimit= AUCinfSaf,
              NsubjectsLessThan= length(ID[auclast<AUCinfSaf]),
              Problessthan=round(100*(NsubjectsLessThan/NSubjects),2),
              NsubjectsMoreThan= length(ID[auclast>=AUCinfSaf]),
              ProbMorethan=round(100*(NsubjectsMoreThan/NSubjects),2))->  SumAUClast
  SumAUClast$Parameter<-"AUClast"
 
  
  SAFAllData<-rbind(SumCmax, SumAUCinf,SumAUClast)
  
  SAFAllData<-SAFAllData[,c(13,1:12)]
  first <- !duplicated(SAFAllData[[1]])
  SAFAllData[[1]][!first] <- ""
  
  
  SAFAllData = SAFAllData %>% rename( N=NSubjects, "N<Target"= NsubjectsLessThan,
                                      "P%<Target"=Problessthan, "N>=Target"=NsubjectsMoreThan,
                                      "P%>=Target"=ProbMorethan, "Safety Target"=SafLimit)
  SAFAllData 
  

})


output$SAFtable2 <- renderTable({ 
  if(is.null(SAFtable1())){return ()} 
  SAFtable1()
}) 

# view the table:
output$SAFtb<-renderUI({
  if(is.null(SAFtable1()))
    h5(tags$img(src = "http://www.jobimu.com/wp-content/uploads/2016/09/GSK_Logo.jpg", width = "500", height = "500"))
  #h5(tags$video(src = "https://www.youtube.com/watch?v=66zvj3m5Wag"))
  #h5(tags$video(src = "https://www.youtube.com/watch?v=66zvj3m5Wag", type = "video", controls = NA, width = "100%"))
  
  else
    tabPanel("SAF Tables", tableOutput("SAFtable2"))
})



########################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### #### 
#### #### #### #### #### ####   TABLE:  NCA Table output           #### #### #### #### #### #### 
#### #### #### #### #### ####                             #### #### #### #### #### #### 
########################################################################################

# create the table
NCAtable1<-reactive({
# Geomtric Mean Function
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }

# input
  effLimit<-input$efficacyLine
  CmaxLimit<-input$cmaxSafinput
  AUCinfSaf<- input$AUCinfSaf


#NCAdata<-all2
NCAdata<-NCA.data()  


#summary of the NCA data
  NCAdata%>%
    group_by(Dose,PPTESTCD,start,end)%>%
    summarise(N = length(ID),
              Min = min(PPORRES),
              Max = max(PPORRES),
              Median=median(PPORRES),
              Mean=round(mean(PPORRES),2),
              GeoMean=round(gm_mean(PPORRES),8))->  SumNCA
 
  
  #TimeAboveMPC
  TimeAboveefficacyLine()%>%
    #SumAllTime2%>%
    group_by(Dose)%>%
    summarise(N = length(ID),
              Min = min(sum),
              Max = max(sum),
              Median=median(sum),
              Mean=round(mean(sum),2),
              GeoMean=round(gm_mean(sum),8))->  SumEff
  SumEff$PPTESTCD<-"TimeAboveEff"
  
  
  Tabledata<-merge(SumNCA, SumEff,all=T)
  
  names(Tabledata)[names(Tabledata)=="PPTESTCD"] <- "Parameter"
  names(Tabledata)[names(Tabledata)=="GeoMean"] <- "Geo-Mean"
  
  Tabledata<-as.data.frame(Tabledata)
  
 
  first <- !duplicated(Tabledata[[1]])
  Tabledata[[1]][!first] <- ""
  Tabledata
})


output$NCAtable2 <- renderTable({ 
  if(is.null(NCAtable1())){return ()} 
  NCAtable1()
}) 

# view the table:
output$tb<-renderUI({
  if(is.null(NCAtable1()))
    h5(tags$img(src = "http://www.jobimu.com/wp-content/uploads/2016/09/GSK_Logo.jpg", width = "500", height = "500"))
  #h5(tags$video(src = "https://www.youtube.com/watch?v=66zvj3m5Wag"))
  #h5(tags$video(src = "https://www.youtube.com/watch?v=66zvj3m5Wag", type = "video", controls = NA, width = "100%"))
  
  else
    tabPanel("NCA Tables", tableOutput("NCAtable2"))
})



########################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### #### 
#### #### #### #### #### ####   Final report          #### #### #### #### #### #### 
#### #### #### #### #### ####                             #### #### #### #### #### #### 
########################################################################################

output$report <- downloadHandler(
  
  # For PDF output, change this to "report.pdf"
  filename =  function() {
    paste("Report-", Sys.Date(), ".html", sep="")
                          },
  
  
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    

    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
   
    
    # model structure:
    
   
    # Author:
    
    AuthorParam<-input$Author
    CompoundParam<-input$Compound
    Commentsenterd<-input$foo
    #selected model:
    selectPKmod <- input$PKmodel  
    
    # Simulations # KEEP amt the first column
    if (selectPKmod == 1) {
      Modelname<-' PK 1-compartment Oral Model'
      ModelParameters<-data.frame(Paramater = c("KA","Clearance", "Vc", "F",
                                                 "KA-BSV%", "CL-BSV%", "Vc-BSV%",  "F-BSV%"), 
                                  Value=c(input$KA1,input$CL1,input$VC1, input$FBIO1,
                                          input$EKA1, input$ECL1,input$EVC1,input$EBV1 ))
    }
   
    # Simulations # KEEP amt the first column
    if (selectPKmod == 2) {
      Modelname<-' PK 2-compartment Oral Model'
      ModelParameters<-data.frame(Paramater = c("KA","Clearance", "Vc", "F",
                                                "Q","Vp", "KA-BSV%", "CL-BSV%", "Vc-BSV%",  "F-BSV%"), 
                                  Value=c(input$KA2,input$CL2,input$VC2, input$FBIO2,
                                          input$Q12, input$V22, input$EKA2, input$ECL2,input$EVC2,input$EBV2 ))
    }
   
     # Simulations # KEEP amt the first column
    if (selectPKmod == 3) {
      Modelname<-' PK 3-compartment Oral Model'
      ModelParameters<-data.frame(Paramater = c("KA","Clearance", "Vc", "F",
                                                "Q","Vp","Q2","Vp2", "KA-BSV%", "CL-BSV%", "Vc-BSV%",  "F-BSV%"), 
                                  Value=c(input$KA3,input$CL3,input$VC3, input$FBIO3,
                                          input$Q13, input$V23, input$Q23, input$V33,input$EKA3, input$ECL3,input$EVC3,input$EBV3 ))
    }
    if (selectPKmod == 4) {
      Modelname<-' PK 1-compartment IV Model'
      ModelParameters<-data.frame(Paramater = c("Clearance", "Vc", 
                                                 "CL-BSV%", "Vc-BSV%"), 
                                  Value=c(input$CL4,input$VC4, 
                                           input$ECL4,input$EVC4 ))
    }
    if (selectPKmod == 5) {
      Modelname<-' PK 2-compartment IV Model'
      ModelParameters<-data.frame(Paramater = c("Clearance", "Vc", 
                                                "Q","Vp",  "CL-BSV%", "Vc-BSV%"), 
                                  Value=c(input$CL5,input$VC5, 
                                          input$Q15, input$V25,  input$ECL5,input$EVC5))
    }
    if (selectPKmod == 6) {
      Modelname<-' PK 3-compartment IV Model'
      ModelParameters<-data.frame(Paramater = c("Clearance", "Vc", 
                                                "Q","Vp","Q2","Vp2",  "CL-BSV%", "Vc-BSV%"), 
                                  Value=c(input$CL6,input$VC6, 
                                          input$Q16, input$V26, input$Q26, input$V36, input$ECL6,input$EVC6))
    }
    DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
   # DoseParam<-DOSEIN
    DosingParams<- data.frame(Information = c("Dose(s)","II", "ADDL"),
                              Value=c(paste(DOSEIN, collapse = " "),input$II,input$ADDL))
    
    
    # DoseParam<-DOSEIN
    SafetyParams<- data.frame(Information = c("AUC","Cmax"),
                              Value=c(input$AUCinfSaf,input$cmaxSafinput))
    
    EfficacyParams<-data.frame(Target= c("Conc","Time"),
                               Value=c(input$effConcLim,input$effTimeLim)) 
      
      
    
    #NCA params:
    NCALimitsParams<-data.frame(Time = c("Start","End"),
                                Value=c(input$NCAfrom,input$NCAto))
    
    
    # for tables and plots, you pass reactives!
    
    #Tables
    
    #NCA table
    NCAtable1<-NCAtable1()
    
    #Eff table
    Efftable1<-Efftable1()
    #SAF table
    SAFtable1<-SAFtable1()
    
    #plots
    
    #PKPlot
    Pkplot<-PLOTConc()
    
    #NCA safety Plots
    plotallNCA1<-plotallNCA1()
    
    plotSAFNCA1<-plotSAFNCA1()
    
    plotEFF1<-plotTimeAbove1()
    plotEFF2<-plotConcEff1()
    
    
    params <- list(AuthorParam=AuthorParam,
                   CompoundParam=CompoundParam,
                   Commentsenterd=Commentsenterd,
                   Modelname=Modelname,
                   ModelParameters = ModelParameters,
                   DosingParams=DosingParams,
                   SafetyParams=SafetyParams,
                   EfficacyParams=EfficacyParams,
                   NCALimitsParams=NCALimitsParams,
                   NCAtable1=NCAtable1,
                   Efftable1=Efftable1,
                   SAFtable1=SAFtable1,
                   Pkplot=Pkplot,
                   plotallNCA1=plotallNCA1,
                   plotSAFNCA1=plotSAFNCA1,
                   plotEFF1=plotEFF1,
                   plotEFF2=plotEFF2)
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)
   

########################################################################################
#### #### #### #### #### ####                             #### #### #### #### #### #### 
#### #### #### #### #### ####  Other downloads          #### #### #### #### #### #### 
#### #### #### #### #### ####                             #### #### #### #### #### #### 
########################################################################################           
# Simulation Dataset                    
output$downloadDataset <- downloadHandler(
  filename =  function() {
    paste("SimDataset-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    data<-sim.data()
    IDMAXIN<-input$IDmax 
    DOSEIN <- as.numeric(unlist(strsplit(as.character(input$DOSE),",")))
    DoseData <- expand.ev(Dose=DOSEIN, ID=1:IDMAXIN)
    DoseData<-DoseData[,c(1,2)]
    MergedData<-merge(data,DoseData,by="ID",all.x=T)
    write.csv(MergedData, file, row.names=F)
  }
)   

# NCA Dataset                    
output$downloadNCA <- downloadHandler(
  filename =  function() {
    paste("NCADataset-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    data<-NCA.data()
    #data<-ncadataset
    castedData<-dcast(data, ID+Dose ~ PPTESTCD,value.var="PPORRES")
    castedData = castedData[order(castedData$Dose),]
    castedData$NCAfrom<-input$NCAfrom
    castedData$NCAto<-input$NCAto
    castedData$CmaxLimit<-input$cmaxSafinput
    castedData$AUCinfSaf<- input$AUCinfSaf
    write.csv(castedData, file, row.names=F)
  }
) 
#
# Conc Efficacy Dataset                    
output$downloadConcEfficacy <- downloadHandler(
  filename =  function() {
    paste("ConcEfficacyDataset-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    data<-ConcefficacyLimit()
    data = data[order(data$Dose),]
    write.csv(data, file, row.names=F)
  }
) 

#
# Time Efficacy Dataset                    
output$downloadTimeEfficacy <- downloadHandler(
  filename =  function() {
    paste("TimeEfficacyDataset-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    data<-TimeAboveefficacyLine()
    data = data[order(data$Dose),]
    data = data %>% rename( TotalTime=sum  )
    write.csv(data, file, row.names=F)
  }
) 
         

}








