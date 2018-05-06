int: min_CS_SC; % Min Number of cases 
int: max_CS_SC; % Max Number of cases 
var min_CS_SC .. max_CS_SC : CS_SC; % Number of cases 

int: min_CS_SS; % Min Number of stages 
int: max_CS_SS; % Max Number of stages 
var min_CS_SS .. max_CS_SS : CS_SS; % Number of stages 

int: min_CS_SDS; % Min Number of discretionary stages 
int: max_CS_SDS; % Max Number of discretionary stages 
var min_CS_SDS .. max_CS_SDS : CS_SDS; % Number of discretionary stages 

int: min_CS_SPF; % Min Number of plan fragments 
int: max_CS_SPF; % Max Number of plan fragments 
var min_CS_SPF .. max_CS_SPF : CS_SPF; % Number of plan fragments 

int: min_CS_DI; % Min Number of case file items 
int: max_CS_DI; % Max Number of case file items 
var min_CS_DI .. max_CS_DI : CS_DI; % Number of case file items 

%int: min_PT2MH; % Min non-blocking human task  
%int: max_PT2MH; % Max non-blocking human task  
%var min_PT2MH .. max_PT2MH : PT2MH; % non-blocking human task  
var min_CS_PT .. max_CS_PT : PT2MH; % non-blocking human task

%int: min_PT2MP; % Min process task  
%int: max_PT2MP; % Max process task  
%var min_PT2MP .. max_PT2MP : PT2MP; % process task  
var min_CS_PT .. max_CS_PT : PT2MP; % process task 

%int: min_PT2MC; % Min case task referring to a case  not in this model  
%int: max_PT2MC; % Max case task referring to a case  not in this model  
%var min_PT2MC .. max_PT2MC : PT2MC; % case task referring to a case  not in this model  
var min_CS_PT .. max_CS_PT : PT2MC; % case task referring to a case  not in this model

%int: min_PT2MC2C; % Min case task referring to a case  in this model  
%int: max_PT2MC2C; % Max case task referring to a case  in this model  
%var min_PT2MC2C .. max_PT2MC2C : PT2MC2C; % case task referring to a case  in this model  
var min_CS_PT .. max_CS_PT : PT2MC2C; % case task referring to a case  in this model

%int: min_PT2MHB; % Min blocking human task   
%int: max_PT2MHB; % Max blocking human task   
%var min_PT2MHB .. max_PT2MHB : PT2MHB; % blocking human task   
var min_CS_PT .. max_CS_PT : PT2MHB; % blocking human task

int: min_CS_PT; % Number of tasks 
int: max_CS_PT; % Number of tasks 
var min_CS_PT .. max_CS_PT: CS_PT; % Number of tasks 
CS_PT =  PT2MH + PT2MP + PT2MC + PT2MC2C + PT2MHB;

%int: min_PDT2MH; % Min discretionary non-blocking human task  
%int: max_PDT2MH; % Max discretionary non-blocking human task  
%var min_PDT2MH .. max_PDT2MH : PDT2MH; % discretionary non-blocking human task  
var min_CS_PDT .. max_CS_PDT : PDT2MH; % discretionary non-blocking human task

%int: min_PDT2MP; % Min discretionary process task  
%int: max_PDT2MP; % Max discretionary process task  
%var min_PDT2MP .. max_PDT2MP : PDT2MP; % discretionary process task  
var min_CS_PDT .. max_CS_PDT : PDT2MP; % discretionary process task

%int: min_PDT2MC; % Min discretionary case task referring to a case  not in this model  
%int: max_PDT2MC; % Max discretionary case task referring to a case  not in this model  
%var min_PDT2MC .. max_PDT2MC : PDT2MC; % discretionary case task referring to a case  not in this model  
var min_CS_PDT .. max_CS_PDT : PDT2MC; % discretionary case task referring to a case  not in this model 

%int: min_PDT2MC2C; % Min discretionary case task referring to a case  in this model  
%int: max_PDT2MC2C; % Max discretionary case task referring to a case  in this model  
%var min_PDT2MC2C .. max_PDT2MC2C : PDT2MC2C; % discretionary case task referring to a case  in this model  
var min_CS_PDT .. max_CS_PDT : PDT2MC2C; % discretionary case task referring to a case  in this model

%int: min_PDT2MHB; % Min discretionary blocking human task   
%int: max_PDT2MHB; % Max discretionary blocking human task   
%var min_PDT2MHB .. max_PDT2MHB : PDT2MHB; % discretionary blocking human task   
var min_CS_PDT .. max_CS_PDT : PDT2MHB; % discretionary blocking human task

int: min_CS_PDT; % Number of tasks 
int: max_CS_PDT; % Number of tasks 
var min_CS_PDT .. max_CS_PDT: CS_PDT; % Number of discretionary tasks 
CS_PDT =  PDT2MH + PDT2MP + PDT2MC + PDT2MC2C + PDT2MHB;


int: min_PE; % Min plan event listener element  
int: max_PE; % Max plan event listener element  
var min_PE .. max_PE : PE; % plan event listener element  

int: min_PE2MHB; % Min user event listener element  
int: max_PE2MHB; % Max user event listener element  
var min_PE2MHB .. max_PE2MHB : PE2MHB; % user event listener element  

int: min_PE2MT; % Min timer event listener element  
int: max_PE2MT; % Max timer event listener element  
var min_PE2MT .. max_PE2MT : PE2MT; % timer event listener element  

var int: CS_PE; % Number of event listeners 
CS_PE =  PE + PE2MHB + PE2MT;

int: min_CS_PM; % Min Number of milestones 
int: max_CS_PM; % Max Number of milestones 
var min_CS_PM .. max_CS_PM : CS_PM; % Number of milestones 

int: min_DCP_S; % Min stage collapsed planning table decorator  
int: max_DCP_S; % Max stage collapsed planning table decorator  
var min_DCP_S .. max_DCP_S : DCP_S; % stage collapsed planning table decorator  

int: min_DCP_H; % Min human task  collapsed planning table decorator  
int: max_DCP_H; % Max human task  collapsed planning table decorator  
var min_DCP_H .. max_DCP_H : DCP_H; % human task  collapsed planning table decorator  

var int: CAS_DCP; % Number of collapsed planing table decorators 
CAS_DCP =  DCP_S + DCP_H;

int: min_DEP_S; % Min stage expanded planning table decorator 
int: max_DEP_S; % Max stage expanded planning table decorator 
var min_DEP_S .. max_DEP_S : DEP_S; % stage expanded planning table decorator 

int: min_DEP_H; % Min human task  expanded planning table decorator 
int: max_DEP_H; % Max human task  expanded planning table decorator 
var min_DEP_H .. max_DEP_H : DEP_H; % human task  expanded planning table decorator 

var int: CAS_DEP; % Number of expanded planing table decorators 
CAS_DEP =  DEP_S + DEP_H;

int: min_CAS_DAC; % Min Number of auto complete decorators 
int: max_CAS_DAC; % Max Number of auto complete decorators 
var min_CAS_DAC .. max_CAS_DAC : CAS_DAC; % Number of auto complete decorators 

int: min_CAS_DC; % Min Number of collapsed decorators 
int: max_CAS_DC; % Max Number of collapsed decorators 
var min_CAS_DC .. max_CAS_DC : CAS_DC; % Number of collapsed decorators 

int: min_CAS_DE; % Min Number of expanded decorators 
int: max_CAS_DE; % Max Number of expanded decorators 
var min_CAS_DE .. max_CAS_DE : CAS_DE; % Number of expanded decorators 

int: min_CAS_DMA; % Min Number of manual activation decorators 
int: max_CAS_DMA; % Max Number of manual activation decorators 
var min_CAS_DMA .. max_CAS_DMA : CAS_DMA; % Number of manual activation decorators 

int: min_CAS_DRN; % Min Number of repetition decorators 
int: max_CAS_DRN; % Max Number of repetition decorators 
var min_CAS_DRN .. max_CAS_DRN : CAS_DRN; % Number of repetition decorators 

int: min_CAS_DR; % Min Number of required decorators 
int: max_CAS_DR; % Max Number of required decorators 
var min_CAS_DR .. max_CAS_DR : CAS_DR; % Number of required decorators 

int: min_SE2OC; % Min entry criterion sentry with associated connector  
int: max_SE2OC; % Max entry criterion sentry with associated connector  
var min_SE2OC .. max_SE2OC : SE2OC; % entry criterion sentry with associated connector  

int: min_SE; % Min entry criterion sentry without a connector  
int: max_SE; % Max entry criterion sentry without a connector  
var min_SE .. max_SE : SE; % entry criterion sentry without a connector  

var int: CAS_SE; % Number of entry criteria sentries 
CAS_SE =  SE2OC + SE;

int: min_SX2OC; % Min exit criterion sentry with associated connector  
int: max_SX2OC; % Max exit criterion sentry with associated connector  
var min_SX2OC .. max_SX2OC : SX2OC; % exit criterion sentry with associated connector  

int: min_SX; % Min exit criterion sentry without a connector  
int: max_SX; % Max exit criterion sentry without a connector  
var min_SX .. max_SX : SX; % exit criterion sentry without a connector  

var int: CAS_SX; % Number of exit criteria sentries 
CAS_SX =  SX2OC + SX;

int: min_PT2MHB_PDT2MHB; % min blocking-human-task
int: max_PT2MHB_PDT2MHB; % max blocking-human-task
var min_PT2MHB_PDT2MHB .. max_PT2MHB_PDT2MHB: PT2MHB_PDT2MHB; % blocking-human-task

int: min_OC; % Min optional connector [sentry] element 
int: max_OC; % Max optional connector [sentry] element 
var min_OC .. max_OC : OC; % optional connector [sentry] element 

var int: CS_OC; % Number of connectors 
CS_OC =  OC + SE2OC + SX2OC;

int: min_CAS_MH; % Min Number of non-blocking human markers 
int: max_CAS_MH; % Max Number of non-blocking human markers 
var int: CAS_MH; % Number of non-blocking human markers 
CAS_MH =  PT2MH + PDT2MH;
constraint CAS_MH >= min_CAS_MH;
constraint CAS_MH <= max_CAS_MH;

int: min_CAS_MP; % Min Number of process markers 
int: max_CAS_MP; % Max Number of process markers 
var int: CAS_MP; % Number of process markers 
CAS_MP =  PT2MP + PDT2MP;
constraint CAS_MH >= min_CAS_MP;
constraint CAS_MH <= max_CAS_MP;

int: min_MC_I;
int: max_MC_I;
var int: MC_I; % case marker with  case  in model
MC_I = PT2MC2C + PDT2MC2C;
constraint MC_I >= min_MC_I;
constraint MC_I <= max_MC_I;

int: min_MC_O;
int: max_MC_O;
var int: MC_O; % case marker _with  case not in m
MC_O = PT2MC + PDT2MC; 
constraint MC_O >= min_MC_O;
constraint MC_O <= max_MC_O;

var int: CAS_MC; % Number of case markers 
CAS_MC =  MC_I + MC_O;

var int: CAS_MHB; % Number of participant markers 
CAS_MHB =  PT2MHB + PDT2MHB + PE2MHB;

var int: CAS_MT; % Number of timer markers 
CAS_MT =  PE2MT;

%% Now calculate metrics %%

int: weight_CS_SC;
int: weight_CS_SS;
int: weight_CS_SDS;
int: weight_CS_SPF;
int: weight_CS_DI;
int: weight_CS_PT;
int: weight_CS_PDT;
int: weight_PE;
int: weight_PE2MHB;
int: weight_PE2MT;
int: weight_CS_PM;
int: weight_DCP_S;
int: weight_DEP_S;
int: weight_DCP_H;
int: weight_DEP_H;
int: weight_CAS_DAC;
int: weight_CAS_DC;
int: weight_CAS_DE;
int: weight_CAS_DMA;
int: weight_CAS_DRN;
int: weight_CAS_DR;
int: weight_SE2OC;
int: weight_SE;
int: weight_SX2OC;
int: weight_SX;
int: weight_CAS_MH;
int: weight_CAS_MP;
int: weight_MC_O;
int: weight_MC_I;
int: weight_PT2MHB_PDT2MHB;
int: weight_OC;

var 0..185: CC; %  CMMN Complexity (max 1 case + 6*3 + 83*2)
CC = (weight_CS_SC * CS_SC) + (weight_CS_SS * CS_SS) + (weight_CS_SDS * CS_SDS) + (weight_CS_SPF * CS_SPF) + (weight_CS_DI * CS_DI) + (weight_CS_PT * CS_PT) + (weight_CS_PDT * CS_PDT) + (weight_PE * PE) + (weight_PE2MHB * PE2MHB) + (weight_PE2MT * PE2MT) + (weight_CS_PM * CS_PM) + (weight_DCP_S * DCP_S) + (weight_DEP_S * DEP_S) + (weight_DCP_H * DCP_H) + (weight_DEP_H * DEP_H) + (weight_CAS_DAC * CAS_DAC) + (weight_CAS_DC * CAS_DC) + (weight_CAS_DE * CAS_DE) + (weight_CAS_DMA * CAS_DMA) + (weight_CAS_DRN * CAS_DRN) + (weight_CAS_DR * CAS_DR) + (weight_SE2OC * SE2OC) + (weight_SE * SE) + (weight_SX2OC * SX2OC) + (weight_SX * SX) + (weight_CAS_MH * CAS_MH) + (weight_CAS_MP * CAS_MP) + (weight_MC_O * MC_O) + (weight_MC_I * MC_I) + (weight_PT2MHB_PDT2MHB * PT2MHB_PDT2MHB) + (weight_OC * OC);

%int: min_CL; % Min  CMMN Length     
%int: max_CL; % Max  CMMN Length     
var 0..45 : CL; %  CMMN Length     
constraint CL >= CAS_DE;
constraint CL <= CAS_DE;

var 1..90: CS; %  CMMN Size     
CS = CS_SC + CS_SS + CS_SDS + CS_SPF + CS_DI + CS_PT + CS_PDT + CS_PE + CS_PM + CS_OC;

var 0..90: CAS; % CMMN annotators size   
CAS = CAS_DCP + CAS_DEP + CAS_DAC + CAS_DC + CAS_DE + CAS_DMA + CAS_DRN + CAS_DR + CAS_SE + CAS_SX + CAS_MH + CAS_MP + CAS_MC + CAS_MHB + CAS_MT;

var int: CTS; %  CMMN total size     
CTS = CS +  CAS;
constraint CTS = 90;

%% All tasks must have types (guaranteed by generation)
constraint (CAS_MH + CAS_MP + CAS_MC + PT2MHB_PDT2MHB) == (CS_PT + CS_PDT);

%% Correct number of non-blocking human markers (tasks and events)
constraint (PT2MHB_PDT2MHB + PE2MHB) = CAS_MHB;

%% Must have less or equal stage planning tables than stages and cases 
constraint (DCP_S + DEP_S)  <= (CS_SC + CS_SS + CS_SDS);

%% Must have less or equal human planning tables than human tasks 
constraint (DCP_H + DEP_H) <= (CAS_MH + PT2MHB + PDT2MHB);

%% expanded and collapsed markers must match the number of stages and planning tables 
constraint (CAS_DC + CAS_DE) == (CS_SS + CS_SDS + CS_SPF);

%% must have a case in the model (guaranteed by generation)
constraint  CS_SC > 0 ;

%% cannot have more  cases and stages than auto complete decorators 
constraint CAS_DAC <= (CS_SC + CS_SS + CS_SDS);

%% cannot have more stages and tasks than manual activation decorators
constraint CAS_DMA <= (CS_SS + CS_PT);

%% cannot have more stages, tasks and milestones than repetition decorators
constraint CAS_DRN <= (CS_SS + CS_PT + CS_PM);

%% cannot have more stages, tasks and milestones than required decorators
constraint CAS_DR <= (CS_SS + CS_PT + CS_PM);

%% must have enough discretionary elements for expanded stages planning tables
constraint DEP_S <= (CS_SDS + CS_SPF + CS_PDT);

%% must have enough discretionary elements for expanded human planning tables
constraint DEP_H <= (CS_SDS + CS_SPF + CS_PDT);

%% must have enough discretionary elements for expanded planning tables
constraint CAS_DEP <= (CS_SDS + CS_SPF + CS_PDT);

%% must have enough extra connectors for expanded human planning tables
constraint OC >= DEP_H;

%% need expanding planning tables when there are discretionary elements
constraint ((CS_SDS + CS_SPF + CS_PDT) > 0) /\ (CAS_DEP > 0);

%% must have enough connectors for case file items, events and human planning tables
constraint (CS_DI + CS_PE + DEP_H) <= CS_OC;

%% need entry or exit criteria with connectors for case file items and events
constraint (CS_DI + CS_PE) <= (SE2OC + SX2OC);

%% repetitions require entry criteria
constraint CAS_DRN <= CAS_SE;

% no more than two entry criterion per task and stages
constraint (CAS_SE * 2) <= (CS_SS + CS_PT); 

% no more than two exit criteria per stage or case 
constraint (CAS_SX * 2) <= (CS_SS + CS_SDS + CS_SC);


solve satisfy;
%solve minimize CC;
%solve maximize CC;

output["\nBasic Symbols",
"\nCS_SC=", show(CS_SC), "\t Number of cases",
"\nCS_SS=", show(CS_SS), "\t Number of stages",
"\nCS_SDS=", show(CS_SDS), "\t Number of discretionary stages",
"\nCS_SPF=", show(CS_SPF), "\t Number of plan fragments",
"\nCS_DI=", show(CS_DI), "\t Number of case file items",
"\nCS_PT=", show(CS_PT), "\t Number of tasks",
"\nCS_PDT=", show(CS_PDT), "\t Number of discretionary tasks",
"\nPE=", show(PE), "\t Event listener elements",
"\nPE2MHB=", show(PE2MHB), "\t User event listener element",
"\nPE2MT=", show(PE2MT), "\t Timer event listener element",
"\nCS_PM=", show(CS_PM), "\t Number of milestones",
"\nDCP_S=", show(DCP_S), "\t Stage collapsed planning table",
"\nDEP_S=", show(DEP_S), "\t Stage expanded planning table",
"\nDCP_H=", show(DCP_H), "\t Human task collapsed planning table",
"\nDEP_H=", show(DEP_H), "\t Human task expanded planning table",
"\nCAS_DAC=", show(CAS_DAC), "\t Number of auto complete decorators",
"\nCAS_DC=", show(CAS_DC), "\t Number of collapsed decorators",
"\nCAS_DE=", show(CAS_DE), "\t Number of expanded decorators",
"\nCAS_DMA=", show(CAS_DMA), "\t Number of manual activation decorators",
"\nCAS_DRN=", show(CAS_DRN), "\t Number of repetition decorators",
"\nCAS_DR=", show(CAS_DR), "\t Number of required decorators",
"\nSE2OC=", show(SE2OC), "\t Entry criterion with associated connector",
"\nSE=", show(SE), "\t Entry criterion without a connector",
"\nSX2OC=", show(SX2OC), "\t Exit criterion with associated connector",
"\nSX=", show(SX), "\t Exit criterion without a connector",
"\nCAS_MH=", show(CAS_MH), "\t Number of non-blocking human markers",
"\nCAS_MP=", show(CAS_MP), "\t Number of process markers",
"\nMC_O=", show(MC_O), "\t Case marker (case not in this model)",
"\nMC_I=", show(MC_I), "\t Case marker (with  case in this model)",
"\nPT2MHB_PDT2MHB=", show(PT2MHB_PDT2MHB), "\t Blocking human marker",
"\nOC=", show(OC), "\t Extra connector (sentry) element",
"\nMetrics:",
"\nCC=", show(CC),
"\nCL=", show(CL),
"\nCS=", show(CS),
"\nCAS=", show(CAS),
"\nCTS=", show(CTS),
"\nSub-Metrics:",
"\nCS_SC=",show(CS_SC),
"\nCS_SS=",show(CS_SS),
"\nCS_SDS=",show(CS_SDS),
"\nCS_SPF=",show(CS_SPF),
"\nCS_DI=",show(CS_DI),
"\nCS_PT=",show(CS_PT),
"\nCS_PDT=",show(CS_PDT),
"\nCS_PE=",show(CS_PE),
"\nCS_PM=",show(CS_PM),
"\nCS_OC=",show(CS_OC),
"\nCAS_DCP=",show(CAS_DCP),
"\nCAS_DEP=",show(CAS_DEP),
"\nCAS_DAC=",show(CAS_DAC),
"\nCAS_DC=",show(CAS_DC),
"\nCAS_DE=",show(CAS_DE),
"\nCAS_DMA=",show(CAS_DMA),
"\nCAS_DRN=",show(CAS_DRN),
"\nCAS_DR=",show(CAS_DR),
"\nCAS_SE=",show(CAS_SE),
"\nCAS_SX=",show(CAS_SX),
"\nCAS_MH=",show(CAS_MH),
"\nCAS_MP=",show(CAS_MP),
"\nCAS_MC=",show(CAS_MC),
"\nCAS_MHB=",show(CAS_MHB),
"\nCAS_MT=",show(CAS_MT), "\nEnd.\n"];
