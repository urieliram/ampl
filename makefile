SYSTEM     = x86-64_linux
LIBFORMAT  = static_pic
 
#CF =/home/uriel/intel/compilers_and_libraries_2020.0.166/linux/bin/intel64/ifort
#CC =/home/uriel/intel/compilers_and_libraries_2020.0.166/linux/bin/intel64/icc

CF = /home/uriel/intel/compilers_and_libraries_2017.8.262/linux/bin/intel64/ifort
CC = /home/uriel/intel/compilers_and_libraries_2017.8.262/linux/bin/intel64/icc

OPCION = -f77 -mcmodel=medium
#CF = gfortran
#OPCION = -mcmodel=medium -ffree-form -std=legacy -ffree-line-length-n

OBJETOS= symtypes.o cplex_cons.o cpx_forman.o \
ParAUHE.o ParAUHE_Hidro.o ParAUHE_MILP.o ParGloRed.o Validaciones.o \
CalCosMarIntervalo.o AUHE.o DefVariables.o  \
FlujosPac.o FlujosPotenciaActiva.o FormaMILP.o FormaMILP_Hidro.o \
FormaMILPUHI.o FormaMILPURC.o FormaMILPURD.o \
FormaMILPURE.o ImprimeAUHE.o ImprimeAUHEH.o \
ImprimeAUHERC.o ImprimeAUHERD.o LecDatRed.o \
LecturasAUHE.o Open_CPLEX.o PreparaIntervalo.o ReportesCSV.o \
LecUnidadesH.o LecUnidadesRC.o LecUnidadesRE.o LecUnidadesRD.o LecAuHeHidro.o \
SenFluPac.o SensibilidadesFluyPer.o Utilerias.o Utility.o dumy.o CondIniciales.o

CPLEXDIR = /home/uriel/cplex1210/cplex

CPLEXLIBDIR   = $(CPLEXDIR)/lib/$(SYSTEM)/$(LIBFORMAT)

CCLNFLAGS = -L$(CPLEXLIBDIR) -lilocplex -lcplex -lm

INCLUDES = -I/home/uriel/cplex1210/cplex/include -I/home/uriel/cplex1210/concert/include

LICENCIA: export INTEL_LICENSE_FILE=/home/uriel/intel/licenses/l_1314450195_TFVGJM8P.lic
@export INTEL_LICENSE_FILE=/home/uriel/intel/licenses/l_1314450195_TFVGJM8P.lic

pe:	$(OBJETOS)
	$(CF) -o AUHE.exe $(OBJETOS) $(CCLNFLAGS)
ParGloRed.o:	ParGloRed.f90
		$(CF) -c  $(OPCION) ParGloRed.f90
AUHE.o:		AUHE.f90
		$(CF) -c  $(OPCION) AUHE.f90
CalCosMarIntervalo.o:	CalCosMarIntervalo.f90
			$(CF) -c $(OPCION) CalCosMarIntervalo.f90
DefVariables.o:	DefVariables.f90
		$(CF) -c $(OPCION) DefVariables.f90
FlujosPac.o:	FlujosPac.f90
		$(CF) -c $(OPCION) FlujosPac.f90
FlujosPotenciaActiva.o:	FlujosPotenciaActiva.f90
			$(CF) -c $(OPCION) FlujosPotenciaActiva.f90
FormaMILP.o:    FormaMILP.f90
		$(CF) -c $(OPCION) FormaMILP.f90
FormaMILP_Hidro.o:    FormaMILP_Hidro.f90
			$(CF) -c $(OPCION) FormaMILP_Hidro.f90
FormaMILPUHI.o:	FormaMILPUHI.f90
		$(CF) -c $(OPCION) FormaMILPUHI.f90
FormaMILPURC.o:	FormaMILPURC.f90
		$(CF) -c $(OPCION) FormaMILPURC.f90
FormaMILPURD.o:	FormaMILPURD.f90
		$(CF) -c $(OPCION) FormaMILPURD.f90
FormaMILPURE.o:	FormaMILPURE.f90
		$(CF) -c $(OPCION) FormaMILPURE.f90
cplex_cons.o:   cplex_cons.f90
		$(CF) -c $(OPCION) cplex_cons.f90
cpx_forman.o:	cpx_forman.c
		$(CC) -c cpx_forman.c
getpid.o:       getpid.c
		$(CF) -c getpid.c
ImprimeAUHE.o:	ImprimeAUHE.f90
		$(CF) -c $(OPCION) ImprimeAUHE.f90
ImprimeAUHEH.o:	ImprimeAUHEH.f90
		$(CF) -c $(OPCION) ImprimeAUHEH.f90
ImprimeAUHERC.o:	ImprimeAUHERC.f90
			$(CF) -c $(OPCION) ImprimeAUHERC.f90
ImprimeAUHERD.o:	ImprimeAUHERD.f90
			$(CF) -c $(OPCION) ImprimeAUHERD.f90
ReportesCSV.o:	ReportesCSV.f90
		$(CF) -c $(OPCION) ReportesCSV.f90
LecDatRed.o:	LecDatRed.f90
		$(CF) -c $(OPCION) LecDatRed.f90
LecturasAUHE.o:	LecturasAUHE.f90
		$(CF) -c $(OPCION) LecturasAUHE.f90
LecAuHeHidro.o:	LecAuHeHidro.f90
		$(CF) -c $(OPCION) LecAuHeHidro.f90
LecUnidadesH.o:	LecUnidadesH.f90
		$(CF) -c $(OPCION) LecUnidadesH.f90
LecUnidadesRC.o:	LecUnidadesRC.f90
			$(CF) -c $(OPCION) LecUnidadesRC.f90
LecUnidadesRD.o:	LecUnidadesRD.f90
			$(CF) -c $(OPCION) LecUnidadesRD.f90
LecUnidadesRE.o:	LecUnidadesRE.f90
			$(CF) -c $(OPCION) LecUnidadesRE.f90
Validaciones.o:	Validaciones.f90
		$(CF) -c $(OPCION) Validaciones.f90
ParAUHE.o:	ParAUHE.f90
		$(CF) -c $(OPCION) ParAUHE.f90
ParAUHE_Hidro.o:	ParAUHE_Hidro.f90
			$(CF) -c $(OPCION) ParAUHE_Hidro.f90
ParAUHE_MILP.o:	ParAUHE_MILP.f90
		$(CF) -c $(OPCION) ParAUHE_MILP.f90
PreparaIntervalo.o:	PreparaIntervalo.f90
			$(CF) -c $(OPCION) PreparaIntervalo.f90
SenFluPac.o:	SenFluPac.f90
		$(CF) -c $(OPCION) SenFluPac.f90
SensibilidadesFluyPer.o:	SensibilidadesFluyPer.f90
				$(CF) -c $(OPCION) SensibilidadesFluyPer.f90
Open_CPLEX.o:	Open_CPLEX.f90
		$(CF) -c $(OPCION) Open_CPLEX.f90
Utilerias.o:	Utilerias.f90
		$(CF) -c $(OPCION) Utilerias.f90
Utility.o:	Utility.f90
		$(CF) -c $(OPCION) Utility.f90
symtypes.o:     symtypes.f90
		$(CF) -c $(OPCION) symtypes.f90
dumy.o:     dumy.f90
		$(CF) -c $(OPCION) dumy.f90
CondIniciales.o:    	CondIniciales.f90
			$(CF) -c $(OPCION) CondIniciales.f90
