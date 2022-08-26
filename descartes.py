
## --------------------------------- SOFT-FIXING (deprecared)---------------------------------------------
        
## SOFT-FIXING solution and solve the sub-MILP. (Versión sin actualizar el cut-off)
# if 1 == 0:  # or precargado == False  
#     t_o = time.time() 
#     model,xx = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,SU,SD,RU,RD,pc_0,mpc,Pb,C,Cs,Tunder,option='Soft',SB_Uu=SB_Uu,nameins=instancia[0:4])
#     sol_soft = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:4],gap=gap,timelimit=timelimit,
#                           tee=False,emphasize=emph,tofiles=False,option='Soft')
#     z_soft,g_soft = sol_soft.solve_problem() 
#     t_soft        = time.time() - t_o + t_lp
#     print("t_soft= ",round(t_soft,1),"z_soft= ",round(z_soft,1),"g_soft= ",round(g_soft,5))
#     sol_soft.cuenta_ceros_a_unos(SB_Uu, No_SB_Uu, lower_Pmin_Uu,'Soft')

        
## -------------------------------- SOFT FIXING + Pmin (deprecared)------------------------------------
        
## SOFT FIX + Pmin solution and solve the sub-MILP (Versión sin actualizar el cut-off)
## Use 'Soft+pmin' if the lower subset of Uu-Pmin will be considered.
# if 1 == 0:  # or precargado == False  
#     t_o = time.time() 
#     model,xx     = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,SU,SD,RU,RD,pc_0,mpc,Pb,C,Cs,Tunder,option='Soft+pmin',SB_Uu=SB_Uu,lower_Pmin_Uu=lower_Pmin_Uu,nameins=instancia[0:4])
#     sol_softpmin = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:4],gap=gap,timelimit=timelimit,
#                              tee=False,emphasize=emph,tofiles=False,option='Soft+pmin')
#     z_softpmin,g_softpmin = sol_softpmin.solve_problem() 
#     t_softpmin            = time.time() - t_o + t_lp
#     print("t_soft+pmin= ",round(t_softpmin,4),"z_soft+pmin= ",round(z_softpmin,1),"g_soft+pmin= ",round(g_softpmin,5))
#     sol_softpmin.cuenta_ceros_a_unos(SB_Uu, No_SB_Uu, lower_Pmin_Uu,'Soft+pmin')
    

## -------------------------------- SOFT FIXING + CUT-OFF (deprecared)------------------------------------
        
## SOFT FIX + CUT-OFF solution and solve the sub-MILP (it is using cutoff ---> z_hard).
# if 1 == 0:  # or precargado == False  
#     t_o = time.time() 
#     model,xx    = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,SU,SD,RU,RD,pc_0,mpc,Pb,C,Cs,Tunder,option='Soft',SB_Uu=SB_Uu,nameins=instancia[0:4])
#     sol_softcut = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:4],gap=gap,cutoff=z_hard,timelimit=timelimit,
#                              tee=False,emphasize=emph,tofiles=False,option='Soft')
#     z_softcut,g_softcut = sol_softcut.solve_problem() 
#     t_softcut           = time.time() - t_o + t_hard ## t_hard (ya incluye el tiempo de lp)
#     print("t_soft+cut= ",round(t_softcut,4),"z_soft+cut= ",round(z_softcut,1),"g_soft+cut= ",round(g_softcut,5))
#     sol_softcut.cuenta_ceros_a_unos(SB_Uu, No_SB_Uu, lower_Pmin_Uu,'Soft+cut')
    
    
## -------------------------------- SOFT FIXING + CUT-OFF + Pmin (deprecared)------------------------------------
        
## SOFT FIX + CUT-OFF + Pmin solution and solve the sub-MILP (it is using cutoff ---> z_hard).
# if 1 == 0:  # or precargado == False  
#     t_o = time.time() 
#     model,xx     = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,SU,SD,RU,RD,pc_0,mpc,Pb,C,Cs,Tunder,option='Soft2',SB_Uu=SB_Uu,nameins=instancia[0:4])
#     sol_softcut2 = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:4],gap=gap,cutoff=z_hard,timelimit=timelimit,
#                              tee=False,emphasize=emph,tofiles=False,option='Soft2')
#     z_softcut2,g_softcut2 = sol_softcut2.solve_problem() 
#     t_softcut2            = time.time() - t_o + t_hard ## t_hard (ya incluye el tiempo de lp)
#     print("t_soft+cut+pmin= ",round(t_softcut2,4),"z_soft+cut+pmin= ",round(z_softcut2,1),"g_soft+cut+pmin= ",round(g_softcut2,5))
#     sol_softcut2.cuenta_ceros_a_unos(SB_Uu, No_SB_Uu, lower_Pmin_Uu,'Soft+pmin+cut')   
     
    
## --------------------------- SOFT FIXING + CUT-OFF + Pmin + FIXING_0 (deprecared)------------------------------------
        
## SOFT FIX + CUT-OFF + Pmin + FIXING_0  solution and solve the sub-MILP (it is using cutoff ---> z_hard).
# if 1 == 1:  # or precargado == False  
#     t_o = time.time() 
#     model,xx     = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,SU,SD,RU,RD,pc_0,mpc,Pb,C,Cs,Tunder,option='Soft3',SB_Uu=SB_Uu2,No_SB_Uu=No_SB_Uu2,lower_Pmin_Uu=lower_Pmin_Uu,nameins=instancia[0:4])
#     sol_softcut3 = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:4],gap=gap,cutoff=z_hard,timelimit=timelimit,
#                              tee=False,emphasize=emph,tofiles=False,option='Soft3')
#     z_softcut3,g_softcut3 = sol_softcut3.solve_problem() 
#     t_softcut3            = time.time() - t_o + t_hard ## t_hard (ya incluye el tiempo de lp)
#     print("t_soft+cut+pmin+fix0= ",round(t_softcut3,4),"z_soft+cut+pmin+fix0= ",round(z_softcut3,1),"g_soft+cut+pmin+fix0= ",round(g_softcut3,5))
    
#     sol_softcut3.cuenta_ceros_a_unos(SB_Uu, No_SB_Uu, lower_Pmin_Uu,'Soft+pmin+cut+fix0')


## En esta función seleccionamos el conjunto de variables delta que quedarán en uno/cero para ser fijadas posteriormente.
    # def select_fixed_variables_delta(self):    
    #     fixed_delta = []; No_fixed_delta = [] 
        
    #     parameter  = 0.9
    #     total      = 0
    #     nulos      = 0
    #     for g,t,s in self.model.indexGTSg:
    #         if self.model.delta[(g,t,s)].value != None:
                
    #             if self.model.delta[(g,t,s)].value >= parameter:
    #                 fixed_delta.append([g,t,s,1])
    #                 # print(g,t,s)                    
    #                 # print(self.model.delta[(g,t,s)].value)
    #             else:
    #                 No_fixed_delta.append([g,t,s,0])
    #         else: ## Si es None                
    #             fixed_delta.append([g,t,s,0])
    #             nulos = nulos + 1
    #         total = total + 1
                
    #     print('Total delta   =', total)  
    #     print('Nulos delta   =', nulos)  
    #     print('Fixed delta  >=', parameter,len(fixed_delta)-nulos)
        
    #     return fixed_delta, No_fixed_delta
    
    
    ## En esta función seleccionamos el conjunto de variables V,W que quedarán en uno/cero para ser fijadas posteriormente.
    # def select_fixed_variables_VW(self):    
        
    #     fixed_V   = []; No_fixed_V = []; fixed_W = []; No_fixed_W = []
        
    #     parameter = 0.9
    #     total     = 0
    #     for t in range(self.tt):
    #         for g in range(self.gg):
    #             if self.V[g][t] != None:
                
    #                 if self.V[g][t] >= parameter:
    #                     fixed_V.append([g,t,1])
    #                 else:
    #                     No_fixed_V.append([g,t,0])
                    
    #             if self.W[g][t] != None:
                
    #                 if self.W[g][t] >= parameter:
    #                     fixed_W.append([g,t,1])
    #                 else:
    #                     No_fixed_W.append([g,t,0])
    #             total = total + 1
                
    #     print('Total V,W   =',total)  
    #     print('Fixed V    >=', parameter, len(fixed_V)) 
    #     print('Fixed W    >=', parameter, len(fixed_W)) 
           
    #     return fixed_V, No_fixed_V, fixed_W, No_fixed_W


## ---------------------------- SOFT VARIABLE FIXING ------------------------------------------

    # ## Si se desea usar la solución fix y calcular un sub-MILP.
    # if(option == 'Soft' or option == 'Soft2' or option == 'Soft3'): 

    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval  ## Soft-fixing I                

    #     ## Adding a new restriction.  
    #     ## https://pyomo.readthedocs.io/en/stable/working_models.html
    #     ## Soft-fixing II
    #     model.cuts = pyo.ConstraintList()
    #     n_subset   = math.ceil( (percent_lbc/100) * len(SB_Uu))
    #     expr       = 0
        
    #     for f in SB_Uu:      
    #         expr += model.u[f[0]+1,f[1]+1]
        
    #     if(option == 'Soft2' or option == 'Soft3'):
    #         # \todo{DEMOSTRAR QUE NOS CONVIENE RELAJAR LOS INTENTOS DE ASIGNACIÓN EN EL SOFT-FIXING}
    #         for f in lower_Pmin_Uu: 
    #             model.u[f[0]+1,f[1]+1].domain = UnitInterval ## Soft-fixing
    #         for f in lower_Pmin_Uu:      
    #             expr += model.u[f[0]+1,f[1]+1]               ## New constraint soft.                        
    #     model.cuts.add(expr >= n_subset)                     ## Adding a new restriction.  
    #     #print('Soft: number of variables Uu that could be outside  the n_subset (',100-percent_lbc,'%): ', len(SB_Uu)-n_subset)

    # if(option == 'Soft3'):   
    #     print('Soft3 -estoy fijando a <0>: No_SB_Uu - lower_Pmin_Uu =',len(No_SB_Uu),'-',len(lower_Pmin_Uu))
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)  ## Hard fixing to 0
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].unfix()   ## Unfixing
    
    
    
## --------------------------------- HARD-FIXING U,V,W---------------------------------------------

# HARD-FIXING U,V,W solution and solve the sub-MILP. (deprecared by Uriel)
# if 1 == 0: # or precargado == False
#     t_o = time.time() 
#     model,xx = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,SU,SD,RU,RD,pc_0,mpc,Pb,C,Cs,Tunder,option='harduvwdel',SB_Uu=SB_Uu,fixed_V=fixed_V,fixed_W=fixed_W,fixed_delta=[],nameins=instancia[0:4])
#     sol_harduvw = Solution(model=model, env=ambiente, executable=executable, nameins=instancia[0:4], gap=gap, timelimit=timelimit,
#                              tee=False, tofiles=False)
#     z_harduvw = sol_harduvw.solve_problem()
#     t_harduvw = time.time() - t_o + t_lp
#     print("t_hardUVW = ",round(t_harduvw,1),"z_hardUVW = ",round(z_harduvw,1))


## --------------------------------- HARD-FIXING U,V,W y delta---------------------------------------------

# HARD-FIXING U,V,W y delta solution and solve the sub-MILP. (deprecared by Uriel)
# if 1 == 0: # or precargado == False
#     t_o = time.time() 
#     model,xx = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,SU,SD,RU,RD,pc_0,mpc,Pb,C,Cs,Tunder,option='harduvwdel',SB_Uu=SB_Uu,fixed_V=fixed_V,fixed_W=fixed_W,fixed_delta=fixed_delta,nameins=instancia[0:4])
#     sol_harduvwdel = Solution(model=model, env=ambiente, executable=executable, nameins=instancia[0:4], gap=gap, timelimit=timelimit,
#                                 tee=False, tofiles=False)
#     z_harduvwdel = sol_harduvwdel.solve_problem()
#     t_harduvwdel = time.time() - t_o + t_lp
#     print("t_hardUVWdel = ",round(t_harduvwdel,1),"z_hardUVWdel = ",round(z_harduvwdel,1))

## ----------------------------------- HARD-FIXING 2 (only Uu) ---------------------------------------------
## HARD-FIXING (only Uu) solution and solve the sub-MILP. (Require run the LP and HF1)
## FIX-->No_SB_Uu2, UNFIX-->[No_SB_Uu2,lower_Pmin_Uu2]
# if False: 
#     t_o = time.time() 
#     timehard2 = timeheu
#     model,xx = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,TD_0,SU,SD,RU,RD,p_0,mpc,Pb,Cb,C,Cs,Tunder,names,option='Hard2',SB_Uu=SB_Uu2,No_SB_Uu=No_SB_Uu2,lower_Pmin_Uu=lower_Pmin_Uu2,nameins=instancia[0:5],mode="Tight")
#     sol_hard2 = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:5],gap=gap,timelimit=timehard2,
#                         tee=False,emphasize=emph,tofiles=False,option='Hard2')
#     z_hard2,g_hard2 = sol_hard2.solve_problem()
#     t_hard2         = time.time() - t_o + t_lp
#     print("t_hard2= ",round(t_hard2,1),"z_hard2= ",round(z_hard2,1),"g_hard2= ",round(g_hard2,5) )


    ## --------------------------- HARD VARIABLE FIXING 2 ----------------------------------------

    # ## Si se desea fijar LR->SB y resolver un sub-MILP.
    # if option == 'Hard2':    
    #     for f in No_SB_Uu: 
    #         model.u[f[0]+1,f[1]+1].fix(0) ## Hard fixing
    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #         model.u[f[0]+1,f[1]+1] = 1
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].unfix() ## Unfixing 



    ## ---------------------------- HARD VARIABLE FIXING (deprecared) ------------------------------------------    
    # ## Si se desea usar la solución fix y calcular un sub-MILP.
    # if(option == 'Hard'):    
    #     for f in SB_Uu: 
    #         model.u[f[0]+1,f[1]+1].fix(1)  ## Hard fixing
    # if(option == 'HardUVWdelta'):   (deprecared)         
    #     for f in SB_Uu: 
    #         model.u[f[0]+1,f[1]+1].fix(1)  ## Hard fixing
    #     for f in fixed_V: 
    #         model.v[f[0]+1,f[1]+1].fix(1)  ## Hard fixing
    #     for f in fixed_W: 
    #          model.w[f[0]+1,f[1]+1].fix(1) ## Hard fixing   
    #     for f in fixed_delta: 
    #          model.delta[f[0],f[1],f[2]].fix(0) ## Hard fixing   
    
        ######################################################################
        ## Con este código corren todas las instancias a factibilidad (menos 52)
        ## Se incrementaba la potencia de t_o de los generadores prendidos 
        ## y los que están abajo del mínimo los pone a cero.    
        ## Es decir, no considera la potencia de arranque.
        # if False: ## (Original que está mal)
        #     aux = power_output_t0[i-1] - power_output_minimum[i-1]
        #     if aux<0:
        #         aux=0
        #     p_0_list.append(aux)
        ######################################################################
        
        
        
    ## ---------------------------- SOFT0 FIXING ------------------------------------------
    
    ## Relajamos la restricción de integralidad de las variables 'Uu' candidatas a '1' en la relajación lineal
    ## y fijamos la solución a cero de las variables fuera del Soporte Binario
    ## Liberamos las variables candidatas por LP 'lower_Pmin_Uu'
    ## Sin ninguna restricción de n_subset=90%   
    # if(option == 'Soft0'):    
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)                ## Hard fixing to '0' those elements outside of Sopport Binary      
    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## We remove the integrality constraint of the Binary Support 
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## Soft-fixing I
    #         model.u[f[0]+1,f[1]+1].unfix()               ## Unfixing


    ## ---------------------------- SOFT4 & SOFT7 FIXING ------------------------------------------
    
    # ## Relajamos la restricción de integralidad de las variables 'Uu' candidatas a '1' en la relajación lineal.
    # ## Hacemos que el 90% de las variables del soporte SB_Uu sigan en el.
    # ## Liberamos las variables candidatas por LP 'lower_Pmin_Uu'
    # if(option == 'Soft4' or option == 'Soft7'):    
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)                ## Hard fixing to '0' those elements outside of Sopport Binary      
    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## We remove the integrality constraint of the Binary Support 
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## Soft-fixing I
    #         model.u[f[0]+1,f[1]+1].unfix()               ## Unfixing
    #     ## Adding a new restriction.  
    #     ## https://pyomo.readthedocs.io/en/stable/working_models.html
    #     ## Soft-fixing II
    #     model.cuts = pyo.ConstraintList()
    #     n_subset   = math.ceil((percent_lbc/100) * (len(SB_Uu))) #-len(lower_Pmin_Uu)
    #     expr       = 0        
    #     ## Se hace n_subset = 90% solo a el Soporte Binario
    #     for f in SB_Uu:      
    #         expr += model.u[f[0]+1,f[1]+1]
    #     model.cuts.add(expr >= n_subset)      


    # ## ---------------------------- SOFT5 FIXING ------------------------------------------
    
    # ## Relajamos la restricción de integralidad de las variables 'Uu' candidatas a '1' en la relajación lineal.
    # ## Hacemos que el 90% de las variables del Soporte  Binario 'SB_Uu' sigan en el.Además de los candidatos 'lower_Pmin_Uu' identificados en la LR
    # ## Por último liberamos las variables candidatas por LP 'lower_Pmin_Uu'
    # if(option == 'Soft5'):    
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)                ## Hard fixing to '0' those elements outside of Sopport Binary      
    #     for f in SB_Uu:           ## SB
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## We remove the integrality constraint of the Binary Support 
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #     for f in lower_Pmin_Uu:   ## B
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## Soft-fixing I
    #         model.u[f[0]+1,f[1]+1].unfix()               ## Unfixing
    #     ## Adding a new restriction.  
    #     ## https://pyomo.readthedocs.io/en/stable/working_models.html
    #     ## Soft-fixing II
    #     model.cuts = pyo.ConstraintList()
    #     n_subset   = math.ceil((percent_lbc/100) * len(SB_Uu))
    #     expr       = 0        
    #     ## Se hace n_subset=90% al Soporte Binario 'SB_Uu' y a Candidatos 'lower_Pmin_Uu' identificados en LR
    #     for f in SB_Uu:           ##SB
    #         expr += model.u[f[0]+1,f[1]+1]
    #     for f in lower_Pmin_Uu:   ##B
    #         expr += model.u[f[0]+1,f[1]+1]      
    #     model.cuts.add(expr >= n_subset)        
                        
    # ## ---------------------------- PURE-SOFT FIXING ------------------------------------------
    
    # ## Relajamos la restricción de integralidad de las variables 'Uu' candidatas a '1' en la relajación lineal
    # ## y fijamos la solución a cero de las variables fuera del Soporte Binario
    # ## Liberamos las variables candidatas por LP 'lower_Pmin_Uu'
    # if(option == 'Softp'):    
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)                ## Hard fixing to '0' those elements outside of Sopport Binary      
    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## We remove the integrality constraint of the Binary Support 
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## Soft-fixing I
    #         model.u[f[0]+1,f[1]+1].unfix()               ## Unfixing
    #     ## Adding a new restriction.  
    #     ## https://pyomo.readthedocs.io/en/stable/working_models.html
    #     ## Soft-fixing II
    #     model.cuts = pyo.ConstraintList()
    #     n_subset   = math.ceil((percent_lbc/100) * (len(SB_Uu))) #-len(lower_Pmin_Uu)
    #     expr       = 0        
    #     ## Se hace n_subset=90% solo a el - Soporte Binario -  
    #     for f in SB_Uu:      
    #         expr += model.u[f[0]+1,f[1]+1]
    #     model.cuts.add(expr >= n_subset)      

    ## ---------------------------- LOCAL BRANCHING CONSTRAINT LBC 0------------------------------------------
    
    # ## Define a neighbourhood with LBC0.
    # if(option == 'lbc0'):   
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)                ## Hard fixing to '0' those elements outside of Sopport Binary      
    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## We remove the integrality constraint of the Binary Support 
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].domain = UnitInterval ## Soft-fixing I
    #         model.u[f[0]+1,f[1]+1].unfix()               ## Unfixing
    #     ## Adding a new restriction.  
    #     ## https://pyomo.readthedocs.io/en/stable/working_models.html
    #     ## Soft-fixing II
    #     model.cuts = pyo.ConstraintList()
    #     n_subset   = math.ceil((percent_lbc/100) * (len(SB_Uu))) #-len(lower_Pmin_Uu)
    #     expr       = 0        
    #     ## Se hace n_subset=90% solo a el - Soporte Binario -  
    #     for f in SB_Uu:      
    #         expr += model.u[f[0]+1,f[1]+1]
    #     model.cuts.add(expr >= n_subset)                                         
    #     #print('LBC: number of variables Uu that may be into the n_subset (',percent_lbc,'%): ', n_subset)
    #     outside90 = len(SB_Uu)-n_subset
    #     print(option+' number of variables Uu that may be outside of Binary Support (',100-percent_lbc,'%): ',outside90 )
        
    #     ## Local Branching Cut
    #     expr = 0        
    #     for f in SB_Uu:                         ## Cuenta los cambios de 1 --> 0  
    #         expr += 1 - model.u[f[0]+1,f[1]+1] 
    #     for f in lower_Pmin_Uu:                 ## Cuenta los cambios de 0 --> 1
    #         expr +=     model.u[f[0]+1,f[1]+1]            
    #     model.cuts.add(expr <= k)               ## Adding a new restrictions (lbc0). 
    
    

    ## ---------------------------- LOCAL BRANCHING CONSTRAINT LBC 2------------------------------------------
    
    ## Define a neighbourhood with LBC2.
    # if(option == 'lbc2'):     
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)       ## Hard fixing to '0' those elements outside of Binary Sopport  
    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].domain = Binary
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #         model.u[f[0]+1,f[1]+1] = 1 
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].domain = Binary 
    #         model.u[f[0]+1,f[1]+1].unfix()      ## Unfixing
    #         model.u[f[0]+1,f[1]+1] = 0            
    #     ## Adding a new restriction.  
    #     ## https://pyomo.readthedocs.io/en/stable/working_models.html
    #     ## Soft-fixing II
    #     model.cuts = pyo.ConstraintList()
    #     n_subset   = math.ceil((percent_lbc/100) * (len(SB_Uu))) #-len(lower_Pmin_Uu)
    #     expr       = 0        
    #     ## Se hace n_subset=90% solo a el - Soporte Binario -  
    #     for f in SB_Uu:      
    #         expr += model.u[f[0]+1,f[1]+1]
    #     model.cuts.add(expr >= n_subset)                                         
    #     #print('LBC: number of variables Uu that may be into the n_subset (',percent_lbc,'%): ', n_subset)
    #     outside90 = len(SB_Uu)-n_subset
    #     print(option+' number of variables Uu that may be outside of Binary Support (',100-percent_lbc,'%): ',outside90 )
        
    #     ## Local branching constraint
    #     expr = 0        
    #     for f in SB_Uu:                         ## Cuenta los cambios de 1 --> 0  
    #         expr += 1 - model.u[f[0]+1,f[1]+1] 
    #     for f in lower_Pmin_Uu:                 ## Cuenta los cambios de 0 --> 1
    #         expr +=     model.u[f[0]+1,f[1]+1]            
    #     model.cuts.add(expr <= k)               ## Adding a new restrictions (lbc0).
        
                
        
    ## ---------------------------- LOCAL BRANCHING CONSTRAINT LBC 8------------------------------------------
    
    ## Define a neighbourhood with LBC8.
    # if(option == 'lbc8'):   
    #     for f in No_SB_Uu:
    #         model.u[f[0]+1,f[1]+1].fix(0)          ## Hard fixing to '0' those elements outside of Sopport Binary      
    #     for f in SB_Uu:  
    #         model.u[f[0]+1,f[1]+1].domain = Binary 
    #         model.u[f[0]+1,f[1]+1].unfix() 
    #     for f in lower_Pmin_Uu:
    #         model.u[f[0]+1,f[1]+1].domain = Binary 
    #         model.u[f[0]+1,f[1]+1].unfix()         ## Unfixing
    #     ## Adding a new restriction.  
    #     ## https://pyomo.readthedocs.io/en/stable/working_models.html
    #     ## Soft-fixing II
    #     model.cuts = pyo.ConstraintList()
    #     n_subset   = math.ceil((percent_lbc/100) * (len(SB_Uu))) #-len(lower_Pmin_Uu)
    #     expr       = 0        
    #     ## Se hace n_subset=90% solo a el - Soporte Binario -  
    #     for f in SB_Uu:      
    #         expr += model.u[f[0]+1,f[1]+1]
    #     model.cuts.add(expr >= n_subset)                                         
    #     #print('LBC: number of variables Uu that may be into the n_subset (',percent_lbc,'%): ', n_subset)
    #     outside90 = len(SB_Uu)-n_subset
    #     print(option+' number of variables Uu that may be outside of Binary Support (',100-percent_lbc,'%): ',outside90 )
        
    #     ## Local Branching Cut
    #     expr = 0        
    #     for f in SB_Uu:                         ## Cuenta los cambios de 1 --> 0  
    #         expr += 1 - model.u[f[0]+1,f[1]+1] 
    #     for f in lower_Pmin_Uu:                 ## Cuenta los cambios de 0 --> 1
    #         expr +=     model.u[f[0]+1,f[1]+1]            
    #     model.cuts.add(expr <= k)               ## Adding a new restrictions (lbc8). 
    
    ## --------------------------------------- LOCAL BRANCHING 2 ------------------------------------------
## LBC INTEGER VERSION OF Uu without soft-fixing
## Include the LOCAL BRANCHING CUT to the solution and solve the sub-MILP (it is using cutoff=z_hard).
# if False:
#     SB_Uu3 = SB_Uu2.copy()
#     No_SB_Uu3 = No_SB_Uu2.copy()
#     lower_Pmin_Uu3 = lower_Pmin_Uu2.copy()
#     for iter in range(10):
#         t_o = time.time() 
#         model,xx = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,TD_0,SU,SD,RU,RD,p_0,mpc,Pb,Cb,C,Cs,Tunder,names,option='lbc2',SB_Uu=SB_Uu3,No_SB_Uu=No_SB_Uu3,lower_Pmin_Uu=lower_Pmin_Uu3,nameins=instancia[0:5],mode="Tight")
#         sol_lbc2 = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:5],gap=gap,cutoff=z_hard,timelimit=timeheu,
#                             tee=False,emphasize=emph,tofiles=False,option='lbc2')
#         z_lbc2,g_lbc2 = sol_lbc2.solve_problem()
#         t_lbc2         = time.time() - t_o + t_hard ## t_hard ya incluye el tiempo de lp
#         print("iter:"+str(iter)+" t_lbc2= ",round(t_lbc2,1),"z_lbc2= ",round(z_lbc2,1),"g_lbc2= ",round(g_lbc2,5) )
        
#         sol_lbc2.cuenta_ceros_a_unos(SB_Uu3, No_SB_Uu3, lower_Pmin_Uu3,'lbc2')
#         SB_Uu3, No_SB_Uu3, xx = sol_hard.select_binary_support_Uu('')    
#         lower_Pmin_Uu3 = sol_lbc2.update_lower_Pmin_Uu(lower_Pmin_Uu3,'lbc2')
        
                    
# ## --------------------------------------- LOCAL BRANCHING 8 ------------------------------------------
# ## Include the LOCAL BRANCHING CUT to the solution and solve the sub-MILP (it is using cutoff=z_hard).
# ## Binary 'Uu'
# if False:
#     SB_Uu3 = SB_Uu2.copy()
#     No_SB_Uu3 = No_SB_Uu2.copy()
#     lower_Pmin_Uu3 = lower_Pmin_Uu2.copy()   
#     t_o = time.time() 
#     model,xx = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,TD_0,SU,SD,RU,RD,p_0,mpc,Pb,Cb,C,Cs,Tunder,names,option='lbc8',SB_Uu=SB_Uu3,No_SB_Uu=No_SB_Uu3,lower_Pmin_Uu=lower_Pmin_Uu3,nameins=instancia[0:5],mode="Tight")
#     sol_lbc8 = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:5],gap=gap,cutoff=z_hard,timelimit=timeheu,
#                         tee=False,emphasize=emph,tofiles=False,option='lbc8')
#     z_lbc8,g_lbc8 = sol_lbc8.solve_problem()
#     t_lbc8         = time.time() - t_o + t_hard ## t_hard ya incluye el tiempo de lp
#     print("t_lbc8= ",round(t_lbc8,1),"z_lbc8= ",round(z_lbc8,1),"g_lbc8= ",round(g_lbc8,5) )
    
#     sol_lbc8.update_lower_Pmin_Uu(lower_Pmin_Uu2,'lbc8')
#     sol_lbc8.cuenta_ceros_a_unos(SB_Uu2, No_SB_Uu2, lower_Pmin_Uu2,'lbc8')

## --------------------------------------- LOCAL BRANCHING 0 ------------------------------------------
## Include the LOCAL BRANCHING CUT to the solution and solve the sub-MILP (it is using cutoff=z_hard).
## UnitInterval 'Uu'
## The first iteration of LB 
# if False:
#     SB_Uu3 = SB_Uu2.copy()
#     No_SB_Uu3 = No_SB_Uu2.copy()
#     lower_Pmin_Uu3 = lower_Pmin_Uu2.copy()   
#     t_o = time.time() 
#     model,xx = uc_Co.uc(G,T,L,S,Pmax,Pmin,TU,TD,De,R,u_0,U,D,TD_0,SU,SD,RU,RD,p_0,mpc,Pb,Cb,C,Cs,Tunder,names,option='lbc0',SB_Uu=SB_Uu3,No_SB_Uu=No_SB_Uu3,lower_Pmin_Uu=lower_Pmin_Uu3,nameins=instancia[0:5],mode="Tight")
#     sol_lbc0 = Solution(model=model,env=ambiente,executable=executable,nameins=instancia[0:5],gap=gap,cutoff=z_hard,timelimit=timeheu,
#                         tee=False,emphasize=emph,tofiles=False,option='lbc0')
#     z_lbc0,g_lbc0 = sol_lbc0.solve_problem()
#     t_lbc0         = time.time() - t_o + t_hard ## t_hard ya incluye el tiempo de lp
#     print("t_lbc0= ",round(t_lbc0,1),"z_lbc0= ",round(z_lbc0,1),"g_lbc0= ",round(g_lbc0,5) )
    
#     sol_lbc0.update_lower_Pmin_Uu(lower_Pmin_Uu2,'lbc0')
#     sol_lbc0.cuenta_ceros_a_unos(SB_Uu2, No_SB_Uu2, lower_Pmin_Uu2,'lbc0')