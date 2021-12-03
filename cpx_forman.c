	/*********************************************************************/
	/* cpx_forman : Este modulo sirve de interface entre                 */
	/*              FORTRAN y CPLEX, contiene los llamados a cada una    */
	/*              de las funciones que se utilizan del CPLEX           */
	/*                                                                   */
	/* Ultima Revision: 10-jul-19    Moises Peralta O.                   */
        /*                               Jose Luis Ceciliano M.              */
	/*                : Creacion de las funciones de CPLEX               */
	/*********************************************************************/


	/* Incluir declaracion de funciones  del CPLEX */

        #include "/home/uriel/cplex1210/cplex/include/ilcplex/cplex.h"
	/*Incluir declaraciones para funciones string */

	#include <string.h>
//	#include <stdio.h>


	CPXENVptr env_glb;
	CPXLPptr lp_glb;
        

	CPXENVptr cpxopencplex_ (status)
	int *status;
	{
	  CPXENVptr env=NULL;

	   //printf("CPXopenCPLEX\n");
	    env_glb = CPXopenCPLEX (status) ;

	   if ( env_glb == NULL ) {
	      char  errmsg[1024];
	      printf ("  Falla CPXopenCPLEX:");
	      CPXgeterrorstring (env, *status, errmsg);
	      fprintf (stderr, "%s", errmsg);
	      printf("\n");
	   }
	   //else printf("  OK. ambiente cplex\n");


	    return ( env_glb );
	}

	int cpxclosecplex_ (env)
	CPXENVptr env;
	{
	  int status;

	    status = CPXcloseCPLEX (&env_glb) ;

	    return ( status );
	}

	int cpxsetintparam_ ( env, a, b )
	CPXENVptr env ;
	int *a   ;
	int *b   ;
	{
	   int status;


	   //  printf ( "CPXsetintparam\n");
	   //  printf ( "  CPXsetintparam ( a=%d, b=%d\n", *a, *b );

	     status = CPXsetintparam (env_glb, *a, *b ) ;

	     if ( status ) 
	      printf ( "  Falla CPXsetintparam, error %d.\n", status);
	     //else
	      //printf ("  OK. CPXsetintparam\n");

	     return ( status ) ;
	}


	CPXLPptr cpxcreateprob_ ( env, status, probname )
	CPXENVptr env  ;
	int *status    ;
	char *probname ;
	{
	   CPXLPptr lp;


	     //printf("CPXcreateprob\n");
	     //printf ("  status= %d\n",*status);
	     //printf ("  probname= %s\n",probname);
	     lp_glb = CPXcreateprob (env_glb, status, probname ) ;

	     if ( lp_glb == NULL ) 
	      fprintf (stderr, "  Falla CPXcreateprob\n");
	     //else
	      //printf("  OK. CPXcreateprob\n");

	     return ( lp_glb ) ;
	}


	int cpxnewcols_ ( env, lp, ccnt, obj, lb, ub, ctype, colname )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int       *ccnt ;
	double    *obj ;
	double    *lb  ;
	double    *ub  ;
	char      *ctype ;
	char      **colname ;

	{
	    int status;


	     //printf("CPXnewcols\n");
	     //printf("  ccnt=%d \n", *ccnt);
	     //printf("  obj[0]=%f,[1]=%f,[2]=%f,[3]=%f\n",obj[0],obj[1],obj[2],obj[3]);
	     //printf("  lb[0]=%f,[1]=%f,[2]=%f,[3]=%f\n",lb[0],lb[1],lb[2],lb[3]);
	     //printf("  ub[0]=%f,[1]=%f,[2]=%f,[3]=%f\n",ub[0],ub[1],ub[2],ub[3]);
	     //printf("  ctype= %c\n", ctype[0]);
	   
	     status = CPXnewcols ( env_glb, lp_glb, *ccnt, obj, lb, ub, ctype, NULL);

	      
	     if ( status ) 
	      printf ( "  Falla CPXnewcols, error %d.\n", status);
	     //else printf ("  OK. CPXnewcols\n");

	     return ( status ) ;
	}



	int cpxaddrows_ (env, lp, ccnt, rcnt, nzcnt, rhs, sense, rmatbeg, rmatind,
			 rmatval, colname, rowname )

	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int       *ccnt ;
	int       *rcnt ;
	int       *nzcnt ;
	double    *rhs  ;
	char      *sense ;
	int       *rmatbeg ;
	int       *rmatind ;
	double    *rmatval ;
	char      **colname ;
	char      **rowname ;
	{

	    int status;

	     //printf("CPXaddrows\n");
	     //printf("  ccnt=%d\n", *ccnt);
	     //printf("  rcnt=%d\n", *rcnt);
	     //printf("  nzcnt=%d\n", *nzcnt);
	     //printf("  rhs[0]=%f,[1]=%f,[2]=%f,[3]=%f\n",rhs[0],rhs[1],rhs[2],rhs[3]);
	     //printf("  sense= %c\n", sense[0]);
	     //printf("  rmatbeg[0]=%d,[1]=%d,[2]=%d\n",rmatbeg[0],rmatbeg[1],rmatbeg[2]);
	     //printf("  rmatind[0]=%d,[1]=%d,[2]=%d\n",rmatind[0],rmatind[1],rmatind[2]);
	     //printf("  rmatval[0]=%f,[1]=%f,[2]=%f\n",rmatval[0],rmatval[1],rmatval[2]);
	   
	     status = CPXaddrows ( env_glb, lp_glb, *ccnt, *rcnt, *nzcnt, rhs, sense,
				     rmatbeg, rmatind, rmatval, NULL, NULL);

	     if ( status ) 
	      printf ( "  Falla CPXaddrows, error %d.\n", status);
	     //else printf ("  OK. CPXaddrows\n");

	     return ( status ) ;
	   
	}


	int cpxcopyquad_ (env, lp, qmatbeg, qmatcnt, qmatind, qmatval )

	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int       *qmatbeg ;
	int       *qmatcnt ;
	int       *qmatind ;
	double    *qmatval ;

	{
	  int status;

	   //printf("CPXcopyquad\n");
	   //printf("  qmatbeg[0]=%d,[1]=%d,[2]=%d\n",qmatbeg[0],qmatbeg[1],qmatbeg[2]);
	   //printf("  qmatcnt[0]=%d,[1]=%d,[2]=%d\n",qmatcnt[0],qmatcnt[1],qmatcnt[2]);
	   //printf("  qmatind[0]=%d,[1]=%d,[2]=%d\n",qmatind[0],qmatind[1],qmatind[2]);
	   //printf("  qmatval[0]=%f,[1]=%f,[2]=%f\n",qmatval[0],qmatval[1],qmatval[2]);

	   status = CPXcopyquad (env_glb, lp_glb, qmatbeg, qmatcnt, qmatind, qmatval ) ;

	   if ( status ) 
	      printf ( "  Falla CPXcopyquad, error %d.\n", status);
	   //else printf ("  OK. CPXcopyquad\n");

	   return ( status ) ;
	}


	int cpxwriteprob_ (env, lp, filename_str, filetype_str )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	char      *filename_str ;
	char      *filetype_str ;
	{

	   int status;

	   //printf("CPXwriteprob\n");
	   //printf("  filename_str= %s\n",filename_str);
	   //printf("  filetype_str= %s\n",filetype_str);

	   status = CPXwriteprob (env_glb, lp_glb, filename_str, filetype_str ) ; 

	   if ( status ) 
	      printf ( "  Falla CPXwriteprob, error %d.\n", status);
	   //else printf ("  OK. CPXwriteprob\n");

	   return ( status ) ;
	}

	int cpxsolwrite_ (env, lp, filename_str)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	char      *filename_str ;
	{

	   int status;

	   //printf("CPXsolwrite\n");
	   //printf("  filename_str= %s\n",filename_str);

	   status = CPXsolwrite (env_glb, lp_glb, filename_str) ; 

	   if ( status ) 
	      printf ( "  Falla CPXsolwrite, error %d.\n", status);
	   //else printf ("  OK. CPXsolwrite\n");

	   return ( status ) ;
	}
	
        int cpxwritemipstarts_ (env, lp, filename_str, begin, end )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	char      *filename_str ;
	int       *begin ;
	int       *end ;
	{

	   int status;

	   status = CPXwritemipstarts (env_glb, lp_glb, filename_str, *begin, *end ) ; 

	   if ( status ) 
	   //   printf ( "  Falla CPXwritemipstarts, error %d.\n", status);
	   //else printf ("  OK. CPXwritemipstarts,\n");

	   return ( status ) ;
	}

	int cpxreadcopymipstarts_ (env, lp, filename_str )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	char      *filename_str ;
	{

	   int status;

	   status = CPXreadcopymipstarts (env_glb, lp_glb, filename_str ) ; 

	   if ( status ) 
	      printf ( "  Falla CPXreadcopymipstarts, error %d.\n", status);
	   //else printf ("  OK. CPXreadcopymipstarts,\n");

	   return ( status ) ;
	}

	int cpxchgobjsen_ (env, lp, maxormin)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int       *maxormin ;    
	{    
	      
	   int status;
	    /* Problem is maximization */
	      
	   //printf("CPXchgobjsen\n");
	   //printf("  maxormin= %d\n",*maxormin);

	   CPXchgobjsen (env_glb, lp_glb, *maxormin ) ;

	   return ( 0 ) ; /*Esta funcion no retorna nada ya que es tipo void */
	}

	int cpxbaropt_ (env, lp)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	{    
	      
	   int status;

	   //printf("CPXmipopt\n");

	   status = CPXbaropt (env_glb, lp_glb ) ;

	   if ( status )
	      printf ( "  Falla CPXbaropt, error %d.\n", status);
	   //else printf ("  OK. CPXbaropt\n");

	   return ( status ) ;
	}

	int cpxsolution_ (env, lp, solstat, fobj, x, pi, slack, dj)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int       *solstat;
	double    *fobj   ;
	double    *x      ;
	double    *pi     ;
	double    *slack  ;
	double    *dj     ;
	{

	   int       status ;
	   int       sol    ;
	   double    objval ;

	   //printf("CPXsolution\n");
	   //printf("  x[0]=%f,[1]=%f,[2]=%f\n",x[0],x[1],x[2]);
	   //printf("  pi[0]=%f,[1]=%f,[2]=%f\n",pi[0],pi[1],pi[2]);
	   //printf("  slack[0]=%f,[1]=%f,[2]=%f\n",slack[0],slack[1],slack[2]);
	   //printf("  dj[0]=%f,[1]=%f,[2]=%f\n",dj[0],dj[1],dj[2]);

	   status = CPXsolution (env_glb, lp_glb, &sol, &objval, x, pi, slack, dj ) ;

	   *solstat = sol ;
	   *fobj = objval ;
	   //printf("  solstat= %d\n",*solstat);
	   //printf("  fobj= %f\n", *fobj );

	   if ( status )
	      printf ( "  Falla CPXsolution, error %d.\n", status);
	   //else printf ("  OK. CPXsolution\n");

	   return ( status ) ;
	}


	int cpxfreeprob_ (env, lp)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	{    
	      
	   int status;

	   //printf("CPXfreeprob\n");

	   status = CPXfreeprob (env_glb, &lp_glb ) ;

	   if ( status )
	      printf ( "  Falla CPXfreeprob, error %d.\n", status);
	   //else printf ("  OK. CPXfreeprob\n");

	   return ( status ) ;
	}

	int cpxprimopt_ (env, lp)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	{

	   int status;

	   //printf("CPXprimopt\n");

	   status = CPXprimopt (env_glb, lp_glb ) ;

	   if ( status )
	      printf ( "  Falla CPXprimopt, error %d.\n", status);
	   //else printf ("  OK. CPXprimopt\n");

	   return ( status ) ;
	}

	int cpxdualopt_ (env, lp)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	{

	   int status;

	   //printf("CPXdualopt\n");

	   status = CPXdualopt (env_glb, lp_glb ) ;

	   if ( status )
	      printf ( "  Falla CPXdualopt, error %d.\n", status);
	   //else printf ("  OK. CPXdualopt\n");

	   return ( status ) ;
	}

	int cpxsetdblparam_ (env, wichparam, newvalue)
	CPXENVptr env  ;
	int       *wichparam;
	double    *newvalue;
	{

	   int status;

	   //printf("CPXsetdblparam\n");
	   //printf("  wichparam= %d\n", *wichparam);
	   //printf("  newvalue= %f\n", *newvalue);

	   status = CPXsetdblparam (env_glb, *wichparam, *newvalue ) ;


	   if ( status )
	      printf ( "  Falla CPXsetdblparam, error %d.\n", status);
	   //else printf ("  OK. CPXsetdblparam\n");

	   return ( status ) ;
	}

	int cpxmipopt_ (env, lp)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	{

	   int status;

	   //printf("CPXmipopt\n");

	   status = CPXmipopt (env_glb, lp_glb ) ;

	   if ( status )
	      printf ( "  Falla CPXmipopt, error %d.\n", status);
	   //else printf ("  OK. CPXmipopt\n");

	   return ( status ) ;
	}

	int cpxgetstat_ (env, lp)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	{

	   int solstat;

	   //printf("CPXgetstat\n");

	   solstat = CPXgetstat (env_glb, lp_glb ) ;

	   if ( solstat==102 )
	      printf ("  OK en su optimo. CPXgetstat\n");
	   //else printf ( "  Problema sin solucion CPXgetstat, error %d.\n", solstat);

	   return ( solstat ) ;
	}

	int cpxgetobjval_ (env, lp, fobjMa )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	double    *fobjMa ;
	{

	   int status       ;
	   double objval_p  ;

	   //printf("CPXgetobjval\n");

	   status = CPXgetobjval (env_glb, lp_glb, &objval_p ) ;
	   *fobjMa = objval_p ;
	   

	   if ( status )
	   //   printf ( "  Falla CPXgetobjval, error %d.\n", status);
	   //else printf ("  OK. CPXgetobjval\n");

	   return ( status ) ;
	}


	int cpxgetbestobjval_ (env, lp, fobjMa )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	double    *fobjMa ;
	{

	   int status       ;
	   double objval_p  ;

	   //printf("CPXgetbestobjval\n");

	   status = CPXgetbestobjval (env_glb, lp_glb, &objval_p ) ;
	   *fobjMa = objval_p ;
	   

	   if ( status )
	   //   printf ( "  Falla CPXgetbestobjval, error %d.\n", status);
	   //else printf ("  OK. CPXgetbestobjval\n");

	   return ( status ) ;
	}


	int cpxgetx_ (env, lp, x, begin, end)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	double    *x   ;
	int       *begin ;
	int       *end   ;
	{

	   int status;

	   //printf("CPXgetx\n");
	   //printf("  x[0]=%f,[1]=%f,[2]=%f\n",x[0],x[1],x[2]);
	   //printf("  begin= %d\n", *begin);
	   //printf("  end= %d\n", *end);

	   status = CPXgetx (env_glb, lp_glb, x, *begin, *end ) ;

	   if ( status )
	      printf ( "  Falla CPXgetx, error %d.\n", status);
	   //else printf ("  OK. CPXgetx\n");

	   return ( status ) ;
	}


	int cpxchgprobtype_ (env, lp, ctype)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int       *ctype ;
	{

	   int status;

	   //printf("CPXchgprobtype\n");
	   //printf("  end= %d\n", *ctype);

	   status = CPXchgprobtype (env_glb, lp_glb, *ctype ) ;

	   if ( status )
	      printf ( "  Falla CPXchgprobtype, error %d.\n", status);
	   //else printf ("  OK. CPXchgprobtype\n");

	   return ( status ) ;
	}

	int cpxaddqconstr_ (env, lp, linzcnt, quadnzcnt, rhs, sense,
			   linind, linval, quadrow, quadcol, quadval, lname_str)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int      *linzcnt   ;
	int      *quadnzcnt ;
	double   *rhs       ;
	int      *sense     ;
	int      *linind   ;
	double   *linval   ;
	int      *quadrow  ;
	int      *quadcol  ;
	double   *quadval  ;
	char     *lname_str ;
	{

	   int status;

	   //printf("CPXaddqconstr\n");

	   status = CPXaddqconstr (env_glb, lp_glb, *linzcnt, *quadnzcnt, *rhs, *sense,
			       linind, linval, quadrow, quadcol, quadval, NULL);

	   if ( status )
	      printf ( "  Falla CPXaddqconstr, error %d.\n", status);
	   //else printf ("  OK. CPXaddqconstr\n");

	   return ( status ) ;
	}




	int cpxdelrows_ (env, lp, begin, end)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int       *begin ;    
	int       *end   ;    
	{    
	      
	   int status;
	      

	   //printf(" CPXdelrows, begin= %d, end= %d", *begin, *end) ;

	   status = CPXdelrows (env_glb, lp_glb, *begin, *end) ;

	   return ( status ) ;
	}

	int cpxgetnumrows_ (env, lp)
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	{

	   int cur_numrows;


	   cur_numrows = CPXgetnumrows (env_glb, lp_glb) ;

	   return ( cur_numrows ) ;
	}

	int cpxrefineconflict_ (env, lp, confnumrows, confnumcols )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	int    *confnumrows ;
	int    *confnumcols ;
	{

	   int confnumrows_p ;
	   int confnumcols_p ;
	   int status;

	   //printf("CPXrefineconflict\n");

	   status = CPXrefineconflict (env_glb, lp_glb, &confnumrows_p, &confnumcols_p ) ;
	   *confnumrows = confnumrows_p ;
	   *confnumcols = confnumcols_p ;
	   

	   if ( status )
	      printf ( "  Falla CPXrefineconflict, error %d.\n", status);
	   //else printf ("  OK. CPXrefineconflict,\n");

	   return ( status ) ;
        }

	int cpxclpwrite_ (env, lp, filename_str )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	char      *filename_str ;
	{

	   int status;

	   status = CPXclpwrite (env_glb, lp_glb, filename_str ) ; 

	   if ( status ) 
	      printf ( "  Falla CPXclpwrite, error %d.\n", status);
	   //else printf ("  OK. CPXclpwrite,\n");

	   return ( status ) ;
	}


        int cpxchgobj_ (env, lp, ccnt, ind, val )
                                                                                                                     
        CPXENVptr env  ;
        CPXLPptr  lp   ;
        int       *ccnt ;
        int       *ind ;
        double    *val ;
        {
                                                                                                                     
            int status;
                                                                                                                     
             status = CPXchgobj ( env_glb, lp_glb, *ccnt, ind, val );
                                                                                                                     
             if ( status )
              printf ( "  Falla CPXchgobj, error %d.\n", status);
             //else printf ("  OK. CPXchgobj\n");
                                                                                                                     
             return ( status ) ;
                                                                                                                     
        }
 
        int cpxchgbds_ (env, lp, ccnt, ind, bndtypes, val )
                                                                                                                     
        CPXENVptr env  ;
        CPXLPptr  lp   ;
        int       *ccnt ;
        int       *ind ;
        double    *val ;
        char      *bndtypes ;
        {
                                                                                                                     
            int status;
                                                                                                                     
             status = CPXchgbds ( env_glb, lp_glb, *ccnt, ind, bndtypes, val );
                                                                                                                     
             if ( status )
              printf ( "  Falla CPXchgbds, error %d.\n", status);
             //else printf ("  OK. CPXchgbds\n");
                                                                                                                     
             return ( status ) ;
                                                                                                                     
        }

        int cpxchgrhs_ (env, lp, ccnt, ind, val )
                                                                                                                     
        CPXENVptr env  ;
        CPXLPptr  lp   ;
        int       *ccnt ;
        int       *ind ;
        double    *val ;
        {
                                                                                                                     
            int status;
                                                                                                                     
             status = CPXchgrhs ( env_glb, lp_glb, *ccnt, ind, val );
                                                                                                                     
             if ( status )
              printf ( "  Falla CPXchgrhs, error %d.\n", status);
             //else printf ("  OK. CPXchgrhs\n");
                                                                                                                     
             return ( status ) ;
                                                                                                                     
        }


        int cpxchgcoeflist_ (env, lp, ccnt, rowlist, collist, val )
                                                                                                                     
        CPXENVptr env  ;
        CPXLPptr  lp   ;
        int       *ccnt ;
        int       *rowlist ;
        int       *collist ;
        double    *val ;
        {
                                                                                                                     
            int status;
                                                                                                                     
             status = CPXchgcoeflist ( env_glb, lp_glb, *ccnt, rowlist, collist, val );
                                                                                                                     
             if ( status )
              printf ( "  Falla CPXchgcoeflist, error %d.\n", status);
             //else printf ("  OK. CPXchgcoeflist\n");
                                                                                                                     
             return ( status ) ;
                                                                                                                     
        }


        int cpxsetlogfilename_ (env, filename, action)
                                                                                                                     
        CPXENVptr env  ;
        char      *filename;
        char      *action;

        {
                                                                                                                     
            int status;
                                                                                                                     
             status = CPXsetlogfilename ( env_glb, filename, action );
                                                                                                                     
             if ( status )
		{
              printf (" !!! filename, %s.\n", filename);
              printf ( " !!! Falla CPXsetlogfilename, error %d.\n", status);
                }                                                                                             

             return ( status ) ;
                                                                                                                     
        }


	int cpxgetmiprelgap_ ( env, lp, gap )
	CPXENVptr env  ;
	CPXLPptr  lp   ;
	double    *gap ;

	{
	    int status;

	     status = CPXgetmiprelgap ( env_glb, lp_glb, gap );

	      
	     if ( status ) 
	      printf ( "  Falla CPXgetmiprelgap, error %d.\n", status);
	     //else printf ("  OK. CPXgetmiprelgap\n");

	     return ( status ) ;
      	}
