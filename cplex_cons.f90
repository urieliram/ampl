!
      MODULE cplex_cons
!
!  Selected constants from the CPLEX header file
!
!  NOTE:  '!***' Following a parameter record indicates the parameter name
!                 has been truncated to 31 characters for Fortran 90
!
!  Incorporate symbolic types module
!
      use symtypes, only:  IL, RD
!
!  The version is an integer, to allow testing in user include files              
!
      integer(IL), parameter :: CPX_VERSION                     =   600
!
!  CPX_INFBOUND:  Any bound bigger than this is treated as infinity               
!
      real(RD), parameter ::    CPX_INFBOUND                    = 1.0d20
!
!  Maximum size for CPLEX string (note CPLEX names have max size of 16 -
!   see the parameter following this next one)           
!
      integer(IL), parameter :: CPX_STR_PARAM_MAX               =   512
!
!  Add a parameter for the reported maximum length for a CPLEX name -
!   see paragraph 'Character Strings' on page 161 of 'Using the CPLEX
!   Callable Library', Version 5.0, CPLEX, Inc., 1997 (which is not
!   contradicted by anything in the 'CPLEX 6.0 Documentation Supplement',
!   1998)
!
      integer(IL), parameter :: CPX_MAX_NAME_LEN                =    16
!
!  Values returned for 'stat' by solution ()                                      
!
      integer(IL), parameter :: CPX_OPTIMAL                     =     1
      integer(IL), parameter :: CPX_INFEASIBLE                  =     2
      integer(IL), parameter :: CPX_UNBOUNDED                   =     3
      integer(IL), parameter :: CPX_OBJ_LIM                     =     4
      integer(IL), parameter :: CPX_IT_LIM_FEAS                 =     5
      integer(IL), parameter :: CPX_IT_LIM_INFEAS               =     6
      integer(IL), parameter :: CPX_TIME_LIM_FEAS               =     7
      integer(IL), parameter :: CPX_TIME_LIM_INFEAS             =     8
      integer(IL), parameter :: CPX_NUM_BEST_FEAS               =     9
      integer(IL), parameter :: CPX_NUM_BEST_INFEAS             =    10
      integer(IL), parameter :: CPX_OPTIMAL_INFEAS              =    11
      integer(IL), parameter :: CPX_ABORT_FEAS                  =    12
      integer(IL), parameter :: CPX_ABORT_INFEAS                =    13
      integer(IL), parameter :: CPX_ABORT_DUAL_INFEAS           =    14
      integer(IL), parameter :: CPX_ABORT_PRIM_INFEAS           =    15
      integer(IL), parameter :: CPX_ABORT_PRIM_DUAL_INFEAS      =    16
      integer(IL), parameter :: CPX_ABORT_PRIM_DUAL_FEAS        =    17
      integer(IL), parameter :: CPX_ABORT_CROSSOVER             =    18
!
!  Error codes                                                                    
!
      integer(IL), parameter :: CPXERR_NO_MEMORY                =  1001
      integer(IL), parameter :: CPXERR_NO_ENVIRONMENT           =  1002
      integer(IL), parameter :: CPXERR_BAD_ARGUMENT             =  1003
      integer(IL), parameter :: CPXERR_NULL_POINTER             =  1004
      integer(IL), parameter :: CPXERR_POINTER_MISMATCH         =  1005
      integer(IL), parameter :: CPXERR_CALLBACK                 =  1006
      integer(IL), parameter :: CPXERR_POSITIVE_SPACE           =  1007
      integer(IL), parameter :: CPXERR_MEMORY_MODEL             =  1008
      integer(IL), parameter :: CPXERR_NO_PROBLEM               =  1009
      integer(IL), parameter :: CPXERR_LIMITS_TOO_BIG           =  1012
      integer(IL), parameter :: CPXERR_BAD_PARAM_NUM            =  1013
      integer(IL), parameter :: CPXERR_PARAM_TOO_SMALL          =  1014
      integer(IL), parameter :: CPXERR_PARAM_TOO_BIG            =  1015
      integer(IL), parameter :: CPXERR_PROMOTION_VERSION        =  1016
      integer(IL), parameter :: CPXERR_NOT_FOR_MIP              =  1017
      integer(IL), parameter :: CPXERR_NOT_FOR_QP               =  1018
      integer(IL), parameter :: CPXERR_CHILD_OF_CHILD           =  1019
      integer(IL), parameter :: CPXERR_TOO_MANY_THREADS         =  1020
      integer(IL), parameter :: CPXERR_CANT_CLOSE_CHILD         =  1021
      integer(IL), parameter :: CPXERR_BAD_PROB_TYPE            =  1022
      integer(IL), parameter :: CPXERR_NOT_ONE_PROBLEM          =  1023
      integer(IL), parameter :: CPXERR_NOT_MIPCLASS             =  1024
      integer(IL), parameter :: CPXERR_NOT_QPCLASS              =  1025
      integer(IL), parameter :: CPXERR_STR_PARAM_TOO_LONG       =  1026
      integer(IL), parameter :: CPXERR_MSG_NO_CHANNEL           =  1051
      integer(IL), parameter :: CPXERR_MSG_NO_FILEPTR           =  1052
      integer(IL), parameter :: CPXERR_MSG_NO_FUNCTION          =  1053
      integer(IL), parameter :: CPXERR_BOUNDS_INFEAS            =  1100
      integer(IL), parameter :: CPXERR_PRESLV_INForUNBD         =  1101
      integer(IL), parameter :: CPXERR_PRESLV_NO_PROB           =  1103
      integer(IL), parameter :: CPXERR_PRESLV_ABORT             =  1106
      integer(IL), parameter :: CPXERR_PRESLV_BASIS_MEM         =  1107
      integer(IL), parameter :: CPXERR_PRESLV_COPYSOS           =  1108
      integer(IL), parameter :: CPXERR_PRESLV_COPYORDER         =  1109
      integer(IL), parameter :: CPXERR_PRESLV_SOLN_MIP          =  1110
      integer(IL), parameter :: CPXERR_PRESLV_SOLN_QP           =  1111
      integer(IL), parameter :: CPXERR_PRESLV_START_LP          =  1112
      integer(IL), parameter :: CPXERR_PRESLV_FAIL_BASIS        =  1114
      integer(IL), parameter :: CPXERR_PRESLV_NO_BASIS          =  1115
      integer(IL), parameter :: CPXERR_PRESLV_COPYMIPSTART      =  1116
!
!  Callable library miscellaneous routines                                        
!
      integer(IL), parameter :: CPX_PARAM_CLONELOG              =  1132
      integer(IL), parameter :: CPXERR_INDEX_RANGE              =  1200
      integer(IL), parameter :: CPXERR_COL_INDEX_RANGE          =  1201
      integer(IL), parameter :: CPXERR_COL_NEW_INDEX_RANGE      =  1202
      integer(IL), parameter :: CPXERR_ROW_INDEX_RANGE          =  1203
      integer(IL), parameter :: CPXERR_ROW_NEW_INDEX_RANGE      =  1204
      integer(IL), parameter :: CPXERR_INDEX_RANGE_LOW          =  1205
      integer(IL), parameter :: CPXERR_INDEX_RANGE_HIGH         =  1206
      integer(IL), parameter :: CPXERR_NEGATIVE_SURPLUS         =  1207
      integer(IL), parameter :: CPXERR_ARRAY_TOO_LONG           =  1208
      integer(IL), parameter :: CPXERR_NAME_CREATION            =  1209
      integer(IL), parameter :: CPXERR_NAME_NOT_FOUND           =  1210
      integer(IL), parameter :: CPXERR_NO_RHS_IN_OBJ            =  1211
      integer(IL), parameter :: CPXERR_CANT_REALLOC_RIMS        =  1214
      integer(IL), parameter :: CPXERR_BAD_SENSE                =  1215
      integer(IL), parameter :: CPXERR_NO_RNGVAL                =  1216
      integer(IL), parameter :: CPXERR_NO_SOLN                  =  1217
      integer(IL), parameter :: CPXERR_NO_RIM                   =  1218
      integer(IL), parameter :: CPXERR_NO_NAMES                 =  1219
      integer(IL), parameter :: CPXERR_NO_OBJ_NAME              =  1220
      integer(IL), parameter :: CPXERR_NOT_FIXED                =  1221
      integer(IL), parameter :: CPXERR_DUP_ENTRY                =  1222
!
!  Simplex related                                                                
!
      integer(IL), parameter :: CPXERR_INDEX_NOT_BASIC          =  1251
      integer(IL), parameter :: CPXERR_NEED_OPT_SOLN            =  1252
      integer(IL), parameter :: CPXERR_BAD_STATUS               =  1253
      integer(IL), parameter :: CPXERR_NOT_UNBOUNDED            =  1254
      integer(IL), parameter :: CPXERR_SBASE_INCOMPAT           =  1255
      integer(IL), parameter :: CPXERR_SINGULAR                 =  1256
      integer(IL), parameter :: CPXERR_PRIIND                   =  1257
      integer(IL), parameter :: CPXERR_NO_LU_FACTOR             =  1258
      integer(IL), parameter :: CPXERR_NO_SENSIT                =  1260
      integer(IL), parameter :: CPXERR_NO_BASIC_SOLN            =  1261
      integer(IL), parameter :: CPXERR_NO_BASIS                 =  1262
      integer(IL), parameter :: CPXERR_ABORT_STRONGBRANCH       =  1263
      integer(IL), parameter :: CPXERR_NO_NORMS                 =  1264
      integer(IL), parameter :: CPXERR_NOT_DUAL_UNBOUNDED       =  1265
!
!  Network related                                                                
!
      integer(IL), parameter :: CPXERR_NET_SMALL                =  1290
      integer(IL), parameter :: CPXERR_NET_IMBALANCE            =  1291
      integer(IL), parameter :: CPXERR_BAD_METHOD               =  1292
!
!  Space related.  Only CPXERR_NO_SPACE should be returned                        
!
      integer(IL), parameter :: CPXERR_NO_SPACE                 =  1401
      integer(IL), parameter :: CPXERR_NO_SPACE_FREE            =  1402
      integer(IL), parameter :: CPXERR_NO_SPACE_FREE_NZ         =  1403
      integer(IL), parameter :: CPXERR_NO_SPACE_FREE_NAM        =  1404
      integer(IL), parameter :: CPXERR_NO_SPACE_NAMES           =  1406
!
!  For readers and writers                                                        
!
      integer(IL), parameter :: CPXERR_NO_FILENAME              =  1421
      integer(IL), parameter :: CPXERR_FAIL_OPEN_WRITE          =  1422
      integer(IL), parameter :: CPXERR_FAIL_OPEN_READ           =  1423
      integer(IL), parameter :: CPXERR_BAD_FILETYPE             =  1424
!
!  Common to LP, MPS, and related readers                                         
!
      integer(IL), parameter :: CPXERR_TOO_MANY_ROWS            =  1431
      integer(IL), parameter :: CPXERR_TOO_MANY_COLS            =  1432
      integer(IL), parameter :: CPXERR_TOO_MANY_COEFFS          =  1433
      integer(IL), parameter :: CPXERR_BAD_NUMBER               =  1434
      integer(IL), parameter :: CPXERR_BAD_EXPO_RANGE           =  1435
      integer(IL), parameter :: CPXERR_NO_OBJ_SENSE             =  1436
!
!  Common to MPS and related readers                                              
!
      integer(IL), parameter :: CPXERR_NO_NAME_SECTION          =  1441
      integer(IL), parameter :: CPXERR_BAD_SOS_TYPE             =  1442
      integer(IL), parameter :: CPXERR_COL_ROW_REPEATS          =  1443
      integer(IL), parameter :: CPXERR_RIM_ROW_REPEATS          =  1444
      integer(IL), parameter :: CPXERR_ROW_REPEATS              =  1445
      integer(IL), parameter :: CPXERR_COL_REPEATS              =  1446
      integer(IL), parameter :: CPXERR_RIM_REPEATS              =  1447
      integer(IL), parameter :: CPXERR_ROW_UNKNOWN              =  1448
      integer(IL), parameter :: CPXERR_COL_UNKNOWN              =  1449
      integer(IL), parameter :: CPXERR_RHS_UNKNOWN              =  1450
      integer(IL), parameter :: CPXERR_BOUND_UNKNOWN            =  1451
      integer(IL), parameter :: CPXERR_RANGE_UNKNOWN            =  1452
      integer(IL), parameter :: CPXERR_NO_ROW_SENSE             =  1453
      integer(IL), parameter :: CPXERR_EXTRA_FX_BOUND           =  1454
      integer(IL), parameter :: CPXERR_EXTRA_FR_BOUND           =  1455
      integer(IL), parameter :: CPXERR_EXTRA_BV_BOUND           =  1456
      integer(IL), parameter :: CPXERR_BAD_BOUND_TYPE           =  1457
      integer(IL), parameter :: CPXERR_UP_BOUND_REPEATS         =  1458
      integer(IL), parameter :: CPXERR_LO_BOUND_REPEATS         =  1459
      integer(IL), parameter :: CPXERR_NO_BOUND_TYPE            =  1460
      integer(IL), parameter :: CPXERR_NO_QMATRIX_SECTION       =  1461
      integer(IL), parameter :: CPXERR_BAD_SECTION_ENDATA       =  1462
      integer(IL), parameter :: CPXERR_INT_TOO_BIG_INPUT        =  1463
!
!  Unique to MPS reader                                                           
!
      integer(IL), parameter :: CPXERR_NO_ROWS_SECTION          =  1471
      integer(IL), parameter :: CPXERR_NO_COLUMNS_SECTION       =  1472
      integer(IL), parameter :: CPXERR_BAD_SECTION_BOUNDS       =  1473
      integer(IL), parameter :: CPXERR_RANGE_SECTION_ORDER      =  1474
      integer(IL), parameter :: CPXERR_BAD_SECTION_QMATRIX      =  1475
      integer(IL), parameter :: CPXERR_NO_OBJECTIVE             =  1476
      integer(IL), parameter :: CPXERR_ROW_REPEAT_PRINT         =  1477
      integer(IL), parameter :: CPXERR_COL_REPEAT_PRINT         =  1478
      integer(IL), parameter :: CPXERR_RIMNZ_REPEATS            =  1479
      integer(IL), parameter :: CPXERR_EXTRA_INTORG             =  1480
      integer(IL), parameter :: CPXERR_EXTRA_INTEND             =  1481
      integer(IL), parameter :: CPXERR_EXTRA_SOSORG             =  1482
      integer(IL), parameter :: CPXERR_EXTRA_SOSEND             =  1483
      integer(IL), parameter :: CPXERR_TOO_MANY_RIMS            =  1484
      integer(IL), parameter :: CPXERR_TOO_MANY_RIMNZ           =  1485
      integer(IL), parameter :: CPXERR_NO_ROW_NAME              =  1486
      integer(IL), parameter :: CPXERR_BAD_OBJ_SENSE            =  1487
!
!  For REVISE files                                                               
!
      integer(IL), parameter :: CPXERR_REV_INDICATOR            =  1501
      integer(IL), parameter :: CPXERR_REV_REPEATS              =  1502
      integer(IL), parameter :: CPXERR_REV_OBJROW_ILLEG         =  1503
      integer(IL), parameter :: CPXERR_REV_SEN_N_ILLEG          =  1504
      integer(IL), parameter :: CPXERR_REV_EXTRA_RIM            =  1505
      integer(IL), parameter :: CPXERR_REV_RIM_NAMES            =  1506
      integer(IL), parameter :: CPXERR_REV_DELRHS_ILLEG         =  1507
      integer(IL), parameter :: CPXERR_REV_RHS_COEFF            =  1508
      integer(IL), parameter :: CPXERR_REV_DELRNG_ILLEG         =  1509
      integer(IL), parameter :: CPXERR_REV_RANGE_COEFF          =  1510
      integer(IL), parameter :: CPXERR_REV_NO_RANGES            =  1511
      integer(IL), parameter :: CPXERR_REV_DELBND_ILLEG         =  1512
      integer(IL), parameter :: CPXERR_REV_BIN_UB               =  1513
      integer(IL), parameter :: CPXERR_REV_BND_COEFF            =  1514
      integer(IL), parameter :: CPXERR_REV_RIM_COEFF            =  1515
      integer(IL), parameter :: CPXERR_REV_TOO_MANY_ROWS        =  1516
      integer(IL), parameter :: CPXERR_REV_TOO_MANY_COLS        =  1517
      integer(IL), parameter :: CPXERR_REV_TOO_MANY_NZS         =  1518
      integer(IL), parameter :: CPXERR_REV_TOO_MANY_RIMV        =  1519
!
!  PAR Files                                                                      
!
      integer(IL), parameter :: CPXERR_PAR_NO_HEADER            =  1525
      integer(IL), parameter :: CPXERR_PAR_BAD_HEADER           =  1526
      integer(IL), parameter :: CPXERR_PAR_SHORT                =  1527
      integer(IL), parameter :: CPXERR_PAR_DATA                 =  1528
!
!  NET files                                                                      
!
      integer(IL), parameter :: CPXERR_NET_DATA                 =  1530
      integer(IL), parameter :: CPXERR_NOT_MIN_COST_FLOW        =  1531
      integer(IL), parameter :: CPXERR_BAD_ROW_ID               =  1532
      integer(IL), parameter :: CPXERR_DEMAND_BALANCE           =  1533
      integer(IL), parameter :: CPXERR_NET_NO_PROBLEM           =  1535
      integer(IL), parameter :: CPXERR_NET_FIRST_CHAR           =  1536
      integer(IL), parameter :: CPXERR_BAD_CHAR                 =  1537
!
!  BAS files                                                                      
!
      integer(IL), parameter :: CPXERR_BAS_FILE_SHORT           =  1550
      integer(IL), parameter :: CPXERR_BAD_INDICATOR            =  1551
      integer(IL), parameter :: CPXERR_NO_ENDATA                =  1552
      integer(IL), parameter :: CPXERR_FILE_ENTRIES             =  1553
      integer(IL), parameter :: CPXERR_SBASE_ILLEGAL            =  1554
      integer(IL), parameter :: CPXERR_BAS_FILE_SIZE            =  1555
      integer(IL), parameter :: CPXERR_NO_VECTOR_SOLN           =  1556
!
!  SAV files                                                                      
!
      integer(IL), parameter :: CPXERR_NOT_SAV_FILE             =  1560
      integer(IL), parameter :: CPXERR_SAV_FILE_DATA            =  1561
      integer(IL), parameter :: CPXERR_SAV_FILE_WRITE           =  1562
      integer(IL), parameter :: CPXERR_FILE_FORMAT              =  1563
!
!  LP reader errors                                                               
!
      integer(IL), parameter :: CPXERR_ADJ_SIGNS                =  1602
      integer(IL), parameter :: CPXERR_RHS_IN_OBJ               =  1603
      integer(IL), parameter :: CPXERR_ADJ_SIGN_SENSE           =  1604
      integer(IL), parameter :: CPXERR_QUAD_IN_ROW              =  1605
      integer(IL), parameter :: CPXERR_ADJ_SIGN_QUAD            =  1606
      integer(IL), parameter :: CPXERR_NO_OPERATOR              =  1607
      integer(IL), parameter :: CPXERR_NO_OP_OR_SENSE           =  1608
      integer(IL), parameter :: CPXERR_NO_ID_FIRST              =  1609
      integer(IL), parameter :: CPXERR_NO_RHS_COEFF             =  1610
      integer(IL), parameter :: CPXERR_NO_NUMBER_FIRST          =  1611
      integer(IL), parameter :: CPXERR_NO_QUAD_EXP              =  1612
      integer(IL), parameter :: CPXERR_QUAD_EXP_NOT_2           =  1613
      integer(IL), parameter :: CPXERR_NO_QP_OPERATOR           =  1614
      integer(IL), parameter :: CPXERR_NO_NUMBER                =  1615
      integer(IL), parameter :: CPXERR_NO_ID                    =  1616
      integer(IL), parameter :: CPXERR_BAD_ID                   =  1617
      integer(IL), parameter :: CPXERR_BAD_EXPONENT             =  1618
      integer(IL), parameter :: CPXERR_NO_BOUND_SENSE           =  1621
      integer(IL), parameter :: CPXERR_BAD_BOUND_SENSE          =  1622
      integer(IL), parameter :: CPXERR_NO_NUMBER_BOUND          =  1623
      integer(IL), parameter :: CPXERR_LP_TOO_MANY_ROWS         =  1624
      integer(IL), parameter :: CPXERR_LP_TOO_MANY_COLS         =  1625
      integer(IL), parameter :: CPXERR_LP_TOO_MANY_COEFFS       =  1626
      integer(IL), parameter :: CPXERR_INVALID_NUMBER           =  1650
      integer(IL), parameter :: CPXERR_IIS_NO_INFO              =  1701
      integer(IL), parameter :: CPXERR_IIS_NO_SOLN              =  1702
      integer(IL), parameter :: CPXERR_IIS_FEAS                 =  1703
      integer(IL), parameter :: CPXERR_IIS_NOT_INFEAS           =  1704
      integer(IL), parameter :: CPXERR_IIS_OPT_INFEAS           =  1705
      integer(IL), parameter :: CPXERR_IIS_DEFAULT              =  1706
      integer(IL), parameter :: CPXERR_IIS_NO_BASIC             =  1707
      integer(IL), parameter :: CPXERR_IIS_NO_PRIMAL            =  1708
      integer(IL), parameter :: CPXERR_IIS_NO_LOAD              =  1709
      integer(IL), parameter :: CPXERR_IIS_SUB_OBJ_LIM          =  1710
      integer(IL), parameter :: CPXERR_IIS_SUB_IT_LIM           =  1711
      integer(IL), parameter :: CPXERR_IIS_SUB_TIME_LIM         =  1712
      integer(IL), parameter :: CPXERR_IIS_NUM_BEST             =  1713
!
!  CPLEX license info                                                             
!
      integer(IL), parameter :: CPXERR_LICENSE_MIN              = 32000
      integer(IL), parameter :: CPXERR_NO_MIP_LIC               = 32301
      integer(IL), parameter :: CPXERR_NO_BARRIER_LIC           = 32302
      integer(IL), parameter :: CPXERR_LICENSE_MAX              = 32999
!
!  Generic constants                                                              
!
      integer(IL), parameter :: CPX_ON                          =     1
      integer(IL), parameter :: CPX_OFF                         =     0
      integer(IL), parameter :: CPX_MAX                         =    -1
      integer(IL), parameter :: CPX_MIN                         =     1
!
!  Pricing options                                                                
!
      integer(IL), parameter :: CPX_PPRIIND_PARTIAL             =    -1
      integer(IL), parameter :: CPX_PPRIIND_AUTO                =     0
      integer(IL), parameter :: CPX_PPRIIND_DEVEX               =     1
      integer(IL), parameter :: CPX_PPRIIND_STEEP               =     2
      integer(IL), parameter :: CPX_PPRIIND_STEEPQSTART         =     3
      integer(IL), parameter :: CPX_PPRIIND_FULL                =     4
      integer(IL), parameter :: CPX_DPRIIND_AUTO                =     0
      integer(IL), parameter :: CPX_DPRIIND_FULL                =     1
      integer(IL), parameter :: CPX_DPRIIND_STEEP               =     2
      integer(IL), parameter :: CPX_DPRIIND_FULLSTEEP           =     3
      integer(IL), parameter :: CPX_DPRIIND_STEEPQSTART         =     4
!
!  Values returned by getmethod (). Also used by MIP algorithms                   
!
      integer(IL), parameter :: CPXALG_NONE                     =     0
      integer(IL), parameter :: CPXALG_PRIMAL                   =     1
      integer(IL), parameter :: CPXALG_DUAL                     =     2
      integer(IL), parameter :: CPXALG_NETWORK                  =     3
      integer(IL), parameter :: CPXALG_BARRIER                  =     4
      integer(IL), parameter :: CPXALG_DUAL_BARRIER             =     5
      integer(IL), parameter :: CPXALG_BARRIER_NO_CROSSOVER     =     6
      integer(IL), parameter :: CPXALG_PIVOTIN                  =     7
      integer(IL), parameter :: CPXALG_PIVOTOUT                 =     8
!
!  Basis status values                                                            
!
      integer(IL), parameter :: CPX_AT_LOWER                    =     0
      integer(IL), parameter :: CPX_BASIC                       =     1
      integer(IL), parameter :: CPX_AT_UPPER                    =     2
      integer(IL), parameter :: CPX_FREE_SUPER                  =     3
!
!  Infeasibility Finder return values                                             
!
      integer(IL), parameter :: CPXIIS_COMPLETE                 =     1
      integer(IL), parameter :: CPXIIS_PARTIAL                  =     2
!
!  Infeasibility Finder display values                                            
!
      integer(IL), parameter :: CPXIIS_TERSE                    =     1
      integer(IL), parameter :: CPXIIS_VERBOSE                  =     2
!
!  Infeasibility Finder row and column statuses                                   
!
      integer(IL), parameter :: CPXIIS_AT_LOWER                 =     0
      integer(IL), parameter :: CPXIIS_FIXED                    =     1
      integer(IL), parameter :: CPXIIS_AT_UPPER                 =     2
!
!  NETOPT optimization status values                                              
!
      integer(IL), parameter :: CPX_NETOPTIMAL                  =    -1
      integer(IL), parameter :: CPX_NETINFEASIBLE               =    -2
      integer(IL), parameter :: CPX_NETUNBOUNDED                =    -3
      integer(IL), parameter :: CPX_NETUNBOUNDED_INF            =    -4
!
!  Extra rim vector types used in 'etype'/'rimtype' array                         
!
      integer(IL), parameter :: EXTRANROW                       =     1
      integer(IL), parameter :: EXTRARHS                        =     2
      integer(IL), parameter :: EXTRARNG                        =     3
      integer(IL), parameter :: EXTRABDL                        =     4
      integer(IL), parameter :: EXTRABDU                        =     5
!
!  Problem Types:  Problem Types
!           Types 4, 9, and 12 are internal, the others are for users                               
!
      integer(IL), parameter :: CPXPROB_LP                      =     0
      integer(IL), parameter :: CPXPROB_MILP                    =     1
      integer(IL), parameter :: CPXPROB_FIXEDMILP               =     3
      integer(IL), parameter :: CPXPROB_NODELP                  =     4
      integer(IL), parameter :: CPXPROB_QP                      =     5
      integer(IL), parameter :: CPXPROB_MIQP                    =     7
      integer(IL), parameter :: CPXPROB_FIXEDMIQP               =     8
      integer(IL), parameter :: CPXPROB_NODEQP                  =     9
      integer(IL), parameter :: CPXPROB_QCP                     =    10
      integer(IL), parameter :: CPXPROB_MIQCP                   =    11
      integer(IL), parameter :: CPXPROB_NODEQCP                 =    12

!
!  CPLEX Parameter numbers                                                        
!
      integer(IL), parameter :: CPX_PARAM_ADVIND                =  1001
      integer(IL), parameter :: CPX_PARAM_AGGFILL               =  1002
      integer(IL), parameter :: CPX_PARAM_AGGIND                =  1003
      integer(IL), parameter :: CPX_PARAM_BASINTERVAL           =  1004
      integer(IL), parameter :: CPX_PARAM_CFILEMUL              =  1005
      integer(IL), parameter :: CPX_PARAM_CLOCKTYPE             =  1006
      integer(IL), parameter :: CPX_PARAM_CRAIND                =  1007
      integer(IL), parameter :: CPX_PARAM_DEPIND                =  1008
      integer(IL), parameter :: CPX_PARAM_DPRIIND               =  1009
      integer(IL), parameter :: CPX_PARAM_PRICELIM              =  1010
      integer(IL), parameter :: CPX_PARAM_RIMREADLIM            =  1011
      integer(IL), parameter :: CPX_PARAM_RIMNZREADLIM          =  1012
      integer(IL), parameter :: CPX_PARAM_EPMRK                 =  1013
      integer(IL), parameter :: CPX_PARAM_EPOPT                 =  1014
      integer(IL), parameter :: CPX_PARAM_EPPER                 =  1015
      integer(IL), parameter :: CPX_PARAM_EPRHS                 =  1016
      integer(IL), parameter :: CPX_PARAM_FASTMIP               =  1017
      integer(IL), parameter :: CPX_PARAM_IISIND                =  1018
      integer(IL), parameter :: CPX_PARAM_SIMDISPLAY            =  1019
      integer(IL), parameter :: CPX_PARAM_ITLIM                 =  1020
      integer(IL), parameter :: CPX_PARAM_ROWREADLIM            =  1021
      integer(IL), parameter :: CPX_PARAM_NETFIND               =  1022
      integer(IL), parameter :: CPX_PARAM_COLREADLIM            =  1023
      integer(IL), parameter :: CPX_PARAM_NZREADLIM             =  1024
      integer(IL), parameter :: CPX_PARAM_OBJLLIM               =  1025
      integer(IL), parameter :: CPX_PARAM_OBJULIM               =  1026
      integer(IL), parameter :: CPX_PARAM_PERIND                =  1027
      integer(IL), parameter :: CPX_PARAM_PERLIM                =  1028
      integer(IL), parameter :: CPX_PARAM_PPRIIND               =  1029
      integer(IL), parameter :: CPX_PARAM_PREIND                =  1030
      integer(IL), parameter :: CPX_PARAM_REINV                 =  1031
      integer(IL), parameter :: CPX_PARAM_REVERSEIND            =  1032
      integer(IL), parameter :: CPX_PARAM_RFILEMUL              =  1033
      integer(IL), parameter :: CPX_PARAM_SCAIND                =  1034
      integer(IL), parameter :: CPX_PARAM_SCRIND                =  1035
      integer(IL), parameter :: CPX_PARAM_SIMTHREADS            =  1036
      integer(IL), parameter :: CPX_PARAM_SINGLIM               =  1037
      integer(IL), parameter :: CPX_PARAM_SINGTOL               =  1038
      integer(IL), parameter :: CPX_PARAM_TILIM                 =  1039
      integer(IL), parameter :: CPX_PARAM_PPPIND                =  1040
      integer(IL), parameter :: CPX_PARAM_XXXIND                =  1041
      integer(IL), parameter :: CPX_PARAM_EFFSLACKIND           =  1042
      integer(IL), parameter :: CPX_PARAM_PREDISP               =  1043
      integer(IL), parameter :: CPX_PARAM_PREDUAL               =  1044
      integer(IL), parameter :: CPX_PARAM_PREMEMFACT            =  1045
      integer(IL), parameter :: CPX_PARAM_ROWGROWTH             =  1046
      integer(IL), parameter :: CPX_PARAM_COLGROWTH             =  1047
      integer(IL), parameter :: CPX_PARAM_NZGROWTH              =  1048
      integer(IL), parameter :: CPX_PARAM_EPOPT_H               =  1049
      integer(IL), parameter :: CPX_PARAM_EPRHS_H               =  1050
      integer(IL), parameter :: CPX_PARAM_FLIPIND               =  1051
      integer(IL), parameter :: CPX_PARAM_WORKMEM               =  1065
      integer(IL), parameter :: CPX_PARAM_MEMORYEMPHASIS        =  1082
      integer(IL), parameter :: CPX_PARAM_NUMERICALEMPHASIS     =  1083
!
!  Barrier is in bardefs.h, MIP is in mipdefs.h, QP is in qpdefs.h                
!
      integer(IL), parameter :: CPX_PARAM_ALL_MIN               =  1000
      integer(IL), parameter :: CPX_PARAM_ALL_MAX               =  5000
!
!  Callback values for wherefrom                                                  
!
      integer(IL), parameter :: CPX_CALLBACK_PRIMAL             =     1
      integer(IL), parameter :: CPX_CALLBACK_DUAL               =     2
      integer(IL), parameter :: CPX_CALLBACK_NETWORK            =     3
      integer(IL), parameter :: CPX_CALLBACK_PRIMAL_CROSSOVER   =     4
      integer(IL), parameter :: CPX_CALLBACK_DUAL_CROSSOVER     =     5
      integer(IL), parameter :: CPX_CALLBACK_BARRIER            =     6
      integer(IL), parameter :: CPX_CALLBACK_PRESOLVE           =     7
!
!  Values for getcallbackinfo function                                            
!
      integer(IL), parameter :: CPX_CALLBACK_INFO_PRIMAL_OBJ    =     1
      integer(IL), parameter :: CPX_CALLBACK_INFO_DUAL_OBJ      =     2
      integer(IL), parameter :: CPX_CALLBACK_INFO_PRIMAL_INFMEA =     3  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_DUAL_INFMEAS  =     4
      integer(IL), parameter :: CPX_CALLBACK_INFO_PRIMAL_FEAS   =     5
      integer(IL), parameter :: CPX_CALLBACK_INFO_DUAL_FEAS     =     6
      integer(IL), parameter :: CPX_CALLBACK_INFO_ITCOUNT       =     7
      integer(IL), parameter :: CPX_CALLBACK_INFO_CROSSOVER_PPU =     8  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_CROSSOVER_PEX =     9  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_CROSSOVER_DPU =    10  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_CROSSOVER_DEX =    11  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_CROSSOVER_SBC =    12  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_PRESOLVE_ROWS =    13  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_PRESOLVE_COLS =    14  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_PRESOLVE_AGGS =    15  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_PRESOLVE_COEF =    16  !***
!
!  Defines for external functions and constants.                                  
!
      integer(IL), parameter :: CPX_PRIM_INFEAS                 =    32
      integer(IL), parameter :: CPX_DUAL_INFEAS                 =    33
      integer(IL), parameter :: CPX_PRIM_DUAL_INFEAS            =    34
      integer(IL), parameter :: CPX_PRIM_OBJ_LIM                =    35
      integer(IL), parameter :: CPX_DUAL_OBJ_LIM                =    36
      integer(IL), parameter :: CPX_OPTIMAL_FACE_UNBOUNDED      =    37
      integer(IL), parameter :: CPX_NUM_BEST_PRIM_DUAL_FEAS     =    38
      integer(IL), parameter :: CPX_NUM_BEST_PRIM_INFEAS        =    39
      integer(IL), parameter :: CPX_NUM_BEST_DUAL_INFEAS        =    40
      integer(IL), parameter :: CPX_NUM_BEST_PRIM_DUAL_INFEAS   =    41
      integer(IL), parameter :: CPX_BARRIER_NUM_ERROR           =    42
      integer(IL), parameter :: CPX_BARRIER_INCONSISTENT        =    43
!
!  Barrier error codes                                                            
!
      integer(IL), parameter :: CPXERR_BARRIER_NUMERICAL        =  4001
      integer(IL), parameter :: CPXERR_BARRIER_AUG_SCHUR        =  4003
      integer(IL), parameter :: CPXERR_BARRIER_NO_MEMORY        =  4004
!
!  Barrier parameters                                                             
!
      integer(IL), parameter :: CPX_PARAM_BARDSTART             =  3001
      integer(IL), parameter :: CPX_PARAM_BAREPCOMP             =  3002
      integer(IL), parameter :: CPX_PARAM_BARGROWTH             =  3003
      integer(IL), parameter :: CPX_PARAM_BAROBJRNG             =  3004
      integer(IL), parameter :: CPX_PARAM_BARPSTART             =  3005
      integer(IL), parameter :: CPX_PARAM_BARVARUP              =  3006
      integer(IL), parameter :: CPX_PARAM_BARALG                =  3007
      integer(IL), parameter :: CPX_PARAM_BARUNROLL             =  3008
      integer(IL), parameter :: CPX_PARAM_BARCOLNZ              =  3009
      integer(IL), parameter :: CPX_PARAM_BARDISPLAY            =  3010
      integer(IL), parameter :: CPX_PARAM_BARFACTOR             =  3011
      integer(IL), parameter :: CPX_PARAM_BARITLIM              =  3012
      integer(IL), parameter :: CPX_PARAM_BARMAXCOR             =  3013
      integer(IL), parameter :: CPX_PARAM_BARORDER              =  3014
      integer(IL), parameter :: CPX_PARAM_BARROWSDEN            =  3015
      integer(IL), parameter :: CPX_PARAM_BARTHREADS            =  3016
      integer(IL), parameter :: CPX_PARAM_BARSTARTALG           =  3017
      integer(IL), parameter :: CPX_PARAM_BARQCPEPCOMP          =  3020

!  MIP error codes                                                                
!
      integer(IL), parameter :: CPXERR_ALREADY_CTYPE            =  3001
      integer(IL), parameter :: CPXERR_BOUNDS_BINARY            =  3002
      integer(IL), parameter :: CPXERR_NOT_MIP                  =  3003
      integer(IL), parameter :: CPXERR_SOS_BOUNDS               =  3004
      integer(IL), parameter :: CPXERR_BAD_PRIORITY             =  3006
      integer(IL), parameter :: CPXERR_ORDER_BAD_DIRECTION      =  3007
      integer(IL), parameter :: CPXERR_ARRAY_BAD_SOS_TYPE       =  3009
      integer(IL), parameter :: CPXERR_UNIQUE_WEIGHTS           =  3010
      integer(IL), parameter :: CPXERR_BOUNDS_INT               =  3011
      integer(IL), parameter :: CPXERR_BAD_DIRECTION            =  3012
      integer(IL), parameter :: CPXERR_NO_SOS                   =  3015
      integer(IL), parameter :: CPXERR_NO_ORDER                 =  3016
      integer(IL), parameter :: CPXERR_NO_INT_SOLN              =  3017
      integer(IL), parameter :: CPXERR_INT_TOO_BIG              =  3018
      integer(IL), parameter :: CPXERR_SUBPROB_SOLVE            =  3019
      integer(IL), parameter :: CPXERR_NO_MIPSTART              =  3020
      integer(IL), parameter :: CPXERR_BAD_CTYPE                =  3021
      integer(IL), parameter :: CPXERR_MISS_SOS_TYPE            =  3301
      integer(IL), parameter :: CPXERR_TRE_FILE_DATA            =  3401
      integer(IL), parameter :: CPXERR_TRE_FILE_WRITE           =  3402
      integer(IL), parameter :: CPXERR_TRE_FILE_VERSION         =  3403
      integer(IL), parameter :: CPXERR_TRE_FILE_OBJ             =  3404
      integer(IL), parameter :: CPXERR_TRE_FILE_COLS            =  3405
      integer(IL), parameter :: CPXERR_TRE_FILE_ROWS            =  3406
      integer(IL), parameter :: CPXERR_TRE_FILE_INTS            =  3407
      integer(IL), parameter :: CPXERR_TRE_FILE_NONZ            =  3408
      integer(IL), parameter :: CPXERR_TRE_FILE_TYPES           =  3409
      integer(IL), parameter :: CPXERR_TRE_FILE_PRESOLVE        =  3410
      integer(IL), parameter :: CPXERR_NO_TREE                  =  3412
      integer(IL), parameter :: CPXERR_TREE_MEMORY_LIMIT        =  3413
      integer(IL), parameter :: CPXERR_TRE_FILE_FORMAT          =  3414
      integer(IL), parameter :: CPXERR_NODE_FILE_OPEN           =  3501
      integer(IL), parameter :: CPXERR_NODE_FILE_READ           =  3502
      integer(IL), parameter :: CPXERR_NODE_FILE_WRITE          =  3503
      integer(IL), parameter :: CPXERR_NODE_ON_DISK             =  3504
      integer(IL), parameter :: CPXERR_PTHREAD_MUTEX_INIT       =  3601
      integer(IL), parameter :: CPXERR_SUN_CONCURRENCY          =  3602
      integer(IL), parameter :: CPXERR_PTHREAD_CREATE           =  3603
!
!  Variable selection values                                                      
!
      integer(IL), parameter :: CPX_VARSEL_MININFEAS            =    -1
      integer(IL), parameter :: CPX_VARSEL_DEFAULT              =     0
      integer(IL), parameter :: CPX_VARSEL_MAXINFEAS            =     1
      integer(IL), parameter :: CPX_VARSEL_PSEUDO               =     2
      integer(IL), parameter :: CPX_VARSEL_STRONG               =     3
!
!  Node selection values                                                          
!
      integer(IL), parameter :: CPX_NODESEL_DFS                 =     0
      integer(IL), parameter :: CPX_NODESEL_BESTBOUND           =     1
      integer(IL), parameter :: CPX_NODESEL_BESTEST             =     2
      integer(IL), parameter :: CPX_NODESEL_BESTEST_ALT         =     3
!
!  Values for generated priority order                                            
!
      integer(IL), parameter :: CPX_MIPORDER_COST               =     1
      integer(IL), parameter :: CPX_MIPORDER_BOUNDS             =     2
      integer(IL), parameter :: CPX_MIPORDER_SCALEDCOST         =     3
!
!  Values for direction array                                                     
!
      integer(IL), parameter :: CPX_BRANCH_GLOBAL               =     0
      integer(IL), parameter :: CPX_BRANCH_DOWN                 =     1
      integer(IL), parameter :: CPX_BRANCH_UP                   =     2
!
!  Values for CPX_PARAM_BRDIR                                                     
!
      integer(IL), parameter :: CPX_BRDIR_DOWN                  =    -1
      integer(IL), parameter :: CPX_BRDIR_AUTO                  =     0
      integer(IL), parameter :: CPX_BRDIR_UP                    =     1
!
!  Use of MIP start values                                                        
!
      integer(IL), parameter :: CPX_MIPSTART_NODE0              =     1
!
!  MIP Problem status codes                                                       
!
      integer(IL), parameter :: CPXMIP_OPTIMAL                  =   101
      integer(IL), parameter :: CPXMIP_OPTIMAL_TOL              =   102
      integer(IL), parameter :: CPXMIP_INFEASIBLE               =   103
      integer(IL), parameter :: CPXMIP_SOL_LIM                  =   104
      integer(IL), parameter :: CPXMIP_NODE_LIM_FEAS            =   105
      integer(IL), parameter :: CPXMIP_NODE_LIM_INFEAS          =   106
      integer(IL), parameter :: CPXMIP_TIME_LIM_FEAS            =   107
      integer(IL), parameter :: CPXMIP_TIME_LIM_INFEAS          =   108
      integer(IL), parameter :: CPXMIP_FAIL_FEAS                =   109
      integer(IL), parameter :: CPXMIP_FAIL_INFEAS              =   110
      integer(IL), parameter :: CPXMIP_MEM_LIM_FEAS             =   111
      integer(IL), parameter :: CPXMIP_MEM_LIM_INFEAS           =   112
      integer(IL), parameter :: CPXMIP_ABORT_FEAS               =   113
      integer(IL), parameter :: CPXMIP_ABORT_INFEAS             =   114
      integer(IL), parameter :: CPXMIP_OPTIMAL_INFEAS           =   115
      integer(IL), parameter :: CPXMIP_FAIL_FEAS_NO_TREE        =   116
      integer(IL), parameter :: CPXMIP_FAIL_INFEAS_NO_TREE      =   117
      integer(IL), parameter :: CPXMIP_NODE_FILE_LIM_FEAS       =   118
      integer(IL), parameter :: CPXMIP_NODE_FILE_LIM_INFEAS     =   119
!
!  Callback values for wherefrom                                                  
!
      integer(IL), parameter :: CPX_CALLBACK_MIP                =   101
      integer(IL), parameter :: CPX_CALLBACK_MIP_BRANCH         =   102
      integer(IL), parameter :: CPX_CALLBACK_MIP_NODE           =   103
      integer(IL), parameter :: CPX_CALLBACK_MIP_HEURISTIC      =   104
      integer(IL), parameter :: CPX_CALLBACK_MIP_SOLVE          =   105
!
!  Values for getcallbackinfo function                                            
!
      integer(IL), parameter :: CPX_CALLBACK_INFO_BEST_INTEGER  =   101
      integer(IL), parameter :: CPX_CALLBACK_INFO_BEST_REMAININ =   102  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_COUNT    =   103
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODES_LEFT    =   104
      integer(IL), parameter :: CPX_CALLBACK_INFO_MIP_ITERATION =   105  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_CUTOFF        =   106
      integer(IL), parameter :: CPX_CALLBACK_INFO_CLIQUE_COUNT  =   107
      integer(IL), parameter :: CPX_CALLBACK_INFO_COVER_COUNT   =   108
!
!  Values for getnodecallbackinfo function                                        
!
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_SIINF    =   201
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_NIINF    =   202
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_ESTIMATE =   203
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_DEPTH    =   204
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_OBJVAL   =   205
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_TYPE     =   206
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_VAR      =   207
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_SOS      =   208
      integer(IL), parameter :: CPX_CALLBACK_INFO_NODE_SEQNUM   =   209
!
!  Values for getvarcallbackinfo function                                         
!
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_TYPE      =   251
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_DOWN_PSEU =   252  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_UP_PSEUDO =   253
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_PRIORITY  =   254
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_DIRECTION =   255
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_INTEGER_V =   256  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_IS_INT_FE =   257  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_LB        =   258
      integer(IL), parameter :: CPX_CALLBACK_INFO_VAR_UB        =   259
!
!  Values for getsoscallbackinfo function                                         
!
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_TYPE      =   240
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_SIZE      =   241
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_IS_FEASIB =   242  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_PRIORITY  =   243
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_MEMBER_IN =   244  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_MEMBER_IS =   245  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_MEMBER_RE =   246  !***
      integer(IL), parameter :: CPX_CALLBACK_INFO_SOS_NUM       =   247
!
!  Callback return codes                                                          
!
      integer(IL), parameter :: CPX_CALLBACK_DEFAULT            =     0
      integer(IL), parameter :: CPX_CALLBACK_SET                =     2
      integer(IL), parameter :: CPX_CALLBACK_FAIL               =     1
      integer(IL), parameter :: CPX_CALLBACK_NO_SPACE           =     3
!
!  MIP Parameter numbers                                                          
!
      integer(IL), parameter :: CPX_PARAM_BRDIR                 =  2001
      integer(IL), parameter :: CPX_PARAM_BTTOL                 =  2002
      integer(IL), parameter :: CPX_PARAM_CLIQUES               =  2003
      integer(IL), parameter :: CPX_PARAM_COEREDIND             =  2004
      integer(IL), parameter :: CPX_PARAM_COVERS                =  2005
      integer(IL), parameter :: CPX_PARAM_CUTLO                 =  2006
      integer(IL), parameter :: CPX_PARAM_CUTUP                 =  2007
      integer(IL), parameter :: CPX_PARAM_EPAGAP                =  2008
      integer(IL), parameter :: CPX_PARAM_EPGAP                 =  2009
      integer(IL), parameter :: CPX_PARAM_EPINT                 =  2010
      integer(IL), parameter :: CPX_PARAM_HEURISTIC             =  2011
      integer(IL), parameter :: CPX_PARAM_MIPDISPLAY            =  2012
      integer(IL), parameter :: CPX_PARAM_MIPINTERVAL           =  2013
      integer(IL), parameter :: CPX_PARAM_MIPTHREADS            =  2014
      integer(IL), parameter :: CPX_PARAM_INTSOLLIM             =  2015
      integer(IL), parameter :: CPX_PARAM_NODEFILEIND           =  2016
      integer(IL), parameter :: CPX_PARAM_NODELIM               =  2017
      integer(IL), parameter :: CPX_PARAM_NODESEL               =  2018
      integer(IL), parameter :: CPX_PARAM_OBJDIF                =  2019
      integer(IL), parameter :: CPX_PARAM_MIPORDIND             =  2020
      integer(IL), parameter :: CPX_PARAM_RCFIXIND              =  2021
      integer(IL), parameter :: CPX_PARAM_RELOBJDIF             =  2022
      integer(IL), parameter :: CPX_PARAM_SOSIND                =  2023
      integer(IL), parameter :: CPX_PARAM_SOSMINSZ              =  2024
      integer(IL), parameter :: CPX_PARAM_STARTALG              =  2025
      integer(IL), parameter :: CPX_PARAM_SUBALG                =  2026
      integer(IL), parameter :: CPX_PARAM_TRELIM                =  2027
      integer(IL), parameter :: CPX_PARAM_VARSEL                =  2028
      integer(IL), parameter :: CPX_PARAM_BNDSTRENIND           =  2029
      integer(IL), parameter :: CPX_PARAM_NODEFILELIM           =  2030
      integer(IL), parameter :: CPX_PARAM_HEURFREQ              =  2031
      integer(IL), parameter :: CPX_PARAM_MIPORDTYPE            =  2032
      integer(IL), parameter :: CPX_PARAM_CUTSFACTOR            =  2033
      integer(IL), parameter :: CPX_PARAM_RELAXPREIND           =  2034
      integer(IL), parameter :: CPX_PARAM_MIPSTART              =  2035
      integer(IL), parameter :: CPX_PARAM_NODEFILEDIR           =  2036
      integer(IL), parameter :: CPX_PARAM_PRESLVND              =  2037
      integer(IL), parameter :: CPX_PARAM_FLOWCOVERS            =  2040
      integer(IL), parameter :: CPX_PARAM_IMPLBD                =  2041
      integer(IL), parameter :: CPX_PARAM_PROBE                 =  2042
      integer(IL), parameter :: CPX_PARAM_GUBCOVERS             =  2044
      integer(IL), parameter :: CPX_PARAM_FRACCUTS              =  2049
      integer(IL), parameter :: CPX_PARAM_FLOWPATHS             =  2051
      integer(IL), parameter :: CPX_PARAM_MIRCUTS               =  2052
      integer(IL), parameter :: CPX_PARAM_DISJCUTS              =  2053
      integer(IL), parameter :: CPX_PARAM_MIPEMPHASIS           =  2058
      integer(IL), parameter :: CPX_PARAM_DIVETYPE              =  2060
      integer(IL), parameter :: CPX_PARAM_RINSHEUR              =  2061
      integer(IL), parameter :: CPX_PARAM_LBHEUR                =  2063
      integer(IL), parameter :: CPX_PARAM_FPHEUR                =  2098
      integer(IL), parameter :: CPX_PARAM_ZEROHALFCUTS          =  2111

!  QP error codes                                                                 
!
      integer(IL), parameter :: CPXERR_BAD_Q                    =  5001
      integer(IL), parameter :: CPXERR_Q_NOT_POS_DEF            =  5002
      integer(IL), parameter :: CPXERR_Q_NOT_MATRIX             =  5003
      integer(IL), parameter :: CPXERR_NOT_QP                   =  5004
      integer(IL), parameter :: CPXERR_Q_SEP_INDEF              =  5006
      integer(IL), parameter :: CPXERR_QP_BAD_BAR_ALG           =  5007
      integer(IL), parameter :: CPXERR_Q_NO_SPACE               =  5009
      integer(IL), parameter :: CPXERR_Q_NEG_ZERO_COMP          =  5010
      integer(IL), parameter :: CPXERR_Q_DUP_ENTRY              =  5011
      integer(IL), parameter :: CPXERR_Q_NOT_SYMMETRIC          =  5012
      integer(IL), parameter :: CPX_PARAM_QPNZREADLIM           =  4001
!
!  Variable types for 'ctype' array
!
      character(1), parameter :: CPX_BINARY                     = 'B'
      character(1), parameter :: CPX_CONTINUOUS                 = 'C'
      character(1), parameter :: CPX_INTEGER                    = 'I'
!
!  Values for 'SOS' type and 'branch' type
! 
      character(1), parameter :: CPX_TYPE_VAR                   = '0'
      character(1), parameter :: CPX_TYPE_SOS1                  = '1'
      character(1), parameter :: CPX_TYPE_SOS2                  = '2'
      character(1), parameter :: CPX_TYPE_SOS3                  = '3'
      character(1), parameter :: CPX_TYPE_USER                  = 'X'
!
      END MODULE cplex_cons