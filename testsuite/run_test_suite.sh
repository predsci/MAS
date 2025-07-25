#!/bin/bash

trap ctrl_c INT

function ctrl_c() {
  ${echo} "${cR}==> Caught CTRL-C, shutting down!${cX}"
  exit 1
}

function display_help {
echo " __ __  __    __   _____ ___  __ _____    __  _  _ _ _____ ___  "
echo "|  V  |/  \ /' _/ |_   _| __/' _/_   _| /' _/| || | |_   _| __| "
echo "| \_/ | /\ |\`._\`.   | | | _|\`._\`. | |   \`._\`.| || | | | | | _|  "
echo "|_| |_|_||_||___/   |_| |___|___/ |_|   |___/ \__/|_| |_| |___| "
echo ""
echo ""
echo "      TEST SUITE v1.0.0"
echo "USAGE:   ./run_test_suite.sh"
echo ""
echo "By default, the above command will run the test suite on all"
echo "default tests using the 'mas' executable from the '../bin/'"
echo "folder (assuming it has been built)."
echo ""
echo "OPTIONS:"
echo ""
echo "Flag options with choices are specified as '-opt=<OPTION>'."
echo ""
echo "-nochecksetup   Don't check the environment."
echo ""
echo "-masexe=        Use this to run the testsuite on a specific mas executable."
echo "                This should be a full path and is useful for development tests."
echo ""
echo "-mpicall=       Use this to run the testsuite with a specific mpi calling mechanism."
echo "                The call should end with '-np' or equivalent as the # of ranks will"
echo "                be placed after the call. "
echo "                The default is 'mpirun -np'."
echo ""
echo "-np=            Number of MPI processes to use.  Note that, due to low resolution,"
echo "                the number of processes is limited.  Default is 1."
echo ""
echo "-test=          Comma-seperated list of subset of tests to run."
echo "                Also can be used to run non-standard/experimental tests."
echo ""
echo "-nocleanup      By default, only the initial and final conditions of a run are "
echo "                kept in the run folders. Set this to keep the full run."
echo ""
echo "-clean          Clear out all testsuite runs."
echo ""
echo "-norun          Does not run the MAS code.  Checks for a previous run and compares if found."
echo ""
echo "-nocompare      Do not compare the runs to their reference runs."
echo ""
echo "-compareprec=   Set the precision for the comparisons (decimal place)"
echo "                Default is 5."
echo ""
echo "-nocolor        Disable color text output."
echo ""
}

#Set number of processors to use for testsuite:
np=1
norun=0
nocompare=0
compareprec=6
novis=0
nocleanup=0
clean=0
nochecksetup=0
nocolor=0
setrefdata=0
ifprec32=0
masexe="mas"
mpicall="mpirun -np"

AVAIL_TEST_RUNS_LIST="
zb_2d_alfven_wave_t
zb_3d_alfven_wave_p_rot
zb_3d_tdm_3rope
zb_3d_fr_rbsl
poly_3d_pw_relaxation
thermo_3d_relaxation
thermo_wtd_3d_relaxation
helio_2d_relaxation"
#poly_1d_parker
#poly_2d_shear
#poly_2d_dipole
#poly_2d_ip_cs
#thermo_1d_wind
#wtd_1d_analytic

TEST_RUNS_LIST=${AVAIL_TEST_RUNS_LIST}

for i in "$@"
do
case $i in
    -norun)
    norun=1
    ;;
    -nocompare)
    nocompare=1
    ;;
    -compareprec=*)
    compareprec="${i#*=}"
    ;;    
    -nocleanup)
    nocleanup=1
    ;;
    -nochecksetup)
    nochecksetup=1
    ;;
    -nocolor)
    nocolor=1
    ;;
    -setrefdata)
    setrefdata=1
    ;;
    -test=*)
    TEST_RUNS_LIST="${i#*=}"
    TEST_RUNS_LIST="${TEST_RUNS_LIST//','/' '}"
    ;;
    -masexe=*)
    masexe="${i#*=}"
    ;;
    -mpicall=*)
    mpicall="${i#*=}"
    ;;
    -pc32)
    ifprec32=1
    ;;
    -np=*)
    np="${i#*=}"
    ;;    
    -clean)
    clean=1
    norun=1
    nocompare=1
    nochecksetup=1
    ;;
    -h)
    display_help
    exit 0
    ;;
    --help)
    display_help
    exit 0
    ;;    
    *)
    echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    echo "ERROR!  Unknown option: $i"
    echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    display_help
    exit 1    
    ;;
esac
done

# ****** Get test suite parameters ******
WD=${PWD}
BINDIR=${WD}/../bin
SRCDIR=${WD}/../src
ROOTDIR=${WD}/..
TSLOG=${RESULTSDIR}/testsuite.log

if [ ${nocolor} == 0 ]
then
  cX="\033[0m"
  cR="\033[1;31m"
  cB="\033[1;34m"
  cG="\033[32m"
  cC="\033[1;96m"
  cM="\033[35m"
  cY="\033[1;93m"
  Bl="\033[1;5;96m"
  echo="echo -e"
else
  cX=
  cR=
  cB=
  cG=
  cC=
  cM=
  cY=
  Bl=
  echo="echo"
fi

${echo} " "
${echo} "${cB}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${cX}"
${echo} "${cY}                                             ${cX}"
${echo} "${cY}      _|      _|     _|_|       _|_|_|       ${cX}"
${echo} "${cY}      _|_|  _|_|   _|    _|   _|             ${cX}"
${echo} "${cY}      _|  _|  _|   _|_|_|_|     _|_|         ${cX}"
${echo} "${cY}      _|      _|   _|    _|         _|       ${cX}"
${echo} "${cY}      _|      _|   _|    _|   _|_|_|         ${cX}"
${echo} "${cY}                                             ${cX}"
${echo} "${cY}  ___ ____ ____ ___    ____ _  _ _ ___ ____  ${cX}"
${echo} "${cY}   |  |___ [__   |     [__  |  | |  |  |___  ${cX}"
${echo} "${cY}   |  |___ ___]  |     ___] |__| |  |  |___  ${cX}"
${echo} "${vY}                                             ${cX}"
${echo} "${Bl}                 GIT VERSION                 ${cX}"
${echo} "${vY}                                             ${cX}"
${echo} "${cB}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${cX}"
${echo} "Welcome to the MAS test suite!"
#
# ****** Test for correct prerequisites and environment ******
#
${echo} "Checking file structure..."

#
# ****** Test for correct prerequisites and environment ******
#
if [ ${nochecksetup} == 0 ]
then
  ${echo} "Checking software requirements..."
  #Check that python is installed:
  PTEST=$(which python3)
  if [ -z "${PTEST}" ]
  then
    ${echo} "${cR}==> ERROR! Python3 does not seem to be installed!${cX}"
    ${echo} "${cR}    This testsuite requires Python3 with the packages:${cX}"
    ${echo} "${cR}      numpy, h5py${cX}"
    exit 1
  fi
  ${echo} "${cG}==> Python3 is installed!${cX}"
 #
 # Check for required packages.
 #
  PYTHON_PKG_LIST="numpy
  h5py
  "

  for pypkg in $PYTHON_PKG_LIST
  do
    python3 -c "import ${pypkg}" 2>/dev/null
    pychk=$?
    if [ $pychk -eq 1 ]; then
      ${echo} "${cR}==> ERROR! Missing required package ${pypkg}.${cX}"
      ${echo} "${cR}    This testsuite requires Python3 with the packages:${cX}"
      ${echo} "${cR}      numpy, h5py${cX}"      
      exit 1
    fi
  done
  ${echo} "${cG}==> All required python3 packages are present!${cX}"
#
# Check that the mas bin directory is in the user's path, if not, add it.
#
  ${echo} "==> Checking PATH...."
  PTEST=$(which mas_compare_run_diags.py)
  if [ -z "${PTEST}" ]
  then
    ${echo} "${cY}==> WARNING: MAS is not in the PATH!${cX}"
    ${echo} "${cY}==> Appending ${BINDIR} to PATH...${cX}"
    export PATH="${BINDIR}:${PATH}"
  fi
  PTEST=$(which mas_compare_run_diags.py)
  if [ -z "${PTEST}" ]; then
    ${echo} "${cR}==> ERROR! MAS bin PATH problem!${cX}"
    exit 1
  fi
  ${echo} "${cG}==> Everything seems OK to run the MAS testsuite!${cX}"
fi
#
# ****** Check that user is running the tests and really wants to set ref data:
#
if [ ${setrefdata} -eq 1 ] && [ ${norun} -eq 1 ]
then
  ${echo} "${cR} ==> ERROR!  You are trying to set reference data without running the tests!${cX}"
  exit 1
fi
if [ ${setrefdata} -eq 1 ]
then
  ${echo} "${cR}╔═╗┌─┐┌┬┐  ╔╗╔┌─┐┬ ┬  ╦═╗┌─┐┌─┐┌─┐┬─┐┌─┐┌┐┌┌─┐┌─┐  ╔╦╗┌─┐┌┬┐┌─┐${cX}"
  ${echo} "${cR}╚═╗├┤  │   ║║║├┤ │││  ╠╦╝├┤ ├┤ ├┤ ├┬┘├┤ ││││  ├┤    ║║├─┤ │ ├─┤${cX}"
  ${echo} "${cR}╚═╝└─┘ ┴   ╝╚╝└─┘└┴┘  ╩╚═└─┘└  └─┘┴└─└─┘┘└┘└─┘└─┘  ═╩╝┴ ┴ ┴ ┴ ┴${cX}"
  read -p "==> Setting new reference data after runs complete...are you SURE? (y/n):" yn
  if [ ${yn} = "y" ]
  then
    read -p "==> Are you really really really SURE?! (y/n):" yn
    if [ ${yn} = "n" ]
    then
      ${echo} "==> ${cR}Aborting!${cX}"
      exit 0
    fi
  else
    ${echo} "==> ${cR}Aborting!${cX}"
    exit 0
  fi
fi

########################################################################
########################################################################
##
## ****** Start loop through test problems ******
##
########################################################################
########################################################################

Ti=0
for TESTNAME in ${TEST_RUNS_LIST}
do
  Ti=$((${Ti}+1))
#
# ****** Make sure test is in the testsuite:
#
  ${echo} "${cC}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${cX}"
  testok=0
  for run_test in ${AVAIL_TEST_RUNS_LIST}; do
    if [[ ${run_test} == ${TESTNAME} ]]; then
      testok=1
      break
    fi
  done
  if [ ${testok} -eq 0 ]
  then
    ${echo} "${cR}==> TEST ${cX}${cM}${TESTNAME}${cX}${cR} is not a valid test in the testsuite!${cX}"
    exit 1
  fi
  ${echo} "STARTING TEST ${cM}${TESTNAME}${cX}"
  ${echo} "${cC}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${cX}"
  ${echo} "==> Gathering test information..."
# ****** Get directories:
  RUNDIR=${WD}/${TESTNAME}/run
  REFDIR=${WD}/${TESTNAME}/reference
  INPUTDIR=${WD}/${TESTNAME}/input

  if [ ! -d ${RUNDIR} ]
  then
    ${echo} "${cR}!!!> ERROR! Run directory does not exist for test ${TESTNAME}!${cX}"
    exit 1
  fi

  if [ ! -d ${REFDIR} ]
  then
    ${echo} "${cR}!!!> ERROR! Reference directory does not exist for test ${TESTNAME}!${cX}"
    exit 1
  fi

  if [ ! -d ${INPUTDIR} ]
  then
    ${echo} "${cR}!!!> ERROR! Input directory does not exist for test ${TESTNAME}!${cX}"
    exit 1
  fi

  if [ ! -e ${INPUTDIR}/mas.in ]
  then
    ${echo} "${cR}!!!> ERROR! Test ${TESTNAME} does not have an input file!${cX}"
    exit 1
  fi

  cd ${RUNDIR}
  
  if [ ${clean} == 1 ]
  then
    ${echo} "==> Clearing run directory..."
    rm -fr ${RUNDIR}/*
  fi

  if [ ${norun} == 0 ]
  then

    if [ -e ${RUNDIR}/mas.out ]
    then
      ${echo} "==> Clearing old run..."
      rm -fr ${RUNDIR}/*
    fi

    ${echo} "==> Copying input files..."
    cp -r ${INPUTDIR}/* ${RUNDIR}/ 2>/dev/null

    if [ ${ifprec32} == 1 ]
    then
      ${echo} "==> Setting preconditioners to single precision ..."
      match="\&data"
      insert="  ifprec_32=.true."
      sed -i "s/${match}/${match}\n${insert}/" "mas.in"
    fi

    cd ${RUNDIR}

    ${echo} "======================================================="
    ${echo} "${cB}==> RUNNING MAS${cX}"
    ${echo} "======================================================="
    ${echo} "==> Running mas with command:"
    ${echo} "==> ${mpicall} ${np} ${masexe} ${TESTNAME} mas.in 1>mas.log 2>mas.err"
    ${mpicall} ${np} ${masexe} ${TESTNAME} mas.in 1>mas.log 2>mas.err

    # Check that a completed run exists in the run folder
    if [ ! -e ${RUNDIR}/mas_timing.out ]
    then
      if [ ${norun} == 0 ]
      then
        ${echo} "${cR}!!!> ERROR! Test ${TESTNAME} did not run correctly!${cX}"
        ${echo} "${cR}!!!> Check the run folder: ${RUNDIR} ${cX}"
        ${echo} "${cR}!!!> mas.log contents: ${cX}"
        cat ${RUNDIR}/mas.log
        ${echo} "${cR}!!!> mas.err contents: ${cX}"
        cat ${RUNDIR}/mas.err
        exit 1
      fi
    fi

  # Get timing data:
    TIME_RUN_TMP=($(grep "### CPU time used (on IPROC0):" ${RUNDIR}/mas.log))
    TIME_RUN_TMP=${TIME_RUN_TMP[6]}
    TIME_RUN[${Ti}]=$(printf "%8.3f" ${TIME_RUN_TMP})
    ${echo} "${cG}==> Test completed in:               ${TIME_RUN[${Ti}]} seconds.${cX}"
    
    if [ ${nocompare} == 0 ]
    then
      TIME_REF_TMP=($(grep "### CPU time used (on IPROC0):" ${REFDIR}/mas.log))
      TIME_REF_TMP=${TIME_REF_TMP[6]}

      SPEEDUP_TMP=`python -c "print(${TIME_REF_TMP}/${TIME_RUN_TMP})"`
      TIME_REF[${Ti}]=$(printf "%8.3f" ${TIME_REF_TMP})
      SPEEDUP[${Ti}]=$(printf "%5.2f" ${SPEEDUP_TMP})

      ${echo} "${cG}==> Speedup compared to reference run: ${SPEEDUP[${Ti}]} x${cX}"
    fi
  fi

  if [ ${setrefdata} -eq 1 ] && [ ${norun} -eq 0 ]
  then
    ${echo} "${cR}=======================================================${cX}"
    ${echo} "${cR}==> SETTING REFERENCE DATA FOR RUN${cX}"
    ${echo} "${cR}=======================================================${cX}"

    ${echo} "${cR}==> Removing old reference data...${cX}"
    rm -fr ${REFDIR}/* 2>/dev/null
    ${echo} "${cR}==> Copying current run data into reference directory...${cX}"
    cp ${RUNDIR}/* ${REFDIR}/
    rm -f ${REFDIR}/*.h5
  fi
#
# ****** Compare run data.
#
  if [ ${nocompare} == 0 ]
  then
    ${echo} "======================================================="
    ${echo} "${cB}==> COMPARING RUN TO REFERENCE DATA${cX}"
    ${echo} "======================================================="
    ${echo} "==> Running comparison...${cR}"
    mas_compare_run_diags.py -p ${compareprec} ${REFDIR} ${RUNDIR}
    PASS_FAIL[${Ti}]=$?
    if [ "${PASS_FAIL[${Ti}]}" = "0" ]
    then
      ${echo} "${cX}${cG}==> Test seems to have PASSED!${cX}"
    else
      ${echo} "${cX}${cR}==> Test seems to have FAILED!${cX}"
      PASS_FAIL[${Ti}]=1
    fi
  fi
#
# ****** Clean run data.
#
  if [ ${nocleanup} == 0 ]
  then
    if [ ${norun} == 0 ]
    then
      ${echo} "======================================================="
      ${echo} "${cB}==> CLEANING RUN DATA${cX}"
      ${echo} "======================================================="
      ${echo} "==> Removing files from run data..."
      rm -fr ${RUNDIR}/*
    fi
  fi
done
${echo} "${cC}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${cX}"


# Display summary and timing results.
if [ ${nocompare} == 0 ]
then
  ${echo} "${cY}===========================================================================${cX}"
  ${echo} "${cY}Summary of test results:${cX}"
  ${echo} "${cY}===========================================================================${cX}"

  Ti=0
    ${echo} "$(printf "%-35s  %9s  %8s  %8s  %7s" "Test name" "PASS/FAIL" "Run-time" "Ref-time" "Speedup")"
  ${echo} "${cY}===========================================================================${cX}"    
  for TESTNAME in ${TEST_RUNS_LIST}
  do
    Ti=$((${Ti}+1))
    passfailcomp=( ${PASS_FAIL[${Ti}]} )
    if [ "${passfailcomp[0]}" = "1" ]; then
      pf="${cR}FAIL     ${cX}"
    else
      pf="${cG}PASS     ${cX}"
    fi
    ${echo} "$(printf "%-35s  %9s  %8s  %8s  %7s" "${TESTNAME}" "${pf}" "${TIME_RUN[${Ti}]}" "${TIME_REF[${Ti}]}" "${SPEEDUP[${Ti}]}")"
  done
fi

${echo} "${cC}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${cX}"

exit 0
