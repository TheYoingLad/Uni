
if [ ! -f junit5all.jar ]; then
    echo Missing file: junit5all.jar
    exit 1
fi

if [ $# -lt 2 ]; then
    echo "Usage: check.bat <path of tester file> <tester class> [CheckThat options] [JUnit properties]"
    echo "Useful CheckThat options:"
    echo "   -Dlang=HU or -Dlang=EN  For structural tester error messages."
    echo "   -Dsummary=time/full     Use either time or full to see more JUnit summary details."
    echo "   -Derrors=verbose        If you really, REALLY want to see the full stack trace."
    echo "Useful JUnit properties:"
    echo "   --disable-ansi-colors   For monochrome terminals."
    echo
    echo "Note: in PowerShell terminals, options have to be surrounded using this very silly syntax:"
    echo "   '\"-Dlang=EN\"'"
    echo
    echo "Note: passing more than one option can be tricky."
    echo "Suggested: see the executed java and javac commands and manually modify them."
    exit 1
fi

TARGET1=$1
TARGET2=$2
PROPS=${3-}
OPTS=${4-}

javac -cp ".:junit5all.jar:checkthat.jar" "${TARGET1}"
retVal=$?
[[ $retVal -ne 0 ]] && exit $retVal
java ${PROPS} -javaagent:checkagent.jar -jar junit5all.jar execute ${OPTS} -E junit-vintage --disable-banner -cp . -cp checkthat.jar -c "${TARGET2}"
