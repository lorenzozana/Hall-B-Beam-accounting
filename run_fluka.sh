i=`echo $1`
NAME=`echo $i | gawk '{sub(/.inp/,"",$0);printf("%s",$0)}'`
echo "Running "$NAME
nice ${FLUPRO}/flutil/rfluka2 -e /home/zana/Hall-B/solenoid.exe -M 1 $NAME 
