file="timeline.txt"
# 3600/5 = 720
hconv=720 
run_per_conf=30
sub=0
shift=0
here_pos=`pwd`
file_rev="reverse_beam_time.txt"
sed 1d $file | tac > $file_rev
rm  Run2/list_exec.txt
while read line ; do
    conf_n=`echo $line | awk '{printf("%s \n",$1)}'`
    conf_n=`echo $conf_n |  sed 's/ /*/g'`
    target=`echo $line | awk '{printf("%s \n",$2)}'`
    target=`echo $target |  sed 's/ /*/g'`
    hours=`echo $line | awk '{printf("%s \n",$3)}'`
    hours=`echo $hours |  sed 's/ /*/g'`
    seconds=`awk -vp=$hours -vq=$hconv 'BEGIN{printf "%6d" ,p * q}'`	
	# seconds divided by 5 (3600/5) since I will consider 5 different time of irradiation followed by 4 of waiting beam
    current=`echo $line | awk '{printf("%s \n",$5)}'`
    current=`echo $current |  sed 's/ /*/g'`
    current=`awk -vp=$current -vq="6.25E12" 'BEGIN{printf "%.2E" ,p * q}'`
	# need to modify the current in particle per second
    energy=`echo $line | awk '{printf("%s \n",$4)}'`
    energy=`echo $energy |  sed 's/ /*/g'`
    energy=`awk -vp=$energy 'BEGIN{printf "%3s",p}'`
    #added 1 second in order to avoid overlapping on the last one in time
    shift1=`awk -vp=$shift 'BEGIN{printf "%7d" ,p + 3601 }'`
    shift2=`awk -vp=$shift 'BEGIN{printf "%7d" ,p + 43201 }'`
    shift3=`awk -vp=$shift 'BEGIN{printf "%7d" ,p + 86401 }'`
    shift4=`awk -vp=$shift 'BEGIN{printf "%7d" ,p + 604801 }'`
    shift5=`awk -vp=$shift 'BEGIN{printf "%7d" ,p + 2592001 }'`
    if [ "${target}" == "NO" ]
    then
	echo "No target time"
    else
	
	echo "Conf n.="$conf_n "target="$target " seconds="$seconds "current="$current " energy="$energy " shift="$shift
	perl -pe "s/.*/BEAM            -$energy             .142857      -0.1      -0.1          ELECTRON/ if $. == 6" < hall-b_cone_${target}.inp >  Run2/hallB_target_${target}_${conf_n}.inp
	j=0
	while [ $j -le ${run_per_conf} ] 
	do
	    rn=`perl -e 'my $minimum = 1E8 ; my $range = 9E7 ; my $random_number = int(rand($range)) + $minimum ; print $random_number '`
	    perl -pe "s/.*/RANDOMIZ          1.$rn./ if $. == 197" < Run2/hallB_target_${target}_${conf_n}.inp > Run2/hallB_target_${target}_${conf_n}_${j}.inp 
	    echo "nice /home/zana/Fluka/flutil/rfluka -e /home/zana/Hall-B/solenoid.exe -M 1 hallB_target_"${target}"_"${conf_n}"_"${j} >> Run2/list_exec.txt 
	    ((j=j+1))
	done
	shift=`awk -vp=$hours -vq=$hconv -vr=$shift 'BEGIN{printf "%d" ,p * q * 5 + r }'`
    fi
done < $file_rev

# read the file in reverse
