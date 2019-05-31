file="timeline.txt"
# 3600/5 = 720
hconv=720 
run_per_conf=100
sub=1
shift=0
here_pos=`pwd`
file_rev="reverse_beam_time.txt"
sed 1d $file | tac > $file_rev
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
    if [ "${target}" == "NO" ]
    then
	echo "No target time"
    else
	echo "Conf n.="$conf_n "target="$target " seconds="$seconds "current="$current " energy="$energy " shift="$shift
	perl -pe "s/.*/BEAM            -$energy             .142857      -0.1      -0.1          ELECTRON/ if $. == 6" < hall-b_cone_${target}.inp >  hallB_target_${target}_${conf_n}.inp
	j=0
	while [ $j -le ${run_per_conf} ] 
	do
	    rn=`perl -e 'my $minimum = 1E8 ; my $range = 9E7 ; my $random_number = int(rand($range)) + $minimum ; print $random_number '`
	    perl -pe "s/.*/RANDOMIZ          1.$rn./ if $. == 197" < hallB_target_${target}_${conf_n}.inp > hallB_target_${target}_${conf_n}_${j}.inp 
	    echo "PROJECT: radcon" > farmrun_radcon_hall${conf_n}_${j}.jsub 
	    echo "TRACK: simulation" >> farmrun_radcon_hall${conf_n}_${j}.jsub
	    echo "JOBNAME: HallBtarget"${conf_n} >> farmrun_radcon_hall${conf_n}_${j}.jsub
	    echo "COMMAND: ~/Hall-B/Hall-B-Beam-accounting/run_fluka.tcsh" >> farmrun_radcon_hall${conf_n}_${j}.jsub
	    echo "MEMORY: 2000 MB" >> farmrun_radcon_hall${conf_n}_${j}.jsub                        
	    echo "OS: centos7" >> farmrun_radcon_hall${conf_n}_${j}.jsub                            
	    echo "INPUT_FILES: "${here_pos}"/hallB_target_"${target}"_"${conf_n}"_"${j}".inp" >> farmrun_radcon_hall${conf_n}_${j}.jsub
	    sub=0
	    sub=`ls -1 /volatile/clas12/zana/hallB_target_${target}_${conf_n}_${j}*_fort.21 | wc -l`
	    if [ ${sub} -eq "0" ]                                                                               
            then
		echo "Submitting job Conf n." $conf_n " n." $j
		jsub farmrun_radcon_hall${conf_n}_${j}.jsub
	    fi
	    ((j=j+1))
	done
	shift=`awk -vp=$hours -vq=$hconv -vr=$shift 'BEGIN{printf "%d" ,p * q * 5 + r }'`
    fi
done < $file_rev
