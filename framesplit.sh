csplit -k -s -f testy. -b '%02d.ppm' testy.ppm '/^###CUT HERE###/' "{100}" 2>/dev/null
for file in `ls testy.??.ppm`
    do
        tail -n +2 $file > a$file
        rm $file
    done          
