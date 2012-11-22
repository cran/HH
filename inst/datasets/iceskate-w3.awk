BEGIN {tmp=0; tech=0}
/,/ {name=$0;
     if ($1=="Gold")   $1=1;
     if ($1=="Silver") $1=2;
     if ($1=="Bronze") $1=3;
     if ($2=="medal")  name= $1 " " $3 " " $4 " " $5 " " $6}
/TECHNICAL/ {tech=1; print name;
             split($0,techn); if ($2=="MERIT") techn[2]="";
             for (i = 1; i <= 14; i++) printf("%s ", techn[i]); print ""}
/PRESENTATION/ {if (tmp==0 && tech==1) {print $0; tmp=1} else {tmp=0; tech=0}}
