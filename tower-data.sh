#!/usr/bin/sh
grep Measure $1 | \
  awk -F= '{printf("%s[%s]",1==NR?"[":",",$2); if(0==NR%5)printf("\n");}; END{print"]"}'
