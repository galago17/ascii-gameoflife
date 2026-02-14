Test pattern: <https://conwaylife.com/wiki/Jaydot>

**v1**: 999 generations, 107 x 46, 311 alive

Evaluation took:  
  505.916 seconds of real time  
  492.080981 seconds of total run time (490.602496 user, 1.478485 system)  
  [ Real times consist of 0.111 seconds GC time, and 505.805 seconds non-GC time. ]  
  [ Run times consist of 0.111 seconds GC time, and 491.970 seconds non-GC time. ]  
  97.27% CPU  
  9,156,407,328 bytes consed  

On average **2 s/generation**.  
(Comparable to v2 average time for the first ~200 generations).

**v2**: 999 generations, 107 x 46, 311 alive

Evaluation took:  
  49.774 seconds of real time  
  49.295530 seconds of total run time (49.131228 user, 0.164302 system)  
  [ Real times consist of 0.018 seconds GC time, and 49.756 seconds non-GC time. ]  
  [ Run times consist of 0.018 seconds GC time, and 49.278 seconds non-GC time. ]  
  99.04% CPU  
  1,402,349,584 bytes consed  

On average **50 ms/generation**.  
(40x faster)
