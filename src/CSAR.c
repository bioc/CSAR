#include "CSAR.h"



void pos2Nhits(int *pos, int *lpos, int *nhits, int *w, int *l){
for(int j=0; j<lpos[0]; j++){
   for(int i=0; i<=(w[0]-1); i++){
nhits[pos[j]+i-1]++;}}
}


