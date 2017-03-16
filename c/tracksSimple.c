//Make coordinates available during search. Now closed tracks are outputted even if pieces aren't used up.
//Current translation is maintained in parameters
#include <stdio.h>

#define L 0 //left curve
#define R 1 //right curve
#define S 2 //straight piece3
#define MAX 2

int perm[23]; //should be at least the max number of pieces
int bag[3];

//a,b,c,d represent a translation by vector (a+b+(c-d)sqrt3,c+d+(a-b)sqrt3)
//go through lexically minimal permutations and print if closed track
void permute(int acc, int mi, int a, int b, int c, int d) {
   if(a == 0 && b==0 && c==0 && d==0) {
      if(acc % (acc - mi) == 0 && bag[L]==bag[R]) {
         for(int i=0; i<acc; i++) printf(perm[i] == L ? "L" : perm[i] == R ? "R" : perm[i] == S ? "S" : "");
         printf("\n");
      }
   } else { 
      for(int p = perm[mi]; p<=MAX; p++) {
         if(bag[p] > 0) {
            bag[p]--;
            perm[acc] = p;
            switch(p) {
               case L: 
                  permute(acc+1,perm[mi] == p ? mi+1 : 0,c-d,2-d,a+2,a-b+2);
                  break;
               case R:
                  permute(acc+1,perm[mi] == p ? mi+1 : 0,c+2,c-d,a-b-2,-b-2);
                  break;
               case S:
                  permute(acc+1,perm[mi] == p ? mi+1 : 0,a+1,b+1,c,d);
                  break;
            }
            bag[p]++;
         }
      }
   }
}

int main() {
   bag[L]=15-1; bag[R]=3; bag[S]=5;
   perm[0] = L;
   permute(1,0,0,2,2,2);
}

