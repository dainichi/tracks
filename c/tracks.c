#include <stdio.h>

const int L = 1; //left curve
const int R = 2; //right curve
const int S = 3; //straight piece3

int perm[23]; //should be at least the max number of pieces
int counter;
int total;

int cyclic(int mi){
   for(int i=mi, c=0; i<total; i++,c++) if(perm[i] != perm[c]) return 0;
   return 1;
}

void check(){ //a,b,c,d represent a translation by vector (a+b+(c-d)sqrt3,c+d+(a-b)sqrt3)
   int a=0, b=0, c=0, d=0, na, nb;
   for(int i=total-1; i >= 0; i-- ){ //rotation and translation
      if(perm[i] == L){na=c-d; nb=2-d; c=a+2; d=a-b+2; a=na; b=nb;}
      else if(perm[i] == R){na=c+2; nb=c-d; c=a-b-2; d=-b-2; a=na; b=nb;}
      else /*if(perm[i] == S)*/ {a=a+1; b=b+1;}
   }
   if(a==0 && b==0 && c==0 && d==0){
      counter++;
      for(int i=0; i<total; i++) printf(perm[i] == L ? "L" : perm[i] == R ? "R" : perm[i] == S ? "S" : "");
      printf("\n");
   }
}

//lexically minimal cyclic permutations
void permute(int l, int r, int s, int acc, int mi){
   if(acc == total){
      if(mi == 0 || cyclic(mi)) check();
   }
   if(l > 0 && L >= perm[mi]){
      perm[acc] = L;
      permute(l-1, r, s, acc+1, perm[mi] == L ? mi+1 : 0);
   }
   if(r > 0 && R >= perm[mi]){
      perm[acc] = R;
      permute(l, r-1, s, acc+1, perm[mi] == R ? mi+1 : 0);
   }
   if(s > 0 && S >= perm[mi]){
      perm[acc] = S;
      permute(l, r, s-1, acc+1, perm[mi] == S ? mi+1 : 0);
   }
}

int main(){
   for(int r = 3; r >= 0; r--){
      for(int s = 5; s >= 0; s--){
         int l = r+12;
         total = l+r+s;
         counter = 0;

         perm[0] = L;
         permute(l-1, r, s, 1, 0);
         perm[0] = R;
         permute(l, r-1, s, 1, 0);
         perm[0] = S;
         permute(l, r, s-1, 1, 0);

         printf("%d L, %d R, %d S, %d solutions\n", l, r, s, counter);
      }
   }
}
