#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#include <stdio.h>
#include <stdlib.h>
#include <emscripten.h>

#define TABLE_LENGTH 10

typedef struct {
    int key[8];  
} KeyTuple;

void print_keytuple(KeyTuple kt) {
    printf("{ ");
    for (int i = 0; i < 8; i++) {
        printf("%d", kt.key[i]);
        if (i < 7) printf(", ");  // Add commas between elements
    }
    printf(" }\n");
}

typedef struct {
    KeyTuple key;
    char **value; 
} MapEntry;

char* pieces;

MapEntry *map = NULL;

void rotate_string(const char *str, int shift, char *out) {
    int len = strlen(str);
    for (int i = 0; i < len; i++) {
        out[i] = str[(i + shift) % len];
    }
    out[len] = '\0'; // Null-terminate
}

int is_min(const char *str) {
    int len = strlen(str);
    char rotated[len + 1];  

    for (int i = 1; i < len; i++) {
        rotate_string(str, i, rotated);
        if (strcmp(rotated, str) < 0) {
            return 0; // Found a smaller rotation
        }
    }
    return 1; // No smaller rotation found
}

void iterateFull(int len, int ang, int l, int r, int s, int a, int b, int c, int d, int ls, int rs, int ss) {
	if(len >= TABLE_LENGTH) {
		KeyTuple key = {{ang, -a-b, d-c, c+d, a-b, l, r, s}};
		char **list = hmget(map, key);
		arrput(list, strdup(pieces));
		hmput(map, key, list);  // Update hashmap
	} else {
		if(l<ls) {
			pieces[TABLE_LENGTH-1-len] = 'L';
			iterateFull(len+1,ang+1, l+1,r,s, c-d,2-d,a+2,a-b+2, ls, rs, ss);
		}
		if(r<rs) {
			pieces[TABLE_LENGTH-1-len] = 'R';
			iterateFull(len+1,ang-1, l,r+1,s, c+2,c-d,a-b-2,-b-2, ls, rs, ss);
		}
		if(s<ss) {
			pieces[TABLE_LENGTH-1-len] = 'S';
			iterateFull(len+1,ang, l,r,s+1, a+1,b+1,c,d, ls, rs, ss);
		}
	}
}

void iterateLex(int mi, int len, int ang, int l, int r, int s, int a, int b, int c, int d, int ls, int rs, int ss, int minn) {
	if(len >= minn) {
		KeyTuple key = {{12-ang, a+b, c-d, c+d, a-b, ls-l, rs-r, ss-s}};
		char **found_list = hmget(map, key);
		for (size_t i = 0; i < arrlen(found_list); i++) {
			char combined[minn + TABLE_LENGTH + 1];
			strcpy(combined, pieces);
			strcat(combined, found_list[i]);
			if(is_min(combined)) {

				//printf("%lu - %lu\n", strlen(pieces), strlen(found_list[i]));

				//printf("%s - %lu\n",combined, strlen(combined));
				/*for (char *p = combined; *p; p++) {
    printf("%c (%02X) ", *p, (unsigned char)*p);
}
printf("\n");*/
				printf("%s\n",combined);
			}
		}
	} else if (pieces[mi] == 'L') {
		if(l<ls) {
			pieces[len] = 'L';
			iterateLex(mi+1,len+1,ang+1, l+1,r,s, c-d,2-d,a+2,a-b+2, ls, rs, ss, minn);
		}
		if(r<rs) {
			pieces[len] = 'R';
			iterateLex(0,len+1,ang-1, l,r+1,s, c+2,c-d,a-b-2,-b-2, ls, rs, ss, minn);
		}
		if(s<ss) {
			pieces[len] = 'S';
			iterateLex(0,len+1,ang, l,r,s+1, a+1,b+1,c,d, ls,rs,ss,minn);
		}
	} else if (pieces[mi] == 'R') {
		if(r<rs) {
			pieces[len] = 'R';
			iterateLex(mi+1,len+1,ang-1, l,r+1,s, c+2,c-d,a-b-2,-b-2, ls,rs,ss,minn);
		}
		if(s<ss) {
			pieces[len] = 'S';
			iterateLex(0,len+1,ang, l,r,s+1, a+1,b+1,c,d, ls,rs,ss,minn);
		}
	} else if (pieces[mi] == 'S') {
		if(s<ss) {
			pieces[len] = 'S';
			iterateLex(mi+1,len+1,ang, l,r,s+1, a+1,b+1,c,d, ls,rs,ss,minn);
		}
	}
}

void build_table(int c, int s) {
	pieces = malloc((TABLE_LENGTH+1) * sizeof(char));
	pieces[TABLE_LENGTH] = '\0';
	iterateFull(0,0, 0,0,0, 0,0,0,0, 12+c,c,s);
	free(pieces);
}


void find(int curved, int straight) {

	int left = 12 + curved;
	int right = curved;
	int total = left + right + straight;
	int minn = total - TABLE_LENGTH;

	pieces = malloc((minn+1) * sizeof(char));
	pieces[minn] = '\0';
	pieces[0] = 'L';
	iterateLex(0,1,1, 1,0,0, 0,2,2,2, left, right, straight, minn);
	if(right > 0) {
		pieces[0] = 'R';
		iterateLex(0,1,-1, 0,1,0, 2,0,-2,-2, left, right, straight, minn); 
	}
	if(straight > 0) {
		pieces[0] = 'S';
		iterateLex(0,1,0, 0,0,1, 1,1,0,0, left, right, straight, minn);
	}
	
	free(pieces);
}

void free_table() {
	for (size_t i = 0; i < hmlen(map); i++) {
		char **values = map[i].value;
		for (size_t j = 0; j < arrlen(values); j++) {
	    		free(values[j]);  // Free each allocated string
		}
		arrfree(values);  // Free the string array
	}
	hmfree(map);  // Free the hashmap
}

void exact(int curved, int straight) {
	build_table(curved, straight);
	find(curved, straight);
	free_table();
}

EMSCRIPTEN_KEEPALIVE
void include_partial(int curved, int straight) {
	build_table(curved, straight);
	for(int c = 0; c <= curved; c++) {
		for(int s = 0; s <= straight; s++) {
			find(c, s);
		}
	}
	free_table();
}

int main() {
	include_partial(3,5);
}

