#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// FIXME: badly recognizes 6,9,0

char *entries[10 + 4];

int cmpc(const void *a, const void *b) { return *(char *)a > *(char *)b; }
int cmpcal(const void *a, const void *b) {
    /* printf("comparing: %s %s\n", *(char **)a, *(char **)b); */
    return strlen(*(char **)a) > strlen(*(char **)b);
}

void swapc(char *a, char *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

int main() {
    int entryCount = 0;
    while (!feof(stdin)) {
        for (int i = 0; i < 10; i++) {
            entries[i] = malloc(10);
            scanf("%s", entries[i]);
            qsort(entries[i], strlen(entries[i]), sizeof(char), cmpc);
            /* printf("%s ", entries[i]); */
        }

        char tmp[10];
        scanf("%s", tmp);
        /* printf("%s ", tmp); */

        for (int i = 10; i < 14; i++) {
            entries[i] = malloc(10);
            scanf("%s", entries[i]);
            qsort(entries[i], strlen(entries[i]), sizeof(char), cmpc);
            /* printf("%s ", entries[i]); */
        }

        qsort(entries, 10, sizeof(char *), cmpcal);

        char **en = entries;
        char num[10] = {0};
        char a, b, c, d, e, f, g;

        num[0] = 1;
        num[1] = 7;
        num[2] = 4;
        num[9] = 8;

        c = en[0][1];
        f = en[0][0];

        // find a
        for (int i = 0; i < 3; i++) {
            if (en[1][i] != c && en[1][i] != f) a = en[1][i];
        }

        // find 6, make sure of c and f
        for (int i = 6; i <= 8; i++) {
            int cn = 0;
            char lst = 0;
            for (int j = 0; j < 6; j++) {
                if (en[i][j] == c || en[i][j] == f) {
                    cn++;
                    lst = en[i][j];
                }
            }
            if (cn == 1) {
                num[i] = 6;
                if (lst == c) {
                    c = f;
                    f = lst;
                }
                break;
            }
        }

        int i5;

        // find 3, 5 and 2
        for (int i = 3; i <= 5; i++) {
            int cc = 0;
            int cf = 0;
            for (int j = 0; j < 5; j++) {
                if (en[i][j] == f) cf = 1;
                if (en[i][j] == c) cc = 1;
            }
            if (cc == 1 && cf == 1) {
                num[i] = 3;
            } else if (cc == 1) {
                num[i] = 2;
            } else if (cf == 1) {
                num[i] = 5;
                i5 = i;
            }
        }

        // find e
        char missing = 'a';
        for (int i = 0; i < 5; i++) {
            while (en[i5][i] == missing || missing == c) missing++;
        }

        e = missing;

        // find 0 and 9
        for (int i = 0; i < 7; i++) {
            if (en[6][i] == e) {
                num[6] = 0;
                num[8] = 9;
                break;
            } else if (i == 6) {
                num[6] = 9;
                num[8] = 0;
            }
        }

        /* if (entryCount == 3) { */
        /*     for (int i = 0; i < 14; i++) { */
        /*         printf("%7s ", entries[i]); */
        /*     } */
        /*     printf("\n"); */
        /*     printf( */
        /*         "a: %c\n" */
        /*         "b: %c\n" */
        /*         "c: %c\n" */
        /*         "d: %c\n" */
        /*         "e: %c\n" */
        /*         "f: %c\n" */
        /*         "g: %c\n", */
        /*         a, b, c, d, e, f, g); */

        /*     for (int i = 0; i < 10; i++) { */
        /*         printf("%d: %d\n", i, num[i]); */
        /*     } */
        /* } */

        int n = 0;

        // decode
        for (int i = 10; i < 14; i++) {
            for (int j = 0; j < 10; j++) {
                if (strcmp(en[i], en[j]) == 0) {
                    n *= 10;
                    n += num[j];
                    break;
                }
            }
        }

        printf("decoded: %d\n", n);

        for (int i = 0; i < 14; i++) {
            free(en[i]);
        }

        entryCount++;
    }
}
