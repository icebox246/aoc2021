#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int mapWidth = 0;
int mapHeight = 0;

typedef struct pair pair;
struct pair {
    int fst, snd;
};

pair priorityQueue[2500005];
int priorityQueueSize = 0;

unsigned int dist[12500005];
unsigned int cost[500005];

int cmpPair(const void *a, const void *b) {
    return ((pair *)b)->snd - ((pair *)a)->snd;
}

void insertPQ(pair item) {
    priorityQueue[priorityQueueSize] = item;
    priorityQueueSize++;
    qsort(priorityQueue, priorityQueueSize, sizeof(pair), cmpPair);
}

pair popPQ() {
    priorityQueueSize--;
    return priorityQueue[priorityQueueSize];
}

pair makePair(int fst, int snd) {
    pair p;
    p.fst = fst;
    p.snd = snd;
    return p;
}

void part1() {
    memset(dist, -1, sizeof(dist));
    insertPQ(makePair(0, 0));
    dist[0] = 0;

    while (priorityQueueSize) {
        pair p = popPQ();
        if (p.snd > dist[p.fst]) continue;
        int cn = p.fst;
        int x = p.fst % mapWidth;
        int y = p.fst / mapWidth;

        if (x > 0) {
            int nb = x - 1 + y * mapWidth;
            if (dist[nb] > dist[cn] + cost[nb]) {
                dist[nb] = dist[cn] + cost[nb];
                insertPQ(makePair(nb, dist[nb]));
            }
        }

        if (x < mapWidth - 1) {
            int nb = x + 1 + y * mapWidth;
            if (dist[nb] > dist[cn] + cost[nb]) {
                dist[nb] = dist[cn] + cost[nb];
                insertPQ(makePair(nb, dist[nb]));
            }
        }

        if (y > 0) {
            int nb = x + (y - 1) * mapWidth;
            if (dist[nb] > dist[cn] + cost[nb]) {
                dist[nb] = dist[cn] + cost[nb];
                insertPQ(makePair(nb, dist[nb]));
            }
        }

        if (y < mapHeight - 1) {
            int nb = x + (y + 1) * mapWidth;
            if (dist[nb] > dist[cn] + cost[nb]) {
                dist[nb] = dist[cn] + cost[nb];
                insertPQ(makePair(nb, dist[nb]));
            }
        }
    }

    printf("Part I:\n%d\n", dist[mapWidth * (mapHeight - 1) + mapWidth - 1]);
}

int getCost(int idx) {
	int x = idx % (mapWidth * 5);
	int y = idx / (mapWidth * 5);

	int incs = x / mapWidth + y / mapHeight;
	int rx = x % mapWidth;
	int ry = y % mapHeight;

	return (cost[rx + ry * mapWidth] - 1 + incs) % 9 + 1;
}

void part2() {
    memset(dist, -1, sizeof(dist));
    insertPQ(makePair(0, 0));
    dist[0] = 0;

    while (priorityQueueSize) {
        pair p = popPQ();
        if (p.snd > dist[p.fst]) continue;
        int cn = p.fst;
        int x = p.fst % (mapWidth * 5);
        int y = p.fst / (mapWidth * 5);

        if (x > 0) {
            int nb = x - 1 + y * (mapWidth * 5);
            if (dist[nb] > dist[cn] + getCost(nb)) {
                dist[nb] = dist[cn] + getCost(nb);
                insertPQ(makePair(nb, dist[nb]));
            }
        }

        if (x < (mapWidth * 5) - 1) {
            int nb = x + 1 + y * (mapWidth * 5);
            if (dist[nb] > dist[cn] + getCost(nb)) {
                dist[nb] = dist[cn] + getCost(nb);
                insertPQ(makePair(nb, dist[nb]));
            }
        }

        if (y > 0) {
            int nb = x + (y - 1) * (mapWidth * 5);
            if (dist[nb] > dist[cn] + getCost(nb)) {
                dist[nb] = dist[cn] + getCost(nb);
                insertPQ(makePair(nb, dist[nb]));
            }
        }

        if (y < mapHeight * 5 - 1) {
            int nb = x + (y + 1) * (mapWidth * 5);
            if (dist[nb] > dist[cn] + getCost(nb)) {
                dist[nb] = dist[cn] + getCost(nb);
                insertPQ(makePair(nb, dist[nb]));
            }
        }
    }

    printf("Part II:\n%d\n", dist[(mapWidth * 5) * (mapHeight * 5 - 1) + (mapWidth * 5) - 1]);
}

int main() {
    char line[128];
    while (1) {
        memset(line, 0, sizeof(line));
        scanf("%s", line);
        if (!strlen(line)) break;
        mapWidth = strlen(line);
        for (int i = 0; i < mapWidth; i++) {
            cost[mapWidth * mapHeight + i] = line[i] - '0';
        }
        mapHeight++;
    }

	part1();
	part2();
}
