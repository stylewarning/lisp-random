// N-body benchmark - C reference
// Compiled with: clang -O3 -ffast-math -march=native -o nbody nbody.c -lm
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define PI 3.141592653589793
#define SOLAR_MASS (4 * PI * PI)
#define DAYS_PER_YEAR 365.24
#define NBODIES 5

struct body {
    double x, y, z;
    double vx, vy, vz;
    double mass;
};

// Global array for better optimization
struct body bodies[NBODIES] = {
    { 0, 0, 0, 0, 0, 0, SOLAR_MASS },
    { 4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01, 1.66007664274403694e-03 * DAYS_PER_YEAR, 7.69901118419740425e-03 * DAYS_PER_YEAR, -6.90460016972063023e-05 * DAYS_PER_YEAR, 9.54791938424326609e-04 * SOLAR_MASS },
    { 8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01, -2.76742510726862411e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR, 2.85885980666130812e-04 * SOLAR_MASS },
    { 1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01, 2.96460137564761618e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR, 4.36624404335156298e-05 * SOLAR_MASS },
    { 1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01, 2.68067772490389322e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR, 5.15138902046611451e-05 * SOLAR_MASS },
};

static inline void advance(struct body * restrict b, double dt) {
    for (int i = 0; i < 5; i++) {
        for (int j = i + 1; j < 5; j++) {
            double dx = b[i].x - b[j].x;
            double dy = b[i].y - b[j].y;
            double dz = b[i].z - b[j].z;
            double dsq = dx * dx + dy * dy + dz * dz;
            double mag = dt / (dsq * sqrt(dsq));

            b[i].vx -= dx * b[j].mass * mag;
            b[i].vy -= dy * b[j].mass * mag;
            b[i].vz -= dz * b[j].mass * mag;
            b[j].vx += dx * b[i].mass * mag;
            b[j].vy += dy * b[i].mass * mag;
            b[j].vz += dz * b[i].mass * mag;
        }
    }
    for (int i = 0; i < 5; i++) {
        b[i].x += dt * b[i].vx;
        b[i].y += dt * b[i].vy;
        b[i].z += dt * b[i].vz;
    }
}

static double energy(struct body *b) {
    double e = 0.0;
    for (int i = 0; i < NBODIES; i++) {
        e += 0.5 * b[i].mass * (b[i].vx * b[i].vx + b[i].vy * b[i].vy + b[i].vz * b[i].vz);
        for (int j = i + 1; j < NBODIES; j++) {
            double dx = b[i].x - b[j].x;
            double dy = b[i].y - b[j].y;
            double dz = b[i].z - b[j].z;
            e -= (b[i].mass * b[j].mass) / sqrt(dx * dx + dy * dy + dz * dz);
        }
    }
    return e;
}

static void offset_momentum(struct body *b) {
    double px = 0, py = 0, pz = 0;
    for (int i = 0; i < NBODIES; i++) {
        px += b[i].vx * b[i].mass;
        py += b[i].vy * b[i].mass;
        pz += b[i].vz * b[i].mass;
    }
    b[0].vx = -px / SOLAR_MASS;
    b[0].vy = -py / SOLAR_MASS;
    b[0].vz = -pz / SOLAR_MASS;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: nbody <iterations>\n");
        return 1;
    }
    int n = atoi(argv[1]);

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    for (int i = 0; i < n; i++) advance(bodies, 0.01);
    clock_gettime(CLOCK_MONOTONIC, &end);
    long ms = (end.tv_sec - start.tv_sec) * 1000 + (end.tv_nsec - start.tv_nsec) / 1000000;
    fprintf(stderr, "%.9f\n", energy(bodies));
    fprintf(stderr, "timing: %ld ms\n", ms);
    return 0;
}
