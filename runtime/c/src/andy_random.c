#include "andy_random.h"

/* RNG module state */
static uint64_t state = 1;


/* seed manually */
void andy_seed(andy_int seed) {
    
    if (seed == 0)
        seed = 1;   /* avoid zero lock */

    state = (uint64_t)seed;
}


/* seed from system clock */
void andy_randomize(void) {

    state = (uint64_t)time(NULL);
}


/* Linear Congruential Generator (LCG) */
/* generate random float: 0.0 ≤ x < 1.0 */
andy_float andy_random(void) {
    
    /* LCG formula */
    state = state * 6364136223846793005ULL + 1;

    /* Upper bits are more random that lower */
    /* convert upper bits to float */
    return (andy_float)(state >> 11) *
	(1.0 / 9007199254740992.0);
}

/* Random integer between 0 and MAX */
andy_int andy_random_int(andy_int max) {

    return (andy_int)(andy_random()*(uint64_t)max);
}
