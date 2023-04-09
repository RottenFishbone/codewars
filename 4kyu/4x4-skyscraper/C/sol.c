#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// Parameters
#define N 7

// Constants
#define N_ARCS (N+N-2)
#define N_CLUES (N * 4)


// NxN array of arcs with N_ARCS arcs linked to each cell. 
// Each arc is represented as size_t vec[2] = { x, y } where x,y is the 
// coordinates of the connected cell.
// e.g.
//  arcs[1][2][i][0] is the x-value of the i-th arc for cell (2,1)
// The first N_ARCS/2 arcs are row-arcs, the latter half are column-arcs
static size_t arcs[N][N][N_ARCS][2] = {{{{-1ull}}}};

/// Find the last set bit in an integer
/// TODO this could be made faster using ffsll to shift 0's
extern inline 
int flsll(uint64_t i){ 
    if (!i) return 0;
    int c = 0; 
    while (i >>= 1) c++;  
    return c+1;
}

/// Determine if there is a single value in a variable's domain
extern inline
int domain_is_final(uint64_t d){ return ffsll(d) == flsll(d) && d; }


int domain_len(uint64_t d){
    int cnt = 0;
    while (d) {
        d >>= ffsll(d);
        cnt++;
    }

    return cnt;
}

/// Determine the value of a domain, returns zero on non-final domain
extern inline
int final_value(uint64_t d) {
    if (!domain_is_final(d)) return 0;
    return ffsll(d);
}

/// Wrapper over calloc that checks for memory allocation failures
void* checked_calloc(size_t n, size_t size){
    void *m = calloc(n, size);
    if (!m) {
        fprintf(stderr, "Failed to allocate memory. Exiting.\n");
        exit(EXIT_FAILURE);
    }
    return m;
}

uint64_t lowest_entropy(uint64_t **domains){
    size_t min = -1ull, id = -1ull;
    for (size_t y = 0; y < N; ++y){
        for (size_t x = 0; x < N; ++x){
            int len = domain_len(domains[x][y]);
            if (len == 1) { continue; }
            if (len < min){
                min = len;
                id = x + y*N;
            }
        }
    }
    return id;
}

void pretty_print(uint64_t **domains){
    printf("----------------------\n");
    for (size_t y = 0; y < N; ++y){
        for (size_t x = 0; x < N; ++x){
            for (size_t z = 0; z < N; ++z) {
                uint64_t d = domains[y][x];
                printf("%c", d & (1ull << z) ? '0' + (char)(z+1) : ' '); 
            }
            printf("|");
        }
        printf("\n");
    }
}

/// Test if any variable in the board has a domain range > 1
int is_solved(uint64_t **domains){
    for (size_t y = 0; y < N; ++y)
        for (size_t x = 0; x < N; ++x)
            if (!domain_is_final(domains[y][x])) return 0;
    return 1;
}

/// Adds all of a nodes connected to the node at `(x, y)`.
/// Nodes with finalized domains will be ignored (as they *should* not be able
/// to be contstrained further.)
void add_arc_nodes(size_t x, size_t y, size_t *worklist, size_t *wl_len, int in_worklist[N][N], uint64_t **domains){
    for (size_t i = 0; i < N_ARCS; ++i){
        size_t *arc = arcs[y][x][i]; 
        size_t ax = arc[0], ay = arc[1];

        // Add non-final nodes that aren't already present
        if (!in_worklist[ay][ax] && !domain_is_final(domains[ay][ax])){

            worklist[(*wl_len)++] = (ax + ay * N);
            in_worklist[ay][ax] = 1;
        }
    }
}

/// AC3-based constraint propagation.
/// Propagates constraints along arcs until the worklist is empty.
/// The worklist contains indicies of variables that have changed since the last constraint
/// propagation.
/// `in_worklist` is a lookup to avoid queuing a node multiple times.
/// Does not handle clue constraints
int propagate_constraints(size_t *worklist, size_t *wl_len, int in_worklist[N][N], uint64_t **domains){
    size_t *arc, ay, ax, nbr;
    int changed = 0;

    while (*wl_len){
        // Pop the top of the worklist
        size_t id = worklist[(*wl_len)-- - 1];   
        size_t x = id % N, y = id / N;
        in_worklist[y][x] = 0;
        
        uint64_t *var = &domains[y][x];

        // If a finalized variable is in the worklist we constrain its arcs
        if (domain_is_final(*var)) 
            add_arc_nodes(x, y, worklist, wl_len, in_worklist, domains);
        
        // Constrain this node based on finalized neighbours
        size_t orig_domain = *var;
        for (size_t i = 0; i < N_ARCS; ++i) {
            arc = arcs[y][x][i]; 
            ax = arc[0]; ay = arc[1];
            nbr = domains[ay][ax];
            
            // Constrain domain if possible
            if (domain_is_final(nbr) && (*var & nbr) != 0) (*var) &= ~nbr;
        }

        // Finalize this node if it permits a unique within its row/col
        uint64_t diff;
        for (size_t i = 0; i < N_ARCS; ++i){
            // The first N_ARCS/2 arcs are row-arcs, reset diff for col-arcs
            if (i % ((N_ARCS/2)) == 0) { diff = *var; }
            if (!diff) continue;
            arc = arcs[y][x][i]; 
            ax = arc[0]; ay = arc[1];
            nbr = domains[ay][ax];
    
            // Cumulative set-difference on neighbours
            diff &= ~nbr;

            // At end of row/col check if diff contains any elements
            if ((i+1) % (N_ARCS/2) == 0 && domain_is_final(diff)) *var = diff;
        }
        
        if (orig_domain != *var) {
            changed = 1;
            // If we ended up altering this node, maybe we can finalize another
            add_arc_nodes(x, y, worklist, wl_len, in_worklist, domains);
        }
    }

    return changed;
}

void enforce_unique_row(uint64_t row[N]){
    int new_finalized=0;
    do {
        // Iterate over each element enforcing uniqueness if its final
        for (size_t i=0; i < N; ++i){
            if (domain_is_final(row[i])){
                uint64_t val = ffsll(row[i]);
                uint64_t mask = 1ull << (val-1);
                // Iterate over all its friends
                for (size_t j=0; j < N; ++j){
                    if (j == i) continue;

                    // Constrain friend
                    if (row[j] & mask){
                        row[j] &= ~mask;
                        // If its now finalized, we need to repeat this
                        if (domain_is_final(row[j])){
                            new_finalized = 1;
                        }
                    }
                }
            }
        }
    } while (new_finalized--);
}

/// Applies a clue constraint to a left to right row. 
/// To use this in reverse or on a column one should transform into
/// a proper row and back again.
/// I'd like to have done all this in place but having directions using the 
/// same code was just not worth the hassle.
int clue_constrain_row(uint64_t row[N], int clue){
    if (row[0] == (1ull<<(N-1))) return 0; 
    // Find the rightmost possible N value, as nothing is seen past it
    size_t furthest_max = 0;
    for (size_t i = (N-1); i >= 0; --i){
        if (row[i] & (1ull << (N-1))){
            furthest_max = i;
            break;
        }
    } // Invariant: furthest_max != 0
    
    size_t num_final = 0;                                        
    size_t available[N] = {0};
    for (size_t i=0; i < N; ++i){
        if (domain_is_final(row[i])) num_final++;
        for (int j = flsll(row[i])-1; j >= ffsll(row[i])-1; --j){
            if (row[i] & (1ull << j)){
                available[j]++;
            }
        }
    }

    // Obviously can't improve a finalized row
    if (num_final == N) return 0;

    // OVER VISIBLE ELIMINATION:
    // Test if choosing any upper or lower bound would assure too many visible
    // We are looking to choose a variable for a cell and confirm the clue is not
    // exceeded.
    // e.g. 2 -> 12 12 3
    //      Choosing 1 for the first cell would make the minimum visible be 3, thus
    //      we can eliminate 1 as an option.
    //    
    int running_max;
    size_t tmp_avail[N];
    uint64_t tmp_row[N];
    for (int i=0; i < furthest_max; ++i){
        if (domain_is_final(row[i])) { continue; }

        uint64_t tmp, min_mask, taken;
        size_t min_visible; 

        // First cell is always visible, choose its highest value as start point
        min_visible = 0; 
        running_max = 0;
        taken = 0;
        // Iterate over all cells, counting the minimum visible cells
        for (size_t j = 0; j <= furthest_max; ++j){
            uint64_t cell;
            int min,max;
            if (j == i){
                tmp = row[i];
                min = max = ffsl(row[i] & (~taken));
                min_mask = 1ull << (min-1);
                row[i] = min_mask;
                taken |= min_mask;
            }
            else {
                cell = row[j] & (~taken);
                min = ffsll(cell); max = flsll(cell);
            }

            if (max > running_max) {
                // Try to take max to block as much as possible
                min_visible++;
                taken |= 1ull << (max-1);
                running_max = max;
            }
            else {
                // Otherwise take min to save higher numbers for later
                taken |= 1ull << (min-1);
            }

            // Check if the minimum number of visible is impossible
            if (min_visible > clue){
                row[i] = tmp & (~min_mask);     // Pop the min choice
                return 1;
            }
        }

        // Put cell back to normal
        row[i] = tmp;
    }

    size_t assured_visible = 1;         // Number of tiles guaranteed to be visible
    running_max = flsll(row[0]);    // The tallest *possible* value since 0
    for (size_t i = 1; i <= furthest_max; ++i){
        // Min is first set bit, max is last set bit
        int min = ffsll(row[i]), max = flsll(row[i]);
        // Track the min number at each cell assured to be visible
        if (min > running_max) assured_visible++;

        running_max = max > running_max ? max : running_max;
    }
    if (assured_visible == clue) return 0; 
    
    // MAX VALUE ELIMINATION:
    // Test if choosing an upper bound would prevent ever having enough visible
    // We are trying to choose a maximum value and see if that prevents the maximum
    // visible cells reaching the clue requirement
    // e.g. 3 -> 12 12 3
    //      Choosing 2 for the first cell would make max visible be 2, thus we can eliminate
    //      2 as an option.
    // Count the number of each value remaining in the row
    // This is used to detect when a value is unique and to constrain the rest
    for (size_t i=0; i < furthest_max; ++i){
        if (domain_is_final(row[i])) { continue; }
        memcpy(tmp_avail, available, sizeof(size_t) * N);
        memcpy(tmp_row, row, sizeof(uint64_t) * N);
        // Reserve our test cell's highest value
        uint64_t test_val = flsll(row[i]);
        uint64_t test_mask = 1ull << (test_val-1);
        tmp_row[i] = test_mask;
        
        size_t max_visible = 0;

        // Remove test value from the pool
        if ((--available[test_val-1]) == 1) enforce_unique_row(tmp_row);

        // Work from highest value to lowest, right to left in array
        // We try to choose the highest value for the rightmost cell each time
        // Whenever we choose a cell we need to move our window inward as everything
        // non-final after it is now blocked.
        size_t right = furthest_max;
        for (size_t j = N; j >= 1; --j){
            uint64_t mask = 1ull << (j-1);
            for (int k = right; k >= 0; --k){
                if (!(tmp_row[k] & mask)) continue;
                // value `j` exists for row element `k`
                
                max_visible++;
                right = k-1; // Shrink the window
                if (!domain_is_final(tmp_row[k])){
                    tmp_row[k] &= mask;
                    if ((--available[j-1]) == 1) enforce_unique_row(tmp_row);
                }
                
                // Our window is closed
                if (k == 0) goto done_count;
                
                break;
            }
        }

done_count:
        if (max_visible < clue){
            row[i] &= ~test_mask;
            return 1;
        }
    }

    return 0;
}

int apply_clue_constraints(uint64_t **domains, int *clues, size_t *worklist, 
                           size_t *wl_len, int in_worklist[N][N]) {
    
    for (size_t i = 0; i < N_CLUES/2; ++i){
        int clue, opp_clue;
        clue = clues[i];
        opp_clue = clues[(N*3)-1 - i%N + i/N * N];
        if (!clue && !opp_clue) continue;
        uint64_t row[N];
        int changed = 0;
        if (i/N == 0){
            // Top down
            if (clue){
                // Clone from col to row
                for (size_t j = 0; j < N; ++j) row[j] = domains[j][i];
                // Clone back if something was altered
                if ((changed = clue_constrain_row(row, clue))){
                    for (size_t j = 0; j < N; ++j) {
                        domains[j][i] = row[j];
                        add_arc_nodes(i, j, worklist, wl_len, in_worklist, domains);
                    }
                    return changed;
                }
            }
            // Bottom up
            else if (opp_clue){
                // Clone from reversed col to row
                for (size_t j = 0; j < N; ++j) row[j] = domains[N-j-1][i];
                // Clone back if something was altered
                if ((changed = clue_constrain_row(row, opp_clue))){
                    for (size_t j = 0; j < N; ++j){
                        if (domains[N-j-1][i] != row[j]){
                            domains[N-j-1][i] = row[j];
                            add_arc_nodes(i, N-j-1, worklist, wl_len, in_worklist, domains);
                        };
                    }
                    return changed;
                }
            }
        }
        else {
            size_t y = i%N;
            // Left to right
            if (opp_clue){
                // Clone from row to row
                for (size_t j = 0; j < N; ++j) row[j] = domains[y][j];
                
                // Clone back if something was altered
                if ((changed = clue_constrain_row(row, opp_clue))){
                    for (size_t j = 0; j < N; ++j) {
                        if (domains[y][j] != row[j]){
                            domains[y][j] = row[j];
                            add_arc_nodes(j, y, worklist, wl_len, in_worklist, domains);
                        }
                    }
                    return changed;
                }
            }
            // Right to left
            else if (clue){
                // Clone from reverse row to row
                for (size_t j = 0; j < N; ++j) row[j] = domains[y][N-j-1];
                // Clone back if something was altered
                if ((changed = clue_constrain_row(row, clue))){
                    for (int j = 0; j < N; ++j) {
                        if (domains[y][N-j-1] != row[j]){
                            domains[y][N-j-1] = row[j];
                            add_arc_nodes(N-j-1, y, worklist, wl_len, in_worklist, domains);
                        }
                    }
                    return changed;
                }
            }
        }
    }
    return 0;
}


int** SolvePuzzle (int *clues) {
    if (N > 64) { 
        // We can't store a domain range of more than 64 in a uint64_t
        fprintf(stderr, "Thats too many skyscrapers, get some help.\n");
        exit(EXIT_FAILURE);
    }
    // Initialize the arcs once
    if (****arcs == -1ull){
        for (size_t y = 0; y < N; ++y){
            for (size_t x = 0; x < N; ++x){
                size_t cnt = 0;
                // Store row-arcs first
                for (size_t i = 0; i < N; i++){
                    if (i == x) continue;
                    arcs[y][x][cnt][0] = i;
                    arcs[y][x][cnt++][1] = y;
                }
                // Then column-arcs
                for (size_t i = 0; i < N; i++){
                    if (i == y) continue;
                    arcs[y][x][cnt][0] = x;
                    arcs[y][x][cnt++][1] = i;
                }
            }
        }
    }

    // Domain of a completely unknown variable in an NxN
    const uint64_t UNKNOWN = N < 64 ? ~0ull >> (64-N): 0ull;    // shifting all bits out is undefined
                                                            
    // Init entire board to UNKNOWN
    uint64_t **domains = checked_calloc(N, sizeof(uint64_t*));
    for (size_t i = 0; i < N; ++i){
        domains[i] = checked_calloc(N, sizeof(uint64_t));
        for (size_t j = 0; j < N; ++j){
            domains[i][j] = UNKNOWN;
        }
    }

    // Initial Clue Constraints
    for (size_t i = 0; i < N_CLUES; ++i) {
        int clue = clues[i];
        if (!clue) { continue; }
        
        for (size_t j = 0; j < clue; ++j){
            size_t x,y;
            if ((i/N)%2 == 0){
                y = (i/N) == 0 ? j : N-j-1;          // Handle both up and down
                x = (i/N) == 0 ? i%N : N-(i%N)-1;    // Swap x ordering on bottom
            } else {
                y = (i/N) == 3 ? (N-(i%N)-1) : i%N;  // Swap x ordering on right
                x = (i/N) == 3 ? j : N-j-1;          // Handle both left and right
            }
            // 1 is a special case where we know the skyscraper is N
            if (clue == 1) { domains[y][x] = 1ull << (N-1); }
            // Handle N separately to save iterations of constraint solving
            else if (clue == N) { domains[y][x] = 1ull << j; }
            // Each cell away from the clue relaxes the constraint
            else { domains[y][x] &= UNKNOWN >> (clue-j-1); }
        }
    }

    // Init worklist to contain everything for initial propagation
    size_t worklist[N*N];
    int in_worklist[N][N];
    for (size_t i = 0; i < N*N; ++i){
        worklist[i] = i;
        in_worklist[i/N][i%N] = 1;
    }
    size_t wl_len = N*N;
    
    while (wl_len){// || !is_solved(domains)) {
        propagate_constraints(worklist, &wl_len, in_worklist, domains);
        apply_clue_constraints(domains, clues, worklist, &wl_len, in_worklist);
    }

    pretty_print(domains);
    return NULL;
}

int main() {
//    int clues[4*4] = {
//        0,1,0,0,
//        0,0,1,2,
//        0,2,0,0,
//        0,3,0,0,
//    };

//    int clues[6*6] = {
//        0,0,0,2,2,0,
//        0,0,0,6,3,0,
//        0,4,0,0,0,0,
//        4,4,0,3,0,0,
//    };

    int clues[7*7] = {
        7, 0, 0, 0, 2, 2, 3,
        0, 0, 3, 0, 0, 0, 0,
        3, 0, 3, 0, 0, 5, 0,
        0, 0, 0, 0, 5, 0, 4,
    }; 


    SolvePuzzle(clues);
    return EXIT_SUCCESS;
}
