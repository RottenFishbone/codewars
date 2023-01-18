// https://www.codewars.com/kata/582041237df353e01d000084
#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>

struct Node {
  struct Node *next;
  int data;
};

typedef bool (*predicate_func) (int);

struct Node *filter_list (const struct Node *list, predicate_func f){
    if (!list) { return NULL; }

    struct Node *this = (struct Node*) list;
    struct Node *root = NULL;
    struct Node *last = NULL;
    do {
        if (!(f)(this->data)) { continue; }
        struct Node *tmp = calloc(1, sizeof(struct Node));
        tmp->data = this->data;

        if (root){ last->next = tmp; } 
        else { root = tmp; }
        last = tmp;
    } while ((this = this->next));

    return root;
}
