#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

int main() {

  struct node {
      int kind;
      struct node *o1;
      struct node *o2;
      struct node *o3;
      int val;
  };

  typedef struct node node;

  node **node_stack = malloc(sizeof(node *) * 1000);
  int nbr_nodes = 0;

//      // allocate the array of 3 floats at each index
//      for (int i = 0; i < 1000; i++)
//        node_stack[i] = malloc(sizeof(float) * 3);


  node *new_node(int k) {
    node_stack[nbr_nodes] = malloc(sizeof(node));
    if (NULL == node_stack[nbr_nodes]) { memory_error(x); }
    node_stack[nbr_nodes]->kind = k;
    nbr_nodes = nbr_nodes + 1;
    return node_stack[nbr_nodes - 1];
  }

  new_node(2);
  new_node(2);
  new_node(5);


  //printf("%i\n", node_stack[1]);
  //free(node_stack[1]);
  //printf("%i\n", node_stack[1]);


  for (int i = 0; i < nbr_nodes; i++) {
    free(node_stack[i]);
  }

  free(node_stack);

  printf("%i\n",node_stack[1]);

  return 0;
}
