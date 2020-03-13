#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <setjmp.h>
#include <limits.h>// pour verifier overflow

/* Analyseur lexical. */

enum {  DO_SYM, ELSE_SYM, IF_SYM, WHILE_SYM, BREAK_SYM, CONTINUE_SYM, GOTO_SYM, PRINT_SYM, LBRA, RBRA, LPAR,// break, continue, goto, print
       RPAR, PLUS, MINUS, LESS, SEMI, EQUAL, INT, ID, EOI,MORE,// >
       EVEN, NOT_EVEN, LESS_OR_EVEN, MORE_OR_EVEN,PRODUCT, QUOTIENT, MODULO,COLON// ==, !=, <=, >=, *, /, %,:
       };
char *words[] = { "do", "else", "if", "while", "break", "continue", "goto", "print", NULL };

jmp_buf fin;
int ch = ' ';
int sym;
int int_val;
char id_name[100];

struct node
{
    int kind;
    struct node *o1;
    struct node *o2;
    struct node *o3;
    int val;
};

typedef struct node node;

node **node_stack ;int nbr_nodes = 0; 

void fin_programme(){// free() et ptr=null

  for (int i = 0; i < nbr_nodes; i++) {
    free(node_stack[i]);
    node_stack[i] = NULL;
  }
  free(node_stack);
  node_stack = NULL;
}

void free_nodes(node* node_parent){// free() et ptr=null
  if(node_parent->o1!=NULL){free_nodes(node_parent->o1);}
  if(node_parent->o2!=NULL){free_nodes(node_parent->o2);}
  if(node_parent->o3!=NULL){free_nodes(node_parent->o3);}

  free(node_parent);
  node_parent = NULL;
  free(node_stack);
  node_stack = NULL;
}

void syntax_error(){fprintf(stderr, "syntax error\n"); longjmp(fin,1);}
void memory_error(){fprintf(stderr,"memory_error"); longjmp(fin,1);}
void overflow_error(){fprintf(stderr,"overflow_error"); longjmp(fin,1);}

void safe_int_op(int a, int b, int operateur){//retourne 0 si overflow ou underflow ou divise par 0
  _Bool overflow = 0;//Overflow == false
  if(operateur == 1){//addition
    if (a > INT_MAX - b){overflow = 1;}; /* overflow */
    if (a < INT_MIN - b){overflow = 1;}; /* underflow */
  }else if(operateur == 2){//soustraction
    if (a > INT_MAX + b){overflow = 1;}; /* overflow */
    if (a < INT_MIN + b){overflow = 1;}; /* underflow */
  }else if(operateur == 3){//multiplication
    if(b != 0){
      if (a > INT_MAX / b){overflow = 1;}; /* overflow */
      if (a < INT_MIN / b){overflow = 1;}; /* underflow */
    }
  }else if(operateur == 4){//division
    if(b != 0){
      if (a == INT_MIN && b == -1){overflow = 1;}; /* |INT_MIN| > |INT_MAX|, donc ce cas extreme overflow aussi*/
    }else {overflow = 1;}//division par 0
  }
  if(overflow){overflow_error();}
}

void initialisation_node_stack(){node_stack = malloc(sizeof(node*) * 1000);if (NULL == node_stack) { memory_error(); } } 
void next_ch() { ch = getchar(); }

void next_sym()
{
  while (ch == ' ' || ch == '\n') next_ch();
  switch (ch)
    { case '{': sym = LBRA;  next_ch(); break;
      case '}': sym = RBRA;  next_ch(); break;
      case '(': sym = LPAR;  next_ch(); break;
      case ')': sym = RPAR;  next_ch(); break;
      case '+': sym = PLUS;  next_ch(); break;
      case '-': sym = MINUS; next_ch(); break;
      case '<': next_ch(); if(ch != '='){ sym = LESS; break; } else { sym = LESS_OR_EVEN; next_ch(); break; } // <=
      case '>': next_ch(); if(ch != '='){ sym = MORE; break; } else { sym = MORE_OR_EVEN; next_ch(); break; } // > et >=
      case ';': sym = SEMI;  next_ch(); break;
      case '!': next_ch(); if(ch != '='){ syntax_error(); } else { sym = NOT_EVEN ; next_ch(); break; } //!=
      case '=': next_ch(); if(ch != '='){ sym = EQUAL; break; } else { sym = EVEN ; next_ch(); break; } // ==
      case ':': sym = COLON;  next_ch(); break; //: (ETIQUETTE)
      case '*': sym = PRODUCT; next_ch(); break; //*
      case '/': sym = QUOTIENT; next_ch(); break; ///
      case '%': sym = MODULO; next_ch(); break; //%
      case EOF: sym = EOI;   next_ch(); break;
      default:
        if (ch >= '0' && ch <= '9')
          {
            int_val = 0;

            while (ch >= '0' && ch <= '9')
              {
                safe_int_op(int_val,10,3);//Verification overflow / underflow
                int_val = int_val*10 + (ch - '0');
                next_ch();
              }

            sym = INT;
          }
        else if (ch >= 'a' && ch <= 'z')
          {
            int i = 0;

            while ((ch >= 'a' && ch <= 'z') || ch == '_')
              {
                id_name[i++] = ch;
                next_ch();
              }

            id_name[i] = '\0';
            sym = 0;

            while (words[sym]!=NULL && strcmp(words[sym], id_name)!=0)
            safe_int_op(sym,1,1);//Verification overflow / underflow
            sym++;

            if (words[sym] == NULL)
              {
                if (id_name[1] == '\0') sym = ID; else syntax_error();
              }
          }
        else syntax_error();
    }
}

/* Analyseur syntaxique. */

enum { VAR, CST, ADD, SUB, LT, ASSIGN,
       IF1, IF2, WHILE, DO, EMPTY, SEQ, EXPR, PROG,
       MT, EV, NEV, LTOEV, MTOEV, //>, ==, !=, <=, >=
       MUL, DIV, MOD,//*, /, %
       BR, CONT, GT, PRT, ETQUT}; //break, continue, goto, print, :(ETIQUETTE)

node *new_node(int k) {
  node_stack[nbr_nodes] = malloc(sizeof(node));
  if (NULL == node_stack[nbr_nodes]) { memory_error(); }
  node_stack[nbr_nodes]->kind = k;
  nbr_nodes = nbr_nodes + 1;
  return node_stack[nbr_nodes - 1];
}

node *paren_expr(); 
node *statement(); 

node *term() /* <term> ::= <id> | <int> | <paren_expr> */
{
  node *x;

  if (sym == ID)           /* <term> ::= <id> */
    {
      x = new_node(VAR);
      x->val = id_name[0]-'a';
      next_sym();
    }
  else if (sym == INT)     /* <term> ::= <int> */
    {
      x = new_node(CST);
      x->val = int_val;
      next_sym();
    }
  else                     /* <term> ::= <paren_expr> */
    x = paren_expr();

  return x;
}

node *mult() //<mult> ::= <term>
{
    node *x = term();

    while (sym == PRODUCT || sym == QUOTIENT || sym==MODULO)
    {
        node *t = x;
        x = new_node(sym==PRODUCT ? MUL : (sym==QUOTIENT ? DIV : MOD));
        next_sym();
        x->o1 = t;
        x->o2 = term();
    }

    return x;
}

node *sum() /* <sum> ::= <mult>|<sum>"+"<mult>|<sum>"-"<mult> */
{
  node *x = mult();

  while (sym == PLUS || sym == MINUS)
    {
      node *t = x;
      x = new_node(sym==PLUS ? ADD : SUB);
      next_sym();
      x->o1 = t;
      x->o2 = mult();
    }

  return x;
}

node *test() //<test> ::= <sum>
{
  node *x = sum();

  if (sym == LESS) /* <test> ::= <sum> | <sum> "<" <sum> */
    {
      node *t = x;
      x = new_node(LT);
      next_sym();
      x->o1 = t;
      x->o2 = sum();
    }
  else  if (sym == MORE) /* <test> ::= <sum> | <sum> ">" <sum> */
  {
      node *t = x;//>
      x = new_node(MT);
      next_sym();
      x->o1 = t;
      x->o2 = sum();
  }
  else  if (sym == EVEN) /* <test> ::= <sum> | <sum> "==" <sum> */
  {
      node *t = x;//==
      x = new_node(EV);
      next_sym();
      x->o1 = t;
      x->o2 = sum();
  }
  else  if (sym == NOT_EVEN) /* <test> ::= <sum> | <sum> "!=" <sum> */
  {
      node *t = x;//!=
      x = new_node(NEV);
      next_sym();
      x->o1 = t;
      x->o2 = sum();
  }
  else  if (sym == LESS_OR_EVEN) /* <test> ::= <sum> | <sum> "!=" <sum> */
  {
      node *t = x;//<=
      x = new_node(LTOEV);
      next_sym();
      x->o1 = t;
      x->o2 = sum();
  }
  else  if (sym == MORE_OR_EVEN) /* <test> ::= <sum> | <sum> "!=" <sum> */
  {
      node *t = x;//>=
      x = new_node(MTOEV);
      next_sym();
      x->o1 = t;
      x->o2 = sum();
  }
  return x;
}

node *expr() /* <expr> ::= <test> | <id> "=" <expr> */
{
  node *x;

  if (sym != ID) return test();

  x = test();

  if (sym == EQUAL)
    {
      node *t = x;
      x = new_node(ASSIGN);
      next_sym();
      x->o1 = t;
      x->o2 = expr();
    }

  return x;
}

node *paren_expr() /* <paren_expr> ::= "(" <expr> ")" */
{
  node *x;
  if (sym == LPAR) next_sym(); else  syntax_error();

  x = expr();

  if (sym == RPAR) next_sym(); else syntax_error();

  return x;
}

node *statement()
{
  node *x;
    if (sym == IF_SYM)       /* "if" <paren_expr> <stat> */
    {
      x = new_node(IF1);
      next_sym();
      x->o1 = paren_expr();
      x->o2 = statement();
      if (sym == ELSE_SYM) /* ... "else" <stat> */
        { x->kind = IF2;
          next_sym();
          x->o3 = statement();
        }
    }
  else if (sym == WHILE_SYM) /* "while" <paren_expr> <stat> */
    {
      x = new_node(WHILE);
      next_sym();
      x->o1 = paren_expr();
      x->o2 = statement();
    }
  else if (sym == DO_SYM)  /* "do" <stat> "while" <paren_expr> ";" */
    {
      x = new_node(DO);
      next_sym();
      x->o1 = statement();
      if (sym == WHILE_SYM) next_sym(); else syntax_error();
      x->o2 = paren_expr();
      if (sym == SEMI) next_sym(); else syntax_error();
    }
  else if (sym == SEMI)    /* ";" */
    {
      x = new_node(EMPTY);
      next_sym();
    }
  else if (sym == LBRA)    /* "{" { <stat> } "}" */
    {
      x = new_node(EMPTY);
      next_sym();
      while (sym != RBRA)
        {
          node *t = x;
          x = new_node(SEQ);
          x->o1 = t;
          x->o2 = statement();
        }
      next_sym();
    }
  else if (sym == BREAK_SYM) /*"break" [ <id> ] ";" */
    {
      x = new_node(BR);
      next_sym();
      if (sym == ID) next_sym();
      if (sym == SEMI) next_sym(); else syntax_error();
    }
  else if (sym == CONTINUE_SYM) /*"continue" [ <id> ] ";" */
    {
      x = new_node(CONT);
      next_sym();
      if (sym == ID) next_sym();
      if (sym == SEMI) next_sym(); else syntax_error();
    }
  else if (sym == GOTO_SYM) /*"goto" [ <id> ] ";" */
    {
      x = new_node(GT);
      next_sym();
      if (sym == ID) next_sym(); else syntax_error();
      if (sym == SEMI) next_sym(); else syntax_error();
    }
  else if (sym == PRINT_SYM) /*"print" <paren_expr> */
    {
      next_sym();
      x = new_node(PRT);
      x->o1 = paren_expr();
      if (sym == SEMI) next_sym(); else syntax_error();
    }
  else if (sym == ID)/* <expr> ";" */
    {
      x = test();
      if(sym == COLON){//:
        node *t = x;
        x = new_node(ETQUT);
        next_sym();
        x->o1 = t;
        x->o2 = statement();
      }else if (sym == EQUAL) {
        node *t = x;
        x = new_node(ASSIGN);
        next_sym();
        x->o1 = t;
        x->o2 = expr();
      }
    }
  else
  {
    x = new_node(EXPR);
    x->o1 = expr();
    if (sym == SEMI) next_sym(); else syntax_error();
  }
  return x;
}

node *program()  /* <program> ::= <stat> */
{
  initialisation_node_stack();
  node *x = new_node(PROG);
  next_sym();
  x->o1 = statement();
  if (sym != EOI) syntax_error();
  return x;
}

/* Generateur de code. */

enum { ILOAD, ISTORE, BIPUSH, DUP, POP, IADD, ISUB,
       GOTO, IFEQ, IFNE, IFLT, RETURN,
       IFLTOEV, IMUL, IDIV, IMOD, IPRINT, PTPUSH };//<=, *, /, %, print, :

typedef signed char code;

code object[1000], *here = object;
void gen(code c) {if(here>&object[1000]){overflow_error();} *here++ = c; }

#ifdef SHOW_CODE
#define g(c) do { printf(" %d",c); gen(c); } while (0)
#define gi(c) do { printf("\n%s", #c); gen(c); } while (0)
#else
#define g(c) gen(c)
#define gi(c) gen(c)
#endif

void fix(code *src, code *dst) { *src = dst-src; }

void c(node *x)
{ switch (x->kind)
    { case VAR   : gi(ILOAD); g(x->val); break;

      case CST   : gi(BIPUSH); g(x->val); break;

      case ADD   : c(x->o1); c(x->o2); gi(IADD); break;

      case SUB   : c(x->o1); c(x->o2); gi(ISUB); break;

      case MUL   : c(x->o1); c(x->o2); gi(IMUL); break;//*

      case DIV   : c(x->o1); c(x->o2); gi(IDIV); break;///

      case MOD   : c(x->o1); c(x->o2); gi(IMOD); break;//%

      case LT    : gi(BIPUSH); g(1);
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case MT    : gi(BIPUSH); g(1);//>
                   c(x->o2);
                   c(x->o1);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case EV    : gi(BIPUSH); g(1);//==
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFEQ); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case NEV   : gi(BIPUSH); g(1);//!=
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFNE); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case LTOEV : gi(BIPUSH); g(1);//<=
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFLTOEV); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case MTOEV : gi(BIPUSH); g(1);//>=
                   c(x->o2);
                   c(x->o1);
                   gi(ISUB);
                   gi(IFLTOEV); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case ASSIGN: c(x->o2);
                   gi(DUP);
                   gi(ISTORE); g(x->o1->val); break;

      case ETQUT : { //gi(PTPUSH);//TODO
                     //gi(ISTORE);
                     g(x->o1->val);
                     c(x->o2); break;
                   }//id: (ETIQUETTE)

      case IF1   : { code *p1;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2); fix(p1,here); break;
                   }

      case IF2   : { code *p1, *p2;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2);
                     gi(GOTO); p2 = here++; fix(p1,here);
                     c(x->o3); fix(p2,here); break;
                   }

      case WHILE : { code *p1 = here, *p2;
                     c(x->o1);
                     gi(IFEQ); p2 = here++;
                     c(x->o2);
                     gi(GOTO); fix(here++,p1); fix(p2,here); break;
                   }

      case DO    : { code *p1 = here; c(x->o1);
                     c(x->o2);
                     gi(IFNE); fix(here++,p1); break;
                   }

      case PRT    : {c(x->o1);
                    gi(IPRINT);break;
      }

      case EMPTY : break;

      case SEQ   : c(x->o1);
                   c(x->o2); break;

      case EXPR  : c(x->o1);
                   gi(POP); break;

      case PROG  : c(x->o1);
                   gi(RETURN); break;
    }
}


/* Machine virtuelle. */

int globals[26];

void run()
{
  int stack[1000], *sp = stack;
  code *pc = object;//taille 1000

  for (;;)

    switch (*pc++)
    {
      case ILOAD   : *sp++ = globals[*pc++];                 break;
      case ISTORE  : globals[*pc++] = *--sp;                 break;
      case BIPUSH  : *sp++ = *pc++;                          break;
      case DUP     : sp++; sp[-1] = sp[-2];                  break;
      case POP     : --sp;                                   break;
      case IADD    : sp[-2] = sp[-2] + sp[-1]; --sp;         break;
      case ISUB    : sp[-2] = sp[-2] - sp[-1]; --sp;         break;
      case IMUL    : sp[-2] = sp[-2] * sp[-1]; --sp;         break;//*
      case IDIV    : sp[-2] = sp[-2] / sp[-1]; --sp;         break;///
      case IMOD    : sp[-2] = sp[-2] % sp[-1]; --sp;         break;//%
      case GOTO    : pc += *pc;                              break;
      case IFEQ    : if (*--sp==0) pc += *pc; else pc++;     break;
      case IFNE    : if (*--sp!=0) pc += *pc; else pc++;     break;
      case IFLT    : if (*--sp< 0) pc += *pc; else pc++;     break;
      case IFLTOEV : if (*--sp<= 0) pc += *pc; else pc++;    break;//<=
      case IPRINT  : printf("%i\n",*--sp);                   break;//print
      case RETURN  : return;
    }
}

/* Programme principal. */

int main()
{
#ifdef SHOW_CODE
  printf("\n");
#endif
  int i;
  node* nodes_program;

  if(setjmp(fin)){// NOUVEAU
    fin_programme();
    return 0;
  }else{
    nodes_program = program();

    c(nodes_program);

    for (i=0; i<26; i++)
      globals[i] = 0;

    run();
  }

  free_nodes(nodes_program);

  //for (i=0; i<26; i++)
  //  if (globals[i] != 0)
  //    printf("%c = %d\n", 'a'+i, globals[i]);//nous retirons cette ligne pour arreter d'imprimer les variables non nulles a la fin de l'execution
  return 0;
}
