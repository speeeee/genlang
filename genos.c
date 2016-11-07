#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#define SYMBOL_TABLE_SIZE 10

#define END_EXPR 0
#define SCOPE    1
#define CALL     2
#define LIN_CALL 3
#define OP_CALL  4
#define WORD     5
#define DWORD    6

#define PRINT_INT 7

#define ADDI_OPT  8
#define SUBI_OPT  9
#define DIVI_OPT  10
#define MULI_OPT  11
#define ADDF_OPT  12
#define SUBF_OPT  13
#define MULF_OPT  14
#define DIVF_OPT  15

#define ADDI 16
#define SUBI 17
#define MULI 18
#define DIVI 19
#define ADDF 20
#define SUBF 21
#define MULF 22
#define DIVF 23

#define END_EXPR_RETURN 24
#define END_GEN         25

#define GENERATOR 26
#define LAZY_GENERATOR 27
#define REST 28 //

// define function and push to symbol table.
#define DFUN 29
// call function
#define CFUN 30 //
#define EFUN 32
#define X    33

// looping with generators (loops until type `fail' is returned).
#define LOOP 34 //

#define COPY_GEN 35
#define HEAD     36
#define ENQUEUE  37

#define OR       38
#define AND      39
#define NOTHING  40

#define EVERY 41

#define GET 42
#define POP 43

#define WORD_T   0
#define DWORD_T  1
#define GEN_T    2
#define LGEN_T   3
#define SCP_T    4
#define ENDG_T   5
#define FUN_T    6
#define FAIL     7
#define SUCCESS  8

#define OPERATOR_OPT(EXPR, TYPE, RTYPE) \
  if(get_elem(0).item.type==FAIL||get_elem(1).item.type==FAIL) { \
    symbol_table_pop(); symbol_table_pop(); symbol_table_push_fail(); } \
  else { TYPE x = *(TYPE *)get_elem(1).item.dat; TYPE y = *(TYPE *)get_elem(0).item.dat; \
    symbol_table_pop(); symbol_table_pop(); symbol_table_push(symbol("A",1,c_##TYPE(EXPR),RTYPE)); } \
  d++; break;
#define OPERATOR(EXPR, TYPE, RTYPE) \
  TYPE x = *(TYPE *)get_elem(1).item.dat; TYPE y = *(TYPE *)get_elem(0).item.dat; \
  symbol_table_pop(); symbol_table_pop(); symbol_table_push(symbol("A",1,c_##TYPE(EXPR),RTYPE)); break;

#define SYM_FUN(TYPE) \
  void *c_##TYPE(TYPE a) { TYPE *n = malloc(sizeof(TYPE)); \
    *n = a; return (void *)n; }
SYM_FUN(int32_t)
SYM_FUN(int64_t)
SYM_FUN(int8_t)
SYM_FUN(double)

#define SZ_ARR(ARR) (sizeof(ARR)/sizeof(ARR[0]))

// DONE: add generators to general evaluation.
// DONE: make HEAD and REST opcodes for generators.
// TODO: make CFUN for anonymous functions.
// TODO: add for generators EVERY, OR, etc.  see `notes2.txt'.

// DONEi define OR. untested
// TODO: up next is testing recursion.

// TODO: define X to be argument of function (first item before SCOPE).

// HOLD: reverse all generators to make consing O(1).
// TODO: a lot of `symbol_table_drop's should be `symbol_table_pop's to stop memory leaks.

// warning: returns pointer.
typedef char *Data;
// Data, Generator, GeneratorFunc
typedef struct { int type; void *dat; } Item;
Item item(int type, void *dat) { return (Item) { type, dat }; }
Data byte_string(int sz, ...) { va_list vl; va_start(vl,sz); Data a = malloc(sz*sizeof(char));
  for(int i=0;i<sz;i++) { a[i] = (char)va_arg(vl,int); } va_end(vl); return a; }
// DONE: switch `data' to type Item.
typedef struct { char *name; int orig; Item item; } Symbol;
// TODO?: use strdup for name so making non-det names later on isn't problematic.
Symbol symbol_d(char *name, Data data, int type) { return (Symbol) { name, 1, item(type,(void *)data) }; }
Symbol symbol(char *name, int orig, void *data, int type) {
  return (Symbol) { name, orig, item(type,data) }; }
Symbol symbol_i(char *name, int orig, Item data) { return (Symbol) { name, orig, data }; }
// rewrite `item_free' for all types.
// generators are special in that `lst' is only freed if the `orig' flag is true.
int item_free(Item a) { free(a.dat); return 0; }
int symbol_free(Symbol a) { if(a.orig) { item_free(a.item); } return 0; }
typedef struct { int32_t iter; int32_t orig; int32_t sz; Item *lst; } Generator;
Generator generator(int32_t iter, int32_t orig, int32_t sz, Item *lst) {
  return (Generator) { iter, orig, sz, lst }; }
// return 0 if succeeds; return -1 if generator is null.
int next_gen(Generator *a) { if(a->iter>=a->sz) { return -1; } a->iter++; return 0; }

typedef struct { Symbol *st; int scope; int sz; } SymbolTable;
SymbolTable symbol_table;
int get_sz(void) { return symbol_table.sz; }
Symbol get_elem(int i) { return symbol_table.st[symbol_table.sz-1-i]; }

// initialize symbol_table with global scope.
void symbol_table_init(void) { symbol_table.st = malloc(SYMBOL_TABLE_SIZE*sizeof(Symbol));
  symbol_table.scope = 1;
  symbol_table.st[symbol_table.sz = 0] = (symbol_d("SCOPE",byte_string(1,1),SCP_T));
  symbol_table.sz++; }
void symbol_table_push(Symbol ns) {
  if(symbol_table.sz>=sizeof(symbol_table.st)) {
    symbol_table.st = realloc(symbol_table.st,2*sizeof(symbol_table.st)); }
  symbol_table.st[symbol_table.sz++] = ns; }
void symbol_table_pop(void) { symbol_free(symbol_table.st[(symbol_table.sz--)-1]); }
void symbol_table_drop(void) { symbol_table.sz--; }
void symbol_table_push_fail(void) { symbol_table_push(symbol_d("FAILURE",NULL,FAIL)); }
void symbol_table_push_success(void) { symbol_table_push(symbol_d("SUCCESS",NULL,SUCCESS)); }

Data parse(Data);
void parse_loop(Data d) { while((d = parse(d))); }

void print_int(Data *d) { if(get_elem(0).item.type==FAIL) {
    symbol_table_pop(); symbol_table_push_fail(); }
  else { printf("%i",*(int *)get_elem(0).item.dat);
    symbol_table_pop(); } (*d)++; }

// change call and copy_gen to take int64_t?
Data parse(Data d) { switch(d[0]) {
  case END_EXPR: return NULL;
  case SCOPE: symbol_table_push(symbol_d("SCOPE",NULL,SCP_T)); d++;
    /* DONE: parse_loop(d); -- may be better to traverse scope recursively;
                           'd' must be address for this; look for other solutions first. */
    /*int isz = 0; int *iters = malloc(sizeof(int));
    // TODO: actually finish this.
    for(int i=0;i<get_sz();i++) {
      if(get_elem(get_sz()-i-1).item.type==GEN_T) {
        iters = realloc(iters,++isz*sizeof(int)); } }*/
    Data nd; while((nd = parse(d))) { if(nd) { d = nd; } } d+=2; break;
  case END_EXPR_RETURN: { Symbol x; int i;
    for(i=get_sz()-2;(x=get_elem(i)).item.type!=SCP_T&&!symbol_free(x);i--);
    symbol_free(x); symbol_table.st[i] = symbol_table.st[get_sz()-1];
    symbol_table.sz = i+1; d++; return NULL; }
  //case END_GEN: symbol_table_push(symbol_d("END_GEN",NULL,ENDG_T)); break;
  case GENERATOR: { int32_t gsz; memcpy(&gsz,++d,sizeof(int32_t)); d+=sizeof(int32_t);
    Item *lst = malloc(gsz*sizeof(Item));
    for(int i=0;i<gsz;i++) { d = parse(d); lst[i] = get_elem(0).item; symbol_table_drop(); }
    Generator *gen = malloc(sizeof(Generator)); *gen = generator(0,1,gsz,lst);
    symbol_table_push(symbol("GEN",1,(void *)gen,GEN_T)); break; }
  case COPY_GEN: { int ind; memcpy(&ind,++d,sizeof(int32_t)); d+=sizeof(int32_t);
    Generator *e = malloc(sizeof(Generator)); 
    Generator q = *(Generator *)get_elem(get_sz()-1-ind).item.dat;
    *e = generator(q.iter,0,q.sz,q.lst); symbol_table_push(symbol("CGEN",0,(void *)e,GEN_T)); break; }
  case HEAD: { Generator *a = (Generator *)get_elem(0).item.dat;
    if(a->iter>=a->sz) { symbol_table_pop(); symbol_table_push_fail(); }
    else { symbol_table_pop();
      symbol_table_push(symbol_i("FROM_GEN",1,a->lst[a->iter++])); } d++; break; }
  case ENQUEUE: {
    if(get_elem(0).item.type==FAIL||get_elem(1).item.type==FAIL) {
      symbol_table_pop(); symbol_table_pop(); symbol_table_push_fail(); }
    else { Generator *a = (Generator *)get_elem(1).item.dat;
      Item b = get_elem(0).item;
      // TODO: make sure this works.
      if(++a->sz>SZ_ARR(a->lst)) { a->lst = realloc(a->lst,(a->sz)*sizeof(Item)); }
      a->lst[a->sz-1] = b; symbol_table_drop(); symbol_table_drop(); } d++; break; }
  case WORD: { Data nd = malloc(sizeof(int32_t)); memcpy(nd,++d,sizeof(int32_t));
               symbol_table_push(symbol_d("TEMP",nd,WORD_T)); d+=sizeof(int32_t); break; }
  case DWORD: { Data nd = malloc(sizeof(int64_t)); memcpy(nd,++d,sizeof(int64_t));
                symbol_table_push(symbol_d("TEMP",nd,DWORD_T)); d+=sizeof(int64_t)+1; break; }
  case PRINT_INT: /*printf("%i",*(int *)get_elem(0).item.dat); d++;
    symbol_table_pop();*/ print_int(&d); symbol_table_push_success(); break;
  case OR: { // succeeds if up to one of its operands fails.
    int ta = get_elem(0).item.type; int tb = get_elem(1).item.type;
    if(ta==FAIL&&tb==FAIL) { symbol_table_pop(); symbol_table_pop(); symbol_table_push_fail(); }
    else { symbol_table_pop(); } break; }
  case AND: { int ta = get_elem(0).item.type; int tb = get_elem(1).item.type;
    if(ta==FAIL||tb==FAIL) { symbol_table_pop(); symbol_table_pop(); symbol_table_push_fail(); }
    else { symbol_table_pop(); } break; }
  case NOTHING: symbol_table_push_fail(); break;
  case ADDI: { OPERATOR_OPT(x+y,int32_t,WORD_T) }
  case REST: { int a = next_gen((Generator *)get_elem(0).item.dat);
    if(a) { symbol_table_pop(); symbol_table_push_fail(); } d++; break; }
  case DFUN: { /*int q, i; // very big error here: just reads the bytes.
                         // quick solution is to make emergency byte-length as an argument
                         // for function definition.
    for(q=0,i=0;d[q+1]!=EFUN||i--;q++) { d[q+1]==DFUN?i++:i; }
    Data nd; nd = malloc((q+1)*sizeof(int8_t));
    nd = memcpy(nd,++d,q); symbol_table_push(symbol("FUN",nd,FUN_T));
    d+=q+1; break; }*/
    int sz; memcpy(&sz,++d,sizeof(int32_t)); d+=sizeof(int32_t);
    Data nd = malloc(sz*sizeof(char)); memcpy(nd,d,sz*sizeof(int8_t)); d+=sz;
    symbol_table_push(symbol("FUN",1,nd,FUN_T));
    break; }
  //case EVERY: 
  case GET: { int ind; memcpy(&ind,++d,sizeof(int32_t)); d+=sizeof(int32_t);
    Item n = get_elem(get_sz()-1-ind).item; symbol_table_push(symbol_i("COPIED",0,n)); break; }
  case POP: symbol_table_pop(); d++; break;
  case CALL: { int ind; memcpy(&ind,++d,sizeof(int32_t)); d+=sizeof(int32_t);
    //int ind = *(int32_t *)get_elem(0).item.dat;
    Data q = (Data)get_elem(get_sz()-1-ind).item.dat;
    while((q = parse(q))); break; } } return d; }

// DONE: when referencing a generator in a new scope, it will be given a new iterator for that
//     : scope.  a simple copy opcode might work.
int main(int argc, char **argv) { symbol_table_init();
  // test 0
  //Data b = byte_string(7,WORD,4,0,0,0,PRINT_INT,END_EXPR);
  // test 1
  //Data b = byte_string(11,GENERATOR,1,0,0,0,WORD,4,0,0,0,END_EXPR);
  // test 2
  //Data b = byte_string(13,WORD,4,0,0,0,WORD,5,0,0,0,ADDI,PRINT_INT,END_EXPR);
  // test 3
  //Data b = byte_string(15,DFUN,PRINT_INT,END_EXPR,EFUN,WORD,4,0,0,0,CALL,1,0,0,0,END_EXPR);
  // test 4
  // TODO: OR test.
  /*
  Data b = byte_string(48    ,GENERATOR,2,0,0,0,WORD,2,0,0,0,WORD,3,0,0,0
                             ,DFUN,23,0,0,0,COPY_GEN,1,0,0,0
                               ,DFUN,8,0,0,0,HEAD,PRINT_INT,CALL,4,0,0,0,AND
                               ,CALL,4,0,0,0
                             ,CALL,2,0,0,0,END_EXPR);
  */
  // test 5
  Data b = byte_string(29,GENERATOR,1,0,0,0,WORD,2,0,0,0,GET,1,0,0,0,WORD,1,0,0,0,ENQUEUE
                         ,GET,1,0,0,0,HEAD,PRINT_INT,END_EXPR);//GET,1,0,0,0,HEAD,PRINT_INT,END_EXPR);
  while((b = parse(b)));
  return 0; }
