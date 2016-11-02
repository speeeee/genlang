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
#define REST 28

// define function and push to symbol table.
#define DFUN 29
// call function
#define CFUN 30
#define EFUN 32
#define X    33

// looping with generators (loops until type `fail' is returned).
#define LOOP 34

#define WORD_T   0
#define DWORD_T  1
#define GEN_T    2
#define LGEN_T   3
#define SCP_T    4
#define ENDG_T   5
#define FUN_T    6
#define FAIL     7

#define OPERATOR_OPT(EXPR, TYPE, RTYPE) \
  if(get_elem(0).item.type==FAIL||get_elem(1).item.type==FAIL) { \
    symbol_table_pop(); symbol_table_pop(); symbol_table_push_fail(); } \
  else { TYPE x = *(TYPE *)get_elem(1).item.dat; TYPE y = *(TYPE *)get_elem(0).item.dat; \
    symbol_table_pop(); symbol_table_pop(); symbol_table_push(symbol("A",c_##TYPE(EXPR),RTYPE)); } \
  d++; break;
#define OPERATOR(EXPR, TYPE, RTYPE) \
  TYPE x = *(TYPE *)get_elem(1).item.dat; TYPE y = *(TYPE *)get_elem(0).item.dat; \
  symbol_table_pop(); symbol_table_pop(); symbol_table_push(symbol("A",c_##TYPE(EXPR),RTYPE)); break;

#define SYM_FUN(TYPE) \
  void *c_##TYPE(TYPE a) { TYPE *n = malloc(sizeof(TYPE)); \
    *n = a; return (void *)n; }
SYM_FUN(int32_t)
SYM_FUN(int64_t)
SYM_FUN(int8_t)
SYM_FUN(double)

// TODO: add generators to general evaluation.
// TODO: make HEAD and REST opcodes for generators.
// TODO: make CFUN for anonymous functions.
// TODO: add for generators EVERY, OR, etc.

// warning: returns pointer.
typedef char *Data;
// Data, Generator, GeneratorFunc
typedef struct { int type; void *dat; } Item;
Item item(int type, void *dat) { return (Item) { type, dat }; }
Data byte_string(int sz, ...) { va_list vl; va_start(vl,sz); Data a = malloc(sz*sizeof(char));
  for(int i=0;i<sz;i++) { a[i] = (char)va_arg(vl,int); } va_end(vl); return a; }
// TODO: switch `data' to type Item.
typedef struct { char *name; Item item; } Symbol;
// TODO?: use strdup for name so making non-det names later on isn't problematic.
Symbol symbol_d(char *name, Data data, int type) { return (Symbol) { name, item(type,(void *)data) }; }
Symbol symbol(char *name, void *data, int type) { return (Symbol) { name, item(type,data) }; }
int item_free(Item a) { free(a.dat); return 0; }
int symbol_free(Symbol a) { item_free(a.item); return 0; }
typedef struct { int32_t iter; int32_t sz; Item *lst; } Generator;
Generator generator(int32_t iter, int32_t sz, Item *lst) { return (Generator) { iter, sz, lst }; }
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

Data parse(Data);
void parse_loop(Data d) { while((d = parse(d))); }

void print_int(Data *d) { if(get_elem(0).item.type==FAIL) {
    symbol_table_pop(); symbol_table_push_fail(); }
  else { printf("%i",*(int *)get_elem(0).item.dat);
    symbol_table_pop(); } (*d)++; }

Data parse(Data d) { switch(d[0]) {
  case END_EXPR: return NULL;
  case SCOPE: symbol_table_push(symbol_d("SCOPE",NULL,SCP_T)); d++;
    /* DONE: parse_loop(d); -- may be better to traverse scope recursively;
                           'd' must be address for this; look for other solutions first. */
    Data nd; while((nd = parse(d))) { if(nd) { d = nd; } } d+=2; break;
  case END_EXPR_RETURN: { Symbol x; int i;
    for(i=get_sz()-2;(x=get_elem(i)).item.type!=SCP_T&&!symbol_free(x);i--);
    symbol_free(x); symbol_table.st[i] = symbol_table.st[get_sz()-1];
    symbol_table.sz = i+1; d++; return NULL; }
  //case END_GEN: symbol_table_push(symbol_d("END_GEN",NULL,ENDG_T)); break;
  case GENERATOR: { int32_t gsz; memcpy(&gsz,++d,sizeof(int32_t)); d+=sizeof(int32_t);
    Item *lst = malloc(gsz*sizeof(Item));
    for(int i=0;i<gsz;i++) { d = parse(d); lst[i] = get_elem(0).item; symbol_table_drop(); }
    Generator *gen = malloc(sizeof(Generator)); *gen = generator(0,gsz,lst);
    symbol_table_push(symbol("GEN",(void *)gen,GEN_T)); break; }
  case WORD: { Data nd = malloc(sizeof(int32_t)); memcpy(nd,++d,sizeof(int32_t));
               symbol_table_push(symbol_d("TEMP",nd,WORD_T)); d+=sizeof(int32_t); break; }
  case DWORD: { Data nd = malloc(sizeof(int64_t)); memcpy(nd,++d,sizeof(int64_t));
                symbol_table_push(symbol_d("TEMP",nd,DWORD_T)); d+=sizeof(int64_t)+1; break; }
  case PRINT_INT: /*printf("%i",*(int *)get_elem(0).item.dat); d++;
    symbol_table_pop();*/ print_int(&d); break;
  case ADDI: { OPERATOR_OPT(x+y,int32_t,WORD_T) }
  case REST: { int a = next_gen((Generator *)get_elem(0).item.dat);
    if(a) { symbol_table_pop(); symbol_table_push_fail(); } d++; break; }
  case DFUN: { int q; for(q=0;d[q+1]!=EFUN;q++); Data nd; nd = malloc((q+1)*sizeof(int8_t));
    nd = memcpy(nd,++d,q); symbol_table_push(symbol("FUN",nd,FUN_T));
    d+=q+1; break; }
  case CALL: { int ind; memcpy(&ind,++d,sizeof(int32_t)); d+=sizeof(int32_t);
    //int ind = *(int32_t *)get_elem(0).item.dat;
    Data q = (Data)get_elem(get_sz()-1-ind).item.dat;
    while((q = parse(q))); break; } } return d; }

int main(int argc, char **argv) { symbol_table_init();
  // test 0
  //Data b = byte_string(7,WORD,4,0,0,0,PRINT_INT,END_EXPR);
  // test 1
  //Data b = byte_string(11,GENERATOR,1,0,0,0,WORD,4,0,0,0,END_EXPR);
  // test 2
  //Data b = byte_string(13,WORD,4,0,0,0,WORD,5,0,0,0,ADDI,PRINT_INT,END_EXPR);
  // test 3
  Data b = byte_string(15,DFUN,PRINT_INT,END_EXPR,EFUN,WORD,4,0,0,0,CALL,1,0,0,0,END_EXPR);
  while((b = parse(b)));
  return 0; }
