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

#define ADDI  8
#define SUBI  9
#define DIVI  10
#define MULI  11
#define ADDF  12
#define SUBF  13
#define MULF  14
#define DIVF  15

#define END_EXPR_RETURN 16

#define GENERATOR 17
#define LAZY_GENERATOR 18

#define WORD_T   0
#define DWORD_T  1
#define GEN_T    2
#define LGEN_T   3
#define ANY      4

#define OPERATOR(EXPR, TYPE) \
  TYPE x = *(TYPE *)symbol_table.st[sz-2].data; TYPE y = *(TYPE *)symbol_table.st[sz-1].data; \
  symbol_table_pop(); symbol_table_pop(); symbol_table_push(symbol("A",c_##TYPE(EXPR))); break;

// warning: returns pointer.
typedef char *Data;
// Data, Generator, GeneratorFunc
typedef struct { int type; void *dat; } Item;
Item item(int type, void *dat) { return (Item) { type, dat }; }
Data byte_string(int sz, ...) { va_list vl; va_start(vl,sz); Data a = malloc(sz*sizeof(char));
  for(int i=0;i<sz;i++) { a[i] = (char)va_arg(vl,int); } va_end(vl); return a; }
// TODO: switch `data' to type Item.
typedef struct { char *name; Item item; } Symbol;
Symbol symbol_d(char *name, Data data, int type) { return (Symbol) { name, item(type,(void *)data) }; }
void item_free(Item a) { free(a.dat); }
void symbol_free(Symbol a) { item_free(a.item); }
typedef struct { int32_t iter; int32_t sz; Item *lst; } Generator;

typedef struct { Symbol *st; int scope; int sz; } SymbolTable;
SymbolTable symbol_table;
Symbol get_elem(int i) { return symbol_table.st[symbol_table.sz-1-i]; }

// initialize symbol_table with global scope.
void symbol_table_init(void) { symbol_table.st = malloc(SYMBOL_TABLE_SIZE*sizeof(Symbol));
  symbol_table.scope = 1;
  symbol_table.st[symbol_table.sz = 0] = (symbol_d("SCOPE",byte_string(1,1),ANY));
  symbol_table.sz++; }
void symbol_table_push(Symbol ns) {
  if(symbol_table.sz>=sizeof(symbol_table.st)) {
    symbol_table.st = realloc(symbol_table.st,2*sizeof(symbol_table.st)); }
  symbol_table.st[symbol_table.sz++] = ns; }
void symbol_table_pop(void) { symbol_free(symbol_table.st[(symbol_table.sz--)-1]); }

Data parse(Data d) { switch(d[0]) {
  case END_EXPR: return NULL;
  case SCOPE: return NULL;
  case WORD: { Data nd = malloc(sizeof(int32_t)); memcpy(nd,++d,sizeof(int32_t));
               symbol_table_push(symbol_d("TEMP",nd,WORD_T)); d+=sizeof(int32_t); break; }
  case DWORD: { Data nd = malloc(sizeof(int64_t)); memcpy(nd,d,sizeof(int64_t));
                symbol_table_push(symbol_d("TEMP",nd,DWORD_T)); d+=sizeof(int64_t)+1; break; }
  case PRINT_INT: printf("%i",*(int *)get_elem(0).item.dat); d++;
    symbol_table_pop(); break; } return d; }

int main(int argc, char **argv) { symbol_table_init();
  // test 0
  Data b = byte_string(7,WORD,4,0,0,0,PRINT_INT,END_EXPR);
  while((b = parse(b)));
  return 0; }
