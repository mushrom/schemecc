#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

enum {
	SCM_TYPE_INTEGER    = 0x00,
	SCM_TYPE_BOOLEAN    = 0x1f,
	SCM_TYPE_EMPTY_LIST = 0x2f,
	SCM_TYPE_PAIR       = 0x01,
	SCM_TYPE_CLOSURE    = 0x06,

	SCM_MASK_INTEGER = 0x3,
	SCM_MASK_BOOLEAN = 0x7f,
	SCM_MASK_HEAP    = 0x7,
};

extern int scheme_thing( void );
void *scheme_heap;

void print_scheme_obj( uint64_t val ){
	if (( val & SCM_MASK_INTEGER ) == SCM_TYPE_INTEGER ){
		printf( "%d", val >> 2 );

	} else if ( val == SCM_TYPE_EMPTY_LIST ){
		printf( "()" );

	} else if (( val & SCM_MASK_BOOLEAN ) == SCM_TYPE_BOOLEAN ){
		printf( "#%c", (val >> 7)? 't' : 'f' );

	} else if (( val & SCM_MASK_HEAP ) == SCM_TYPE_PAIR ){
		uint64_t *thing = (uint64_t *)( val & ~SCM_MASK_HEAP );
		printf( "(" );
		print_scheme_obj( *thing );
		printf( " . " );
		print_scheme_obj( *(thing + 1));
		printf( ")" );

	} else if (( val & SCM_MASK_HEAP ) == SCM_TYPE_CLOSURE ){
		printf( "#<closure @ %p>", (void *)( val & ~SCM_MASK_HEAP ));
	}
}

int main( int argc, char *argv[] ){
	scheme_heap = malloc( 4096 * 32 );
	uint64_t val = scheme_thing( );

	printf( "debug: val = 0x%x (%d)\n", val, val );
	print_scheme_obj( val );
	printf( "\n" );

	return 0;
}
