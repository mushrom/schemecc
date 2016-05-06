#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>

enum {
	SCM_TYPE_INTEGER    = 0x00,
	SCM_TYPE_BOOLEAN    = 0x1f,
	SCM_TYPE_EMPTY_LIST = 0x2f,
	SCM_TYPE_CHAR       = 0x0f,
	SCM_TYPE_PAIR       = 0x01,
	SCM_TYPE_CLOSURE    = 0x06,
	SCM_TYPE_VECTOR     = 0x02,
	SCM_TYPE_STRING     = 0x03,

	SCM_MASK_INTEGER = 0x3,
	SCM_MASK_BOOLEAN = 0x7f,
	SCM_MASK_CHAR    = 0xff,
	SCM_MASK_HEAP    = 0x7,
};

typedef int64_t scm_data;

extern int scheme_thing( void );
void *scheme_heap;

static inline scm_data unshift_fixnum( scm_data foo ){
	return foo >> 2;
}

static inline scm_data unshift_char( scm_data character ){
	return character >> 8;
}

static inline scm_data shift_fixnum( scm_data foo ){
	return foo << 2;
}

scm_data s_write_char( scm_data character ){
	putchar( unshift_char( character ));

	return shift_fixnum( 1 );
}

void print_scheme_obj( scm_data val ){
	if (( val & SCM_MASK_INTEGER ) == SCM_TYPE_INTEGER ){
		printf( "%d", val >> 2 );

	} else if ( val == SCM_TYPE_EMPTY_LIST ){
		printf( "()" );

	} else if (( val & SCM_MASK_BOOLEAN ) == SCM_TYPE_BOOLEAN ){
		printf( "#%c", (val >> 7)? 't' : 'f' );

	} else if (( val & SCM_MASK_CHAR ) == SCM_TYPE_CHAR ){
		printf( "#\\%c", val >> 8 );

	} else if (( val & SCM_MASK_HEAP ) == SCM_TYPE_PAIR ){
		uint64_t *thing = (scm_data *)( val & ~SCM_MASK_HEAP );
		printf( "(" );
		print_scheme_obj( *thing );
		printf( " . " );
		print_scheme_obj( *(thing + 1));
		printf( ")" );

	} else if (( val & SCM_MASK_HEAP ) == SCM_TYPE_CLOSURE ){
		printf( "#<closure @ %p>", (void *)( val & ~SCM_MASK_HEAP ));

	} else if (( val & SCM_MASK_HEAP ) == SCM_TYPE_VECTOR ){
		scm_data *vecptr = (void *)( val & ~SCM_MASK_HEAP );
		unsigned veclen = unshift_fixnum( *vecptr );
		unsigned i = 0;

		//printf( "#<vector @ %p, len = %u>", vecptr, unshift_fixnum( *vecptr ));

		printf( "#(" );

		for ( i = 0; i < veclen; i++ ){
			print_scheme_obj( *(vecptr + i + 1));

			if ( i + 1 < veclen ){
				putchar( ' ' );
			}
		}

		putchar( ')' );

	} else if (( val & SCM_MASK_HEAP ) == SCM_TYPE_STRING ){
		scm_data *dataptr = (void *)( val & ~SCM_MASK_HEAP );
		char *strptr = (char *)dataptr;

		printf( "\"%s\"", strptr + 8 );
	}
}

int main( int argc, char *argv[] ){
	scheme_heap = sbrk( 0x1000 * 32 );
	memset( scheme_heap, 0, 0x1000 * 32 );

	uint64_t val = scheme_thing( );

	//printf( "debug: val = 0x%x (%d)\n", val, val );
	print_scheme_obj( val );
	printf( "\n" );

	return 0;
}
