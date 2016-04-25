#include <stdio.h>

enum {
	SCM_TYPE_INTEGER = 0x00,
	SCM_TYPE_BOOLEAN = 0x1f,
	SCM_TYPE_EMPTY_LIST = 0x2f,

	SCM_MASK_INTEGER = 0x2,
	SCM_MASK_BOOLEAN = 0x7f,
};

extern int scheme_thing( void );

int main( int argc, char *argv[] ){
	int val = scheme_thing( );

	printf( "debug: val = 0x%x (%d)\n", val, val );

	if (( val & SCM_MASK_INTEGER ) == SCM_TYPE_INTEGER ){
		printf( "%d\n", val >> SCM_MASK_INTEGER );

	} else if ( val == SCM_TYPE_EMPTY_LIST ){
		printf( "()\n" );

	} else if (( val & SCM_MASK_BOOLEAN ) == SCM_TYPE_BOOLEAN ){
		printf( "#%c\n", (val >> 7)? 't' : 'f' );
	}

	return 0;
}
