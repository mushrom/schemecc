#include <stdio.h>

enum {
	SCM_TYPE_INTEGER = 0x00,
	SCM_MASK_INTEGER = 0x2,
	SCM_TYPE_EMPTY_LIST = 0x2f,
};

extern int scheme_thing( void );

int main( int argc, char *argv[] ){
	int val = scheme_thing( );

	printf( "debug: val = 0x%x (%d)\n", val, val );

	if (( val & SCM_MASK_INTEGER ) == SCM_TYPE_INTEGER ){
		printf( "%d\n", val >> SCM_MASK_INTEGER );

	} else if ( val == SCM_TYPE_EMPTY_LIST ){
		printf( "()\n" );
	}

	return 0;
}
