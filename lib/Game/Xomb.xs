#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

typedef SV * Game_Xomb;

MODULE = Game::Xomb		PACKAGE = Game::Xomb		

Game_Xomb
new(...)
    INIT:
    	char *classname;
	/* get the class name if called as an object method */
	if ( sv_isobject(ST(0)) ) {
	    classname = HvNAME(SvSTASH(SvRV(ST(0))));
	}
	else {
	    classname = (char *)SvPV_nolen(ST(0));
	}

    CODE:
    	/* This is a standard hash-based object */
    	RETVAL = (Game_Xomb)newHV();

	/* Single init value */
	if ( items == 2 ) 
	    hv_store((HV *)RETVAL, "value", 5, newSVsv(ST(1)), 0);
	/* name/value pairs */
	else if ( (items-1)%2 == 0 ) {
	    int i;
	    for ( i=1; i < items; i += 2 ) {
		hv_store_ent((HV *)RETVAL, ST(i), newSVsv(ST(i+1)), 0);
	    }
	}
	/* odd number of parameters */
	else {
	    Perl_croak(aTHX_
		"Usage: Game::Xomb->new()\n"
		"    or Game::Xomb->new(number)\n"
		"    or Game::Xomb->new(key => value, ...)\n"
	    );
	}

    OUTPUT:
    	RETVAL

IV
increment(obj)
    Game_Xomb obj

    INIT:
    	RETVAL = 0;
	if ( items > 1 )
	    Perl_croak(aTHX_ "Usage: Game::Xomb->increment()");

    CODE:
    	SV **svp;
	if ((svp = hv_fetch((HV*)obj, "value", 5, FALSE))) {
	    RETVAL = SvIV(*svp);
	    RETVAL++;
	    hv_store((HV *)obj, "value", 5, newSViv(RETVAL), 0);
	}
    OUTPUT:
    	RETVAL
