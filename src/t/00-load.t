#!perl -T

use strict;
use warnings;

use Test::More tests => 1;

BEGIN {
	use_ok( 'Template::Sandbox' );
}

diag( "Testing Template::Sandbox $Template::Sandbox::VERSION, Perl $], $^X" );
