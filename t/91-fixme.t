#!/usr/bin/perl

use strict;
use warnings;

my @MODULES = (
	'Test::Fixme 0.04',
);

# Don't run tests during end-user installs
use Test::More;
plan( skip_all => 'Author tests not required for installation' )
	unless ( $ENV{RELEASE_TESTING} or $ENV{AUTOMATED_TESTING} );

# Load the testing modules
foreach my $MODULE ( @MODULES ) {
	eval "use $MODULE";
	if ( $@ ) {
		$ENV{RELEASE_TESTING}
		? die( "Failed to load required release-testing module $MODULE" )
		: plan( skip_all => "$MODULE not available for testing" );
	}
}

open my $fh, '<', 'MANIFEST' or
    plan( skip_all => 'Unable to read MANIFEST file' );
my @manifest = <$fh>;
my $manifest_regexp = '(?:' . join( '|', map { chomp; "\Q$_\E" } @manifest ) . ')';
run_tests( filename_match => qr/^(?:\.\/)?$manifest_regexp$/ );

1;
