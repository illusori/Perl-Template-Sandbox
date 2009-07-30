#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 13;

my ( $template, $syntax );

#
#  1: top-level numeric var added after compile.
$syntax = "<: expr a :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
$template->add_var( a => 42 );
is( ${$template->run()}, '42', 'top-level numeric var added after compile' );

#
#  2: top-level numeric var added before compile.
$syntax = "<: expr a :>";
$template = Template::Sandbox->new();
$template->add_var( a => 42 );
$template->set_template_string( $syntax );
is( ${$template->run()}, '42', 'top-level numeric var added before compile' );

#
#  3: top-level string var added after compile.
$syntax = "<: expr str :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
$template->add_var( str => 'a string' );
is( ${$template->run()}, 'a string', 'top-level string var added after compile' );

#
#  4: top-level string var added before compile.
$syntax = "<: expr str :>";
$template = Template::Sandbox->new();
$template->add_var( str => 'a string' );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'a string', 'top-level string var added before compile' );

#
#  5: second-level numeric var added after compile.
$syntax = "<: expr a.b :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
$template->add_var( a => { b => 42 } );
is( ${$template->run()}, '42', 'second-level numeric var added after compile' );

#
#  6: second-level numeric var added before compile.
$syntax = "<: expr a.b :>";
$template = Template::Sandbox->new();
$template->add_var( a => { b => 42 } );
$template->set_template_string( $syntax );
is( ${$template->run()}, '42', 'second-level numeric var added before compile' );

#
#  7: second-level string var added after compile.
$syntax = "<: expr str.sub :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
$template->add_var( str => { sub => 'a string' } );
is( ${$template->run()}, 'a string', 'second-level string var added after compile' );

#
#  8: second-level string var added before compile.
$syntax = "<: expr str.sub :>";
$template = Template::Sandbox->new();
$template->add_var( str => { sub => 'a string' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'a string', 'second-level string var added before compile' );

#
#  9: numeric array index 0.
$syntax = "<: expr arr[ 0 ] :>";
$template = Template::Sandbox->new();
$template->add_var( arr => [ 42, 43, 44, 45 ] );
$template->set_template_string( $syntax );
is( ${$template->run()}, '42', 'numeric array index 0: arr[ 0 ]' );

#
#  10: numeric array index 2.
$syntax = "<: expr arr[ 2 ] :>";
$template = Template::Sandbox->new();
$template->add_var( arr => [ 42, 43, 44, 45 ] );
$template->set_template_string( $syntax );
is( ${$template->run()}, '44', 'numeric array index 2: arr[ 2 ]' );

#
#  11: bracket hash index 'alpha'.
$syntax = "<: expr hash[ 'alpha' ] :>";
$template = Template::Sandbox->new();
$template->add_var( hash => { 'alpha' => 'a', 'beta' => 'b' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'a', "bracket hash index 'alpha': hash[ 'alpha' ]" );

#
#  12: dotted hash index 'alpha'.
#  We've tested this already in effect, but here for completeness.
$syntax = "<: expr hash.alpha :>";
$template = Template::Sandbox->new();
$template->add_var( hash => { 'alpha' => 'a', 'beta' => 'b' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'a', "dotted hash index 'alpha': hash.alpha" );

#
#  13: mix it all together
$syntax = "<: expr str.sub :> <: expr numbers[ 3 ] :> <: expr a :>";
$template = Template::Sandbox->new();
$template->add_vars(
    {
        str     => { sub => 'a string' },
        a       => 'U',
        numbers => [ 1, 2, 3, 4, 5 ],
    } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'a string 4 U', "mix it all together: $syntax" );
