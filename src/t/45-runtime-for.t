#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 16;

my ( $template, $syntax );

#
#  1: <: for x in 2 :>.<: endfor :>
$syntax = "<: for x in 2 :>.<: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '...', 'for constant numeric set do constant content' );

#
#  2: <: for x in y :>.<: endfor :>
$syntax = "<: for x in y :>.<: endfor :>";
$template = Template::Sandbox->new();
$template->add_var( y => 4 );
$template->set_template_string( $syntax );
is( ${$template->run()}, '.....', 'for variable numeric set do constant content' );

#
#  3: <: for x in 2 :><: expr x :>.<: endfor :>
$syntax = "<: for x in 2 :><: expr x :>.<: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '0.1.2.', 'for constant numeric set do loop var content' );

#
#  4: <: for x in y :><: expr x :> in <: expr y :>.<: endfor :>
$syntax = "<: for x in y :><: expr x :> in <: expr y :>.<: endfor :>";
$template = Template::Sandbox->new();
$template->add_var( y => 4 );
$template->set_template_string( $syntax );
is( ${$template->run()}, '0 in 4.1 in 4.2 in 4.3 in 4.4 in 4.', 'for variable numeric set do loop var content' );

#
#  5: does __first__ work?
$syntax = "<: for x in 4 :><: expr x.__first__ :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '10000', 'x.__first__' );

#
#  6: does __inner__ work?
$syntax = "<: for x in 4 :><: expr x.__inner__ :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '01110', 'x.__inner__' );

#
#  7: does __last__ work?
$syntax = "<: for x in 4 :><: expr x.__last__ :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '00001', 'x.__last__' );

#
#  8: does __odd__ work?
$syntax = "<: for x in 4 :><: expr x.__odd__ :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '01010', 'x.__odd__' );

#
#  9: does __even__ work?
$syntax = "<: for x in 4 :><: expr x.__even__ :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '10101', 'x.__even__' );

#
#  10: does __counter__ work?
$syntax = "<: for x in 4 :><: expr x.__counter__ :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '01234', 'x.__counter__' );

#  These two could break if template functions are broken.

#
#  11: does __prev__ work?
$syntax = "<: for x in 4 :><: if defined( x.__prev__ ) :><: expr x.__prev__ :><: else :>[undef]<: endif :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '[undef]0123', 'x.__prev__' );

#
#  12: does __next__ work?
$syntax = "<: for x in 4 :><: if defined( x.__next__ ) :><: expr x.__next__ :><: else :>[undef]<: endif :><: endfor :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()}, '1234[undef]', 'x.__next__' );

#
#  13: loop across array
$syntax = "<: for x in y :><: expr x :>/<: endfor :>";
$template = Template::Sandbox->new();
$template->add_var( y => [ qw/one two three/ ] );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'one/two/three/', 'for array show value' );

#
#  14: loop across hash keys (alpha sorted by keys)
$syntax = "<: for x in y :><: expr x :>-<: endfor :>";
$template = Template::Sandbox->new();
$template->add_var( y => { one => 'ONE', two => 'TWO', three => 'THREE' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'one-three-two-', 'for hash show keys' );

#
#  15: loop across hash keys and values (alpha sorted by keys)
$syntax = "<: for x in y :><: expr x :> => <: expr x.__value__ :>, <: endfor :>";
$template = Template::Sandbox->new();
$template->add_var( y => { one => 'ONE', two => 'TWO', three => 'THREE' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'one => ONE, three => THREE, two => TWO, ', 'for hash show keys => values' );

#
#  16: <: for iterator=x set=y :> syntax
$syntax = "<: for iterator=x set=y :><: expr x :> => <: expr x.__value__ :>, <: endfor :>";
$template = Template::Sandbox->new();
$template->add_var( y => { one => 'ONE', two => 'TWO', three => 'THREE' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'one => ONE, three => THREE, two => TWO, ', 'for iterator=x set=y syntax' );
