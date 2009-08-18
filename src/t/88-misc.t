#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 6;

my ( $template, $syntax );

#
#  1-3:  total time
$syntax = 'a literal string<: if a :>yes<: else :>no<: endif :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( $template->total_time(), 0, 'zero total-time before run' );
$template->run();
isnt( $template->total_time(), 0, 'non-zero total-time after run' );
$template->clear_total_time();
is( $template->total_time(), 0, 'zero total-time after clear' );

#
#  4: tersedump()
is( Template::Sandbox::_tersedump( { a => 1 } ),
    "{\n          \"a\" => 1\n        }\n",
    '_tersedump()' );

#
#  5: tinydump()
is( Template::Sandbox::_tinydump( { a => 1 } ), "{a=>1}",
    '_tersedump()' );

#  6: cache_key()
like( Template::Sandbox->cache_key( { a => 1 } ),
    qr/^[a-zA-Z0-9]+$/,
    'cache_key()' );
