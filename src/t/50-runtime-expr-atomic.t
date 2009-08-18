#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 4;

my ( $template, $syntax );

#
#  1: literal number
$syntax = '<: expr 1 :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'atomic expr literal number' );

#
#  2: literal string
$syntax = q~<: expr 'a string' :>~;
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    'a string',
    'atomic expr literal string' );

#
#  3: template variable
$syntax = '<: expr a :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
$template->add_var( a => 12 );
is( ${$template->run()},
    '12',
    'atomic expr variable' );

#
#  4: bracketed variable
$syntax = '<: expr ( a ) :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
$template->add_var( a => 12 );
is( ${$template->run()},
    '12',
    'atomic expr bracketed variable' );
