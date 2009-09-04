#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;
use Test::Exception;

plan tests => 1;

my ( $template, $syntax );

#
#  1: Malformed expression.
$syntax = '<: expr a a :>';
$template = Template::Sandbox->new();
throws_ok { $template->set_template_string( $syntax ) }
    qr/compile error: Not a well-formed expression: a a at line 1, char 1 of/,
    'malformed expression';
