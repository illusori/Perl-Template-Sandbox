#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 2;

my ( $template );

ok( Template::Sandbox->new(), "constructor via class" );

$template = Template::Sandbox->new();
ok( $template->new(), "constructor via instance" );
