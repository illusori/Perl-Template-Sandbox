#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 20;

my ( $template, $syntax );

#
#  1: negative literal number
$syntax = '<: expr -1 :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '-1',
    'unary op negative literal number' );

#
#  2: negated (!) true literal number
$syntax = '<: expr !1 :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (!) true literal number' );

#
#  3: negated (!) false literal number
$syntax = '<: expr !0 :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (!) false literal number' );

#
#  4: negated (not) true literal number
$syntax = '<: expr not 1 :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (not) true literal number' );

#
#  5: negated (not) false literal number
$syntax = '<: expr not 0 :>';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (not) false literal number' );

#
#  6: negative literal string
#  This is odd, probably should raise an error or a warning at least.
$syntax = "<: expr -'a string' :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '-a string',
    'unary op negative literal string' );

#
#  7: negated (!) true literal string
$syntax = "<: expr !'a string' :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (!) true literal string' );

#
#  8: negated (!) false literal string
$syntax = "<: expr !'' :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (!) false literal string' );

#
#  9: negated (not) true literal string
$syntax = "<: expr not 'a string' :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (not) true literal string' );

#
#  10: negated (not) false literal string
$syntax = "<: expr not '' :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (not) false literal string' );

#
#  11: negative variable number
$syntax = '<: expr -a :>';
$template = Template::Sandbox->new();
$template->add_var( a => 1 );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '-1',
    'unary op negative variable number' );

#
#  12: negated (!) true variable number
$syntax = '<: expr !a :>';
$template = Template::Sandbox->new();
$template->add_var( a => 1 );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (!) true variable number' );

#
#  13: negated (!) false variable number
$syntax = '<: expr !a :>';
$template = Template::Sandbox->new();
$template->add_var( a => 0 );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (!) false variable number' );

#
#  14: negated (not) true variable number
$syntax = '<: expr not a :>';
$template = Template::Sandbox->new();
$template->add_var( a => 1 );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (not) true variable number' );

#
#  15: negated (not) false variable number
$syntax = '<: expr not a :>';
$template = Template::Sandbox->new();
$template->add_var( a => 0 );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (not) false variable number' );

#
#  16: negative variable string
#  This is odd, probably should raise an error or a warning at least.
$syntax = "<: expr -a :>";
$template = Template::Sandbox->new();
$template->add_var( a => 'a string' );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '-a string',
    'unary op negative variable string' );

#
#  17: negated (!) true variable string
$syntax = "<: expr !a :>";
$template = Template::Sandbox->new();
$template->add_var( a => 'a string' );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (!) true variable string' );

#
#  18: negated (!) false variable string
$syntax = "<: expr !a :>";
$template = Template::Sandbox->new();
$template->add_var( a => '' );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (!) false variable string' );

#
#  19: negated (not) true variable string
$syntax = "<: expr not a :>";
$template = Template::Sandbox->new();
$template->add_var( a => 'a string' );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '0',
    'unary op negated (not) true variable string' );

#
#  20: negated (not) false variable string
$syntax = "<: expr not a :>";
$template = Template::Sandbox->new();
$template->add_var( a => '' );
$template->set_template_string( $syntax );
is( ${$template->run()},
    '1',
    'unary op negated (not) false variable string' );
