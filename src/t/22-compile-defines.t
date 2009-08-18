#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 9;

my ( $template, $syntax );

#
#  1: Define with value.
$syntax = '${PAGE}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    "string",
    'define with value' );

#
#  2:  Default value.
$syntax = '${NOSUCHDEFINE:this is my default value}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    "this is my default value",
    'define falling through to default' );

#
#  3:  No such define.
$syntax = '${NOSUCHDEFINE}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    "[undefined preprocessor define 'NOSUCHDEFINE']",
    'missing define' );

#
#  4: Recursive define.
#  FILENAME gets set to "string://$template_string", ie: contains ${FILENAME}
$syntax = '${FILENAME}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    "string:///[recursive define 'FILENAME']",
    'recursive define' );

#
#  5: Quoted define with value.
$syntax = '${\'PAGE\'}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    "'string'",
    'quoted define with value' );

#
#  6: Quoted define with quotes in value.
$syntax = '${\'FILENAME\'}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    q('string:///${\\'FILENAME\\'}'),
    'quoted define with quotes in value' );

#
#  7: Quoted define falling through to default.
$syntax = '${\'NOSUCHDEFINE:default value quoted\'}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    q('default value quoted'),
    'quoted define falling through to default' );

#
#  8: Quoted missing define.
$syntax = '${\'NOSUCHDEFINE\'}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( ${$template->run()},
    q('[undefined preprocessor define \\'NOSUCHDEFINE\\']'),
    'quoted missing define' );

#
#  9: set_template_string() with passed defines.
$syntax = '${DEFINEA}';
$template = Template::Sandbox->new();
$template->set_template_string( $syntax,
    {
        DEFINEA => 'some value or other',
    } );
is( ${$template->run()},
    "some value or other",
    'defines set in set_template_string()' );
