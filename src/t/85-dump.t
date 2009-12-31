#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

plan tests => 2;

my ( $template, $syntax, $expected );

#
#  1:  literal string dump
$syntax = 'a literal string';

$expected = <<'END_OF_EXPECTED';
0000: [template-string        1   1][literal     ] "a literal string"
END_OF_EXPECTED

$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( $template->dumpable_template(), $expected,
    'literal string dump' );

#
#  2:  complex dump
$syntax = <<'END_OF_SYNTAX';
literal start
<: if 1 :>
constant if true
<: else :>
not optimized away :(
<: endif :>
<: expr a :>
<: if a :>
variable if true
<: else :>
variable if false
<: endif :>
<: for x in a :>
for loop contents
<: endfor :>
<: for x in a :>
second loop, run <: expr x.__counter__ :>
<: endfor :>
literal end
END_OF_SYNTAX

$expected = <<'END_OF_EXPECTED';
0000: [template-string        1   1][literal     ] "literal start\nconstant if true\n"
0001: [template-string        4   1][jump        ] 3
0002: [template-string        4  11][literal     ] "not optimized away :(\n"
0003: [template-string        6  12][literal     ] ""
0004: [template-string        7   1][expr        ] [104,"a",["a"],["a"]]
0005: [template-string        7  13][literal     ] "\n"
0006: [template-string        8   1][jump_if     ] 9 unless [104,"a",["a"],["a"]]
0007: [template-string        8  11][literal     ] "variable if true\n"
0008: [template-string       10   1][jump        ] 10
0009: [template-string       10  11][literal     ] "variable if false\n"
0010: [template-string       12  12][literal     ] ""
0011: [template-string       13   1][for         ] x in [104,"a",["a"],["a"]] then 14 (no special-vars)
0012: [template-string       13  17][literal     ] "for loop contents\n"
0013: [template-string       15   1][end_for     ] x in [104,"a",["a"],["a"]] repeat 12
0014: [template-string       15  13][literal     ] ""
0015: [template-string       16   1][for         ] x in [104,"a",["a"],["a"]] then 20
0016: [template-string       16  17][literal     ] "second loop, run "
0017: [template-string       17  18][expr        ] [104,"x.__counter__",["x","__counter__"],["x","__counter__"]]
0018: [template-string       17  42][literal     ] "\n"
0019: [template-string       18   1][end_for     ] x in [104,"a",["a"],["a"]] repeat 16
0020: [template-string       18  13][literal     ] "literal end\n"
END_OF_EXPECTED

$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
is( $template->dumpable_template(), $expected,
    'complex dump' );
