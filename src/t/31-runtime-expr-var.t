#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

BEGIN
{
    eval "use Test::Exception";
    plan skip_all => "Test::Exception required for testing template vars" if @_;
}

plan tests => 22;

my ( $template, $syntax );

#  TODO:  Surely there's a Test:: module for this?
#         Test::Trap looks to clash with Test::Exception and not old perls.
sub warns_ok( &$$ )
{
    my ( $test, $like, $desc ) = @_;
    my ( $warning_contents );

    {
        $warning_contents = '';
        local $SIG{ __WARN__ } = sub { $warning_contents .= $_[ 0 ]; };
        $test->();
    }

    like( $warning_contents, $like, $desc );
}

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
#  11: numeric array faux-index __size__.
$syntax = "<: expr arr.__size__ :>";
$template = Template::Sandbox->new();
$template->add_var( arr => [ 42, 43, 44, 45 ] );
$template->set_template_string( $syntax );
is( ${$template->run()}, '4', 'numeric array faux-index __size__: arr.__size__' );

#
#  12: bracket hash index 'alpha'.
$syntax = "<: expr hash[ 'alpha' ] :>";
$template = Template::Sandbox->new();
$template->add_var( hash => { 'alpha' => 'a', 'beta' => 'b' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'a', "bracket hash index 'alpha': hash[ 'alpha' ]" );

#
#  13: variable bracket hash index.
$syntax = "<: expr hash[ greek ] :>";
$template = Template::Sandbox->new();
$template->add_var( hash => { 'alpha' => 'a', 'beta' => 'b' } );
$template->add_var( greek => 'beta' );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'b', "variable bracket hash index: hash[ greek ]" );

#
#  14: dotted hash index 'alpha'.
#  We've tested this already in effect, but here for completeness.
$syntax = "<: expr hash.alpha :>";
$template = Template::Sandbox->new();
$template->add_var( hash => { 'alpha' => 'a', 'beta' => 'b' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, 'a', "dotted hash index 'alpha': hash.alpha" );

#
#  15: dotted hash faux-index '__size__'.
#  We've tested this already in effect, but here for completeness.
$syntax = "<: expr hash.__size__ :>";
$template = Template::Sandbox->new();
$template->add_var( hash => { 'alpha' => 'a', 'beta' => 'b' } );
$template->set_template_string( $syntax );
is( ${$template->run()}, '2', "dotted hash faux-index '__size__': hash.__size__" );

#
#  16: mix it all together
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

#
#  17: warn on undef value
$syntax = "<: expr a :>";
$template = Template::Sandbox->new();
$template->set_template_string( $syntax );
warns_ok { $template->run() }
    qr/Template runtime error: undefined template value 'a' at line 1, char 1 of/,
    'warn on undef value of template var';

#
#  18: undef value for subscript of parent.
$syntax = "<: expr a[ b ] :>";
$template = Template::Sandbox->new();
$template->add_vars( {
    a => { 'existing' => 1, },
    b => 'non-existing',
    } );
$template->set_template_string( $syntax );
warns_ok { $template->run() }
    qr/Template runtime error: undefined template value 'a\[ b \]' at line 1, char 1 of/,
    'warn on undef value of template var hash value';

#
#  19: error on subscript of non-reference parent.
$syntax = "<: expr a[ b ] :>";
$template = Template::Sandbox->new();
$template->add_vars( {
    a => 'not a hash, idiot',
    b => 'index index index',
    } );
$template->set_template_string( $syntax );
throws_ok { $template->run() }
    qr/Template runtime error: Can't get key 'index index index' \(from 'b'\) of non-reference parent in 'a\[ b \]' at line 1, char 1 of/,
    'error on subscript of non-reference parent';

#
#  20: error on subscript of undef parent.
$syntax = "<: expr a[ b ] :>";
$template = Template::Sandbox->new();
$template->add_vars( {
    b => 'index index index',
    } );
$template->set_template_string( $syntax );
throws_ok { $template->run() }
    qr/Template runtime error: Can't get key 'index index index' \(from 'b'\) of undefined parent in 'a\[ b \]' at line 1, char 1 of/,
    'error on subscript of undef parent';

#
#  21: error on undef subscript of parent.
$syntax = "<: expr a[ b ] :>";
$template = Template::Sandbox->new();
$template->add_vars( {
    a => { 'existing' => 1, },
    } );
$template->set_template_string( $syntax );
{
    #  We want to hide the warning that 'b' is undef because it's
    #  intentional as the test...
    local $SIG{ __WARN__ } = sub {};
    throws_ok { $template->run() }
        qr/Template runtime error: Undefined index 'b' in 'a\[ b \]' at line 1, char 1 of/,
        'error on undef subscript of parent';
}

#
#  22: error on string index of array ref.
$syntax = "<: expr a[ b ] :>";
$template = Template::Sandbox->new();
$template->add_vars( {
    a => [ 'not a hash, idiot' ],
    b => 'index index index',
    } );
$template->set_template_string( $syntax );
throws_ok { $template->run() }
    qr/Template runtime error: Can't index array-reference with string 'index index index' \(from 'b'\) in 'a\[ b \]' at line 1, char 1 of/,
    'error on string index of array ref';
