#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;
use Template::Sandbox::Library;

BEGIN
{
    eval "use Test::Exception";
    plan skip_all => "Test::Exception required for testing custom syntax" if @_;
}

my ( $num_single_tests );
my ( $num_instance_imports, $num_import_tests );
my ( $num_tests );

$num_single_tests     = 25;
$num_instance_imports = 2;
$num_import_tests     = 11;

$num_tests = $num_single_tests +
    ( $num_instance_imports * $num_import_tests );

plan tests => $num_tests;

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
sub doesnt_warn( &$ )
{
    my ( $test, $desc ) = @_;
    my ( $warning_contents );

    {
        $warning_contents = '';
        local $SIG{ __WARN__ } = sub { $warning_contents .= $_[ 0 ]; };
        $test->();
    }

    is( $warning_contents, '', $desc );
}
    
package Template::Sandbox::TestLibrary;

use base 'Template::Sandbox::Library';

use Template::Sandbox qw/:function_sugar/;

__PACKAGE__->set_library_functions(
    'first_function' =>
        ( no_args sub { '[first_function function was in da library]' } ),
    'second_function' =>
        ( one_arg sub { '[second_function function was in da library]' } ),
    'third_function' =>
        ( no_args sub { '[third_function function was in da library]' } ),
    );

__PACKAGE__->set_library_tags(
    'one_and_two'   => [ qw/first_function second_function/ ],
    'two_and_three' => [ qw/second_function third_function/ ],
    );

package main;

my ( $template, $pre_template, $post_template,
     $first_syntax, $second_syntax, $third_syntax,
     $first_expected, $second_expected, $third_expected );

$first_syntax  = "<: expr first_function() :>";
$second_syntax = "<: expr second_function( 1 ) :>";
$third_syntax  = "<: expr third_function() :>";

$first_expected  = '[first_function function was in da library]';
$second_expected = '[second_function function was in da library]';
$third_expected  = '[third_function function was in da library]';

#
#  1-3:  Test that the custom functions really don't exist and cause a fail.
$template = Template::Sandbox->new();
throws_ok { $template->set_template_string( $first_syntax ) }
    qr/compile error: Unknown function: first_function at line 1, char 1 of/,
    "verify first_function() doesn't exist already";
throws_ok { $template->set_template_string( $second_syntax ) }
    qr/compile error: Unknown function: second_function at line 1, char 1 of/,
    "verify second_function() doesn't exist already";
throws_ok { $template->set_template_string( $third_syntax ) }
    qr/compile error: Unknown function: third_function at line 1, char 1 of/,
    "verify third_function() doesn't exist already";

#
#  4-6:  Test that plain "use" doesn't register any functions.
eval "use Template::Sandbox::TestLibrary";
throws_ok { $template->set_template_string( $first_syntax ) }
    qr/compile error: Unknown function: first_function at line 1, char 1 of/,
    "verify first_function() doesn't exist after plain use";
throws_ok { $template->set_template_string( $second_syntax ) }
    qr/compile error: Unknown function: second_function at line 1, char 1 of/,
    "verify second_function() doesn't exist after plain use";
throws_ok { $template->set_template_string( $third_syntax ) }
    qr/compile error: Unknown function: third_function at line 1, char 1 of/,
    "verify third_function() doesn't exist after plain use";

foreach my $import_method ( qw/manual constructor/ )
{
    my ( $constructor );

    if( $import_method eq 'manual' )
    {
        $constructor = sub
            {
                my ( $pre_t, $t, $post_t );

                $pre_t  = Template::Sandbox->new();
                $t      = Template::Sandbox->new();
                Template::Sandbox::TestLibrary->export_template_functions(
                    $t, @_ );
                $post_t = Template::Sandbox->new();

                return( ( $pre_t, $t, $post_t ) );
            };
    }
    elsif( $import_method eq 'constructor' )
    {
        $constructor = sub
            {
                my ( $pre_t, $t, $post_t );

                $pre_t  = Template::Sandbox->new();
                $t      = Template::Sandbox->new(
                    library => [ 'Template::Sandbox::TestLibrary' => @_ ],
                    );
                $post_t = Template::Sandbox->new();

                return( ( $pre_t, $t, $post_t ) );
            };
    }

    #
    #  +1-5: Test instance import of single function.
    ( $pre_template, $template, $post_template ) =
        $constructor->( qw/first_function/ );
    lives_ok { $template->set_template_string( $first_syntax ) }
        "parse with $import_method instance import of single function";
    is( ${$template->run()}, $first_expected,
        "run of $import_method instance import of single function" );
    throws_ok { $pre_template->set_template_string( $first_syntax ) }
        qr/compile error: Unknown function: first_function at line 1, char 1 of/,
        "verify $import_method instance import doesn't contaminate existinng instances";
    throws_ok { $post_template->set_template_string( $first_syntax ) }
        qr/compile error: Unknown function: first_function at line 1, char 1 of/,
        "verify $import_method instance import doesn't contaminate new instances";
    throws_ok { $template->set_template_string( $second_syntax ) }
        qr/compile error: Unknown function: second_function at line 1, char 1 of/,
        "verify $import_method instance import of single doesn't import extras";

    #
    #  +6-8: Test instance import of two functions.
    ( $pre_template, $template, $post_template ) =
        $constructor->( qw/first_function third_function/ );
    lives_ok
        { $template->set_template_string( $first_syntax . $third_syntax ) }
        "parse with $import_method instance import of two functions";
    is( ${$template->run()}, $first_expected . $third_expected,
        "run of $import_method instance import of two functions" );
    throws_ok { $template->set_template_string( $second_syntax ) }
        qr/compile error: Unknown function: second_function at line 1, char 1 of/,
        "verify $import_method instance import of two functions doesn't import extras";

    #
    #  +9-11: Test instance import of tag.
    ( $pre_template, $template, $post_template ) =
        $constructor->( qw/:one_and_two/ );
    lives_ok
        { $template->set_template_string( $first_syntax . $second_syntax ) }
        "parse with $import_method instance import of tag";
    is( ${$template->run()}, $first_expected . $second_expected,
        "run of $import_method instance import of tag" );
    throws_ok { $template->set_template_string( $third_syntax ) }
        qr/compile error: Unknown function: third_function at line 1, char 1 of/,
        "verify $import_method instance import of tag doesn't import extras";
}

#
#  7-11: Test class import of single function.
$pre_template = Template::Sandbox->new();
Template::Sandbox::TestLibrary->import( qw/first_function/ );
$template = Template::Sandbox->new();
lives_ok { $template->set_template_string( $first_syntax ) }
    "new template parse with class import of single function";
is( ${$template->run()}, $first_expected,
    "new template run of class import of single function" );
lives_ok { $pre_template->set_template_string( $first_syntax ) }
    "existing template parse with class import of single function";
is( ${$pre_template->run()}, $first_expected,
    "existing template run of class import of single function" );
throws_ok { $template->set_template_string( $second_syntax ) }
    qr/compile error: Unknown function: second_function at line 1, char 1 of/,
    "verify class import of single doesn't import extras";
Template::Sandbox->unregister_template_function( qw/first_function/ );

#
#  12-16: Test class import of two functions.
$pre_template = Template::Sandbox->new();
Template::Sandbox::TestLibrary->import( qw/first_function third_function/ );
$template = Template::Sandbox->new();
lives_ok { $template->set_template_string( $first_syntax . $third_syntax ) }
    "new template parse with class import of two functions";
is( ${$template->run()}, $first_expected . $third_expected,
    "new template run of class import of two functions" );
lives_ok
    { $pre_template->set_template_string( $first_syntax . $third_syntax ) }
    "existing template parse with class import of two functions";
is( ${$pre_template->run()}, $first_expected . $third_expected,
    "existing template run of class import of two functions" );
throws_ok { $template->set_template_string( $second_syntax ) }
    qr/compile error: Unknown function: second_function at line 1, char 1 of/,
    "verify class import of two functions doesn't import extras";
Template::Sandbox->unregister_template_function(
    qw/first_function third_function/
    );

#
#  17-21: Test class import of tag.
$pre_template = Template::Sandbox->new();
Template::Sandbox::TestLibrary->import( qw/:one_and_two/ );
$template = Template::Sandbox->new();
lives_ok { $template->set_template_string( $first_syntax . $second_syntax ) }
    "new template parse with class import of tag";
is( ${$template->run()}, $first_expected . $second_expected,
    "new template run of class import of tag" );
lives_ok
    { $pre_template->set_template_string( $first_syntax . $second_syntax ) }
    "existing template parse with class import of tag";
is( ${$pre_template->run()}, $first_expected . $second_expected,
    "existing template run of class import of tag" );
throws_ok { $template->set_template_string( $third_syntax ) }
    qr/compile error: Unknown function: third_function at line 1, char 1 of/,
    "verify class import of tag doesn't import extras";
Template::Sandbox->unregister_template_function(
    qw/first_function second_function/
    );

#
#  22-25: Test class import of tag.
$pre_template = Template::Sandbox->new();
Template::Sandbox::TestLibrary->import( qw/:all/ );
$template = Template::Sandbox->new();
lives_ok { $template->set_template_string(
    $first_syntax . $second_syntax . $third_syntax ) }
    "new template parse with class import of :all";
is( ${$template->run()}, $first_expected . $second_expected . $third_expected,
    "new template run of class import of :all" );
lives_ok
    { $pre_template->set_template_string(
    $first_syntax . $second_syntax . $third_syntax ) }
    "existing template parse with class import of tag";
is( ${$pre_template->run()},
    $first_expected . $second_expected . $third_expected,
    "existing template run of class import of :all" );
Template::Sandbox->unregister_template_function(
    qw/first_function second_function third_function/
    );
