#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox qw/:function_sugar/;
use Test::Exception;

plan tests => 63;

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
    
my ( $template, $pre_template, $post_template, $function,
     $syntax, $oldsyntax, $expected );

$function = "nonexistingfunction";
$syntax = "<: expr ${function}() :>";

#
#  1:  Test that the custom function really doesn't exist and causes a fail.
$template = Template::Sandbox->new();
throws_ok { $template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    "verify custom syntax doesn't exist already";

#
#  2-4:  Function added during construction.
ok( $template = Template::Sandbox->new(
    template_function => [
        $function =>
            no_args sub { '[during-construction custom function was ere]' },
        ],
    ), 'construct with custom function as option' );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with during-construction custom function';
is( ${$template->run()}, '[during-construction custom function was ere]',
    'run of during-construction custom function' );


#
#  5-7:  Function added after construction.
$template = Template::Sandbox->new();
lives_ok { $template->register_template_function(
    $function => no_args sub { '[post-construction custom function was ere]' },
    ) } 'post-construct register of custom function';
lives_ok { $template->set_template_string( $syntax ) }
    'parse with post-construction registered custom function';
is( ${$template->run()}, '[post-construction custom function was ere]',
    'run of post-construction registered custom function' );

#
#  8-10:  Check add_template_function synonym.
$template = Template::Sandbox->new();
lives_ok { $template->add_template_function(
    $function => no_args sub { '[post-construction custom function was ere]' },
    ) } 'post-construct add of custom function (method synonym)';
lives_ok { $template->set_template_string( $syntax ) }
    'parse with post-construction added custom function';
is( ${$template->run()}, '[post-construction custom function was ere]',
    'run of post-construction added custom function' );

#
#  11-12:  Function unregister.
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            no_args sub { '[during-construction custom function was ere]' },
        ],
    );
lives_ok { $template->unregister_template_function( $function ) }
    'unregister custom function';
throws_ok { $template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    "verify custom function no-longer exists";

#
#  13-14:  Function delete synonym.
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            no_args sub { '[during-construction custom function was ere]' },
        ],
    );
lives_ok { $template->delete_template_function( $function ) }
    'delete custom function (method synonym)';
throws_ok { $template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    "verify custom function no-longer exists";

#
#  15-16: Constant single arg to one-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( 1 ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            one_arg sub
            {
                return( '[one_arg custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with constant single arg to one_arg custom function';
is( ${$template->run()}, '[one_arg custom function was ere with args: 1]',
    'run of constant single arg to one_arg custom function' );
$syntax = $oldsyntax;

#
#  17-18: Variable single arg to one-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            one_arg sub
            {
                return( '[one_arg custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
$template->add_var( a => 45 );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with variable single arg to one_arg custom function';
is( ${$template->run()}, '[one_arg custom function was ere with args: 45]',
    'run of variable single arg to one_arg custom function' );
$syntax = $oldsyntax;

#
#  19-20: Constant arg to two-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( 1, 2 ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            two_args sub
            {
                return( '[two_args custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with constant args to two_args custom function';
is( ${$template->run()}, '[two_args custom function was ere with args: 1,2]',
    'run of constant args to two_args custom function' );
$syntax = $oldsyntax;

#
#  21-22: Variable args to two-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a, b ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            two_args sub
            {
                return( '[two_args custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
$template->add_vars( { a => 45, b => 19, } );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with variable args to two_args custom function';
is( ${$template->run()}, '[two_args custom function was ere with args: 45,19]',
    'run of variable args to two_args custom function' );
$syntax = $oldsyntax;

#
#  23-24: Constant arg to three-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( 1, 2, 5 ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            three_args sub
            {
                return( '[three_args custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with constant args to three_args custom function';
is( ${$template->run()},
    '[three_args custom function was ere with args: 1,2,5]',
    'run of constant args to three_args custom function' );
$syntax = $oldsyntax;

#
#  25-26: Variable args to three-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a, b, c ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            three_args sub
            {
                return( '[three_args custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
$template->add_vars( { a => 45, b => 19, c => 25, } );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with variable args to three_args custom function';
is( ${$template->run()},
    '[three_args custom function was ere with args: 45,19,25]',
    'run of variable args to three_args custom function' );
$syntax = $oldsyntax;

#
#  27: No args to one-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}() :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            one_arg sub
            {
                return( '[one_arg custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
throws_ok { $template->set_template_string( $syntax ) }
    qr/compile error: too few args to nonexistingfunction\(\), expected 1 and got 0 in nonexistingfunction\(\) at line 1, char 1 of/,
    'error on missing args to single-arg function';
$syntax = $oldsyntax;

#
#  28: Two args to one-arg function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( 1, 2 ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            one_arg sub
            {
                return( '[one_arg custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
throws_ok { $template->set_template_string( $syntax ) }
    qr/compile error: too many args to nonexistingfunction\(\), expected 1 and got 2 in nonexistingfunction\( 1, 2 \) at line 1, char 1 of/,
    'error on two args to single-arg function';
$syntax = $oldsyntax;

#
#  29-30: construct-option function instance locality testing
$pre_template = Template::Sandbox->new();
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            no_args sub { '[during-construction custom function was ere]' },
        ],
    );
$post_template = Template::Sandbox->new();
throws_ok { $pre_template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    "construct-option instance-function doesn't exist in existing instances";
throws_ok { $post_template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    "construct-option instance-function doesn't exist in new instances";

#
#  31-32: post-construct function instance locality testing
$pre_template = Template::Sandbox->new();
$template = Template::Sandbox->new();
$template->register_template_function(
    $function => no_args sub { '[post-construction custom function was ere]' },
    );
$post_template = Template::Sandbox->new();
throws_ok { $pre_template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    "post-construct instance-function doesn't exist in existing instances";
throws_ok { $post_template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    "post-construct instance-function doesn't exist in new instances";

#
#  33-39: class-method testing
$pre_template = Template::Sandbox->new();
lives_ok { Template::Sandbox->register_template_function(
    $function => no_args sub { '[class-method custom function was ere]' },
    ) }
    'class-method register of custom function';
$post_template = Template::Sandbox->new();
lives_ok { $pre_template->set_template_string( $syntax ) }
    'existing template parse with class-method registered custom function';
is( ${$pre_template->run()}, '[class-method custom function was ere]',
    'existing template run of class-method custom function' );
lives_ok { $post_template->set_template_string( $syntax ) }
    'new template parse with class-method registered custom function';
is( ${$post_template->run()}, '[class-method custom function was ere]',
    'new template run of class-method custom function' );
lives_ok { Template::Sandbox->unregister_template_function( $function ) }
    'class-method unregister of custom function';
$template = Template::Sandbox->new();
throws_ok { $template->set_template_string( $syntax ) }
    qr/compile error: Unknown function: nonexistingfunction at line 1, char 1 of/,
    'verify class-method custom function was removed';

#
#  40-42:  needs_template testing
#  TODO:  check it's _our_ template
ok( $template = Template::Sandbox->new(
    template_function => [
        $function =>
            needs_template no_args sub
            {
                '[needs_template function got a: ' . ref( $_[ 0 ] ) . ']'
            },
        ],
    ), 'construct with needs_template custom function as option' );
lives_ok { $template->set_template_string( $syntax ) }
    'parse with needs_template custom function';
is( ${$template->run()}, '[needs_template function got a: Template::Sandbox]',
    'run of needs_template custom function' );

#
#  43:  non-constant needs_template function.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            needs_template one_arg sub
            {
                '[needs_template function got: ' . ref( $_[ 0 ] ) .
                    ', ' . $_[ 1 ] . ']'
            },
        ],
    );
$template->set_template_string( $syntax );
$template->add_var( a => 12 );
is( ${$template->run()}, '[needs_template function got: Template::Sandbox, 12]',
    'run of non-constant-folded needs_template custom function' );
$syntax = $oldsyntax;

#
#  44-45:  warnings on remove of non-existing function
$template = Template::Sandbox->new();
{
    local $^W = 1;
    warns_ok { $template->unregister_template_function( $function ) }
        qr/Template post-initialization error: Template function 'nonexistingfunction' does not exist, cannot be removed. at .*70-custom-function\.t line/,
        'warn on unregister of non-existing function';
}
{
    local $^W = 0;
    doesnt_warn { $template->unregister_template_function( $function ) }
        'warning suppression on unregister of non-existing function';
}

#
#  46-47:  warnings on add of existing function
$template = Template::Sandbox->new();
$template->register_template_function(
    $function => no_args sub { '[post-construction custom function was ere]' },
    );
{
    local $^W = 1;
    warns_ok
        {
            $template->register_template_function(
                $function => no_args
                    sub { '[post-construction custom function was ere]' },
                );
        }
        qr/Template post-initialization error: Template function 'nonexistingfunction' exists, overwriting. at .*70-custom-function\.t line/,
        'warn on register of existing function';
}
{
    local $^W = 0;
    doesnt_warn
        {
            $template->register_template_function(
                $function => no_args
                    sub { '[post-construction custom function was ere]' },
                );
        }
        'warning suppression on register of existing function';
}

#
#  48: Does local instance-function mask class-function?
Template::Sandbox->register_template_function(
    $function => no_args sub { '[class-method custom function was ere]' },
    );
$template = Template::Sandbox->new();
$template->register_template_function(
    $function => no_args sub { '[instance-method custom function was ere]' },
    );
$template->set_template_string( $syntax );
is( ${$template->run()}, '[instance-method custom function was ere]',
    'instance registration masks class registration' );
Template::Sandbox->unregister_template_function( $function );

#
#  49: undef args produce warning
$oldsyntax = $syntax;
$syntax = "<: expr $function( a ) :>";
$template = Template::Sandbox->new();
$template->register_template_function(
    $function => one_arg sub { '[nobody expects the undef inquisition]' },
    );
$template->set_template_string( $syntax );
warns_ok { $template->run(); }
    qr/Template runtime error: undefined template value 'a' at line 1, char 1 of/,
    'warn on undef function args';
$syntax = $oldsyntax;

#
#  50: undef_ok prevents undef args warning
$oldsyntax = $syntax;
$syntax = "<: expr $function( a ) :>";
$template = Template::Sandbox->new();
$template->register_template_function(
    $function => undef_ok one_arg
        sub { "[what's a little undef among friends?]" },
    );
$template->set_template_string( $syntax );
doesnt_warn { $template->run(); }
    'undef_ok function sugar suppresses undef warning';
$syntax = $oldsyntax;

#
#  51: hashref function definition args to constructor.
throws_ok
    {
        $template = Template::Sandbox->new(
            template_function => [
                $function =>
                    { 'you were expecting' => 'an arrayref or coderef?' },
                ],
            );
    }
    qr/Template initialization error: Bad template function '$function' to register_template_function\(\), expected sub ref or 'function_sugar'ed sub ref, got: HASH at .*Template.*Sandbox\.pm line/,
    'error on hashref definition in construct-option function';

#
#  52: bad function definition args to constructor.
throws_ok
    {
        $template = Template::Sandbox->new(
            template_function => [
                $function =>
                    'you were expecting an arrayref or coderef?',
                ],
            );
    }
    qr/Template initialization error: Bad template function '$function' to register_template_function\(\), expected sub ref or 'function_sugar'ed sub ref, got: 'you were expecting an arrayref or coderef\?' at .*Template.*Sandbox\.pm line/,
    'error on scalar definition in construct-option function';

#
#  53: hashref function definition args to register method.
$template = Template::Sandbox->new();
throws_ok
    {
        $template->register_template_function(
            $function => { 'you were expecting' => 'an arrayref or coderef?' },
            );
    }
    qr/Template post-initialization error: Bad template function '$function' to register_template_function\(\), expected sub ref or 'function_sugar'ed sub ref, got: HASH at .*Template.*Sandbox\.pm line/,
    'error on hashref definition in post-construct function';

#
#  54: hashref function definition args to register method.
$template = Template::Sandbox->new();
throws_ok
    {
        $template->register_template_function(
            $function => 'you were expecting an arrayref or coderef?',
            );
    }
    qr/Template post-initialization error: Bad template function '$function' to register_template_function\(\), expected sub ref or 'function_sugar'ed sub ref, got: 'you were expecting an arrayref or coderef\?' at .*Template.*Sandbox\.pm line/,
    'error on scalar definition in post-construct function';

#
#  55: raw sub register autoapplies function sugar.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( 1, 2 ) :>";
$template = Template::Sandbox->new(
    template_function => [
        $function =>
            sub
            {
                return( '[raw-sub custom function was ere with args: ' .
                    join( ',', @_ ) . ']' );
            },
        ],
    );
$template->set_template_string( $syntax );
is( ${$template->run()}, '[raw-sub custom function was ere with args: 1,2]',
    'raw-sub custom function auto-applies function sugar' );
$syntax = $oldsyntax;

#
#  56: error on run of instance function removed after compile
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a ) :>";
$template = Template::Sandbox->new();
$template->register_template_function(
    $function => one_arg sub { '[instance-method custom function was ere]' },
    );
$template->add_var( a => 1 );
$template->set_template_string( $syntax );
$template->unregister_template_function( $function );
throws_ok { $template->run() }
    qr{Template runtime error: Unknown function: nonexistingfunction at line 1, char 1 of},
    'error on run of instance function removed after compile';
$syntax = $oldsyntax;

#
#  57: error on run of class function removed after compile
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a ) :>";
Template::Sandbox->register_template_function(
    $function => one_arg sub { '[class-method custom function was ere]' },
    );
$template = Template::Sandbox->new();
$template->add_var( a => 1 );
$template->set_template_string( $syntax );
Template::Sandbox->unregister_template_function( $function );
throws_ok { $template->run() }
    qr{Template runtime error: Unknown function: nonexistingfunction at line 1, char 1 of},
    'error on run of class function removed after compile';
$syntax = $oldsyntax;

#
#  58-59: copy_global_functions tests.
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a ) :>";
Template::Sandbox->register_template_function(
    $function => one_arg sub { '[class-method custom function was ere]' },
    );
$template = Template::Sandbox->new(
    copy_global_functions => 1,
    );
$template->add_var( a => 1 );
Template::Sandbox->unregister_template_function( $function );
lives_ok { $template->set_template_string( $syntax ); }
    'copy_global_functions instance compiles after class function deleted';
lives_and { is ${$template->run()}, '[class-method custom function was ere]' }
    'copy_global_functions instance runs after class function deleted';
$syntax = $oldsyntax;

#
#  60:  return ref to scalar (const func)
$template = Template::Sandbox->new();
$template->register_template_function(
    $function => no_args sub
        {
            my $ret = '[return-by-reference custom function was ere]';
            return( \$ret );
        },
    );
$template->set_template_string( $syntax );
is( ${$template->run()}, '[return-by-reference custom function was ere]',
    'constant custom function returning scalar-reference' );

#
#  61:  return ref to scalar (inconst func)
$oldsyntax = $syntax;
$syntax = "<: expr ${function}( a ) :>";
$template = Template::Sandbox->new();
$template->add_var( a => 50 );
$template->register_template_function(
    $function => one_arg sub
        {
            my $ret = '[return-by-reference custom function was ere]';
            return( \$ret );
        },
    );
$template->set_template_string( $syntax );
is( ${$template->run()}, '[return-by-reference custom function was ere]',
    'non-constant custom function returning scalar-reference' );
$syntax = $oldsyntax;

#
#  62: apparantly-constant function optimization.
$oldsyntax = $syntax;
$syntax = "<: for x in 3 :>\nOn loop <: expr x :> function returns: <: expr ${function}() :>\n<: end for :>\n";
$template = Template::Sandbox->new();
{
    my ( $callcount );

    $callcount = 0;

    $template->register_template_function(
        $function => no_args sub
            {
                $callcount++;
                "[I've been called $callcount times]";
            },
        );
}
$template->set_template_string( $syntax );
$expected = <<END_OF_EXPECTED;
On loop 0 function returns: [I've been called 1 times]
On loop 1 function returns: [I've been called 1 times]
On loop 2 function returns: [I've been called 1 times]
On loop 3 function returns: [I've been called 1 times]
END_OF_EXPECTED
is( ${$template->run()}, $expected,
    'apparantly-constant function optimization' );
$syntax = $oldsyntax;

#
#  63: inconstant function-sugar on apparantly-constant function
$oldsyntax = $syntax;
$syntax = "<: for x in 3 :>\nOn loop <: expr x :> function returns: <: expr ${function}() :>\n<: end for :>\n";
$template = Template::Sandbox->new();
{
    my ( $callcount );

    $callcount = 0;

    $template->register_template_function(
        $function => no_args inconstant sub
            {
                $callcount++;
                "[I've been called $callcount times]";
            },
        );
}
$template->set_template_string( $syntax );
$expected = <<END_OF_EXPECTED;
On loop 0 function returns: [I've been called 1 times]
On loop 1 function returns: [I've been called 2 times]
On loop 2 function returns: [I've been called 3 times]
On loop 3 function returns: [I've been called 4 times]
END_OF_EXPECTED
is( ${$template->run()}, $expected,
    'inconstant function-sugar on apparantly-constant function' );
$syntax = $oldsyntax;

#  TODO: call non-existing function when different local function added
#  TODO: call non-existing function when different class function added
#  TODO: multiple custom functions as single constructor param
#  TODO: multiple custom functions as multiple constructor param
