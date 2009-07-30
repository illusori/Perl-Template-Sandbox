#
#  Template::Sandbox: Yet Another Template System.
#
#  Known issues:
#    - ${VALUE} define replacement breaks char-count and potentially
#      line count values (if ${VALUE} has newlines).
#
#  Future ideas:
#

package Template::Sandbox;

use strict;
use warnings;

use Carp;
use Class::Handle;
use Cwd ();
use Data::Dumper;
use IO::File;
use Log::Any;
use Scalar::Util;
use Storable;
use Time::HiRes;

my ( $total_time );

#my ( @function_table );

#  Array indices.
sub SELF()   { 0; }
sub OP_LHS() { 1; }
sub OP_RHS() { 2; }

#  Compiled line indices.
#  TODO: currently unused.
sub LINE_INSTR() { 0; }
sub LINE_POS()   { 1; }
sub LINE_ARG()   { 2; }

#  Instruction opcodes.
sub LITERAL()      { 0; }
#sub FORM()         { 1; }  #  TODO: remove and shuffle up
#sub URL()          { 2; }  #  TODO: remove and shuffle up
sub DEBUG()        { 3; }
sub EXPR()         { 4; }
sub JUMP()         { 5; }
sub JUMP_IF()      { 6; }
sub FOR()          { 7; }
sub END_FOR()      { 8; }
sub CONTEXT_PUSH() { 9; }
sub CONTEXT_POP()  { 10; }

#  Starting point for opcodes for locally registered syntaxes.
sub LOCAL_SYNTAX() { 1_000_000; }

#  Expression opcodes.
sub OP_TREE()  { 100; }
sub UNARY_OP() { 101; }
sub FUNC()     { 102; }
sub METHOD()   { 103; }
sub VAR()      { 104; }
sub TEMPLATE() { 105; }

#  Template function array indices.
sub FUNC_FUNC()         { 0; }
sub FUNC_ARG_NUM()      { 1; }
sub FUNC_NEEDS_TEMPLATE { 2; }
sub FUNC_INCONST()      { 3; }
sub FUNC_UNDEF_OK()     { 4; }

#  The lower the weight the tighter it binds.
my %operators = (
    #  Logic operators
    'or'  => [ 100, sub { $_[ SELF ]->_eval_expression( $_[ OP_LHS ], 1 ) or
                          $_[ SELF ]->_eval_expression( $_[ OP_RHS ], 1 ) },
               1 ],
    'and' => [ 99,  sub { $_[ SELF ]->_eval_expression( $_[ OP_LHS ], 1 ) and
                          $_[ SELF ]->_eval_expression( $_[ OP_RHS ], 1 ) },
               1 ],
    '||'  => [ 98,  sub { $_[ SELF ]->_eval_expression( $_[ OP_LHS ], 1 ) ||
                          $_[ SELF ]->_eval_expression( $_[ OP_RHS ], 1 ) },
               1 ],
    '&&'  => [ 96,  sub { $_[ SELF ]->_eval_expression( $_[ OP_LHS ], 1 ) &&
                          $_[ SELF ]->_eval_expression( $_[ OP_RHS ], 1 ) },
               1 ],
    #  Comparison operators
    'cmp' => [ 95,  sub { $_[ OP_LHS ] cmp $_[ OP_RHS ] } ],
    'ne'  => [ 94,  sub { $_[ OP_LHS ] ne  $_[ OP_RHS ] } ],
    'eq'  => [ 93,  sub { $_[ OP_LHS ] eq  $_[ OP_RHS ] } ],
    '<=>' => [ 92,  sub { $_[ OP_LHS ] <=> $_[ OP_RHS ] } ],
    '!='  => [ 91,  sub { $_[ OP_LHS ] !=  $_[ OP_RHS ] } ],
    '=='  => [ 90,  sub { $_[ OP_LHS ] ==  $_[ OP_RHS ] } ],
    'ge'  => [ 89,  sub { $_[ OP_LHS ] ge  $_[ OP_RHS ] } ],
    'le'  => [ 88,  sub { $_[ OP_LHS ] le  $_[ OP_RHS ] } ],
    'gt'  => [ 87,  sub { $_[ OP_LHS ] gt  $_[ OP_RHS ] } ],
    'lt'  => [ 86,  sub { $_[ OP_LHS ] lt  $_[ OP_RHS ] } ],
    '>='  => [ 85,  sub { $_[ OP_LHS ] >=  $_[ OP_RHS ] } ],
    '<='  => [ 84,  sub { $_[ OP_LHS ] <=  $_[ OP_RHS ] } ],
    '>'   => [ 83,  sub { $_[ OP_LHS ] >   $_[ OP_RHS ] } ],
    '<'   => [ 82,  sub { $_[ OP_LHS ] <   $_[ OP_RHS ] } ],

    #  Assignment
    '='   => [ 75,  sub { $_[ SELF ]->_assign_var( $_[ OP_LHS ],
                              $_[ SELF ]->_eval_expression( $_[ OP_RHS ] ) )
                        },
               1 ],

    #  Arithmetic/concat
    '.'   => [ 70,  sub { $_[ OP_LHS ] .  $_[ OP_RHS ] } ],
    '+'   => [ 69,  sub { $_[ OP_LHS ] +  $_[ OP_RHS ] } ],
    '-'   => [ 68,  sub { $_[ OP_LHS ] -  $_[ OP_RHS ] } ],
    '%'   => [ 67,  sub { $_[ OP_LHS ] %  $_[ OP_RHS ] } ],
    '/'   => [ 66,  sub { $_[ OP_LHS ] /  $_[ OP_RHS ] } ],
    '*'   => [ 65,  sub { $_[ OP_LHS ] *  $_[ OP_RHS ] } ],
    );

sub def_func
{
    my ( $ret, $flag, $val ) = @_;
    $ret = [ $ret ] if ref( $ret ) ne 'ARRAY';
    $ret->[ $flag ] = $val;
#warn "def_func: ..." . tersedump( $ret );
    return( $ret );
}

sub inconstant { return( def_func( @_, FUNC_INCONST, 1 ) ); }
sub needs_template { return( def_func( @_, FUNC_NEEDS_TEMPLATE, 1 ) ); }
sub undef_ok   { return( def_func( @_, FUNC_UNDEF_OK, 1 ) ); }
sub has_args   { return( def_func( $_[ 0 ], FUNC_ARG_NUM, $_[ 1 ] ) ); }
sub no_args    { return( has_args( @_, 0 ) ); }
sub one_arg    { return( has_args( @_, 1 ) ); }
sub two_args   { return( has_args( @_, 2 ) ); }
sub three_args { return( has_args( @_, 3 ) ); }
sub any_args   { return( has_args( @_, -1 ) ); }


#  These void() and size() are required since they get used internally
#  for certain backwards-compat behaviours/syntax sugars.
#  defined() is required by the test suite, so it stays here too.
my %functions = (
    #  Takes any arg and returns '', useful for hiding expression results.
    void    => ( any_args sub { '' } ),

    size    => ( one_arg
        sub
        {
            return( undef ) unless defined( $_[ 0 ] );
            my $type = Scalar::Util::reftype( $_[ 0 ] );
            return( $type eq 'HASH'   ? scalar( keys( %{$_[ 0 ]} ) ) :
                    $type eq 'ARRAY'  ? scalar( @{$_[ 0 ]} ) :
                    $type eq 'SCALAR' ? length( ${$_[ 0 ]} ) :
                                        length( $_[ 0 ] ) );
        } ),

    defined => ( one_arg undef_ok sub { defined( $_[ 0 ] ) ? 1 : 0 } ),
    );

#print "Content-type: text/plain\n\n" . Data::Dumper::Dumper( \%functions );

my %token_aliases = (
    'foreach'     => 'for',
    'end for'     => 'endfor',
    'endforeach'  => 'endfor',
    'end foreach' => 'endfor',
    'end include' => 'endinclude',
    'els if'      => 'elsif',
    'else if'     => 'elsif',
    'elseif'      => 'elsif',
    'end if'      => 'endif',
    'els unless'  => 'elsunless',
    'else unless' => 'elsunless',
    'elseunless'  => 'elsunless',
    'end unless'  => 'endunless',
    );

#  zero_width => boolean,
#    Zero-width tokens gobble one of the surrounding \n if they're
#    on a line by themselves, preventing "blank-line spam" in the
#    template output.
#  TODO: move syntaxes into substructure to avoid .key hackery.
my %syntaxes = (
    #  Faux values to define the auto opcode generation for local syntaxes.
    '.next_instr'      => LOCAL_SYNTAX,
    '.instr_increment' => 1,
    '.instr'           => {},

    #  Actual syntax definitions.
    'var'     => {
        positional_args => [ 'var' ],
        valid_args      => [ 'var' ],
        },
    'debug'   => {
        positional_args => [ 'type', 'state' ],
        valid_args      => [ 'type', 'state' ],
        zero_width => 1,
        },
    '#'       => {
        zero_width => 1,
        },
    'include' => {
        positional_args => [ 'filename' ],
        zero_width => 1,
        },
    'endinclude'      => {
        zero_width => 1,
        },
    'for'     => {
        positional_args => [ 'iterator', 'set' ],
        valid_args      => [ 'iterator', 'set' ],
        zero_width      => 1,
        },
    'endfor'      => {
        zero_width => 1,
        },
    'if'      => {
        zero_width => 1,
        },
    'unless'  => {
        zero_width => 1,
        },
    'else'      => {
        zero_width => 1,
        },
    'elsif'      => {
        zero_width => 1,
        },
    'elsunless'      => {
        zero_width => 1,
        },
    'endif'      => {
        zero_width => 1,
        },
    'endunless'      => {
        zero_width => 1,
        },
    );

our ( $single_quoted_text_regexp );

$single_quoted_text_regexp = qr/
    \'
    (?:
        #  Quoteless, backslashless text.
        (?> [^\'\\]+ )
        |
        #  Escaped characters.
        (?> (?:\\\\)* \\ . )
    )*
    \'
    /sx;

our ( $double_quoted_text_regexp );

$double_quoted_text_regexp = qr/
    \"
    (?:
        #  Quoteless, backslashless text.
        (?> [^\"\\]+ )
        |
        #  Escaped characters.
        (?> (?:\\\\)* \\ . )
    )*
    \"
    /sx;

our ( $matching_square_brackets_regexp );

$matching_square_brackets_regexp = qr/
    \[
    (?:
        #  Bracketless, quoteless subtext.
        (?> [^\[\]\"\']+ )
        |
        #  Quoted text.
        (??{ $double_quoted_text_regexp }) |
        (??{ $single_quoted_text_regexp })
        |
        #  Expression containing sub-brackets.
        (??{ $matching_square_brackets_regexp })
    )*
    \]
    /sx;

our ( $matching_round_brackets_regexp );

$matching_round_brackets_regexp = qr/
    \(
    (?:
        #  Bracketless, quoteless subtext.
        (?> [^\(\)\"\']+ )
        |
        #  Quoted text.
        (??{ $double_quoted_text_regexp }) |
        (??{ $single_quoted_text_regexp })
        |
        #  Expression containing sub-brackets.
        (??{ $matching_round_brackets_regexp })
    )*
    \)
    /sx;

my $variable_segment_regexp = qr/
    #  avariable or a_varable or avariable[ expr ]
    [a-zA-Z_][a-zA-Z0-9_]*
    (?: $matching_square_brackets_regexp )?
    /sx;
my $capture_variable_segment_regexp = qr/
    ^
    ([a-zA-Z_][a-zA-Z0-9_]*)
    ($matching_square_brackets_regexp)?
    $
    /sx;

my $variable_regexp = qr/
    #  variablesegment.variablesegment.variablesegment etc.
    $variable_segment_regexp
    (?:
        \.
        $variable_segment_regexp
    )*
    /sx;
my $capture_variable_regexp = qr/
    ^
    #  variablesegment.variablesegment.variablesegment etc.
    ($variable_segment_regexp)
    ((?:
        \.
        $variable_segment_regexp
    )*)
    $
    /sx;

my $function_regexp = qr/
    #  abc( expr )
    [a-zA-Z_]+
    $matching_round_brackets_regexp
    /sx;

my $capture_function_regexp = qr/
    ^
    #  abc( expr )
    ([a-zA-Z_]+)
    ($matching_round_brackets_regexp)
    $
    /sx;

my $method_regexp = qr/
    #  variable.method( expr )
    #  variable->method( expr )
    $variable_regexp
    (?: \-\> | \. )
    $function_regexp
    /sx;

my $capture_method_regexp = qr/
    ^
    #  variable.method( expr )
    #  variable->method( expr )
    ($variable_regexp)
    (?: \-\> | \. )
    #  abc( expr )
    ([a-zA-Z_]+)
    ($matching_round_brackets_regexp)
    $
    /sx;

my $literal_number_regexp = qr/
    #  1 or more digits.
    \d+
    #  Optionally a decimal fraction.
    (?: \. \d+ )?
    /sx;

my $unary_operator_regexp = qr/
    (?: \! | not (?=\s) | - )
    /sx;

my $atomic_expr_regexp = qr/
    #  Optionally a unary operator
    (?: $unary_operator_regexp \s* )?
    #  Followed by an atomic value
    (?:
        #  A bracketed sub-expression.
        $matching_round_brackets_regexp
        |
        #  A variable
        $variable_regexp
        |
        #  A function
        $function_regexp
        |
        #  A method
        $method_regexp
        |
        #  A literal number
        $literal_number_regexp
        |
        #  A literal string
        $single_quoted_text_regexp
    )
    /sx;

my $operator_regexp = join( '|', map { "\Q$_\E" } keys( %operators ) );
$operator_regexp = qr/
    (?: $operator_regexp )
    /sx;

my $expr_regexp = qr/
    \s*
    (?:
        #  A sequence of atomic epressions and operators.
        $atomic_expr_regexp
        (?:
            \s+
            $operator_regexp
            \s+
            $atomic_expr_regexp
        )*
    )
    \s*
    /sx;
#            (?:
#                \s+
#                $operator_regexp
#                \s+
#                $atomic_expr_regexp
#            )
#            |
#            (?:
#                \s+
#                \?
#                \s+
#                $atomic_expr_regexp
#                \s+
#                \:
#                $atomic_expr_regexp
#            )

my $capture_expr_op_remain_regexp = qr/
    ^
    \s*
    ($atomic_expr_regexp)
    \s+
    ($operator_regexp)
    \s+
    (.*?)
    \s*
    $
    /sx;

#my $capture_expr_if_else_remain_regexp = qr/
#    ^
#    \s*
#    ($atomic_expr_regexp)
#    \s+ \? \s+
#    ($atomic_expr_regexp)
#    \s+ \: \s+
#    ($atomic_expr_regexp)
#    \s*
#    (.*?)
#    \s*
#    $
#    /sx;

my $capture_expr_comma_remain_regexp = qr/
    ^
    \s*
    ($expr_regexp)
    \s*
    (?:
        (?: , | => )
        \s*
        (.*?)
    )?
    \s*
    $
    /sx;

BEGIN
{
    use Exporter   ();

    $Template::Sandbox::VERSION     = 1.00;
    @Template::Sandbox::ISA         = qw( Exporter );

    @Template::Sandbox::EXPORT      = qw();
    @Template::Sandbox::EXPORT_OK   = qw(
        inconstant
        needs_template
        undef_ok
        has_args no_args
        one_arg two_args three_args any_args
        );
    %Template::Sandbox::EXPORT_TAGS = (
        function_sugar => [ qw(
            inconstant
            needs_template
            undef_ok
            has_args no_args
            one_arg two_args three_args any_args
            ) ],
        );
}

sub _find_local_functions
{
    my ( $self ) = @_;

    return( \%functions ) unless ref( $self );

    $self->{ local_functions } ||= {};
    return( $self->{ local_functions } );
}

sub register_template_function
{
    my $self = shift;
    my ( $local_functions );

    $local_functions = $self->_find_local_functions();

    while( my $name = shift )
    {
        my ( $func );

        $func = shift;

        die "Bad template function '$name' to register_template_function(), " .
            "expected sub ref or 'function_sugar'ed sub ref, got: $func"
            unless ref( $func ) eq 'ARRAY' or ref( $func ) eq 'CODE';

        #  do local $^W = undef; in calling block to suppress.
        warn "Template function '$name' already exists, overwriting."
            if $^W and $local_functions->{ $name };

        $local_functions->{ $name } = $func;
    }
}

sub add_template_function
{
    my $self = shift;

    $self->register_template_function( @_ );
}

sub unregister_template_function
{
    my $self = shift;
    my ( $local_functions );

    $local_functions = $self->_find_local_functions();

    while( my $name = shift )
    {
        warn "Template function '$name' does not exist."
            if $^W and not $local_functions->{ $name };

        delete $local_functions->{ $name };
    }
}

sub delete_template_function
{
    my $self = shift;

    $self->unregister_template_function( @_ );
}

sub _find_local_syntaxes
{
    my ( $self ) = @_;

    return( \%syntaxes ) unless ref( $self );

    $self->{ local_syntaxes } ||= {
        #  Faux values to define the auto opcode generation for local syntaxes.
        #  We use negative values to avoid clash with class-level opcodes.
        '.next_instr'      => -(LOCAL_SYNTAX),
        '.instr_increment' => -1,
        '.instr'           => {},
        };
    $self->{ local_token_aliases } ||= {};
    return( $self->{ local_syntaxes } );
}

sub register_template_syntax
{
    my $self = shift;
    my ( $local_syntaxes );

    $local_syntaxes = $self->_find_local_syntaxes();

    while( my $name = shift )
    {
        my ( $syntax );

        $syntax = shift;

        die "Bad template syntax '$name' to register_template_syntax(), " .
            "expected hash ref, got: $syntax"
            unless ref( $syntax ) eq 'HASH';

        die "Missing compile callback for syntax $name"
             unless $syntax->{ compile_method };
        die "Missing run callback for syntax $name"
             unless $syntax->{ run_method };

        #  do local $^W = undef; in calling block to suppress.
        warn "Template syntax '$name' already exists, overwriting."
            if $^W and $local_syntaxes->{ $name };

        $syntax = { %{$syntax} };

        #  Icky.
        $syntax->{ instr } = $local_syntaxes->{ '.next_instr' };
        $local_syntaxes->{ '.next_instr' } +=
            $local_syntaxes->{ '.instr_increment' };

        $local_syntaxes->{ $name } = { %{$syntax} };
        $local_syntaxes->{ '.instr' }->{ $syntax->{ instr } } = $name;
    }
}

sub add_template_syntax
{
    my $self = shift;

    $self->register_template_syntax( @_ );
}

sub unregister_template_syntax
{
    my $self = shift;
    my ( $local_syntaxes );

    $local_syntaxes = $self->_find_local_syntaxes();

    while( my $name = shift )
    {
        warn "Template syntax '$name' does not exist."
            if $^W and not $local_syntaxes->{ $name };

        delete $local_syntaxes->{ $name };
    }
}

sub delete_template_syntax
{
    my $self = shift;

    $self->unregister_template_syntax( @_ );
}

sub get_valid_singular_constructor_param
{
    my ( $self ) = @_;

    return( qw/filename cache logger template_root
        ignore_module_dependencies/ );
}

sub get_valid_multiple_constructor_param
{
    my ( $self ) = @_;

    return( qw/copy_global_functions template_function template_syntax/ );
}

#  TODO: implement these constructor options:
#    -
sub new
{
    my $this = shift;
    my ( $self, $class, %param, %valid_singular, %valid_multiple );

    $self = {};
    $class = ref( $this ) || $this;
    bless $self, $class;

    %valid_singular =
        map { $_ => 1 } $self->get_valid_singular_constructor_param();
    %valid_multiple =
        map { $_ => 1 } $self->get_valid_multiple_constructor_param();

    #  Read remaining args.
    %param = ();
    while( my $param_name = shift )
    {
        my $param_value = shift;

        if( $valid_singular{ $param_name } )
        {
            $param{ $param_name } = $param_value;
        }
        elsif( $valid_multiple{ $param_name } )
        {
            $param{ $param_name } ||= [];
            push @{$param{ $param_name }}, $param_value;
        }
        else
        {
            $self->error( "Unknown constructor param: '$param_name'" );
        }
    }

    $self->{ phase } = 'initialization';
    $self->initialize( { %param } );
    $self->{ phase } = 'post-initialization';

    return( $self );
}

sub initialize
{
    my ( $self, $param ) = @_;

    #  Do this early in case anything needs logging.
    if( exists $param->{ logger } )
    {
        $self->{ logger } = $param->{ logger };
    }
    else
    {
        $self->{ logger } = Log::Any->get_logger();
    }

    #  For the paranoid, to prevent other code changing them after
    #  we initialize.
    $self->{ local_functions } = Storable::dclone( \%functions )
        if exists $param->{ copy_global_functions };

    if( exists $param->{ template_function } )
    {
        foreach my $arg ( @{$param->{ template_function }} )
        {
            $self->register_template_function( @{$arg} );
        }
    }
    if( exists $param->{ template_syntax } )
    {
        foreach my $arg ( @{$param->{ template_syntax }} )
        {
            $self->register_template_syntax( @{$arg} );
        }
    }

    $self->{ ignore_module_dependencies } =
        $param->{ ignore_module_dependencies }
        if exists $param->{ ignore_module_dependencies };

    $self->{ cache }  = $param->{ cache }
        if exists $param->{ cache };
    $self->set_template_root( $param->{ template_root } )
        if exists $param->{ template_root };
    $self->set_template( $param->{ filename } )
        if exists $param->{ filename };

    $self->{ vars }   = {};
    $self->{ debug }  = {};
}

sub set_template_root
{
    my ( $self, $dir ) = @_;

    $self->{ template_root } = $dir;
}

sub get_template_candidates
{
    my ( $self, $filename, $current_dir ) = @_;

    return( $self->{ template_root } . '/' . $filename );
}

sub get_include_candidates
{
    my ( $self, $filename, $current_dir ) = @_;

    return( $current_dir . '/' . $filename );
}

sub find_template
{
    my ( $self, $filename, $current_dir ) = @_;
    my ( @candidates );

    @candidates = $self->get_template_candidates( $filename, $current_dir );
    foreach my $candidate ( @candidates )
    {
        return( $candidate ) if -e $candidate;
    }

    $self->error( "Unable to find matching template from candidates:\n" .
        join( "\n", @candidates ) );
}

sub find_include
{
    my ( $self, $filename, $current_dir ) = @_;
    my ( @candidates );

    @candidates = $self->get_include_candidates( $filename, $current_dir );

    foreach my $candidate ( @candidates )
    {
        return( $candidate ) if -e $candidate;
    }

    $self->error( "Unable to find matching include from candidates:\n" .
        join( "\n", @candidates ) );
}

sub cache_key
{
    my ( $self, $keys ) = @_;
    my ( $digester );

    $digester = Digest->new( 'MD5' );

    {
        local $Storable::canonical = 1;
        $digester->add( Storable::nfreeze( $keys ) );
    }

    return( $digester->hexdigest() );
}

sub get_additional_dependencies
{
    my ( $self ) = @_;

    return( undef );
}

sub set_template
{
    my ( $self, $filename, $defines ) = @_;
    my ( $cache_key );

#my $start_time = Time::HiRes::time();

    #  Shallow copy is safe, keys/values should only be scalars.
    $defines = $defines ? { %{$defines} } : {};

    #  PAGE is the leaf filename of what they _asked_ for.
    if( $filename =~ /^(.*)\/(.*?)$/ )
    {
        $defines->{ PAGE } = $2;
    }
    else
    {
        $defines->{ PAGE } = $filename;
    }

    $self->{ filename }    = $self->find_template( $filename, Cwd::cwd() );
    $defines->{ FILENAME } = $self->{ filename };
    $defines->{ DIR }      = ( $filename =~ /^(.*)\/(.*?)$/ ) ? $1 : '';

    $self->{ defines }        = $defines;
    $self->{ special_values } = {};

    if( $self->{ cache } )
    {
#my $fetchstart = Time::HiRes::time();
        #  $defines at this stage includes all unique compile-time
        #  parameters that effect the final compiled template, this
        #  is more than just the filename, so we need to generate
        #  a simple string key from multiple inputs.
        $cache_key = $self->cache_key( $defines );
        $self->{ template } = $self->{ cache }->get( $cache_key );
#warn "Cache fetch: " . $self->{ filename } . " " .
#  sprintf( "%.6fs", Time::HiRes::time() - $fetchstart );
    }

    unless( $self->{ template } )
    {
        my ( $compiletime );

        $compiletime = time();  #  Before the compile, to be safe.

        $self->{ dependencies } = $self->get_additional_dependencies() || [];

        #  If we're caching, the validity of the cache depends on the
        #  last-modified of the template module as well as the template
        #  files, unless we've been told to ignore it.
        if( $self->{ cache } and not $self->{ ignore_module_dependencies } )
        {
            my ( $class_handle );

            $class_handle = Class::Handle->new( ref( $self ) );
            push @{$self->{ dependencies }},
                #  TODO: Ew, ugly and non-portable.
                grep { defined( $_ ) }
                map  { s/\:\:/\//g; s/$/\.pm/; $INC{ $_ }; }
                $class_handle->self_and_super_path();
        }

        $self->{ template } =
            $self->_read_template( $self->{ filename }, $defines );

        $self->_compile_template();

        if( $self->{ cache } )
        {
            #  If they're using Cache::CacheFactory we can make use of
            #  the dependencies and created at timestamps, if not we
            #  fall back on the basic Cache::Cache style API.
#  TODO: unroll the isa() to initialize
#  TODO: wrap compat cache behaviour with our own dependencies checking.
            if( $self->{ cache }->isa( 'Cache::CacheFactory' ) )
            {
                $self->{ cache }->set(
                    key          => $cache_key,
                    data         => $self->{ template },
                    dependencies => $self->{ dependencies },
                    created_at   => $compiletime,
                    );
            }
            else
            {
                $self->{ cache }->set( $cache_key, $self->{ template } );
            }
        }
    }

#CORE::warn( "set_template( $filename ) took " .
#  sprintf( "%.3f", Time::HiRes::time() - $start_time ) . "s" );
}

#  TODO: split/merge parts from set_template() above.
sub set_template_string
{
    my ( $self, $template_string, $defines ) = @_;
    my ( $cache_key );

#my $start_time = Time::HiRes::time();

    #  Shallow copy is safe, keys/values should only be scalars.
    $defines = $defines ? { %{$defines} } : {};

    #  Hmmm.
    $defines->{ PAGE } = 'string';

    #  Erk.  Better way of making this cacheable surely?
#  TODO:  erk, gets copied into pos for every instr.
    $self->{ filename }    = 'string:///' . $template_string;
    $defines->{ FILENAME } = $self->{ filename };
    $defines->{ DIR }      = Cwd::cwd();

    $self->{ defines }        = $defines;
    $self->{ special_values } = {};

    if( $self->{ cache } )
    {
#my $fetchstart = Time::HiRes::time();
        #  $defines at this stage includes all unique compile-time
        #  parameters that effect the final compiled template, this
        #  is more than just the filename, so we need to generate
        #  a simple string key from multiple inputs.
        $cache_key = $self->cache_key( $defines );
        $self->{ template } = $self->{ cache }->get( $cache_key );
#warn "Cache fetch: " . $self->{ filename } . " " .
#  sprintf( "%.6fs", Time::HiRes::time() - $fetchstart );
    }

    unless( $self->{ template } )
    {
        my ( $compiletime );

        $compiletime = time();  #  Before the compile, to be safe.

        $self->{ dependencies } = $self->get_additional_dependencies() || [];

        #  If we're caching, the validity of the cache depends on the
        #  last-modified of the template module as well as the template
        #  files, unless we've been told to ignore it.
        if( $self->{ cache } and not $self->{ ignore_module_dependencies } )
        {
            my ( $class_handle );

            $class_handle = Class::Handle->new( ref( $self ) );
            push @{$self->{ dependencies }},
                #  TODO: Ew, ugly and non-portable.
                grep { defined( $_ ) }
                map  { s/\:\:/\//g; s/$/\.pm/; $INC{ $_ }; }
                $class_handle->self_and_super_path();
        }

        $self->{ template } =
            $self->_read_template_from_string( $template_string, $defines );

        $self->_compile_template();

        if( $self->{ cache } )
        {
            #  If they're using Cache::CacheFactory we can make use of
            #  the dependencies and created at timestamps, if not we
            #  fall back on the basic Cache::Cache style API.
#  TODO: unroll the isa() to initialize
#  TODO: wrap compat cache behaviour with our own dependencies checking.
            if( $self->{ cache }->isa( 'Cache::CacheFactory' ) )
            {
                $self->{ cache }->set(
                    key          => $cache_key,
                    data         => $self->{ template },
                    dependencies => $self->{ dependencies },
                    created_at   => $compiletime,
                    );
            }
            else
            {
                $self->{ cache }->set( $cache_key, $self->{ template } );
            }
        }
    }

#CORE::warn( "set_template( $filename ) took " .
#  sprintf( "%.3f", Time::HiRes::time() - $start_time ) . "s" );
}

sub _error_message
{
    my $self = shift;
    my ( $error, $pos );

    $error = join( '', @_ );
    $error = "Template " . ( $self->{ phase } ? $self->{ phase } . ' ' : '' ) .
        "error: $error";
    $pos = $self->{ current_pos };
    if( $pos )
    {
        my ( $files );

        if( $self->{ template } and
            ( ref( $self->{ template } ) eq 'HASH' ) and
            $self->{ template }->{ files } )
        {
            $files = $self->{ template }->{ files };
        }
        elsif( $self->{ files } )
        {
            $files = $self->{ files };
        }
        else
        {
            $files = [];
        }
        $error .= " at line $pos->[ 1 ], char $pos->[ 2 ] of " .
            "'$files->[ $pos->[ 0 ] ]'";
        if( $self->{ pos_stack } )
        {
            my ( $first );

            $first = 1;
            foreach $pos ( @{$self->{ pos_stack }} )
            {
                $error .= "\n  called from " .
                    "line $pos->[ 1 ], char $pos->[ 2 ] of " .
                    "'$files->[ $pos->[ 0 ] ]'"
                    unless $first;
                $first = 0;
            }
        }
    }
    return( $error );
}

sub log_error
{
    my ( $self, $message ) = @_;

    $self->{ logger }->error( $message ) if $self->{ logger };
}

sub log_warning
{
    my ( $self, $message ) = @_;

    $self->{ logger }->warning( $message ) if $self->{ logger };
}

sub error
{
    my $self = shift;
    my ( $message );

    $message = $self->_error_message( @_ );
    $self->log_error( $message );
    $self->fatal_exit( $message );
}

sub fatal_exit
{
    my ( $self, $message ) = @_;

    die $message;
}

sub warning
{
    my $self = shift;
    my ( $message );

    $message = $self->_error_message( @_ );
    $self->log_warning( $message );
}

sub add_var
{
    my ( $self, $var, $value ) = @_;

    $self->{ vars }->{ $var } = $value;
}

sub add_vars
{
    my ( $self, $vars ) = @_;

    foreach my $var ( keys( %{$vars} ) )
    {
        $self->{ vars }->{ $var } = $vars->{ $var };
    }
}

sub _var_value
{
    my ( $self, $var ) = @_;

    return( $self->{ vars }->{ $var } );
}

sub merge_var
{
    my ( $self, $var, $value, $ref ) = @_;

    $ref = $self->{ vars } unless $ref;

#CORE::warn( "merge_var( ",
#  Data::Dumper::Dumper( $var ), ", ",
#  Data::Dumper::Dumper( $value ), ", ",
#  Data::Dumper::Dumper( $ref->{ $var } ), ")\n" );

    unless( exists( $ref->{ $var } ) and ref( $value ) and
        ( ref( $value ) eq 'HASH' or ref( $value ) eq 'ARRAY' ) )
    {
#CORE::warn( "Doesn't exist, setting\n" );
        $ref->{ $var } = $value;
        return;
    }

    if( ref( $value ) eq 'HASH' )
    {
        foreach my $key ( keys( %{$value} ) )
        {
            $self->merge_var( $key, $value->{ $key }, $ref->{ $var } );
        }
    }
    elsif( ref( $value ) eq 'ARRAY' )
    {
        if( ref( $ref->{ $var } ) eq 'ARRAY' )
        {
            push @{$ref->{ $var }}, @{$value};
        }
        else
        {
            #  Ew, trying to merge array with non-array?
            #  TODO: error?
            $ref->{ $var } = $value;
        }
    }
}

sub merge_vars
{
    my ( $self, $vars ) = @_;

    foreach my $var ( keys( %{$vars} ) )
    {
        $self->merge_var( $var, $vars->{ $var } );
    }
}

sub _escape_string
{
    my ( $self, $string ) = @_;

    $string =~ s/\'/\\\'/g;
    return( $string );
}

sub _define_value
{
    my ( $self, $defines, $define, $default, $quote ) = @_;

#$self->warning( "replacing define '$define'" );
    $define = defined( $defines->{ $define } ) ?
        $defines->{ $define } :
        ( defined( $default ) ?
          $default :
          "[undefined preprocessor define '$2']" );

    $define = "'" . $self->_escape_string( $define ) . "'" if $quote;

    return( $define );
}

sub _replace_defines
{
    my ( $self, $template_content, $defines ) = @_;

    #  Replace any preprocessor defines.
    1 while $template_content =~ s/\$\{('?)([A-Z0-9_]+)(?::([^\}]*))?\1\}/
        $self->_define_value( $defines, $2, $3, $1 )/gex;

    return( $template_content );
}

sub _read_template
{
    my ( $self, $filename, $defines ) = @_;
    my ( $fh, $template );

    push @{$self->{ dependencies }}, $filename;

    $fh = new IO::File "< $filename" or confess "Unable to read $filename: $!";
    {
        local $/;
        $template = <$fh>;
    }
    $fh->close;

    #  Replace any preprocessor defines.
    $self->_replace_defines( $template, $defines );

#$self->warning( "Template becomes: $template" ) if $filename =~ /item_blueprint_activity_times_manufacturing.html/;

    return( $template );
}

sub _read_template_from_string
{
    my ( $self, $template, $defines ) = @_;

    #  Replace any preprocessor defines.
    $self->_replace_defines( $template, $defines );

    return( $template );
}

#  Looks for combination of positional and named parameters to a syntax
#  token and returns a hashref of named parameters.
sub _parse_args
{
    my ( $self, $args, $type ) = @_;
    my ( $count, %param, @words, @pos_param, @keyword_param, $instr,
         @positions, @valid, $syntax );

    #  Heeeello hackery.
    $args = "iterator=\"$1\" set=\"$2\""
        if $type eq 'for' and $args =~ /^(.*) in (.*)$/;

    $syntax = $self->{ local_syntaxes }->{ $type } || $syntaxes{ $type };

    @positions = $syntax->{ positional_args } ?
        @{$syntax->{ positional_args }} : ();
    @valid     = $syntax->{ valid_args } ? @{$syntax->{ valid_args }} : undef;

    %param = ();

    @words = split( /\s+/, $args );
    #  Merge quoted args.
#  TODO: rename instr to in_str for semantic clarity vs "instr"uction.
    $instr = 0;
    for( $count = 0; $count <= $#words; $count++ )
    {
        if( $instr )
        {
            $instr = 0 if $words[ $count ] =~ /\"$/;
            $words[ $count - 1 ] .= ' ' . $words[ $count ];
            @words =
                ( @words[ 0..$count - 1 ], @words[ $count + 1..$#words ] );
            $count--;
        }
        else
        {
            next unless $words[ $count ] =~ /^\"/ or $words[ $count ] =~ /=\"/;
            next if $words[ $count ] =~ /\"$/;
            $instr = 1;
        }
    }
    #  Merge $[...] url args.
    $instr = 0;
    for( $count = 0; $count <= $#words; $count++ )
    {
        if( $instr )
        {
            $instr = 0 if $words[ $count ] =~ /\]\=/;
            $words[ $count - 1 ] .= ' ' . $words[ $count ];
            @words =
                ( @words[ 0..$count - 1 ], @words[ $count + 1..$#words ] );
            $count--;
        }
        else
        {
            next unless $words[ $count ] =~ /^\$\[/;
            next if $words[ $count ] =~ /\]\=/;
            $instr = 1;
        }
    }

#my ( $warning_happened );
#local $SIG{__WARN__} = sub { $warning_happened = 1; print "Content-type: text/plain\n\n"; };

    #  Split into positional parameters and keyword paramaters.
    for( $count = 0; $count <= $#words; $count++ )
    {
        last if $words[ $count ] =~ /=/;
    }

    @pos_param     = $count            ? @words[ 0..$count - 1 ]   : ();
    @keyword_param = $count <= $#words ? @words[ $count..$#words ] : ();

    #  Squidge any "overshoot" positional param onto the final pos param.
    @pos_param = ( @pos_param[ 0..$#positions - 1 ],
        join( ' ', @pos_param[ $#positions..$#pos_param ] ) )
        if $#pos_param > $#positions;

    $count = 0;
    foreach my $word ( @pos_param )
    {
        $word = $1 if $word =~ /^\"(.*)\"$/;
        $param{ $positions[ $count++ ] } = $word;
#print "parse_args( $args, $type )\n" if $warning_happened;
    }

    foreach my $word ( @keyword_param )
    {
        my ( $keyword, $value );

        ( $keyword, $value ) = split( /=/, $word, 2 );

        unless( defined( $value ) )
        {
            #  TODO: error here.
            $self->error( "Undefined value for keyword: '$keyword' on " .
                "parse_args( $args, $type )" );
        }

        $value = $1 if $value =~ /^\"(.*)\"$/;

        #  TODO: validate arg names.
        $param{ $keyword } = $value;
    }

#$self->warning( "Read param for <: $type $args :> as: ", Data::Dumper::Dumper( \%param ), "\n" ) if $type eq 'include';

    return( { %param } );
}

sub _compile_template
{
    my ( $self ) = @_;
    my ( $i, @hunks, @files, @pos_stack, @nest_stack, @compiled, %includes,
         %trim, $trim_next, %file_numbers, @define_stack,
         $local_syntaxes, $local_token_aliases, $local_syntax_regexp );

    @files           = ( $self->{ filename } );
    %file_numbers    = ( $self->{ filename } => 0 );
    $self->{ files } = \@files;
    
    #  Stack of what position in which file we're currently at.
    @pos_stack    = ( [ $file_numbers{ $self->{ filename } }, 1, 1 ] );
    #  Stack of what defines are available.
    @define_stack = ( $self->{ defines } );
    #  Stack of unclosed block-level statements.
    @nest_stack   = ();
    #  The tokenized/compiled template.
    @compiled     = ();
    #  Files we're currently including.
    %includes     = ( $self->{ filename } => 1 );
    #  Stuff we're going to trim later.
    %trim         = ();

    $local_token_aliases = $self->{ local_token_aliases } || {};
    $local_syntaxes      = $self->{ local_syntaxes } || {};

#  TODO: class-level syntaxes
#  TODO: split into class/instance versions and unroll to construct time?
#  TODO: or generate-on-demand but class/instance copy invalidated on change.
    #  Egads!
    $local_syntax_regexp = join( ' | ',
        map { join( ' \s+ ', map { '\Q' . $_ . '\E' } split( /\s+/, $_ ) ) }
        grep( /^[^\.]/,
            keys( %{$local_token_aliases} ), keys( %{$local_syntaxes} ) ) );
    $local_syntax_regexp = ' | ' . $local_syntax_regexp
        if $local_syntax_regexp;
#  TODO: qr// it?  need to benchmark

    @hunks = split( /(?=<:)/, $self->{ template }, -1 );
    delete $self->{ template };

    $self->{ pos_stack } = \@pos_stack;
    $self->{ phase }     = 'compile';

my ( $dumpme );
    for( $i = 0; $i <= $#hunks; $i++ )
    {
        my ( $hunk, $pos, $lines, $queue_pos, $last, $next );

        $hunk = $hunks[ $i ];

        $pos = [ @{$pos_stack[ 0 ]}[ 0..2 ] ];
        $self->{ current_pos } = $pos;

#  TODO: now that matching regexp is variable, unroll qr// of it outside loop.
        if( $hunk =~ /^<: \s*
            (
              var | expr |
              (?:if|unless) | else? \s* (?:if|unless) | else |
              end \s* (?:if|unless) |
              for(?:each)? | end \s* for(?:each)? |
              include | end \s* include |
              \# |
              debug
              $local_syntax_regexp
            ) \s+ (.*?) \s* :> (.+)? $/sx )
        {
            my ( $token, $syntax, $args, $rest );

            $token = $1;
            $args  = $2;
            $rest  = $3;

            #  TODO:  still possible?  What triggers it?
            if( $args =~ /<:/ )
            {
                #  error, unclosed token?
                $self->error( "unexepected <:, possibly unterminated :>" );
            }

            if( defined( $rest ) )
            {
                $hunk =~ s/:>(?:.*)$/:>/s;
                @hunks = ( @hunks[ 0..$i - 1 ], $hunk, $rest,
                    @hunks[ $i + 1..$#hunks ] );
                $next = $i + 1;
            }

            $token =~ s/\s+/ /g;

            $token = $local_token_aliases->{ $token }
                if $local_token_aliases->{ $token };
            $token = $token_aliases{ $token }
                if $token_aliases{ $token };
            $syntax = $local_syntaxes->{ $token } || $syntaxes{ $token };

#if( $includes{ 'user/top_nav' } and not $dumpme )
#{
#use CGI;
#print CGI->header('text/plain');
#$dumpme = 1;
#}
            #  Fudge things a little so that flow-control tokens
            #  on a line by themselves don't produce a bunch of
            #  empty lines in the output.
            #  Are we a zero-width token on a line by itself?
            if( $syntax->{ zero_width } and
                $#compiled >= 0 and $i < $#hunks and
                ( ( $compiled[ $#compiled ]->[ 0 ] == LITERAL and
                    $compiled[ $#compiled ]->[ 2 ] =~ /\n\ *$/ ) or
                  ( $compiled[ $#compiled ]->[ 0 ] == CONTEXT_PUSH ) ) and
                $hunks[ $i + 1 ] =~ /^\n\ */ )
            {
                $trim_next = 1;
            }
            else
            {
                $trim_next = 0;
            }

            if( $syntax->{ compile_method } )
            {
                my ( $compiler, $opcode );

                $args = $self->_parse_args( $args, $token );

                $compiler = $syntax->{ compile_method };
                $opcode   = $syntax->{ opcode };
                {
                    no strict 'refs';
                    $args = $self->$compiler( $token, $pos, $args );
                }
                push @compiled, [ $opcode, $pos, $args ] if defined $args;
            }
            elsif( $token eq 'debug' )
            {
                $args = $self->_parse_args( $args, $token );

                $args = 0 unless scalar( keys( %{$args} ) );

                push @compiled,
                    [ DEBUG, $pos, $args ];
            }
            elsif( $token eq 'expr' or $token eq 'var' )
            {
                push @compiled,
                    [ EXPR, $pos, $self->_compile_expression( $args ), 0 ];
            }
            elsif( $token eq 'if' or $token eq 'unless' )
            {
                push @compiled,
                    [ JUMP_IF, $pos, undef,
                       $self->_compile_expression( $args ),
                       $token eq 'if' ? 1 : 0 ];
                unshift @nest_stack, [ 'if', $#compiled ];
            }
            elsif( $token eq 'elsif' or $token eq 'elsunless' )
            {
                if( $#nest_stack == -1 or
                    ( $nest_stack[ 0 ][ 0 ] ne 'if' and
                      $nest_stack[ 0 ][ 0 ] ne 'elsif' ) )
                {
                    $self->error( "elsif found without opening if or elsif" );
                }
                #  Closing jump of previous block.
                push @compiled,
                    [ JUMP, $pos, undef ];
                push @compiled,
                    [ JUMP_IF, $pos, undef,
                       $self->_compile_expression( $args ),
                       $token eq 'elsif' ? 1 : 0 ];
                #  Now, update jump address of previous if/elsif
                $compiled[ $nest_stack[ 0 ][ 1 ] ][ 2 ] =
                    $#compiled;
                unshift @nest_stack, [ 'elsif', $#compiled ];
            }
            elsif( $token eq 'else' )
            {
                if( $#nest_stack == -1 or
                    ( $nest_stack[ 0 ][ 0 ] ne 'if' and
                      $nest_stack[ 0 ][ 0 ] ne 'elsif' ) )
                {
                    $self->error( "else found without opening if or elsif" );
                }
                #  Closing jump of previous block.
                push @compiled,
                    [ JUMP, $pos, undef ];
                #  Now, update jump address of previous if/elsif
                $compiled[ $nest_stack[ 0 ][ 1 ] ][ 2 ] =
                    $#compiled + 1;
                unshift @nest_stack, [ 'else', $#compiled + 1 ];
            }
            elsif( $token eq 'endif' or $token eq 'endunless' )
            {
                if( $#nest_stack == -1 or
                    ( $nest_stack[ 0 ][ 0 ] ne 'if' and
                      $nest_stack[ 0 ][ 0 ] ne 'elsif' and
                      $nest_stack[ 0 ][ 0 ] ne 'else' ) )
                {
                    $self->error(
                        "endif found without opening if, elsif or else" );
                }

                #  Update jump address of previous if/elsif
                $compiled[ $nest_stack[ 0 ][ 1 ] ][ 2 ] =
                    $#compiled + 1
                    unless $nest_stack[ 0 ][ 0 ] eq 'else';

                while( $#nest_stack != -1 )
                {
                    my ( $last );

                    $last = shift @nest_stack;

                    if( $last->[ 0 ] eq 'if' )
                    {
                        #  It's our opening if, stop popping.
                        last;
                    }
                    elsif( $last->[ 0 ] eq 'elsif' or $last->[ 0 ] eq 'else' )
                    {
                        #  Need to update the jump address of the closing
                        #  jump of the block _prior_ to this elsif/else.
                        $compiled[ $last->[ 1 ] - 1 ][ 2 ] = $#compiled + 1;
                    }
                    else
                    {
                        $self->error(
                            "nesting stack appears to be corrupted" );
                    }
                }
            }
            elsif( $token eq 'for' )
            {
                my ( $iterator, $set );

                $args = $self->_parse_args( $args, $token );

                #  Extract the var name.
                $iterator = $args->{ iterator };
                delete $args->{ iterator };
                $set      = $args->{ set };
                delete $args->{ set };
                $args = 0 unless scalar( keys( %{$args} ) );

                push @compiled,
                    [ FOR, $pos, undef, $iterator,
                      $self->_compile_expression( $set ),
                      $args ];
                unshift @nest_stack, [ FOR, $#compiled ];
            }
            elsif( $token eq 'endfor' )
            {
                my ( $last );

                if( $#nest_stack == -1 or
                    $nest_stack[ 0 ][ 0 ] ne FOR )
                {
                    $self->error(
                         "endfor found without opening for" );
                }

                $last = shift @nest_stack;

                #  Grab our iterator, set and args from the opening for
                push @compiled,
                    [ END_FOR, $pos, $last->[ 1 ] + 1,
                      $compiled[ $last->[ 1 ] ][ 3 ],
                      $compiled[ $last->[ 1 ] ][ 4 ],
                      $compiled[ $last->[ 1 ] ][ 5 ] ];
                #  Update jump address of opening for.
                $compiled[ $last->[ 1 ] ][ 2 ] = $#compiled + 1;
            }
            elsif( $token eq 'include' )
            {
                my ( $filename, $inc_template, @inc_hunks, %defines );

                #  We support var renaming:
                #  ie: <: include pagerwidget offset=param.offset
                #         total=results.__size__ pagesize=param.n :>

                $args = $self->_parse_args( $args, 'include' );

                #  Extract the filename.
                #  If the filename is empty-string then we ignore the
                #  include statement, allowing us to do things like
                #  <: include ${DEFINE} :> without knowing if the define
                #  exists or not.
                if( $filename = $args->{ filename } )
                {
                    delete $args->{ filename };

                    $filename = $self->find_include( $filename,
                        $define_stack[ 0 ]->{ DIR } );

                    if( $includes{ $filename } )
                    {
                        $self->error(
                            "recursive include of $filename" );
                    }

                    #  Parse out any defines.
                    %defines = (
                        FILENAME => $filename,
                        DIR      => ( $filename =~ /^(.*)\/(.*?)$/ ) ? $1 : '',
                        );
                    foreach my $key (
                        grep { $_ eq uc( $_ ) } keys( %{$args} ) )
                    {
                        $defines{ $key } = $args->{ $key };
                        delete $args->{ $key };
                    }
                    $args = { map
                        { $_ => $self->_compile_expression( $args->{ $_ } ) }
                        keys( %{$args} ) };
                    $args = 0 unless scalar( keys( %{$args} ) );

                    foreach my $define ( keys( %{$define_stack[ 0 ]} ) )
                    {
                        $defines{ $define } = $define_stack[ 0 ]->{ $define }
                            unless exists $defines{ $define };
                    }
                    unshift @define_stack, { %defines };

                    $includes{ $filename } = 1;
                    $inc_template =
                        $self->_read_template( $filename, \%defines );
                    $inc_template =~ s/\n$//;
                    @inc_hunks = split( /(?=<:)/, $inc_template, -1 );
                    $inc_template = 0;

                    @hunks = ( @hunks[ 0..$i ], @inc_hunks,
                        '<: endinclude :>',
                        @hunks[ $i + 1..$#hunks ] );

                    push @compiled,
                        [ CONTEXT_PUSH, $pos, $args ];
                    unless( exists( $file_numbers{ $filename } ) )
                    {
                        push @files, $filename;
                        $file_numbers{ $filename } = $#files;
                    }
                    $queue_pos = [ $file_numbers{ $filename }, 1, 1 ];
                }
            }
            elsif( $token eq 'endinclude' )
            {
                #  <: endinclude :> is a faux-token, it never gets read
                #  in from a template (isn't valid syntax even), but gets
                #  inserted to mark the end of the inserted hunks from
                #  an <: include :>
                my ( $last );

                if( $#pos_stack == 0 )
                {
                    $self->error(
                        "endinclude found while not within an include" );
                }

                $last = shift @pos_stack;
                delete $includes{ $last->[ 0 ] };
                shift @define_stack;
                push @compiled,
                    [ CONTEXT_POP, $pos ];
                next;  #  So we don't update pos with this faux-token.
            }
            elsif( $token eq '#' )
            {
                #  We're a comment, don't compile it.
            }
            else
            {
                $self->error(
                    "unrecognised token ($token)" );
            }
        }
        else
        {
            #  We're a literal unless we're a malformed token
            $self->error(
                "unrecognised token ($hunk)" ) if $hunk =~ /^<:/;
            if( length( $hunk ) )
            {
                push @compiled, [ LITERAL, $pos, $hunk ];
                $trim{ $#compiled } = 1 if $trim_next;
            }
            $trim_next = 0;
        }

        #  Update pos.
        #  TODO: adjust for any trimmage.
        $lines = $#{ [ $hunk =~ /(\n)/g ] } + 1;
        $pos_stack[ 0 ][ 1 ] += $lines;
        if( $lines )
        {
            $hunk =~ /\n(.*?)$/m;
            $pos_stack[ 0 ][ 2 ] = length( $1 ) + 1;
        }
        else
        {
            $pos_stack[ 0 ][ 2 ] += length( $hunk );
        }

        unshift @pos_stack, $queue_pos if $queue_pos;
    }

    if( $#nest_stack != -1 )
    {
        $self->error(
            "unterminated if or for block" );
    }

    if( $#pos_stack != 0 )
    {
        $self->error(
            "include stack not empty, corrupted?" );
    }

    #  TODO: scan for undef jump addresses.

    foreach my $addr ( keys( %trim ) )
    {
        $self->error(
            "trim on non-literal, trim-stack corrupted?" )
            unless $compiled[ $addr ]->[ 0 ] == LITERAL;
        $compiled[ $addr ]->[ 2 ] =~ s/^\n//;
    }

    #  We're done.
    $self->{ template } = $self->optimize_template(
        {
            program => [ @compiled ],
            files   => [ @files ],
        } );
    delete $self->{ current_pos };
    delete $self->{ pos_stack };
    delete $self->{ files };
    delete $self->{ phase };

#$dumpme = 1;
#use CGI;
#print CGI->header('text/plain');

if( $dumpme )
{
print "\n----\n" . $self->_dumpable_template() . "----\n";
exit(0);
}
}

#  Warning, pass-by-ref: modifies $template.
sub optimize_template
{
    my ( $self, $template ) = @_;
    my ( $program, @nest_stack, %deletes, @function_table, %function_index );

    #  Optimization pass:
    #    TODO: unroll constant low-count fors?

    $program = $template->{ program };

    #  Void-wrap assign expressions.
    for( my $i = 0; $i <= $#{$program}; $i++ )
    {
        my ( $expr );

        #  Are we an EXPR instr and is our expr an OP_TREE expr and op '='?
        next unless $program->[ $i ]->[ 0 ] == EXPR and
                    $program->[ $i ]->[ 2 ]->[ 0 ] == OP_TREE and
                    $program->[ $i ]->[ 2 ]->[ 2 ] eq '=';

        $program->[ $i ]->[ 3 ] = 1;
    }


    #  Fold constant expr into constant instr.
    for( my $i = 0; $i <= $#{$program}; $i++ )
    {
        #  Are we an EXPR instr and is our expr a LITERAL expr?
        next unless $program->[ $i ]->[ 0 ] == EXPR and
                    $program->[ $i ]->[ 2 ]->[ 0 ] == LITERAL;

#warn "Folding literal expr $i (val: " . $program->[ $i ]->[ 2 ]->[ 2 ] . ") (orig: " . $program->[ $i ]->[ 2 ]->[ 1 ] . ") into literal instr.";

        $program->[ $i ]->[ 0 ] = LITERAL;
        $program->[ $i ]->[ 2 ] = $program->[ $i ]->[ 2 ]->[ 2 ];
    }


    #  Fold constant JUMP_IF into JUMP or delete.
    %deletes    = ();
    for( my $i = 0; $i <= $#{$program}; $i++ )
    {
        my ( $line, $value );

        $line = $program->[ $i ];
        next unless $line->[ 0 ] == JUMP_IF and
                    $line->[ 3 ]->[ 0 ] == LITERAL;

        $value = $self->_eval_expression( $line->[ 3 ], 1 );
        $value = not $value if $line->[ 4 ];

        if( $value )
        {
            #  Always true, fold it into a JUMP.
#warn "Folding constant JUMP_IF into JUMP.";
            $program->[ $i ] = [ JUMP, $line->[ 1 ], $line->[ 2 ] ];
        }
        else
        {
            #  Always false, remove the JUMP.
#warn "Folding constant JUMP_IF into no-op.";
            $deletes{ $i } = 1;
        }
    }
    $self->delete_instr( $program, keys( %deletes ) );


    #  Trim empty context pushes (TODO: that have no assigns in top level)
    %deletes    = ();
    @nest_stack = ();
    for( my $i = 0; $i <= $#{$program}; $i++ )
    {
        if( $program->[ $i ]->[ 0 ] == CONTEXT_PUSH )
        {
            unshift @nest_stack, $i;
            $deletes{ $i } = 1 unless $program->[ $i ]->[ 2 ];
            next;
        }
        if( $program->[ $i ]->[ 0 ] == CONTEXT_POP )
        {
            my ( $match );

            $match = shift @nest_stack;
            $deletes{ $i } = 1 if $deletes{ $match };
            next;
        }
    }
    $self->delete_instr( $program, keys( %deletes ) );

    #  Now scan for adjacent literals to merge where the second
    #  isn't a jump target.
    %deletes = ();
    OUTER: for( my $i = $#{$program}; $i > 0; $i-- )
    {
        #  Are both ourself and our previous instr a literal?
        next if $program->[ $i ]->[ 0 ]     != LITERAL or
                $program->[ $i - 1 ]->[ 0 ] != LITERAL;

        #  Do any jumps lead to the second literal?
        for( my $j = 0; $j <= $#{$program}; $j++ )
        {
            my ( $j_instr );

            $j_instr = $program->[ $j ]->[ 0 ];
            next unless $j_instr == JUMP or
                        $j_instr == JUMP_IF or
                        $j_instr == FOR or
                        $j_instr == END_FOR;
            next OUTER if $program->[ $j ]->[ 2 ] == $i;
        }

#warn "Merging literal $i to previous.";
#warn "Merging literals [" . $program->[ $i - 1 ]->[ 2 ] . "] and [" . $program->[ $i ]->[ 2 ] . "]";

        #  Ok, no reason for us to remain apart, let's get married.
        $program->[ $i - 1 ]->[ 2 ] .= $program->[ $i ]->[ 2 ];
        $deletes{ $i } = 1;
    }
#warn "Literal merges: " . scalar( keys( %deletes ) );
    $self->delete_instr( $program, keys( %deletes ) );


if( 0 )
{
    #  TODO: walk program looking for functions, adding to function table.
    @function_table = ();
    %function_index = ();
    foreach my $line ( @{$program} )
    {
        my ( $op, @op_queue );
        @op_queue = ();

        if( $line->[ 0 ] == EXPR )
        {
            push @op_queue, $line->[ 2 ];
        }
        elsif( $line->[ 0 ] == JUMP_IF )
        {
            push @op_queue, $line->[ 3 ];
        }
        elsif( $line->[ 0 ] == URL )
        {
            push @op_queue, %{$line->[ 2 ]};
        }
        elsif( $line->[ 0 ] == FOR )
        {
            push @op_queue, $line->[ 4 ];
        }
        elsif( $line->[ 0 ] == CONTEXT_PUSH )
        {
            push @op_queue, values( %{$line->[ 2 ]} );
        }
        while( defined( $op = shift( @op_queue ) ) )
        {
            next if not ref( $op ) or $op->[ 0 ] == VAR or
                    $op->[ 0 ] == LITERAL or $op->[ 0 ] == TEMPLATE;
            if( $op->[ 0 ] == OP_TREE )
            {
                push @op_queue, $op->[ 3 ], $op->[ 4 ];
                next;
            }
            if( $op->[ 0 ] == UNARY_OP )
            {
                push @op_queue, $op->[ 3 ];
                next;
            }
            if( $op->[ 0 ] == METHOD )
            {
                push @op_queue, @{$op->[ 4 ]};
                next;
            }
            $self->error( "Unknown EXPR opcode: " . $op->[ 0 ] .
                " in function table construction." )
                unless $op->[ 0 ] == FUNC;

#warn "Looking at op " . tinydump( $op );
#warn "  Is function $op->[ 2 ]().";
            if( not $function_index{ $op->[ 2 ] } )
            {
                push @function_table, $op->[ 2 ];
                $function_index{ $op->[ 2 ] } = $#function_table;
            }
            $op->[ 2 ] = $function_index{ $op->[ 2 ] };
#warn "  Replaced with $op->[ 2 ].";
            push @op_queue, @{$op->[ 3 ]};
        }
    }
    $template->{ function_table } = [ @function_table ];
}

    return( $template );
}

#  Warning, pass-by-ref: modifies $program.
sub delete_instr
{
    my ( $self, $program, @addrs ) = @_;
    my ( $renumber );

#warn "Deleting instr $addr: " . Data::Dumper::Dumper( $program->[ $addr ] );

    #  Delete all the stuff we've marked for deletion.
    foreach my $addr ( sort { $b <=> $a } @addrs )
    {
        $renumber = $#{$program} + 1;
        while( --$renumber >= 0 )
        {
            if( $program->[ $renumber ]->[ 0 ] == JUMP    or
                $program->[ $renumber ]->[ 0 ] == JUMP_IF or
                $program->[ $renumber ]->[ 0 ] == FOR     or
                $program->[ $renumber ]->[ 0 ] == END_FOR )
            {
                $program->[ $renumber ]->[ 2 ]--
                    if $program->[ $renumber ]->[ 2 ] > $addr;
            }
        }

        splice( @{$program}, $addr, 1 );
    }
}

sub _compile_expression
{
    my ( $self, $expression ) = @_;
    my ( @top_level, $highest_weight, $highest_pos );

    $expression =~ s/^\s+//;
    $expression =~ s/\s+$//;

#$self->error( "expression = '$expression', expr_regexp = $expr_regexp" )
#  unless $expression =~ /^$expr_regexp$/;

    $self->error( "Not a well-formed expression: $expression" )
        unless $expression =~ /^$expr_regexp$/;

    while( $expression =~ $capture_expr_op_remain_regexp )
    {
        my ( $lhs, $op, $rhs );

        $lhs = $1;
        $op  = $2;
        $rhs = $3;

        push @top_level, $lhs, $op;
        $expression = $rhs;
    }

    return( $self->_build_op_tree( [ @top_level, $expression ] ) )
        if $#top_level >= 0;

    #  Not a compound statement, must be atomic.

    #  Is it a unary op?
    if( $expression =~ /^($unary_operator_regexp)\s*(.*)$/o )
    {
        my ( $op, $subexpr );

        $op      = $1;
        $subexpr = $2;

        $subexpr = $self->_compile_expression( $subexpr );

        #  Fold constant values.
        return( [ LITERAL, $expression,
            $self->_eval_unary_op( $op, $subexpr ) ] )
            if $subexpr->[ 0 ] == LITERAL;

        return( [ UNARY_OP, $expression, $op, $subexpr ] );
    }

    #  Is it a bracketed expression?
    return( $self->_compile_expression( substr( $expression, 1, -1 ) ) )
        if $expression =~ /^$matching_round_brackets_regexp$/o;

    #  A literal number
    return( [ LITERAL, $expression, $expression, 0 ] )
        if $expression =~ /^$literal_number_regexp$/o;

    #  A literal string
    if( $expression =~ /^$single_quoted_text_regexp$/o )
    {
        my ( $string );

        #  Strip leading/trailing ' and unescape backslashed characters.
        $string = substr( $expression, 1, -1 );
        $string =~ s/\\(.)/$1/go;
        return( [ LITERAL, $expression, $string, 0 ] );
    }

    #  A variable
    return( $self->_compile_var( $expression ) )
        if $expression =~ /^$variable_regexp$/o;

    #  A function
    if( $expression =~ $capture_function_regexp )
    {
        my ( $func, $args, $numargs );

        $func = $1;
        $args = length( $2 ) > 2 ? substr( $2, 1, -2 ) : '';

        $args = $self->_compile_func_args( $args );

        $self->error( "Unknown function: $func" )
            unless exists $functions{ $func };

        #  Check the number of args.
        if( ( $numargs = $functions{ $func }->[ FUNC_ARG_NUM ] ) >= 0 )
        {
            $self->error( "too few args to $func(), expected $numargs " .
                "and got " . ( $#{$args} + 1 ) . " in $expression" )
                if $#{$args} + 1 < $numargs;
            $self->error( "too many args to $func(), expected $numargs " .
                "and got " . ( $#{$args} + 1 ) . " in $expression" )
                if $#{$args} + 1 > $numargs;
        }

        unless( $functions{ $func }->[ FUNC_INCONST ] )
        {
            my ( $nonliteral );

            foreach my $arg ( @{$args} )
            {
                next if $arg->[ 0 ] == LITERAL;
                $nonliteral = 1;
                last;
            }

#CORE::warn( "$expression has " . ( $nonliteral ? "nonliteral" : "literal" ) . " args" );
            return( [ LITERAL, $expression,
                $self->_eval_function( $func, $args ), 1 ] )
                unless $nonliteral;
        }

        unshift @{$args}, [ TEMPLATE ]
            if $functions{ $func }->[ FUNC_NEEDS_TEMPLATE ];

        return( [ FUNC, $expression, $func, $args ] );
    }

    #  A method
    if( $expression =~ $capture_method_regexp )
    {
        my ( $var, $method, $args );

        $var    = $1;
        $method = $2;
        $args   = length( $3 ) > 2 ? substr( $3, 1, -2 ) : '';

        $var  = $self->_compile_var( $var );
        $args = $self->_compile_func_args( $args );

        return( [ METHOD, $expression, $var, $method, $args ] );
    }

    $self->error( "Unrecognised atomic expression element: $expression" );
}

sub _build_op_tree
{
    my ( $self, $arr ) = @_;
    my ( $highest_weight, $highest_pos, $op, $lhs, $rhs );

#print "build_op_tree( ", Data::Dumper::Dumper( $arr ), "\n";

    $self->error( "Empty expression" ) if $#{$arr} < 0;

    for( my $i = 0; $i <= $#{$arr}; $i += 2 )
    {
        #  TODO: this is a crappy hack to provide compat with recursion.
        next if ref( $arr->[ $i ] );
        $arr->[ $i ] = $self->_compile_expression( $arr->[ $i ] );
    }

    return( $arr->[ 0 ] ) if $#{$arr} == 0;

    #  Look for literals to fold together.
#print "Looking at: ", Data::Dumper::Dumper( $arr ), "\n";
    for( my $i = 1; $i < $#{$arr}; $i += 2 )
    {
        my ( $op, $weight );

        $op = $arr->[ $i ];
        $weight = $operators{ $op }->[ 0 ];

        $lhs = $arr->[ $i - 1 ];
        $rhs = $arr->[ $i + 1 ];

#print "  Looking at op $i: '$op'\n";
        #  If we're higher or equal precedence to the operators either
        #  side of us, and our lhs and rhs are literal values, we're
        #  eligible for folding.
        if( ( ( $i < 3 ) or
              ( $weight <= $operators{ $arr->[ $i - 2 ] }->[ 0 ] ) ) and
            ( ( $i >= $#{$arr} - 1 ) or
              ( $weight <= $operators{ $arr->[ $i + 2 ] }->[ 0 ] ) ) and
            ( $lhs->[ 0 ] == LITERAL ) and ( $rhs->[ 0 ] == LITERAL ) )
        {
            my ( $original );

            #  Rebuild of "original" is surely hackery of the finest order. :(
            $original = ( $lhs->[ 3 ] ? "( $lhs->[ 1 ] )" : $lhs->[ 1 ] ) .
                " $op " .
                ( $rhs->[ 3 ] ? "( $rhs->[ 1 ] )" : $rhs->[ 1 ] );

            splice( @{$arr}, $i - 1, 3,
                [ LITERAL, $original,
                  $self->_eval_op( $op, $lhs, $rhs ), 1 ] );
            $i = ( $i <= 3 ) ? 1 : $i - 4;
#print "  Folding, arr becomes: ", Data::Dumper::Dumper( $arr ), ", i = $i\n";
        }
    }

    return( $arr->[ 0 ] ) if $#{$arr} == 0;

    $highest_weight = 0;
    for( my $i = 1; $i < $#{$arr}; $i += 2 )
    {
        my ( $op );

        $op = $arr->[ $i ];
#print "looking at op $i: $op\n";
        if( $operators{ $op }->[ 0 ] > $highest_weight )
        {
            $highest_weight = $operators{ $op }->[ 0 ];
            $highest_pos    = $i;
        }
    }
#print "highest_pos = $highest_pos, highest_op = $highest_op\n";

    $op  = $arr->[ $highest_pos ];
    $lhs = $self->_build_op_tree( [ @{$arr}[ 0..$highest_pos - 1 ] ] );
    $rhs = $self->_build_op_tree( [ @{$arr}[ $highest_pos + 1..$#{$arr} ] ] );

    return( [ OP_TREE, '', $op, $lhs, $rhs ] );
}

sub _compile_var
{
    my ( $self, $var ) = @_;
    my ( $original, @segments, @originals );

#print "compile_var( $var )\n";

    $original = $var;

    #  Special vars that are symbolic literals.
    return( [ LITERAL, 'undef', undef ] ) if $var eq 'undef';
    return( [ LITERAL, 'null',  undef ] ) if $var eq 'null';
    return( [ LITERAL, 'cr', "\n" ] )     if $var eq 'cr';

    @segments  = ();
    @originals = ();
    while( $var and ( $var =~ $capture_variable_regexp ) )
    {
        my ( $segment );

        $segment = $1;
        $var     = $2 ? substr( $2, 1 ) : '';
#print "Segment: $segment\nRest: $var\n";

        $segment =~ $capture_variable_segment_regexp;
        push @segments, $1;
        if( $2 )
        {
            #  var[ ... ] expression subscript notation.
            my ( $index, $subscript );

            $subscript = substr( $2, 1, -1 );
            $index = $self->_compile_expression( $subscript );

            #  If it's a constant push it up as if it
            #  was a dotted literal index.
            if( $index->[ 0 ] == LITERAL )
            {
                push @segments, $index->[ 2 ];
            }
            else
            {
                push @segments, $index;
            }
            push @originals, $subscript;
        }
    }

    $self->error( "Malformed variable segment: '$var' in '$original'" )
        if $var;

    if( $segments[ $#segments ] eq '__size__' )
    {
        pop @segments;
        pop @originals;
        return( [ FUNC, $original, 'size',
            [ [ VAR, $original, [ @segments ], [ @originals ] ] ],
            ] );
    }

    return( [ VAR, $original, [ @segments ], [ @originals ] ] );
}

sub _compile_func_args
{
    my ( $self, $arglist ) = @_;
    my ( $original, @args );

    $arglist =~ s/^\s+//;
    $arglist =~ s/\s+$//;

    $original = $arglist;

    @args = ();
    while( defined( $arglist ) and length( $arglist ) and
        ( $arglist =~ $capture_expr_comma_remain_regexp ) )
    {
        my ( $nextarg );

        $nextarg = $1;
        $arglist = $2;
        push @args, $self->_compile_expression( $nextarg );
    }
    $self->error(
        "Malformed function arguments list: '$arglist' in '$original'" )
        if $arglist;
    return( [ @args ] );
}

sub _eval_expression
{
    my ( $self, $expr, $undef_ok ) = @_;
    my ( $type, $val );

    $self->error( "Bad arg to _eval_expression(): $expr" )
        unless ref( $expr );

    $type = $expr->[ 0 ];
#$self->{ exprcount }->{ $type }++;
#my $exprstart = Time::HiRes::time();
    if( $type == LITERAL )
    {
        $val = $expr->[ 2 ];
    }
    elsif( $type == VAR )
    {
        $val = $self->_eval_var( @{$expr}, $undef_ok );
    }
    elsif( $type == OP_TREE )
    {
#        $val = $self->_eval_op( $expr->[ 2 ], $expr->[ 3 ], $expr->[ 4 ] );
        #  WARNING: this is unrolled below from _eval_op: keep in sync.
#eval
#{
        $val = $operators{ $expr->[ 2 ] };
        #  Do we defer evaluation or not?
        if( $val->[ 2 ] )
        {
            $val = $val->[ 1 ]->( $self, $expr->[ 3 ], $expr->[ 4 ] );
        }
        else
        {
            $val = $val->[ 1 ]->( $self,
                $self->_eval_expression( $expr->[ 3 ] ),
                $self->_eval_expression( $expr->[ 4 ] ) );
        }
#};
#$self->error( "$@" ) if $@;
    }
    elsif( $type == UNARY_OP )
    {
        #  TODO: unroll?
        $val = $self->_eval_unary_op( $expr->[ 2 ], $expr->[ 3 ] );
    }
    elsif( $type == FUNC )
    {
#        $val = $self->_eval_function( $expr->[ 2 ], $expr->[ 3 ] );
        #  WARNING: this is unrolled below from _eval_function: keep in sync.

#warn "Eval func $expr->[ 2 ] against " . tinydump( [ @function_table ] );
#        $val = $function_table[ $expr->[ 2 ] ];
        $val = $functions{ $expr->[ 2 ] };
        if( $val->[ FUNC_UNDEF_OK ] )
        {
            $val = $val->[ FUNC_FUNC ]->(
                map { $self->_eval_expression( $_, 1 ) } @{$expr->[ 3 ]} );
        }
        else
        {
            $val = $val->[ FUNC_FUNC ]->(
                map { $self->_eval_expression( $_ ) } @{$expr->[ 3 ]} );
        }
    }
    elsif( $type == METHOD )
    {
        $val = $self->_eval_method( $expr->[ 2 ], $expr->[ 3 ], $expr->[ 4 ] );
    }
    elsif( $type == TEMPLATE )
    {
        return( $self );
    }
    else
    {
        $self->error( "Unknown expression opcode: $type" );
    }
#$self->{ exprprofile }->{ $type } += Time::HiRes::time() - $exprstart;

    #  Undef warning.
    $self->warning( "undefined template value '$expr->[ 1 ]'" )
        unless defined( $val ) or $undef_ok or $expr->[ 1 ] eq 'undef';

    return( $val );
}

sub _eval_op
{
    my ( $self, $op, $lhs, $rhs ) = @_;

#my $ret;
#$self->{ opcount }->{ $op }++;
#my $opstart = Time::HiRes::time();
#$ret = $operators{ $op }->[ 1 ]->( $self, $lhs, $rhs );
#$self->{ opprofile }->{ $op } += Time::HiRes::time() - $opstart;
#return( $ret );

    #  WARNING: this function is unrolled above in _eval_expr: keep in sync.

    $op = $operators{ $op };

    #  Do we defer evaluation or not?
    return( $op->[ 1 ]->( $self,
        $self->_eval_expression( $lhs ),
        $self->_eval_expression( $rhs ) ) )
        unless $op->[ 2 ];

    return( $op->[ 1 ]->( $self, $lhs, $rhs ) );
}

sub _eval_unary_op
{
    my ( $self, $op, $expr ) = @_;

    return( !$self->_eval_expression( $expr, 1 ) )
        if $op eq '!';
    return( not $self->_eval_expression( $expr, 1 ) )
        if $op eq 'not';
    return( -$self->_eval_expression( $expr ) )
        if $op eq '-';

    $self->error( "Unknown unary operator: '$op'" );
}

sub _assign_var
{
    my ( $self, $lhs, $rhs ) = @_;
    my ( $var_stack, $counter, $sz, $var );

    #  TODO: this should be compile-time ideally.
    $self->error( "Invalid LHS to assignment: $lhs->[ 1 ]" )
        if $lhs->[ 0 ] != VAR;

    #  TODO: this should be compile-time ideally.
    $self->error( "Can only assign to top-level variables: $lhs->[ 1 ]" )
        if $#{$lhs->[ 2 ]} > 0;

    $var = $lhs->[ 2 ]->[ 0 ];

    $var_stack = $self->{ var_stack };
    $var_stack->[ 0 ]->{ $var } = $rhs;
    $sz      = $#{$var_stack};
    $counter = 1;
    while( $counter <= $sz )
    {
        return( $rhs ) unless exists( $var_stack->[ $counter ]->{ $var } );
        $var_stack->[ $counter ]->{ $var } = $rhs;
        $counter++;
    }

    return( $rhs );
}

sub _eval_var
{
    my ( $self, $instr, $original, $segments, $originals, $undef_ok ) = @_;
    my ( $val, $stem, $last, $i );

    $last = $#{$segments};

    #  The stem value _is_ the value if there's no other segments.
    return( $self->{ var_stack }->[ 0 ]->{ $segments->[ 0 ] } )
        if $last == 0;

    #  Determine the stem (top-level) value
    $stem = $segments->[ 0 ];
    $val  = $self->{ var_stack }->[ 0 ]->{ $stem };

    #  Check to see if it's a special loop variable or something.
    if( $last >= 1 and
        $self->{ special_values }->{ $stem } and
        exists( $self->{ special_values }->{ $stem }->{ $segments->[ 1 ] } ) )
    {
        #  Don't bother checking that the leaf isn't a ref, it won't
        #  match a key and saves on a ref() call when it isn't.
        $val = $self->{ special_values }->{ $stem }->{ $segments->[ 1 ] };
        $i = 2;
    }
    else
    {
        $i = 1;
    }

    #  Navigate our way down the remaining segments.
    for( ; $i <= $last; $i++ )
    {
        my ( $leaf, $type );

        $leaf = $segments->[ $i ];

        if( ref( $leaf ) )
        {
            #  It's an index expression of the style var[index]
            $leaf = $self->_eval_expression( $leaf );

            unless( defined $leaf )
            {
                return( undef ) if $undef_ok;
                $self->error(
                    "Undefined index '$originals->[ $i ]' in " .
                    "'$original'" );
            }

            #  Check to see if it's a special loop variable or something.
            #  Only need to do this if we're an EXPR subscript, constant
            #  ones will have been checked outside the loop.
            if( $i == 1 )
            {
                if( $self->{ special_values }->{ $stem } and
                    exists( $self->{ special_values }->{ $stem }->{ $leaf } ) )
                {
                    $val = $self->{ special_values }->{ $stem }->{ $leaf };
                    next;
                }
            }
        }

        unless( defined( $val ) )
        {
            return( undef ) if $undef_ok;
            $self->error(
                "Can't get key '$leaf' " .
                ( $originals->[ $i ] ne $leaf ?
                  "(from '$originals->[ $i ]') " : "" ) .
#"(with segments " . Data::Dumper::Dumper( $segments ) . ") " .
                  "of undefined parent in '$original'" );
        }

        $type = ref( $val );

        if( not $type )
        {
            $self->error(
                "Can't get key '$leaf' " .
                ( $originals->[ $i ] ne $leaf ?
                    "(from '$originals->[ $i ]') " : "" ) .
                "of non-reference parent in '$original'" );
        }
        elsif( $type eq 'ARRAY' )
        {
            $self->error(
                "Can't index array-reference with string '$leaf' " .
                ( $originals->[ $i ] ne $leaf ?
                    "(from '$originals->[ $i ]') " : "" ) .
                "in '$original'" )
                unless $leaf =~ /^\d+$/o;
            $val = defined( $val->[ $leaf ] ) ? $val->[ $leaf ] : undef;
        }
        else
        {
            $val = defined( $val->{ $leaf } ) ? $val->{ $leaf } : undef;
        }
    }

    return( $val );
}

sub _eval_function
{
    my ( $self, $func, $args ) = @_;

    #  WARNING: this function is unrolled above in _eval_expr: keep in sync.

    $func = $functions{ $func };

    #  TODO: should be flag on function definition.
    if( $func->[ FUNC_UNDEF_OK ] )
    {
        $args = [ map { $self->_eval_expression( $_, 1 ) } @{$args} ];
    }
    else
    {
        $args = [ map { $self->_eval_expression( $_ ) } @{$args} ];
    }

#$self->{ funccount }->{ $func }++;
#my $ret;
#my $start_time = Time::HiRes::time();
#    $ret = $functions{ $func }->[ 1 ]->( $self, @{$args} );
#$self->{ funcprofile }->{ $func } += Time::HiRes::time() - $start_time;
#    return( $ret );

    if( $func->[ FUNC_NEEDS_TEMPLATE ] )
    {
        return( $func->[ FUNC_FUNC ]->( $self, @{$args} ) );
    }
    else
    {
        return( $func->[ FUNC_FUNC ]->( @{$args} ) );
    }
}

sub _eval_method
{
    my ( $self, $var, $method, $args ) = @_;
    my ( $varname, $ret );

    $varname = $var->[ 1 ];
    $var = $self->_eval_var( @{$var} );

    $self->error( "Can't call method on undefined value $varname" )
        unless defined $var;
    $self->error( "Can't call method on non-reference value $varname: $var" )
        unless ref( $var );

    #  For security reasons we don't want to allow calling
    #  just any old method on any old object from within a
    #  potentially user-defined template.
    $self->error( 'Invalid method to call from within a template: ' .
        ref( $var ) . "->$method" )
        unless $var->valid_template_method( $method );

    $args = [ map { $self->_eval_expression( $_ ) } @{$args} ];

    {
        no strict 'refs';
        $ret = $var->$method( @{$args} );
    }

    return( $ret );
}

sub run
{
    my ( $self ) = @_;
    my ( $lineno, $ret, @var_stack, @for_stack, $run_start, $total_instr,
        $program, $last_instr, $special_values );

#CORE::warn( "Template::run( $self, $input )" );

    $run_start = Time::HiRes::time();

#$ret = ' ' x 80_000;
    $ret = '';
    $lineno = 0;

#print "Content-type: text/html\n\n<PRE>" . Data::Dumper::Dumper( $self->_var_value( 'param' ) ) . "from " . Data::Dumper::Dumper( $input ) . "</PRE>";

    @var_stack = ( $self->{ vars } );
    @for_stack = ();

    $self->{ var_stack } = \@var_stack;
    $self->{ phase }     = 'runtime';

    $total_instr = 0;

#foreach my $prof ( qw/instr expr func op/ )
#{
#    $self->{ "${prof}count" }   = {};
#    $self->{ "${prof}profile" } = {};
#}

    #  Local unroll of some of our properties
    $program        = $self->{ template }->{ program };
#    @function_table =
#        map { $functions{ $_ } } @{$self->{ template }->{ function_table }};
    $special_values = $self->{ special_values };

    $last_instr  = $#{$program};
    while( $lineno <= $last_instr )
    {
        my ( $line, $instr );

        $line  = $program->[ $lineno++ ];
        $self->{ current_pos } = $line->[ 1 ];

        $instr = $line->[ 0 ];

        $total_instr++;

        #  TODO: look at $pos->[ 0 ] to determine file and recreate
        #    the "stack" for error traces if neccessary.

#$self->{ instrcount }->{ $instr }++;
#my $instrstart = Time::HiRes::time();
        if( $instr == LITERAL )
        {
            $ret .= $line->[ 2 ];
        }
        elsif( $instr == EXPR )
        {
            my ( $value );

            $value = $self->_eval_expression( $line->[ 2 ] );
            $ret .= ( ( ref( $value ) eq 'SCALAR' ) ? ${$value} : $value )
                unless $line->[ 3 ];
        }
        elsif( $instr == JUMP )
        {
#$ret .= "[jump]";
            $lineno = $line->[ 2 ];
        }
        elsif( $instr == JUMP_IF )
        {
            my ( $value );

#$ret .= "[jump if/unless $line->[3]]";
            $value = $self->_eval_expression( $line->[ 3 ], 1 );
            $value = not $value if $line->[ 4 ];
            $lineno = $line->[ 2 ] if $value;
        }
        elsif( $instr == FOR )
        {
            my ( $iterator, $set, $set_value, $hash, $last );

            $iterator = $line->[ 3 ];
            $set      = $line->[ 4 ];

            $set_value = $self->_eval_expression( $set, 1 );
            $set_value = [] unless defined $set_value;

            if( ref( $set_value ) eq 'HASH' )
            {
                $hash      = $set_value;
                $set_value = [ sort( keys( %{$set_value} ) ) ];
            }
            elsif( not ref( $set_value ) )
            {
                #  If it's a number make it into a loop of 0..number.
                #  If they want 1..number they can <: if x != 1 :> inside it.
                $set_value = [ 0..int( $set_value ) ];
            }

            $last = $#{$set_value};
            if( $last == -1 )
            {
                $lineno = $line->[ 2 ];
            }
            else
            {
                my ( $value, $context );

                $value = $set_value->[ 0 ];
                $special_values->{ $iterator } =
                    {
                        __counter__ => 0,
                        __even__    => 1,
                        __odd__     => 0,
                        __first__   => 1,
                        __inner__   => 0,
                        __last__    => $last == 0 ? 1 : 0,
                        __prev__    => undef,
                        __next__    => $last == 0 ?
                                       undef : $set_value->[ 1 ],
                    };
                $special_values->{ $iterator }->{ __value__ } =
                    $hash->{ $value }
                    if $hash;
                #  Optimization: only create a new context if needed.
                if( $var_stack[ 0 ]->{ $iterator } )
                {
                    $context = { %{$var_stack[ 0 ]} };
                    $context->{ $iterator } = $value;
                    unshift @var_stack, $context;
                }
                else
                {
                    $var_stack[ 0 ]->{ $iterator } = $value;
                }
                unshift @for_stack, [ 0, $set_value, $hash, $context ? 0 : 1 ];
            }
        }
        elsif( $instr == END_FOR )
        {
            my ( $iterator, $set, $set_value, $counter, $hash, $last );

            $iterator = $line->[ 3 ];
            $set      = $line->[ 4 ];

            $counter   = $for_stack[ 0 ]->[ 0 ] + 1;
            $set_value = $for_stack[ 0 ]->[ 1 ];
            $hash      = $for_stack[ 0 ]->[ 2 ];
            $last      = $#{$set_value};

            if( $counter <= $last )
            {
                my ( $value );

                $value = $set_value->[ $counter ];
                $special_values->{ $iterator } =
                    {
                        __counter__ => $counter,
                        __even__    => ( $counter % 2 ) ? 0 : 1,
                        __odd__     => $counter % 2,
                        __first__   => 0,
                        __inner__   => $counter == $last ? 0 : 1,
                        __last__    => $counter == $last ? 1 : 0,
                        __prev__    => $set_value->[ $counter - 1 ],
                        __next__    => $counter == $last ?
                                           undef :
                                           $set_value->[ $counter + 1 ],
                    };
                $special_values->{ $iterator }->{ __value__ } =
                    $hash->{ $value }
                    if $hash;

                $var_stack[ 0 ]->{ $iterator } = $value;

                $for_stack[ 0 ]->[ 0 ] = $counter;

                $lineno = $line->[ 2 ];
            }
            else
            {
                if( $for_stack[ 0 ]->[ 3 ] )
                {
                    delete $var_stack[ 0 ]->{ $iterator };
                }
                else
                {
                    shift @var_stack;
                }
                shift @for_stack;
                delete $special_values->{ $iterator };
            }
        }
        elsif( $instr == CONTEXT_PUSH )
        {
            my ( $context, $new_context );

            $new_context = $line->[ 2 ] || {};
            $context = { %{$var_stack[ 0 ]} };
            foreach my $var ( keys( %{$new_context} ) )
            {
                $context->{ $var } = $self->_eval_expression(
                    $new_context->{ $var }, 1 )
            }
            unshift @var_stack, $context;
        }
        elsif( $instr == CONTEXT_POP )
        {
#$ret .= "[context_pop]";
            shift @var_stack;
        }
#  TODO:  ick, hate cut-n-paste code.
#  TODO:  unroll constant parts of hash lookups to local var
        elsif( $self->{ local_syntaxes }->{ '.instr' }->{ $instr } )
        {
            my ( $executor, $token, $value );

            $token    = $self->{ local_syntaxes }->{ '.instr' }->{ $instr };
            $executor = $self->{ local_syntaxes }->{ $token }->{ run_method };
            {
                no strict 'refs';
                $value = $self->$executor( $token, $line->[ 2 ] );
            }
            $ret .= $value if defined $value;
        }
#  TODO:  ick, hate cut-n-paste code.
#  TODO:  unroll constant parts of hash lookups to local var
        elsif( $syntaxes{ '.instr' }->{ $instr } )
        {
            my ( $executor, $token, $value );

            $token    = $syntaxes{ '.instr' }->{ $instr };
            $executor = $syntaxes{ $token }->{ run_method };
            {
                no strict 'refs';
                $value = $self->$executor( $token, $line->[ 2 ] );
            }
            $ret .= $value if defined $value;
        }
        elsif( $instr == DEBUG )
        {
            $self->{ debug }->{ $line->[ 2 ]->{ type } } =
                ( $line->[ 2 ]->{ state } eq 'on' );
        }
#$self->{ instrprofile }->{ $instr } += Time::HiRes::time() - $instrstart;
    }

    delete $self->{ current_pos };
    delete $self->{ var_stack };
    delete $self->{ input };
    delete $self->{ phase };

    my $tt = Time::HiRes::time() - $run_start;
#  TODO:  belongs in application wrapper
#    if( $self->{ format } eq 'html' )
#    {
#        $ret .= "<!-- $self->{ filename } run-time: " .
#            sprintf( "%7.3f", $tt * 1000 ) . "ms, " .
#            "instr run: $total_instr, prog length: " .
#            ( $last_instr + 1 ) . " instr -->\n";
#    }
    $total_time += $tt;

#  TODO:  belongs in application wrapper
#if( $self->{ format } eq 'html' )
#{
#foreach my $prof ( qw/instr expr func op/ )
#{
#$ret .= "<!-- $self->{ filename } ${prof}count:\n";
#foreach ( sort { $self->{ "${prof}count" }->{ $b } <=> $self->{ "${prof}count" }->{ $a } } keys( %{$self->{ "${prof}count" }} ) )
#{
#  $ret .= sprintf( "%12s => %d\n", $_, $self->{ "${prof}count" }->{ $_ } );
#}
#$ret .= "-->\n";
#$ret .= "<!-- $self->{ filename } ${prof}profile:\n";
#foreach ( sort { $self->{ "${prof}profile" }->{ $b } <=> $self->{ "${prof}profile" }->{ $a } } keys( %{$self->{ "${prof}profile" }} ) )
#{
#  $ret .= sprintf( "%12s => %7.3fms (%7.3fms)\n", $_, $self->{ "${prof}profile" }->{ $_ } * 1000, $self->{ "${prof}profile" }->{ $_ } * 1000 / $self->{ "${prof}count" }->{ $_ } );
#}
#$ret .= "-->\n";
#delete $self->{ "${prof}count" };
#delete $self->{ "${prof}profile" };
#}
##my $tmp = $self->_dumpable_template();
###my $tmp = $self->_decompile_template();
##$tmp =~ s/-->/-- >/g;
##$ret .= "<!-- $self->{ filename } dump:\n$tmp-->\n";
#}

    return( \$ret );
}

sub tersedump
{
    return( Data::Dumper->new( [ @_ ] )->Terse(1)->Useqq(1)->Dump() );
}

sub tinydump
{
    return( Data::Dumper->new( [ @_ ] )->Indent(0)->Quotekeys(0)->Pair('=>')->Terse(1)->Useqq(1)->Dump() );
}

sub _dumpable_template
{
    my ( $self ) = @_;
    my ( $lineno, $ret );

    $ret = '';
    $lineno = 0;

    foreach my $line ( @{$self->{ template }->{ program }} )
    {
        my ( $instr );

        $ret .= sprintf( "%04d: [%-20s %3d %3d][%-12s] ", $lineno++,
            $line->[ 1 ][ 0 ], $line->[ 1 ][ 1 ], $line->[ 1 ][ 2 ],
            $line->[ 0 ] );

        $instr = $line->[ 0 ];
        if( $instr == LITERAL )
        {
#            $ret .= "\"$line->[2]\"\n";
            $ret .= tinydump( $line->[ 2 ] ) . "\n";
        }
        elsif( $instr == EXPR )
        {
            $ret .= tinydump( $line->[ 2 ] ) .
                ( $line->[ 3 ] ? " (void)" : "" ). "\n";
        }
        elsif( $instr == JUMP )
        {
            $ret .= "$line->[2]\n";
        }
        elsif( $instr == JUMP_IF )
        {
            $ret .= $line->[ 2 ] .
                ( $line->[ 4 ] ? ' unless ' : ' if ' ) .
                tinydump( $line->[ 3 ] ) . "\n";
        }
        elsif( $instr == FOR )
        {
            $ret .= "$line->[ 3 ] in " . tinydump( $line->[ 4 ] ) .
                " then $line->[ 2 ]\n";
        }
        elsif( $instr == END_FOR )
        {
            $ret .= "$line->[ 3 ] in " . tinydump( $line->[ 4 ] ) .
                " repeat $line->[ 2 ]\n";
        }
        elsif( $instr == CONTEXT_PUSH )
        {
            $ret .= "context push of " . tinydump( $line->[ 2 ] ) . "\n";
        }
        elsif( $instr == CONTEXT_POP )
        {
            $ret .= "context pop\n";
        }
#  TODO: local syntax support.
    }

    return( $ret );
}

sub _dump_template
{
    my ( $self ) = @_;

    print $self->_dumpable_template();
}

sub _decompile_template
{
    my ( $self ) = @_;
    my ( $lineno, $ret );

    $ret = '';
    $lineno = 0;

    foreach my $line ( @{$self->{ template }->{ program }} )
    {
        my ( $instr );

        $instr = $line->[ 0 ];
        if( $instr == LITERAL )
        {
            $ret .= ( $line->[ 2 ] =~ /^$/ ) ?
                "<: empty literal :>" : $line->[ 2 ];
            next;
        }
        $ret .= "<: $instr ";
        if( $instr == EXPR )
        {
            my ( $dump );

            $dump = Data::Dumper::Dumper( $line->[ 2 ] );
            $dump =~ s/^\$VAR1 = //;
            $dump =~ s/;\n$//;
            $ret .= $line->[ 2 ]->[ 1 ] . " ($dump)";
        }
        elsif( $instr == JUMP )
        {
            $ret .= "$line->[2]";
        }
        elsif( $instr == JUMP_IF )
        {
            $ret .= $line->[ 2 ] .
                ( $line->[ 4 ] ? ' unless ' : ' if ' ) .
                "$line->[3]";
        }
        elsif( $instr == FOR )
        {
            $ret .= "$line->[ 3 ] in $line->[ 4 ] then $line->[ 2 ]";
        }
        elsif( $instr == END_FOR )
        {
            $ret .= "$line->[ 3 ] in $line->[ 4 ] repeat $line->[ 2 ]";
        }
        elsif( $instr == CONTEXT_PUSH )
        {
            my ( $dump );

            $dump = defined( $line->[ 2 ] ) ? Data::Dumper::Dumper( $line->[ 2 ] ) : 'undef';
            $dump =~ s/^\$VAR1 = //;
            $dump =~ s/;\n$//;
            $dump =~ s/\s+/ /g;
            $ret .= "context push of $dump";
        }
        elsif( $instr == CONTEXT_POP )
        {
            $ret = substr( $ret, 0, -1 );
        }
#  TODO: support for local syntax
        else
        {
            $ret .= "(unhandled by decompile)";
        }
        $ret .= " :>";
    }

    return( $ret );
}

sub clear_total_time { $total_time = 0; }
sub total_time { return( $total_time ); }

1;
