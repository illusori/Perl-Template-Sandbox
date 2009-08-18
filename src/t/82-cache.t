#!perl -T

use strict;
use warnings;

use Test::More;

use File::Spec;
use FindBin;
use Cwd ();

use Template::Sandbox qw/:function_sugar/;

my ( %caches );

BEGIN
{
    eval "use Test::Exception";
    plan skip_all => "Test::Exception required for testing caching" if @_;

    %caches = (
        'Cache::Cache'        => undef,
        'Cache::CacheFactory' => undef,
        );

    eval "use Cache::MemoryCache";
    $caches{ 'Cache::Cache' } = Cache::MemoryCache->new() unless @_;    

    eval "use Cache::CacheFactory";
    $caches{ 'Cache::CacheFactory' } = Cache::CacheFactory->new(
        storage  => 'memory',
        validity => 'lastmodified',
        ) unless @_;

    plan skip_all =>
        "Cache::MemoryCache or Cache::CacheFactory required for cache tests"
         unless $caches{ 'Cache::Cache' } or $caches{ 'Cache::CacheFactory' };
}

my ( $tests_per_variant, $option_variants, $template_variants,
    $tests_per_cache );

$tests_per_variant = 6;
$option_variants   = 2;
$template_variants = 2;
$tests_per_cache   =
    $tests_per_variant * $option_variants * $template_variants;

plan tests => $tests_per_cache * scalar( keys( %caches ) );

my ( $template, $template_root, $expected, $compile_counter,
     $template_file, $template_string );

#  TODO:  nasty nasty nasty, find out how Template::Toolkit etc do it.
{
    my ( @candidate_dirs );

    foreach my $startdir ( Cwd::cwd(), $FindBin::Bin )
    {
        push @candidate_dirs,
            File::Spec->catdir( $startdir, 't/test_templates' ),
            File::Spec->catdir( $startdir, 'test_templates' );
    }

    @candidate_dirs = grep { -d $_ } @candidate_dirs;

    plan skip_all => ( 'unable to find t/test_templates relative to bin: ' .
        $FindBin::Bin . ' or cwd: ' . Cwd::cwd() )
        unless @candidate_dirs;

    $template_root = $candidate_dirs[ 0 ];
}

$compile_counter = 0;

#  Trickery here, we make a function that appears to be constant
#  so that it gets evaluated once during compile phase and folded
#  into the template as a literal value.
#  Thus only recompiles will increment the counter, a cache hit
#  skips the compile phase and uses the old compiled value.
Template::Sandbox->register_template_function(
    compile_counter => no_args sub { ++$compile_counter; },
    );

$template_file   = File::Spec->catfile( $template_root, 'cache.txt' );
$template_string = "<: expr compile_counter() :>\n";

SKIP: foreach my $cache_type ( sort( keys( %caches ) ) )
{
    my ( $cache );

    plan skip => "No $cache_type available", $tests_per_cache
        unless $caches{ $cache_type };

    $cache = $caches{ $cache_type };

    foreach my $option ( '', 'ignore_module_dependencies' )
    {
        my ( $constructor );

        if( $option )
        {
            $constructor = sub
                {
                    return( Template::Sandbox->new(
                        cache   => $cache,
                        $option => 1,
                        ) );
                };
        }
        else
        {
            $constructor = sub
                {
                    return( Template::Sandbox->new(
                        cache   => $cache,
                        ) );
                };
        }

        foreach my $template_type ( qw/file string/ )
        {
            my ( $compiler, $previous_compile_counter,
                 $cached_compile_counter, $test_desc );

            if( $template_type eq 'file' )
            {
                $compiler = sub
                    {
                        $_[ 0 ]->set_template( $template_file );
                    };
            }
            else
            {
                $compiler = sub
                    {
                        $_[ 0 ]->set_template_string( $template_string );
                    };
            }

            $cache->Clear();

            #
            #  1-3: test cache miss
            $test_desc = "$cache_type miss of template $template_type";
            $template = $constructor->();
            $previous_compile_counter = $compile_counter;
            $compiler->( $template );
            is( $compile_counter, $previous_compile_counter + 1,
                "compile count increased by $test_desc" );
            $cached_compile_counter = $compile_counter;
            $previous_compile_counter = $compile_counter;
            is( ${$template->run()}, "$compile_counter\n",
                "compile count in template is current val on $test_desc" );
            is( $compile_counter, $previous_compile_counter,
                "compile count unchanged by run after $test_desc" );

            #  Manually fake a compile to ensure we've moved on from
            #  the value cached inside the compiled template.
            $compile_counter++;

            #
            #  4-6: test cache hit
            $test_desc = "$cache_type hit of template $template_type";
            $template = $constructor->();
            $previous_compile_counter = $compile_counter;
            $compiler->( $template );
            is( $compile_counter, $previous_compile_counter,
                "compile count unchanged by $test_desc" );
            $previous_compile_counter = $compile_counter;
            is( ${$template->run()}, "$cached_compile_counter\n",
                "compile count in template is cached val on $test_desc" );
            is( $compile_counter, $previous_compile_counter,
                "compile count unchanged by run after $test_desc" );

#  TODO:       test cache stale-hit
        }
    }

    #  Cleanup in case I add persistent caches to the test later.
    $cache->Clear();
}
