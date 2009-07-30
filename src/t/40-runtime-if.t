#!perl -T

use strict;
use warnings;

use Test::More;

use Template::Sandbox;

my @first_tokens  = qw/if unless/;
my @second_tokens = qw/if unless none/;
my @third_tokens  = qw/else none/;

my $num_tests = 2 *
    ( scalar( @first_tokens ) * 2 ) *
    ( scalar( @second_tokens ) * 2 ) *
    ( scalar( @third_tokens ) );

plan tests => $num_tests;

my ( $template, $syntax );

my @branches = ( 'one', 'two', 'three' );

#  Yick, yick, and double yick.
#  Yes this produces some dupes, but better some dupes and to be exhaustive
#  than try to make it even more complex to avoid the dupes.
foreach my $first ( @first_tokens )
{
    foreach my $first_val ( 0, 1 )
    {
        my ( $first_true, $first_clause );

        $first_true = $first_val;
        $first_true = !$first_true if $first eq 'unless';

        $first_clause = "<: $first $first_val :>$branches[ 0 ]";

foreach my $second ( @second_tokens )
{
    foreach my $second_val ( 0, 1 )
    {
        my ( $second_true, $second_clause );

        if( $second eq 'none' )
        {
            $second_true   = 0;
            $second_clause = '';
        }
        else
        {
            $second_true = $second_val;
            $second_true = !$second_true if $second eq 'unless';
            $second_clause = "<: els$second $second_val :>$branches[ 1 ]";
        }

foreach my $third ( @third_tokens )
{
    my ( $third_true, $third_clause, $result );

    if( $third eq 'none' )
    {
        $third_true   = 0;
        $third_clause = '';
    }
    else
    {
        $third_true = 1;
        $third_clause = "<: else :>$branches[ 2 ]";
    }

    $syntax = "$first_clause$second_clause$third_clause<: endif :>";
    $result = '';
    $result = $branches[ 2 ] if $third_true;
    $result = $branches[ 1 ] if $second_true;
    $result = $branches[ 0 ] if $first_true;

    $template = Template::Sandbox->new();
    $template->set_template_string( $syntax );
    is( ${$template->run()}, $result, $syntax . " = '$result'" );
}
}
}
}
}


#  I truly loathe cut-n-paste code, but sometimes it's the easiest way...
foreach my $first ( @first_tokens )
{
    foreach my $first_val ( 0, 1 )
    {
        my ( $first_true, $first_clause );

        $first_true = $first_val;
        $first_true = !$first_true if $first eq 'unless';

        $first_clause = "<: $first a :>$branches[ 0 ]";

foreach my $second ( @second_tokens )
{
    foreach my $second_val ( 0, 1 )
    {
        my ( $second_true, $second_clause );

        if( $second eq 'none' )
        {
            $second_true   = 0;
            $second_clause = '';
        }
        else
        {
            $second_true = $second_val;
            $second_true = !$second_true if $second eq 'unless';
            $second_clause = "<: els$second b :>$branches[ 1 ]";
        }

foreach my $third ( @third_tokens )
{
    my ( $third_true, $third_clause, $result );

    if( $third eq 'none' )
    {
        $third_true   = 0;
        $third_clause = '';
    }
    else
    {
        $third_true = 1;
        $third_clause = "<: else :>$branches[ 2 ]";
    }

    $syntax = "$first_clause$second_clause$third_clause<: endif :>";
    $result = '';
    $result = $branches[ 2 ] if $third_true;
    $result = $branches[ 1 ] if $second_true;
    $result = $branches[ 0 ] if $first_true;

    $template = Template::Sandbox->new();
    $template->set_template_string( $syntax );
    $template->add_vars(
        {
            a => $first_val,
            b => $second_val,
        } );
    is( ${$template->run()}, $result, $syntax . " ( a => $first_val, b => $second_val ) = '$result'" );
}
}
}
}
}
