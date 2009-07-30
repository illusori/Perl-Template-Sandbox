#!/usr/bin/perl -w

use strict;
use warnings;

use Module::Build;

my $build = Module::Build->new(
    module_name  => 'Template::Sandbox',
    license      => 'perl',
    dist_author  => 'Sam Graham <libtemplate-sandbox-perl@illusori.co.uk>',
    build_requires => {
        'Module::Build'    => 0.07,
        'Test::More'       => 0,
        },
    requires => {
        'Carp'             => 0,
        'Storable'         => 1.014,
        'Class::Handle'    => 0,
        'Cwd'              => 0,
        'Data::Dumper'     => 0,
        'IO::File'         => 0,
        'Log::Any'         => 0,
        'Scalar::Util'     => 0,
        'Time::HiRes'      => 0,
        },
    recommends => {
        },
    sign => 1,
    dynamic_config => 0,
    create_makefile_pl => 'passthrough',
    );
$build->create_build_script;
