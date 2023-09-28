#!/usr/bin/env perl
#***************************************************************************
#                                  _   _ ____  _
#  Project                     ___| | | |  _ \| |
#                             / __| | | | |_) | |
#                            | (__| |_| |  _ <| |___
#                             \___|\___/|_| \_\_____|
#
# Copyright (C) Daniel Stenberg, <daniel@haxx.se>, et al.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at https://curl.se/docs/copyright.html.
#
# You may opt to use, copy, modify, merge, publish, distribute and/or sell
# copies of the Software, and permit persons to whom the Software is
# furnished to do so, under the terms of the COPYING file.
#
# This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
# KIND, either express or implied.
#
# SPDX-License-Identifier: curl
#
###########################################################################
#
# Invoke script in the root of the git checkout. Scans all files in git unless
# given a specific single file.
#
# Usage: copyright.pl [file]
#

my %skips;

# file names
my %skiplist = (
    # REUSE-specific file
    ".reuse/dep5" => "<built-in>",

    # License texts
    "LICENSES/BSD-3-Clause.txt" => "<built-in>",
    "LICENSES/BSD-4-Clause-UC.txt" => "<built-in>",
    "LICENSES/ISC.txt" => "<built-in>",
    "LICENSES/curl.txt" => "<built-in>",
    "COPYING" => "<built-in>",

    );

sub scanfile {
    my ($f) = @_;
    my $line=1;
    my $found = 0;
    open(F, "<$f") || return -1;
    while (<F>) {
        chomp;
        my $l = $_;
        # check for a copyright statement and save the years
        if($l =~ /.* ?copyright .* (\d\d\d\d|)/i) {
            my $count = 0;
            while($l =~ /([\d]{4})/g) {
                push @copyright, {
                  year => $1,
                  line => $line,
                  col => index($l, $1),
                  code => $l
                };
                $count++;
            }
            if(!$count) {
                # year-less
                push @copyright, {
                    year => -1,
                    line => $line,
                    col => index($l, $1),
                    code => $l
                };
                $count++;
            }
            $found = $count;
        }
        if($l =~ /SPDX-License-Identifier:/) {
            $spdx = 1;
        }
        # allow within the first 100 lines
        if(++$line > 100) {
            last;
        }
    }
    close(F);
    return $found;
}

sub checkfile {
    my ($file, $skipped, $pattern) = @_;
    $spdx = 0;
    my $found = scanfile($file);

    if($found < 1) {
        if($skipped) {
            # just move on
            $skips{$pattern}++;
            return 0;
        }
        if(!$found) {
            print "$file:1: missing copyright range\n";
            return 2;
        }
        # this means the file couldn't open - it might not exist, consider
        # that fine
        return 1;
    }
    if(!$spdx) {
        if($skipped) {
            # move on
            $skips{$pattern}++;
            return 0;
        }
        print "$file:1: missing SPDX-License-Identifier\n";
        return 2;
    }

    if($skipped) {
        print "$file:1: ignored superfluously by $pattern\n" if($verbose);
        $superf{$pattern}++;
    }

    return 1;
}

sub dep5 {
    my ($file) = @_;
    my @files;
    my $copy;
    open(F, "<$file") || die "can't open $file";
    my $line = 0;
    while(<F>) {
        $line++;
        if(/^Files: (.*)/i) {
            push @files, `git ls-files $1`;
        }
        elsif(/^Copyright: (.*)/i) {
            $copy = $1;
        }
        elsif(/^License: (.*)/i) {
            my $license = $1;
            for my $f (@files) {
                chomp $f;
                if($f =~ /\.gitignore\z/) {
                    # ignore .gitignore
                }
                else {
                    if($skiplist{$f}) {
                        print STDERR "$f already skipped at $skiplist{$f}\n";
                    }
                    $skiplist{$f} = "dep5:$line";
                }
            }
            undef @files;
        }
    }
    close(F);
}

dep5(".reuse/dep5");

my $checkall = 0;
my @all;
my $verbose;
if($ARGV[0] eq "-v") {
    $verbose = 1;
    shift @ARGV;
}
if($ARGV[0]) {
    push @all, @ARGV;
}
else {
    @all = `git ls-files`;
    $checkall = 1;
}

for my $f (@all) {
    chomp $f;
    my $skipped = 0;
    my $miss;
    my $wro;
    my $pattern;
    if($skiplist{$f}) {
        $pattern = $skip;
        $skiplisted++;
        $skipped = 1;
        $skip{$f}++;
    }

    my $r = checkfile($f, $skipped, $pattern);
    $mis=1 if($r == 2);
    $wro=1 if(!$r);

    if(!$skipped) {
        $missing += $mis;
        $wrong += $wro;
    }
}

if($verbose) {
    print STDERR "$missing files have no copyright\n" if($missing);
    print STDERR "$wrong files have wrong copyright year\n" if ($wrong);
    print STDERR "$skiplisted files are skipped\n" if ($skiplisted);

    for my $s (@skiplist) {
        if(!$skips{$s}) {
            printf ("Never skipped pattern: %s\n", $s);
        }
        if($superf{$s}) {
            printf ("%s was skipped superfluously %u times and legitimately %u times\n",
                    $s, $superf{$s}, $skips{$s});
        }
    }
}

if($checkall) {
    for(keys %skiplist) {
        if(!$skip{$_}) {
            printf STDERR "$_ is marked for SKIP but is missing!\n";
        }
    }
}

exit 1 if($missing || $wrong);
