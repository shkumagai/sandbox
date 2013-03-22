#!/usr/bin/env perl
#
# sample implementation of Burrows Wheeler Transform (BWT).
#
# refer to: http://d.hatena.ne.jp/naoya/20081016/1224173077
#

use strict;
use warnings;

use constant UCHAR_MAX => 0x100;

sub bs_encode {
    my $text = shift . chr 0;
    my $len = length $text;

    my @block;
    for (my $i = 0; $i < $len; $i++) {
        push @block, $text;
        $text .= substr($text, 0, 1);
        substr($text, 0, 1) = '';
    }
    return join '', map { substr($_, -1, 1) } sort @block;
}

sub bs_decode {
    my $bwt = shift;

    my $len = length $bwt;
    my @data = split //, $bwt;
    my $pos = -1;

    my @count;
    for (my $i = 0; $i < UCHAR_MAX; $i++) {
        $count[$i] = 0;
    }

    for (my $i = 0; $i < $len; $i++) {
        if ($data[$i] eq chr(0)) {
            $pos = $i;
        }
        $count[ ord $data[$i] ]++;
    }

    for (my $i = 0; $i < UCHAR_MAX; $i++) {
        $count[$i] += $count[$i - 1];
    }

    my @LFMapping;
    for (my $i = $len - 1; $i >= 0; $i--) {
        $LFMapping[ --$count[ ord $data[$i] ] ] = $i
    }

    my @buf;
    for (0..$len - 1) {
        $pos = $LFMapping[ $pos ];
        push @buf, $data[ $pos ];
    }

    return join '', @buf;
}

sub main {
    my $orig = $ARGV[0];
    printf "original : %s\n", $orig;

    my $encoded = bs_encode($orig);
    printf "encoded  : %s\n", $encoded;
    printf "decoded  : %s\n", bs_decode($encoded);
}

main if (__FILE__ eq $0);
