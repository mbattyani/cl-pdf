#!/usr/bin/perl

#
#  Convert ttx to ufm
#  Erik Ronström 2019
#
#  cl-pdf uses an extended version of the afm file format to read font metrics
#  for unicode fonts. The ufm files can be produced from ttf files using a
#  special version of ttf2pt1, but unfortunately ttf2pt1 is old and cannot
#  handle many newer fonts well. For example, it fails to load kern Apple-flavoured
#  kern tables, and it prints 'Unknown' as font name for many fonts.
#
#  The ttx tool, on the other hand, seems to do a much better job in parsing
#  ttf files, so the idea is to use that and convert the resulting ttx files
#  info ufm files.
#
#  Usage:
#  ttx Times.ttf
#  ttx2ufm Times.ttx > Times.ufm
#


use strict;
use warnings;

use XML::Parser;
use Math::Round;

my $p = XML::Parser->new(Style => 'Objects');
my $ttf = $p->parsefile($ARGV[0]);

die unless $ttf->[0]->isa("ttFont");


# Collect tables of interest
my @interestingTables = ("GlyphOrder", "cmap", "head", "hhea", "glyf", "name", "hmtx", "kern", "post");
my %tables;
foreach my $table (@{$ttf->[0]->{'Kids'}}) {
    foreach my $t (@interestingTables) {
        $tables{$t} = $table->{'Kids'} if ($table->isa($t));
    }
}

my @glyphNames;  # index -> name
my %glyphNames;  # name -> index
my $unitsPerEm;
my %fontMetrics;
my %glyphMetrics;
my %encoding;
my @kerningPairs;
my @name;
my $italicAngle;
my $isFixedPitch;
my $ascender;
my $descender;
my $underlinePosition;
my $underlineThickness;

# Load glyph names from GlyphOrder table
foreach my $glyph (@{$tables{'GlyphOrder'}}) {
    next unless $glyph->isa("GlyphID");
    push(@glyphNames, $glyph->{'name'});
    $glyphNames{$glyph->{'name'}} = $glyph->{'id'};
}

# Load head
foreach my $entry (@{$tables{'head'}}) {
    $unitsPerEm = $entry->{'value'} if $entry->isa('unitsPerEm');
    $fontMetrics{'xMin'} = $entry->{'value'} if $entry->isa('xMin');
    $fontMetrics{'yMin'} = $entry->{'value'} if $entry->isa('yMin');
    $fontMetrics{'xMax'} = $entry->{'value'} if $entry->isa('xMax');
    $fontMetrics{'yMax'} = $entry->{'value'} if $entry->isa('yMax');
}
die('Unknown unitsPerEm\n') unless $unitsPerEm;
my $scaleFactor = 1000 / $unitsPerEm;

foreach my $key (keys %fontMetrics) {
    $fontMetrics{$key} = round($fontMetrics{$key} * $scaleFactor);
}

# Load hhea (Horizontal Header Table)
foreach my $entry (@{$tables{'hhea'}}) {
    $ascender = round($entry->{'value'} * $scaleFactor) if $entry->isa('ascent');
    $descender = round($entry->{'value'} * $scaleFactor) if $entry->isa('descent');
}

# Load name
foreach my $namerecord (@{$tables{'name'}}) {
    next unless $namerecord->isa('namerecord');
    next unless $namerecord->{'langID'} eq '0x0';
    if ($namerecord->{'nameID'} <= 6) {
        my $data = $namerecord->{'Kids'}->[0]->{'Text'};
        $data =~ s/^\s*//;
        $data =~ s/\s*$//;
        $name[$namerecord->{'nameID'}] = $data;
    }
}

# Load post
foreach my $entry (@{$tables{'post'}}) {
    $italicAngle = $entry->{'value'} if $entry->isa('italicAngle');
    $isFixedPitch = $entry->{'value'} if $entry->isa('isFixedPitch');
    $underlinePosition = round($entry->{'value'} * $scaleFactor) if $entry->isa('underlinePosition');
    $underlineThickness = round($entry->{'value'} * $scaleFactor) if $entry->isa('underlineThickness');
}

# Load cmap (encoding)
my $cmap;
foreach my $map (@{$tables{'cmap'}}) {
    if ($map->isa('cmap_format_4') && $map->{'platformID'} == 0) {
        $cmap = $map;
        last;
    }
}
die("Didn't find cmap") unless $cmap;
foreach my $entry (@{$cmap->{'Kids'}}) {
    next unless $entry->isa('map');
    $encoding{hex($entry->{'code'})} = $entry->{'name'};
}

# Load hmtx (Horizontal Metrics Table)
foreach my $mtx (@{$tables{'hmtx'}}) {
    next unless $mtx->isa('mtx');
    $glyphMetrics{$mtx->{'name'}}->{'width'} = round($mtx->{'width'} * $scaleFactor);
    $glyphMetrics{$mtx->{'name'}}->{'lsb'} = round($mtx->{'lsb'} * $scaleFactor);
}

# Load bounding boxes from glyf
foreach my $glyph (@{$tables{'glyf'}}) {
    next unless $glyph->isa('TTGlyph');
    my $name = $glyph->{'name'};
    $glyphMetrics{$name}->{'xMin'} = round(($glyph->{'xMin'} || 0) * $scaleFactor);
    $glyphMetrics{$name}->{'yMin'} = round(($glyph->{'yMin'} || 0) * $scaleFactor);
    $glyphMetrics{$name}->{'xMax'} = round(($glyph->{'xMax'} || 0) * $scaleFactor);
    $glyphMetrics{$name}->{'yMax'} = round(($glyph->{'yMax'} || 0) * $scaleFactor);
}

# Load kerning pairs
foreach my $subtable (@{$tables{'kern'}}) {
    next unless $subtable->isa("kernsubtable") && $subtable->{"format"} == 0;
    foreach my $pair (@{$subtable->{'Kids'}}) {
        next unless $pair->isa("pair");
        push(@kerningPairs, {l => $pair->{'l'}, r => $pair->{'r'}, v => round($pair->{'v'} * $scaleFactor)});
    }
}

# print "Keys: " . join(", ", keys(%{$name[0]})) . "\n";

print "StartFontMetrics 4.1\n";
print "FontName $name[6]\n";
print "FullName $name[4]\n";
print "Notice $name[0]\n";
print "EncodingScheme FontSpecific\n";
print "FamilyName $name[1]\n";
print "Weight $name[2]\n";
print "Version $name[5]\n";
print "Characters " . @glyphNames . "\n";
print "ItalicAngle $italicAngle\n";
print "Ascender $ascender\n";
print "Descender $descender\n";
print "UnderlineThickness $underlineThickness\n";
print "UnderlinePosition $underlinePosition\n";
print "IsFixedPitch " . ($isFixedPitch ? 'true' : 'false') . "\n";
print "FontBBox $fontMetrics{xMin} $fontMetrics{yMin} $fontMetrics{xMax} $fontMetrics{yMax}\n";

print "StartCharMetrics " . scalar(keys %encoding) . "\n";
foreach my $code (sort { $a <=> $b } keys %encoding) {
    my $name = $encoding{$code};
    my $m = $glyphMetrics{$name};
    my $index = $glyphNames{$name};
    my $width = $m->{'width'} || 0;
    print "C $code ; WX $width ; N $name ; B $m->{xMin} $m->{yMin} $m->{xMax} $m->{yMax} ; I $index ;\n"
}
print "EndCharMetrics\n";

print "StartKernData\n";
print "StartKernPairs " . scalar(@kerningPairs) . "\n";
foreach my $pair (@kerningPairs) {
    print "KPX $pair->{l} $pair->{r} $pair->{v}\n";
}
print "EndKernPairs\n";
print "EndKernData\n";

print "EndFontMetrics\n";

print("\n");
