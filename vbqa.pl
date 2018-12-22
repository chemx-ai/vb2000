#! /usr/bin/perl
# Copyright (C) 2017 Jiabo Li, Brian Duke, and Roy McWeeny
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 
#############################################################################
#
# Perl script for VB2000 QA
#
# Author:
# Jiabo Li
# SciNet Technologies, 2005
#
# Updated July 2018 to allow general directory name as parameter 
# with TEMPOUT as default.
# Brian J. Duke
#
#############################################################################

my $BaseEnergy;
my $BaseCPUTime;
my $TestEnergy;
my $TestCPUTime;
my $TotalTime;
my $run;
my $fail;

$run =  0;
$fail =  0;
$TotalTime = 0;

my $Directory = $ARGV[0];
if ($Directory eq "")  {
  $Directory = "TEMPOUT";
}
# Default file is TEMPOUT

print " Useage: ./vbqa.pl directory name where results are.\n";
print " If directory name is absent - TEMPOUT is assumed.\n\n";

print " Comparing TESTOUT and $Directory \n\n";
if ((! -d "./TESTOUT") || (! -d "./$Directory" )) {
   print "TESTOUT and/or $Directory does not exists\n";
   exit;
}

foreach $file ( b2h6DZvb12,c2h2ba10,c2h2ba6,c2h2DYN,c2h2oldbanana,
  c2h2TB1,c2h2TB2,c2h2TB3,c2h2TB4,c2h2vb10,c2h2vb10bsc,c2h2vb3s,
  c2h4vb12,c2h6vb14,c6h6vb,c6h6vb14,ch3ohvb10,ch3ohvb10scci,ch4gvb4,
  ch4vb8,ch4vb8sc,clch3cl,clch3clSN2s,ethanol,ethanoldot,h1,h13,
  h2bovb,h2cc2DP,H2Fd95,h2oDZcasvb4,h2oDZcasvb8in6,h2oDZvbDLMO,
  h2olocal,h2triplet,h3bovb,h3vb1,h4casvb,h4scvb,HCld95,hfsplit3,
  ohlocal,test1,test10,test11,test12,test13,test2,test3,test4,
  test5,test6,test7,test8,test9,test90,tmmDZ6s0,tmmDZ6s1,tmmDZsinglet,
  tmmDZtriplet
){
  $TestName = $file.".out";
  $BASEFILE = "./TESTOUT/" . $TestName;
  $TESTFILE = "./$Directory/" . $TestName;
  print " Test Report for $TestName \n";
  CompareFinalE_and_TotalTime($BASEFILE,$TESTFILE,$TotalTime);
  $TotalTime = $TotalTime + $TestCPUTime;
}

print " Total time: $TotalTime     Number run: $run     Number fail: $fail\n\n";

sub CompareFinalE_and_TotalTime {

  $TestEnergy = 0;
  $TestCPUTime= 0;

  open(BASEFILE,"$BASEFILE");
  $BASEFILE = $_[0];
  open(BASEFILE,"$BASEFILE");

  while (<BASEFILE>) {
    if(/ENERGY AND DIFF/) { $BaseEnergy = substr($_,36,20) }
    if(/TOTAL CPU TIME/) { $BaseCPUTime = substr($_,28,11) }
  }

  close(BASEFILE);

  $TESTFILE = $_[1];
  open(TESTFILE,"$TESTFILE");

  while (<TESTFILE>) {
    if(/ENERGY AND DIFF/) { $TestEnergy = substr($_,36,20) }
    if(/TOTAL CPU TIME/) { $TestCPUTime = substr($_,28,11) }
  }

  close(TESTFILE);

  print " ----------------------------------------------------------------\n";
  print "             Baseline                Test             Status     \n";
  $DeltaEnergy = $TestEnergy-$BaseEnergy;
  $Status      = "OK";
  $run = $run + 1;
  if(abs($DeltaEnergy) > 0.000002) {$Status = "Fail"; $fail = $fail + 1}
  print " Energy  $BaseEnergy   $TestEnergy  $Status \n";
  $DeltaTime = $BaseCPUTime-$TestCPUTime;
  print " CPU time$BaseCPUTime            $TestCPUTime            $DeltaTime \n\n";

}

