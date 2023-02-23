# 04.08.2009 Volker Pohlers
# erstellt dependecies-Datei für Arnold-Assembler

@ARGV = glob("@ARGV" || '*.asm');
#print "@ARGV\n";

while (<>) {
	if (/^\s+(binclude|include|addz80file|addfile)\s*"?(.*?)[",\s\n]/i) {
		#print "$_";
		#print "$ARGV: $2\n";
		$dependecies{$ARGV}{$2} = 1;	# $dependecies{file}{include}
	}
}

foreach (sort keys %dependecies) { 
	my $bin=$_;
	$bin =~ s/\..+/.bin/ unless $bin=~/.inc/;
	print "$bin:\t$_ ", join ( ' ', sort keys %{$dependecies{$_}} ), "\n" 
}
