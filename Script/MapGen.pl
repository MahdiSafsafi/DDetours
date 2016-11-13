# Author: Mahdi Safsafi
# https://github.com/MahdiSafsafi/delphi-detours-library
open( MF, ">x86.map" ) || die "Could not open map file.";
print MF"# Original map: x86-opcode-map.txt\n";
open( F, 'x86-opcode-map.txt' );

while (<F>) {
	chomp;
	next if (/^#/);
	next if (/^$/);
	next if (/^(Referrer|AVXcode):/);
	$_ =~ s/\Qone byte opcode\E/Table1/;
	$_ =~ s/\Q2-byte opcode (0x0f)\E/Table2/;
	$_ =~ s/\Q3-byte opcode 1 (0x0f 0x38)\E/Table38/;
	$_ =~ s/\Q3-byte opcode 2 (0x0f 0x3a)\E/Table3A/;

	$_ =~ s/([0-9a-f]+:)\s*$/$1 InvalidOpCode/;
	print MF "$_\n" if (/(Grp)*Table:/);
	print MF "$_\n" if (/EndTable/);

	if (/^([0-9a-f]+):\S*(.+)$/) {
		my $op   = $1;
		my $ns   = $2;
		my @list = split( /\|/, $ns );
		my @ins;
		foreach (@list) {
			my $mnems;
			if (/\s*(.+?)(\s|$)/) {
				$mnems = $1;
			}
			else {
				die 'unable to match mnems.';
			}
			my @mnems_list     = split( /\//, $mnems );
			my $base_mnem      = $mnems_list[0];
			my @new_mnems_list = ($base_mnem);
			foreach (@mnems_list) {
				if ( ( $base_mnem ne $_ ) and ( lc($_) eq $_ ) ) {
					die "mnems are > 2!" if ( scalar(@mnems_list) != 2 );
					my $n    = length($_);
					my $idx  = length($base_mnem) - $n;
					my $mnem = $base_mnem;
					substr( $mnem, $idx, $n ) = $_;
					push( @new_mnems_list, $mnem );
				}
			}
			my $args = (/\s.+?\s(.+)$/) ? $1 : '';
			$mnems = join( '/', @new_mnems_list );
			my $s = "$mnems $args";
			push( @ins, $s );    #unless (/\(ev(o)*\)/);
		}
		$op = uc($op);
		my $s = "\$$op: " . join( '|', @ins );
		print MF "$s\n";
	}
}
close(MF);
close(F)
