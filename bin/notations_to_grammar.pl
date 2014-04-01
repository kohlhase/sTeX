## Retrieve all OMDoc notations from a given directory and construct the BNF needed for a Marpa grammar

#/usr/bin/perl -w
use strict;
use warnings;
use feature 'switch';

use XML::LibXML;
use Data::Dumper;
$Data::Dumper::Sortkeys = 1;
# Prepare STDERR and STDOUT for outputing Unicode characters
binmode(STDERR,':encoding(UTF-8)');
binmode(STDOUT,':encoding(UTF-8)');

# Traverse and Scan for OMDoc files
# e.g. 
# sTeX/bin$ perl notations_to_grammar.pl ~/localmh/MathHub/smglom/smglom/source/
my $root_directory = shift;

# Example of a generic Marpa grammar rule:
# Note that the final grammar is ideally in BNF form, as for example described in:
# http://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2013/01/dsl_simpler2.html

#my @rules = [{lhs=>'NAME',rhs=>'xml realization',action=>\&Semantics::Example}]; 
my @rules;

# Grab all the OMDoc files:
my @omdoc_files = split("\n",`find $root_directory -name "*.omdoc"`);
# For each file, 
# TODO: I only use the first 10 files as an example here, remove the [0..10] splice for the full set
foreach my $omdoc_file(@omdoc_files[0..10]) {
	print STDERR "File: $omdoc_file\n";
	# Parse in a DOM via LibXML
	my $doc = XML::LibXML->new(encoding=>'utf-8',recover=>1)->parse_file($omdoc_file);
	# Obtain all notation definitions:
	my $xpc = XML::LibXML::XPathContext->new($doc->documentElement);
	my @notations = $doc->findnodes('//*[local-name()="notation"]');
	# Build one grammar rule from each notation:
	foreach my $notation(@notations) {
		# Each rule needs a unique name, for now use "cd:name" from the attributes
		my $uname = $notation->getAttribute('cd').'__'.$notation->getAttribute('name');
		# The first two non-empty children are the prototype and rendering
		my ($prototype,$rendering) = grep {ref $_ eq 'XML::LibXML::Element'} $notation->childNodes;
		# print STDERR "Name: $uname\n";
		# print STDERR "P: ",$prototype->toString(1),"\n\n";
		# print STDERR "R: ",$rendering->toString(1),"\n\n";
		# For each notation we have to generic rules:
		# The main notation rule, which carries the semantics, and has a RHS of its rendering
		push @rules, {lhs=>$uname, rhs=>$uname."__rendering", action=>construct_action($prototype->firstChild)};
		# The generic principle that every notation is an expression, which can be used as an argument in other notations
		push @rules, {lhs=>'Expression', rhs=>$uname};
		# Construct the necessary grammar rules for this rendering (via recursive DFS traversal)
		my @constructed_rules = @{construct_rules($rendering)};
		# The first returned rule is the rule for the XML root of the rendering, and hence the main :rendering grammar rule
		$constructed_rules[0]->{lhs} = $uname."__rendering";
		# Add to our collection of rules
		push @rules, @constructed_rules;
	}
}

# Throw out glue rules, e.g. those without RHS or LHS:
@rules = grep {(ref $_) && $_->{lhs} && $_->{rhs}} @rules; 
# Also throw out rules with identical LHS, prioritizing the last ones (overwrites)
my %unique_rules = ();
foreach my $rule(@rules) {
	my $lhs = delete $rule->{lhs}; # We essentially hash all rules for a given LHS under it as key, so we delete it from the individual rule.
	my $rhs = $rule->{rhs};
	$unique_rules{$lhs} = [] unless defined $unique_rules{$lhs};
	# If we have never seen this RHS before, add it to the list of rules that derive this category (LHS):
	if (! grep {$_->{rhs} eq $rhs} @{$unique_rules{$lhs}} ) {
		push @{$unique_rules{$lhs}}, $rule; } }

# Print our pre-grammar datastructure:
# print STDERR "Constructed rules: \n";
# print STDERR Dumper(\%unique_rules);

# Create the BNF form:
my $BNF = ":start ::= Expression\n";
foreach my $LHS(sort keys %unique_rules) {
	foreach my $subrule (@{$unique_rules{$LHS}}) {
		my $RHS = $subrule->{rhs};
		$BNF .= "$LHS ::= $RHS\n";
	}
}
print STDERR "\n===\nBNF Form\n===\n";
print STDERR $BNF;

# TODO: Continue from here to create the BNF that Marpa::R2 understands
# TODO: Add support for the remaining MathML elements - msub, msup and so on.

### Helper routines:

sub construct_rules {
	my ($node) = @_;
	my $node_name = $node->nodeName;
	my @child_nodes = grep {ref $_ eq 'XML::LibXML::Element'} $node->childNodes;
	given ($node_name) {
		when ('omdoc:rendering') {
			my ($root) = shift @child_nodes;
			return construct_rules($root); }
		when ('m:mo') {
			my $lexeme = quote($node->textContent);
			return [{lhs=>$lexeme,rhs=>"moSTART '$lexeme' moEND"}]	}
		when ('m:mi') {
			my $lexeme = quote($node->textContent);
			return [{lhs=>$lexeme,rhs=>"miSTART '$lexeme' miEND"}]	}
		when ('m:mrow') {
			my @sub_rules = map {@{construct_rules($_)}} @child_nodes;
			my @top_names = map {$_->{lhs}} @sub_rules;
			return [{lhs=>"mrow:".uid(),rhs=>"mrowSTART ".join(" ",@top_names)." mrowEND"}, @sub_rules]	}
		when ('render') {
			# Render is a wildcard, any expression is valid here.
			return [{lhs=>'Expression'}];	}
		default {
			print STDERR "TODO: ",$node_name,"\n";
		}
	};
	return []; }

# TODO: Extrapolate meaningful semantic AST constructors, empty subroutine for now
sub construct_action {
	my ($prototype) = @_;
	return sub{}; }

# Simplest possible unique ID generator
my $uid=1;
sub uid {
	return $uid++; }

sub quote {
	# Note that we only want to quote the apostrophe, not use the full power of quotemeta, since we're not building a regex,
	# rather just normal string literals
	$_[0] =~ s/'/\\'/g; # This replaces all ' with \'
	return $_[0]; }