package Sort::Tree;

#========================================================================
#
# Sort::Tree
#
# DESCRIPTION
#                                                                       
# Pair of routines for reorganizing a list into a parent/child tree,
# useful for generating directory-tree like displays.
#
# AUTHOR
#   Bryce Harrington <bryce@osdl.org>
#
# COPYRIGHT
#   Copyright (C) 2003 Bryce Harrington & Open Source Development Lab
#   All Rights Reserved.
#
#   This module is free software; you can redistribute it and/or
#   modify it under the same terms as Perl itself.
#
#------------------------------------------------------------------------
#
# Last Modified:  $Date: 2003/04/04 07:08:19 $
#
# $Id: Tree.pm,v 1.5 2003/04/04 07:08:19 bryce Exp $
#
# $Log: Tree.pm,v $
#
#========================================================================
=head1 Sort::Tree

This module includes two routines, list_to_tree and tree_to_list.  These
are used to organize an unordered list of objects into a tree form.  For
example, you'd perform a database query to gain a list of folders in a
document system, and then order them by parentage for display in a
webpage.

=head2 Usage

=head3 Simple example:

This is a bit trivially contrived but should illustrate the basic
functioning of the module:

use Sort::Tree;

my @creatures = (
                 { id => 1, class => -1, name => 'animal' },
                 { id => 2, class => 1,  name => 'mammal' },
                 { id => 3, class => 1,  name => 'bird' },
                 { id => 4, class => 1,  name => 'reptile' },
                 { id => 5, class => 2,  name => 'primate' },
                 { id => 6, class => 2,  name => 'feline' },
                 { id => 7, class => 5,  name => 'human' },
                 { id => 8, class => 6,  name => 'housecat' },
                 { id => 9, class => 3,  name => 'penguin' },
                 { id => 10,class => 4,  name => 'gecko' }
                 );

my @tree = Sort::Tree::list_to_tree(\@creatures, 'id', 'class');

foreach my $row (Sort::Tree::tree_to_list(\@tree,
                                          ['id'],
                                          [\&Sort::Tree::numerically],
                                          'class')) {
    print ' ' x $row->{class}, $row->{name}, "\n";
}

The following is displayed:

animal
 mammal
  primate
     human
  feline
      housecat
 bird
   penguin
 reptile
    gecko

=head3 Advanced example:

The following code is lifted from the make_tree routine of 'docsys'.  It
shows how to prepare data for being processed by Sort::Tree with the
intent of creating a fixed-depth tree HTML display.  Unlike the previous
example, this keeps track of the nesting level, cuts off at a defined
depth, and indicates where you'd hook in database and template
processing calls.  A lot of details are left out for conciseness, that
you'll need to flesh out.

my $idField = 'uid';
my $parentField = 'parent_id';
my $icon_spacer = 'blank.gif';  # 1x1 transparent GIF
my $depth = 3;

my @rows = ();

# Only descend into $depth level's worth of parents
for (my $level = 0; ($level<=$depth); $level++) {
  my $pfilter = '';
  last if ($#parents < 0);  # No parents left
  if ($level == 0) {
    # This is the root node
    $pfilter = "$idField=$parent_id";
  } elsif ($#parents == 0) {
    # There is only one parent
    $pfilter = "$parent_field=$parents[0]&&$idField<>$parent_id";
  } else {
    # There are multiple parents to select
    $pfilter = "$parent_field IN (" . join(',', @parents) . ")"
             . "&&$idField<>$parent_id";
  }

  # TODO:  Add your database select() handler for getfrom_db()...
  my $sth = $DB->getfrom_db($table, '', $pfilter, $sort, 0);
  @parents = ();

  last if ($sth == 0);

  # Iterate through the database items for additional operations
  while (my $data = $DB->getnext_db($sth)) {
    # Note the current level and fill in the {symbol} field
    $data->{level} = $level;
    if ($level == $depth) {
      $data->{symbol} = ($icon_spacer x $level) . $icon_folderclosed;
    } else {
      $data->{symbol} = ($icon_spacer x $level) . $icon_folderopen;
    }

    $data->{row_data} = my_make_row('foobar.tt2', $data);
    push @rows, $data;

    if ($level == 0) {
      @parents = ($data->{uid});
    } elsif ($data->{$idField} != $data->{$parent_field}) {
      # Add uid to the next @parents list
      push @parents, $data->{$idField};
    }
  }
}

# Reorganize data into tree form
my @rows = list_to_tree(\@rows, $idField, $parentField);

my $content = '';
foreach my $row (tree_to_list(\@rows, ['descriptor'],[\&alphabetically])) {
  next unless (defined ($row->{$idField};
  $content .= $row->{row_data} . "\n";  

  # TODO:  Do other stuff with this node (populate files into folders, etc.)
}

# TODO:  Do template stuff (add your header, footer, etc.)

print $content;

=cut

use strict;
use Carp;

require Exporter;


$Sort::Tree::ISA = qw( Exporter );
$Sort::Tree::VERSION = '1.06';
@Sort::Tree::EXPORT = qw( 
			  list_to_tree 
			  tree_to_list 
			  numerically 
			  alphabetically 
			  chronologically 
			  reverse_numerically 
			  reverse_alphabetically
			  reverse_chronologically 
			  );
@Sort::Tree::EXPORT_OK = qw( list_to_tree tree_to_list );
@Sort::Tree::EXPORT_TAGS = qw( 'all' => [ qw( list_to_tree tree_to_list ) ] );

use constant DEBUGGING => 0;

=head3 list_to_tree($list, $idField, $parentField)

Takes a list of queried objects and builds a tree, resorting it into
tree order and including the nesting level.  Inspired by DBIx::Tree.

=cut
sub list_to_tree {
    my ($list, $idField, $parentField) = @_;

    $idField ||= 'id';
    $parentField ||= 'parent_id';
    return () unless ($list);

    my $root_id = -1;

    warn "Using id field $idField and parent field $parentField\n" if DEBUGGING;

    my @tree;
    my %index;
    # Put objects into a nested tree structure
    foreach my $obj (@{$list}) {
	die "list_to_tree:  Object undefined\n" unless $obj;
	my $id = $obj->{$idField} || die "list_to_tree:  No $idField in object\n";
	my $pid = $obj->{$parentField} || $id;

	if ($root_id == -1) {
	    $pid = $id;
	    $root_id = $id;
	}

	warn "Adding object #$id to parent #$pid\n" if DEBUGGING;

	# Add object node to index
	if (defined $index{$id}) {
	    if (defined $index{$id}->{$idField}) {
		die "Sort::Tree::list_to_tree:  Duplicate object $id.\n";
	    } else {
		$obj->{kids} = $index{$id}->{kids};
		$index{$id} = $obj;
	    }
	} else {
	    $index{$id} = $obj;
	}

	# If this is a root object, put into tree directly
	if ($id == $pid) {
	    warn "Adding $id to tree\n" if DEBUGGING;
	    push @tree, $obj;

	    warn "Now there are ", $#tree+1, " items in tree\n" if DEBUGGING;
	# Add it as a child of the appropriate parent object
	} else {
	    warn "Adding $id as child of $pid\n" if DEBUGGING;
	    push @{$index{$pid}->{kids}}, $obj;
	}
    }   

    warn "Tree:  @tree  (", $#tree+1, " items)\n" if DEBUGGING;

    return @tree;
}

# Various sorting routines
#  Look at Date::Interval for comparing date ranges
#  Look at Sort::Versions for comparing version numbers
#  Look at Number::Compare for comparing file sizes (1G, 42k, etc.)
#  Look at Date::Manip for parsing dates for comparison
sub numerically {             my ($a,$b,$f) = @_;  $a->{$f} <=> $b->{$f} }
sub alphabetically {          my ($a,$b,$f) = @_;  $a->{$f} cmp $b->{$f} }
sub chronologically {         my ($a,$b,$f) = @_;  $a->{$f} cmp $b->{$f} }
sub reverse_numerically {     my ($a,$b,$f) = @_;  $b->{$f} <=> $a->{$f} }
sub reverse_alphabetically {  my ($a,$b,$f) = @_;  $b->{$f} cmp $a->{$f} }
sub reverse_chronologically { my ($a,$b,$f) = @_;  $a->{$f} cmp $b->{$f} }

=head3 tree_to_list(tree, cmpFields, cmpFuncs, idField, depth, max_depth)

Takes a tree and serializes it into a sorted list.  Recursive.
Inspired by DBIx::Tree (but not derived from it)

  Parameters:
    $tree - the tree data structure
    $cmpFields - Field to do comparison on (default idField)
    $cmpFuncs - Ordering function (default &numerically)
    $idField - 
    $depth - Depth to display (default 0)
    $max_depth - Maximum depth to display; -1 for all (default -1)

=cut
sub tree_to_list {
    my ($tree, $cmpFields, $cmpFuncs, $idField, $depth, $max_depth) = @_;

    # error checking
    die "No valid tree object" unless $tree;

    # Get the cmp items for the current level
    my $cmpField = shift @{$cmpFields} || $idField;
    my $cmpFunc = shift @{$cmpFuncs} || \&numerically;

    # Defaults
    $idField ||= 'id';
    $depth ||= 0;
    $max_depth ||= -1;

    # If we're at the end of the list, reuse last cmp item
    $cmpFuncs = [$cmpFunc] unless @{$cmpFuncs};
    $cmpFields = [$cmpField] unless @{$cmpFields};

    while (! defined $tree->[0]->{$cmpField} && @{$cmpFields}>0) {
	$cmpField = shift @{$cmpFields};
    }

    # Iterate through tree and generate sorted threaded list
    my @list;
    foreach my $node (sort { &$cmpFunc($a,$b,$cmpField);
			    } @{$tree}) {
	$node->{depth} = $depth;
	push @list, $node;

	# If this obj has children, sort & parse those next
	if (defined $node->{kids} && $depth != $max_depth) {
	    push @list, tree_to_list($node->{kids},
				     $cmpFields,$cmpFuncs,
				     $idField,$depth+1,$max_depth);
	}
    }

    return @list;
}


#========================================================================
# Subroutines
#------------------------------------------------------------------------


__END__

=head1 NAME



=head1 SYNOPSIS

use 
=head1 DESCRIPTION

B<Sort::Tree> will...


=head1 METHODS



=head1 PREREQUISITES

This script requires the C<strict> module.  It also requires
C<foobar 1.00>.

=head1 COREQUISITES

CGI

=head1 SCRIPT CATEGORIES

CPAN/Administrative

=head1 BUGS

None known.

=head1 VERSION

0.00
Distributed as part of  version ,
released on .


=head1 SEE ALSO

L<perl(1)>

=head1 AUTHOR

Bryce Harrington E<lt>bryce@osdl.orgE<gt>

L<http://www.osdl.org/|http://www.osdl.org/>

=head1 COPYRIGHT

Copyright (C) 2003 Bryce Harrington.
All Rights Reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 REVISION

Revision: $Revision: 1.5 $

=cut
1;
