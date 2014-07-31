package Getopt::Again;
use strict;
use warnings;

use Carp qw/croak/;
use Scalar::Util qw/reftype/;

our $VERSION = '0.000002';

my %DEFAULTS = (
    ALL => {default => undef, list => 0, example => '', required => 0, process => undef},

    bool   => {default => 0},
    string => {example => ' "foo"'},
    path   => {example => ' "/foo/bar/"'},
    file   => {example => ' "/foo/bar.txt"'},
    regex  => {},
);

my %VALID_FIELDS = (
    type        => 1,
    list        => 1,
    default     => 1,
    example     => 1,
    process     => 1,
    description => 1,
    split_on    => 1,
    name        => 1,
    regex       => 1,
);

sub import {
    my $class  = shift;
    my $caller = caller;

    my $meta = $class->new(@_);

    my %exports = (
        opt_meta => sub { $meta },

        opt_bools => sub { $meta->add($_, type => 'bool',   default => 0) for @_ },
        opt_lists => sub { $meta->add($_, type => 'string', list    => 1) for @_ },

        opt_params => sub { $meta->add($_, type => 'string') for @_ },
        opt_paths  => sub { $meta->add($_, type => 'path')   for @_ },
        opt_files  => sub { $meta->add($_, type => 'file')   for @_ },

        opt_add   => sub { $meta->add(@_) },
        opt_usage => sub { $meta->usage_string(@_) },
        opt_help  => sub { $meta->help_string(@_) },
        opt_parse => sub { $meta->parse(@_) },

        opt_use_help => sub { $meta->help(@_) },
    );

    for my $name (keys %exports) {
        no strict 'refs';
        *{"$caller\::$name"} = $exports{$name};
    }
}

sub new {
    my $class  = shift;
    my %params = @_;

    my $self = bless {}, $class;

    for my $i (keys %params) {
        croak "Not an option '$i'" unless $self->can($i);
        $self->$i($params{$i});
    }

    return $self;
}

sub _named_params {
    my $self = shift;
    $self->{_named_params} ||= {};
    return $self->{_named_params};
}

sub _pattern_params {
    my $self = shift;
    $self->{_pattern_params} ||= [];
    return $self->{_pattern_params};
}

sub _all_params {
    my $self = shift;
    $self->{_all_params} ||= [];
    return $self->{_all_params};
}

sub _clear_opt_cache {
    my $self = shift;
    delete $self->{opt_cache};
    return;
}

sub _gen_opt_cache {
    my $self = shift;

    my %seen;
    my %opts;
    for my $config (@{$self->_all_params}) {
        my ($f) = split '', $config->{name};
        if ($seen{$f}++) {
            delete $opts{$f}; # More than 1
        }
        else {
            $opts{$f} = $config;
        }
    }

    return {%opts, %{$self->_named_params}};
}

sub _reversed_opt_cache {
    my $self = shift;

    my $out = {};
    my $cache = $self->{opt_cache} ||= $self->_gen_opt_cache;
    for my $key (keys %$cache) {
        my $opt = $cache->{$key};
        if ($key eq $opt->{name}) {
            unshift @{$out->{$opt->{name}}} => $key;
        }
        else {
            push @{$out->{$opt->{name}}} => $key;
        }
    }

    return $out;
}

sub opt_spec {
    my $self = shift;
    my ($opt) = @_;

    $self->{opt_cache} ||= $self->_gen_opt_cache;

    my $config = $self->{opt_cache}->{$opt};
    return $config if $config;

    for my $config (@{$self->_pattern_params}) {
        next unless $opt =~ $config->{regex};
        return $config;
    }

    return;
}

sub add {
    my $self = shift;
    my ($name, @params) = @_;

    croak "name is required, and must not be a ref (got: " . ($name || '(UNDEF)') . ")"
        unless defined $name && !ref $name;

    $self->_clear_opt_cache;

    my %params = @params > 1 ? (@params) : (process => $params[0]);
    my $config = $self->process_opt(%params, name => $name);
    my $alias = $config->{alias};

    croak "Alias must be a string, or an array of strings"
        unless reftype $alias eq 'ARRAY';

    push @{$self->_all_params} => $config;

    if($config->{regex}) {
        push @{$self->_pattern_params} => $config;
    }

    for my $name (@$alias) {
        croak "Conflict, option '$name' was already defined by " . $self->_named_params->{$name}->{name}
            if $self->_named_params->{$name} && $self->_named_params->{$name} != $config;

        $self->_named_params->{$name} = $config;
    }
}

sub process_opt {
    my $self = shift;
    my %params = @_;

    my $type = delete $params{type};
    $type ||= ($params{regex} || length($params{name}) > 1)
        ? 'string'
        : 'bool';

    my $process = delete $params{process};
    croak "process must either be a coderef or a regexp (got: $process)"
        if $process && !(ref $process && reftype($process) =~ m/^(REGEXP|CODE)$/);

    my $alias = delete $params{alias} || [];
    $alias = [$alias] unless ref $alias;
    croak "alias must be a string, or an array strings, got: $alias"
        unless reftype $alias eq 'ARRAY';
    unshift @$alias => $params{name};

    for my $field (keys %params) {
        croak "'$field' is not a valid option field"
            unless $VALID_FIELDS{$field};
    }

    return {
        %{$DEFAULTS{ALL}},
        %{$DEFAULTS{$type}},
        %params,
        type  => $type,
        alias => $alias,
        process => $process,
    };
}

sub parse {
    my $self = shift;
    my @in = @_;

    my (%out, @out);

    my $no_parse = 0;
    while(my $item = shift @in) {
        if ($item eq '--') {
            $no_parse = 1;
            next;
        }

        if ($item =~ m/^(-{1,2})([^=]+)(?:=(.*))?$/ && !$no_parse) {
            my ($dash, $arg, $val) = ($1, $2, $3);
            $dash = length($dash);

            if($val) {
                $val =~ s/^'(.+)'$/$1/;
                $val =~ s/^"(.+)"$/$1/;
            }

            my @args;
            if ($dash > 1) {
                @args = ([$arg, $val]);
            }
            else {
                @args = split '', $arg;
                push @args => [ pop(@args), $val ];
            }

            for my $set (@args) {
                $self->_register($set, \@in, \%out);
            }
        }
        else {
            push @out => $item;
        }
    }

    $self->_populate(\%out);
    $self->_process(\%out);

    return (\%out, \@out);
}

sub _register {
    my $self = shift;
    my ($set, $in, $out) = @_;
    my ($name, $val);

    if (ref $set) {
        ($name, $val) = @$set;
    }
    else {
        $name = $set;
    }

    my $spec = $self->opt_spec($name);
    croak "Unknown option: '$name'" unless $spec;

    if ($spec->{type} eq 'bool') {
        $out->{$spec->{name}} = !$spec->{default} ? 1 : 0;
    }
    else {
        if ( $spec->{type} eq 'regex' && $name =~ $spec->{regex}) {
            $val = $1 || $';
        }
        elsif (!defined $val) {
            $val = shift @$in;
        }

        if ($spec->{list}) {
            $out->{$spec->{name}} ||= [];
            push @{$out->{$spec->{name}}} => $val;
        }
        else {
            $out->{$spec->{name}} = $val;
        }
    }
}

sub _populate {
    my $self = shift;
    my ($out) = @_;

    for my $opt (@{$self->_all_params}) {
        next if defined $out->{$opt->{name}};
        $out->{$opt->{name}} = $opt->{default};
        $out->{$opt->{name}} ||= [] if $opt->{list};
    }
}

sub _process {
    my $self = shift;
    my ($out) = @_;

    for my $opt (keys %$out) {
        my $spec = $self->opt_spec($opt);
        next unless defined $out->{$opt};

        # Split if requested
        $out->{$opt} = [ map { split $spec->{split_on}, $_ } @{$out->{$opt}} ]
            if $spec->{list} && defined $spec->{split_on};

        if ($spec->{process}) {
            my $type = reftype $spec->{process};
            if ($type eq 'CODE') {
                local $_ = $out->{$opt};
                $out->{$opt} = $spec->{process}->($out->{$opt});
            }
            else {
                my $items = $spec->{list} ? $out->{$opt} : [ $out->{$opt} ];
                for my $item (@$items) {
                    next if $item =~ $spec->{process};
                    die "Invalid value for '$opt', got: '$item'\n";
                }
            }
        }
    }
}

sub usage_string {
    my $self = shift;
    ($self->{usage_string}) = @_ if @_;
    $self->{usage_string} ||= "[-short_flags] [--long_flag] [--option '...'] [--option=...] arg1, arg2";
    return "$0 $self->{usage_string}";
}

sub help_string {
    my $self = shift;
    my $names = $self->_reversed_opt_cache;

    print "Options:\n";

    my $out = "";
    for my $name (sort keys %$names) {
        my $opt = $self->opt_spec($name);
        $out .= "  $name ($opt->{type})\n";
        for my $alias (@{$names->{$name}}) {
            $out .= "   " . (length($alias) > 1 ? '--' : '-') . $alias . $opt->{example} . "\n";
        }
        if ($opt->{regex}) {
            $out .= "   --$opt->{regex}\n"
        }
        $out .= $opt->{description} . "\n" if $opt->{description};
        $out .= "\n";
    }

    return $out;
}

# Inject a help parameter that exits 0
sub enable_help {
    my $self = shift;
    my ($bool) = @_;
    return unless $bool;

    $self->add('help' => (
        type => 'bool',
        alias => 'h',
        process => sub {
            return unless $_;
            print $self->usage_string;
            print "\n";
            print $self->help_string;
            print "\n";
            $self->_exit(0);
        },
    ));
}

sub _exit { shift; exit $_[0] };

1;

__END__

=pod

=head1 NAME

Getopt::Again - Yet another attempt at a universal Getopt tool.

=head1 DESCRIPTION

Getopt tool that takes short form C<-abc> and/or long form C<--alpha> options.
Options may be boolen, or accept values. Values can be specified via
C<--foo val> or C<--foo=val> forms. Can also use a regex to slurp up matching
options. Ability to auto-generate usage and help strings for you when desired.

=head1 ANOTHER GETOPT TOOL!!!!

Yeah... Sorry about that. Here is a relevant XKCD: L<http://xkcd.com/927/>.

I have not yet found a Getopt module that works the way I would like. It may
exist, but the author is doing a poor job of reaching me and my searches have
failed.

I went a long time refusing to add yet more clutter to the Getopt namespace,
but I am tired of writing custom parsing for options every time I write a
script. So here it is, my way, and I don't care if nobody ever uses it, I won't
force you.

=head1 SYNOPSYS

=head2 SCRIPT

    use Getopt::Again enable_help => 1; # Default is 0

    opt_bools  qw/alpha beta/;  # Quickly define some boolean options
    opt_lists  qw/gamma zeta/;  # Quickly define some options that accept multiple args
    opt_params qw/foo bar/;     # Quickly define some options that accept a single arg
    opt_paths  qw/origin dest/; # Quickly define some options that take directory paths
    opt_files  qw/config/;      # Quickly define some options that take file paths

    # All arguments to opt_add are optional except the name.
    opt_add my_opt => (
        type        => 'string',           # Or 'bool', 'regex', 'path', 'file'
        list        => 1,                  # True to take multiple values
        default     => 'stuff',            # Default value
        example     => " 'a string'",      # Example value, for help/usage
        alias       => [ 'the_opt', 'm' ], # Alternate names
        process     => sub { ... },        # Can also be a validation regex
        description => "My option ..."     # For help/usage
        split_on    => ",",                # Split values (list only)
        regex       => qr/^foo-(.+)$/,     # Match options like this, use the capture for value
    );

    my $usage = opt_usage();
    my $help  = opt_help();

    # Get the options and args from @ARGV. @ARGV is not altered
    my (%opts, @args) = opt_parse(@ARGV);

=head2 OOP

    use Getopt::Again();

    my $ga = Getopt::Again->new(enable_help => 1);

    $ga->add('my_opt' => (
        type        => 'string',           # Or 'bool', 'regex', 'path', 'file'
        list        => 1,                  # True to take multiple values
        default     => 'stuff',            # Default value
        example     => " 'a string'",      # Example value, for help/usage
        alias       => [ 'the_opt', 'm' ], # Alternate names
        process     => sub { ... },        # Can also be a validation regex
        description => "My option ..."     # For help/usage
        split_on    => ",",                # Split values (list only)
        regex       => qr/^foo-(.+)$/,     # Match options like this, use the capture for value
    ));

    my $usage = $ga->usage_string();
    my $help  = $ga->help_string();

    # Get the options and args from @ARGV, @ARGV is not altered
    my (%opts, @args) = $ga->parse(@ARGV);

=head1 PARSING RULES

=over 4

=item A single dash denotes 1-character options, so C<-xyz> is seen as the x, y, and z options.

    command -xyz

    command -x -y -z

    command --xerxes --yolk --zebra

=item The final option in a set of single dash options may have an argument.

    command -xf thing

    command -xyzf thing

=item Double dash denotes long form C<--foo> is seen as the 'foo' option.

    command --thing

=item Options that accept arguments may take the C<--opt=val> or C<--opt val> forms.

    command --thing=val

    command --thing val

=item Options that can have a list of values may specify a delimiter, and/or can be listed multiple times.

    command --thing x --thing y --thing z

    command --thing x,y,z

    command --thing x.y.z

=item When it is not ambiguous, you may use the first character of the long form in a short form.

These work:

    command --xerxes
    command -x

This would not:

    command --xerxes
    command --xandu
    command -x # Nope! ambiguous (unless -x is defined directly)

=item End of params is C<--> as expected, nothing after this will be parsed, instead they are added to the list of arguments.

    command -xyz -- --this_is_not_an_opt --this_is_an_arg

=back

=head1 EXPORTS

=over 4

=item $ga = opt_meta()

Get the underlying Getopt::Again object.

=item opt_bools(qw/awake alive/)

Quickly define some boolean options.

=item opt_lists(qw/friends family/)

Quickly define some list options.

=item opt_params(qw/name age/)

Quickly define some options that take strings.

=item opt_paths(qw/origin dest/)

Quickly define some options that take paths (they must exist).

=item opt_files(qw/input config/)

Quickly define some options that take files (they must exist).

=item opt_add(NAME, %OPTIONS)

Add an option.

=item $usage = opt_usage()

=item opt_usage($set)

Get or set the usage string (returns "$0 $string", do not include $0 when
setting).

=item $help = opt_help()

Get the help string.

=item ($opts, $args) = opt_parse(...)

Parse a parameters (@ARGV is the usual argument).

An exception will be thrown if an unknown option is specified.

$opts is a hashref, $args is an arrayref.

=item opt_use_help()

Enable the --help and -h options.

=back

=head1 OPTION PROPERTIES

=over 4

=item type

Values may be one of: C<'string', 'bool', 'regex', 'file', 'path'>.

If none is specified it is guessed. Names with more than 1 character default to
string, named with 1 character only default to bool.

=item list

True or false. When true the option will always be an arrayref containing 0 or more values.

=item default

Specify the default value when none was specified. For bools this gets
normalized to true or false. For lists the default is a new empty array.

=item example

Example for documentation, usually an example value prefixed with either a space or equal sign:

    example => ' foo',

or

    example => '=foo',

=item process

Post-processing on the value. If this is a regex than an exception will be
thrown for any value that does not match.

The value may also be a coderef. For the coderef C<$_> will be set to the value
being processed. The value will also be passed in as the only argument.
Whatever the coderef returns will replace the value, so this is a hook to allow
you to modify the value.

=item description

Provide a long description of the option for the help string.

=item split_on

Specify a delimiter for list options.

=item regex

Specify a regex to be used to identify parameters. Use this if you want
something like C<enable-*> to be captured.

    list  => 1,
    regex => qr/^enable-(.*)$/,

This will match all the following:

    --enable-food
    --enable-protection

In these cases the value will be set to the first capture group, so in this case:

    [
        'food',
        'protection'
    ]

If there is no capture C<$1>, then the remaining string after the match C<$'>
will be used.

=back

=head1 OBJECT METHODS

=over 4

=item $ga->opt_spec($name)

Get the specification for an option.

=item $ga->add($name => %properties)

Add an option

=item ($opts, $args) = $ga->parse(@ARGV)

Parse a parameters (@ARGV is the usual argument).

An exception will be thrown if an unknown option is specified.

$opts is a hashref, $args is an arrayref.

=item $ga->usage_string

Get or set the usage string (returns "$0 $string", do not include $0 when
setting).

=item $ga->help_string

Get the help string

=item $ga->enable_help

Enable the help (C<--help> and C<-h>) tools.

=back

=head1 SEE ALSO

Everything in the massive Getopt namespace.

B<On second thought, don't, you do not have that much time>.

=head1 AUTHORS

Chad Granum L<exodist7@gmail.com>

=head1 COPYRIGHT

Copyright (C) 2014 Chad Granum

Getopt-Again is free software; Standard perl license (GPL and Artistic).

Getopt-Again is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the license for more details.

=cut

