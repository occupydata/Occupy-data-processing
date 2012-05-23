# Subclass of WWW::Mechanize I wrote some years back to add a few things
# useful for scraping. --Keith C. Ivey <keith at iveys dot org>, May 2012

# Copyright (c) 2012 Keith C. Ivey
#   
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights 
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# Contact: keith at iveys dot org

package ScraperBot;
use strict;
use base 'WWW::Mechanize';
use LWP 5.802; # version needed for easy handling of compressed responses

# Perl versions earlier than 5.7.1 need Encode::compat to get
# decoded_content() to work
if ( $] < 5.007001 ) {
	require Encode::compat;
}

sub new {
	my $class = shift;
	my %params = @_;
	my $cache_time = delete $params{cache_time};
	my $cookie_file = delete $params{cookie_file};
    my $debug = delete $params{debug};
	$params{autocheck} = 1 unless exists $params{autocheck};
	$params{stack_depth} = 3 unless exists $params{stack_depth};
	my $self = $class->SUPER::new(%params);
	bless $self, $class;
	# Pretend to be IE 6:
	$self->agent_alias('Windows IE 6');
	# Set header to allow compressed responses:
	$self->add_header('Accept-Encoding', 'gzip,deflate');
	if ($cookie_file) {
		# Allow cookies to be saved between runs:
		$self->cookie_jar(
			HTTP::Cookies->new(
				file => $cookie_file,
				autosave => 1,
			)
		);
	}
	$self->timeout(30);
	if ($cache_time) {
		require Cache::FileCache;
		require Storable;
		( my $namespace = lc $class ) =~ tr/a-z0-9/-/cs;
		my $cache_parms = {
			default_expires_in => $cache_time,
			auto_purge_interval => '8h',
            namespace => $namespace,
		};
		$self->{_cache} = Cache::FileCache->new( $cache_parms );
	}
    $self->{debug} = $debug;
	return $self;
}

# This _update_page() method is a copy of the one in WWW::Mechanize except
# for two lines where I've changed ->content to ->decoded_content to handle
# compressed responses.  Probably something similar will be in a later
# version of WWW::Mechanize itself.
sub _update_page {
    my ($self, $request, $res) = @_;

    $self->{req} = $request;
    $self->{redirected_uri} = $request->uri->as_string;

    $self->{res} = $res;

    # These internal hash elements should be dropped in favor of
    # the accessors soon. -- 1/19/03
    $self->{status}  = $res->code;
    $self->{base}    = $res->base;
    $self->{ct}      = $res->content_type || "";

    if ( $res->is_success ) {
        $self->{uri} = $self->{redirected_uri};
        $self->{last_uri} = $self->{uri};
    } else {
        if ( $self->{autocheck} ) {
            $self->die( "Error ", $request->method, "ing ", $request->uri, ": ", $res->message );
        }
    }

    $self->_reset_page;
    if ($self->is_html) {
        $self->update_html( $res->decoded_content || $res->content ); # was ->content (KCI)
    } else {
        $self->{content} = $res->decoded_content || $res->content; # was ->content (KCI)
    }

    return $res;
}

# This update_html() method adds destruction of the HTML tree object if it
# exists, so that it can be rebuilt if needed on the next request.
sub update_html {
	my $self = shift;
	$self->SUPER::update_html(@_);
	if ( $self->{tree} ) {
		# Call delete() method to break circular refs and avoid memory leak:
		$self->{tree}->delete;
		delete $self->{tree};
	}
}

# The uri() method is buggy in the current version of WWW::Mechanize (doesn't
# get updated on redirects), and it's better for it to return a URI object
# rather than a string anyway.
sub uri {
	my $self = shift;
	$self->response->request->uri;
}

# The tree() method is specific to this subclass, not overriding an inherited
# method.  It makes it easier to go through complex HTML structures using the
# methods from HTML::Element.
sub tree {
	my $self = shift;
	return unless $self->is_html;
	unless ( $self->{tree} ) {
		require HTML::TreeBuilder;
		$self->{tree} = HTML::TreeBuilder->new();
		$self->{tree}->implicit_body_p_tag(1);
		$self->{tree}->ignore_unknown(0);
		$self->{tree}->store_comments(1);
		$self->{tree}->parse($self->content);
		$self->{tree}->eof();
	}
	return $self->{tree};
}

# This DESTROY() method is just to destroy the tree object if it exists when 
# the object goes away.  Without that, there could be a memory leak.
sub DESTROY {
	my $self = shift;
	$self->{tree}->delete if $self->{tree}; # to avoid memory leak
	$self->SUPER::DESTROY() if $self->can('SUPER::DESTROY');
}

# The follow_refresh() method checks for a 0- or 1-sec Refresh HTTP header
# (which might have been a real header or one created from the appropriate
# META tag) and follows that if it exists.
sub follow_refresh {
	my $self = shift;
	my $requests = 0;
	while ( my $refresh = $self->response->header('Refresh') ) {
		$refresh =~ s/^[01];\s*(?:URL=)?//i or last;
		$self->get($refresh);
		$requests++;
		last if $requests >= 7;
	}
	return $requests;
}

sub get_redirect {
    my $self = shift;
    my $save = $self->max_redirect();
    $self->max_redirect(0);
    eval { $self->get(@_); };
    $self->max_redirect($save);
    return $self->response->header('Location');
}

# The LWP::UserAgent clone() method for some reason doesn't preserve the
# cookie jar.  Also, any property that's an object or reference ends up 
# being the same as the object or reference in the original ScraperBot.
# So this clones everything (actually, embedded coderefs get copied,
# but that shouldn't hurt anything in this case).
sub clone {
	my $self = shift;
	require Clone::PP;
	return Clone::PP::clone($self);
}

# The is_cached() and _make_request() methods are mostly copied from
# WWW::Mechanize::Cached so we can have caching when we want it but 
# not when we don't.
sub is_cached {
	my $self = shift;
	return $self->{_is_cached};
}

sub _make_request {
	my $self = shift;
	my $request = shift;

	my $cache = $self->{_cache};

	my $req = $request->as_string;
	my $response = $cache && $cache->get( $req );
	if ( $response ) {
		$response = Storable::thaw($response);
		$self->{_is_cached} = 1;
    }
	else {
		$response = $self->SUPER::_make_request( $request, @_ );
        if ($self->{debug}) {
            print STDERR "REQUEST (", $request->uri->as_string, "):\n",
                $request->as_string, "\n";
            print STDERR "RESPONSE:\n", $response->status_line, "\n",
                $response->headers_as_string, "\n";
        }
		eval { $cache->set( $req, Storable::freeze($response) ); } if $cache;
		$self->{_is_cached} = 0;
    }

	# An odd line to need.
	$self->{proxy} = {} unless defined $self->{proxy};

	return $response;
}

1;
