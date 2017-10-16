## Developer Notes

These are internal developer notes.

### What Escape Sequences do we consider?

Seems like the natural thing is to do CSI sequences, and maybe for simplicity
just focus on the SGR variety.  But even with SGR, there is some question as to
how we interpret 'cuter' variants.

Some issues to consider:

* What to do with "weird" characters (e.g. symbols) in the middle of a sequence?
  It appears this is undefined behavior.
* What about the intermediate bytes?  This is probably the most problematic one
  as there are no examples for how they are used.
* What about potentially incorrect usage?  Do we warn?

Seems we should go by the strict defini

### Interface

* `state_at_pos`
* `find_csi`: forward state object to next CSI
* `parse_csi`: update state object with CSI data
* `parse_csi_token`: part of the above?

### Functions

* `substr`
* `strip_ansi`
* `strwrap`
* `trim`

### What do we do Next?

* Spend time figuring out how to integrate character width?
* All C substr?
* All C strwrap?
* Tab resolution?
* Generating a table of state vs. position

### State vs. Position

Should position be in bytes?  Probably in characters to use in combination with
ansi_strsub or whatever that ends up called.

### What About Strip and Wrap?

We could:

* record position and content of all ANSI tags,
* strip them,
* `strwrap`
* Re-insert tags
    * This will require counting bytes on every chunked line

One problem with this is that we end up having to copy the strings several
times, including complex piecewise copies to insert the tags in the middle of
strings. Another issue specific to `strwrap` is that `strwrap` takes several
liberties with the strings, so we would have to make sure that they are in a
form that `strwrap` won't feel compelled to modify, otherwise we'll end up
offset.

What about `substr`?  Could we just do the same.  Strip all tags, run the
`substr` normally, and then inject the tags back?  But with `substr` we will
have no idea what the byte positions of the substrings are, so this is probably
not an option.

For `strwrap` less of an issue because we should have the entire string so we
can count from the beginning.  However, with the white space modification and
indents and all that business it starts getting hairy to keep track of
everything.

Another problem with this is that we pretty much have to process the tags on the
whole string as we can't be sure how long it is.  E.g. if the user asks for 20
characters for `substr`, we can't stop because we don't know how many bytes it
would take to get to twenty characters.

Another problem is that we have to issue multiple R calls.

The alternative is to compute the width of every dubious character ourselves and
figure out the byte position of the wrap points.  The main issue with this is
that we have to :

* Create the STRSXP for use with R_nchar.
    * Need to compute the bytes for each element, including UTF-8, etc
    * Possibly need to figure out combining characters (e.g. emojis)
* Need a vector as long as # of characters to keep track of offsets or TRUE
  position, or, more likely, for each character the position?  We'll need the
  length too.

So we need to track the ANSI offsets as well as the UTF8 many bytes to one or
many bytes to many.  So we need for each character, it's offset, or for each
special character sequence, the offset it adds?  For stwrap we need to track
both, for substr we only need to track the number of characters, not their
display width.

Do we want to do this on an as-needed basis?  Possibly, only issue is if we use
`substr` with the same string repeated and overlapping cuts.  I guess we just
need to record the byte position of each start/end spot, so we can still do on
an as needed basis.

So we will walk the string until we pass all the cut points.


