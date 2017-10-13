## Developer Notes

These are internal developer notes.

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

One problem with this is that we end up having to copy the strings several
times, including complex piecewise copies to insert the tags in the middle of
strings.

The alternative is to compute the width of every dubious character ourselves and
figure out the byte position of the wrap points.  The main issue with this is
that we have to create the STRSXP for use with R_nchar
