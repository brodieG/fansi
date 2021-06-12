# Memory Allocation Notes

## Overview

As part of the 1.0 release we improved the handling of internal buffer
allocations to ensure we do not strand older unneeded allocations on the stack
when we grow the buffer (i.e. the previously allocated smaller buffer should be
made eligible for GC.  This is done by keeping track of the `vmaxget` value and
confirming that the buffer that is about to be replaced is still at the end of
the R_alloc stack.

We tested that the new mechanism works in two ways:

1. By confirming that a buffer is released that should be released by keeping a
   reference to it and trying to write to it after it is scheduled to be
   released.
2. By monitoring peak memory to confirm that peak memory usage is lower with the
   change.

## Test 1

In `FANSI_size_buff_prot_test`, we interleave growing allocations between
buffers.  We temporarily modified the code to track the very first allocation.

    save_ptr = buff1.buff;

Then, after the first growth that would make `save_ptr` eligible for garbage
collection, we wrote to the string:

    strcpy(save_ptr, "hello world!");

It's possible this alone would cause a segfault if by bad luck we were right at
the gc trigger point in the prior allocation.  But, if that didn't happen, we
can let all the rest of the C code run, and add:

    // For valgrind testing, we run a full gc at this point.  This should gc the
    // allocation that `save_ptr` is pointing at.
    SEXP s, t;
    s = t = PROTECT(allocList(3));
    SET_TYPEOF(t, LANGSXP);
    SETCAR(t, install("gc"));
    SETCADR(t, ScalarLogical(1));
    SETCADDR(t, ScalarLogical(1));
    t = CDR(CDR(t));
    SET_TAG(t, install("full"));
    PrintValue(s);  // did we generate the call correctly?  yes.
    // No problem we haven not gc'ed yet (assuming no other gc happened in
    // interim, should chec if it does blow up by turning on gcinfo)
    Rprintf("okay '%s'\n", save_ptr);
    SEXP res = PROTECT(eval(s, R_GlobalEnv));
    PrintValue(res);
    UNPROTECT(3)
    // Confirm next error is not from the eval proper
    Rprintf("done gc\n");

    // And now confirm that save_ptr has indeed been freed
    Rprintf("boom '%s'\n", save_ptr);
    // With valgrind, this produces error messages (excerpting key piece):
    //
    // ==174== Invalid read of size 1
    // ==174==    at 0x483EF46: strlen
    // ... SNIP ...
    // ==174==    by 0x4A31F0D: Rprintf (printutils.c:905)
    // ==174==    by 0xB9B20CF: FANSI_size_buff_prot_test (write.c:207)
    // ==174==  Address 0xb94d950 is 48 bytes inside a block of size 4,152 free'd
    // ==174==    at 0x483CA3F: free
    // ==174==    by 0x49E3925: ReleaseLargeFreeVectors (memory.c:1114)
    // ==174==    by 0x49EEB15: RunGenCollect (memory.c:1896)
    // ==174==    by 0x49F3910: R_gc_internal (memory.c:3129)
    // ... SNIP ...
    // ==174==  Block was alloc'd at
    // ==174==    at 0x483B7F3: malloc
    // ==174==    by 0x49F24BE: Rf_allocVector3 (memory.c:2806)
    // ==174==    by 0x49D5EF8: Rf_allocVector (Rinlinedfuns.h:595)
    // ==174==    by 0x49F06AD: R_alloc (memory.c:2257)
    // ==174==    by 0xB9B1D7C: FANSI_size_buff (write.c:142)
    // ==174==    by 0xB9B1E31: FANSI_size_buff_prot_test (write.c:158)
    // ... SNIP ...

This is what we expect to see.  Had to run under `valgrind` to actually see it.

## Test 2

Use a vector that is designed to require a growth each time the next element is
used.  We pre-generate test data and store them in RDSes so that the test data
generation does not mess up the `max used` `gc` reported values.

    library(fansi)
    # start.len <- 2^17
    # extra to account for addition of <span ...></span>, erring conservatice
    # extra <- 60
    # steps <- 8
    # reps <- numeric(steps)
    # reps[1] <- start.len + extra
    # for(i in seq_len(steps - 1) + 1) reps[i] <- reps[i - 1] * 2 + extra
    # all <- vapply(reps, \(x) paste0(rep("a", x), collapse=""), "")
    # fin <- paste0("\033[31m", all)
    # saveRDS(fin, 'fin.rds')

    # On fansi 0.5.0

    > gctorture(TRUE)
    > fin <- readRDS('fin.rds')
    > gc(full=TRUE)
              used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    Ncells  448746 24.0     941227 50.3         NA   681934 36.5
    Vcells 4993177 38.1   10146329 77.5      16384  4993312 38.1
    > system.time(res <- sgr_to_html(fin))
       user  system elapsed
      0.772   0.015   0.791
    > gc(full=TRUE)
              used (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    Ncells  449436 24.1     941227  50.3         NA   681934  36.5
    Vcells 9176280 70.1   15603506 119.1      16384 13357604 102.0

    # On fansi 0.5.0.9000

    > fin <- readRDS('fin.rds')
    > gc(full=TRUE)
              used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    Ncells  448960 24.0     941838 50.3         NA   681934 36.5
    Vcells 4993437 38.1   10146329 77.5      16384  4993572 38.1
    > system.time(res <- sgr_to_html(fin))
       user  system elapsed
      0.862   0.017   0.881
    > gc(full=TRUE)
              used (Mb) gc trigger  (Mb) limit (Mb) max used (Mb)
    Ncells  449650 24.1     941838  50.3         NA   681934 36.5
    Vcells 9176540 70.1   15623511 119.2      16384 11275221 86.1
    > gctorture(FALSE)
    > packageVersion('fansi')
    [1] ‘0.5.0.9000’

    > sizes <- unname(sapply(fin, object.size))
    > sizes
    [1]   131248   262440   524824  1049592  2099128  4198200  8396344 16792632
    > sum(sizes[-length(sizes)]/2^20)
    [1] 15.88991

As expected, the difference between `fansi` 0.5.0 and `fansi` 1.0 (0.5.0.9000
dev) is `102.0 - 86.1 == 15.9` which is pretty much exactly the size of all the
elements prior to the largest one, which means the prior `fansi` did not release
them, but this one does.

