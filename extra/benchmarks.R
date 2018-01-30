ulysses <- readLines("http://www.gutenberg.org/files/4300/4300-0.txt")

system.time(csi <- fansi::strwrap_csi(ulysses, 30))
system.time(normal <- strwrap(ulysses, 30))

all.equal(normal, csi)
