Description
-----------

cl-bencode is a Common Lisp bencode library.  Bencode is the encoding
used by BitTorrent.

Features
--------

cl-bencode supports all four different types of bencode values.

* byte strings as strings or byte vectors
* integers as integers
* lists as lists
* and dictionaries as hashtables

It's proven capable of roundtripping several torrents found in the
wild.

String encoding
---------------

The bencode specification does not deal with character
encodings. cl-bencode uses flexi-streams' external-formats for
character encoding.  The default is UTF-8.  See
http://weitz.de/flexi-streams/#external-formats

Binary strings
--------------

The parameter \*binary-key-p\* may hold a function. When decoding
dictionary values, this function is passed a list, where the first
element is the key of the value. If the dictionary was in turn a
dictionary value, that key is the second element of the list, and so
on. Should a dictionary be a value in a bencoded list, the
corresponding element in the list will be the symbol :list.  When the
function return a true value, the dictionary value will be
binary. Otherwise it will be decoded as a string.

The default function in \*binary-key-p\* returns true for the "pieces"
value in the "info" dictionary. All other values are decoded as
strings.

Example
-------

    CL-USER> (ql:quickload "bencode")
    ; ...
    ("bencode")
    CL-USER> (with-open-file (stream "/tmp/torrent" 
                                     :element-type '(unsigned-byte 8))
               (bencode:decode stream))
    #<HASH-TABLE :TEST EQUAL :COUNT 4 {1005E0E3D3}>
    CL-USER> (gethash "announce" *)
    "http://bttracker.debian.org:6969/announce"
    T
    CL-USER> (type-of (gethash "pieces" (gethash "info" **)))
    (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (88860))
    CL-USER> (equalp (ironclad:digest-sequence :sha1 (bencode:encode *** nil))
                     (ironclad:digest-file :sha1 "/tmp/torrent"))
    T
    CL-USER> (asdf:test-system "bencode")
    ......................................
    T
    CL-USER> 

Todo
----

* Documentation
