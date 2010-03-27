Description
-----------

cl-bencode is a Common Lisp bencode library. Bencode is the encoding
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

Example
-------

    CL-USER> (with-open-file (stream "/tmp/torrent" :element-type '(unsigned-byte 8))
               (bencode:decode (flex:make-flexi-stream stream)))
    #<HASH-TABLE :TEST EQUAL :COUNT 4 {BBA1EA9}>
    CL-USER> (gethash "announce-list" *)
    (("udp://tracker.openbittorrent.com:80/announce")
     ("http://tracker.openbittorrent.com/announce"))
    T
    CL-USER> (ironclad:digest-sequence :sha1 (bencode:encode ** nil))
    #(182 59 189 138 153 64 38 223 79 48 24 51 171 34 93 41 82 178 25 152)
    CL-USER> (ironclad:digest-file :sha1 "/tmp/torrent")
    #(182 59 189 138 153 64 38 223 79 48 24 51 171 34 93 41 82 178 25 152)
    CL-USER> 

Todo
----

* Tests
* Documentation
* Error handling
