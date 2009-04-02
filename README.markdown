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

    CL-USER> (with-open-file (stream "/tmp/t.torrent" :element-type '(unsigned-byte 8))
               (bencode:decode (flex:make-flexi-stream stream)))
    #<COM.SCHOBADOO.BENCODE:BDICTIONARY {C06E8D9}>
    CL-USER> (gethash "announce-list" *)
    (("http://tracker.thepiratebay.org/announce")
     ("udp://tracker.thepiratebay.org:80/announce"))
    T
    CL-USER> (gethash "info" **)
    #<COM.SCHOBADOO.BENCODE:BDICTIONARY {C4D3F81}>
    T
    CL-USER> (ironclad:digest-sequence :sha1 (bencode:encode *** nil))
    #(255 76 172 253 93 185 89 178 3 115 13 223 110 96 77 6 47 10 18 98)
    CL-USER> (ironclad:digest-file :sha1 "/tmp/t.torrent")
    #(255 76 172 253 93 185 89 178 3 115 13 223 110 96 77 6 47 10 18 98)
    CL-USER> 

Todo
----

* Tests
* Documentation
* Error handling
