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

The parameter \*binary-dictionary-keys\* can be used to avoid decoding
byte string values of dictionaries with a given key.  The default
value will leave the "pieces" entry of a torrent "info" dictionary
intact when decoding the meta-info dictionary.

Example
-------

    BENCODE> (with-open-file (stream "/tmp/torrent"
                                     :element-type '(unsigned-byte 8))
               (decode stream))
    #<HASH-TABLE :TEST EQUAL :COUNT 4 {CB06069}>
    BENCODE> (gethash "announce-list" *)
    (("http://tracker.openbittorrent.com/announce")
     ("udp://tracker.openbittorrent.com:80/announce"))
    T
    BENCODE> (type-of (gethash "pieces" (gethash "info" **)))
    (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (420660))
    BENCODE> (equalp (ironclad:digest-sequence :sha1 (encode *** nil))
                     (ironclad:digest-file :sha1 "/tmp/torrent"))
    T
    BENCODE> 

Todo
----

* Tests
* Documentation
* Error handling
