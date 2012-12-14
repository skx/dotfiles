#!/bin/sh
#
#  Detect whether a named file has had its contents changed since the last
# test.  We detect this via both SHA1 and MD5 hashes of the file contents.
#
#  The file checksums are stored beneath ~/.haschanged.  So that lookups
# consistently return the same thing we must call "haschanged" with exactly
# the same path each time.
#
#
# Return codes
# ------------
#
#   Error    : -1
#   Changed  :  0
#   No change:  1
#
#
# Sample usage
# ------------
#
#    #!/bin/sh
#    if ( haschanged /etc/passwd >/dev/null ) ; then
#      echo "Changed"
#    else
#      echo "Not changed"
#    fi
#
# Notes
# -----
#
#  I don't think it makes sense to allow the script to be called with multiple
# files as arguments.  In the event of no files having changed it would be OK
# but when one has changed the return code can't easily tell you *which* one
# it was.
#
#  That might be OK for some things.  eg.  /etc/passwd + /etc/shadow, but
# not for others.  Sure the output message could tell you interactively,
# but that isn't really how I'd expect it to be used.
#
#
# Steve
# --
#


#
# Make sure we got at least one argument.
#
if [ -z "$1" ]; then
    echo "Usage: haschanged file1"
    exit -1
fi


#
#  Make sure the file exists.
#
file=$1
if [ ! -e "${file}" ]; then
    echo "File not found: ${file}"
    exit -1
fi


#
#  The file must be readable - or we can't compute the hash.
#
if [ ! -r "${file}" ]; then
    echo "File not readable: ${file}"
    exit -1
fi


#
#  Make sure we have the expected hashing tools
#
if [ ! -x /usr/bin/md5sum ]; then
    echo "/usr/bin/md5sum missing or not executable."
    exit -1
fi

if [ ! -x /usr/bin/sha1sum ]; then
    echo "/usr/bin/sha1sum missing or not executable."
    exit -1
fi


#
#  Make sure we have a cache directory
#
if [ ! -d ~/.haschanged ]; then
    mkdir ~/.haschanged
    chmod 700 ~/.haschanged
fi


#
#  Find the current SHA1 and MD5 hashes of the file contents.
#  
md5=$(/usr/bin/md5sum   "${file}" 2>/dev/null | awk '{print $1}')
sha1=$(/usr/bin/sha1sum "${file}" 2>/dev/null | awk '{print $1}')

#
#  Find a predictable filename to use in our cache directory.
#
#  File format:
#
#    "MD5-hash [space] SHA1-hash"
#
cache=`echo "${file}" | tr \/ -`


#
#  Get the stored values, if present.
#
if [ -e ~/.haschanged/${cache} ]; then
    oldmd5=$(awk '{print $1}'  < ~/.haschanged/${cache})
    oldsha1=$(awk '{print $2}' < ~/.haschanged/${cache})
fi


#
#  Now replace the values in the cache with the current ones.
#  This handles the cases where the hashes don't currently exist, or
# have changed.
#
echo "${md5} ${sha1}" > ~/.haschanged/${cache}


#
#  Do the old and new MD5 checksums differ?
#
if [ "${md5}" != "${oldmd5}" ]; then
    echo "MD5 checksum changed: ${file}"
    exit 0
fi

#
#  Do the old and new SHA1 checksums differ?
#
if [ "${sha1}" != "${sha1}" ]; then
    echo "SHA1 checksum changed: ${file}"
    exit 0
fi

#
#  The hashes of the file contents are identical - no change.
#
exit 1



