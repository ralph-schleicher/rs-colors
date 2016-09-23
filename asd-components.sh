#! /bin/sh

# The empty string.
COMPONENTS=

for arg
do
    case $arg in
      *.lisp)
	file=`basename "$arg" .lisp`
	if test "x$COMPONENTS" = x
	then
	    :
	else
	    # The backslash escapes the newline for sed(1).
	    COMPONENTS="$COMPONENTS\\
	       "
	fi
	COMPONENTS="$COMPONENTS(:file \"$file\")"
	;;
    esac
done

sed -e "s/@COMPONENTS@/$COMPONENTS/g"

## asd-components.sh ends here
