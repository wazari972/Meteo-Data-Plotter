import sys
import re

def addQuotes( str ):
    matches = re.match( '^".*"$', str )
    if matches == None:
        return '"' + str + '"'
    return str

# sed s/+ACI-//g -i Revel.csv
# sed 's/+ADs-/;/g' -i Revel.csv
    
    #
    
# Iterate over standard input. NOTE - this isn't line-buffered, don't try using
# this script interactively...
for line in sys.stdin:

    # Remove trailing linefeed.
    line = line.strip()

    # Split the line into parts separated by commas.
    parts = line.split( ',' )

    # Add quotes to each part that doesn't have quotes already.
    newParts = map( addQuotes, parts )

    # Concatenate the parts back to a single line.
    concatParts = ','.join( newParts )

    # And print it.
    print concatParts
