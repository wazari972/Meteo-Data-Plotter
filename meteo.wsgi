import os
import csv
import collections

import cgi

COLUMNS = collections.OrderedDict([("DATE", 9), ("MIN", 3), ("MAX",3), ("RAIN", 3), ("PRESURE", 2), ("HYGRO", 2), ("INT_MIN", 2), ("INT_MAX", 2), ("COMMENT", 20)])
COLUMNS_OPT = ("INT_MIN", "INT_MAX", "COMMENT")

def read_lines(filename):
    reader = csv.DictReader(open(filename, "rb"), 
                            COLUMNS,
                            delimiter=';')
    first = True
    
    for line in reader:
        if first:
            first = False
            continue
        yield line

def get_value_table(filename):
    FORM = "<form method='GET'>\n%s</form>\n"
    TABLE = "<table>\n%s\n</table>\n"
    ROW = "\t<tr>%s</tr>"
    HEAD = "<th>%s</th>"
    CELL = "<td>%s</td>"
    EMPTY = "<input type='text' name='%s' size='%d'/>"
    HIDDEN = "<input type='hidden' name='compute' value='YES'/>"
    GO_BT = "<input type='submit' value='GO'/>"
    
    rows = []
    for line in read_lines(filename):
        rows.append(ROW % "".join([CELL % line[col] for col in COLUMNS.keys()]))
        
    rows.append(ROW % "".join([CELL % EMPTY % (col, length) for col, length in COLUMNS.items()]+[CELL % "%s%s" % (GO_BT, HIDDEN)]))
    rows.append(ROW % "".join([HEAD % col for col in COLUMNS.keys()]))
    
    return FORM % TABLE % "\n".join(rows[::-1])
    
    
def write_line(filename, form):
    if not (form.has_key("compute") and form["compute"].value == "YES"):
        print "don't compute %s" % form["compute"]
        return
    line = []
    for key in COLUMNS.keys():
        try:
            line.append(form[key].value)
        except KeyError as e:
            if key not in COLUMNS_OPT:
                raise e
            line.append("")
    #write line in file
    ofile = open(filename, "a")
    try:
        writer = csv.writer(ofile, delimiter=';')
        writer.writerow(line)
    finally:
        ofile.close()
        
def application(environ, start_response):
    """Simplest possible application object"""
    filename = "/srv/http/meteo/Revel2.csv"
    
    output = "<html><body>%s</body></html>"
    
    form = cgi.FieldStorage(fp=environ['wsgi.input'], environ=environ)
    try:
        write_line(filename, form)
    except KeyError as e:
        output = output % "<b>Missing key in the values provided: %s</b><br/>\n%%s" % e
                
    output = output % get_value_table(filename)
    
    status = '200 OK'
    response_headers = [('Content-type', 'text/html'),
                        ('Content-Length', str(len(output)))]
    start_response(status, response_headers)
    return [output]

#print get_value_table("Revel2.csv")