import cgi

class server:
    data = cgi.FieldStorage(keep_blank_values=True)

    #Returns a string of the GET data. O_O'
    def get_get_data(self, var):
        #self.data = cgi.FieldStorage(keep_blank_values=True)
        if var in self.data.keys():
            return self.data[var].value
        return ""
