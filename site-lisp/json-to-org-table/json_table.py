def cs(string):
    """Replaces any string org mode wouldn't like"""
    # string = string.strip("\'")
    string = string.replace("\r","")
    string = string.replace("\n","")
    return string
    
def keys_to_headers(keys):
    pass

def lf(key, ref=None, i=None):
    """Link format"""
    if i:
        return "[[{}_{}{}]]".format(key, ref, str(i))
    elif ref:
        return "[[{}_{}]]".format(key, ref)
    else:
        return "[[{}]]".format(key)

def dp(key, value):
    """Inputs a dict and outputs a table"""
    return "|{}|{}|\n".format(cs(key),cs(value))

def sp(key):
    return "|{}|\n".format(cs(key))

def tablify(elt, ref=None):
    # TODO Short List of Dicts
    # TODO List of Lists
    # All named go in drawer
    # Clean up spaces
    cur = ""
    nex = []
        
    if isinstance(elt, dict):
        # Add the headers
        # If the reference is none this top level
        if ref == "":
            cur += ""
        elif ref:
            #cur += "* {}\n".format(ref)
            cur += "#+name: {}\n".format(ref)

        cur += "|key|value|\n|-\n"
        
        # Iterate over the keys
        for key in elt.keys():
            value = elt[key]
            # If the value is None add empty string
            if not value:
                cur += dp(key, "")
            # If the value is a list create a new table
            elif isinstance(value, list):
                cur += dp(key, lf(key, ref))
                nex.append(key)
            # If the value is a large dict- create a new table
            elif isinstance(value, dict):
                cur += dp(key, lf(key, ref))
                nex.append(key)
            else:
                cur += dp(key, str(value))

        if not ref:
            pass
            # cur += ":EXTRA:\n"
        for key in nex:
            if ref:
                cur += tablify(elt[key], key + "_" + ref)
            else:
                cur += tablify(elt[key], key)

    elif isinstance(elt, list) and isinstance(elt[0], dict):
        # Add the headers
        # If the reference is none this top level
        if ref == "":
            cur += ""
        elif ref:
            #cur += "* {}\n".format(ref)
            cur += "#+name: {}\n".format(ref)
        else:
            ref = ""
        keys = elt[0].keys()
        cur += "|" + "|".join(keys) + "|\n"
        cur += "|-\n"
        for i, list_item in enumerate(elt, start=1):
            cur += "|"
            for key in keys:
                value = list_item[key]
                if isinstance(value, dict):
                    cur += lf(key, ref, i)
                    cur += "|"
                    nex.append((key, value, i,))
                else:
                    cur += cs(str(list_item[key])) + "|"
            cur += "\n"
        for key, value, i in nex:
            cur += tablify(value, "{}_{}{}".format(key, ref, str(i)) )
        
    elif isinstance(elt, list):
        # Add the headers
        # If the reference is none this top level
        if ref == "":
            cur += ""
        elif ref:
            #cur += "* {}\n".format(ref)
            cur += "#+name: {}\n".format(ref)
        else:
            ref = ""

        for i, list_item in enumerate(elt, start=1):
            # If it's a dict with a single item just make a normal table
            #elif isinstance(list_item, dict) and len(elt) == 1:
            #    cur += tablify(elt[0], ref="")
    
            # If it's a list of lists make a table with no headers
            if isinstance(list_item, list):
                cur += "|"
                for entry in list_item:
                    cur += cs(str(list_item[key])) + "|"
                cur += "\n"
    
            # If it's a list of strings create a 1 column table with the key as the header        
            else:
                if i == 1:
                    cur += "|{}|\n|-\n".format(ref)
                cur += "|{}|".format(cs(str(list_item)))
                cur += "\n"
    if not ref:
        pass
        #cur += ":END:"
    else:
        cur += "\n"
    return cur
