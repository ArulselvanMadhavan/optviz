def any(lname, xs):
    for x in xs:
        if x in lname:
            return True
    return False

def all(lname, xs):
    for x in xs:
        if x in lname:
            return False
    return True
