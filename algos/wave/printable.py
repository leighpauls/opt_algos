
class Printable:
    def __repr__(self):
        return self.__str__()
    def __str__(self):
        res = ["<", str(self.__class__), ':\n']
        for k, v in self.__dict__.items():
            res += ['  ', k, ": ", str(v).replace('\n', '\n  '), ',\n']
        res += [">"]
        return "".join(res)
