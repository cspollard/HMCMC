from sys import argv, stdout
myargv = argv[:]

import ROOT
import json
print "HELLO"; stdout.flush()

def main(fname):
    fin = ROOT.TFile.Open(fname)

    d = {}
    for k in fin.GetListOfKeys():
        h = fin.Get(k.GetName())
        print h; stdout.flush()
        d[h.GetName()] = toList(h)
        continue

    print json.dumps(d)

    return


def toList(h):
    return (h.GetTitle(),
            [h.GetBinContent(iBin) + 1e-10
                for iBin in range(1, h.GetNbinsX()+2)])

if __name__ == "__main__":
    main(myargv[1])
