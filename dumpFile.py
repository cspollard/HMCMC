from sys import argv
myargv = argv[:]

import ROOT
import json

regions = [
        "lvJ_0tag_0additionaltag_unblind_SR",
        "lvJ_1tag_0additionaltag_unblind_SR",
        "lvJ_2tag_0additionaltag_unblind_SR",
        "lvJ_0tag_1additionaltag_unblind_SR",
        "lvJ_1tag_1additionaltag_unblind_SR",
        "lvJ_2tag_1additionaltag_unblind_SR"
        ]

def main(fname, norm):
    fin = ROOT.TFile.Open(fname)

    dtmp = {}
    for r in regions:
        sbkg = fin.Get(r + "/Nominal/bkg_vh_m")
        hs = sbkg.GetHists()

        ssig = fin.Get(r + "/Nominal/sig_vh_m")
        for h in ssig.GetHists():
            if "2000" in h.GetName():
                hs.append(h)
                break
            else:
                pass

            continue

        map(lambda h: h.Scale(norm), hs)

        dtmp[r] = map(toList, hs)

        continue

    ddtmp = {}
    for reg, procHists in dtmp.iteritems():
        for (proc, h) in procHists:
            if proc not in ddtmp:
                ddtmp[proc] = {reg : h}
            else:
                ddtmp[proc][reg] = h

            continue
        continue


    print json.dumps(ddtmp)

    return


def toList(h):
    return (h.GetTitle(),
            [h.GetBinContent(iBin) for iBin in range(1,
                h.GetNbinsX()+2)])

if __name__ == "__main__":
    main(myargv[1], float(myargv[2]))
