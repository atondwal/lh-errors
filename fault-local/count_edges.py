#!/usr/bin/env python

import argparse
import json
import os.path as path

def getConsLoc(conslist, id):
    for con in conslist:
        if con["id"] == id:
            return con["loc"]
    
    return None

def getConsAtLoc(conslist, loc):
    consloc = []
    for con in conslist:
        if con["loc"] == loc:
            consloc.append(con)

    return consloc

def main():
    parser = argparse.ArgumentParser(description="count in/out edges for implicated constraints")
    parser.add_argument("flout", help="JSON file containing FL info for a Liquid Haskell file")
    args = parser.parse_args()

    if not path.isfile(args.flout):
        print "flout file {0} doesn't exist".format(args.flout)
        return None

    with open(args.flout) as jsonfile:
        flout = json.loads(jsonfile.read())

    graph = flout["graph"]
    flcons = map(lambda c: int(c), flout["results"][0]["info"].split())
    flcons.sort()

    for flcon in flcons:
        inEdges = filter(lambda e: e["dest"] == flcon, graph)
        inFail = filter(lambda e: e["source"] in flcons, inEdges)

        outEdges = filter(lambda e: e["source"] == flcon, graph)
        outFail = filter(lambda e: e["dest"] in flcons, outEdges)

        print "---------------------"
        print "Constraint #", flcon
        print "in fail: ", len(inFail)
        print "in not fail: ", (len(inEdges) - len(inFail))
        print "out fail: ", len(outFail)
        print "out not fail: ", (len(outEdges) - len(outFail))

        loc = getConsLoc(flout["cons"], flcon)
        if loc is not None:
            consloc = map(lambda c: c["id"], getConsAtLoc(flout["cons"], loc))
            conslocFail = filter(lambda c: c in flcons, consloc)
            print "locs fail: ", len(conslocFail)
            print "locs no fail: ", (len(consloc) - len(conslocFail))

if __name__ == "__main__":
    main()
