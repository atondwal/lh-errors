#!/usr/bin/env python

from subprocess import call
import sys
import json
import argparse
import os.path as path

from simpletable import SimpleTable, HTMLPage
from jinja2 import Template

# kvgraph visualizer
from graphviz import Digraph
def loadJSONSrcSpan(j):
    return (j["sline"],j["scol"],j["eline"],j["ecol"])

sline   = 0
scol    = 1
eline   = 2
ecol    = 3

def spanSubset(s1,s2):
    start = s2[sline] < s1[sline] or (s2[sline] == s1[sline] and s2[scol] <= s1[scol])
    end = s2[eline] > s1[eline] or (s2[eline] == s1[eline] and s2[ecol] >= s1[ecol])
    return start and end

# post-process fl locs by removing subset locs
def processLocs(locs):
    if len(locs) >= 2:
        newlocs = []
        for i, l in enumerate(locs):
            subset = any(map(lambda l2: spanSubset(l,l2),locs[:i]+locs[i+1:]))
            if not subset:
                newlocs.append(l)
        
        return newlocs

    else:
        return locs

# calculate correct / false pos / false neg src spans for algos
def getAlgoStats(locs,fllocs):
    correct = 0
    falsePos = 0
    falseNeg = 0
    for loc in locs:
        subset = any(map(lambda flloc: spanSubset(loc,flloc),fllocs))
        superset  = any(map(lambda flloc: spanSubset(flloc,loc),fllocs))
        if subset or superset:
            correct += 1
        else:
            falseNeg += 1

    falsePos = len(fllocs) - correct
    return correct, falsePos, falseNeg

# calculate aggregate stats for algos
def aggregateAlgoStats(algoStats, tolerance=0):
    aggStats = []
    for algo, stats in algoStats.items():
        correct = sum(map(lambda l: l[1], stats))
        falsePos = sum(map(lambda l: l[2], stats))
        falsePosTolerance = sum(map(lambda l: max(l[2]-tolerance, 0), stats))
        falseNeg = sum(map(lambda l: l[3], stats))
        fileFalsePos = len(filter(lambda l: l[2] > tolerance, stats))
        fileFalseNeg = len(filter(lambda l: l[3] > 0, stats))
        time = sum(map(lambda l: l[4], stats)) / len(stats)
        aggStats.append([algo, len(stats), fileFalsePos, fileFalseNeg,
            correct, falsePosTolerance, falsePos, falseNeg, time])

    return aggStats

# calculate a hit table between algos and files
def algoFileHit(fileStats, algos):
    hitTable = []
    for fname, stats in fileStats.items():
        hits = {}
        for row in stats:
            val = ""
            # correct
            if row[1] > 0:
                val = val + "C"
            # false neg
            if row [3] > 0:
                val = val + "-"
            # false pos
            if row[2] > 0:
                val = val + "+"
            hits[row[0]] = val

        algoVals = []
        for algo in algos:
            if algo in hits.keys():
                algoVals.append(hits[algo])
            else:
                algoVals.append("none")

        hitTable.append([fname]+algoVals)

    return hitTable

# calculate algo stats for each file
def getFileStats(algoStats):
    fileStats = {}
    for algo, stats in algoStats.items():
        for row in stats:
            if row[0] not in fileStats:
                fileStats[row[0]] = []

            fileStats[row[0]].append([algo,row[1],row[2],row[3],row[4],row[5]])

    return fileStats

def main():
    parser = argparse.ArgumentParser(description="generate summary of LH fault local algo results")
    parser.add_argument("filelist", help="JSON file of list of Liquid Haskell files to process")
    parser.add_argument("outfile", help="name of HTML file that summary of algos will be dumped")
    parser.add_argument("-c","--clobber",action="store_true", help="overwrite existing FL info even if they exist")
    parser.add_argument("--nograph",action="store_true", help="don't create kvgraph diagram")
    args = parser.parse_args()

    if not path.isfile(args.filelist):
        print "File list {0} doesn't exist".format(args.filelist)
        return None

    with open(args.filelist) as jsonfile:
        files = json.loads(jsonfile.read())

    flDir = ".liquidfl/"
    erroutExt = ".errout"
    floutExt = ".flout"
    flcompareExt = ".flcompare.html"
    tmpCompareFile = "/.cabal/template.flcompare.html"
    tmpCompare = ""
    graphExt = ".kvgraph"
    graphViewExt = ".kvgraph.html"
    tmpGraphViewFile = "/.cabal/template.kvgraph.html"
    tmpSummaryFile = "/.cabal/template.flsummary.html"
    algoStats = {"vanilla":[]}
    algos = None

    with open(path.expanduser("~") + tmpCompareFile) as f:
        tmpCompareStr = f.read()

    with open(path.expanduser("~") + tmpGraphViewFile) as f:
        tmpGraphViewStr = f.read()
        
    # run liquid haskell on files
    for f in files:
        # only process LH file if FL data doesn't exist, or clobber flag is set
        erroutFilename = flDir + f["file"] + erroutExt
        floutFilename = flDir + f["file"] + floutExt
        process = not (path.isfile(erroutFilename) and path.isfile(floutFilename))
        process = process or args.clobber

        if process:
            cmdstr = "liquid --faultlocal " + f["file"]
            print cmdstr
            call(cmdstr.split())

    # process FL output
    for f in files:
        erroutFilename = flDir + f["file"] + erroutExt
        floutFilename = flDir + f["file"] + floutExt
        flcompareFilename = flDir + f["file"] + flcompareExt

        # check if FL data exists before attempting to process it
        if not (path.isfile(erroutFilename) and path.isfile(floutFilename)):
            break

        locs = map(loadJSONSrcSpan, f["locs"])

        # process vanilla liquid haskell error locations
        with open(erroutFilename) as erroutFile:
            errout = json.loads(erroutFile.read())

        erroutLocs = map(loadJSONSrcSpan, errout["locs"])
        correct, falsePos, falseNeg = getAlgoStats(locs, processLocs(erroutLocs))
        algoStats["vanilla"].append([f["file"], correct, falsePos, falseNeg, errout["time"],errout["info"]])

        # process fault local algo output
        print f["file"], ": aggregating statistics..."
        with open(floutFilename) as floutFile:
            flout = json.loads(floutFile.read())

        for i, algo in enumerate(flout["results"]):
            fllocs = map(loadJSONSrcSpan, algo["locs"])
            correct, falsePos, falseNeg = getAlgoStats(locs, processLocs(fllocs))

            if algo["name"] not in algoStats:
                algoStats[algo["name"]] = []

            algoStats[algo["name"]].append([f["file"], correct, falsePos, falseNeg, algo["time"], algo["info"]])

        # generate KVgraph diagram
        graphViewFile = flDir + f["file"] + graphViewExt
        if ((not path.isfile(graphViewFile)) or args.clobber) and not args.nograph:
            print f["file"], ": generating kvgraph..."

            kvnodes = map(lambda c: str(c["id"]), flout["cons"])
            kvedges = map(lambda e: (str(e["source"]),str(e["dest"])), flout["graph"])
            graph = Digraph(format="svg", engine="fdp", name=f["file"] + " - KVGraph")
            graph.graph_attr["ranksep"] = "20.0"
            graph.graph_attr["nodesep"] = "20.0"
            graph.graph_attr["overlap"] = "false"
            graph.graph_attr["splines"] = "true"
        
            for node in kvnodes:
                graph.node(node, node)

            graph.edges(kvedges)
            graphFile = flDir + f["file"] + graphExt
            graph.render(graphFile, cleanup=False)

            # generate kvgraph viewer
            with open(graphFile + ".svg") as svgDataFile:
                svgData = svgDataFile.read()
                svgData = svgData.replace('\n','')
                svgData = svgData.replace('\r','')

            tmpGraphView = Template(tmpGraphViewStr)
            with open(graphViewFile,"w") as viewOutFile:
                viewOut = tmpGraphView.render(
                            graphfile=f["file"] + graphExt + ".svg",
                            svg=svgData)
                viewOutFile.write(viewOut.encode("utf-8"))

        # generate flcompare file
        print f["file"], ": generating flcompare page..."
        tmp = Template(tmpCompareStr)
        tmpAlgos = [{"name":"vanilla","info":errout["info"]}] + flout["results"]
        tmpCons = sorted(flout["cons"], key=lambda c: int(c["id"]))
        tmpOut = tmp.render(filename=f["file"], algos=tmpAlgos, cons=tmpCons)
        with open(flcompareFilename,"w") as tmpOutFile:
            tmpOutFile.write(tmpOut.encode("utf-8"))

        # get list of algos, if it doesn't already exist
        algos = ["vanilla"] + map(lambda r: r["name"], flout["results"])


    # print algo stats to html file
    print "generating summary page..."

    # load template summary page
    with open(path.expanduser("~") + tmpSummaryFile) as f:
        tmpSummaryStr = f.read()

    tables = []

    # create aggregate tables for tolerances 0 to 4
    for t in range(0,5):
        tables.append({
            "name":"Aggregate (tolerance = {0})".format(t),
            "headers":["Algorithm", "Files", "Files with False Pos", "Files with False Neg", "Total Correct", "False Pos above Tolerance", "Total False Pos", "Total False Neg", "Avg Time"],
            "rows": sorted(aggregateAlgoStats(algoStats,t), key=lambda r: algos.index(r[0]))
        })
        
    fileStats = getFileStats(algoStats)
    afcStats = algoFileHit(fileStats, algos)

    tables.append({
        "name":"Hit Table",
        "headers":["Files"] + sorted(algoStats.keys(), key=lambda a: algos.index(a)),
        "rows": sorted(afcStats, key=lambda r: r[0])
    })

    for algo, stats in algoStats.items():
        newStats = []
        for stat in stats:
            newStat = stat
            fileLink = "<a href='" + flDir + stat[0] + flcompareExt + "'>" + stat[0] + "</a>"
            newStat[0] = fileLink
            newStats.append(newStat)

        tables.append({
            "name":algo,
            "headers":["File", "Correct", "False Pos", "False Neg", "Time", "Info"],
            "rows": sorted(newStats, key=lambda r: r[0])
        })

    for fname, stats in fileStats.items():
        tables.append({
            "name":fname,
            "headers":["Algorithm", "Correct", "False Pos", "False Neg", "Time", "Info"],
            "rows": sorted(stats, key=lambda r: algos.index(r[0]))
        })

    tmp = Template(tmpSummaryStr)
    tmpOut = tmp.render(tables=tables)
    with open(args.outfile,"w") as tmpOutFile:
        tmpOutFile.write(tmpOut.encode("utf-8"))


if __name__ == "__main__":
    main()
