#!/usr/bin/env python

from subprocess import call
import sys
import json
import argparse
import os.path as path

from simpletable import SimpleTable, HTMLPage
from jinja2 import Template

css = """
table.mytable {
    font-family: monospace;
    font-size:12px;
    color:#000000;
    border-width: 1px;
    border-color: #eeeeee;
    border-collapse: collapse;
    background-color: #ffffff;
    width=100%;
    max-width:550px;
    table-layout:fixed;
}
table.mytable th {
    border-width: 1px;
    padding: 8px;
    border-style: solid;
    border-color: #eeeeee;
    background-color: #e6eed6;
    color:#000000;
}
table.mytable td {
    border-width: 1px;
    padding: 8px;
    border-style: solid;
    border-color: #eeeeee;
}
#code {
    display:inline;
    font-family: courier;
    color: #3d9400;
}
#string {
    display:inline;
    font-weight: bold;
}
"""

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
def aggregateAlgoStats(algoStats):
    aggStats = []
    for algo, stats in algoStats.items():
        correct = sum(map(lambda l: l[1], stats))
        falsePos = sum(map(lambda l: l[2], stats))
        falseNeg = sum(map(lambda l: l[3], stats))
        fileFalsePos = len(filter(lambda l: l[2] > 0, stats))
        fileFalseNeg = len(filter(lambda l: l[3] > 0, stats))
        time = sum(map(lambda l: l[4], stats)) / len(stats)
        aggStats.append([algo, len(stats), fileFalsePos, fileFalseNeg,
            correct, falsePos, falseNeg, time])

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
            # false pos
            if row[2] > 1:
                val = val + "+"
            # false neg
            if row [3] > 1:
                val = val + "-"
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
    algoStats = {"vanilla":[]}

    with open(path.expanduser("~") + tmpCompareFile) as f:
        tmpCompare = f.read()
        
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
        correct, falsePos, falseNeg = getAlgoStats(locs, erroutLocs)
        algoStats["vanilla"].append([f["file"], correct, falsePos, falseNeg, errout["time"],errout["info"]])

        # process fault local algo output
        with open(floutFilename) as floutFile:
            flout = json.loads(floutFile.read())

        for i, algo in enumerate(flout):
            fllocs = map(loadJSONSrcSpan, algo["locs"])
            correct, falsePos, falseNeg = getAlgoStats(locs, fllocs)

            if algo["name"] not in algoStats:
                algoStats[algo["name"]] = []

            algoStats[algo["name"]].append([f["file"], correct, falsePos, falseNeg, algo["time"], algo["info"]])

        # generate flcompare file
        tmp = Template(tmpCompare)
        tmpAlgos = [{"info":errout["info"]}] + flout
        tmpOut = tmp.render(filename=f["file"], algos=tmpAlgos)
        with open(flcompareFilename,"w") as tmpOutFile:
            tmpOutFile.write(tmpOut.encode())


    # print algo stats to html file
    page = HTMLPage()
    for algo, stats in algoStats.items():
        newStats = []
        for stat in stats:
            newStat = stat
            fileLink = "<a href='" + flDir + stat[0] + flcompareExt + "'>" + stat[0] + "</a>"
            newStat[0] = fileLink
            newStats.append(newStat)

        table = SimpleTable(newStats,
                header_row=["File", "Correct", "False Pos", "False Neg", "Time", "Info"], 
                css_class="mytable")
        page.add_table(table,algo)

    aggStats = aggregateAlgoStats(algoStats)
    aggTable = SimpleTable(aggStats,
            header_row=["Algorithm", "Files", "Files with False Pos", "Files with False Neg", "Total Correct", "Total False Pos", "Total False Neg", "Avg Time"],
            css_class="mytable")
    page.add_table(aggTable,"Aggregate")

    fileStats = getFileStats(algoStats)
    afcStats = algoFileHit(fileStats, algoStats.keys())
    afcTable = SimpleTable(afcStats,
        header_row = ["Files"] + algoStats.keys(),
        css_class="mytable")
    page.add_table(afcTable,"Hit Table")

    for fname, stats in fileStats.items():
        table = SimpleTable(stats,
                header_row=["Algorithm", "Correct", "False Pos", "False Neg", "Time", "Info"],
                css_class="mytable")
        page.add_table(table,fname)

    page.css = css
    page.save(sys.argv[2])
    

if __name__ == "__main__":
    main()
