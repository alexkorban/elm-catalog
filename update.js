#!/usr/bin/env node
const Fs = require("fs-extra")
const R = require("ramda")
const { spawn } = require("cross-spawn")
const stringify = require("json-stable-stringify")

console.log("Getting search.json...")

const res = spawn.sync("curl", ["--compressed", "https://package.elm-lang.org/search.json"], { stdio: "pipe" })

if (res.status != 0)
    throw new Error("Couldn't get search.json")

const newPackages = JSON.parse(res.stdout.toString())

console.log("New packages: " + R.length(newPackages))

const taggedPackagesStr = Fs.readFileSync("tagged-packages.js").toString()
const taggedPackages = JSON.parse(taggedPackagesStr.slice(taggedPackagesStr.indexOf("[")))

console.log("Tagged packages: " + R.length(taggedPackages))

const merge = (package) => {
    taggedPkg = R.find(R.propEq("name", package.name), taggedPackages)
    if (!R.isNil(taggedPkg)) {
        let newPkg = R.mergeDeepRight(taggedPkg, package)
        if (taggedPkg.tags[0] == "exclude" && package.version != taggedPkg.version) {
            console.log("Re-evaluate: " + newPkg.name)
            // Updated excluded packages should come up as new – for re-evaluation
            return R.mergeLeft({tags: ["uncat/new"]}, newPkg) 
        }
        else 
            return newPkg
    }
    else 
        return R.mergeRight({tags: ["uncat/new"]}, package)
}

// Overwrites the existing file!
Fs.writeFileSync("tagged-packages.js", "window.packages = \n" + stringify(R.map(merge, newPackages), {space: 4}))

console.log("Done!")