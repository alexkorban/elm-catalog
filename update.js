#!/usr/bin/env node
const Fs = require("fs-extra")
const { Octokit } = require("@octokit/core")
const Promise = require("bluebird")
const R = require("ramda")
const { spawn } = require("cross-spawn")
const stringify = require("json-stable-stringify")

console.log("Getting search.json...")

const res = spawn.sync("curl", ["--compressed", "https://package.elm-lang.org/search.json"], { stdio: "pipe" })

if (res.status != 0)
    throw new Error("Couldn't get search.json")

const newPackages = JSON.parse(res.stdout.toString())

console.log("New packages: " + R.length(newPackages))

const taggedPackagesStr = Fs.readFileSync("public/tagged-packages.js").toString()
const taggedPackages = JSON.parse(taggedPackagesStr.slice(taggedPackagesStr.indexOf("[")))

console.log("Tagged packages: " + R.length(taggedPackages))

const merge = (package) => {
    taggedPkg = R.find(R.propEq("name", package.name), taggedPackages)
    if (!R.isNil(taggedPkg)) {
        let newPkg = R.mergeDeepRight(taggedPkg, package)
        if (taggedPkg.tags[0] == "exclude" && package.version != taggedPkg.version) {
            console.log("Re-evaluate: " + newPkg.name)
            // Updated excluded packages should come up as new – for re-evaluation
            return R.mergeLeft({ tags: ["uncat/new"] }, newPkg)
        }
        else
            return newPkg
    }
    else
        return R.mergeRight({ tags: ["uncat/new"] }, package)
}

const octokit = new Octokit({
    auth: process.env.GITHUB_TOKEN,
})

const pkgParentPromise = (pkg) => {
    if (!R.isNil(process.env.GITHUB_TOKEN) && !R.includes("uncat/deleted", pkg.tags)) {
        console.log("Getting repo info for " + pkg.name)
        return octokit.request("GET /repos/" + pkg.name).then((res) => {
            return R.mergeLeft(pkg, { forkOf: R.defaultTo(null, R.path(["data", "parent", "full_name"], res)) })
        }).catch((error) => {
            console.error(`Error while getting repo info for ${pkg.name}: `, error.message)
            if (error.message == "Not Found")
                return R.evolve({ tags: R.append("uncat/deleted") }, pkg)
            else
                return pkg  // the package object shouldn't have a forkOf property 
            // if we failed to retrieve repo info
        })
    }
    else
        return Promise.resolve(pkg)
}

Promise.map(R.map(merge, newPackages), (pkg) => {
    return R.has("forkOf", pkg) ? Promise.resolve(pkg) : pkgParentPromise(pkg)
}, { concurrency: 3 })
    .then((updatedPackages) => {
        // Overwrites the existing file!
        Fs.writeFileSync("public/tagged-packages.js", "window.packages = \n" + stringify(updatedPackages, { space: 4 }))

        console.log("Done!")
    })
    .catch((error) => {
        console.error("Error while getting repos: ", error.message)
        console.log(error.stack)
    })