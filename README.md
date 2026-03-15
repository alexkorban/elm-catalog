This project is a catalog of categorised Elm packages and tools. The list of tagged packages is in `public/tagged-packages.js`. The list of tagged tools is in `public/tools.js`.

# Packages

The tags are a two level categorisation scheme with a category and subcategory. The mapping between tags and the names of categories and subcategories can be found in `Main.elm`, in `humanisePkgCat` and `humanisePkgSubcat`. 

A package can have multiple tags (preferably no more than three).

The update command tags new packages with `uncat/new` so that they can appear in the "Uncategorised" section of the catalog after the daily update runs on Netlify. 

# Tools

Tools have a single level categorisation scheme. The mapping between tags and categories name can be found in `Main.elm` in `humaniseToolCat`. Tools are added to the list manually. 

# How this runs on korban.net

The catalog is deployed into a separate app on Netlify. Its JS is then included in the korban.net website so that it displays at korban.net/elm/catalog. This allows the catalog to be maintained separately from the main website and to be updated on a daily schedule via GitHub Actions. 

# Useful commands

Updating the list with new/updated packages: 

```
npm run update
```


Getting a list of unique tags used in `tagged-packages.js`: 

```
tail -n +2 public/tagged-packages.js | jq  '[.[] | .["tags"]] | flatten | unique | sort'
```

# Prompts

Tag new packages: 

"Check the README.md to understand the project. Tag new packages (those tagged `uncat/new`) in `tagged-packages.js` using existing tags. Remove `uncat/new` tag when tagging. If no existing tags fit, leave the entry unchanged."