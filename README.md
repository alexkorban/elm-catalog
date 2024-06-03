Getting a list of unique tags used in `tagged-packages.js`: 

```
tail -n +2 public/tagged-packages.js | jq  '[.[] | .["tags"]] | flatten | unique | sort'
```