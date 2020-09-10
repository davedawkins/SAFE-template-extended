# SAFE Template Extended

This template was created with `dotnet new safe` from [SAFE Stack](https://safe-stack.github.io/). I've then added the following features to approach something closer to the standard TodoMVC example. 

Server:
- replaced the in-memory storage with `LiteDB` using [LiteDB.FSharp](https://github.com/Zaid-Ajaj/LiteDB.FSharp).

Client:
- option to use in-memory storage so that I can run a live demo (see below) on github.io
- added view filters
- return key will add new todo
- double-click to edit existing do
- checkbox to toggle todo complete
- (x) button to delete todo
- (clear completed) button to delete all completed todos
- style.css

Build:
- publish.js, PublishApp target in build.fsx to support publish to gh-pages

[Live Demo](https://davedawkins.github.io/SAFE-template-extended/)

