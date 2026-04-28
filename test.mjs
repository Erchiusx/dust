import mod from "./polyfill.mjs"

mod.test()
mod.test()
console.log( await mod.returns() )
