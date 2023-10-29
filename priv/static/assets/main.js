"use strict"

// Arizona

const arizona = arizonaFactory()

const params = { }

arizona.connect(params, () => {
    console.log("Arizona is connected")
})
