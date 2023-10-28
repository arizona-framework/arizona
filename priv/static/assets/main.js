"use strict"

const view = document.querySelector(`meta[name="view"]`).getAttribute("content")

// Arizona

const arizona = arizonaFactory()

const params = { view }

arizona.connect(params, () => {
    console.log("Arizona is connected")
})
