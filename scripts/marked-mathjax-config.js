{
    loader: { load: ["input/tex", "output/svg"] },
    messageStyle: "none",
    packages: { "[+]": ["tagFormat"] },
    tagSide: "right",
    "HTML-CSS": {
        messageStyle: "normal",
        linebreaks: {
            automatic: false
        }
    },
    svg: {
        fontCache: "global",
    },
    tex2jax: {
        tags: "ams",
        inlineMath: [["$","$"],["\\(","\\)"]],
        displayMath: [["$$","$$"],["\\[","\\]"]],
        processEscapes: true,
        macros: {
            nth: ["n\\text{th}"],
            NN: ["\\mathbb{N}"],
            ZZ: ["\\mathbb{Z}"],
            QQ: ["\\mathbb{Q}"],
            RR: ["\\mathbb{R}"],
            II: ["\\mathbb{I}"],
            CC: ["\\mathbb{C}"],
            cond: ["\\\hspace{5pt}\\text{#1}\\hspace{5pt}{#2}", 2],
            Int: ["\\int_{#3}^{#4}\\!{#2}\\,\\mathrm{d}{#1}", 4],
            eval: ["\\left[ #1 \\right \\vert_{#2}^{#3}", 3],
            paren: ["\\left({#1}\\right)", 1],
            rfrac: ["{}^{#1}\\!/_{#2}", 2],
            abs: ["\\left\\lvert{#1}\\right\\rvert", 1],
            stext: ["\\hspace{10pt}\\text{#1}\\hspace{10pt}", 1],
            sfrac: [" \\stackrel{#1}{}\\!\\!\\unicode{x2215}_{\\!#2}", 2],
            d: ["\\mathrm{d}"],
            dd: ["\\frac{\\mathrm{d}#1}{\\mathrm{d}#2}", 2],
        },
    }
}
