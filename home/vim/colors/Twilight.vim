" Vim color file

set background=dark
highlight clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "Twilight"

if has("gui_running") || &t_Co == 88 || &t_Co == 256
    " functions {{{
    " returns an approximate grey index for the given grey level
    fun <SID>grey_number(x)
        if &t_Co == 88
            if a:x < 23
                return 0
            elseif a:x < 69
                return 1
            elseif a:x < 103
                return 2
            elseif a:x < 127
                return 3
            elseif a:x < 150
                return 4
            elseif a:x < 173
                return 5
            elseif a:x < 196
                return 6
            elseif a:x < 219
                return 7
            elseif a:x < 243
                return 8
            else
                return 9
            endif
        else
            if a:x < 14
                return 0
            else
                let l:n = (a:x - 8) / 10
                let l:m = (a:x - 8) % 10
                if l:m < 5
                    return l:n
                else
                    return l:n + 1
                endif
            endif
        endif
    endfun

    " returns the actual grey level represented by the grey index
    fun <SID>grey_level(n)
        if &t_Co == 88
            if a:n == 0
                return 0
            elseif a:n == 1
                return 46
            elseif a:n == 2
                return 92
            elseif a:n == 3
                return 115
            elseif a:n == 4
                return 139
            elseif a:n == 5
                return 162
            elseif a:n == 6
                return 185
            elseif a:n == 7
                return 208
            elseif a:n == 8
                return 231
            else
                return 255
            endif
        else
            if a:n == 0
                return 0
            else
                return 8 + (a:n * 10)
            endif
        endif
    endfun

    " returns the palette index for the given grey index
    fun <SID>grey_color(n)
        if &t_Co == 88
            if a:n == 0
                return 16
            elseif a:n == 9
                return 79
            else
                return 79 + a:n
            endif
        else
            if a:n == 0
                return 16
            elseif a:n == 25
                return 231
            else
                return 231 + a:n
            endif
        endif
    endfun

    " returns an approximate color index for the given color level
    fun <SID>rgb_number(x)
        if &t_Co == 88
            if a:x < 69
                return 0
            elseif a:x < 172
                return 1
            elseif a:x < 230
                return 2
            else
                return 3
            endif
        else
            if a:x < 75
                return 0
            else
                let l:n = (a:x - 55) / 40
                let l:m = (a:x - 55) % 40
                if l:m < 20
                    return l:n
                else
                    return l:n + 1
                endif
            endif
        endif
    endfun

    " returns the actual color level for the given color index
    fun <SID>rgb_level(n)
        if &t_Co == 88
            if a:n == 0
                return 0
            elseif a:n == 1
                return 139
            elseif a:n == 2
                return 205
            else
                return 255
            endif
        else
            if a:n == 0
                return 0
            else
                return 55 + (a:n * 40)
            endif
        endif
    endfun

    " returns the palette index for the given R/G/B color indices
    fun <SID>rgb_color(x, y, z)
        if &t_Co == 88
            return 16 + (a:x * 16) + (a:y * 4) + a:z
        else
            return 16 + (a:x * 36) + (a:y * 6) + a:z
        endif
    endfun

    " returns the palette index to approximate the given R/G/B color levels
    fun <SID>color(r, g, b)
        " get the closest grey
        let l:gx = <SID>grey_number(a:r)
        let l:gy = <SID>grey_number(a:g)
        let l:gz = <SID>grey_number(a:b)

        " get the closest color
        let l:x = <SID>rgb_number(a:r)
        let l:y = <SID>rgb_number(a:g)
        let l:z = <SID>rgb_number(a:b)

        if l:gx == l:gy && l:gy == l:gz
            " there are two possibilities
            let l:dgr = <SID>grey_level(l:gx) - a:r
            let l:dgg = <SID>grey_level(l:gy) - a:g
            let l:dgb = <SID>grey_level(l:gz) - a:b
            let l:dgrey = (l:dgr * l:dgr) + (l:dgg * l:dgg) + (l:dgb * l:dgb)
            let l:dr = <SID>rgb_level(l:gx) - a:r
            let l:dg = <SID>rgb_level(l:gy) - a:g
            let l:db = <SID>rgb_level(l:gz) - a:b
            let l:drgb = (l:dr * l:dr) + (l:dg * l:dg) + (l:db * l:db)
            if l:dgrey < l:drgb
                " use the grey
                return <SID>grey_color(l:gx)
            else
                " use the color
                return <SID>rgb_color(l:x, l:y, l:z)
            endif
        else
            " only one possibility
            return <SID>rgb_color(l:x, l:y, l:z)
        endif
    endfun

    " returns the palette index to approximate the 'rrggbb' hex string
    fun <SID>rgb(rgb)
        let l:r = ("0x" . strpart(a:rgb, 0, 2)) + 0
        let l:g = ("0x" . strpart(a:rgb, 2, 2)) + 0
        let l:b = ("0x" . strpart(a:rgb, 4, 2)) + 0

        return <SID>color(l:r, l:g, l:b)
    endfun

    " sets the highlighting for the given group
    fun <SID>X(group, fg, bg, attr)
        if a:fg != ""
            exec "hi " . a:group . " guifg=#" . a:fg . " ctermfg=" . <SID>rgb(a:fg)
        endif
        if a:bg != ""
            exec "hi " . a:group . " guibg=#" . a:bg . " ctermbg=" . <SID>rgb(a:bg)
        endif
        if a:attr != ""
            exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
        endif
    endfun
    " }}}

    call <SID>X("Normal", "ffffff", "", "")

    " highlight groups
    "call <SID>X("Cursor", "708090", "f0e68c", "")
    "CursorIM
    "Directory
    "DiffAdd
    "DiffChange
    "DiffDelete
    "DiffText
    "ErrorMsg
    "call <SID>X("VertSplit", "c2bfa5", "7f7f7f", "reverse")
    "call <SID>X("Folded", "ffd700", "4d4d4d", "")
    "call <SID>X("FoldColumn", "d2b48c", "4d4d4d", "")
    "call <SID>X("IncSearch", "708090", "f0e68c", "")
    call <SID>X("LineNr", "CCCCCC", "", "")
    "call <SID>X("ModeMsg", "D4D4D4", "", "")
    "call <SID>X("MoreMsg", "2e8b57", "", "")
    "call <SID>X("NonText", "addbe7", "000000", "bold")
    "call <SID>X("Question", "00ff7f", "", "")
    "call <SID>X("Search", "f5deb3", "cd853f", "")
    "call <SID>X("SpecialKey", "9acd32", "", "")
    "call <SID>X("StatusLine", "c2bfa5", "000000", "reverse")
    "call <SID>X("StatusLineNC", "c2bfa5", "7f7f7f", "reverse")
    "call <SID>X("Title", "cd5c5c", "", "")
    call <SID>X("Visual", "D3D3D3", "3E3E3E", "reverse")
    "VisualNOS
    "call <SID>X("WarningMsg", "fa8072", "", "")
    "WildMenu
    "Menu
    "Scrollbar
    "Tooltip

    " syntax highlighting groups
    call <SID>X("Comment", "828282", "", "")
    call <SID>X("Constant", "CF6A4C", "", "")
    call <SID>X("Identifier", "7587A6", "", "none")
    call <SID>X("Function", "9B703F", "", "")
    call <SID>X("Define", "CDA869", "", "none")
    call <SID>X("Statement", "CDA869", "", "")
    call <SID>X("String", "8F9D6A", "", "")
    call <SID>X("PreProc", "AFC4DB", "", "")
    call <SID>X("Type", "F9EE98", "", "")
    call <SID>X("Special", "DAEFA3", "", "")
    "Underlined
    call <SID>X("Ignore", "666666", "", "")
    "Error
    call <SID>X("Todo", "ff4500", "eeee00", "")

    " delete functions {{{
    delf <SID>X
    delf <SID>rgb
    delf <SID>color
    delf <SID>rgb_color
    delf <SID>rgb_level
    delf <SID>rgb_number
    delf <SID>grey_color
    delf <SID>grey_level
    delf <SID>grey_number
    " }}}
endif

" --------------------------- generated by coloration -------------------
hi Cursor ctermfg=233 ctermbg=248 cterm=NONE guifg=#141414 guibg=#a7a7a7 gui=NONE
" hi Visual ctermfg=NONE ctermbg=59 cterm=NONE guifg=NONE guibg=#3c4043 gui=NONE
hi CursorLine ctermfg=NONE ctermbg=235 cterm=NONE guifg=NONE guibg=#2b2b2b gui=NONE
hi CursorColumn ctermfg=NONE ctermbg=235 cterm=NONE guifg=NONE guibg=#2b2b2b gui=NONE
" hi ColorColumn ctermfg=NONE ctermbg=235 cterm=NONE guifg=NONE guibg=#2b2b2b gui=NONE
" hi LineNr ctermfg=245 ctermbg=235 cterm=NONE guifg=#868686 guibg=#2b2b2b gui=NONE
" hi VertSplit ctermfg=240 ctermbg=240 cterm=NONE guifg=#565656 guibg=#565656 gui=NONE
hi MatchParen ctermfg=179 ctermbg=black cterm=underline guifg=#cda869 guibg=NONE gui=underline
hi StatusLine ctermfg=231 ctermbg=240 cterm=bold guifg=#f8f8f8 guibg=#565656 gui=bold
hi StatusLineNC ctermfg=231 ctermbg=240 cterm=NONE guifg=#f8f8f8 guibg=#565656 gui=NONE
hi Pmenu ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi PmenuSel ctermfg=NONE ctermbg=59 cterm=NONE guifg=NONE guibg=#3c4043 gui=NONE

" hi Search ctermfg=NONE ctermbg=black cterm=underline guifg=NONE guibg=NONE gui=underline
hi Search ctermfg=black ctermbg=yellow cterm=NONE guifg=#141414 guibg=#6B8E23 gui=NONE
hi IncSearch ctermfg=black ctermbg=yellow cterm=NONE guifg=#141414 guibg=#8f9d6a gui=NONE

" hi Directory ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi Folded ctermfg=100 ctermbg=black cterm=NONE guifg=#5f5a60 guibg=#141414 gui=NONE
hi Normal ctermfg=231 ctermbg=black cterm=NONE guifg=#f8f8f8 guibg=#141414 gui=NONE
hi Boolean ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi Character ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi Comment ctermfg=59 ctermbg=black cterm=NONE guifg=#5f5a60 guibg=NONE gui=italic
hi Conditional ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi Constant ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi Define ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi DiffAdd ctermfg=231 ctermbg=64 cterm=bold guifg=#f8f8f8 guibg=#427f09 gui=bold
hi DiffDelete ctermfg=88 ctermbg=black cterm=NONE guifg=#870404 guibg=NONE gui=NONE
hi DiffChange ctermfg=231 ctermbg=17 cterm=NONE guifg=#f8f8f8 guibg=#1a2f4e gui=NONE
hi DiffText ctermfg=231 ctermbg=24 cterm=bold guifg=#f8f8f8 guibg=#204a87 gui=bold
" hi ErrorMsg ctermfg=NONE ctermbg=black cterm=NONE guifg=NONE guibg=NONE gui=NONE
" hi WarningMsg ctermfg=NONE ctermbg=black cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi Float ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
" hi Function ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi Identifier ctermfg=228 ctermbg=black cterm=NONE guifg=#f9ee98 guibg=NONE gui=NONE
hi Keyword ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi Label ctermfg=107 ctermbg=black cterm=NONE guifg=#8f9d6a guibg=NONE gui=NONE
hi NonText ctermfg=239 ctermbg=black cterm=NONE guifg=#4f4f4f guibg=#1f1f1f gui=NONE
hi Number ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi Operator ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi PreProc ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi Special ctermfg=231 ctermbg=black cterm=NONE guifg=#f8f8f8 guibg=NONE gui=NONE

hi SpecialKey ctermfg=blue ctermbg=black cterm=NONE guifg=#4f4f4f guibg=#2b2b2b gui=NONE

hi Statement ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi StorageClass ctermfg=228 ctermbg=black cterm=NONE guifg=#f9ee98 guibg=NONE gui=NONE
hi String ctermfg=107 ctermbg=black cterm=NONE guifg=#8f9d6a guibg=NONE gui=NONE
hi Tag ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi Title ctermfg=231 ctermbg=black cterm=bold guifg=#f8f8f8 guibg=NONE gui=bold
hi Todo ctermfg=59 ctermbg=black cterm=inverse,bold guifg=#5f5a60 guibg=NONE gui=inverse,bold,italic
hi Type ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi Underlined ctermfg=NONE ctermbg=black cterm=underline guifg=NONE guibg=NONE gui=underline
hi rubyClass ctermfg=180 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi rubyFunction ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi rubyInterpolationDelimiter ctermfg=106 ctermbg=black cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi rubySymbol ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi rubyConstant ctermfg=104 ctermbg=black cterm=NONE guifg=#9b859d guibg=NONE gui=NONE
hi rubyStringDelimiter ctermfg=107 ctermbg=black cterm=NONE guifg=#8f9d6a guibg=NONE gui=NONE
hi rubyBlockParameter ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi rubyInstanceVariable ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi rubyInclude ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi rubyGlobalVariable ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi rubyRegexp ctermfg=179 ctermbg=black cterm=NONE guifg=#e9c062 guibg=NONE gui=NONE
hi rubyRegexpDelimiter ctermfg=179 ctermbg=black cterm=NONE guifg=#e9c062 guibg=NONE gui=NONE
hi rubyEscape ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi rubyControl ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi rubyClassVariable ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi rubyOperator ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi rubyException ctermfg=179 ctermbg=black cterm=NONE guifg=#cda869 guibg=NONE gui=NONE
hi rubyPseudoVariable ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi rubyRailsUserClass ctermfg=103 ctermbg=black cterm=NONE guifg=#9b859d guibg=NONE gui=NONE
hi rubyRailsARAssociationMethod ctermfg=186 ctermbg=black cterm=NONE guifg=#dad085 guibg=NONE gui=NONE
hi rubyRailsARMethod ctermfg=186 ctermbg=black cterm=NONE guifg=#dad085 guibg=NONE gui=NONE
hi rubyRailsRenderMethod ctermfg=186 ctermbg=black cterm=NONE guifg=#dad085 guibg=NONE gui=NONE
hi rubyRailsMethod ctermfg=186 ctermbg=black cterm=NONE guifg=#dad085 guibg=NONE gui=NONE
hi erubyDelimiter ctermfg=NONE ctermbg=black cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi erubyComment ctermfg=59 ctermbg=black cterm=NONE guifg=#5f5a60 guibg=NONE gui=italic
hi erubyRailsMethod ctermfg=186 ctermbg=black cterm=NONE guifg=#dad085 guibg=NONE gui=NONE
hi htmlTag ctermfg=137 ctermbg=black cterm=NONE guifg=#ac885b guibg=NONE gui=NONE
hi htmlEndTag ctermfg=137 ctermbg=black cterm=NONE guifg=#ac885b guibg=NONE gui=NONE
hi htmlTagName ctermfg=137 ctermbg=black cterm=NONE guifg=#ac885b guibg=NONE gui=NONE
hi htmlArg ctermfg=137 ctermbg=black cterm=NONE guifg=#ac885b guibg=NONE gui=NONE
hi htmlSpecialChar ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi javaScriptFunction ctermfg=228 ctermbg=black cterm=NONE guifg=#f9ee98 guibg=NONE gui=NONE
hi javaScriptRailsFunction ctermfg=186 ctermbg=black cterm=NONE guifg=#dad085 guibg=NONE gui=NONE
hi javaScriptBraces ctermfg=NONE ctermbg=black cterm=NONE guifg=NONE guibg=NONE gui=NONE
hi yamlKey ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi yamlAnchor ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi yamlAlias ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi yamlDocumentHeader ctermfg=107 ctermbg=black cterm=NONE guifg=#8f9d6a guibg=NONE gui=NONE
hi cssURL ctermfg=103 ctermbg=black cterm=NONE guifg=#7587a6 guibg=NONE gui=NONE
hi cssFunctionName ctermfg=186 ctermbg=black cterm=NONE guifg=#dad085 guibg=NONE gui=NONE
hi cssColor ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi cssPseudoClassId ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi cssClassName ctermfg=95 ctermbg=black cterm=NONE guifg=#9b703f guibg=NONE gui=NONE
hi cssValueLength ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi cssCommonAttr ctermfg=167 ctermbg=black cterm=NONE guifg=#cf6a4c guibg=NONE gui=NONE
hi cssBraces ctermfg=NONE ctermbg=black cterm=NONE guifg=NONE guibg=NONE gui=NONE

" ---------------------------------------------------
" Vimdiff colors
highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17
highlight DiffText   cterm=bold ctermfg=10 ctermbg=88

" colors for folds
highlight Folded gui=italic   guifg=black   guibg=#3467ff
highlight Folded cterm=italic ctermfg=black ctermbg=110

