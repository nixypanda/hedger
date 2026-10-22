module Ui.Theme exposing
    ( Mode(..)
    , Theme
    , forMode
    , light
    , modeFromStringResult
    , modeToString
    , toggle
    , transparentColor
    )

import Element exposing (Color, rgb255)


type Mode
    = Light
    | Dark


type alias Theme =
    { mode : Mode
    , pageBg : Color
    , cardBg : Color
    , surface : Color
    , elevatedBg : Color
    , textPrimary : Color
    , textMedium : Color
    , textMuted : Color
    , transparent : Color
    , divider : Color
    , inputFill : Color
    , primary : Color
    , primaryButtonFg : Color
    , primaryPressed : Color
    , primarySoft : Color
    , primaryDisabled : Color
    , dangerBg : Color
    , dangerFg : Color
    , dangerSolid : Color
    , dangerSolidFg : Color
    , successBg : Color
    , successFg : Color
    , successSolid : Color
    , warningBg : Color
    , warningFg : Color
    , warningSolid : Color
    , selectedBg : Color
    , hoverBg : Color
    , focusRing : Color
    , positive : Color
    , negative : Color
    , neutral : Color
    , asset : Color
    , liability : Color
    , income : Color
    , expense : Color
    , equity : Color
    , stale : Color
    , chipBg : Color
    , chipFg : Color
    , chipBorder : Color
    , chipSelectedBg : Color
    , chipSelectedFg : Color
    , borrowingBg : Color
    , borrowingFg : Color
    , repaymentBg : Color
    , repaymentFg : Color
    , assetBg : Color
    , assetFg : Color
    , navigationActiveBg : Color
    , navigationActiveFg : Color
    , navigationBrandBg : Color
    , navigationFg : Color
    , navigationGradientEnd : Color
    , navigationGradientStart : Color
    , chartSeries : List Color
    , acquireScale : List Color
    , rebalanceScale : List Color
    , disposeScale : List Color
    , cardShadow : String
    }


transparentColor : Color
transparentColor =
    Element.rgba 0 0 0 0


light : Theme
light =
    { mode = Light
    , pageBg = rgb255 246 247 255
    , cardBg = rgb255 255 255 255
    , surface = rgb255 255 255 255
    , elevatedBg = rgb255 255 255 255
    , textPrimary = rgb255 30 24 61
    , textMedium = rgb255 73 66 105
    , textMuted = rgb255 107 100 135
    , transparent = transparentColor
    , divider = rgb255 232 230 246
    , inputFill = rgb255 249 248 255
    , primary = rgb255 91 46 196
    , primaryButtonFg = rgb255 255 255 255
    , primaryPressed = rgb255 83 44 194
    , primarySoft = rgb255 241 236 255
    , primaryDisabled = rgb255 186 165 235
    , dangerBg = rgb255 254 242 242
    , dangerFg = rgb255 153 27 27
    , dangerSolid = rgb255 185 28 28
    , dangerSolidFg = rgb255 255 255 255
    , successBg = rgb255 236 253 245
    , successFg = rgb255 4 120 87
    , successSolid = rgb255 5 150 105
    , warningBg = rgb255 255 251 235
    , warningFg = rgb255 146 64 14
    , warningSolid = rgb255 245 158 11
    , selectedBg = rgb255 239 233 255
    , hoverBg = rgb255 247 244 255
    , focusRing = rgb255 124 58 237
    , positive = rgb255 16 185 129
    , negative = rgb255 244 63 94
    , neutral = rgb255 71 85 105
    , asset = rgb255 59 130 246
    , liability = rgb255 244 63 94
    , income = rgb255 16 185 129
    , expense = rgb255 245 158 11
    , equity = rgb255 124 58 237
    , stale = rgb255 148 163 184
    , chipBg = rgb255 241 245 249
    , chipFg = rgb255 55 65 81
    , chipBorder = rgb255 203 213 225
    , chipSelectedBg = rgb255 112 62 232
    , chipSelectedFg = rgb255 255 255 255
    , borrowingBg = rgb255 255 241 242
    , borrowingFg = rgb255 190 18 60
    , repaymentBg = rgb255 207 250 254
    , repaymentFg = rgb255 15 118 110
    , assetBg = rgb255 239 246 255
    , assetFg = rgb255 37 99 235
    , navigationActiveBg = rgb255 255 255 255
    , navigationActiveFg = rgb255 89 49 190
    , navigationBrandBg = Element.rgba255 255 255 255 0.18
    , navigationFg = rgb255 239 236 255
    , navigationGradientEnd = rgb255 69 80 229
    , navigationGradientStart = rgb255 126 48 232
    , chartSeries =
        [ rgb255 112 62 232
        , rgb255 16 185 129
        , rgb255 245 158 11
        , rgb255 59 130 246
        , rgb255 6 182 212
        , rgb255 244 63 94
        , rgb255 100 116 139
        , rgb255 15 118 110
        , rgb255 194 65 12
        , rgb255 217 70 239
        ]
    , acquireScale =
        [ rgb255 4 120 87
        , rgb255 13 148 136
        , rgb255 22 163 74
        , rgb255 77 124 15
        , rgb255 14 116 144
        , rgb255 21 128 61
        ]
    , rebalanceScale =
        [ rgb255 29 78 216
        , rgb255 14 165 233
        , rgb255 79 70 229
        , rgb255 8 145 178
        , rgb255 37 99 235
        , rgb255 2 132 199
        ]
    , disposeScale =
        [ rgb255 190 18 60
        , rgb255 220 38 38
        , rgb255 219 39 119
        , rgb255 234 88 12
        , rgb255 159 18 57
        , rgb255 225 29 72
        ]
    , cardShadow = "0 1px 2px rgba(76, 29, 149, 0.04), 0 10px 30px rgba(76, 29, 149, 0.07)"
    }


dark : Theme
dark =
    { mode = Dark
    , pageBg = rgb255 15 23 42
    , cardBg = rgb255 30 41 59
    , surface = rgb255 30 41 59
    , elevatedBg = rgb255 39 51 73
    , textPrimary = rgb255 241 245 249
    , textMedium = rgb255 203 213 225
    , textMuted = rgb255 148 163 184
    , transparent = transparentColor
    , divider = rgb255 51 65 85
    , inputFill = rgb255 15 23 42
    , primary = rgb255 96 165 250
    , primaryButtonFg = rgb255 15 23 42
    , primaryPressed = rgb255 147 197 253
    , primarySoft = rgb255 30 58 95
    , primaryDisabled = rgb255 51 89 137
    , dangerBg = rgb255 69 26 32
    , dangerFg = rgb255 252 165 165
    , dangerSolid = rgb255 248 113 113
    , dangerSolidFg = rgb255 15 23 42
    , successBg = rgb255 20 66 54
    , successFg = rgb255 110 231 183
    , successSolid = rgb255 52 211 153
    , warningBg = rgb255 71 50 20
    , warningFg = rgb255 251 191 36
    , warningSolid = rgb255 251 191 36
    , selectedBg = rgb255 30 58 95
    , hoverBg = rgb255 39 51 73
    , focusRing = rgb255 96 165 250
    , positive = rgb255 52 211 153
    , negative = rgb255 248 113 113
    , neutral = rgb255 163 177 196
    , asset = rgb255 34 211 238
    , liability = rgb255 251 113 133
    , income = rgb255 74 222 128
    , expense = rgb255 251 191 36
    , equity = rgb255 165 180 252
    , stale = rgb255 100 116 139
    , chipBg = rgb255 51 65 85
    , chipFg = rgb255 226 232 240
    , chipBorder = rgb255 71 85 105
    , chipSelectedBg = rgb255 96 165 250
    , chipSelectedFg = rgb255 15 23 42
    , borrowingBg = rgb255 76 17 36
    , borrowingFg = rgb255 251 113 133
    , repaymentBg = rgb255 8 51 47
    , repaymentFg = rgb255 94 234 212
    , assetBg = rgb255 23 37 84
    , assetFg = rgb255 147 197 253
    , navigationActiveBg = rgb255 255 255 255
    , navigationActiveFg = rgb255 89 49 190
    , navigationBrandBg = Element.rgba255 255 255 255 0.18
    , navigationFg = rgb255 239 236 255
    , navigationGradientEnd = rgb255 69 80 229
    , navigationGradientStart = rgb255 126 48 232
    , chartSeries =
        [ rgb255 96 165 250
        , rgb255 52 211 153
        , rgb255 251 191 36
        , rgb255 165 180 252
        , rgb255 34 211 238
        , rgb255 251 113 133
        , rgb255 148 163 184
        , rgb255 45 212 191
        , rgb255 251 146 60
        , rgb255 216 180 254
        ]
    , acquireScale =
        [ rgb255 52 211 153
        , rgb255 110 231 183
        , rgb255 74 222 128
        , rgb255 45 212 191
        , rgb255 163 230 53
        , rgb255 16 185 129
        ]
    , rebalanceScale =
        [ rgb255 96 165 250
        , rgb255 56 189 248
        , rgb255 129 140 248
        , rgb255 147 197 253
        , rgb255 59 130 246
        , rgb255 6 182 212
        ]
    , disposeScale =
        [ rgb255 251 113 133
        , rgb255 248 113 113
        , rgb255 244 114 182
        , rgb255 251 146 60
        , rgb255 253 164 175
        , rgb255 239 68 68
        ]
    , cardShadow = "0 1px 2px rgba(0, 0, 0, 0.24), 0 10px 28px rgba(0, 0, 0, 0.32)"
    }


forMode : Mode -> Theme
forMode mode =
    case mode of
        Light ->
            light

        Dark ->
            dark


toggle : Mode -> Mode
toggle mode =
    case mode of
        Light ->
            Dark

        Dark ->
            Light


modeToString : Mode -> String
modeToString mode =
    case mode of
        Light ->
            "light"

        Dark ->
            "dark"


modeFromStringResult : String -> Result String Mode
modeFromStringResult value =
    case value of
        "light" ->
            Ok Light

        "dark" ->
            Ok Dark

        _ ->
            Err "Unknown theme mode."
