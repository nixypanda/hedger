module View.Analytics exposing (warningList)

import Element exposing (Element)
import Ui.Feedback as Feedback
import Ui.Theme exposing (Theme)


warningList : Theme -> List String -> Element msg
warningList =
    Feedback.warningList
