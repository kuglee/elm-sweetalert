module Sweetalert exposing
    ( Button
    , ButtonType(..)
    , Config
    , Icon(..)
    , Msg
    , Padding
    , Position(..)
    , State
    , focusOnButton
    , getFocusOn
    , init
    , isVisible
    , new
    , toConfig
    , update
    , view
    , withBackdropHideAnimation
    , withBackdropShowAnimation
    , withBackdropStyle
    , withBackground
    , withButtonRowStyle
    , withCancelButtonAriaLabel
    , withCancelButtonColor
    , withCancelButtonStyle
    , withCancelButtonText
    , withCloseButtonAriaLabel
    , withCloseButtonStyle
    , withConfirmButtonAriaLabel
    , withConfirmButtonColor
    , withConfirmButtonStyle
    , withConfirmButtonText
    , withContainerStyle
    , withDenyButtonAriaLabel
    , withDenyButtonColor
    , withDenyButtonStyle
    , withDenyButtonText
    , withFocusOn
    , withFooter
    , withHeader
    , withIcon
    , withIconColor
    , withIconHideAnimation
    , withIconLabel
    , withIconShowAnimation
    , withIconStyle
    , withOnBackdropPress
    , withOnCancelButtonPress
    , withOnCloseButtonPress
    , withOnConfirmButtonPress
    , withOnDenyButtonPress
    , withOnEscapeKeyPress
    , withPadding
    , withPopupHideAnimation
    , withPopupShowAnimation
    , withPopupStyle
    , withPosition
    , withReverseButtons
    , withShowCancelButton
    , withShowCloseButton
    , withShowDenyButton
    , withText
    , withTextLabel
    , withTextStyle
    , withTitleLabel
    , withTitleStyle
    , withTitleText
    , withWidth
    , withoutAllowEnterKey
    , withoutAllowEscapeKey
    , withoutAllowOutsidePress
    , withoutShowBackdrop
    , withoutShowConfirmButton
    , withoutShowFooterDivider
    )

import Array exposing (Array)
import Browser.Dom as Dom
import Css
import Css.Global
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Html.Styled
import Json.Decode as Decode exposing (Decoder)
import Keyboard.Event exposing (KeyboardEvent, considerKeyboardEvent)
import Keyboard.Key as Key
import Simple.Animation as Animation exposing (Animation, Millis)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Task


type alias Model =
    { visibility : Visibility
    , currentFocusedButton : Maybe ButtonType
    , previousFocusedButton : Maybe ButtonType
    , backdropAnimationCompleted : Bool
    , popupAnimationCompleted : Bool
    }


type State
    = State Model


init : State
init =
    State
        { visibility = Visible
        , currentFocusedButton = Nothing
        , previousFocusedButton = Nothing
        , backdropAnimationCompleted = False
        , popupAnimationCompleted = False
        }


isVisible : State -> Bool
isVisible (State model) =
    model.visibility /= Hidden


type Visibility
    = Visible
    | Hiding
    | Hidden


type HidAnimationType
    = Backdrop
    | Popup


updateAnimationCompleted : HidAnimationType -> Model -> Model
updateAnimationCompleted hideAnimationType model =
    case hideAnimationType of
        Backdrop ->
            { model | backdropAnimationCompleted = True }

        Popup ->
            { model | popupAnimationCompleted = True }


type Msg
    = NoOp
    | BackdropPressed
    | DismissAnimationCompleted HidAnimationType
    | EscapeKeyPressed
    | CloseButtonPressed
    | CancelButtonPressed
    | DenyButtonPressed
    | ConfirmButtonPressed
    | FocusOnButton ButtonType
    | ButtonFocusResult (Result Dom.Error ())
    | FocusOnDummy String
    | GotFocus ButtonType
    | LostFocus ButtonType


focusOnButton : ButtonType -> Msg
focusOnButton buttonType =
    FocusOnButton buttonType


getFocusOn : Config msg -> Maybe ButtonType
getFocusOn (Config config) =
    config.focusOn


update : Msg -> State -> ( State, Cmd Msg )
update msg (State model) =
    Tuple.mapFirst State <|
        case msg of
            NoOp ->
                ( model, Cmd.none )

            BackdropPressed ->
                ( { model | visibility = Hiding }
                , Task.perform (\_ -> FocusOnDummy backdropPressDummyButtonId)
                    (Task.succeed ())
                )

            DismissAnimationCompleted animationCompleted ->
                let
                    newModel =
                        updateAnimationCompleted animationCompleted model

                    visibility =
                        if
                            newModel.backdropAnimationCompleted
                                && newModel.popupAnimationCompleted
                        then
                            Hidden

                        else
                            model.visibility
                in
                ( { newModel
                    | visibility = visibility
                    , currentFocusedButton = Nothing
                    , previousFocusedButton = Nothing
                  }
                , Cmd.none
                )

            EscapeKeyPressed ->
                ( { model | visibility = Hiding }
                , Task.perform (\_ -> FocusOnDummy escapeKeyDummyButtonId)
                    (Task.succeed ())
                )

            CloseButtonPressed ->
                ( { model | visibility = Hiding }, Cmd.none )

            CancelButtonPressed ->
                ( { model | visibility = Hiding }, Cmd.none )

            DenyButtonPressed ->
                ( { model | visibility = Hiding }, Cmd.none )

            ConfirmButtonPressed ->
                ( { model | visibility = Hiding }, Cmd.none )

            FocusOnButton buttonType ->
                ( { model | currentFocusedButton = Just buttonType }
                , Task.attempt ButtonFocusResult (Dom.focus (buttonTypeToId buttonType))
                )

            FocusOnDummy id ->
                ( model
                , Task.attempt ButtonFocusResult (Dom.focus id)
                )

            ButtonFocusResult result ->
                case result of
                    Ok () ->
                        ( model, Cmd.none )

                    Err (Dom.NotFound _) ->
                        ( { model | currentFocusedButton = Nothing }
                        , Cmd.none
                        )

            GotFocus buttonType ->
                let
                    previousFocusedButton =
                        if
                            lostFocus
                                model.currentFocusedButton
                                model.previousFocusedButton
                        then
                            Nothing

                        else
                            model.previousFocusedButton
                in
                ( { model
                    | currentFocusedButton = Just buttonType
                    , previousFocusedButton = previousFocusedButton
                  }
                , Cmd.none
                )

            LostFocus buttonType ->
                ( { model | previousFocusedButton = Just buttonType }, Cmd.none )


type alias Button msg =
    { attributes : List (Attribute msg)
    , onPress : Maybe msg
    , label : Element msg
    }



-- CONFIG


type Config msg
    = Config
        { titleText : String
        , titleLabel : Maybe (Element msg)
        , text : String
        , textLabel : Maybe (Element msg)
        , iconLabel : Maybe (Element msg)
        , iconColor : Maybe Color
        , icon : Maybe Icon
        , popupStyle : Maybe (List (Attribute msg))
        , containerStyle : List (Attribute msg)
        , iconStyle : List (Attribute msg)
        , titleStyle : List (Attribute msg)
        , textStyle : List (Attribute msg)
        , cancelButtonStyle : Maybe (List (Attribute msg))
        , denyButtonStyle : Maybe (List (Attribute msg))
        , confirmButtonStyle : Maybe (List (Attribute msg))
        , confirmButtonColor : Maybe Color
        , denyButtonColor : Maybe Color
        , cancelButtonColor : Maybe Color
        , buttonRowStyle : List (Attribute msg)
        , reverseButtons : Bool
        , showCancelButton : Bool
        , showDenyButton : Bool
        , showConfirmButton : Bool
        , cancelButtonText : String
        , denyButtonText : String
        , confirmButtonText : String
        , popupShowAnimation : Maybe Animation
        , iconShowAnimation : Maybe Animation
        , backdropShowAnimation : Maybe Animation
        , popupHideAnimation : Maybe Animation
        , iconHideAnimation : Maybe Animation
        , backdropHideAnimation : Maybe Animation
        , showBackdrop : Bool
        , backdropStyle : List (Attribute msg)
        , allowOutsidePress : Bool
        , showCloseButton : Bool
        , closeButtonStyle : List (Attribute msg)
        , allowEnterKey : Bool
        , allowEscapeKey : Bool
        , focusOn : Maybe ButtonType
        , closeButtonAriaLabel : Maybe String
        , confirmButtonAriaLabel : Maybe String
        , denyButtonAriaLabel : Maybe String
        , cancelButtonAriaLabel : Maybe String
        , header : Maybe (Element msg)
        , footer : Maybe (Element msg)
        , onCancelButtonPress : Maybe msg
        , onDenyButtonPress : Maybe msg
        , onConfirmButtonPress : Maybe msg
        , onCloseButtonPress : Maybe msg
        , onBackdropPress : Maybe msg
        , onEscapeKeyPress : Maybe msg
        , width : Length
        , padding : Padding
        , background : List (Attribute msg)
        , position : Position
        , showFooterDivider : Bool
        }


type Conf constraints msg
    = Conf (Config msg)


toConfig : Conf constraints msg -> Config msg
toConfig (Conf config) =
    config


new :
    Conf
        { canHaveTitleLabel : ()
        , canHaveTitleText : ()
        , canHaveTextLabel : ()
        , canHaveText : ()
        , canHaveIcon : ()
        , hasIconOrIconLabel : ()
        , canHavePopupStyle : ()
        , canHaveContainerStyle : ()
        , canHaveIconStyle : ()
        , canHaveTitleStyle : ()
        , canHaveTextStyle : ()
        , canHaveButtonStyle : ()
        , canHaveCancelButtonStyle : ()
        , canHaveDenyButtonStyle : ()
        , canHaveConfirmButtonStyle : ()
        , canHaveButtonRowStyle : ()
        , canHaveReverseButtons : ()
        , canHaveShowCancelButton : ()
        , canHaveShowDenyButton : ()
        , canHaveShowConfirmButton : ()
        , canHaveCancelButtonText : ()
        , canHaveDenyButtonText : ()
        , canHaveConfirmButtonText : ()
        , canHaveShowBackdrop : ()
        , canHaveBackdropStyle : ()
        , canHaveAllowOutsidePress : ()
        , canHaveShowCloseButton : ()
        , canHaveCloseButtonStyle : ()
        , canHaveAllowEnterKey : ()
        , canHaveAllowEscapeKey : ()
        , canHaveFocusOn : ()
        , canHaveCloseButtonAriaLabel : ()
        , canHaveConfirmButtonAriaLabel : ()
        , canHaveDenyButtonAriaLabel : ()
        , canHaveCancelButtonAriaLabel : ()
        , canHaveHeader : ()
        , canHaveFooter : ()
        , canHaveOnConfirmButtonPress : ()
        , canHaveOnDenyButtonPress : ()
        , canHaveOnCancelButtonPress : ()
        , canHaveOnCloseButtonPress : ()
        , canHaveOnBackdropPress : ()
        , canHaveOnEscapeKeyPress : ()
        , canHaveIconLabel : ()
        , canHaveIconColor : ()
        , canHaveConfirmButtonColor : ()
        , canHaveDenyButtonColor : ()
        , canHaveCancelButtonColor : ()
        , canHaveWidth : ()
        , canHavePadding : ()
        , canHavePosition : ()
        , canHaveBackground : ()
        , canHaveShowFooterDivider : ()
        , canHavePopupShowAnimation : ()
        , canHaveIconShowAnimation : ()
        , canHaveBackdropShowAnimation : ()
        , canHavePopupHideAnimation : ()
        , canHaveIconHideAnimation : ()
        , canHaveBackdropHideAnimation : ()
        , hasConfirmButton : ()
        , hasBackdrop : ()
        }
        msg
new =
    Conf
        (Config
            { titleText = ""
            , titleLabel = Nothing
            , text = ""
            , textLabel = Nothing
            , iconLabel = Nothing
            , iconColor = Nothing
            , icon = Nothing
            , popupStyle = Nothing
            , containerStyle = defaultContainerStyle
            , iconStyle = defaultIconStyle
            , titleStyle = defaultTitleStyle
            , textStyle = defaultTextStyle
            , cancelButtonStyle = Nothing
            , denyButtonStyle = Nothing
            , confirmButtonStyle = Nothing
            , confirmButtonColor = Nothing
            , denyButtonColor = Nothing
            , cancelButtonColor = Nothing
            , buttonRowStyle = defaultButtonRowStyle
            , reverseButtons = False
            , showCancelButton = False
            , showDenyButton = False
            , showConfirmButton = True
            , cancelButtonText = "Cancel"
            , denyButtonText = "No"
            , confirmButtonText = "OK"
            , popupShowAnimation = Just defaultAppearanceAnimation
            , iconShowAnimation = Nothing
            , backdropShowAnimation = Nothing
            , popupHideAnimation = Just defaultDisappearanceAnimation
            , iconHideAnimation = Nothing
            , backdropHideAnimation = Nothing
            , showBackdrop = True
            , backdropStyle = defaultBackdropStyle
            , allowOutsidePress = True
            , showCloseButton = False
            , closeButtonStyle = defaultCloseButtonStyle
            , allowEnterKey = True
            , allowEscapeKey = True
            , focusOn = Just Confirm
            , closeButtonAriaLabel = Just "Close this dialog"
            , confirmButtonAriaLabel = Nothing
            , denyButtonAriaLabel = Nothing
            , cancelButtonAriaLabel = Nothing
            , header = Nothing
            , footer = Nothing
            , onCancelButtonPress = Nothing
            , onDenyButtonPress = Nothing
            , onConfirmButtonPress = Nothing
            , onCloseButtonPress = Nothing
            , onBackdropPress = Nothing
            , onEscapeKeyPress = Nothing
            , width = px 512
            , padding = { top = 0, bottom = 20, left = 0, right = 0 }
            , background = defaultBackgroundStyle
            , position = Center
            , showFooterDivider = True
            }
        )


withTitleLabel :
    Element msg
    ->
        Conf
            { constraints
                | canHaveTitleLabel : ()
                , canHaveTitleText : ()
                , canHaveTitleStyle : ()
            }
            msg
    -> Conf constraints msg
withTitleLabel label (Conf (Config config)) =
    Conf (Config { config | titleLabel = Just label })


withTitleText :
    String
    -> Conf { constraints | canHaveTitleText : (), canHaveTitleLabel : () } msg
    -> Conf constraints msg
withTitleText text (Conf (Config config)) =
    Conf (Config { config | titleText = text })


withTitleStyle :
    List (Attribute msg)
    -> Conf { constraints | canHaveTitleStyle : () } msg
    -> Conf constraints msg
withTitleStyle style (Conf (Config config)) =
    Conf (Config { config | titleStyle = style })


withTextLabel :
    Element msg
    ->
        Conf
            { constraints
                | canHaveTextLabel : ()
                , canHaveText : ()
                , canHaveTextStyle : ()
            }
            msg
    -> Conf constraints msg
withTextLabel label (Conf (Config config)) =
    Conf (Config { config | textLabel = Just label })


withText :
    String
    -> Conf { constraints | canHaveText : (), canHaveTextLabel : () } msg
    -> Conf constraints msg
withText text (Conf (Config config)) =
    Conf (Config { config | text = text })


withTextStyle :
    List (Attribute msg)
    -> Conf { constraints | canHaveTextStyle : () } msg
    -> Conf constraints msg
withTextStyle style (Conf (Config config)) =
    Conf (Config { config | textStyle = style })


withIcon :
    Icon
    -> Conf { constraints | canHaveIcon : (), canHaveIconLabel : () } msg
    -> Conf { constraints | hasIcon : (), hasIconOrIconLabel : () } msg
withIcon icon (Conf (Config config)) =
    Conf (Config { config | icon = Just icon })


withIconLabel :
    Element msg
    -> Conf { constraints | canHaveIconLabel : (), canHaveIcon : () } msg
    -> Conf { constraints | hasIconOrIconLabel : () } msg
withIconLabel label (Conf (Config config)) =
    Conf (Config { config | iconLabel = Just label })


withIconStyle :
    List (Attribute msg)
    -> Conf { constraints | canHaveIconStyle : (), hasIcon : () } msg
    -> Conf { constraints | hasIcon : () } msg
withIconStyle style (Conf (Config config)) =
    Conf (Config { config | iconStyle = style })


withIconColor :
    Color
    -> Conf { constraints | canHaveIconColor : (), hasIcon : () } msg
    -> Conf { constraints | hasIcon : () } msg
withIconColor color (Conf (Config config)) =
    Conf (Config { config | iconColor = Just color })


withoutShowConfirmButton :
    Conf
        { constraints | canHaveShowConfirmButton : (), hasConfirmButton : () }
        msg
    -> Conf constraints msg
withoutShowConfirmButton (Conf (Config config)) =
    Conf (Config { config | showConfirmButton = False })


withOnConfirmButtonPress :
    msg
    -> Conf { constraints | canHaveOnConfirmButtonPress : () } msg
    -> Conf constraints msg
withOnConfirmButtonPress msg (Conf (Config config)) =
    Conf (Config { config | onConfirmButtonPress = Just msg })


withConfirmButtonText :
    String
    ->
        Conf
            { constraints
                | canHaveConfirmButtonText : ()
                , hasConfirmButton : ()
            }
            msg
    -> Conf { constraints | hasConfirmButton : () } msg
withConfirmButtonText text (Conf (Config config)) =
    Conf (Config { config | confirmButtonText = text })


withConfirmButtonAriaLabel :
    String
    ->
        Conf
            { constraints
                | canHaveConfirmButtonAriaLabel : ()
                , hasConfirmButton : ()
            }
            msg
    -> Conf { constraints | hasConfirmButton : () } msg
withConfirmButtonAriaLabel text (Conf (Config config)) =
    Conf (Config { config | confirmButtonAriaLabel = Just text })


withConfirmButtonStyle :
    List (Attribute msg)
    ->
        Conf
            { constraints
                | canHaveConfirmButtonStyle : ()
                , hasConfirmButton : ()
            }
            msg
    -> Conf { constraints | hasConfirmButton : () } msg
withConfirmButtonStyle style (Conf (Config config)) =
    Conf (Config { config | confirmButtonStyle = Just style })


withConfirmButtonColor :
    Color
    ->
        Conf
            { constraints
                | canHaveConfirmButtonColor : ()
                , hasConfirmButton : ()
            }
            msg
    -> Conf { constraints | hasConfirmButton : () } msg
withConfirmButtonColor color (Conf (Config config)) =
    Conf (Config { config | confirmButtonColor = Just color })


withShowDenyButton :
    Conf { constraints | canHaveShowDenyButton : () } msg
    -> Conf { constraints | hasDenyButton : () } msg
withShowDenyButton (Conf (Config config)) =
    Conf (Config { config | showDenyButton = True })


withOnDenyButtonPress :
    msg
    -> Conf { constraints | canHaveOnDenyButtonPress : () } msg
    -> Conf constraints msg
withOnDenyButtonPress msg (Conf (Config config)) =
    Conf (Config { config | onDenyButtonPress = Just msg })


withDenyButtonText :
    String
    ->
        Conf
            { constraints | canHaveDenyButtonText : (), hasDenyButton : () }
            msg
    -> Conf { constraints | hasDenyButton : () } msg
withDenyButtonText text (Conf (Config config)) =
    Conf (Config { config | denyButtonText = text })


withDenyButtonAriaLabel :
    String
    ->
        Conf
            { constraints
                | canHaveDenyButtonAriaLabel : ()
                , hasDenyButton : ()
            }
            msg
    -> Conf { constraints | hasDenyButton : () } msg
withDenyButtonAriaLabel text (Conf (Config config)) =
    Conf (Config { config | denyButtonAriaLabel = Just text })


withDenyButtonStyle :
    List (Attribute msg)
    ->
        Conf
            { constraints | canHaveDenyButtonStyle : (), hasDenyButton : () }
            msg
    -> Conf { constraints | hasDenyButton : () } msg
withDenyButtonStyle style (Conf (Config config)) =
    Conf (Config { config | denyButtonStyle = Just style })


withDenyButtonColor :
    Color
    ->
        Conf
            { constraints | canHaveDenyButtonColor : (), hasDenyButton : () }
            msg
    -> Conf { constraints | hasDenyButton : () } msg
withDenyButtonColor color (Conf (Config config)) =
    Conf (Config { config | denyButtonColor = Just color })


withShowCancelButton :
    Conf { constraints | canHaveShowCancelButton : () } msg
    -> Conf { constraints | hasCancelButton : () } msg
withShowCancelButton (Conf (Config config)) =
    Conf (Config { config | showCancelButton = True })


withOnCancelButtonPress :
    msg
    -> Conf { constraints | canHaveOnCancelButtonPress : () } msg
    -> Conf constraints msg
withOnCancelButtonPress msg (Conf (Config config)) =
    Conf (Config { config | onCancelButtonPress = Just msg })


withCancelButtonText :
    String
    ->
        Conf
            { constraints | canHaveCancelButtonText : (), hasCancelButton : () }
            msg
    -> Conf { constraints | hasCancelButton : () } msg
withCancelButtonText text (Conf (Config config)) =
    Conf (Config { config | cancelButtonText = text })


withCancelButtonAriaLabel :
    String
    ->
        Conf
            { constraints
                | canHaveCancelButtonAriaLabel : ()
                , hasCancelButton : ()
            }
            msg
    -> Conf { constraints | hasCancelButton : () } msg
withCancelButtonAriaLabel text (Conf (Config config)) =
    Conf (Config { config | cancelButtonAriaLabel = Just text })


withCancelButtonStyle :
    List (Attribute msg)
    ->
        Conf
            { constraints
                | canHaveCancelButtonStyle : ()
                , hasCancelButton : ()
            }
            msg
    -> Conf { constraints | hasCancelButton : () } msg
withCancelButtonStyle style (Conf (Config config)) =
    Conf (Config { config | cancelButtonStyle = Just style })


withCancelButtonColor :
    Color
    ->
        Conf
            { constraints
                | canHaveCancelButtonColor : ()
                , hasCancelButton : ()
            }
            msg
    -> Conf { constraints | hasCancelButton : () } msg
withCancelButtonColor color (Conf (Config config)) =
    Conf (Config { config | cancelButtonColor = Just color })


withButtonRowStyle :
    List (Attribute msg)
    -> Conf { constraints | canHaveButtonRowStyle : () } msg
    -> Conf constraints msg
withButtonRowStyle style (Conf (Config config)) =
    Conf (Config { config | buttonRowStyle = style })


withReverseButtons :
    Conf { constraints | canHaveReverseButtons : () } msg
    -> Conf constraints msg
withReverseButtons (Conf (Config config)) =
    Conf (Config { config | reverseButtons = True })


withShowCloseButton :
    Conf { constraints | canHaveShowCloseButton : () } msg
    -> Conf { constraints | hasCloseButton : () } msg
withShowCloseButton (Conf (Config config)) =
    Conf (Config { config | showCloseButton = True })


withOnCloseButtonPress :
    msg
    -> Conf { constraints | canHaveOnCloseButtonPress : () } msg
    -> Conf constraints msg
withOnCloseButtonPress msg (Conf (Config config)) =
    Conf (Config { config | onCloseButtonPress = Just msg })


withCloseButtonAriaLabel :
    String
    ->
        Conf
            { constraints
                | canHaveCloseButtonAriaLabel : ()
                , hasCloseButton : ()
            }
            msg
    -> Conf { constraints | hasCloseButton : () } msg
withCloseButtonAriaLabel text (Conf (Config config)) =
    Conf (Config { config | closeButtonAriaLabel = Just text })


withCloseButtonStyle :
    List (Attribute msg)
    ->
        Conf
            { constraints | canHaveCloseButtonStyle : (), hasCloseButton : () }
            msg
    -> Conf { constraints | hasCloseButton : () } msg
withCloseButtonStyle style (Conf (Config config)) =
    Conf (Config { config | closeButtonStyle = style })


withoutShowBackdrop :
    Conf { constraints | canHaveShowBackdrop : (), hasBackdrop : () } msg
    -> Conf constraints msg
withoutShowBackdrop (Conf (Config config)) =
    Conf (Config { config | showBackdrop = False })


withOnBackdropPress :
    msg
    ->
        Conf
            { constraints
                | canHaveOnBackdropPress : ()
                , canHaveAllowOutsidePress : ()
            }
            msg
    -> Conf constraints msg
withOnBackdropPress msg (Conf (Config config)) =
    Conf (Config { config | onBackdropPress = Just msg })


withBackdropStyle :
    List (Attribute msg)
    -> Conf { constraints | canHaveBackdropStyle : (), hasBackdrop : () } msg
    -> Conf { constraints | hasBackdrop : () } msg
withBackdropStyle style (Conf (Config config)) =
    Conf (Config { config | backdropStyle = style })


withoutAllowOutsidePress :
    Conf { constraints | canHaveAllowOutsidePress : () } msg
    -> Conf constraints msg
withoutAllowOutsidePress (Conf (Config config)) =
    Conf (Config { config | allowOutsidePress = False })


withoutAllowEnterKey :
    Conf { constraints | canHaveAllowEnterKey : () } msg
    -> Conf constraints msg
withoutAllowEnterKey (Conf (Config config)) =
    Conf (Config { config | allowEnterKey = False })


withoutAllowEscapeKey :
    Conf { constraints | canHaveAllowEscapeKey : () } msg
    -> Conf constraints msg
withoutAllowEscapeKey (Conf (Config config)) =
    Conf (Config { config | allowEscapeKey = False })


withOnEscapeKeyPress :
    msg
    -> Conf { constraints | canHaveOnEscapeKeyPress : () } msg
    -> Conf constraints msg
withOnEscapeKeyPress msg (Conf (Config config)) =
    Conf (Config { config | onEscapeKeyPress = Just msg })


withFocusOn :
    Maybe ButtonType
    -> Conf { constraints | canHaveFocusOn : () } msg
    -> Conf constraints msg
withFocusOn focusOn (Conf (Config config)) =
    Conf (Config { config | focusOn = focusOn })


withHeader :
    Element msg
    -> Conf { constraints | canHaveHeader : () } msg
    -> Conf constraints msg
withHeader element (Conf (Config config)) =
    Conf (Config { config | header = Just element })


withFooter :
    Element msg
    -> Conf { constraints | canHaveFooter : () } msg
    -> Conf { constraints | hasFooter : () } msg
withFooter element (Conf (Config config)) =
    Conf (Config { config | footer = Just element })


withoutShowFooterDivider :
    Conf { constraints | canHaveShowFooterDivider : (), hasFooter : () } msg
    -> Conf constraints msg
withoutShowFooterDivider (Conf (Config config)) =
    Conf (Config { config | showFooterDivider = False })


withPopupStyle :
    List (Attribute msg)
    -> Conf { constraints | canHavePopupStyle : () } msg
    -> Conf constraints msg
withPopupStyle style (Conf (Config config)) =
    Conf (Config { config | popupStyle = Just style })


withBackground :
    List (Attribute msg)
    -> Conf { constraints | canHaveBackground : () } msg
    -> Conf constraints msg
withBackground style (Conf (Config config)) =
    Conf (Config { config | background = style })


withContainerStyle :
    List (Attribute msg)
    -> Conf { constraints | canHaveContainerStyle : () } msg
    -> Conf constraints msg
withContainerStyle style (Conf (Config config)) =
    Conf (Config { config | containerStyle = style })


withWidth :
    Length
    -> Conf { constraints | canHaveWidth : () } msg
    -> Conf constraints msg
withWidth length (Conf (Config config)) =
    Conf (Config { config | width = length })


withPadding :
    Padding
    -> Conf { constraints | canHavePadding : () } msg
    -> Conf constraints msg
withPadding padding (Conf (Config config)) =
    Conf (Config { config | padding = padding })


withPosition :
    Position
    -> Conf { constraints | canHavePosition : () } msg
    -> Conf constraints msg
withPosition position (Conf (Config config)) =
    Conf (Config { config | position = position })


withPopupShowAnimation :
    Animation
    -> Conf { constraints | canHavePopupShowAnimation : () } msg
    -> Conf constraints msg
withPopupShowAnimation animation (Conf (Config config)) =
    Conf (Config { config | popupShowAnimation = Just animation })


withIconShowAnimation :
    Animation
    ->
        Conf
            { constraints
                | canHaveIconShowAnimation : ()
                , hasIconOrIconLabel : ()
            }
            msg
    -> Conf { constraints | hasIconOrIconLabel : () } msg
withIconShowAnimation animation (Conf (Config config)) =
    Conf (Config { config | iconShowAnimation = Just animation })


withBackdropShowAnimation :
    Animation
    ->
        Conf
            { constraints
                | canHaveBackdropShowAnimation : ()
                , hasBackdrop : ()
            }
            msg
    -> Conf { constraints | hasBackdrop : () } msg
withBackdropShowAnimation animation (Conf (Config config)) =
    Conf (Config { config | backdropShowAnimation = Just animation })


withPopupHideAnimation :
    Animation
    -> Conf { constraints | canHavePopupHideAnimation : () } msg
    -> Conf constraints msg
withPopupHideAnimation animation (Conf (Config config)) =
    Conf (Config { config | popupHideAnimation = Just animation })


withIconHideAnimation :
    Animation
    ->
        Conf
            { constraints
                | canHaveIconHideAnimation : ()
                , hasIconOrIconLabel : ()
            }
            msg
    -> Conf { constraints | hasIconOrIconLabel : () } msg
withIconHideAnimation animation (Conf (Config config)) =
    Conf (Config { config | iconHideAnimation = Just animation })


withBackdropHideAnimation :
    Animation
    ->
        Conf
            { constraints
                | canHaveBackdropHideAnimation : ()
                , hasBackdrop : ()
            }
            msg
    -> Conf { constraints | hasBackdrop : () } msg
withBackdropHideAnimation animation (Conf (Config config)) =
    Conf (Config { config | backdropHideAnimation = Just animation })



-- VIEW


view :
    (Msg -> msg)
    -> State
    -> Config msg
    -> Element msg
view toMsg (State model) (Config config) =
    let
        popupAnimation =
            case model.visibility of
                Hiding ->
                    Maybe.withDefault emptyAnimationWithOnComplete config.popupHideAnimation

                _ ->
                    Maybe.withDefault emptyAnimation config.popupShowAnimation

        backdropAnimation =
            case model.visibility of
                Hiding ->
                    Maybe.withDefault emptyAnimationWithOnComplete config.backdropHideAnimation

                _ ->
                    Maybe.withDefault emptyAnimation config.backdropShowAnimation

        onBackdropAnimationComplete =
            if model.visibility == Hiding then
                [ onAnimationComplete (toMsg (DismissAnimationCompleted Backdrop)) ]

            else
                []

        onPopupAnimationComplete =
            if model.visibility == Hiding then
                [ onAnimationComplete (toMsg (DismissAnimationCompleted Popup)) ]

            else
                []

        closeButtonView =
            if config.showCloseButton then
                buttonView config.closeButtonStyle
                    { toMsg = toMsg
                    , allowEnterKey = config.allowEnterKey
                    , onPress1 = config.onCloseButtonPress
                    , onPress2 = Just (toMsg CloseButtonPressed)
                    , label = text "×"
                    , id = buttonTypeToId Close
                    , ariaLabel = config.closeButtonAriaLabel
                    , buttonType = Close
                    }

            else
                Element.none

        labelColor =
            case ( config.icon, config.iconColor ) of
                ( _, Just iconColor ) ->
                    [ Font.color iconColor ]

                ( Just icon, _ ) ->
                    [ Font.color (getIconLabelColor icon) ]

                _ ->
                    []

        iconView_ =
            case config.icon of
                Just icon ->
                    el (labelColor ++ config.iconStyle)
                        (el iconFontSizeFixStyle
                            (iconView
                                { icon = icon
                                , label = config.iconLabel
                                , color = config.iconColor
                                , showAnimation = config.iconShowAnimation
                                , hideAnimation = config.iconHideAnimation
                                , isShown = model.visibility
                                }
                            )
                        )

                Nothing ->
                    Element.none

        titleView =
            case config.titleLabel of
                Just title ->
                    title

                Nothing ->
                    if config.titleText /= "" then
                        paragraph config.titleStyle [ text config.titleText ]

                    else
                        Element.none

        textView =
            case config.textLabel of
                Just text ->
                    text

                Nothing ->
                    if config.text /= "" then
                        paragraph config.textStyle [ text config.text ]

                    else
                        Element.none

        headerView =
            case config.header of
                Just header ->
                    header

                Nothing ->
                    Element.none

        footerView =
            case config.footer of
                Just footer ->
                    if config.showFooterDivider then
                        footerWithDividerView footer

                    else
                        footer

                Nothing ->
                    Element.none

        focusedButton =
            case model.currentFocusedButton of
                Just _ ->
                    model.currentFocusedButton

                Nothing ->
                    config.focusOn

        cancelButtonView =
            let
                cancelButtonColor =
                    case ( config.cancelButtonColor, config.cancelButtonStyle ) of
                        ( Just cancelButtonColor_, Nothing ) ->
                            [ Background.color cancelButtonColor_ ]

                        ( Nothing, Nothing ) ->
                            [ Background.color defaultCancelButtonColor ]

                        _ ->
                            []

                cancelButtonStyle =
                    Maybe.withDefault defaultCancelButtonStyle config.cancelButtonStyle
            in
            if config.showCancelButton then
                buttonView (cancelButtonStyle ++ cancelButtonColor)
                    { toMsg = toMsg
                    , allowEnterKey = config.allowEnterKey
                    , onPress1 = config.onCancelButtonPress
                    , onPress2 = Just (toMsg CancelButtonPressed)
                    , label = text config.cancelButtonText
                    , id = buttonTypeToId Cancel
                    , ariaLabel = config.cancelButtonAriaLabel
                    , buttonType = Cancel
                    }

            else
                Element.none

        denyButtonView =
            let
                denyButtonColor =
                    case ( config.denyButtonColor, config.denyButtonStyle ) of
                        ( Just denyButtonColor_, Nothing ) ->
                            [ Background.color denyButtonColor_ ]

                        ( Nothing, Nothing ) ->
                            [ Background.color defaultDenyButtonColor ]

                        _ ->
                            []

                denyButtonStyle =
                    Maybe.withDefault defaultDenyButtonStyle config.denyButtonStyle
            in
            if config.showDenyButton then
                buttonView (denyButtonStyle ++ denyButtonColor)
                    { toMsg = toMsg
                    , allowEnterKey = config.allowEnterKey
                    , onPress1 = config.onDenyButtonPress
                    , onPress2 = Just (toMsg DenyButtonPressed)
                    , label = text config.denyButtonText
                    , id = buttonTypeToId Deny
                    , ariaLabel = config.denyButtonAriaLabel
                    , buttonType = Deny
                    }

            else
                Element.none

        confirmButtonView =
            let
                confirmButtonColor =
                    case ( config.confirmButtonColor, config.confirmButtonStyle ) of
                        ( Just confirmButtonColor_, Nothing ) ->
                            [ Background.color confirmButtonColor_ ]

                        ( Nothing, Nothing ) ->
                            [ Background.color defaultConfirmButtonColor ]

                        _ ->
                            []

                confirmButtonStyle =
                    Maybe.withDefault defaultConfirmButtonStyle config.confirmButtonStyle
            in
            if config.showConfirmButton then
                buttonView (confirmButtonStyle ++ confirmButtonColor)
                    { toMsg = toMsg
                    , allowEnterKey = config.allowEnterKey
                    , onPress1 = config.onConfirmButtonPress
                    , onPress2 = Just (toMsg ConfirmButtonPressed)
                    , label = text config.confirmButtonText
                    , id = buttonTypeToId Confirm
                    , ariaLabel = config.confirmButtonAriaLabel
                    , buttonType = Confirm
                    }

            else
                Element.none

        buttonList =
            List.filterMap
                (\( button, show ) ->
                    if show then
                        Just button

                    else
                        Nothing
                )
                [ ( Confirm, config.showConfirmButton )
                , ( Deny, config.showDenyButton )
                , ( Cancel, config.showCancelButton )
                ]

        buttonArrayWithoutCloseButton =
            Array.fromList
                (if not config.reverseButtons then
                    buttonList

                 else
                    List.reverse buttonList
                )

        buttonArray =
            if config.showCloseButton then
                Array.push Close buttonArrayWithoutCloseButton

            else
                buttonArrayWithoutCloseButton

        buttons =
            [ confirmButtonView, denyButtonView, cancelButtonView ]

        buttonsView =
            if not (List.isEmpty buttonList) then
                row config.buttonRowStyle
                    (if config.reverseButtons then
                        List.reverse buttons

                     else
                        buttons
                    )

            else
                Element.none

        showBackdropAttrs =
            if
                config.showBackdrop
                    && ((config.backdropHideAnimation == Nothing)
                            && (model.visibility == Visible)
                       )
                    || ((config.backdropHideAnimation /= Nothing)
                            && (model.visibility /= Hidden)
                       )
            then
                config.backdropStyle

            else if
                config.showBackdrop
                    && (config.backdropHideAnimation /= Nothing)
                    && (model.visibility /= Hidden)
            then
                config.backdropStyle

            else
                []

        allowOutsidePressAttrs =
            if config.allowOutsidePress then
                [ htmlAttribute
                    (Html.Events.on "click"
                        (pressOutsideTarget popupId (toMsg BackdropPressed))
                    )
                ]

            else
                []

        popupStyle =
            case config.popupStyle of
                Just popupStyle_ ->
                    popupStyle_

                Nothing ->
                    defaultPopupStyle
                        { width = config.width
                        , padding = config.padding
                        , background = config.background
                        , position = config.position
                        }

        escapeDummyButton =
            case config.onEscapeKeyPress of
                Just onEscapeKeyPress ->
                    dummyButton escapeKeyDummyButtonId onEscapeKeyPress

                Nothing ->
                    Element.none

        backdropDummyButton =
            case config.onBackdropPress of
                Just onBackdropPress ->
                    dummyButton backdropPressDummyButtonId onBackdropPress

                Nothing ->
                    Element.none
    in
    disabledScroll
        [ width fill
        , height fill
        , behindContent
            (ael backdropAnimation
                ([ width fill
                 , height fill
                 ]
                    ++ showBackdropAttrs
                    ++ onBackdropAnimationComplete
                )
                Element.none
            )
        ]
        (el
            ([ width fill
             , height fill

             -- scrollbar bug workaround: https://github.com/mdgriffith/elm-ui/issues/149
             , htmlAttribute (Html.Attributes.style "height" "100vh")
             , scrollbarY
             ]
                ++ allowOutsidePressAttrs
                ++ [ onKeysDown toMsg focusedButton model.previousFocusedButton buttonArray ]
            )
            (ael popupAnimation
                (popupStyle
                    ++ [ htmlAttribute (Html.Attributes.id popupId)
                       , Events.onFocus (Maybe.withDefault (toMsg NoOp) config.onEscapeKeyPress)
                       ]
                    ++ onPopupAnimationComplete
                )
                (column
                    (config.containerStyle ++ [ inFront closeButtonView ])
                    [ headerView
                    , iconView_
                    , titleView
                    , textView
                    , buttonsView
                    , footerView
                    , escapeDummyButton
                    , backdropDummyButton
                    ]
                )
            )
        )


popupId : String
popupId =
    "swal-pupup"


type alias Padding =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


type Position
    = Top
    | TopStart
    | TopEnd
    | Center
    | CenterEtart
    | CenterEnd
    | Bottom
    | BottomStart
    | BottomEnd


positionToAttributes : Position -> List (Attribute msg)
positionToAttributes position =
    case position of
        Top ->
            [ centerX ]

        TopStart ->
            []

        TopEnd ->
            [ alignRight ]

        Center ->
            [ centerY, centerX ]

        CenterEtart ->
            [ centerY ]

        CenterEnd ->
            [ centerY, alignRight ]

        Bottom ->
            [ alignBottom, centerX ]

        BottomStart ->
            [ alignBottom ]

        BottomEnd ->
            [ alignBottom, alignRight ]


type ButtonType
    = Confirm
    | Deny
    | Cancel
    | Close


buttonTypeToId : ButtonType -> String
buttonTypeToId button =
    case button of
        Confirm ->
            "swal-confirm"

        Deny ->
            "swal-deny"

        Cancel ->
            "swal-cancel"

        Close ->
            "swal-close"


type Direction
    = Left
    | Right


lostFocus : Maybe a -> Maybe a -> Bool
lostFocus currButtonType prevButtonType =
    currButtonType /= Nothing && currButtonType == prevButtonType


getNextFocusButton :
    Maybe ButtonType
    -> Maybe ButtonType
    -> Direction
    -> Bool
    -> Array ButtonType
    -> Maybe ButtonType
getNextFocusButton currButtonType prevButtonType dir circular buttonArray =
    let
        indexedList =
            List.indexedMap (\i e -> ( i, e )) (Array.toList buttonArray)

        currButtonIndex : Maybe Int
        currButtonIndex =
            List.head
                (List.filterMap
                    (\( i, b ) ->
                        if Just b == currButtonType then
                            Just i

                        else
                            Nothing
                    )
                    indexedList
                )

        currButtonIndex_ =
            case currButtonIndex of
                Just buttonIndex ->
                    buttonIndex

                Nothing ->
                    if dir == Right then
                        0

                    else
                        Array.length buttonArray - 1

        isCloseButton index =
            Maybe.withDefault False (Maybe.map ((==) Close) (Array.get index buttonArray))

        didLoseFocus =
            lostFocus currButtonType prevButtonType
    in
    if dir == Right then
        let
            nextButtonIndex_ =
                currButtonIndex_ + 1

            nextButtonIndex =
                if didLoseFocus then
                    0

                else if not circular then
                    if isCloseButton nextButtonIndex_ then
                        currButtonIndex_

                    else
                        nextButtonIndex_

                else
                    modBy (Array.length buttonArray) nextButtonIndex_
        in
        Array.get nextButtonIndex buttonArray

    else
        let
            prevButtonIndex_ =
                currButtonIndex_ - 1

            prevButtonIndex =
                if didLoseFocus then
                    Array.length buttonArray - 1

                else if not circular then
                    if isCloseButton currButtonIndex_ || isCloseButton prevButtonIndex_ then
                        currButtonIndex_

                    else
                        prevButtonIndex_

                else
                    modBy (Array.length buttonArray) prevButtonIndex_
        in
        Array.get prevButtonIndex buttonArray


getFocusKeyMsg :
    Bool
    -> (Msg -> msg)
    -> Maybe ButtonType
    -> Maybe ButtonType
    -> Array ButtonType
    -> Direction
    -> Maybe msg
getFocusKeyMsg circular toMsg currButton prevButton buttonArray direction =
    let
        nextFocusButtonInfo =
            getNextFocusButton currButton prevButton direction circular buttonArray
    in
    case nextFocusButtonInfo of
        Just buttonType ->
            Just (toMsg (FocusOnButton buttonType))

        _ ->
            Just (toMsg NoOp)


getArrowKeyMsg :
    (Msg -> msg)
    -> Maybe ButtonType
    -> Maybe ButtonType
    -> Array ButtonType
    -> Direction
    -> Maybe msg
getArrowKeyMsg =
    getFocusKeyMsg False


getTabKeyMsg :
    (Msg -> msg)
    -> Maybe ButtonType
    -> Maybe ButtonType
    -> Array ButtonType
    -> Direction
    -> Maybe msg
getTabKeyMsg =
    getFocusKeyMsg True


onKeysDown :
    (Msg -> msg)
    -> Maybe ButtonType
    -> Maybe ButtonType
    -> Array ButtonType
    -> Attribute msg
onKeysDown toMsg currButton prevButton buttonArray =
    let
        getKeyMsg : KeyboardEvent -> Maybe msg
        getKeyMsg keyboardEvent =
            case keyboardEvent.keyCode of
                Key.Escape ->
                    Just (toMsg EscapeKeyPressed)

                Key.Tab ->
                    if not keyboardEvent.shiftKey then
                        getTabKeyMsg toMsg currButton prevButton buttonArray Right

                    else
                        getTabKeyMsg toMsg currButton prevButton buttonArray Left

                Key.Right ->
                    getArrowKeyMsg toMsg currButton prevButton buttonArray Right

                Key.Down ->
                    getArrowKeyMsg toMsg currButton prevButton buttonArray Right

                Key.Left ->
                    getArrowKeyMsg toMsg currButton prevButton buttonArray Left

                Key.Up ->
                    getArrowKeyMsg toMsg currButton prevButton buttonArray Left

                _ ->
                    Nothing
    in
    htmlAttribute
        (Html.Events.preventDefaultOn "keydown"
            (considerKeyboardEvent
                (\keyboardEvent ->
                    Maybe.map (\msg -> ( msg, True )) (getKeyMsg keyboardEvent)
                )
            )
        )


onButtonKeysUp : Maybe msg -> Attribute msg
onButtonKeysUp msg =
    let
        getKeyMsg : KeyboardEvent -> Maybe msg
        getKeyMsg keyboardEvent =
            case keyboardEvent.keyCode of
                Key.Enter ->
                    msg

                Key.Spacebar ->
                    msg

                _ ->
                    Nothing
    in
    htmlAttribute
        (Html.Events.preventDefaultOn "keyup"
            (considerKeyboardEvent
                (\keyboardEvent ->
                    Maybe.map (\msg_ -> ( msg_, True )) (getKeyMsg keyboardEvent)
                )
            )
        )


buttonView :
    List (Attribute msg)
    ->
        { toMsg : Msg -> msg
        , allowEnterKey : Bool
        , onPress1 : Maybe msg
        , onPress2 : Maybe msg
        , label : Element msg
        , id : String
        , ariaLabel : Maybe String
        , buttonType : ButtonType
        }
    -> Element msg
buttonView attrs { toMsg, allowEnterKey, onPress1, onPress2, label, id, ariaLabel, buttonType } =
    let
        ariaAttributes =
            case ariaLabel of
                Just ariaLabel_ ->
                    ariaLabelStyle ariaLabel_

                Nothing ->
                    []

        maybeNoOpMsg msg =
            if allowEnterKey then
                case msg of
                    Just m ->
                        Just m

                    Nothing ->
                        Just (toMsg NoOp)

            else
                Nothing

        onEnterWithNoOp msg =
            onButtonKeysUp (maybeNoOpMsg msg)

        onClick msg =
            case msg of
                Just m ->
                    [ Events.onClick m ]

                Nothing ->
                    []
    in
    el
        ([ onEnterWithNoOp onPress1
         , width fill

         {- Close button focus bug fix:
            because of width fill, the next focused button will be the close button
         -}
         , htmlAttribute (Html.Attributes.tabindex -1)
         , Events.onFocus (toMsg (FocusOnDummy backdropPressDummyButtonId))
         ]
            ++ onClick onPress1
        )
        (Input.button
            (attrs
                ++ ariaAttributes
                ++ [ onEnterWithNoOp onPress2
                   , htmlAttribute (Html.Attributes.id id)
                   , Events.onFocus (toMsg (GotFocus buttonType))
                   , Events.onLoseFocus (toMsg (LostFocus buttonType))
                   ]
            )
            { onPress = onPress2
            , label = label
            }
        )


{-| The onFocus event of the dummyButton is used to
send onEscapeKeyPress and onBackdropPress events
-}
dummyButton : String -> msg -> Element msg
dummyButton id onFocus =
    Input.button
        [ htmlAttribute (Html.Attributes.id id)
        , htmlAttribute (Html.Attributes.tabindex -1)
        , Events.onFocus onFocus
        , focused []
        ]
        { onPress = Nothing
        , label = Element.none
        }


escapeKeyDummyButtonId : String
escapeKeyDummyButtonId =
    "swal_escape_key"


backdropPressDummyButtonId : String
backdropPressDummyButtonId =
    "swal_backdrop_press"



-- ICONS


type Icon
    = Success
    | Error
    | Warning
    | Info
    | Question


iconView :
    { icon : Icon
    , label : Maybe (Element msg)
    , color : Maybe Color
    , showAnimation : Maybe Animation
    , hideAnimation : Maybe Animation
    , isShown : Visibility
    }
    -> Element msg
iconView { icon, label, color, showAnimation, hideAnimation, isShown } =
    case icon of
        Success ->
            successIconView
                { label = label
                , color = color
                , showAnimation = showAnimation
                , hideAnimation = hideAnimation
                , isShown = isShown
                }

        Error ->
            errorIconView
                { label = label
                , color = color
                , showAnimation = showAnimation
                , hideAnimation = hideAnimation
                , isShown = isShown
                }

        Warning ->
            warningIconView
                { label = label
                , color = color
                , showAnimation = showAnimation
                , hideAnimation = hideAnimation
                , isShown = isShown
                }

        Info ->
            infoIconView
                { label = label
                , color = color
                , showAnimation = showAnimation
                , hideAnimation = hideAnimation
                , isShown = isShown
                }

        Question ->
            questionIconView
                { label = label
                , color = color
                , showAnimation = showAnimation
                , hideAnimation = hideAnimation
                , isShown = isShown
                }


getIconLabelColor : Icon -> Color
getIconLabelColor icon =
    case icon of
        Success ->
            successIconColor

        Error ->
            errorIconColor

        Warning ->
            warningLabelColor

        Info ->
            infoIconLabelColor

        Question ->
            questionIconLabelColor


successIconView :
    { label : Maybe (Element msg)
    , color : Maybe Color
    , showAnimation : Maybe Animation
    , hideAnimation : Maybe Animation
    , isShown : Visibility
    }
    -> Element msg
successIconView { label, color, showAnimation, hideAnimation, isShown } =
    let
        label_ =
            case label of
                Just l ->
                    iconLabelView
                        { label = l
                        }

                Nothing ->
                    defaultSuccessIconLabel
                        { color = borderColor
                        , shouldAnimate = showAnimation == Nothing
                        }

        borderColor =
            case color of
                Just color_ ->
                    color_

                Nothing ->
                    successIconColor

        animation =
            case isShown of
                Hiding ->
                    Maybe.withDefault emptyAnimation hideAnimation

                _ ->
                    Maybe.withDefault emptyAnimation showAnimation
    in
    ael animation
        (iconBorderStyle { borderColor = borderColor })
        label_


errorIconView :
    { label : Maybe (Element msg)
    , color : Maybe Color
    , showAnimation : Maybe Animation
    , hideAnimation : Maybe Animation
    , isShown : Visibility
    }
    -> Element msg
errorIconView { label, color, showAnimation, hideAnimation, isShown } =
    let
        label_ =
            case label of
                Just l ->
                    iconLabelView
                        { label = l
                        }

                Nothing ->
                    defaultErrorIconLabel
                        { color = borderColor
                        , shouldAnimate = showAnimation == Nothing
                        }

        borderColor =
            case color of
                Just color_ ->
                    color_

                Nothing ->
                    errorIconColor

        animation =
            case isShown of
                Hiding ->
                    Maybe.withDefault emptyAnimation hideAnimation

                _ ->
                    Maybe.withDefault errorIconAnimation showAnimation
    in
    ael animation
        (iconBorderStyle { borderColor = borderColor })
        label_


warningIconView :
    { label : Maybe (Element msg)
    , color : Maybe Color
    , showAnimation : Maybe Animation
    , hideAnimation : Maybe Animation
    , isShown : Visibility
    }
    -> Element msg
warningIconView { label, color, showAnimation, hideAnimation, isShown } =
    let
        label_ =
            case label of
                Just l ->
                    l

                Nothing ->
                    defaultWarningIconLabel

        borderColor =
            case color of
                Just color_ ->
                    color_

                Nothing ->
                    rgb255 0xFA 0xCE 0xA8

        animation =
            case isShown of
                Hiding ->
                    Maybe.withDefault emptyAnimation hideAnimation

                _ ->
                    Maybe.withDefault emptyAnimation showAnimation
    in
    ael animation
        []
        (baseIconView
            { borderColor = borderColor
            , label = label_
            }
        )


infoIconView :
    { label : Maybe (Element msg)
    , color : Maybe Color
    , showAnimation : Maybe Animation
    , hideAnimation : Maybe Animation
    , isShown : Visibility
    }
    -> Element msg
infoIconView { label, color, showAnimation, hideAnimation, isShown } =
    let
        label_ =
            case label of
                Just l ->
                    l

                Nothing ->
                    defaultInfoIconLabel

        borderColor =
            case color of
                Just color_ ->
                    color_

                Nothing ->
                    rgb255 0x9D 0xE0 0xF6

        animation =
            case isShown of
                Hiding ->
                    Maybe.withDefault emptyAnimation hideAnimation

                _ ->
                    Maybe.withDefault emptyAnimation showAnimation
    in
    ael animation
        []
        (baseIconView
            { borderColor = borderColor
            , label = label_
            }
        )


questionIconView :
    { label : Maybe (Element msg)
    , color : Maybe Color
    , showAnimation : Maybe Animation
    , hideAnimation : Maybe Animation
    , isShown : Visibility
    }
    -> Element msg
questionIconView { label, color, showAnimation, hideAnimation, isShown } =
    let
        label_ =
            case label of
                Just l ->
                    l

                Nothing ->
                    defaultQuestionIconLabel

        borderColor =
            case color of
                Just color_ ->
                    color_

                Nothing ->
                    rgb255 0xC9 0xDA 0xE1

        animation =
            case isShown of
                Hiding ->
                    Maybe.withDefault emptyAnimation hideAnimation

                _ ->
                    Maybe.withDefault emptyAnimation showAnimation
    in
    ael animation
        []
        (baseIconView
            { borderColor = borderColor
            , label = label_
            }
        )


iconLabelView : { label : Element msg } -> Element msg
iconLabelView { label } =
    el iconTextStyle label


baseIconView : { borderColor : Color, label : Element msg } -> Element msg
baseIconView { borderColor, label } =
    el (iconBorderStyle { borderColor = borderColor })
        (iconLabelView { label = label })


defaultSuccessIconLabel : { color : Color, shouldAnimate : Bool } -> Element msg
defaultSuccessIconLabel { color, shouldAnimate } =
    let
        ( lineTipAnimation, lineLongAnimation, circularLineAnimation ) =
            if shouldAnimate then
                ( successIconLineTipAnimation
                , successIconLineLongAnimation
                , successIconCircularLineAnimation
                )

            else
                ( emptyAnimation, emptyAnimation, emptyAnimation )

        successLineStyle =
            [ Background.color color
            , htmlAttribute (Html.Attributes.style "position" "absolute")
            , htmlAttribute (Html.Attributes.style "height" ".3125em")
            , htmlAttribute (Html.Attributes.style "border-radius" "0.125em")
            ]

        circularLineStyle =
            [ rotate (degrees -45)
            , Border.rounded 100
            , htmlAttribute (Html.Attributes.style "position" "absolute")
            , htmlAttribute (Html.Attributes.style "width" "3.75em")
            , htmlAttribute (Html.Attributes.style "height" "7.5em")
            ]

        circleStyle =
            iconBorderStyle { borderColor = colorWithAlpha 0.3 color }
                ++ [ htmlAttribute (Html.Attributes.style "position" "absolute")
                   , htmlAttribute (Html.Attributes.style "top" "-.25em")
                   , htmlAttribute (Html.Attributes.style "left" "-.25em")
                   ]

        lineTipView =
            ael lineTipAnimation
                (successLineStyle
                    ++ [ rotate (degrees 45)
                       , htmlAttribute (Html.Attributes.style "top" "2.875em")
                       , htmlAttribute (Html.Attributes.style "left" ".8125em")
                       , htmlAttribute (Html.Attributes.style "width" "1.5625em")
                       ]
                )
                Element.none

        lineLongView =
            ael lineLongAnimation
                (successLineStyle
                    ++ [ rotate (degrees -45)
                       , htmlAttribute (Html.Attributes.style "top" "2.375em")
                       , htmlAttribute (Html.Attributes.style "right" ".5em")
                       , htmlAttribute (Html.Attributes.style "width" "2.9375em")
                       ]
                )
                Element.none

        circleAndCheckmarkView =
            column circleStyle
                [ lineTipView
                , lineLongView
                ]

        circularLineLeftView =
            el
                (circularLineStyle
                    ++ [ Background.color whiteColor
                       , htmlAttribute (Html.Attributes.style "top" "-.4375em")
                       , htmlAttribute (Html.Attributes.style "left" "-2.0635em")
                       , htmlAttribute (Html.Attributes.style "transform-origin" "3.75em 3.75em")
                       , htmlAttribute (Html.Attributes.style "border-radius" "7.5em 0 0 7.5em")
                       ]
                )
                Element.none

        circularLineRightView =
            ael circularLineAnimation
                (circularLineStyle
                    ++ [ Background.color whiteColor
                       , htmlAttribute (Html.Attributes.style "top" "-.6875em")
                       , htmlAttribute (Html.Attributes.style "left" "1.875em")
                       , htmlAttribute (Html.Attributes.style "transform-origin" "0 3.75em")
                       , htmlAttribute (Html.Attributes.style "border-radius" "0 7.5em 7.5em 0")
                       ]
                )
                Element.none

        fixView =
            el
                [ Background.color whiteColor
                , rotate (degrees -45)
                , htmlAttribute (Html.Attributes.style "position" "absolute")
                , htmlAttribute (Html.Attributes.style "top" "0.5em")
                , htmlAttribute (Html.Attributes.style "left" "1.625em")
                , htmlAttribute (Html.Attributes.style "width" ".4375em")
                , htmlAttribute (Html.Attributes.style "height" "5.625em")
                ]
                Element.none

        maskView =
            column
                []
                [ circularLineLeftView
                , circularLineRightView
                , fixView
                ]
    in
    column [ behindContent maskView ]
        [ circleAndCheckmarkView ]


defaultErrorIconLabel : { color : Color, shouldAnimate : Bool } -> Element msg
defaultErrorIconLabel { color, shouldAnimate } =
    let
        xMarkStyle =
            [ width (px 47)
            , height (px 6)
            , Border.rounded 3
            , Background.color color
            ]

        animation =
            if shouldAnimate then
                errorXMarkAnimation

            else
                emptyAnimation
    in
    ael animation
        [ centerX, centerY ]
        (column [ centerX, centerY ]
            [ el (xMarkStyle ++ [ rotate (degrees 45), moveDown 3 ])
                Element.none
            , el (xMarkStyle ++ [ rotate (degrees -45), moveUp 3 ])
                Element.none
            ]
        )


defaultWarningIconLabel : Element msg
defaultWarningIconLabel =
    text "!"


defaultInfoIconLabel : Element msg
defaultInfoIconLabel =
    text "i"


defaultQuestionIconLabel : Element msg
defaultQuestionIconLabel =
    text "?"


footerWithDividerView : Element msg -> Element msg
footerWithDividerView child =
    column [ width fill ]
        [ el [ width fill, paddingXY 0 16 ] (el footerDividerStyle Element.none)
        , child
        ]



-- COLORS


whiteColor : Color
whiteColor =
    rgb255 0xFF 0xFF 0xFF


successIconColor : Color
successIconColor =
    rgb255 0xA5 0xDC 0x86


errorIconColor : Color
errorIconColor =
    rgb255 0xF2 0x74 0x74


warningLabelColor : Color
warningLabelColor =
    rgb255 0xF8 0xBB 0x86


infoIconLabelColor : Color
infoIconLabelColor =
    rgb255 0x3F 0xC3 0xEE


questionIconLabelColor : Color
questionIconLabelColor =
    rgb255 0x87 0xAD 0xBD


defaultBackdropColor : Color
defaultBackdropColor =
    rgba255 0x00 0x00 0x00 0.6


defaultCloseButtonHoverColor : Color
defaultCloseButtonHoverColor =
    rgb255 0xF2 0x74 0x74


defaultConfirmButtonColor : Color
defaultConfirmButtonColor =
    rgb255 0x73 0x67 0xF0


defaultDenyButtonColor : Color
defaultDenyButtonColor =
    rgb255 0xEA 0x54 0x55


defaultCancelButtonColor : Color
defaultCancelButtonColor =
    rgb255 0x6E 0x7D 0x88



-- STYLES


defaultConfirmButtonStyle : List (Attribute msg)
defaultConfirmButtonStyle =
    defaultButtonStyle { color = defaultConfirmButtonColor }


defaultDenyButtonStyle : List (Attribute msg)
defaultDenyButtonStyle =
    defaultButtonStyle { color = defaultDenyButtonColor }


defaultCancelButtonStyle : List (Attribute msg)
defaultCancelButtonStyle =
    defaultButtonStyle { color = defaultCancelButtonColor }


defaultButtonStyle : { color : Color } -> List (Attribute msg)
defaultButtonStyle { color } =
    let
        rgbColor =
            toRgb color

        shadowColor =
            fromRgb { rgbColor | alpha = 0.5 }
    in
    [ Font.size 16
    , Font.color whiteColor
    , Border.rounded 4
    , Background.color color
    , paddingXY 18 12
    , focused
        [ Border.shadow
            { offset = ( 0, 0 ), size = 3, blur = 0, color = shadowColor }
        ]
    , Element.mouseDown
        [ Background.gradient
            { angle = degrees 180, steps = [ rgba 0 0 0 0.2, rgba 0 0 0 0.2 ] }
        ]
    , Element.mouseOver
        [ Background.gradient
            { angle = degrees 180, steps = [ rgba 0 0 0 0.1, rgba 0 0 0 0.1 ] }
        ]
    ]


defaultButtonRowStyle : List (Attribute msg)
defaultButtonRowStyle =
    [ spacing 10
    , centerX
    , paddingEach { top = 25, bottom = 5, left = 0, right = 0 }
    ]


defaultBackgroundStyle : List (Attr decorative msg)
defaultBackgroundStyle =
    [ Background.color whiteColor ]


defaultBackdropStyle : List (Attribute msg)
defaultBackdropStyle =
    [ Background.color defaultBackdropColor ]


defaultCloseButtonStyle : List (Attribute msg)
defaultCloseButtonStyle =
    [ Font.size 40
    , Font.color (rgb255 0xCC 0xCC 0xCC)
    , alignRight
    , paddingXY 12 4
    , Element.mouseOver
        [ Font.color defaultCloseButtonHoverColor ]
    ]


defaultPopupStyle :
    { width : Length
    , padding : Padding
    , background : List (Attribute msg)
    , position : Position
    }
    -> List (Attribute msg)
defaultPopupStyle { width, padding, background, position } =
    [ Border.rounded 5
    , Element.width width
    , paddingEach padding
    , behindContent
        (el ([ Element.width fill, height fill, Border.rounded 5 ] ++ background)
            Element.none
        )
    ]
        ++ positionToAttributes position


defaultContainerStyle : List (Attribute msg)
defaultContainerStyle =
    [ width fill, centerX ]


defaultIconStyle : List (Attribute msg)
defaultIconStyle =
    [ Font.size 60
    , centerX
    , paddingEach { top = 40, bottom = 10, left = 0, right = 0 }
    ]


defaultTitleStyle : List (Attribute msg)
defaultTitleStyle =
    [ Font.size 30
    , Font.color (rgb255 0x59 0x59 0x59)
    , Font.semiBold
    , Font.center
    , paddingEach { top = 24, bottom = 2, left = 16, right = 16 }
    , spacing 15
    ]


defaultTextStyle : List (Attribute msg)
defaultTextStyle =
    [ Font.size 18
    , Font.color (rgb255 0x54 0x54 0x54)
    , paddingEach { top = 18, bottom = 6, left = 26, right = 26 }
    , spacing 15
    ]


ariaLabelStyle : String -> List (Attribute msg)
ariaLabelStyle text =
    [ htmlAttribute (Html.Attributes.attribute "aria-label" text) ]


disabledTextSelectionStyle : List (Attribute msg)
disabledTextSelectionStyle =
    [ htmlAttribute (Html.Attributes.style "-webkit-user-select" "none")
    , htmlAttribute (Html.Attributes.style "user-select" "none")
    ]


iconFontSizeFixStyle : List (Attribute msg)
iconFontSizeFixStyle =
    [ htmlAttribute (Html.Attributes.style "font-size" "calc(16/60*100%)") ]


iconBorderStyle : { borderColor : Color } -> List (Attribute msg)
iconBorderStyle { borderColor } =
    [ Border.color borderColor
    , centerX
    , htmlAttribute (Html.Attributes.style "box-sizing" "content-box")
    , htmlAttribute (Html.Attributes.style "width" "5em")
    , htmlAttribute (Html.Attributes.style "height" "5em")
    , htmlAttribute (Html.Attributes.style "border-width" "0.25em")
    , Border.rounded 100
    ]


iconTextStyle : List (Attribute msg)
iconTextStyle =
    disabledTextSelectionStyle
        ++ [ Font.center
           , centerX
           , htmlAttribute (Html.Attributes.style "font-size" "3.75em")
           , htmlAttribute (Html.Attributes.style "padding-top" "0.14em")
           ]


footerDividerStyle : List (Attribute msg)
footerDividerStyle =
    [ width fill
    , height (px 1)
    , Background.color (rgb255 0xEE 0xEE 0xEE)
    ]



-- ANIMATIONS


defaultAppearanceAnimation : Animation
defaultAppearanceAnimation =
    percentSteps
        { duration = 300
        , options = []
        }
        [ percentStep 0 [ P.scale 0.7 ]
        , percentStep 45 [ P.scale 1.05 ]
        , percentStep 80 [ P.scale 0.95 ]
        , percentStep 100 [ P.scale 1 ]
        ]


defaultDisappearanceAnimation : Animation
defaultDisappearanceAnimation =
    Animation.fromTo
        { duration = 150
        , options = [ Animation.count 1 ]
        }
        [ P.scale 1, P.opacity 1 ]
        [ P.scale 0.5, P.opacity 0 ]


successIconCircularLineAnimation : Animation
successIconCircularLineAnimation =
    percentSteps
        { duration = 4250
        , options = [ Animation.easeIn ]
        }
        [ percentStep 0 [ P.rotate -45 ]
        , percentStep 5 [ P.rotate -45 ]
        , percentStep 12 [ P.rotate -405 ]
        , percentStep 100 [ P.rotate -405 ]
        ]


successIconLineLongAnimation : Animation
successIconLineLongAnimation =
    percentSteps
        { duration = 750
        , options = []
        }
        [ percentStep 0
            [ P.property "top" "3.375em"
            , P.property "right" "2.875em"
            , P.property "width" "0"
            ]
        , percentStep 65
            [ P.property "top" "3.375em"
            , P.property "right" "2.875em"
            , P.property "width" "0"
            ]
        , percentStep 84
            [ P.property "top" "2.1875em"
            , P.property "right" "0"
            , P.property "width" "3.4375em"
            ]
        , percentStep 100
            [ P.property "top" "2.375em"
            , P.property "right" "0.5em"
            , P.property "width" "2.9375em"
            ]
        ]


successIconLineTipAnimation : Animation
successIconLineTipAnimation =
    percentSteps
        { duration = 750
        , options = []
        }
        [ percentStep 0
            [ P.property "top" "1.1875em"
            , P.property "left" "0.0625em"
            , P.property "width" "0"
            ]
        , percentStep 54
            [ P.property "top" "1.0625em"
            , P.property "left" "0.125em"
            , P.property "width" "0"
            ]
        , percentStep 70
            [ P.property "top" "2.1875em"
            , P.property "left" "-0.375em"
            , P.property "width" "3.125em"
            ]
        , percentStep 84
            [ P.property "top" "3em"
            , P.property "left" "1.3125em"
            , P.property "width" "1.0625em"
            ]
        , percentStep 100
            [ P.property "top" "2.8125em"
            , P.property "left" "0.8125em"
            , P.property "width" "1.5625em"
            ]
        ]


errorIconAnimation : Animation
errorIconAnimation =
    Animation.fromTo
        { duration = 500
        , options = []
        }
        [ P.property "transform" "rotateX(100deg)", P.opacity 0 ]
        [ P.property "transform" "rotateX(0deg)", P.opacity 1 ]


errorXMarkAnimation : Animation
errorXMarkAnimation =
    percentSteps
        { duration = 500
        , options = []
        }
        [ percentStep 0 [ P.y 26, P.scale 0.4, P.opacity 0 ]
        , percentStep 50 [ P.y 26, P.scale 0.4, P.opacity 0 ]
        , percentStep 80 [ P.y -6, P.scale 1.15 ]
        , percentStep 100 [ P.y 0, P.scale 1, P.opacity 1 ]
        ]



-- UTILS


ael : Animation -> List (Attribute msg) -> Element msg -> Element msg
ael =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }
        Element.el


onAnimationComplete : msg -> Attribute msg
onAnimationComplete msg =
    htmlAttribute (Html.Events.on "animationend" (Decode.succeed msg))


emptyAnimation : Animation
emptyAnimation =
    Animation.fromTo
        { duration = 0
        , options = []
        }
        []
        []


{-| emptyAnimation doesn't trigger onAnimationComplete
-}
emptyAnimationWithOnComplete : Animation
emptyAnimationWithOnComplete =
    Animation.fromTo
        { duration = 0
        , options = [ Animation.count 1 ]
        }
        []
        []


type PercentStep
    = PercentStep Millis (List P.Property)


percentStep : Millis -> List P.Property -> PercentStep
percentStep =
    PercentStep


percentStepsToSteps : Millis -> List PercentStep -> List Animation.Step
percentStepsToSteps duration steps =
    let
        calcStepDuration :
            PercentStep
            -> ( List ( Int, List P.Property ), Millis )
            -> ( List ( Int, List P.Property ), Millis )
        calcStepDuration (PercentStep currPercent properties) ( acc, prevPercent ) =
            ( acc ++ [ ( (currPercent - prevPercent) * duration // 100, properties ) ]
            , currPercent
            )

        percentToMillis : List ( Millis, List P.Property )
        percentToMillis =
            Tuple.first (List.foldl calcStepDuration ( [], 0 ) steps)
    in
    List.map (\( percent, properties ) -> Animation.step percent properties) percentToMillis


percentSteps :
    { duration : Millis, options : List Animation.Option }
    -> List PercentStep
    -> Animation
percentSteps { duration, options } steps_ =
    Animation.steps
        { startAt = []
        , options = options
        }
        (percentStepsToSteps duration steps_)


disabledScroll : List (Attribute msg) -> Element msg -> Element msg
disabledScroll attrs child =
    column attrs
        [ html
            (Html.Styled.toUnstyled
                (Css.Global.global
                    [ Css.Global.html [ Css.overflow Css.hidden ] ]
                )
            )
        , child
        ]


{-| source: <https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh>
-}
pressOutsideTarget : String -> msg -> Decoder msg
pressOutsideTarget id msg =
    let
        isOutsideTarget id_ =
            Decode.oneOf
                [ Decode.field "id" Decode.string
                    |> Decode.andThen
                        (\id__ ->
                            if id_ == id__ then
                                -- found match by id
                                Decode.succeed False

                            else
                                -- try next decoder
                                Decode.fail "check parent node"
                        )
                , Decode.lazy (\_ -> isOutsideTarget id_ |> Decode.field "parentNode")

                -- fallback if all previous decoders failed
                , Decode.succeed True
                ]
    in
    Decode.field "target" (isOutsideTarget id)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed msg

                else
                    Decode.fail "inside element"
            )


colorWithAlpha : Float -> Color -> Color
colorWithAlpha alpha color =
    let
        color_ =
            toRgb color
    in
    fromRgb { color_ | alpha = alpha }
